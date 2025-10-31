// A simple type class for Chisel datatypes that can add and multiply. To add your own type, simply create your own:
//     implicit MyTypeArithmetic extends Arithmetic[MyType] { ... }

package gemmini

import chisel3._
import chisel3.util._
import hardfloat._
import mxHardware._

// Bundles that represent the raw bits of custom datatypes
case class Float(expWidth: Int, sigWidth: Int, isRecoded: Boolean = false) extends Bundle {
  val bits = UInt((expWidth + sigWidth + (if (isRecoded) 1 else 0)).W)
  val bias: Int = (1 << (expWidth-1)) - 1
}


case class MxFloat(expWidth: Int, sigWidth: Int, count: Int, isRecoded: Boolean = false) extends Bundle {
  val bits = UInt((1<<log2Ceil(count * (expWidth + sigWidth + (if (isRecoded) 1 else 0)))).W)
  val bias: Int = (1 << (expWidth-1)) - 1
}

case class DummySInt(w: Int) extends Bundle {
  val bits = UInt(w.W)
  def dontCare: DummySInt = {
    val o = Wire(new DummySInt(w))
    o.bits := 0.U
    o
  }
}

// case class MxFloat(mxParams: MxParams, modeId: Int = 0) extends Bundle {
//   val bits = UInt(mxParams.multOutWidth.W)
//   def currentMode: PE_MxMode = mxParams.modesSupported(modeId)
//   def Width_act: Int = currentMode.actWidth  
//   def Width_w: Int = currentMode.weiWidth 
//   def Width_out: Int = currentMode.outTotalWidth 
// }

// case class MxFloat(mxParams: MxParams, modeId: Int = 0) extends Bundle {
//   def currentMode: PE_MxMode = mxParams.modesSupported(modeId)
//   val numOutputs: Int = currentMode.numOutputs //TODO: add this in Mxparameters
//   val bits = if (numOutputs == 1) {
//     UInt(mxParams.multOutWidth.W)
//   } else {
//     Vec(numOutputs, UInt((mxParams.outTotalWidth / numOutputs).W))
//   }
  
//   def Width_act: Int = currentMode.actWidth  
//   def Width_w: Int = currentMode.weiWidth 
//   def Width_out: Int = currentMode.outTotalWidth 
//   def getSlice(index: Int): UInt = {
//     if (numOutputs == 1) {
//       bits.asUInt
//     } else {
//       require(index < numOutputs, s"Index $index out of range for numOutputs $numOutputs")
//       bits(index)
//     }
//   }
  
//   def getAllSlices: Seq[UInt] = {
//     if (numOutputs == 1) {
//       Seq(bits.asUInt)
//     } else {
//       bits
//     }
//   }
// }

// The Arithmetic typeclass which implements various arithmetic operations on custom datatypes
abstract class Arithmetic[T <: Data] {
  implicit def cast(t: T): ArithmeticOps[T]
}

abstract class ArithmeticOps[T <: Data](self: T) {
  def *(t: T): T
  def mac(m1: T, m2: T): T // Returns (m1 * m2 + self)
  def +(t: T): T
  def -(t: T): T
  def >>(u: UInt): T // This is a rounding shift! Rounds away from 0
  def >(t: T): Bool
  def identity: T
  def withWidthOf(t: T): T
  def clippedToWidthOf(t: T): T // Like "withWidthOf", except that it saturates
  def relu: T
  def zero: T
  def minimum: T

  // Optional parameters, which only need to be defined if you want to enable various optimizations for transformers
  def divider(denom_t: UInt, options: Int = 0): Option[(DecoupledIO[UInt], DecoupledIO[T])] = None
  def sqrt: Option[(DecoupledIO[UInt], DecoupledIO[T])] = None
  def reciprocal[U <: Data](u: U, options: Int = 0): Option[(DecoupledIO[UInt], DecoupledIO[U])] = None
  def mult_with_reciprocal[U <: Data](reciprocal: U) = self
}

object Arithmetic {
  implicit object UIntArithmetic extends Arithmetic[UInt] {
    override implicit def cast(self: UInt) = new ArithmeticOps(self) {
      override def *(t: UInt) = self * t
      override def mac(m1: UInt, m2: UInt) = m1 * m2 + self
      override def +(t: UInt) = self + t
      override def -(t: UInt) = self - t

      override def >>(u: UInt) = {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt & ((1.U << (u - 1.U)).asUInt - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = point_five & (zeros | ones_digit)

        (self >> u).asUInt + r
      }

      override def >(t: UInt): Bool = self > t

      override def withWidthOf(t: UInt) = self.asTypeOf(t)

      override def clippedToWidthOf(t: UInt) = {
        val sat = ((1 << (t.getWidth-1))-1).U
        Mux(self > sat, sat, self)(t.getWidth-1, 0)
      }

      override def relu: UInt = self

      override def zero: UInt = 0.U
      override def identity: UInt = 1.U
      override def minimum: UInt = 0.U
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
    override implicit def cast(self: SInt) = new ArithmeticOps(self) {
      override def *(t: SInt) = self * t
      override def mac(m1: SInt, m2: SInt) = m1 * m2 + self
      override def +(t: SInt) = self + t
      override def -(t: SInt) = self - t

      override def >>(u: UInt) = {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt & ((1.U << (u - 1.U)).asUInt - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = (point_five & (zeros | ones_digit)).asBool

        (self >> u).asSInt + Mux(r, 1.S, 0.S)
      }

      override def >(t: SInt): Bool = self > t

      override def withWidthOf(t: SInt) = {
        if (self.getWidth >= t.getWidth)
          self(t.getWidth-1, 0).asSInt
        else {
          val sign_bits = t.getWidth - self.getWidth
          val sign = self(self.getWidth-1)
          Cat(Cat(Seq.fill(sign_bits)(sign)), self).asTypeOf(t)
        }
      }

      override def clippedToWidthOf(t: SInt): SInt = {
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt
      }

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)

      override def zero: SInt = 0.S
      override def identity: SInt = 1.S
      override def minimum: SInt = (-(1 << (self.getWidth-1))).S

      override def divider(denom_t: UInt, options: Int = 0): Option[(DecoupledIO[UInt], DecoupledIO[SInt])] = {
        // TODO this uses a floating point divider, but we should use an integer divider instead

        val input = Wire(Decoupled(denom_t.cloneType))
        val output = Wire(Decoupled(self.cloneType))

        // We translate our integer to floating-point form so that we can use the hardfloat divider
        val expWidth = log2Up(self.getWidth) + 1
        val sigWidth = self.getWidth

        def sin_to_float(x: SInt) = {
          val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
          in_to_rec_fn.io.signedIn := true.B
          in_to_rec_fn.io.in := x.asUInt
          in_to_rec_fn.io.roundingMode := consts.round_minMag // consts.round_near_maxMag
          in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

          in_to_rec_fn.io.out
        }

        def uin_to_float(x: UInt) = {
          val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
          in_to_rec_fn.io.signedIn := false.B
          in_to_rec_fn.io.in := x
          in_to_rec_fn.io.roundingMode := consts.round_minMag // consts.round_near_maxMag
          in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

          in_to_rec_fn.io.out
        }

        def float_to_in(x: UInt) = {
          val rec_fn_to_in = Module(new RecFNToIN(expWidth = expWidth, sigWidth, self.getWidth))
          rec_fn_to_in.io.signedOut := true.B
          rec_fn_to_in.io.in := x
          rec_fn_to_in.io.roundingMode := consts.round_minMag // consts.round_near_maxMag

          rec_fn_to_in.io.out.asSInt
        }

        val self_rec = sin_to_float(self)
        val denom_rec = uin_to_float(input.bits)

        // Instantiate the hardloat divider
        val divider = Module(new DivSqrtRecFN_small(expWidth, sigWidth, options))

        input.ready := divider.io.inReady
        divider.io.inValid := input.valid
        divider.io.sqrtOp := false.B
        divider.io.a := self_rec
        divider.io.b := denom_rec
        divider.io.roundingMode := consts.round_minMag
        divider.io.detectTininess := consts.tininess_afterRounding

        output.valid := divider.io.outValid_div
        output.bits := float_to_in(divider.io.out)

        assert(!output.valid || output.ready)

        Some((input, output))
      }

      override def sqrt: Option[(DecoupledIO[UInt], DecoupledIO[SInt])] = {
        // TODO this uses a floating point divider, but we should use an integer divider instead

        val input = Wire(Decoupled(UInt(0.W)))
        val output = Wire(Decoupled(self.cloneType))

        input.bits := DontCare

        // We translate our integer to floating-point form so that we can use the hardfloat divider
        val expWidth = log2Up(self.getWidth) + 1
        val sigWidth = self.getWidth

        def in_to_float(x: SInt) = {
          val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
          in_to_rec_fn.io.signedIn := true.B
          in_to_rec_fn.io.in := x.asUInt
          in_to_rec_fn.io.roundingMode := consts.round_minMag // consts.round_near_maxMag
          in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

          in_to_rec_fn.io.out
        }

        def float_to_in(x: UInt) = {
          val rec_fn_to_in = Module(new RecFNToIN(expWidth = expWidth, sigWidth, self.getWidth))
          rec_fn_to_in.io.signedOut := true.B
          rec_fn_to_in.io.in := x
          rec_fn_to_in.io.roundingMode := consts.round_minMag // consts.round_near_maxMag

          rec_fn_to_in.io.out.asSInt
        }

        val self_rec = in_to_float(self)

        // Instantiate the hardloat sqrt
        val sqrter = Module(new DivSqrtRecFN_small(expWidth, sigWidth, 0))

        input.ready := sqrter.io.inReady
        sqrter.io.inValid := input.valid
        sqrter.io.sqrtOp := true.B
        sqrter.io.a := self_rec
        sqrter.io.b := DontCare
        sqrter.io.roundingMode := consts.round_minMag
        sqrter.io.detectTininess := consts.tininess_afterRounding

        output.valid := sqrter.io.outValid_sqrt
        output.bits := float_to_in(sqrter.io.out)

        assert(!output.valid || output.ready)

        Some((input, output))
      }

      override def reciprocal[U <: Data](u: U, options: Int = 0): Option[(DecoupledIO[UInt], DecoupledIO[U])] = u match {
        case Float(expWidth, sigWidth, false) =>
          val input = Wire(Decoupled(UInt(0.W)))
          val output = Wire(Decoupled(u.cloneType))

          input.bits := DontCare

          // We translate our integer to floating-point form so that we can use the hardfloat divider
          def in_to_float(x: SInt) = {
            val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
            in_to_rec_fn.io.signedIn := true.B
            in_to_rec_fn.io.in := x.asUInt
            in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
            in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

            in_to_rec_fn.io.out
          }

          val self_rec = in_to_float(self)
          val one_rec = in_to_float(1.S)

          // Instantiate the hardloat divider
          val divider = Module(new DivSqrtRecFN_small(expWidth, sigWidth, options))

          input.ready := divider.io.inReady
          divider.io.inValid := input.valid
          divider.io.sqrtOp := false.B
          divider.io.a := one_rec
          divider.io.b := self_rec
          divider.io.roundingMode := consts.round_near_even
          divider.io.detectTininess := consts.tininess_afterRounding

          output.valid := divider.io.outValid_div
          output.bits := fNFromRecFN(expWidth, sigWidth, divider.io.out).asTypeOf(u)

          assert(!output.valid || output.ready)

          Some((input, output))

        case _ => None
      }

      override def mult_with_reciprocal[U <: Data](reciprocal: U): SInt = reciprocal match {
        case recip @ Float(expWidth, sigWidth, false) =>
          def in_to_float(x: SInt) = {
            val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
            in_to_rec_fn.io.signedIn := true.B
            in_to_rec_fn.io.in := x.asUInt
            in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
            in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

            in_to_rec_fn.io.out
          }

          def float_to_in(x: UInt) = {
            val rec_fn_to_in = Module(new RecFNToIN(expWidth = expWidth, sigWidth, self.getWidth))
            rec_fn_to_in.io.signedOut := true.B
            rec_fn_to_in.io.in := x
            rec_fn_to_in.io.roundingMode := consts.round_minMag

            rec_fn_to_in.io.out.asSInt
          }

          val self_rec = in_to_float(self)
          val reciprocal_rec = recFNFromFN(expWidth, sigWidth, recip.bits)

          // Instantiate the hardloat divider
          val muladder = Module(new MulRecFN(expWidth, sigWidth))
          muladder.io.roundingMode := consts.round_near_even
          muladder.io.detectTininess := consts.tininess_afterRounding

          muladder.io.a := self_rec
          muladder.io.b := reciprocal_rec

          float_to_in(muladder.io.out)

        case _ => self
      }
    }
  }

  implicit object FloatArithmetic extends Arithmetic[Float] {
    // TODO Floating point arithmetic currently switches between recoded and standard formats for every operation. However, it should stay in the recoded format as it travels through the systolic array

    override implicit def cast(self: Float): ArithmeticOps[Float] = new ArithmeticOps(self) {
      override def *(t: Float): Float = {
        val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val t_resizer =  Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        val muladder = Module(new MulRecFN(self.expWidth, self.sigWidth))

        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := t_rec_resized

        val out = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        out.bits := (if (out.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        out
      }

      override def mac(m1: Float, m2: Float): Float = {
        // Recode all operands
        val m1_rec = if (m1.isRecoded) m1.bits else recFNFromFN(m1.expWidth, m1.sigWidth, m1.bits)
        val m2_rec = if (m2.isRecoded) m2.bits else recFNFromFN(m2.expWidth, m2.sigWidth, m2.bits)
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Resize m1 to self's width
        val m1_resizer = Module(new RecFNToRecFN(m1.expWidth, m1.sigWidth, self.expWidth, self.sigWidth))
        m1_resizer.io.in := m1_rec
        m1_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        m1_resizer.io.detectTininess := consts.tininess_afterRounding
        val m1_rec_resized = m1_resizer.io.out

        // Resize m2 to self's width
        val m2_resizer = Module(new RecFNToRecFN(m2.expWidth, m2.sigWidth, self.expWidth, self.sigWidth))
        m2_resizer.io.in := m2_rec
        m2_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        m2_resizer.io.detectTininess := consts.tininess_afterRounding
        val m2_rec_resized = m2_resizer.io.out

        // Perform multiply-add
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := m1_rec_resized
        muladder.io.b := m2_rec_resized
        muladder.io.c := self_rec

        // Convert result to standard format // TODO remove these intermediate recodings
        val out = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        out.bits := (if (out.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        out
      }

      override def +(t: Float): Float = {
        require(self.getWidth >= t.getWidth) // This just makes it easier to write the resizing code

        // Recode all operands
        val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Generate 1 as a float
        val in_to_rec_fn = Module(new INToRecFN(1, self.expWidth, self.sigWidth))
        in_to_rec_fn.io.signedIn := false.B
        in_to_rec_fn.io.in := 1.U
        in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val one_rec = in_to_rec_fn.io.out

        // Resize t
        val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        // Perform addition
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := t_rec_resized
        muladder.io.b := one_rec
        muladder.io.c := self_rec

        val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        result.bits := (if (result.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        result
      }

      override def -(t: Float): Float = {
        val t_sgn = t.bits(t.getWidth-1)
        val neg_t = Cat(~t_sgn, t.bits(t.getWidth-2,0)).asTypeOf(t)
        self + neg_t
      }

      override def >>(u: UInt): Float = {
        // Recode self
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Get 2^(-u) as a recoded float
        val shift_exp = Wire(UInt(self.expWidth.W))
        shift_exp := self.bias.U - u
        val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

        assert(shift_exp =/= 0.U, "scaling by denormalized numbers is not currently supported")

        // Multiply self and 2^(-u)
        val muladder = Module(new MulRecFN(self.expWidth, self.sigWidth))

        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := shift_rec

        val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        result.bits := (if (result.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        result
      }

      override def >(t: Float): Bool = {
        // Recode all operands
        val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Resize t to self's width
        val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        val comparator = Module(new CompareRecFN(self.expWidth, self.sigWidth))
        comparator.io.a := self_rec
        comparator.io.b := t_rec_resized
        comparator.io.signaling := false.B

        comparator.io.gt
      }

      override def withWidthOf(t: Float): Float = {
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth, t.isRecoded))
        result.bits := (if (result.isRecoded) resizer.io.out else fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out))
        result
      }

      override def clippedToWidthOf(t: Float): Float = {
        // TODO check for overflow. Right now, we just assume that overflow doesn't happen
        val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth, t.isRecoded))
        result.bits := (if (result.isRecoded) resizer.io.out else fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out))
        result
      }

      override def relu: Float = {
        val raw = if (self.isRecoded) rawFloatFromRecFN(self.expWidth, self.sigWidth, self.bits) else rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        result.bits := Mux(!raw.isZero && raw.sign, 0.U, self.bits)
        result
      }

      override def zero: Float = 0.U.asTypeOf(self)
      override def identity: Float = {
        require(!self.isRecoded)
        Cat(0.U(2.W), ~(0.U((self.expWidth-1).W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      }
      override def minimum: Float = {
        require(!self.isRecoded)
        Cat(1.U, ~(0.U(self.expWidth.W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      }
    }
  }

  implicit object DummySIntArithmetic extends Arithmetic[DummySInt] {
    override implicit def cast(self: DummySInt) = new ArithmeticOps(self) {
      override def *(t: DummySInt) = self.dontCare
      override def mac(m1: DummySInt, m2: DummySInt) = self.dontCare
      override def +(t: DummySInt) = self.dontCare
      override def -(t: DummySInt) = self.dontCare
      override def >>(t: UInt) = self.dontCare
      override def >(t: DummySInt): Bool = false.B
      override def identity = self.dontCare
      override def withWidthOf(t: DummySInt) = self.dontCare
      override def clippedToWidthOf(t: DummySInt) = self.dontCare
      override def relu = self.dontCare
      override def zero = self.dontCare
      override def minimum: DummySInt = self.dontCare
    }
  }

  implicit object MxFloatArithmetic extends Arithmetic[MxFloat] {
    override implicit def cast(self: MxFloat): ArithmeticOps[MxFloat] = new ArithmeticOps(self) {

      // TODO: fix the inputs to the multiplier and mac modules
      override def *(t: MxFloat): MxFloat = {
        require(!self.isRecoded && !t.isRecoded)
        val multiplier = Module(new mxHardware.MxFpMul(lut = false))
        val result = Wire(MxFloat(multiplier.ts.cType.exp, multiplier.ts.cType.sig, 4, true))

        val typeA = Wire(new MxTypes)
        typeA.exp := self.expWidth.U
        typeA.sig := self.sigWidth.U

        val typeW = Wire(new MxTypes)
        typeW.exp := t.expWidth.U
        typeW.sig := t.sigWidth.U

        val mode = Wire(new mxHardware.mxMode)
        mode.actWidth := self.expWidth.U
        mode.weiWidth := t.expWidth.U
        mode.actInputs := self.count.U
        mode.weiInputs := t.count.U
        mode.numOutputs := t.count.U
        mode.shift(0)(0) := 0.U
        mode.shift(1)(0) := 0.U
        mode.shift(0)(1) := 0.U
        mode.shift(1)(1) := 0.U

        multiplier.io.in_activation := self.bits(self.expWidth + self.sigWidth-1, 0)
        multiplier.io.type_a := typeA
        multiplier.io.mode := mode
        multiplier.io.in_weights := t.bits(t.expWidth + t.sigWidth-1, 0)
        multiplier.io.type_w := typeW
        multiplier.io.enable := true.B  // TODO：do we need an enable signal here?
        result := multiplier.io.out.asTypeOf(self)
        result
      }

      override def mac(m1: MxFloat, m2: MxFloat): MxFloat = {
        require(!m1.isRecoded && !m2.isRecoded) // mxFloat inputs must be in standard format
        val macc = Module(new mxHardware.MxFpMul(lut = false))
        val result = Wire(MxFloat(macc.ts.cType.exp, macc.ts.cType.sig, 4, true))

        val typeA = Wire(new MxTypes)
        typeA.exp := m1.expWidth.U
        typeA.sig := m1.sigWidth.U

        val typeW = Wire(new MxTypes)
        typeW.exp := m2.expWidth.U
        typeW.sig := m2.sigWidth.U

        val mode = Wire(new mxHardware.mxMode)
        mode.actWidth := m1.expWidth.U
        mode.weiWidth := m2.expWidth.U
        mode.actInputs := m1.count.U
        mode.weiInputs := m2.count.U
        mode.numOutputs := m2.count.U
        mode.shift(0)(0) := 0.U
        mode.shift(1)(0) := 0.U
        mode.shift(0)(1) := 0.U
        mode.shift(1)(1) := 0.U

        val rec_c = if (self.isRecoded) self.bits else VecInit(self.bits.asTypeOf(Vec(4, UInt((self.expWidth + self.sigWidth).W))).map(f => recFNFromFN(self.expWidth, self.sigWidth, f))).asUInt

        macc.io.in_activation := m1.bits((m1.count)*(m1.expWidth + m1.sigWidth) - 1, 0)
        macc.io.type_a := typeA
        macc.io.mode := mode
        macc.io.in_weights := m2.bits((m2.count)*(m2.expWidth + m2.sigWidth) - 1, 0)
        macc.io.type_w := typeW
        macc.io.enable := true.B  // TODO：do we need an enable signal here?
        macc.io.rec_c := rec_c
        result := macc.io.out.asTypeOf(self)
        result
      }

      // TODO: Replace placeholder arithmetic
      // Currently just returns self.bits to get things to compile


      override def +(t: MxFloat): MxFloat = {
        self
        // val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)

        // // Recode all operands
        // val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        // val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // // Generate 1 as a float
        // val in_to_rec_fn = Module(new INToRecFN(1, self.expWidth, self.sigWidth))
        // in_to_rec_fn.io.signedIn := false.B
        // in_to_rec_fn.io.in := 1.U
        // in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        // val one_rec = in_to_rec_fn.io.out

        // // Resize t
        // val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        // t_resizer.io.in := t_rec
        // t_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // t_resizer.io.detectTininess := consts.tininess_afterRounding
        // val t_rec_resized = t_resizer.io.out

        // // Perform addition
        // val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        // muladder.io.op := 0.U
        // muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // muladder.io.detectTininess := consts.tininess_afterRounding

        // muladder.io.a := t_rec_resized
        // muladder.io.b := one_rec
        // muladder.io.c := self_rec

        // val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        // result.bits := (if (result.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        // result
      }

      override def -(t: MxFloat): MxFloat = {
        self
        // val t_sgn = t.bits(t.getWidth-1)
        // val neg_t = Cat(~t_sgn, t.bits(t.getWidth-2,0)).asTypeOf(t)
        // self + neg_t
      }

      override def >>(u: UInt): MxFloat = {
        self
        // Recode self
        // val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // // Get 2^(-u) as a recoded float
        // val shift_exp = Wire(UInt(self.expWidth.W))
        // shift_exp := self.bias.U - u
        // val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        // val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

        // assert(shift_exp =/= 0.U, "scaling by denormalized numbers is not currently supported")

        // // Multiply self and 2^(-u)
        // val muladder = Module(new MulRecFN(self.expWidth, self.sigWidth))

        // muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // muladder.io.detectTininess := consts.tininess_afterRounding

        // muladder.io.a := self_rec
        // muladder.io.b := shift_rec

        // val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        // result.bits := (if (result.isRecoded) muladder.io.out else fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out))
        // result
      }

      override def >(t: MxFloat): Bool = {
        false.B
        // Recode all operands
        // val t_rec = if (t.isRecoded) t.bits else recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        // val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // // Resize t to self's width
        // val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        // t_resizer.io.in := t_rec
        // t_resizer.io.roundingMode := consts.round_near_even
        // t_resizer.io.detectTininess := consts.tininess_afterRounding
        // val t_rec_resized = t_resizer.io.out

        // val comparator = Module(new CompareRecFN(self.expWidth, self.sigWidth))
        // comparator.io.a := self_rec
        // comparator.io.b := t_rec_resized
        // comparator.io.signaling := false.B

        // comparator.io.gt
      }

      override def withWidthOf(t: MxFloat): MxFloat = {
        if ((self.isRecoded && !t.isRecoded) || (!self.isRecoded && t.isRecoded)) { 
          val result = Wire(MxFloat(t.expWidth, t.sigWidth, t.count, t.isRecoded))
          val elems = Wire(Vec(t.count, UInt((t.expWidth + t.sigWidth + (if (t.isRecoded) 1 else 0)).W)))
          val input = self.bits.asTypeOf(Vec(self.count, UInt((self.expWidth + self.sigWidth + (if (self.isRecoded) 1 else 0)).W)))

          for (i <- 0 until t.count) {
            val elem = input(i)
            val self_rec = if (self.isRecoded) elem else recFNFromFN(self.expWidth, self.sigWidth, elem)

            val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
            resizer.io.in := self_rec
            resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
            resizer.io.detectTininess := consts.tininess_afterRounding

            elems(i) := (if (result.isRecoded) resizer.io.out else fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out))
          }
          result := elems.asTypeOf(result)
          result
        } else {
          self
        }
 
      }


      //override def withWidthOf(t: MxFloat): MxFloat = {
       // self
        // val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        // resizer.io.in := self_rec
        // resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // resizer.io.detectTininess := consts.tininess_afterRounding

        // val result = Wire(Float(t.expWidth, t.sigWidth, t.isRecoded))
        // result.bits := (if (result.isRecoded) resizer.io.out else fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out))
        // result
      // }

      override def clippedToWidthOf(t: MxFloat): MxFloat = {
        self
        // TODO check for overflow. Right now, we just assume that overflow doesn't happen
        // val self_rec = if (self.isRecoded) self.bits else recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        // resizer.io.in := self_rec
        // resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        // resizer.io.detectTininess := consts.tininess_afterRounding

        // val result = Wire(Float(t.expWidth, t.sigWidth, t.isRecoded))
        // result.bits := (if (result.isRecoded) resizer.io.out else fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out))
        // result
      }

      override def relu: MxFloat = {
        self
        // val raw = if (self.isRecoded) rawFloatFromRecFN(self.expWidth, self.sigWidth, self.bits) else rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        // val result = Wire(Float(self.expWidth, self.sigWidth, self.isRecoded))
        // result.bits := Mux(!raw.isZero && raw.sign, 0.U, self.bits)
        // result
      }

      override def zero: MxFloat = 0.U.asTypeOf(self)
      override def identity: MxFloat = {
        self
        // require(!self.isRecoded)
        // Cat(0.U(2.W), ~(0.U((self.expWidth-1).W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      }
      override def minimum: MxFloat = {
        self
        // require(!self.isRecoded)
        // Cat(1.U, ~(0.U(self.expWidth.W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      }
      
    }

  }
 

  // implicit class MxFloatArithmetic(mxParams: MxParams) extends Arithmetic[MxFloat] {
  //   override implicit def cast(self: MxFloat): ArithmeticOps[MxFloat] = new ArithmeticOps(self) {
      
  //     override def *(t: MxFloat): MxFloat = {
  //       val result = Wire(new MxFloat(self.mxParams, self.modeId)) 
  //       val mode = self.currentMode
  //       result.bits := mode.outTotalWidth.U // Placeholder
  //       result
  //     }

  //     override def mac(m1: MxFloat, m2: MxFloat): MxFloat = {
  //       // Perform multiply
  //       val mul = Module(new MxFpMul(supportedTypes: TypeSupport, lut: false))
        
  //       mul.io.in_activation = m1.bits
  //       mul.io.in_a_type = 
  //       mul.io.a_altfmt = 
  //       mul.io.in_weights = 
  //       mul.io.in_w_type =
  //       mul.io.w_altfmt = 
  //       mul.io.enable = true.B  // TODO：do we need an enable signal here?
  //       val mul_result :=  cat(mul.io.out_s, mul.io.out_exp) //
        
  //       if (mode.numOutputs == 1) {
    
  //         val mul_resizer = Module(new RawFPToRecFN(
  //         inExpWidth = getMultResultExpWidth(mode),
  //         inSigWidth = getMultResultSigWidth(mode), 
  //         outExpWidth = self.Width_act,
  //         outSigWidth = self.Width_w))
    
  //         mul_resizer.io.in := mul_result
  //         mul_resizer.io.roundingMode := consts.round_near_even
  //         mul_resizer.io.detectTininess := consts.tininess_afterRounding
  //         result.bits := mul_resizer.io.out
    
  //       } else {
 
  //         val outputWidth = mode.outTotalWidth / mode.numOutputs
  //         val selfSliceWidth = if (self.numOutputs == 1) {
  //           self.bits.getWidth
  //         } else {
  //           self.bits(0).getWidth  
  //         }
  //         val mul_resizers = Seq.tabulate(mode.numOutputs) { i =>
  //         Module(new RawFPToRecFN(
  //         inExpWidth = getMultResultExpWidth(mode, outputWidth),
  //         inSigWidth = getMultResultSigWidth(mode, outputWidth),
  //         outExpWidth = getSelfSliceExpWidth(selfSliceWidth),
  //         outSigWidth = getSelfSliceSigWidth(selfSliceWidth)
  //         ))
  //       }

  //       for (i <- 0 until mode.numOutputs) {
  //         val startBit = i * outputWidth
  //         val endBit = startBit + outputWidth - 1
  //         val sliced_mul_result = mul_result(endBit, startBit)
  //         val self_slice = getSlice(i % self.numOutputs) 
      
  //         mul_resizers(i).io.in := sliced_mul_result
  //         mul_resizers(i).io.roundingMode := consts.round_near_even
  //         mul_resizers(i).io.detectTininess := consts.tininess_afterRounding

  //         result.bits(i) := mul_resizers(i).io.out
  //         }
  //       }
  
  //       result
  //     }
      
  //   }

  // }

}

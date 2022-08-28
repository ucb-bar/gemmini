// A simple type class for Chisel datatypes that can add and multiply. To add your own type, simply create your own:
//     implicit MyTypeArithmetic extends Arithmetic[MyType] { ... }

package gemmini

import chisel3._
import chisel3.util._
import hardfloat._

// Bundles that represent the raw bits of custom datatypes
case class Float(expWidth: Int, sigWidth: Int) extends Bundle {
  val bits = UInt((expWidth + sigWidth).W)

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

class Complex(val w: Int) extends Bundle {
  val real = SInt(w.W)
  val imag = SInt(w.W)
}

object Complex {
  def apply(w: Int, real: SInt, imag: SInt): Complex = {
    val result = Wire(new Complex(w))
    result.real := real
    result.imag := imag
    result
  }
}

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
  def divider(denom_t: UInt): Option[(DecoupledIO[UInt], DecoupledIO[T])] = None
  def sqrt: Option[(DecoupledIO[UInt], DecoupledIO[T])] = None
  def reciprocal[U <: Data](u: U): Option[(DecoupledIO[UInt], DecoupledIO[U])] = None
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
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = point_five & (zeros | ones_digit)

        (self >> u).asUInt() + r
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
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = (point_five & (zeros | ones_digit)).asBool()

        (self >> u).asSInt() + Mux(r, 1.S, 0.S)
      }

      override def >(t: SInt): Bool = self > t

      override def withWidthOf(t: SInt) = {
        if (self.getWidth >= t.getWidth)
          self(t.getWidth-1, 0).asSInt()
        else {
          val sign_bits = t.getWidth - self.getWidth
          val sign = self(self.getWidth-1)
          Cat(Cat(Seq.fill(sign_bits)(sign)), self).asTypeOf(t)
        }
      }

      override def clippedToWidthOf(t: SInt): SInt = {
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
      }

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)

      override def zero: SInt = 0.S
      override def identity: SInt = 1.S
      override def minimum: SInt = (-(1 << (self.getWidth-1))).S

      override def divider(denom_t: UInt): Option[(DecoupledIO[UInt], DecoupledIO[SInt])] = {
        // TODO this uses a floating point divider, but we should use an integer divider instead

        val input = Wire(Decoupled(denom_t.cloneType))
        val output = Wire(Decoupled(self.cloneType))

        // We translate our integer to floating-point form so that we can use the hardfloat divider
        val expWidth = log2Up(self.getWidth) + 1
        val sigWidth = self.getWidth

        def sin_to_float(x: SInt) = {
          val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
          in_to_rec_fn.io.signedIn := true.B
          in_to_rec_fn.io.in := x.asUInt()
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

          rec_fn_to_in.io.out.asSInt()
        }

        val self_rec = sin_to_float(self)
        val denom_rec = uin_to_float(input.bits)

        // Instantiate the hardloat divider
        val divider = Module(new DivSqrtRecFN_small(expWidth, sigWidth, 0))

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
          in_to_rec_fn.io.in := x.asUInt()
          in_to_rec_fn.io.roundingMode := consts.round_minMag // consts.round_near_maxMag
          in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

          in_to_rec_fn.io.out
        }

        def float_to_in(x: UInt) = {
          val rec_fn_to_in = Module(new RecFNToIN(expWidth = expWidth, sigWidth, self.getWidth))
          rec_fn_to_in.io.signedOut := true.B
          rec_fn_to_in.io.in := x
          rec_fn_to_in.io.roundingMode := consts.round_minMag // consts.round_near_maxMag

          rec_fn_to_in.io.out.asSInt()
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

      override def reciprocal[U <: Data](u: U): Option[(DecoupledIO[UInt], DecoupledIO[U])] = u match {
        case Float(expWidth, sigWidth) =>
          val input = Wire(Decoupled(UInt(0.W)))
          val output = Wire(Decoupled(u.cloneType))

          input.bits := DontCare

          // We translate our integer to floating-point form so that we can use the hardfloat divider
          def in_to_float(x: SInt) = {
            val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
            in_to_rec_fn.io.signedIn := true.B
            in_to_rec_fn.io.in := x.asUInt()
            in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
            in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

            in_to_rec_fn.io.out
          }

          val self_rec = in_to_float(self)
          val one_rec = in_to_float(1.S)

          // Instantiate the hardloat divider
          val divider = Module(new DivSqrtRecFN_small(expWidth, sigWidth, 0))

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
        case recip @ Float(expWidth, sigWidth) =>
          def in_to_float(x: SInt) = {
            val in_to_rec_fn = Module(new INToRecFN(intWidth = self.getWidth, expWidth, sigWidth))
            in_to_rec_fn.io.signedIn := true.B
            in_to_rec_fn.io.in := x.asUInt()
            in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
            in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

            in_to_rec_fn.io.out
          }

          def float_to_in(x: UInt) = {
            val rec_fn_to_in = Module(new RecFNToIN(expWidth = expWidth, sigWidth, self.getWidth))
            rec_fn_to_in.io.signedOut := true.B
            rec_fn_to_in.io.in := x
            rec_fn_to_in.io.roundingMode := consts.round_minMag

            rec_fn_to_in.io.out.asSInt()
          }

          val self_rec = in_to_float(self)
          val reciprocal_rec = recFNFromFN(expWidth, sigWidth, recip.bits)

          // Instantiate the hardloat divider
          val muladder = Module(new MulAddRecFN(expWidth, sigWidth))
          muladder.io.op := 0.U
          muladder.io.roundingMode := consts.round_near_even
          muladder.io.detectTininess := consts.tininess_afterRounding

          muladder.io.a := self_rec
          muladder.io.b := reciprocal_rec
          muladder.io.c := 0.U

          float_to_in(muladder.io.out)

        case _ => self
      }
    }
  }

  implicit object FloatArithmetic extends Arithmetic[Float] {
    // TODO Floating point arithmetic currently switches between recoded and standard formats for every operation. However, it should stay in the recoded format as it travels through the systolic array

    override implicit def cast(self: Float): ArithmeticOps[Float] = new ArithmeticOps(self) {
      override def *(t: Float): Float = {
        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val t_resizer =  Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := t_rec_resized
        muladder.io.c := 0.U

        val out = Wire(Float(self.expWidth, self.sigWidth))
        out.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        out
      }

      override def mac(m1: Float, m2: Float): Float = {
        // Recode all operands
        val m1_rec = recFNFromFN(m1.expWidth, m1.sigWidth, m1.bits)
        val m2_rec = recFNFromFN(m2.expWidth, m2.sigWidth, m2.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

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
        val out = Wire(Float(self.expWidth, self.sigWidth))
        out.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        out
      }

      override def +(t: Float): Float = {
        require(self.getWidth >= t.getWidth) // This just makes it easier to write the resizing code

        // Recode all operands
        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

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

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        result
      }

      override def -(t: Float): Float = {
        val t_sgn = t.bits(t.getWidth-1)
        val neg_t = Cat(~t_sgn, t.bits(t.getWidth-2,0)).asTypeOf(t)
        self + neg_t
      }

      override def >>(u: UInt): Float = {
        // Recode self
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Get 2^(-u) as a recoded float
        val shift_exp = Wire(UInt(self.expWidth.W))
        shift_exp := self.bias.U - u
        val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

        assert(shift_exp =/= 0.U, "scaling by denormalized numbers is not currently supported")

        // Multiply self and 2^(-u)
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := shift_rec
        muladder.io.c := 0.U

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        result
      }

      override def >(t: Float): Bool = {
        // Recode all operands
        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

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
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth))
        result.bits := fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out)
        result
      }

      override def clippedToWidthOf(t: Float): Float = {
        // TODO check for overflow. Right now, we just assume that overflow doesn't happen
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth))
        result.bits := fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out)
        result
      }

      override def relu: Float = {
        val raw = rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := Mux(!raw.isZero && raw.sign, 0.U, self.bits)
        result
      }

      override def zero: Float = 0.U.asTypeOf(self)
      override def identity: Float = Cat(0.U(2.W), ~(0.U((self.expWidth-1).W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      override def minimum: Float = Cat(1.U, ~(0.U(self.expWidth.W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
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

  implicit object ComplexArithmetic extends Arithmetic[Complex] {
    override implicit def cast(self: Complex) = new ArithmeticOps(self) {
      override def *(other: Complex): Complex = {
        val w = self.w max other.w

        Complex(w,
          self.real * other.real - self.imag * other.imag,
          self.real * other.imag + self.imag * other.real
        )
      }

      override def +(other: Complex): Complex = {
        /*
        TUTORIAL:
          Implement the addition operator over here.
         */
        val w = self.w max other.w

        Complex(w,
          self.real + other.real,
          self.imag + other.imag,
        )
      }

      def mac(m1: Complex, m2: Complex): Complex = {
        /*
        TUTORIAL:
          Implement the multiply-accumulate operation (self + m1 * m2) over here.
         */
        self + m1 * m2
      }

      override def zero = Complex(self.w, 0.S, 0.S)
      override def identity: Complex = self
      override def withWidthOf(other: Complex) = Complex(other.w, self.real, self.imag)

      def clippedToWidthOf(other: Complex): Complex = {
        // Like "withWidthOf", except that it saturates
        val maxsat = ((1 << (other.w - 1)) - 1).S
        val minsat = (-(1 << (other.w - 1))).S

        Complex(other.w,
          Mux(self.real > maxsat, maxsat, Mux(self.real < minsat, minsat, self.real)),
          Mux(self.imag > maxsat, maxsat, Mux(self.imag < minsat, minsat, self.imag)),
        )
      }

      // Not implemented because not necessary for this tutorial
      override def >>(u: UInt) = self
      override def >(t: Complex) = false.B
      override def relu = self
      override def -(other: Complex): Complex = self
      override def minimum: Complex = 0.U.asTypeOf(self)
    }
  }
}

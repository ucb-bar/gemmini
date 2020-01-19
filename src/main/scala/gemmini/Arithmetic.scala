// A simple type class for Chisel datatypes that can add and multiply. To add your own type, simply create your own:
//     implicit MyTypeArithmetic extends Arithmetic[MyType] { ... }

package gemmini

import chisel3._
import chisel3.util._
import hardfloat._

abstract class Arithmetic[T <: Data] {
  implicit def cast(t: T): ArithmeticOps[T]
}

abstract class ArithmeticOps[T <: Data](self: T) {
  // def *(t: T): T
  def mac(m1: T, m2: T): T // Returns (m1 * m2 + self)
  def +(t: T): T
  def >>(u: UInt): T // This is a rounding shift! Rounds away from 0
  def withWidthOf(t: T): T
  def clippedToWidthOf(t: T): T // Like "withWidthOf", except that it saturates
  def relu: T
  def relu6(shift: UInt): T
}

object Arithmetic {
  implicit object UIntArithmetic extends Arithmetic[UInt] {
    override implicit def cast(self: UInt) = new ArithmeticOps(self) {
      // override def *(t: UInt) = self * t
      override def mac(m1: UInt, m2: UInt) = m1 * m2 + self
      override def +(t: UInt) = self + t

      override def >>(u: UInt) = {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = point_five & (zeros | ones_digit)

        (self >> u).asUInt() + r
      }

      override def withWidthOf(t: UInt) = self(t.getWidth-1, 0)

      override def clippedToWidthOf(t: UInt) = {
        val sat = ((1 << (t.getWidth-1))-1).U
        Mux(self > sat, sat, self)(t.getWidth-1, 0)
      }

      override def relu: UInt = self
      override def relu6(shift: UInt): UInt = {
        val max6 = (6.U << shift).asUInt()
        val maxwidth = ((1 << (self.getWidth-1))-1).U
        val max = Mux(max6 > maxwidth, maxwidth, max6)(self.getWidth-1, 0).asUInt()
        Mux(self < max, self, max)
      }
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
    override implicit def cast(self: SInt) = new ArithmeticOps(self) {
      // override def *(t: SInt) = self * t
      override def mac(m1: SInt, m2: SInt) = m1 * m2 + self
      override def +(t: SInt) = self + t

      override def >>(u: UInt) = {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = (point_five & (zeros | ones_digit)).asBool()

        (self >> u).asSInt() + Mux(r, 1.S, 0.S)
      }

      override def withWidthOf(t: SInt) = self(t.getWidth-1, 0).asSInt()

      override def clippedToWidthOf(t: SInt): SInt = {
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
      }

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)
      override def relu6(shift: UInt): SInt = {
        val max6 = (6.S << shift).asSInt()
        val maxwidth = ((1 << (self.getWidth-1))-1).S
        val max = Mux(max6 > maxwidth, maxwidth, max6)(self.getWidth-1, 0).asSInt()
        MuxCase(self, Seq((self < 0.S) -> 0.S, (self > max) -> max))
      }
    }
  }

  implicit object FloatArithmetic extends Arithmetic[Float] {
    override implicit def cast(self: Float): ArithmeticOps[Float] = new ArithmeticOps(self) {
      override def mac(m1: Float, m2: Float): Float = {
        val m1_rec = recFNFromFN(self.expWidth, self.sigWidth, m1.bits)
        val m2_rec = recFNFromFN(self.expWidth, self.sigWidth, m2.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := m1_rec
        muladder.io.b := m2_rec
        muladder.io.c := self_rec

        val out = Wire(Float(self.expWidth, self.sigWidth))
        out.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        out
      }

      override def +(t: Float): Float = {
        val t_rec = recFNFromFN(self.expWidth, self.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val in_to_rec_fn = Module(new INToRecFN(1, self.expWidth, self.sigWidth))
        in_to_rec_fn.io.signedIn := false.B
        in_to_rec_fn.io.in := 1.U
        in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val one_rec = in_to_rec_fn.io.out

        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := t_rec
        muladder.io.b := one_rec
        muladder.io.c := self_rec

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        result
      }

      override def >>(u: UInt): Float = {
        // Recode self
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        // Get 2^(-u) as a recoded float
        val shift_exp = Wire(UInt(self.expWidth.W))
        shift_exp := self.bias.U - u
        val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

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

        /*
        val raw = rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        val shifted_raw = WireInit(raw)

        when (!raw.isZero) {
          shifted_raw.sExp := raw.sExp - u.asSInt()
        }

        val raw_to_rec_fn_converter = Module(new RoundRawFNToRecFN(self.expWidth, self.sigWidth, options = 0)) // TODO add correct options here so that efficiency may be improved

        raw_to_rec_fn_converter.io.invalidExc := false.B
        raw_to_rec_fn_converter.io.infiniteExc := false.B

        raw_to_rec_fn_converter.io.in := shifted_raw

        raw_to_rec_fn_converter.io.roundingMode := consts.round_near_maxMag
        raw_to_rec_fn_converter.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, raw_to_rec_fn_converter.io.out)
        result
        */
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

      override def relu6(shift: UInt): Float = {
        // Get a constant 6 as a float
        val in_to_rec_fn = Module(new INToRecFN(log2Up(6+1), self.expWidth, self.sigWidth))
        in_to_rec_fn.io.signedIn := false.B
        in_to_rec_fn.io.in := 6.U
        in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val six_rec = in_to_rec_fn.io.out

        // Get 2^shift as a float
        val shift_exp = Wire(UInt(self.expWidth.W))
        shift_exp := self.bias.U + shift
        val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

        // Get 6*(2^shift) as a float
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := six_rec
        muladder.io.b := shift_rec
        muladder.io.c := 0.U

        val shifted_rec = muladder.io.out

        /*
        val six_raw = rawFloatFromIN(signedIn = false.B, in = 6.U(3.W))

        val shifted_raw = WireInit(six_raw)

        when (!six_raw.isZero) {
          shifted_raw.sExp := six_raw.sExp + shift.asSInt()
        }

        val raw_to_rec_fn_converter = Module(new RoundRawFNToRecFN(self.expWidth, self.sigWidth, options = 0)) // TODO add correct options here so that efficiency may be improved
        raw_to_rec_fn_converter.io.in := shifted_raw
        raw_to_rec_fn_converter.io.roundingMode := consts.round_near_maxMag
        raw_to_rec_fn_converter.io.detectTininess := consts.tininess_afterRounding
        raw_to_rec_fn_converter.io.invalidExc := false.B
        raw_to_rec_fn_converter.io.infiniteExc := false.B

        val shifted_rec = raw_to_rec_fn_converter.io.out
        */

        // Now, compare self and 6*(2^shift) to calculate the activation function
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)
        val self_raw = rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        val comparer = Module(new CompareRecFN(self.expWidth, self.sigWidth))
        comparer.io.a := self_rec
        comparer.io.b := shifted_rec
        comparer.io.signaling := false.B

        val larger_than_six = comparer.io.gt

        val result_rec = Mux(!self_raw.isZero && self_raw.sign, 0.U,
          Mux(larger_than_six, shifted_rec, self_rec))

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, self_rec)
        result
      }
    }
  }
}

case class Float(expWidth: Int, sigWidth: Int) extends Bundle {
  val bits = UInt((expWidth + sigWidth).W)

  val bias: Int = (1 << (expWidth-1)) - 1
}

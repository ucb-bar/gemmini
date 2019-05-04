// A simple type class for Chisel datatypes that can add and multiply. To add your own type, simply creat your own:
//     implicit MyTypeArithmetic extends Arithmetic[MyType] { ... }

package systolic

import chisel3._
import chisel3.util._

abstract class Arithmetic[T <: Data] {
  implicit def cast(t: T): ArithmeticOps[T]
}

abstract class ArithmeticOps[T <: Data](self: T) {
  def +(t: T): T
  def *(t: T): T
  def >>(u: UInt): T // This is a rounding shift! Rounds away from 0
  def withWidthOf(t: T): T
  def clippedToWidthOf(t: T): T
  def relu: T
  def relu6: T
}

object Arithmetic {
  implicit object UIntArithmetic extends Arithmetic[UInt] {
    implicit def cast(self: UInt) = new ArithmeticOps(self) {
      def +(t: UInt) = self + t
      def *(t: UInt) = self * t

      def >>(u: UInt) = {
        Mux(u === 0.U, self, (self + (1.U << (u-1.U)).asUInt()) >> u).asUInt() // TODO is the mux necessary here? What is (1 << (0.U-1.U))?
      }

      def withWidthOf(t: UInt) = self(t.getWidth-1, 0)

      def clippedToWidthOf(t: UInt) = {
        // val sat = Cat(Seq.fill(t.getWidth)(1.U(1.W)))
        val sat = ((1 << (t.getWidth-1))-1).U
        Mux(self > sat, sat, self)(t.getWidth-1, 0)
      }

      override def relu: UInt = self
      override def relu6: UInt = Mux(self < 6.U, self, 6.U)
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
    implicit def cast(self: SInt) = new ArithmeticOps(self) {
      def +(t: SInt) = self + t
      def *(t: SInt) = self * t

      // TODO is there a more efficient way of doing this rounding shift?
      def >>(u: UInt) = {
        val abs = Mux(self >= 0.S, self, 0.S - self)
        val offset = (1.U << (u-1.U)).asUInt()
        val abs_result = Mux(u === 0.U, abs, (abs + offset.asSInt()) >> u).asSInt()
        Mux(self >= 0.S, abs_result, 0.S - abs_result)
      }

      def withWidthOf(t: SInt) = self(t.getWidth-1, 0).asSInt()

      override def clippedToWidthOf(t: SInt): SInt = {
        // val maxsat = Cat(0.U(1.W), Seq.fill(t.getWidth-1)(1.U(1.W)):_*).asSInt()
        // val minsat = Cat(1.U(1.W), Seq.fill(t.getWidth-1)(0.U(1.W)):_*).asSInt()
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
      }

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)
      override def relu6: SInt = MuxCase(self, Seq((self < 0.S) -> 0.S, (self > 6.S) -> 6.S))
    }
  }
}

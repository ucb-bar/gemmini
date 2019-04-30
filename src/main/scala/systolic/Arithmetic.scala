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
  def >>(u: UInt): T
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
      def >>(u: UInt) = (self >> u).asUInt()

      def withWidthOf(t: UInt) = self(t.getWidth-1, 0)

      def clippedToWidthOf(t: UInt) = {
        val sat = Cat(Seq.fill(t.getWidth)(1.U(1.W)))
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
      def >>(u: UInt) = (self >> u).asSInt()

      def withWidthOf(t: SInt) = self(t.getWidth-1, 0).asSInt()

      override def clippedToWidthOf(t: SInt): SInt = {
        val maxsat = Cat(0.U(1.W), Seq.fill(t.getWidth-1)(1.U(1.W)):_*).asSInt()
        val minsat = Cat(1.U(1.W), Seq.fill(t.getWidth-1)(0.U(1.W)):_*).asSInt()
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
      }

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)
      override def relu6: SInt = MuxCase(self, Seq((self < 0.S) -> 0.S, (self > 6.S) -> 6.S))
    }
  }
}

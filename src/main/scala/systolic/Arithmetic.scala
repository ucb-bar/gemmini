// A simple type class for Chisel datatypes that can add and multiply. To add your own type, simply creat your own:
//     implicit MyTypeArithmetic extends Arithmetic[MyType] { ... }

package systolic

import chisel3._

abstract class Arithmetic[T <: Data] {
  implicit def cast(t: T): ArithmeticOps[T]
}

abstract class ArithmeticOps[T <: Data](self: T) {
  def +(t: T): T
  def *(t: T): T
  def doubleWidth: T
}

object Arithmetic {
  implicit object UIntArithmetic extends Arithmetic[UInt] {
    implicit def cast(self: UInt) = new ArithmeticOps(self) {
      def +(t: UInt) = self + t
      def *(t: UInt) = self * t
      def doubleWidth = UInt((self.getWidth * 2).W)
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
    implicit def cast(self: SInt) = new ArithmeticOps(self) {
      def +(t: SInt) = self + t
      def *(t: SInt) = self * t
      def doubleWidth = SInt((self.getWidth * 2).W)
    }
  }
}

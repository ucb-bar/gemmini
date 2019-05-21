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
  def relu6(shift: UInt): T
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
      override def relu6(shift: UInt): UInt = {
        val max = (6.U << shift).asUInt()
        Mux(self < max, self, max)
      }
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
    implicit def cast(self: SInt) = new ArithmeticOps(self) {
      override def +(t: SInt) = self + t
      override def *(t: SInt) = self * t

      /*override def +(t: SInt) = {
        val adder = Module(new AddWrapper(self.getWidth, t.getWidth))
        adder.io.x := self
        adder.io.y := t
        adder.io.out
      }*/

      /*override def *(t: SInt) = {
        val mult = Module(new MultiplyWrapper(self.getWidth, t.getWidth))
        mult.io.x := self
        mult.io.y := t
        mult.io.out
      }*/

      override def >>(u: UInt) = {
        val pos_offset = (1.U << (u-1.U)).asUInt()
        val neg_offset = ~((-1).S << (u-1.U))
        val pos_sum = self + pos_offset.asSInt()
        val neg_sum = self + neg_offset.asSInt()
        Mux(u === 0.U, self,
            (Mux(self >= 0.S, pos_sum, neg_sum) >> u).asSInt)
      }

      /*override def >>(u: UInt) = {
        val shifter = Module(new ShiftWrapper(self.getWidth, u.getWidth))
        shifter.io.x := self
        shifter.io.y := u
        shifter.io.out
      }*/

      override def withWidthOf(t: SInt) = self(t.getWidth-1, 0).asSInt()

      override def clippedToWidthOf(t: SInt): SInt = {
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
      }

      /*override def clippedToWidthOf(t: SInt): SInt = {
        val clipper = Module(new ClippedWrapper(self.getWidth, t.getWidth))
        clipper.io.x := self
        clipper.io.out
      }*/

      override def relu: SInt = Mux(self >= 0.S, self, 0.S)
      override def relu6(shift: UInt): SInt = {
        val max = (6.S << shift).asSInt()
        MuxCase(self, Seq((self < 0.S) -> 0.S, (self > max) -> max))
      }
    }
  }
}

class MultiplyWrapper(w1: Int, w2: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(w1.W))
    val y = Input(SInt(w2.W))
    val out = Output(SInt((w1 + w2).W))
  })

  io.out := io.x * io.y
}

class AddWrapper(w1: Int, w2: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(w1.W))
    val y = Input(SInt(w2.W))
    val out = Output(SInt((w1 max w2).W))
  })

  io.out := io.x + io.y
}

class ShiftWrapper(w1: Int, w2: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(w1.W))
    val y = Input(UInt(w2.W))
    val out = Output(SInt(w1.W))
  })

  val u = io.y
  val self = io.x

  val pos_offset = (1.U << (u-1.U)).asUInt()
  val neg_offset = ~((-1).S << (u-1.U))
  val pos_sum = self + pos_offset.asSInt()
  val neg_sum = self + neg_offset.asSInt()
  io.out := Mux(u === 0.U, self,
    (Mux(self >= 0.S, pos_sum, neg_sum) >> u).asSInt)
}

class ClippedWrapper(w1: Int, w2: Int) extends Module {
  val io = IO(new Bundle {
    val x = Input(SInt(w1.W))
    val out = Output(SInt(w2.W))
  })

  val self = io.x

  val maxsat = ((1 << (w2-1))-1).S
  val minsat = (-(1 << (w2-1))).S
  io.out := MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(w2-1, 0).asSInt()
}

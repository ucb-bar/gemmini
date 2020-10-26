package gemmini

import chisel3._
import chisel3.util._
import Util._

class WeightedArbiter[T <: Data](t: T, weightA: Int) extends Module {
  val io = IO(new Bundle {
    val inA = Flipped(Decoupled(t))
    val inB = Flipped(Decoupled(t))
    val out = Decoupled(t)
  })

  val count = Reg(UInt(log2Up(weightA+1).W))
  val A_chosen = WireInit(false.B)
  val B_chosen = WireInit(false.B)

  io.inA.ready := false.B
  io.inB.ready := false.B

  when(io.inA.valid && io.inB.valid) {
    when (count < weightA.U) {
      io.out <> io.inA
      A_chosen := true.B
    }.otherwise {
      io.out <> io.inB
      B_chosen := true.B
    }
  }.elsewhen(io.inA.valid) {
    io.out <> io.inA
  }.otherwise {
    io.out <> io.inB
  }

  when (io.out.fire()) {
    when (A_chosen) {
      count := satAdd(count, 1.U, (weightA + 1).U)
    }.elsewhen(B_chosen) {
      count := 0.U
    }
  }

  assert(!(A_chosen && B_chosen))
}

package gemmini

import chisel3._
import chisel3.util._
import Util._

class WeightedArbiter[T <: Data](t: T, maxWeightA: Int) extends Module {
  val io = IO(new Bundle {
    val inA = Flipped(Decoupled(t))
    val inB = Flipped(Decoupled(t))
    val weightA = Input(UInt(log2Up(maxWeightA+1).W))
    val forceA = Input(Bool())
    val forceB = Input(Bool())
    val out = Decoupled(t)
  })

  val count = Reg(UInt(log2Up(maxWeightA+1).W))
  val A_chosen = WireInit(false.B)
  val B_chosen = WireInit(false.B)

  val weightA = io.weightA

  io.inA.ready := false.B
  io.inB.ready := false.B

  when(io.forceA) {
    io.out <> io.inA
  }.elsewhen(io.forceB) {
    io.out <> io.inB
  }.elsewhen(io.inA.valid && io.inB.valid) {
    when (count < weightA) {
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
      count := satAdd(count, 1.U, weightA + 1.U)
    }.elsewhen(B_chosen) {
      count := 0.U
    }
  }

  assert(!(io.forceA && io.forceB))
  assert(!(A_chosen && B_chosen))
  assert((!io.inA.valid && !io.inB.valid) || weightA > 0.U)
}

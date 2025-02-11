package gemmini

import chisel3._
import chisel3.util._
import Util._

class WeightedArbiter[T <: Data](t: T, maxWeightA: Int, staticWeightAEnabled: Boolean) extends Module {
  val io = IO(new Bundle {
    val inA = Flipped(Decoupled(t))
    val inA_label = Input(UInt(5.W)) // TODO magic number
    val inA_label_valid = Input(Bool())
    val inB = Flipped(Decoupled(t))
    val inB_label = Input(UInt(5.W)) // TODO magic number
    val inB_label_valid = Input(Bool())
    val weightA = Input(UInt(log2Up(maxWeightA+1).W))
    val forceA = Input(Bool())
    val forceB = Input(Bool())
    val out = Decoupled(t)
    val out_label = Output(UInt(5.W)) // TODO magic number
    val out_label_valid = Output(Bool())

    val inA_idle = Input(Bool())
    val inB_idle = Input(Bool())
    val inA_k = Input(UInt(16.W)) // TODO magic number
    val inB_k = Input(UInt(16.W)) // TODO magic number
    val inA_i = Input(UInt(16.W)) // TODO magic number
    val inB_j = Input(UInt(16.W)) // TODO magic number
  })

  val count = Reg(UInt(log2Up(maxWeightA+1).W))
  val A_chosen = WireInit(false.B)
  val B_chosen = WireInit(false.B)

  val staticWeightA = io.weightA === 0.U && staticWeightAEnabled.B
  val weightA = if (staticWeightAEnabled) { io.weightA } else { Mux(io.weightA === 0.U, 3.U, io.weightA) }

  io.inA.ready := false.B
  io.inB.ready := false.B

  when(io.forceA) {
    io.out <> io.inA
    io.out_label := io.inA_label
    io.out_label_valid := io.inA_label_valid
  }.elsewhen(io.forceB) {
    io.out <> io.inB
    io.out_label := io.inB_label
    io.out_label_valid := io.inB_label_valid
  }.elsewhen(!staticWeightA) {
    when(io.inA.valid && io.inB.valid) {
      when(count < weightA) {
        io.out <> io.inA
        io.out_label := io.inA_label
        io.out_label_valid := io.inA_label_valid
        A_chosen := true.B
      }.otherwise {
        io.out <> io.inB
        io.out_label := io.inB_label
        io.out_label_valid := io.inB_label_valid
        B_chosen := true.B
      }
    }.elsewhen(io.inA.valid) {
      io.out <> io.inA
      io.out_label := io.inA_label
      io.out_label_valid := io.inA_label_valid
    }.otherwise {
      io.out <> io.inB
      io.out_label := io.inB_label
      io.out_label_valid := io.inB_label_valid
    }
  }.otherwise {
    when (io.inA_idle) {
      io.out <> io.inB
      io.out_label := io.inB_label
      io.out_label_valid := io.inB_label_valid
    }.elsewhen(io.inB_idle) {
      io.out <> io.inA
      io.out_label := io.inA_label
      io.out_label_valid := io.inA_label_valid
    }.elsewhen(io.inA_k > io.inB_k || (io.inB_k === 0.U && io.inB_j === 0.U)) {
      io.out <> io.inB
      io.out_label := io.inB_label
      io.out_label_valid := io.inB_label_valid
    }.otherwise {
      io.out <> io.inA
      io.out_label := io.inA_label
      io.out_label_valid := io.inA_label_valid
    }
  }

  when (io.out.fire) {
    when (A_chosen) {
      count := satAdd(count, 1.U, weightA + 1.U)
    }.elsewhen(B_chosen) {
      count := 0.U
    }
  }

  assert(!(io.forceA && io.forceB))
  assert(!(A_chosen && B_chosen))
  assert((!io.inA.valid && !io.inB.valid) || (weightA > 0.U || staticWeightAEnabled.B))
}
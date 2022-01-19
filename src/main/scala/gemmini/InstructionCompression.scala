package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters

import GemminiISA._
import Util._

class InstCompressor(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val busy = Output(Bool())
  })

  val buf = Reg(Vec(2, UDValid(new RoCCCommand)))

  val is_preload = buf(0).bits.inst.funct === PRELOAD_CMD

  val fused_cmd = WireInit(buf(0).bits)
  fused_cmd.inst.funct := buf(1).bits.inst.funct
  fused_cmd.rs1 := Cat(buf(0).bits.rs1(31, 0), buf(1).bits.rs1(31, 0))
  fused_cmd.rs2 := Cat(buf(0).bits.rs2(31, 0), buf(1).bits.rs2(31, 0))

  io.in.ready := !buf(0).valid || (buf(0).valid && is_preload && !buf(1).valid) || io.out.fire
  io.out.valid := (buf(0).valid && !is_preload) || (buf(0).valid && is_preload && buf(1).valid)
  io.out.bits := Mux(is_preload, fused_cmd, buf(0).bits)

  io.busy := buf(0).valid

  when (io.out.fire) {
    buf.foreach(_.pop())
  }

  when (io.in.fire) {
    val waddr = Mux(buf(0).valid && is_preload && !buf(1).valid, 1.U, 0.U)
    buf(waddr).push(io.in.bits)
  }

  when (reset.asBool()) {
    buf.foreach(_.valid := false.B)
  }
}

class InstDecompressor(rob_entries: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(rob_entries)))
    val out = Decoupled(new GemminiCmd(rob_entries))
  })

  val buf = Reg(UDValid(new GemminiCmd(rob_entries)))
  val cmd = buf.bits.cmd

  val is_compute = cmd.inst.funct === COMPUTE_AND_FLIP_CMD || cmd.inst.funct === COMPUTE_AND_STAY_CMD
  val pushed_preload = Reg(Bool())

  val unfused_cmd = WireInit(buf.bits)
  unfused_cmd.cmd.inst.funct := Mux(pushed_preload, cmd.inst.funct, PRELOAD_CMD)
  unfused_cmd.cmd.rs1 := Mux(pushed_preload, cmd.rs1(31, 0), cmd.rs1(63, 32))
  unfused_cmd.cmd.rs2 := Mux(pushed_preload, cmd.rs2(31, 0), cmd.rs2(63, 32))

  io.in.ready := !buf.valid || (io.out.fire && !(is_compute && !pushed_preload))
  io.out.valid := buf.valid
  io.out.bits := Mux(is_compute, unfused_cmd, buf.bits)

  when (io.out.fire) {
    when (is_compute && !pushed_preload) {
      pushed_preload := true.B
    }.otherwise {
      buf.pop()
    }
  }

  when (io.in.fire) {
    buf.push(io.in.bits)
    pushed_preload := false.B
  }

  when (reset.asBool()) {
    buf.valid := false.B
  }
}

object InstCompressor {
  def apply(enq: ReadyValidIO[RoCCCommand])(implicit p: Parameters) = {
    val ic = Module(new InstCompressor)
    ic.io.in <> enq
    (ic.io.out, ic.io.busy)
  }
}

object InstDecompressor {
  def apply(enq: ReadyValidIO[GemminiCmd], rob_entries: Int)(implicit p: Parameters) = {
    val id = Module(new InstDecompressor(rob_entries))
    id.io.in <> enq
    id.io.out
  }
}

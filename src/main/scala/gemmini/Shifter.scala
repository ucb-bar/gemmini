package gemmini

import chisel3._
import chisel3.util._

import gemmini.Util._

// TODO add SinglePort SRAM option
class ShiftSRAM[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(t)
    val out = Output(t)
    val en = Input(Bool())
  })

  assert(n >= 2)

  val mem = TwoPortSyncMem(n, t, 1)

  val waddr = Reg(UInt(log2Ceil(n).W))
  val raddr = wrappingAdd(waddr, 1.U, n)

  val read = RegNext(io.en)
  val out_buf = RegEnable(mem.io.rdata, read)

  mem.io.wen := io.en
  mem.io.wdata := io.in
  mem.io.waddr := waddr
  mem.io.mask.foreach(_ := true.B) // TODO use a TwoPortSyncMem without a mask. That should also be an option

  mem.io.ren := io.en
  mem.io.raddr := raddr
  io.out := Mux(read, mem.io.rdata, out_buf)

  when (io.en) {
    waddr := wrappingAdd(waddr, 1.U, n)
  }
}

object Shifter {
  def apply[T <: Data](in: T, n: Int, en: Bool, useSRAM: Boolean): T = {
    if (!useSRAM) {
      ShiftRegister(in, n, en)
    } else if (n < 2) {
      // println("Had to use shift registers instead of SRAM, because the delay is too small")
      ShiftRegister(in, n, en)
    } else {
      val shifter = Module(new ShiftSRAM(n, in.cloneType))
      shifter.io.in := in
      shifter.io.en := en
      shifter.io.out
    }
  }
}

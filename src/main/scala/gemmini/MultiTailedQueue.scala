package gemmini

import chisel3._
import chisel3.util._
import Util._

class MultiTailedQueue[T <: Data](gen: T, entries: Int, maxpush: Int)
  extends Module {
  val io = IO(new Bundle {
    val enq = new Bundle {
      val ready = Output(UInt((log2Ceil(maxpush) + 1).W))
      val bits = Input(Vec(maxpush, gen))
      val push = Input(UInt((log2Ceil(maxpush) + 1).W))
    }
    val deq = Decoupled(gen)
  })

  val regs  = Reg(Vec(entries, gen))
  val raddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val waddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val avail = RegInit(entries.U(log2Ceil(entries+1).W))

  assert(maxpush >= 1)
  assert(io.enq.ready <= avail)
  assert(io.enq.push <= io.enq.ready)

  // push interface
  io.enq.ready := Mux(avail < maxpush.U, avail, maxpush.U)
  for (i <- 0 until maxpush) {
    when(io.enq.push > i.U) {
      regs(wrappingAdd(waddr, i.U, entries)) := io.enq.bits(i)
    }
  }
  waddr := wrappingAdd(waddr, io.enq.push, entries)

  // pop interface
  io.deq.bits := regs(raddr)
  io.deq.valid := (avail < entries.U)
  raddr := wrappingAdd(raddr, io.deq.fire, entries)

  // countgth calc
  avail := avail - io.enq.push + io.deq.fire
}

object MultiTailedQueue {
  def apply[T <: Data](gen: T, entries: Int, maxpush: Int)
    = new MultiTailedQueue(gen.cloneType, entries, maxpush)
}

package gemmini

import chisel3._
import chisel3.util._
import Util._

trait TagQueueTag {
  def make_this_garbage(dummy: Int = 0): Unit
}

class TagQueue[T <: Data with TagQueueTag](t: T, entries: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(t.cloneType))
    val deq = Decoupled(t.cloneType)
    val all = Output(Vec(entries, t.cloneType))
  })

  val regs = Reg(Vec(entries, t.cloneType))
  val raddr = RegInit(0.U(log2Up(entries).W))
  val waddr = RegInit(0.U(log2Up(entries).W))
  val len = RegInit(0.U(log2Up(entries+1).W))

  val empty = len === 0.U
  val full = len === entries.U

  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits := regs(raddr)
  io.all := regs

  when (io.enq.fire) {
    regs(waddr) := io.enq.bits
    waddr := wrappingAdd(waddr, 1.U, entries)
  }

  when (io.deq.fire) {
    regs(raddr).make_this_garbage()
    raddr := wrappingAdd(raddr, 1.U, entries)
  }

  when (io.enq.fire && !io.deq.fire) {
    len := len + 1.U
  }.elsewhen(!io.enq.fire && io.deq.fire) {
    len := len - 1.U
  }

  when (reset.asBool()) {
    regs.foreach(_.make_this_garbage())
  }

  assert(len <= entries.U)
}

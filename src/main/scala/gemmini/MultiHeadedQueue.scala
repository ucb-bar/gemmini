package gemmini

import chisel3._
import chisel3.util._
import Util._

class MultiHeadedQueue[T <: Data](gen: T, entries: Int, heads: Int, maxpop: Int = 2) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))

    val deq = new Bundle {
      val valid = Output(Vec(heads, Bool()))
      val bits = Output(Vec(heads, gen))
      val pop = Input(UInt(log2Ceil((entries min maxpop) + 1).W))
    }

    val len = Output(UInt(log2Ceil(entries+1).W))
  })

  assert(heads >= 1)

  val regs = Reg(Vec(entries, gen))
  val raddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val waddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val len = RegInit(0.U(log2Ceil(entries+1).W))

  io.enq.ready := len < entries.U
  io.len := len

  for (i <- 0 until heads) {
    io.deq.valid(i) := len > i.U
    io.deq.bits(i) := regs(wrappingAdd(raddr, i.U, entries))
  }

  // Pushing
  when (io.enq.fire) {
    regs(waddr) := io.enq.bits
    waddr := wrappingAdd(waddr, 1.U, entries)
    len := len + 1.U
  }

  // Popping
  when(io.deq.pop > 0.U) {
    raddr := wrappingAdd(raddr, io.deq.pop, entries)
    len := len - io.deq.pop + io.enq.fire
  }

  assert(io.deq.pop <= len && io.deq.pop <= heads.U && io.deq.pop <= maxpop.U)
}

object MultiHeadedQueue {
  def apply[T <: Data](src: ReadyValidIO[T], entries: Int, heads: Int, maxpop: Int=2) = {
    val q = Module(new MultiHeadedQueue(src.bits.cloneType, entries, heads, maxpop=maxpop))
    q.io.enq <> src
    (q.io.deq, q.io.len)
  }
}

package systolic

import chisel3._
import chisel3.util._
import Util._

// TODO Only supports popping up to two elements at a time
class MultiHeadedQueue[T <: Data](gen: T, entries: Int, heads: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))

    val deq = new Bundle {
      val valid = Output(Vec(heads, Bool()))
      val bits = Output(Vec(heads, gen))
      val pop = Input(UInt(2.W))
    }

    val full = Output(Bool())
  })

  assert(heads >= 1)
  assert(io.deq.pop <= 2.U, "cannot pop more than two elements at a time for now (TODO)")

  val regs = Reg(Vec(entries, gen))
  val raddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val waddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val len = RegInit(0.U(log2Ceil(entries+1).W))

  io.enq.ready := len < entries.U
  io.full := len === entries.U

  for (i <- 0 until heads) {
    io.deq.valid(i) := len > i.U
    io.deq.bits(i) := regs(wrappingAdd(raddr, i.U, entries))
  }

  // Pushing
  when (io.enq.fire()) {
    regs(waddr) := io.enq.bits
    waddr := wrappingAdd(waddr, 1.U, entries)
    len := len + 1.U
  }

  // Popping
  when (io.deq.valid(0) && io.deq.valid(1) && io.deq.pop =/= 0.U) {
    val maxpop = Mux(io.deq.pop <= 2.U, io.deq.pop, 2.U)
    raddr := wrappingAdd(raddr, maxpop, entries)
    len := len - maxpop + io.enq.fire()
  }.elsewhen (io.deq.valid(0) && io.deq.pop =/= 0.U) {
    val maxpop = Mux(io.deq.pop <= 1.U, io.deq.pop, 1.U)
    raddr := wrappingAdd(raddr, maxpop, entries)
    len := len - maxpop + io.enq.fire()
  }
}

object MultiHeadedQueue {
  def apply[T <: Data](src: ReadyValidIO[T], entries: Int, heads: Int) = {
    val q = Module(new MultiHeadedQueue(src.bits.cloneType, entries, heads: Int))
    q.io.enq <> src
    (q.io.deq, q.io.full)
  }
}

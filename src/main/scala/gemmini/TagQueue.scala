package gemmini

import chisel3._
import chisel3.util._
import Util._

trait TagQueueTag {
  def make_this_garbage(dummy: Int = 0): Unit
}

class TagQueue[T <: TagQueueTag with Data](entries: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val bits = Input(t)
    }

    val out = new Bundle {
      val next = Input(Bool())
      val bits = Output(Vec(2, t))
      val all = Output(Vec(entries, t))
    }

    // This should really be a constructor parameter, but Chisel errors out when it is
    // val garbage = Input(t)
  })

  // val regs = RegInit(VecInit(Seq.fill(entries)(io.garbage)))
  val regs = Reg(Vec(entries, t.cloneType))
  val raddr = RegInit(0.U((log2Ceil(entries) max 1).W))
  val waddr = RegInit(3.U((log2Ceil(entries) max 1).W))

  val raddr_inc = wrappingAdd(raddr, 1.U, entries)
  val raddr_inc2 = wrappingAdd(raddr, 2.U, entries)

  io.out.bits(0) := Mux(io.out.next, regs(raddr_inc), regs(raddr))
  io.out.bits(1) := Mux(io.out.next, regs(raddr_inc2), regs(raddr_inc))
  io.out.all := regs

  when (io.in.valid) {
    waddr := wrappingAdd(waddr, 1.U, entries)
    regs(waddr) := io.in.bits
  }

  when (io.out.next) {
    raddr := raddr_inc
  }

  when (reset.toBool()) {
    regs.foreach(_.make_this_garbage())
  }
}

package systolic

import chisel3._
import chisel3.util._
import Util._

// TODO make this output garbage when it's empty
class TagQueue[T <: Data](len: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val valid = Input(Bool())
      val bits = Input(t)
    }

    val out = new Bundle {
      val next = Input(Bool())
      val bits = Output(Vec(2, t))
    }

    // This should really be a constructor parameter, but Chisel errors out when it is
    val garbage = Input(t)
  })

  val regs = RegInit(VecInit(Seq.fill(len)(io.garbage)))
  val raddr = RegInit(0.U((log2Ceil(len) max 1).W))
  val waddr = RegInit(2.U((log2Ceil(len) max 1).W))

  val raddr_inc = wrappingAdd(raddr, 1.U, len)
  val raddr_inc2 = wrappingAdd(raddr, 2.U, len)

  io.out.bits(0) := Mux(io.out.next, regs(raddr_inc), regs(raddr))
  io.out.bits(1) := Mux(io.out.next, regs(raddr_inc2), regs(raddr_inc))

  when (io.in.valid) {
    waddr := wrappingAdd(waddr, 1.U, len)
    regs(waddr) := io.in.bits
  }

  when (io.out.next) {
    raddr := wrappingAdd(raddr, 1.U, len)
  }
}

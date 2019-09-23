package systolic

import chisel3._
import chisel3.util._

class LineBreakerIn(val spad_rows: Int, val maxReqBits: Int, val maxLen: Int) extends Bundle {
  val data = UInt(maxReqBits.W)
  val spaddr = UInt(log2Up(spad_rows).W)
  val lgLen = UInt(log2Up(log2Ceil(maxLen+1)).W)
  val cmd_id = UInt(8.W) // TODO magic number
}

class LineBreakerOut(val spad_width: Int, val spad_rows: Int) extends Bundle {
  val data = UInt(spad_width.W)
  val spaddr = UInt(log2Up(spad_rows).W)
  val cmd_id = UInt(8.W) // TODO magic number
  val last = Bool()
}

// This module is meant to break up the output of the ReservationBuffer into smaller packets, each of which is the size
// of one scratchpad row
class LineBreaker(spad_width: Int, spad_rows: Int, maxReqBits: Int, mesh_rows: Int) extends Module {
  val maxLen = (maxReqBits / spad_width) + (maxReqBits % spad_width)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new LineBreakerIn(spad_rows, maxReqBits, maxLen)))
    val out = Decoupled(new LineBreakerOut(spad_width, spad_rows))
  })

  val s_idle :: s_output :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val req = RegEnable(io.in.bits, io.in.fire())

  val reqDataAddr = Reg(UInt(log2Up(maxLen).W))
  val last = reqDataAddr === ((1.U << req.lgLen).asUInt() - 1.U)

  io.in.ready := state === s_idle || (io.out.fire() && last)

  io.out.valid := state === s_output
  io.out.bits.data := req.data >> (reqDataAddr * spad_width.U)
  io.out.bits.spaddr := req.spaddr + (reqDataAddr * mesh_rows.U)
  io.out.bits.cmd_id := req.cmd_id
  io.out.bits.last := last

  when (io.in.fire()) {
    reqDataAddr := 0.U
    state := s_output
  }.elsewhen (io.out.fire()) {
    reqDataAddr := reqDataAddr + 1.U

    when (last) {
      state := s_idle
    }
  }
}

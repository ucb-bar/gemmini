
package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.RoCCCommand

import GemminiISA._
import Util._

class QueueMvin(cmd_t: RoCCCommand, nEntries: Int, local_addr_t: LocalAddr, block_rows: Int, block_cols: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(nEntries).W)))
    val local_address_pointer_A = Output(local_addr_t)
    val local_address_pointer_B = Output(local_addr_t)

    val issue = new ROBIssue(cmd_t, nEntries)

    val busy = Output(Bool())

  })

  class Entry extends Bundle {
    val is_config = Bool()

    val dst = new Bundle { //dest spad address
      val start = local_addr_t.cloneType
      def end = local_addr_t.cloneType
    }

    val issued = Bool()

    val complete_on_issue = Bool() //can delete after issue

    val cmd = cmd_t.cloneType

  }

  val entries = Module(new Queue(new Entry, nEntries)) //in order
  val macro_cmd = Reg(new Entry)
  val macro_cmd_valid = RegInit(false.B)

  io.issue.cmd := DontCare
  io.issue.cmd.inst.funct := LOAD_CMD
  io.issue.cmd.rs1 := macro_cmd.cmd.rs1 // + counter
  io.issue.cmd.rs2 := macro_cmd.cmd.rs2 | (16.U << 32).asUInt() // + counter (with rows and cols)
  io.issue.cmd.status := macro_cmd.cmd.status

  io.issue.valid := macro_cmd_valid // Maybe you also want to add some condition with the spad address counter

  when (io.issue.fire()) {
    macro_cmd.dst.start := macro_cmd.dst.start + 20.U
    when (macro_cmd.dst.start >= macro_cmd.dst.end) {
      macro_cmd_valid := false.B
    }
  }

  entries.io.deq.ready := !macro_cmd_valid
  when (entries.io.deq.fire()) {
    macro_cmd := entries.io.deq.bits
    macro_cmd_valid := true.B
  }
}

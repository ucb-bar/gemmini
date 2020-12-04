
package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.RoCCCommand

import GemminiISA._
import Util._


class QueueCompute (cmd_t: RoCCCommand, nEntries: Int, local_addr_t: LocalAddr, block_rows: Int, block_cols: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(nEntries).W)))

    val issue = new Bundle {
      val st = new ROBIssue(cmd_t, nEntries)
      val ex = new ROBIssue(cmd_t, nEntries)
    }
    val busy = Output(Bool())

  })

  class Entry extends Bundle {

    val is_config = Bool()

    val op1 = UDValid(local_addr_t.cloneType)
    val op2 = UDValid(local_addr_t.cloneType)

    val dst = UDValid(new Bundle { //dest spad address
      val start = local_addr_t.cloneType
      def end = local_addr_t.cloneType
    })

    val issued = Bool()

    val complete_on_issue = Bool() //can delete after issue

    val cmd = cmd_t.cloneType

    val deps = Vec(nEntries, Bool()) //other command dependent on
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _) //no dependency, can run
  }

  val entries = Reg(Vec(nEntries, UDValid(new Entry)))
  //when do we use Module, Bundle, Vec?

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  io.issue.cmd := DontCare
  /*
  issue execution command from whichever queue entry that is ready (address < spad_pointer)
  if there are multiple entries ready, then oldest one gets priority
  after all counter has increment -> issue store command -> retire

   */
}

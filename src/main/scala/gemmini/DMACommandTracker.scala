package gemmini

import chisel3._
import chisel3.util._


// This module is meant to go inside the Load controller, where it can track which commands are currently
// in flight and which are completed
class DMACommandTracker[T <: Data](val nCmds: Int, val maxBytes: Int, tag_t: => T, prng_seed: Int, proportion_of_slow_accesses_out_of_128: Int, stall_delay: Int) extends Module {
  def cmd_id_t = UInt((log2Ceil(nCmds) max 1).W)

  val io = IO(new Bundle {
    // TODO is there an existing decoupled interface in the standard library which matches this use-case?
    val alloc = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())

      class BitsT(tag_t: => T, cmd_id_t: UInt) extends Bundle {
        // This was only spun off as its own class to resolve CloneType errors
        val tag = Input(tag_t.cloneType)
        val bytes_to_read = Input(UInt(log2Up(maxBytes+1).W))
        val cmd_id = Output(cmd_id_t.cloneType)

        override def cloneType: this.type = new BitsT(tag_t.cloneType, cmd_id_t.cloneType).asInstanceOf[this.type]
      }

      val bits = new BitsT(tag_t.cloneType, cmd_id_t.cloneType)

      def fire(dummy: Int = 0) = valid && ready
    }

    class RequestReturnedT(cmd_id_t: UInt) extends Bundle {
      // This was only spun off as its own class to resolve CloneType errors
      val bytes_read = UInt(log2Up(maxBytes+1).W)
      val cmd_id = cmd_id_t.cloneType

      override def cloneType: this.type = new RequestReturnedT(cmd_id_t.cloneType).asInstanceOf[this.type]
    }

    val request_returned = Flipped(Valid(new RequestReturnedT(cmd_id_t.cloneType)))

    class CmdCompletedT(cmd_id_t: UInt, tag_t: T) extends Bundle {
      val cmd_id = cmd_id_t.cloneType
      val tag = tag_t.cloneType

      override def cloneType: this.type = new CmdCompletedT(cmd_id_t.cloneType, tag_t.cloneType).asInstanceOf[this.type]
    }

    val cmd_completed = Decoupled(new CmdCompletedT(cmd_id_t.cloneType, tag_t.cloneType))

    val busy = Output(Bool())
  })

  class Entry extends Bundle {
    val valid = Bool()
    val tag = tag_t.cloneType
    val bytes_left = UInt(log2Up(maxBytes+1).W)

    val stall_cycles = UInt(32.W) // TODO magic number

    def init(dummy: Int = 0): Unit = {
      valid := false.B
    }
  }

  // val cmds = RegInit(VecInit(Seq.fill(nCmds)(entry_init)))
  val cmds = Reg(Vec(nCmds, new Entry))
  val cmd_valids = cmds.map(_.valid)

  val next_empty_alloc = MuxCase(0.U, cmd_valids.zipWithIndex.map { case (v, i) => (!v) -> i.U })

  io.alloc.ready := !cmd_valids.reduce(_ && _)
  io.alloc.bits.cmd_id := next_empty_alloc

  io.busy := cmd_valids.reduce(_ || _)

  val cmd_completed_id = MuxCase(0.U, cmds.zipWithIndex.map { case (cmd, i) =>
    (cmd.valid && cmd.bytes_left === 0.U && cmd.stall_cycles === 0.U) -> i.U
  })
  io.cmd_completed.valid := cmds.map(cmd => cmd.valid && cmd.bytes_left === 0.U && cmd.stall_cycles === 0.U).reduce(_ || _)
  io.cmd_completed.bits.cmd_id := cmd_completed_id
  io.cmd_completed.bits.tag := cmds(cmd_completed_id).tag

  when (io.alloc.fire()) {
    cmds(next_empty_alloc).valid := true.B
    cmds(next_empty_alloc).tag := io.alloc.bits.tag
    cmds(next_empty_alloc).bytes_left := io.alloc.bits.bytes_to_read

    val random_number = random.GaloisLFSR.maxPeriod(width=8, seed=Some(prng_seed))

    cmds(next_empty_alloc).stall_cycles := Mux(random_number < proportion_of_slow_accesses_out_of_128.U,
      stall_delay.U, 0.U)
  }

  when (io.request_returned.fire()) {
    val cmd_id = io.request_returned.bits.cmd_id
    cmds(cmd_id).bytes_left := cmds(cmd_id).bytes_left - io.request_returned.bits.bytes_read

    assert(cmds(cmd_id).valid)
    assert(cmds(cmd_id).bytes_left >= io.request_returned.bits.bytes_read)
  }

  when (io.cmd_completed.fire()) {
    cmds(io.cmd_completed.bits.cmd_id).valid := false.B
  }

  cmds.foreach { cmd =>
    when (cmd.valid && cmd.bytes_left === 0.U && cmd.stall_cycles > 0.U) {
      cmd.stall_cycles := cmd.stall_cycles - 1.U
    }
  }

  when (reset.asBool()) {
    cmds.foreach(_.init())
  }
}


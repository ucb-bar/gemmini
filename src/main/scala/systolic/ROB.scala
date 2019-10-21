package systolic

import chisel3._
import chisel3.util._

import freechips.rocketchip.tile.RoCCCommand

import SystolicISA._
import Util._

class ROBIssue[T <: Data](cmd_t: T, nEntries: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(log2Up(nEntries).W))

  def fire(dummy: Int=0) = valid && ready

  override def cloneType: this.type = new ROBIssue(cmd_t, nEntries).asInstanceOf[this.type]
}

// class ROB[T <: RoCCCommand](cmd_t: T, nEntries: Int, sprows: Int, block_rows: Int) extends Module {
class ROB[T <: RoCCCommand](cmd_t: T, nEntries: Int, local_addr_t: LocalAddr, block_rows: Int) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(nEntries).W)))

    val issue = new Bundle {
      val ld = new ROBIssue(cmd_t, nEntries)
      val st = new ROBIssue(cmd_t, nEntries)
      val ex = new ROBIssue(cmd_t, nEntries)
    }

    val busy = Output(Bool())
  })

  val ldq :: stq :: exq :: Nil = Enum(3)
  val q_t = ldq.cloneType

  val raddr = RegInit(0.U(log2Up(nEntries).W))
  val waddr = RegInit(0.U(log2Up(nEntries).W))

  val empty = RegInit(true.B)
  val full = !empty && raddr === waddr

  io.busy := !empty

  class Entry extends Bundle {
    val q = q_t.cloneType

    val is_config = Bool()

    val op1 = UDValid(local_addr_t.cloneType)
    val op2 = UDValid(local_addr_t.cloneType)

    val dst = UDValid(new Bundle {
      val start = local_addr_t.cloneType
      val len = UInt(8.W) // TODO magic number

      def end(dummy: Int = 0) = start + len * block_rows.U
    })

    val ready = Bool()
    val issued = Bool()
    val completed = Bool()

    val cmd = cmd_t.cloneType
  }

  val entries = Reg(Vec(nEntries, UDValid(new Entry)))

  // Update entries which are ready for execution
  entries.zipWithIndex.foreach { case (entry, i) =>
    val raws = entries.map { e =>
      // We search for all entries which write to an address which we read from
      e.valid && !e.bits.completed && e.bits.dst.valid && (
        (entry.bits.op1.valid && e.bits.dst.bits.start <= entry.bits.op1.bits && e.bits.dst.bits.end() > entry.bits.op1.bits) ||
          (entry.bits.op2.valid && e.bits.dst.bits.start <= entry.bits.op2.bits && e.bits.dst.bits.end() > entry.bits.op2.bits))
    }

    val wars = entries.map { e =>
      // We search for all entries which read from an address that we write to
      e.valid && !e.bits.completed && entry.bits.dst.valid && (
        (e.bits.op1.valid && entry.bits.dst.bits.start <= e.bits.op1.bits && entry.bits.dst.bits.end() > e.bits.op1.bits) ||
          (e.bits.op2.valid && entry.bits.dst.bits.start <= e.bits.op2.bits && entry.bits.dst.bits.end() > e.bits.op2.bits))
    }

    val waws = entries.map { e =>
      // We search for all entries which write to an address that we write to
      e.valid && !e.bits.completed && entry.bits.dst.valid && e.bits.dst.valid && (
        (entry.bits.dst.bits.start <= e.bits.dst.bits.start && entry.bits.dst.bits.end() > e.bits.dst.bits.start) ||
          (e.bits.dst.bits.start <= entry.bits.dst.bits.start && e.bits.dst.bits.end() > entry.bits.dst.bits.start))
    }

    val precedes = (0 until nEntries).map(j => Mux(raddr <= i.U, (j < i).B && j.U >= raddr, (j < i).B || j.U >= raddr))
    val different_q = entries.map(e => entry.bits.q =/= e.bits.q)

    val raw = (raws, precedes, different_q).zipped.map { case (rw, p, dq) => rw && p && dq }.reduce(_ || _)
    val war = (wars, precedes, different_q).zipped.map { case (wr, p, dq) => wr && p && dq }.reduce(_ || _)
    val waw = (waws, precedes, different_q).zipped.map { case (ww, p, dq) => ww && p && dq }.reduce(_ || _)

    // Commands within the same pipeline are issued in order, for now
    // TODO allowed out-of-order issue if necessary for perf. reasons
    val not_oldest = (entries, precedes).zipped.map { case (e, p) =>
        p && e.valid && !e.bits.issued && e.bits.q === entry.bits.q
    }.reduce(_ || _)

    val is_st_and_must_wait_for_prior_ex_config = entry.bits.q === stq && !entry.bits.is_config &&
      (entries, precedes).zipped.map { case (e, p) =>
        p && e.valid && !e.bits.completed && e.bits.is_config && e.bits.q === exq
      }.reduce(_ || _)

    val is_ex_config_and_must_wait_for_prior_st = entry.bits.is_config && entry.bits.q === exq &&
      (entries, precedes).zipped.map { case (e, p) =>
        p && e.valid && !e.bits.completed && e.bits.q === stq && !e.bits.is_config
      }.reduce(_ || _)

    val dep = raw || war || waw || // Data dependencies between queues
      not_oldest || // In order issue within queue
      is_st_and_must_wait_for_prior_ex_config || is_ex_config_and_must_wait_for_prior_st // Misc. dependencies

    when (!dep) {
      entry.bits.ready := true.B
    }
  }

  // Pop entries at the head of the table which have completed
  when (!empty && entries(raddr).bits.completed) {
    entries(raddr).valid := false.B

    val next_raddr = wrappingAdd(raddr, 1.U, nEntries)
    raddr := next_raddr

    when (waddr === next_raddr) {
      empty := true.B
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire()) {
    entries(io.completed.bits).bits.completed := true.B
    assert(entries(io.completed.bits).valid)
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld), (stq, io.issue.st), (exq, io.issue.ex)).foreach { case (q, io) =>
    val issue_id = MuxCase((nEntries-1).U, entries.zipWithIndex.map { case (e, i) =>
      (e.valid && e.bits.ready && !e.bits.issued && e.bits.q === q) -> i.U
    })

    io.valid := entries.map(e => e.valid && e.bits.ready && !e.bits.issued && e.bits.q === q).reduce(_ || _)
    io.cmd := entries(issue_id).bits.cmd
    io.rob_id := issue_id

    when (io.fire()) {
      entries(issue_id).bits.issued := true.B
    }
  }

  // Read in commands to the buffer
  io.alloc.ready := !full

  when (io.alloc.fire()) {
    val spAddrBits = 32
    val cmd = io.alloc.bits
    val funct = cmd.inst.funct
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    empty := false.B
    waddr := wrappingAdd(waddr, 1.U, nEntries)

    entries(waddr).valid := true.B
    entries(waddr).bits.ready := false.B
    entries(waddr).bits.issued := false.B
    entries(waddr).bits.completed := false.B
    entries(waddr).bits.cmd := cmd

    entries(waddr).bits.is_config := funct === CONFIG_CMD

    entries(waddr).bits.op1.valid := funct === PRELOAD_CMD || funct === COMPUTE_AND_STAY_CMD ||
      funct === COMPUTE_AND_FLIP_CMD
    entries(waddr).bits.op1.bits := cmd.rs1.asTypeOf(local_addr_t)

    entries(waddr).bits.op2.valid := funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD ||
      funct === LOAD_CMD || funct === STORE_CMD
    entries(waddr).bits.op2.bits := cmd.rs2.asTypeOf(local_addr_t)

    entries(waddr).bits.dst.valid := funct === PRELOAD_CMD || funct === LOAD_CMD
    entries(waddr).bits.dst.bits.start := cmd.rs2.asTypeOf(local_addr_t)
    entries(waddr).bits.dst.bits.len := Mux(funct === PRELOAD_CMD, 1.U, cmd.rs2(63, spAddrBits)) // TODO magic number

    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = (funct === COMPUTE_AND_FLIP_CMD || funct === COMPUTE_AND_STAY_CMD || funct === PRELOAD_CMD) ||
      (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)

    entries(waddr).bits.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq
    ))
  }

  when (reset.toBool()) {
    entries.foreach(_.valid := false.B)
  }
}

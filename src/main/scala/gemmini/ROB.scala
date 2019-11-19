package gemmini

import chisel3._
import chisel3.util._
import chisel3.core.dontTouch

import freechips.rocketchip.tile.RoCCCommand

import GemminiISA._
import Util._


// TODO unify this class with GemminiCmdWithDeps
class ROBIssue[T <: Data](cmd_t: T, nEntries: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(log2Up(nEntries).W))

  def fire(dummy: Int=0) = valid && ready

  override def cloneType: this.type = new ROBIssue(cmd_t, nEntries).asInstanceOf[this.type]
}

// class ROB[T <: RoCCCommand](cmd_t: T, nEntries: Int, sprows: Int, block_rows: Int) extends Module {
class ROB(cmd_t: RoCCCommand, nEntries: Int, local_addr_t: LocalAddr, block_rows: Int) extends Module {
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

  class Entry extends Bundle {
    val q = q_t.cloneType

    val is_config = Bool()

    val op1 = UDValid(local_addr_t.cloneType)
    val op2 = UDValid(local_addr_t.cloneType)
    val op3 = UDValid(local_addr_t.cloneType)

    val dst = UDValid(new Bundle {
      val start = local_addr_t.cloneType
      val len = UInt(8.W) // TODO magic number

      def end(dummy: Int = 0) = start + len * block_rows.U
    })

    val issued = Bool()

    val complete_on_issue = Bool()

    val cmd = cmd_t.cloneType

    val deps = Vec(nEntries, Bool())
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _)
  }

  val entries = Reg(Vec(nEntries, UDValid(new Entry)))

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  io.busy := !empty

  // Read in commands to the buffer
  io.alloc.ready := !full

  val last_allocated = Reg(UInt(log2Up(nEntries).W))

  when (io.alloc.fire()) {
    val spAddrBits = 32
    val cmd = io.alloc.bits
    val funct = cmd.inst.funct
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    val waddr = MuxCase((nEntries-1).U, entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })

    val new_entry = Wire(new Entry)
    new_entry.issued := false.B
    new_entry.cmd := cmd

    new_entry.is_config := funct === CONFIG_CMD

    new_entry.op1.valid := funct_is_compute
    new_entry.op1.bits := cmd.rs1.asTypeOf(local_addr_t)

    new_entry.op2.valid := funct_is_compute || funct === LOAD_CMD || funct === STORE_CMD
    new_entry.op2.bits := cmd.rs2.asTypeOf(local_addr_t)

    new_entry.op3.valid := funct_is_compute
    new_entry.op3.bits := cmd.rs1(63, 32).asTypeOf(local_addr_t)

    new_entry.dst.valid := funct_is_compute || funct === LOAD_CMD
    new_entry.dst.bits.start := Mux(funct_is_compute, cmd.rs2(63, 32), cmd.rs2(31, 0)).asTypeOf(local_addr_t)
    new_entry.dst.bits.len := Mux(funct_is_compute, 1.U, cmd.rs2(63, spAddrBits)) // TODO magic number

    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = funct_is_compute || (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)

    new_entry.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq
    ))

    val raws = entries.map { e =>
      // We search for all entries which write to an address which we read from
      e.valid && e.bits.dst.valid && (
        (new_entry.op1.valid && e.bits.dst.bits.start <= new_entry.op1.bits && e.bits.dst.bits.end() > new_entry.op1.bits) ||
          (new_entry.op2.valid && e.bits.dst.bits.start <= new_entry.op2.bits && e.bits.dst.bits.end() > new_entry.op2.bits) ||
          (new_entry.op3.valid && e.bits.dst.bits.start <= new_entry.op3.bits && e.bits.dst.bits.end() > new_entry.op3.bits))
    }

    val wars = entries.map { e =>
      // We search for all entries which read from an address that we write to
      e.valid && new_entry.dst.valid && (
        (e.bits.op1.valid && new_entry.dst.bits.start <= e.bits.op1.bits && new_entry.dst.bits.end() > e.bits.op1.bits) ||
          (e.bits.op2.valid && new_entry.dst.bits.start <= e.bits.op2.bits && new_entry.dst.bits.end() > e.bits.op2.bits) ||
          (e.bits.op3.valid && new_entry.dst.bits.start <= e.bits.op3.bits && new_entry.dst.bits.end() > e.bits.op3.bits))
    }

    val waws = entries.map { e =>
      // We search for all entries which write to an address that we write to
      e.valid && new_entry.dst.valid && e.bits.dst.valid && (
        (new_entry.dst.bits.start <= e.bits.dst.bits.start && new_entry.dst.bits.end() > e.bits.dst.bits.start) ||
          (e.bits.dst.bits.start <= new_entry.dst.bits.start && e.bits.dst.bits.end() > new_entry.dst.bits.start))
    }

    val older_in_same_q = entries.map { e =>
      e.valid && e.bits.q === new_entry.q && !e.bits.issued
    }

    val is_st_and_must_wait_for_prior_ex_config = entries.map { e =>
      e.valid && new_entry.q === stq && !new_entry.is_config && e.bits.q === exq && e.bits.is_config
    }

    val is_ex_config_and_must_wait_for_prior_st = entries.map { e =>
      e.valid && new_entry.q === exq && new_entry.is_config && e.bits.q === stq && !e.bits.is_config
    }

    new_entry.deps := (Cat(raws) | Cat(wars) | Cat(waws) | Cat(older_in_same_q) |
      Cat(is_st_and_must_wait_for_prior_ex_config) | Cat(is_ex_config_and_must_wait_for_prior_st)).toBools().reverse

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exq

    entries(waddr).valid := true.B
    entries(waddr).bits := new_entry

    last_allocated := waddr
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld), (stq, io.issue.st), (exq, io.issue.ex)).foreach { case (q, io) =>
    val issue_id = MuxCase((nEntries-1).U, entries.zipWithIndex.map { case (e, i) =>
      (e.valid && e.bits.ready() && !e.bits.issued && e.bits.q === q) -> i.U
    })

    io.valid := entries.map(e => e.valid && e.bits.ready() && !e.bits.issued && e.bits.q === q).reduce(_ || _)
    io.cmd := entries(issue_id).bits.cmd
    io.rob_id := issue_id

    when (io.fire()) {
      entries(issue_id).bits.issued := true.B

      // Clear out all the dependency bits for instructions which depend on the same queue
      entries.foreach { e =>
        when (e.bits.q === entries(issue_id).bits.q || entries(issue_id).bits.complete_on_issue) {
          e.bits.deps(issue_id) := false.B
        }
      }

      entries(issue_id).valid := !entries(issue_id).bits.complete_on_issue
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire()) {
    entries.foreach(_.bits.deps(io.completed.bits) := false.B)

    entries(io.completed.bits).valid := false.B
    assert(entries(io.completed.bits).valid)
  }

  val utilization = PopCount(entries.map(e => e.valid))
  val utilization_ld_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === ldq))
  val utilization_st_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === stq))
  val utilization_ex_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === exq))
  val utilization_ld_q = PopCount(entries.map(e => e.valid && e.bits.q === ldq))
  val utilization_st_q = PopCount(entries.map(e => e.valid && e.bits.q === stq))
  val utilization_ex_q = PopCount(entries.map(e => e.valid && e.bits.q === exq))

  val packed_deps = VecInit(entries.map(e => Cat(e.bits.deps)))
  dontTouch(packed_deps)

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid, PopCount(e.bits.deps), 0.U)))
  val min_pop_count = pop_count_packed_deps.reduce((acc, d) => minOf(acc, d))
  // assert(min_pop_count < 2.U)
  dontTouch(pop_count_packed_deps)
  dontTouch(min_pop_count)

  for (i <- 0 until 2) {
  }


  val cycles_since_issue = RegInit(0.U(32.W))

  when (io.issue.ld.fire() || io.issue.st.fire() || io.issue.ex.fire() || !io.busy) {
    cycles_since_issue := 0.U
  }.elsewhen(io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < 10000.U, "pipeline stall")

  val cntr = Counter(10000000)
  when (cntr.inc()) {
    printf(p"Utilization: $utilization\n")
    printf(p"Utilization ld q (incomplete): $utilization_ld_q_unissued\n")
    printf(p"Utilization st q (incomplete): $utilization_st_q_unissued\n")
    printf(p"Utilization ex q (incomplete): $utilization_ex_q_unissued\n")
    printf(p"Utilization ld q: $utilization_ld_q\n")
    printf(p"Utilization st q: $utilization_st_q\n")
    printf(p"Utilization ex q: $utilization_ex_q\n")
    printf(p"Packed deps: $packed_deps\n")
    printf(p"Last allocated: $last_allocated\n\n")
  }

  when (reset.toBool()) {
    entries.foreach(_.valid := false.B)
  }
}

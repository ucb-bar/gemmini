
package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.RoCCCommand
import GemminiISA._
import Util._

// TODO unify this class with GemminiCmdWithDeps
class ROBIssue[T <: Data](cmd_t: T, rob_entries: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(log2Up(rob_entries).W))

  def fire(dummy: Int=0) = valid && ready

  override def cloneType: this.type = new ROBIssue(cmd_t, rob_entries).asInstanceOf[this.type]
}

// TODO we don't need to store the full command in here. We should be able to release the command directly into the relevant controller and only store the associated metadata in the ROB. This would reduce the size considerably
class ROB[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], cmd_t: RoCCCommand) extends Module {
  import config._

  val block_rows = tileRows * meshRows
  val block_cols = tileColumns * meshColumns

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(rob_entries).W)))

    val issue = new Bundle {
      val ld = new ROBIssue(cmd_t, rob_entries)
      val st = new ROBIssue(cmd_t, rob_entries)
      val ex = new ROBIssue(cmd_t, rob_entries)
    }

    val ld_utilization = Output(UInt(log2Up(rob_entries+1).W))
    val st_utilization = Output(UInt(log2Up(rob_entries+1).W))
    val ex_utilization = Output(UInt(log2Up(rob_entries+1).W))

    val busy = Output(Bool())

    val solitary_preload = Input(Bool()) // TODO very hacky. from ExecuteController, to prevent infinite fence stalls. remove later

    val counter = new CounterEventIO()
  })

  // TODO make this a ChiselEnum
  val ldq :: stq :: exq :: Nil = Enum(3)
  val q_t = ldq.cloneType

  class OpT extends Bundle {
    val start = local_addr_t.cloneType
    val end = local_addr_t.cloneType
    val wraps_around = Bool()

    def overlaps(other: OpT): Bool = {
      (other.start <= start && (start <= other.end || other.wraps_around)) ||
        (start <= other.start && (other.start <= end || wraps_around))
    }
  }

  class Entry extends Bundle {
    val q = q_t.cloneType

    val is_config = Bool()

    val op1 = UDValid(new OpT)
    val op2 = UDValid(new OpT)
    val dst = UDValid(new OpT)

    val issued = Bool()

    val complete_on_issue = Bool()

    val cmd = cmd_t.cloneType

    val deps = Vec(rob_entries, Bool())
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _)
  }

  val entries = Reg(Vec(rob_entries, UDValid(new Entry)))

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  // TODO we could also check for a solitary preload by recording the last instruction that was allocated, rather than
  // reading all entries to check for preloads, which is an O(n) operation in terms of area cost
  val utilization = PopCount(entries.map(_.valid))
  val solitary_preload = utilization === 1.U && entries.map(e => e.valid && e.bits.cmd.inst.funct === PRELOAD_CMD).reduce(_ || _)
  io.busy := !empty && !(solitary_preload && io.solitary_preload)

  // Read in commands to the buffer
  io.alloc.ready := !full

  val last_allocated = Reg(UInt(log2Up(rob_entries).W))
  val a_stride = Reg(UInt(16.W)) // TODO magic numbers // TODO we also need to check the transpose to see how many rows we're reading
  val block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W)))

  val new_entry = Wire(new Entry)
  new_entry := DontCare
  val new_entry_id = MuxCase((rob_entries-1).U, entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })
  val alloc_fire = io.alloc.fire()

  val raws_probe = WireInit(0.U(rob_entries.W))
  val waws_probe = WireInit(0.U(rob_entries.W))
  val wars_probe = WireInit(0.U(rob_entries.W))
  val older_in_same_q_probe = WireInit(0.U(rob_entries.W))
  val is_st_and_must_wait_for_prior_ex_config_probe = WireInit(0.U(rob_entries.W))
  val is_ex_config_and_must_wait_for_prior_st_probe = WireInit(0.U(rob_entries.W))

  val wars_op1_probe = WireInit(0.U(rob_entries.W))
  val wars_op2_probe = WireInit(0.U(rob_entries.W))

  dontTouch(new_entry)

  when (io.alloc.fire()) {
    val spAddrBits = 32
    val cmd = io.alloc.bits
    val funct = cmd.inst.funct
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    new_entry.issued := false.B
    new_entry.cmd := cmd

    new_entry.is_config := funct === CONFIG_CMD

    new_entry.op1.valid := funct === PRELOAD_CMD || funct_is_compute
    new_entry.op1.bits.start := cmd.rs1.asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      val preload_rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48)
      new_entry.op1.bits.end := new_entry.op1.bits.start + preload_rows
      new_entry.op1.bits.wraps_around := new_entry.op1.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val compute_rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48) * a_stride
      new_entry.op1.bits.end := new_entry.op1.bits.start + compute_rows
      new_entry.op1.bits.wraps_around := new_entry.op1.bits.start.add_with_overflow(compute_rows)._2
    }

    new_entry.op2.valid := funct_is_compute || funct === STORE_CMD
    new_entry.op2.bits.start := cmd.rs2.asTypeOf(local_addr_t)
    when (funct_is_compute) {
      val compute_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48)
      new_entry.op2.bits.end := new_entry.op2.bits.start + compute_rows
      new_entry.op2.bits.wraps_around := new_entry.op2.bits.start.add_with_overflow(compute_rows)._2
    }.otherwise {
      val mvout_rows = cmd.rs2(48 + mvout_rows_bits - 1, 48)
      new_entry.op2.bits.end := new_entry.op2.bits.start + mvout_rows
      new_entry.op2.bits.wraps_around := new_entry.op2.bits.start.add_with_overflow(mvout_rows)._2
    }

    new_entry.dst.valid := funct === PRELOAD_CMD || funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD
    new_entry.dst.bits.start := cmd.rs2(31, 0).asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      val preload_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48)
      new_entry.dst.bits.end := new_entry.dst.bits.start + preload_rows
      new_entry.dst.bits.wraps_around := new_entry.dst.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val id = MuxCase(0.U, Seq((new_entry.cmd.inst.funct === LOAD2_CMD) -> 1.U,
        (new_entry.cmd.inst.funct === LOAD3_CMD) -> 2.U))
      val block_stride = block_strides(id)

      val mvin_cols = cmd.rs2(spAddrBits + mvin_cols_bits - 1, spAddrBits)
      val mvin_mats = mvin_cols / block_cols.U + (mvin_cols % block_cols.U =/= 0.U)
      val mvin_rows = mvin_mats * block_stride

      new_entry.dst.bits.end := new_entry.dst.bits.start + mvin_rows
      new_entry.dst.bits.wraps_around := new_entry.dst.bits.start.add_with_overflow(mvin_rows)._2
    }

    val is_load = funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = funct === STORE_CMD || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = funct === PRELOAD_CMD || funct_is_compute || (funct === CONFIG_CMD && (config_cmd_type === CONFIG_EX || config_cmd_type === CONFIG_IM2COL))

    new_entry.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq
    ))

    assert(is_load || is_store || is_ex)

    // TODO we should checck whether op1 and op2 are valid here
    val raws = entries.map { e =>
      // We search for all entries which write to an address which we read from
      e.valid && e.bits.dst.valid && e.bits.q =/= new_entry.q && (
        (new_entry.op1.valid && new_entry.op1.bits.overlaps(e.bits.dst.bits)) ||
          (new_entry.op2.valid && new_entry.op2.bits.overlaps(e.bits.dst.bits)))
    }

    // TODO we should checck whether op1 and op2 are valid here
    val wars = entries.map { e =>
      // We search for all entries which read from an address that we write to
      e.valid && new_entry.dst.valid && e.bits.q =/= new_entry.q && (
        (e.bits.op1.valid && e.bits.op1.bits.overlaps(new_entry.dst.bits)) ||
          (e.bits.op2.valid && e.bits.op2.bits.overlaps(new_entry.dst.bits)))
    }

    val wars_op1 = entries.map { e =>
      // We search for all entries which read from an address that we write to
      e.valid && new_entry.dst.valid && e.bits.q =/= new_entry.q && (
        e.bits.op1.bits.overlaps(new_entry.dst.bits))
    }

    val wars_op2 = entries.map { e =>
      // We search for all entries which read from an address that we write to
      e.valid && new_entry.dst.valid && e.bits.q =/= new_entry.q && (
        e.bits.op2.bits.overlaps(new_entry.dst.bits))
    }

    // TODO we should checck whether op1 and op2 are valid here
    val waws = entries.map { e =>
      // We search for all entries which write to an address that we write to
      e.valid && new_entry.dst.valid && e.bits.dst.valid && e.bits.q =/= new_entry.q &&
        (new_entry.dst.bits.overlaps(e.bits.dst.bits) || e.bits.dst.bits.overlaps(new_entry.dst.bits))
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
      Cat(is_st_and_must_wait_for_prior_ex_config) | Cat(is_ex_config_and_must_wait_for_prior_st)).asBools().reverse

    raws_probe := Cat(raws)
    waws_probe := Cat(waws)
    wars_probe := Cat(wars)
    wars_op1_probe := Cat(wars_op1)
    wars_op2_probe := Cat(wars_op2)
    older_in_same_q_probe := Cat(older_in_same_q)
    is_st_and_must_wait_for_prior_ex_config_probe := Cat(is_st_and_must_wait_for_prior_ex_config)
    is_ex_config_and_must_wait_for_prior_st_probe := Cat(is_ex_config_and_must_wait_for_prior_st)

    dontTouch(raws_probe)
    dontTouch(waws_probe)
    dontTouch(wars_probe)
    dontTouch(wars_op1_probe)
    dontTouch(wars_op2_probe)
    dontTouch(older_in_same_q_probe)
    dontTouch(is_st_and_must_wait_for_prior_ex_config_probe)
    dontTouch(is_ex_config_and_must_wait_for_prior_st_probe)

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exq

    entries(new_entry_id).valid := true.B
    entries(new_entry_id).bits := new_entry

    last_allocated := new_entry_id

    when (new_entry.is_config && new_entry.q === exq) {
      a_stride := new_entry.cmd.rs1(31, 16) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
    }.elsewhen(new_entry.is_config && new_entry.q === ldq) {
      val id = new_entry.cmd.rs1(4,3) // TODO magic numbers
      val block_stride = new_entry.cmd.rs1(31, 16) // TODO magic numbers
      block_strides(id) := block_stride
    }
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld), (stq, io.issue.st), (exq, io.issue.ex)).foreach { case (q, io) =>
    val issue_id = MuxCase((rob_entries-1).U, entries.zipWithIndex.map { case (e, i) =>
      (e.valid && e.bits.ready() && !e.bits.issued && e.bits.q === q) -> i.U
    })

    io.valid := entries.map(e => e.valid && e.bits.ready() && !e.bits.issued && e.bits.q === q).reduce(_ || _)
    io.cmd := entries(issue_id).bits.cmd
    io.rob_id := issue_id

    when (io.fire()) {
      entries(issue_id).bits.issued := true.B

      // Clear out all the dependency bits for instructions which depend on the same queue
      entries.zipWithIndex.foreach { case (e, i) =>
        val is_same_q = Mux(alloc_fire && new_entry_id === i.U,
          new_entry.q === entries(issue_id).bits.q,
          e.bits.q === entries(issue_id).bits.q)

        when (is_same_q || entries(issue_id).bits.complete_on_issue) {
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

  // val utilization = PopCount(entries.map(e => e.valid))
  val utilization_ld_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === ldq))
  val utilization_st_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === stq))
  val utilization_ex_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === exq))
  val utilization_ld_q = PopCount(entries.map(e => e.valid && e.bits.q === ldq))
  val utilization_st_q = PopCount(entries.map(e => e.valid && e.bits.q === stq))
  val utilization_ex_q = PopCount(entries.map(e => e.valid && e.bits.q === exq))

  io.ld_utilization := utilization_ld_q
  io.st_utilization := utilization_st_q
  io.ex_utilization := utilization_ex_q

  val packed_deps = VecInit(entries.map(e => Cat(e.bits.deps.reverse)))
  dontTouch(packed_deps)

  val valids = VecInit(entries.map(_.valid))
  val functs = VecInit(entries.map(_.bits.cmd.inst.funct))
  val issueds = VecInit(entries.map(_.bits.issued))

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid, PopCount(e.bits.deps), 0.U)))
  val min_pop_count = pop_count_packed_deps.reduce((acc, d) => minOf(acc, d))
  // assert(min_pop_count < 2.U)
  dontTouch(pop_count_packed_deps)
  dontTouch(min_pop_count)

  val cycles_since_issue = RegInit(0.U(16.W))

  when (io.issue.ld.fire() || io.issue.st.fire() || io.issue.ex.fire() || !io.busy) {
    cycles_since_issue := 0.U
  }.elsewhen(io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < 10000.U, "pipeline stall")

  val instructions_allocated = RegInit(0.U(32.W))
  when (io.alloc.fire()) {
    instructions_allocated := instructions_allocated + 1.U
  }
  dontTouch(instructions_allocated)

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

  when (reset.asBool()) {
    entries.foreach(_.valid := false.B)
  }

  CounterEventIO.init(io.counter)
  io.counter.connectExternalCounter(CounterExternal.ROB_LD_COUNT, utilization_ld_q)
  io.counter.connectExternalCounter(CounterExternal.ROB_ST_COUNT, utilization_st_q)
  io.counter.connectExternalCounter(CounterExternal.ROB_EX_COUNT, utilization_ex_q)
  io.counter.connectEventSignal(CounterEvent.ROB_ACTIVE_CYCLES, io.busy)
  io.counter.connectEventSignal(CounterEvent.ROB_FULL_CYCLES, !io.alloc.ready)
}

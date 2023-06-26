
package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.util.PlusArg
import GemminiISA._
import Util._

import midas.targetutils.PerfCounter
import midas.targetutils.SynthesizePrintf


// TODO unify this class with GemminiCmdWithDeps
class ReservationStationIssue[T <: Data](cmd_t: T, id_width: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(id_width.W))

  def fire(dummy: Int=0) = valid && ready
}

// TODO we don't need to store the full command in here. We should be able to release the command directly into the relevant controller and only store the associated metadata in the ROB. This would reduce the size considerably
class ReservationStation[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V],
                                                                       cmd_t: GemminiCmd) extends Module {
  import config._

  val block_rows = tileRows * meshRows
  val block_cols = tileColumns * meshColumns

  val max_instructions_completed_per_type_per_cycle = 2 // Every cycle, at most two instructions of a single "type" (ld/st/ex) can be completed: one through the io.completed port, and the other if it is a "complete-on-issue" instruction

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(ROB_ID_WIDTH.W)))

    val issue = new Bundle {
      val ld = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val st = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val ex = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
    }

    val conv_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val conv_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val conv_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val matmul_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  // TODO make this a ChiselEnum
  val ldq :: exq :: stq :: Nil = Enum(3)
  val q_t = ldq.cloneType

  class OpT extends Bundle {
    val start = local_addr_t.cloneType
    val end = local_addr_t.cloneType
    val wraps_around = Bool()

    def overlaps(other: OpT): Bool = {
      ((other.start <= start && (start < other.end || other.wraps_around)) ||
        (start <= other.start && (other.start < end || wraps_around))) &&
        !(start.is_garbage() || other.start.is_garbage()) // TODO the "is_garbage" check might not really be necessary
    }
  }

  val instructions_allocated = RegInit(0.U(32.W))
  when (io.alloc.fire) {
    instructions_allocated := instructions_allocated + 1.U
  }
  dontTouch(instructions_allocated)

  class Entry extends Bundle {
    val q = q_t.cloneType

    val is_config = Bool()

    val opa = UDValid(new OpT)
    val opa_is_dst = Bool()
    val opb = UDValid(new OpT)

    // val op1 = UDValid(new OpT)
    // val op1 = UDValid(new OpT)
    // val op2 = UDValid(new OpT)
    // val dst = UDValid(new OpT)

    val issued = Bool()

    val complete_on_issue = Bool()

    val cmd = cmd_t.cloneType

    // instead of one large deps vector, we need 3 separate ones if we want
    // easy indexing, small area while allowing them to be different sizes
    val deps_ld = Vec(reservation_station_entries_ld, Bool())
    val deps_ex = Vec(reservation_station_entries_ex, Bool())
    val deps_st = Vec(reservation_station_entries_st, Bool())

    def ready(dummy: Int = 0): Bool = !(deps_ld.reduce(_ || _) || deps_ex.reduce(_ || _) || deps_st.reduce(_ || _))

    // Debugging signals
    val allocated_at = UInt(instructions_allocated.getWidth.W)
  }

  assert(isPow2(reservation_station_entries_ld))
  assert(isPow2(reservation_station_entries_ex))
  assert(isPow2(reservation_station_entries_st))

  val entries_ld = Reg(Vec(reservation_station_entries_ld, UDValid(new Entry)))
  val entries_ex = Reg(Vec(reservation_station_entries_ex, UDValid(new Entry)))
  val entries_st = Reg(Vec(reservation_station_entries_st, UDValid(new Entry)))

  val entries = entries_ld ++ entries_ex ++ entries_st

  val empty_ld = !entries_ld.map(_.valid).reduce(_ || _)
  val empty_ex = !entries_ex.map(_.valid).reduce(_ || _)
  val empty_st = !entries_st.map(_.valid).reduce(_ || _)
  val full_ld = entries_ld.map(_.valid).reduce(_ && _)
  val full_ex = entries_ex.map(_.valid).reduce(_ && _)
  val full_st = entries_st.map(_.valid).reduce(_ && _)

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  val utilization = PopCount(entries.map(e => e.valid)) // TODO it may be cheaper to count the utilization in a register, rather than performing a PopCount
  val solitary_preload = RegInit(false.B) // This checks whether or not the reservation station received a "preload" instruction, but hasn't yet received the following "compute" instruction
  io.busy := !empty && !(utilization === 1.U && solitary_preload)
  
  // Tell the conv and matmul FSMs if any of their issued instructions completed
  val conv_ld_issue_completed = WireInit(false.B)
  val conv_st_issue_completed = WireInit(false.B)
  val conv_ex_issue_completed = WireInit(false.B)

  val conv_ld_completed = WireInit(false.B)
  val conv_st_completed = WireInit(false.B)
  val conv_ex_completed = WireInit(false.B)

  val matmul_ld_issue_completed = WireInit(false.B)
  val matmul_st_issue_completed = WireInit(false.B)
  val matmul_ex_issue_completed = WireInit(false.B)

  val matmul_ld_completed = WireInit(false.B)
  val matmul_st_completed = WireInit(false.B)
  val matmul_ex_completed = WireInit(false.B)
  
  io.conv_ld_completed := conv_ld_issue_completed +& conv_ld_completed
  io.conv_st_completed := conv_st_issue_completed +& conv_st_completed
  io.conv_ex_completed := conv_ex_issue_completed +& conv_ex_completed

  io.matmul_ld_completed := matmul_ld_issue_completed +& matmul_ld_completed
  io.matmul_st_completed := matmul_st_issue_completed +& matmul_st_completed
  io.matmul_ex_completed := matmul_ex_issue_completed +& matmul_ex_completed

  // Config values set by programmer
  val a_stride = Reg(UInt(a_stride_bits.W))
  val c_stride = Reg(UInt(c_stride_bits.W))
  val a_transpose = Reg(Bool())
  val ld_block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W)))
  val st_block_stride = block_rows.U
  val pooling_is_enabled = Reg(Bool())
  val ld_pixel_repeats = Reg(Vec(load_states, UInt(pixel_repeats_bits.W))) // This is the ld_pixel_repeat MINUS ONE

  val new_entry = Wire(new Entry)
  new_entry := DontCare

  val new_allocs_oh_ld = Wire(Vec(reservation_station_entries_ld, Bool()))
  val new_allocs_oh_ex = Wire(Vec(reservation_station_entries_ex, Bool()))
  val new_allocs_oh_st = Wire(Vec(reservation_station_entries_st, Bool()))

  val new_entry_oh = new_allocs_oh_ld ++ new_allocs_oh_ex ++ new_allocs_oh_st
  new_entry_oh.foreach(_ := false.B)

  val alloc_fire = io.alloc.fire()

  io.alloc.ready := false.B
  when (io.alloc.valid) {
    val spAddrBits = 32
    val cmd = io.alloc.bits.cmd
    val funct = cmd.inst.funct
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    new_entry.issued := false.B
    new_entry.cmd := io.alloc.bits

    new_entry.is_config := funct === CONFIG_CMD

    val op1 = Wire(UDValid(new OpT))
    op1.valid := false.B
    op1.bits := DontCare
    val op2 = Wire(UDValid(new OpT))
    op2.valid := false.B
    op2.bits := DontCare
    val dst = Wire(UDValid(new OpT))
    dst.valid := false.B
    dst.bits := DontCare
    assert(!(op1.valid && op2.valid && dst.valid))

    new_entry.opa_is_dst := dst.valid
    when (dst.valid) {
      new_entry.opa := dst
      new_entry.opb := Mux(op1.valid, op1, op2)
    } .otherwise {
      new_entry.opa := Mux(op1.valid, op1, op2)
      new_entry.opb := op2
    }

    op1.valid := funct === PRELOAD_CMD || funct_is_compute
    op1.bits.start := cmd.rs1.asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      // TODO check b_transpose here iff WS mode is enabled
      val preload_rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48)
      op1.bits.end := op1.bits.start + preload_rows
      op1.bits.wraps_around := op1.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48)
      val cols = cmd.rs1(32 + log2Up(block_cols + 1) - 1, 32)
      val compute_rows = Mux(a_transpose, cols, rows) * a_stride
      op1.bits.end := op1.bits.start + compute_rows
      op1.bits.wraps_around := op1.bits.start.add_with_overflow(compute_rows)._2
    }

    op2.valid := funct_is_compute || funct === STORE_CMD
    op2.bits.start := cmd.rs2.asTypeOf(local_addr_t)
    when (funct_is_compute) {
      val compute_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48)
      op2.bits.end := op2.bits.start + compute_rows
      op2.bits.wraps_around := op2.bits.start.add_with_overflow(compute_rows)._2
    }.elsewhen (pooling_is_enabled) {
      // If pooling is enabled, then we assume that this command simply mvouts everything in this accumulator bank from
      // start to the end of the bank // TODO this won't work when acc_banks =/= 2
      val acc_bank = op2.bits.start.acc_bank()

      val next_bank_addr = WireInit(0.U.asTypeOf(local_addr_t))
      next_bank_addr.is_acc_addr := true.B
      next_bank_addr.data := (acc_bank + 1.U) << local_addr_t.accBankRowBits

      op2.bits.end := next_bank_addr
      op2.bits.wraps_around := next_bank_addr.acc_bank() === 0.U
    }.otherwise {
      val block_stride = st_block_stride

      val mvout_cols = cmd.rs2(32 + mvout_cols_bits - 1, 32)
      val mvout_rows = cmd.rs2(48 + mvout_rows_bits - 1, 48)

      val mvout_mats = mvout_cols / block_cols.U(mvout_cols_bits.W) + (mvout_cols % block_cols.U =/= 0.U)
      val total_mvout_rows = ((mvout_mats - 1.U) * block_stride) + mvout_rows

      op2.bits.end := op2.bits.start + total_mvout_rows
      op2.bits.wraps_around := pooling_is_enabled || op2.bits.start.add_with_overflow(total_mvout_rows)._2
    }

    dst.valid := funct === PRELOAD_CMD || funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD
    dst.bits.start := cmd.rs2(31, 0).asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      val preload_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48) * c_stride
      dst.bits.end := dst.bits.start + preload_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val id = MuxCase(0.U, Seq((new_entry.cmd.cmd.inst.funct === LOAD2_CMD) -> 1.U,
        (new_entry.cmd.cmd.inst.funct === LOAD3_CMD) -> 2.U))
      val block_stride = ld_block_strides(id)
      val pixel_repeats = ld_pixel_repeats(id)

      val mvin_cols = cmd.rs2(32 + mvin_cols_bits - 1, 32)
      val mvin_rows = cmd.rs2(48 + mvin_rows_bits - 1, 48)

      val mvin_mats = mvin_cols / block_cols.U(mvin_cols_bits.W) + (mvin_cols % block_cols.U =/= 0.U)
      val total_mvin_rows = ((mvin_mats - 1.U) * block_stride) + mvin_rows

      // TODO We have to know how the LoopConv's internals work here. Our abstractions are leaking
      if (has_first_layer_optimizations) {
        val start = cmd.rs2(31, 0).asTypeOf(local_addr_t)
        // TODO instead of using a floor-sub that's hardcoded to the Scratchpad bank boundaries, we should find some way of letting the programmer specify the start address
        dst.bits.start := Mux(start.is_acc_addr, start,
          Mux(start.full_sp_addr() > (local_addr_t.spRows / 2).U,
            start.floorSub(pixel_repeats, (local_addr_t.spRows / 2).U)._1,
            start.floorSub(pixel_repeats, 0.U)._1,
          )
        )
      }

      dst.bits.end := dst.bits.start + total_mvin_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(total_mvin_rows)._2
    }

    val is_load = funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_ex = funct === PRELOAD_CMD || funct_is_compute || (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    val is_store = funct === STORE_CMD || (funct === CONFIG_CMD && (config_cmd_type === CONFIG_STORE || config_cmd_type === CONFIG_NORM))
    val is_norm = funct === CONFIG_CMD && config_cmd_type === CONFIG_NORM // normalization commands are a subset of store commands, so they still go in the store queue

    new_entry.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq
    ))

    assert(is_load || is_store || is_ex)
    assert(ldq < exq && exq < stq)

    val not_config = !new_entry.is_config
    when (is_load) {
      // war (after ex/st) | waw (after ex)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !new_entry.is_config && (
        (new_entry.opa.bits.overlaps(e.bits.opa.bits) && e.bits.opa.valid) || // waw if preload, war if compute
        (new_entry.opa.bits.overlaps(e.bits.opb.bits) && e.bits.opb.valid))}) // war

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && e.bits.opa.valid && not_config &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // war
    }.elsewhen (is_ex) {
      // raw (after ld) | war (after st) | waw (after ld)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config && (
        new_entry.opa.bits.overlaps(e.bits.opa.bits) || // waw if preload, raw if compute
        new_entry.opb.bits.overlaps(e.bits.opa.bits))}) // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && e.bits.opa.valid && not_config && new_entry.opa_is_dst &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // war
    }.otherwise {
      // raw (after ld/ex)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && e.bits.opa.valid && not_config &&
        e.bits.opa_is_dst && new_entry.opa.bits.overlaps(e.bits.opa.bits)}) // raw only if ex is preload

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && !e.bits.issued }) // same q
    }

    new_entry.allocated_at := instructions_allocated

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exq

    Seq(
      (ldq, entries_ld, new_allocs_oh_ld, reservation_station_entries_ld),
      (exq, entries_ex, new_allocs_oh_ex, reservation_station_entries_ex),
      (stq, entries_st, new_allocs_oh_st, reservation_station_entries_st))
      .foreach { case (q, entries_type, new_allocs_type, entries_count) =>
        when (new_entry.q === q) {
          val is_full = PopCount(Seq(dst.valid, op1.valid, op2.valid)) > 1.U
          when (q =/= exq) { assert(!is_full) }

          // looking for the first invalid entry
          val alloc_id = MuxCase((entries_count - 1).U, entries_type.zipWithIndex.map { case (e, i) => !e.valid -> i.U })

          when (!entries_type(alloc_id).valid) {
            io.alloc.ready := true.B
            entries_type(alloc_id).valid := true.B
            entries_type(alloc_id).bits := new_entry
            new_allocs_type(alloc_id) := true.B
          }
        }
      }

    when (io.alloc.fire) {
      when (new_entry.is_config && new_entry.q === exq) {
        a_stride := new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        c_stride := new_entry.cmd.cmd.rs2(63, 48) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        val set_only_strides = new_entry.cmd.cmd.rs1(7) // TODO magic numbers
        when (!set_only_strides) {
          a_transpose := new_entry.cmd.cmd.rs1(8) // TODO magic numbers
        }
      }.elsewhen(new_entry.is_config && new_entry.q === ldq) {
        val id = new_entry.cmd.cmd.rs1(4,3) // TODO magic numbers
        val block_stride = new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers
        val repeat_pixels = maxOf(new_entry.cmd.cmd.rs1(8 + pixel_repeats_bits - 1, 8), 1.U) // TODO we use a default value of pixel repeats here, for backwards compatibility. However, we should deprecate and remove this default value eventually
        ld_block_strides(id) := block_stride
        ld_pixel_repeats(id) := repeat_pixels - 1.U
      }.elsewhen(new_entry.is_config && new_entry.q === stq && !is_norm) {
        val pool_stride = new_entry.cmd.cmd.rs1(5, 4) // TODO magic numbers
        pooling_is_enabled := pool_stride =/= 0.U
      }.elsewhen(funct === PRELOAD_CMD) {
        solitary_preload := true.B
      }.elsewhen(funct_is_compute) {
        solitary_preload := false.B
      }
    }
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld, entries_ld), (exq, io.issue.ex, entries_ex), (stq, io.issue.st, entries_st))
    .foreach { case (q, io, entries_type) =>

    val issue_valids = entries_type.map(e => e.valid && e.bits.ready() && !e.bits.issued)
    val issue_sel = PriorityEncoderOH(issue_valids)
    val issue_id = OHToUInt(issue_sel)
    val global_issue_id = Cat(q.asUInt, issue_id.pad(log2Up(res_max_per_type)))
    assert(q.getWidth == 2)
    assert(global_issue_id.getWidth == 2 + log2Up(res_max_per_type))

    val issue_entry = Mux1H(issue_sel, entries_type)

    io.valid := issue_valids.reduce(_||_)
    io.cmd := issue_entry.bits.cmd
    // use the most significant 2 bits to indicate instruction type
    io.rob_id := global_issue_id

    val complete_on_issue = entries_type(issue_id).bits.complete_on_issue
    val from_conv_fsm = entries_type(issue_id).bits.cmd.from_conv_fsm
    val from_matmul_fsm = entries_type(issue_id).bits.cmd.from_matmul_fsm

    when (io.fire()) {
      entries_type.zipWithIndex.foreach { case (e, i) =>
        when (issue_sel(i)) {
          e.bits.issued := true.B
          e.valid := !e.bits.complete_on_issue
        }
      }

      // Update the "deps" vectors of all instructions which depend on the one that is being issued
      Seq((ldq, entries_ld), (exq, entries_ex), (stq, entries_st))
        .foreach { case (q_, entries_type_) =>

        entries_type_.zipWithIndex.foreach { case (e, i) =>
          val deps_type = if (q == ldq) e.bits.deps_ld
                          else if (q == exq) e.bits.deps_ex else e.bits.deps_st
          when (q === q_) {
            deps_type(issue_id) := false.B
          }.otherwise {
            when (issue_entry.bits.complete_on_issue) {
              deps_type(issue_id) := false.B
            }
          }
        }
      }

      // If the instruction completed on issue, then notify the conv/matmul FSMs that another one of their commands
      // completed
      when (q === ldq) { conv_ld_issue_completed := complete_on_issue && from_conv_fsm }
      when (q === stq) { conv_st_issue_completed := complete_on_issue && from_conv_fsm }
      when (q === exq) { conv_ex_issue_completed := complete_on_issue && from_conv_fsm }

      when (q === ldq) { matmul_ld_issue_completed := complete_on_issue && from_matmul_fsm }
      when (q === stq) { matmul_st_issue_completed := complete_on_issue && from_matmul_fsm }
      when (q === exq) { matmul_ex_issue_completed := complete_on_issue && from_matmul_fsm }
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire) {
    val type_width = log2Up(res_max_per_type)
    val queue_type = io.completed.bits(type_width + 1, type_width)
    val issue_id = io.completed.bits(type_width - 1, 0)

    when (queue_type === ldq) {
      entries.foreach(_.bits.deps_ld(issue_id) := false.B)
      entries_ld(issue_id).valid := false.B

      conv_ld_completed := entries_ld(issue_id).bits.cmd.from_conv_fsm
      matmul_ld_completed := entries_ld(issue_id).bits.cmd.from_matmul_fsm

      assert(entries_ld(issue_id).valid)
    }.elsewhen (queue_type === exq) {
      entries.foreach(_.bits.deps_ex(issue_id) := false.B)
      entries_ex(issue_id).valid := false.B

      conv_ex_completed := entries_ex(issue_id).bits.cmd.from_conv_fsm
      matmul_ex_completed := entries_ex(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_ex(issue_id).valid)
    }.elsewhen (queue_type === stq) {
      entries.foreach(_.bits.deps_st(issue_id) := false.B)
      entries_st(issue_id).valid := false.B

      conv_st_completed := entries_st(issue_id).bits.cmd.from_conv_fsm
      matmul_st_completed := entries_st(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_st(issue_id).valid)
    }.otherwise {
      assert(queue_type =/= 3.U)
    }
  }

  // Explicitly mark "opb" in all ld/st queues entries as being invalid.
  // This helps us to reduce the total reservation table area
  Seq(entries_ld, entries_st).foreach { entries_type =>
    entries_type.foreach { e =>
      e.bits.opb.valid := false.B
      e.bits.opb.bits := DontCare
    }
  }

  // val utilization = PopCount(entries.map(e => e.valid))
  val utilization_ld_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === ldq))
  val utilization_st_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === stq))
  val utilization_ex_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === exq))
  val utilization_ld_q = PopCount(entries_ld.map(e => e.valid))
  val utilization_st_q = PopCount(entries_st.map(e => e.valid))
  val utilization_ex_q = PopCount(entries_ex.map(e => e.valid))

  val valids = VecInit(entries.map(_.valid))
  val functs = VecInit(entries.map(_.bits.cmd.cmd.inst.funct))
  val issueds = VecInit(entries.map(_.bits.issued))
  val packed_deps = VecInit(entries.map(e =>
    Cat(Cat(e.bits.deps_ld.reverse), Cat(e.bits.deps_ex.reverse), Cat(e.bits.deps_st.reverse))))

  dontTouch(valids)
  dontTouch(functs)
  dontTouch(issueds)
  dontTouch(packed_deps)

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid,
    PopCount(e.bits.deps_ld) + PopCount(e.bits.deps_ex) + PopCount(e.bits.deps_st), 0.U)))
  val min_pop_count = pop_count_packed_deps.reduce((acc, d) => minOf(acc, d))
  // assert(min_pop_count < 2.U)
  dontTouch(pop_count_packed_deps)
  dontTouch(min_pop_count)

  val cycles_since_issue = RegInit(0.U(16.W))

  when (io.issue.ld.fire() || io.issue.st.fire() || io.issue.ex.fire() || !io.busy || io.completed.fire) {
    cycles_since_issue := 0.U
  }.elsewhen(io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < PlusArg("gemmini_timeout", 10000), "pipeline stall")

  for (e <- entries) {
    dontTouch(e.bits.allocated_at)
  }

  val cntr = Counter(2000000)
  when (cntr.inc()) {
    printf(p"Utilization: $utilization\n")
    printf(p"Utilization ld q (incomplete): $utilization_ld_q_unissued\n")
    printf(p"Utilization st q (incomplete): $utilization_st_q_unissued\n")
    printf(p"Utilization ex q (incomplete): $utilization_ex_q_unissued\n")
    printf(p"Utilization ld q: $utilization_ld_q\n")
    printf(p"Utilization st q: $utilization_st_q\n")
    printf(p"Utilization ex q: $utilization_ex_q\n")

    if (use_firesim_simulation_counters) {
      printf(SynthesizePrintf("Utilization: %d\n", utilization))
      printf(SynthesizePrintf("Utilization ld q (incomplete): %d\n", utilization_ld_q_unissued))
      printf(SynthesizePrintf("Utilization st q (incomplete): %d\n", utilization_st_q_unissued))
      printf(SynthesizePrintf("Utilization ex q (incomplete): %d\n", utilization_ex_q_unissued))
      printf(SynthesizePrintf("Utilization ld q: %d\n", utilization_ld_q))
      printf(SynthesizePrintf("Utilization st q: %d\n", utilization_st_q))
      printf(SynthesizePrintf("Utilization ex q: %d\n", utilization_ex_q))
    }

    printf(p"Packed deps: $packed_deps\n")
  }

  if (use_firesim_simulation_counters) {
    PerfCounter(io.busy, "reservation_station_busy", "cycles where reservation station has entries")
    PerfCounter(!io.alloc.ready, "reservation_station_full", "cycles where reservation station is full")
  }

  when (reset.asBool) {
    entries.foreach(_.valid := false.B)
  }

  CounterEventIO.init(io.counter)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_LD_COUNT, utilization_ld_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_ST_COUNT, utilization_st_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_EX_COUNT, utilization_ex_q)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_ACTIVE_CYCLES, io.busy)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_FULL_CYCLES, !io.alloc.ready)
}

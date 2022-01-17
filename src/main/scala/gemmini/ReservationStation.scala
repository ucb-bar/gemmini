
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
class ReservationStationIssue[T <: Data](cmd_t: T, rob_entries: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(log2Up(rob_entries).W))

  def fire(dummy: Int=0) = valid && ready

}

// TODO we don't need to store the full command in here. We should be able to release the command directly into the relevant controller and only store the associated metadata in the ROB. This would reduce the size considerably
class ReservationStation[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], cmd_t: RoCCCommand) extends Module {
  import config._

  val block_rows = tileRows * meshRows
  val block_cols = tileColumns * meshColumns

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(log2Up(rob_entries).W)))

    val issue = new Bundle {
      val ld = new ReservationStationIssue(cmd_t, rob_entries)
      val st = new ReservationStationIssue(cmd_t, rob_entries)
      val ex = new ReservationStationIssue(cmd_t, rob_entries)
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

    val deps = Vec(rob_entries, Bool())
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _)

    // Debugging signals
    val allocated_at = UInt(instructions_allocated.getWidth.W)
  }
  val full_entries = Reg(Vec(reservation_station_full_entries, UDValid(new Entry)))
  val partial_entries = Reg(Vec(reservation_station_partial_entries, UDValid(new Entry)))

  val entries = full_entries ++ partial_entries

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  // TODO we could also check for a solitary preload by recording the last instruction that was allocated, rather than
  // reading all entries to check for preloads, which is an O(n) operation in terms of area cost
  val utilization = PopCount(entries.map(_.valid))
  val solitary_preload = utilization === 1.U && entries.map(e => e.valid && e.bits.cmd.inst.funct === PRELOAD_CMD).reduce(_ || _)
  io.busy := !empty && !(solitary_preload && io.solitary_preload)

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
  val new_full_allocs = Wire(Vec(reservation_station_full_entries, Bool()))
  new_full_allocs.foreach(_ := false.B)
  val new_partial_allocs = Wire(Vec(reservation_station_partial_entries, Bool()))
  new_partial_allocs.foreach(_ := false.B)
  val new_entry_oh = new_full_allocs ++ new_partial_allocs
  val alloc_fire = io.alloc.fire

  val raws_probe = WireInit(0.U(rob_entries.W))
  val waws_probe = WireInit(0.U(rob_entries.W))
  val wars_probe = WireInit(0.U(rob_entries.W))
  val older_in_same_q_probe = WireInit(0.U(rob_entries.W))
  val is_st_and_must_wait_for_prior_ex_config_probe = WireInit(0.U(rob_entries.W))
  val is_ex_config_and_must_wait_for_prior_st_probe = WireInit(0.U(rob_entries.W))

  val wars_op1_probe = WireInit(0.U(rob_entries.W))
  val wars_op2_probe = WireInit(0.U(rob_entries.W))
  val raws_op1_probe = WireInit(0.U(rob_entries.W))
  val raws_op2_probe = WireInit(0.U(rob_entries.W))

  dontTouch(raws_probe)
  dontTouch(waws_probe)
  dontTouch(wars_probe)
  dontTouch(wars_op1_probe)
  dontTouch(wars_op2_probe)
  dontTouch(raws_op1_probe)
  dontTouch(raws_op2_probe)
  dontTouch(older_in_same_q_probe)
  dontTouch(is_st_and_must_wait_for_prior_ex_config_probe)
  dontTouch(is_ex_config_and_must_wait_for_prior_st_probe)

  dontTouch(new_entry)
  io.alloc.ready := false.B
  when (io.alloc.valid) {
    val spAddrBits = 32
    val cmd = io.alloc.bits
    val funct = cmd.inst.funct
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    new_entry.issued := false.B
    new_entry.cmd := cmd

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

      val mvout_mats = mvout_cols / block_cols.U + (mvout_cols % block_cols.U =/= 0.U)
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
      val id = MuxCase(0.U, Seq((new_entry.cmd.inst.funct === LOAD2_CMD) -> 1.U,
        (new_entry.cmd.inst.funct === LOAD3_CMD) -> 2.U))
      val block_stride = ld_block_strides(id)
      val pixel_repeats = ld_pixel_repeats(id)

      val mvin_cols = cmd.rs2(32 + mvin_cols_bits - 1, 32)
      val mvin_rows = cmd.rs2(48 + mvin_rows_bits - 1, 48)

      val mvin_mats = mvin_cols / block_cols.U + (mvin_cols % block_cols.U =/= 0.U)
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
    val is_store = funct === STORE_CMD || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = funct === PRELOAD_CMD || funct_is_compute || (funct === CONFIG_CMD && (config_cmd_type === CONFIG_EX || config_cmd_type === CONFIG_IM2COL))
    val is_im2col = funct === CONFIG_CMD && config_cmd_type === CONFIG_IM2COL // im2col commands are a subset of ex commands, so they still go in the ex queue

    new_entry.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq
    ))

    assert(is_load || is_store || is_ex)
    // This can be RAW op1/op2 <- dst, or WAW dst <- dst
    val opa_matches_opa = VecInit(entries.map { e => e.valid && e.bits.opa.valid && new_entry.opa.bits.overlaps(e.bits.opa.bits) })
    // This can be WAR dst <- op1/op2
    val opa_matches_opb = VecInit(entries.map { e => e.valid && e.bits.opb.valid && new_entry.opa.bits.overlaps(e.bits.opb.bits) })
    // This can be RAW op2 <- dst
    val opb_matches_opa = VecInit(entries.map { e => e.valid && e.bits.opa.valid && new_entry.opb.bits.overlaps(e.bits.opa.bits) })

    val op1_matches_opa = VecInit((entries zip (opa_matches_opa zip opb_matches_opa)).map { case (e, (a, b)) =>
      e.valid && op1.valid && Mux(dst.valid, b, a)
    })
    val op2_matches_opa = VecInit((entries zip (opa_matches_opa zip opb_matches_opa)).map { case (e, (a, b)) =>
      e.valid && op2.valid && Mux(dst.valid || op1.valid, b, a)
    })
    val dst_matches_opa = VecInit((entries zip opa_matches_opa).map { case (e, a) =>
      e.valid && dst.valid && a
    })
    val dst_matches_opb = VecInit((entries zip opa_matches_opb).map { case (e, b) =>
      e.valid && dst.valid && b
    })

    val op1_raws_opa = VecInit((entries zip op1_matches_opa).map { case (e, m) =>
      m && op1.valid && e.bits.q =/= new_entry.q && e.bits.opa_is_dst
    })
    val op2_raws_opa = VecInit((entries zip op2_matches_opa).map { case (e, m) =>
      m && op2.valid && e.bits.q =/= new_entry.q && e.bits.opa_is_dst
    })
    val raws = VecInit((op1_raws_opa zip op2_raws_opa).map { case (a, b) => a || b })

    val dst_wars_opa = VecInit((entries zip dst_matches_opa).map { case (e, m) =>
      m && dst.valid && e.bits.q =/= new_entry.q && !e.bits.opa_is_dst
    })
    val dst_wars_opb = VecInit((entries zip dst_matches_opb).map { case (e, m) =>
      m && dst.valid && e.bits.q =/= new_entry.q
    })
    val wars = VecInit((dst_wars_opa zip dst_wars_opb).map { case (a, b) => a || b })

    val dst_waws_opa = VecInit((entries zip dst_matches_opa).map { case (e, m) =>
      m && dst.valid && (e.bits.q =/= new_entry.q || new_entry.q === ldq) && e.bits.opa_is_dst
    })
    val waws = dst_waws_opa

    val older_in_same_q = VecInit(entries.map { e =>
      e.valid && e.bits.q === new_entry.q && !e.bits.issued
    })

    val is_st_and_must_wait_for_prior_ex_config = VecInit(entries.map { e =>
      e.valid && new_entry.q === stq && !new_entry.is_config && e.bits.q === exq && e.bits.is_config
    })

    val is_ex_config_and_must_wait_for_prior_st = VecInit(entries.map { e =>
      // TODO when acc reads no longer rely upon config-ex's relu6, this dependency can be broken
      e.valid && new_entry.q === exq && new_entry.is_config && e.bits.q === stq && !e.bits.is_config
    })

    new_entry.deps := (Cat(raws) | Cat(wars) | Cat(waws) | Cat(older_in_same_q) |
      Cat(is_st_and_must_wait_for_prior_ex_config) | Cat(is_ex_config_and_must_wait_for_prior_st)).asBools().reverse

    raws_probe := Cat(raws.reverse)
    waws_probe := Cat(waws.reverse)
    wars_probe := Cat(wars.reverse)
    older_in_same_q_probe := Cat(older_in_same_q.reverse)
    is_st_and_must_wait_for_prior_ex_config_probe := Cat(is_st_and_must_wait_for_prior_ex_config.reverse)
    is_ex_config_and_must_wait_for_prior_st_probe := Cat(is_ex_config_and_must_wait_for_prior_st.reverse)

    new_entry.allocated_at := instructions_allocated

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exq

    val is_full = PopCount(Seq(dst.valid, op1.valid, op2.valid)) > 1.U
    val full_alloc_id = MuxCase((reservation_station_full_entries-1).U, full_entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })
    val partial_alloc_id = MuxCase((reservation_station_partial_entries-1).U, partial_entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })

    when (!is_full && !partial_entries(partial_alloc_id).valid) {
      io.alloc.ready := true.B
      partial_entries(partial_alloc_id).valid := true.B
      partial_entries(partial_alloc_id).bits := new_entry
      partial_entries(partial_alloc_id).bits.opb.valid := false.B
      partial_entries(partial_alloc_id).bits.opb.bits := DontCare
      new_partial_allocs(partial_alloc_id) := true.B
    } .elsewhen (!full_entries(full_alloc_id).valid) {
      io.alloc.ready := true.B
      full_entries(full_alloc_id).valid := true.B
      full_entries(full_alloc_id).bits := new_entry
      new_full_allocs(full_alloc_id) := true.B
    }

    when (io.alloc.fire) {
      when (new_entry.is_config && new_entry.q === exq && !is_im2col) {
        a_stride := new_entry.cmd.rs1(31, 16) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        c_stride := new_entry.cmd.rs2(63, 48) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        val set_only_strides = new_entry.cmd.rs1(7) // TODO magic numbers
        when (!set_only_strides) {
          a_transpose := new_entry.cmd.rs1(8) // TODO magic numbers
        }
      }.elsewhen(new_entry.is_config && new_entry.q === ldq) {
        val id = new_entry.cmd.rs1(4,3) // TODO magic numbers
        val block_stride = new_entry.cmd.rs1(31, 16) // TODO magic numbers
        val repeat_pixels = maxOf(new_entry.cmd.rs1(8 + pixel_repeats_bits - 1, 8), 1.U) // TODO we use a default value of pixel repeats here, for backwards compatibility. However, we should deprecate and remove this default value eventually
        ld_block_strides(id) := block_stride
        ld_pixel_repeats(id) := repeat_pixels - 1.U
      }.elsewhen(new_entry.is_config && new_entry.q === stq) {
        val pool_stride = new_entry.cmd.rs1(5, 4) // TODO magic numbers
        pooling_is_enabled := pool_stride =/= 0.U
      }
    }
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld), (stq, io.issue.st), (exq, io.issue.ex)).foreach { case (q, io) =>
    val issue_valids = entries.map(e => e.valid && e.bits.ready() && !e.bits.issued && e.bits.q === q)
    val issue_sel = PriorityEncoderOH(issue_valids)
    val issue_id = OHToUInt(issue_sel)
    val issue_entry = Mux1H(issue_sel, entries)

    io.valid := issue_valids.reduce(_||_)
    io.cmd := issue_entry.bits.cmd
    io.rob_id := OHToUInt(issue_sel)

    when (io.fire()) {
      // Clear out all the dependency bits for instructions which depend on the same queue
      entries.zipWithIndex.foreach { case (e, i) =>
        val is_same_q = Mux(alloc_fire && new_entry_oh(i),
          new_entry.q === issue_entry.bits.q,
          e.bits.q === issue_entry.bits.q)

        when (is_same_q || issue_entry.bits.complete_on_issue) {
          e.bits.deps(issue_id) := false.B
        }
      }
      for ((e, i) <- entries.zipWithIndex) {
        when (issue_sel(i)) {
          e.bits.issued := true.B
          e.valid := !e.bits.complete_on_issue
        }
      }
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire) {
    entries.foreach(_.bits.deps(io.completed.bits) := false.B)

    for ((e, i) <- entries.zipWithIndex) {
      when (i.U === io.completed.bits) {
        e.valid := false.B
        assert(e.valid)
      }
    }
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

  val valids = VecInit(entries.map(_.valid))
  val functs = VecInit(entries.map(_.bits.cmd.inst.funct))
  val issueds = VecInit(entries.map(_.bits.issued))
  val packed_deps = VecInit(entries.map(e => Cat(e.bits.deps.reverse)))

  dontTouch(valids)
  dontTouch(functs)
  dontTouch(issueds)
  dontTouch(packed_deps)

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid, PopCount(e.bits.deps), 0.U)))
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

  when (reset.asBool()) {
    entries.foreach(_.valid := false.B)
  }

  CounterEventIO.init(io.counter)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_LD_COUNT, utilization_ld_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_ST_COUNT, utilization_st_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_EX_COUNT, utilization_ex_q)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_ACTIVE_CYCLES, io.busy)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_FULL_CYCLES, !io.alloc.ready)
}

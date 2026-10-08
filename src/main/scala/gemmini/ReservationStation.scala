
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

  def fire = valid && ready
}

// TODO we don't need to store the full command in here. We should be able to release the command directly into the relevant controller and only store the associated metadata in the ROB. This would reduce the size considerably
class ReservationStation[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V],
                                                                       cmd_t: GemminiCmd) extends Module {
  import config._

  val block_rows = tileRows * meshRows
  val block_cols = tileColumns * meshColumns

  private val n_vec_entries = if (has_vpu || has_spad_requant) res_max_per_type min 16 else 1   // = n_vec below
  val n_all_entries = reservation_station_entries_ld + reservation_station_entries_ex + reservation_station_entries_st +
    n_vec_entries

  val max_instructions_completed_per_type_per_cycle = 2 // Every cycle, at most two instructions of a single "type" (ld/st/ex) can be completed: one through the io.completed port, and the other if it is a "complete-on-issue" instruction

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(ROB_ID_WIDTH.W)))

    val issue = new Bundle {
      val ld = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val st = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val ex = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val vec = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)   // VPU_EXEC / SPAD_REQUANT
    }

    val conv_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val conv_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val conv_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val matmul_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val busy = Output(Bool())

    // vector queue: 0 = VPU idle, 1 = SPAD_REQUANT can start, 2 = an FP4 SPAD_REQUANT can start (requantizer
    // drained of every beat); spad banks with requant rows still to be written
    val vec_unit_ready = Input(Vec(3, Bool()))
    val vec_sr_fp4_waiting = Output(Bool())   // an FP4 SPAD_REQUANT is next and only waits for its unit
    val vec_pending_banks = Input(UInt(sp_banks.W))
    // MX single-element mode: a tile's preload C address is its 16-row acc group + a column-group offset (j * 4)
    val mx_packed_acc = Input(Bool())
    val vec_free = Output(UInt(5.W))   // free vector-queue entries
    val ld_free = Output(UInt(log2Up(reservation_station_entries_ld + 1).W))   // free load-queue entries
    // has_loop_retire_counter: every entry's valid bit, and the entry allocated this cycle (one-hot), both in the
    // order ld ++ ex ++ st ++ vec
    val entry_valid = Option.when(has_loop_retire_counter)(Output(UInt(n_all_entries.W)))
    val alloc_oh = Option.when(has_loop_retire_counter)(Output(UInt(n_all_entries.W)))

    val counter = new CounterEventIO()
  })

  // val ldq :: exq :: stq :: Nil = Enum(3)
  val ldq = 0
  val exq = 1
  val stq = 2
  val q_t = UInt(2.W)
  val ldqu = 0.U(2.W)
  val exqu = 1.U(2.W)
  val stqu = 2.U(2.W)
  // 4th queue: VPU_EXEC / SPAD_REQUANT (one entry, never allocated, when neither unit is built)
  val vecq = 3
  val vecqu = 3.U(2.W)
  val has_vec = has_vpu || has_spad_requant
  val n_vec = if (has_vec) res_max_per_type min 16 else 1   // a block's vector work enters at once (ids fit the per-type width)

  class OpT extends Bundle {
    val start = local_addr_t.cloneType
    val end = local_addr_t.cloneType
    val wraps_around = Bool()

    // conservative spad bank mask of [start, end)
    def sp_banks_mask(dummy: Int = 0): UInt = {
      val last = WireInit(end); last.data := Mux(end.data === start.data, end.data, end.data - 1.U)   // end is exclusive
      val lo = start.sp_bank(); val hi = last.sp_bank()
      val all = wraps_around || hi < lo || end.is_acc_addr
      Mux(start.is_acc_addr || start.is_garbage(), 0.U,
        VecInit((0 until sp_banks).map(b => all || (b.U >= lo && b.U <= hi))).asUInt)
    }

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
    val deps_vec = Vec(n_vec, Bool())
    val opc = UDValid(new OpT)   // vector entries only: src2
    val opd = UDValid(new OpT)   // vector entries only: second destination (EXPSUM row sums)
    val reads_act0 = Bool()      // ex entries: issued under a scale config that may read act half 0

    def ready(dummy: Int = 0): Bool = !(deps_ld.reduce(_ || _) || deps_ex.reduce(_ || _) || deps_st.reduce(_ || _) ||
      deps_vec.reduce(_ || _))

    // Debugging signals
    val allocated_at = UInt(instructions_allocated.getWidth.W)
  }

  assert(isPow2(reservation_station_entries_ld))
  assert(isPow2(reservation_station_entries_ex))
  assert(isPow2(reservation_station_entries_st))

  val entries_ld = Reg(Vec(reservation_station_entries_ld, UDValid(new Entry)))
  val entries_ex = Reg(Vec(reservation_station_entries_ex, UDValid(new Entry)))
  val entries_st = Reg(Vec(reservation_station_entries_st, UDValid(new Entry)))
  val entries_vec = Reg(Vec(n_vec, UDValid(new Entry)))

  val entries = entries_ld ++ entries_ex ++ entries_st ++ entries_vec

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
  io.vec_free := PopCount(entries_vec.map(!_.valid))
  io.ld_free := PopCount(entries_ld.map(!_.valid))
  
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
  val a_stride = RegInit(0.U(a_stride_bits.W))
  val c_stride = RegInit(0.U(c_stride_bits.W))
  val a_transpose = RegInit(false.B)
  val ld_block_strides = RegInit(0.U.asTypeOf(Vec(load_states, UInt(block_stride_bits.W))))
  val st_block_stride = block_rows.U
  val pooling_is_enabled = RegInit(false.B)
  val ld_pixel_repeats = RegInit(0.U.asTypeOf(Vec(load_states, UInt(pixel_repeats_bits.W)))) // This is the ld_pixel_repeat MINUS ONE

  val new_entry = Wire(new Entry)
  new_entry := DontCare

  val new_allocs_oh_ld = Wire(Vec(reservation_station_entries_ld, Bool()))
  val new_allocs_oh_ex = Wire(Vec(reservation_station_entries_ex, Bool()))
  val new_allocs_oh_st = Wire(Vec(reservation_station_entries_st, Bool()))
  val new_allocs_oh_vec = Wire(Vec(n_vec, Bool()))

  val new_entry_oh = new_allocs_oh_ld ++ new_allocs_oh_ex ++ new_allocs_oh_st ++ new_allocs_oh_vec
  new_entry_oh.foreach(_ := false.B)
  require(entries.length == n_all_entries)
  io.entry_valid.foreach(_ := VecInit(entries.map(_.valid)).asUInt)
  io.alloc_oh.foreach(_ := VecInit(new_entry_oh).asUInt)

  val alloc_fire = io.alloc.fire

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

    op2.valid := funct_is_compute || funct === STORE_CMD || funct === STORE_SPAD_CMD
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

    dst.valid := funct === PRELOAD_CMD || funct === STORE_SPAD_CMD ||
      funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD
    dst.bits.start := cmd.rs2(31, 0).asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      val preload_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48) * c_stride
      // packed MX acc: the low bits of the C address pick a column group within the row group, not a row; range the
      // group's rows (a range that starts mid-group would claim the next group's rows: false WAR on its stores)
      when (io.mx_packed_acc && dst.bits.start.is_acc_addr) {
        dst.bits.start.data := cmd.rs2(31, 0).asTypeOf(local_addr_t).data & (~(block_rows - 1).U(dst.bits.start.data.getWidth.W)).asUInt
      }
      dst.bits.end := dst.bits.start + preload_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(preload_rows)._2
    }.elsewhen(funct === STORE_SPAD_CMD) {
      // TODO: make it so that spad move has its own load config states
      val mv_cols = cmd.rs2(32 + mvout_cols_bits - 1, 32)
      val mv_rows = cmd.rs2(48 + mvout_rows_bits - 1, 48)
      // MX: an absolute row step (rs1 bit 63) spreads the rows: [dst, dst + rows*step + 16) also covers the tiled
      // layout's second beat 16 rows below each row
      val st_step = cmd.rs1(62, 32)
      // BF16 store (rs2[63], from StCSpad): row r writes its beats (<= 8) to consecutive rows at dst + r*step, so the
      // exact span is (rows-1)*step + 8; other flagged stores keep the conservative rows*step + 16 (tiled beats)
      val mvout_dst_rows = if (use_mx_scaling) Mux(cmd.rs1(63),
        Mux(cmd.rs2(63), (mv_rows - 1.U) * st_step + 8.U, mv_rows * st_step + 16.U), mv_rows) else mv_rows

      dst.bits.start := cmd.rs1(31, 0).asTypeOf(local_addr_t)
      dst.bits.end := dst.bits.start + mvout_dst_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(mvout_dst_rows.asUInt)._2

      assert(!pooling_is_enabled, "cannot pool while moving between internal memories")
      assert(!dst.bits.start.is_acc_addr, "cannot move to accumulator memory")
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
    val is_ex = funct === PRELOAD_CMD || funct_is_compute || (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX) || (funct === CONFIG_SCALE_MEM)
    val is_store = funct === STORE_CMD || funct === STORE_SPAD_CMD || (funct === CONFIG_CMD && (config_cmd_type === CONFIG_STORE || config_cmd_type === CONFIG_NORM))
    val is_norm = funct === CONFIG_CMD && config_cmd_type === CONFIG_NORM // normalization commands are a subset of store commands, so they still go in the store queue

    val is_vpu = has_vpu.B && funct === VPU_EXEC
    val is_sreq = has_spad_requant.B && funct === SPAD_REQUANT
    val is_vec = is_vpu || is_sreq

    new_entry.q := Mux1H(Seq(
      is_load -> ldqu,
      is_store -> stqu,
      is_ex -> exqu,
      is_vec -> vecqu
    ))

    assert(is_load || is_store || is_ex || is_vec)

    // vector entries: opa = dst (written), opb = src1, opc = src2; spad row ranges (rows / M*N sizes, conservative)
    def spRange(start: UInt, n: UInt): UDValid[OpT] = {
      val o = Wire(UDValid(new OpT))
      val la = WireInit(0.U.asTypeOf(local_addr_t))
      la.data := start
      o.valid := true.B
      o.bits.start := la
      o.bits.end := la + n
      o.bits.wraps_around := (start +& n) > local_addr_t.spRows.U
      o
    }
    new_entry.opc.valid := false.B
    new_entry.opc.bits := DontCare
    new_entry.opd.valid := false.B
    new_entry.opd.bits := DontCare
    when (is_vpu) {
      val vc = gemmini.vpu.VpuCmd.decode(cmd.rs1, cmd.rs2, 14)
      new_entry.opa_is_dst := true.B
      new_entry.opa := spRange(vc.dst, gemmini.vpu.VpuCmd.dstRows(vc))
      new_entry.opb := spRange(vc.src1, vc.rows)
      new_entry.opc := spRange(vc.src2, gemmini.vpu.VpuCmd.src2Rows(vc))
      new_entry.opc.valid := gemmini.vpu.VpuOp.usesSrc2(vc.op)
      if (vpu_params.expSum) {
        new_entry.opd := spRange(vc.dst2, gemmini.vpu.VpuCmd.dst2Rows(vc))
        new_entry.opd.valid := gemmini.vpu.VpuOp.hasDst2(vc.op)
      }
    }.elsewhen (is_sreq) {
      val sc = SpadRequantCmd.decode(cmd.rs1, cmd.rs2, 14)
      val m16 = (sc.m +& 15.U) & ~15.U(17.W)
      val m32 = (sc.m +& 31.U) & ~31.U(17.W)
      new_entry.opa_is_dst := true.B
      new_entry.opa := spRange(sc.dst, Mux(sc.fp4, Mux(sc.tiled, (m32 * sc.n) >> 5, (sc.m * sc.n) >> 5),
                                                   Mux(sc.tiled, (m16 * sc.n) >> 4, (sc.m * sc.n) >> 4)))
      new_entry.opb := spRange(sc.src, (sc.m * sc.n) >> 3)
    }

    val not_config = !new_entry.is_config
    when (is_load) {
      // war/waw (after ex) | war/waw (after st)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !new_entry.is_config && (
        (new_entry.opa.bits.overlaps(e.bits.opa.bits) && e.bits.opa.valid) || // waw if preload, war if compute
        (new_entry.opa.bits.overlaps(e.bits.opb.bits) && e.bits.opb.valid))}) // war

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && not_config && (
        (new_entry.opa.bits.overlaps(e.bits.opa.bits) && e.bits.opa.valid) || // waw if st_spad, war otherwise
        (new_entry.opa.bits.overlaps(e.bits.opb.bits) && e.bits.opb.valid))}) // war if st_spad
    }.elsewhen (is_ex) {
      // raw/waw (after ld) | war/waw/raw (after st)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config && (
        new_entry.opa.bits.overlaps(e.bits.opa.bits) || // waw if preload, raw if compute
        new_entry.opb.bits.overlaps(e.bits.opa.bits))}) // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && e.bits.opa.valid && not_config &&
        Mux(e.bits.opa_is_dst,
          // if st writes, raw/waw for ex a/b <- st a
          new_entry.opa.bits.overlaps(e.bits.opa.bits) || new_entry.opb.bits.overlaps(e.bits.opa.bits) ||
          // additionally if ex writes, war for ex a <- st b
            (new_entry.opa_is_dst && new_entry.opa.bits.overlaps(e.bits.opb.bits))
        ,
          // if st only reads, only check ex writes, war for ex a <- st a
          new_entry.opa_is_dst && new_entry.opa.bits.overlaps(e.bits.opa.bits)
        )
      })
    }.otherwise {
      assert((!new_entry.opa_is_dst) || new_entry.opb.valid)
      // raw (after ld/ex), waw/war if destination is spad
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config && (
        new_entry.opa.bits.overlaps(e.bits.opa.bits) || // waw/raw
        new_entry.opb.valid && new_entry.opb.bits.overlaps(e.bits.opa.bits))}) // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && not_config &&
        Mux(new_entry.opa_is_dst,
          // if st writes, war/waw for st a <- ex a/b
          (new_entry.opa.bits.overlaps(e.bits.opa.bits) && e.bits.opa.valid) ||
            (new_entry.opa.bits.overlaps(e.bits.opb.bits) && e.bits.opb.valid) ||
          // additionally if ex writes, raw for st b <- ex a
            (e.bits.opa.valid && e.bits.opa_is_dst && new_entry.opb.bits.overlaps(e.bits.opa.bits))
        ,
          // if st only reads, only check ex writes, raw for st a <- ex a
          e.bits.opa.valid && e.bits.opa_is_dst && new_entry.opa.bits.overlaps(e.bits.opa.bits)
        )
      })

      // new_entry.deps_st := VecInit(entries_st.map { e => e.valid && !e.bits.issued }) // same q
      new_entry.deps_st := VecInit(entries_st.map { e => e.valid }) // same q
    }

    // ---- 4th (vector) queue dependencies. ov = row-range overlap of valid operands ----
    def ov(a: UDValid[OpT], b: UDValid[OpT]): Bool = a.valid && b.valid && a.bits.overlaps(b.bits)
    def ovAny(as: Seq[UDValid[OpT]], bs: Seq[UDValid[OpT]]): Bool = (for (a <- as; b <- bs) yield ov(a, b)).reduce(_ || _)
    def opsOf(e: Entry): Seq[UDValid[OpT]] = Seq(e.opa, e.opb, e.opc, e.opd)
    // e writes opa (when opa_is_dst) and opd (when valid); conflict unless both sides only read
    def conflicts(n: Entry, e: Entry): Bool =
      (n.opa_is_dst && ovAny(Seq(n.opa), opsOf(e))) || (e.opa_is_dst && ovAny(opsOf(n), Seq(e.opa))) ||
        ovAny(Seq(n.opd), opsOf(e)) || ovAny(opsOf(n), Seq(e.opd))
    val is_scale_cfg = funct === CONFIG_SCALE_MEM
    // a BF16 scratchpad store (rs2[63], set by StCSpad) shares the requantizer with SPAD_REQUANT beat by beat; only
    // quantized stores (scale coalescer) are ordered against it
    def bf16_store(c: RoCCCommand): Bool = c.inst.funct === STORE_SPAD_CMD && c.rs2(63)
    // SPAD_REQUANT files resident act scales into act half 0; a managed, non-resident config reading act half 1
    // touches neither, so it needn't wait for it
    val scale_cfg_after_sr = is_scale_cfg && !(cmd.rs2(17) && cmd.rs1(60) && !cmd.rs1(63))
    // computes read the act scales of the last config ahead of them; a resident SPAD_REQUANT rewrites act half 0, so it
    // waits for older ex entries issued under a config that may read act half 0 (WAR on the scales)
    val cfg_reads_act0 = RegInit(true.B)
    when (io.alloc.fire && is_scale_cfg) { cfg_reads_act0 := scale_cfg_after_sr }
    new_entry.reads_act0 := Mux(is_scale_cfg, scale_cfg_after_sr, cfg_reads_act0)
    val sr_resident = is_sreq && cmd.rs1(29)
    when (is_vec) {
      // after older ld/ex/st that write rows it touches or read rows it writes; SPAD_REQUANT also after every older
      // store (shared requantizer + scale coalescer; their act-scale reads finish before they complete)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && !e.bits.is_config && conflicts(new_entry, e.bits) })
      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && (conflicts(new_entry, e.bits) ||
        (sr_resident && e.bits.reads_act0)) })
      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && ((is_sreq && !bf16_store(e.bits.cmd.cmd)) ||
        conflicts(new_entry, e.bits)) })
      // vector entries: SPAD_REQUANT (one unit) runs in order; VPU commands run side by side on the VPUs and next to
      // SPAD_REQUANT. Across units they are ordered by row conflicts and by shared READ banks: each bank has one VPU
      // read port, fixed-latency and always winning, so two readers of a bank would starve each other. Writes are a
      // separate port (VPU writes are queued and arbitrated per bank), so sharing only a write bank is fine.
      // opa is the destination of every vector entry; opb / opc are its sources.
      def read_banks(x: Entry): UInt = Seq(x.opb, x.opc).map(o => Mux(o.valid, o.bits.sp_banks_mask(), 0.U)).reduce(_ | _)
      new_entry.deps_vec := VecInit(entries_vec.map { e => e.valid &&
        ((is_sreq && e.bits.cmd.cmd.inst.funct === SPAD_REQUANT) || conflicts(new_entry, e.bits) ||
          (read_banks(new_entry) & read_banks(e.bits)) =/= 0.U) })
    }.otherwise {
      // ld/ex/st after older vector entries they conflict with; stores (requantizer) and CONFIG_SCALE_MEM that may read
      // act half 0 (the next matmul's act-scale reads must see the resident scales) after an older SPAD_REQUANT
      new_entry.deps_vec := VecInit(entries_vec.map { e => e.valid && !new_entry.is_config && (conflicts(new_entry, e.bits) ||
        (((is_store && !bf16_store(cmd)) || scale_cfg_after_sr) && e.bits.cmd.cmd.inst.funct === SPAD_REQUANT)) })
    }
    // CONFIG_SCALE_MEM has no spad operands (its op ranges are decoded garbage), so the ex-vs-ld overlap above is a false
    // RAW on every queued mvin; loads never touch the scale config (the ex controller waits for the scales to land)
    when (is_scale_cfg) { new_entry.deps_ld := VecInit(Seq.fill(reservation_station_entries_ld)(false.B)) }

    new_entry.allocated_at := instructions_allocated

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exqu

    Seq(
      (ldqu, entries_ld, new_allocs_oh_ld, reservation_station_entries_ld),
      (exqu, entries_ex, new_allocs_oh_ex, reservation_station_entries_ex),
      (stqu, entries_st, new_allocs_oh_st, reservation_station_entries_st),
      (vecqu, entries_vec, new_allocs_oh_vec, n_vec))
      .foreach { case (q, entries_type, new_allocs_type, entries_count) =>
        when (new_entry.q === q) {
          val is_full = PopCount(Seq(dst.valid, op1.valid, op2.valid)) > 1.U
          val is_full2 = PopCount(Seq(dst.valid, op1.valid, op2.valid)) === 2.U
          when (q === ldqu) { assert(!is_full) }
          when (q === stqu && new_entry.cmd.cmd.inst.funct =/= STORE_SPAD_CMD) { assert(!is_full) }
          when (q === stqu && new_entry.cmd.cmd.inst.funct === STORE_SPAD_CMD) { assert(is_full2) }

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
      when (new_entry.is_config && new_entry.q === exqu) {
        a_stride := new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        c_stride := new_entry.cmd.cmd.rs2(63, 48) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        val set_only_strides = new_entry.cmd.cmd.rs1(7) // TODO magic numbers
        when (!set_only_strides) {
          a_transpose := new_entry.cmd.cmd.rs1(8) // TODO magic numbers
        }
      }.elsewhen(new_entry.is_config && new_entry.q === ldqu) {
        val id = new_entry.cmd.cmd.rs1(4,3) // TODO magic numbers
        val block_stride = new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers
        val repeat_pixels = maxOf(new_entry.cmd.cmd.rs1(8 + pixel_repeats_bits - 1, 8), 1.U) // TODO we use a default value of pixel repeats here, for backwards compatibility. However, we should deprecate and remove this default value eventually
        ld_block_strides(id) := block_stride
        ld_pixel_repeats(id) := repeat_pixels - 1.U
      }.elsewhen(new_entry.is_config && new_entry.q === stqu && !is_norm) {
        val pool_stride = new_entry.cmd.cmd.rs1(5, 4) // TODO magic numbers
        pooling_is_enabled := pool_stride =/= 0.U
      }.elsewhen(funct === PRELOAD_CMD) {
        solitary_preload := true.B
      }.elsewhen(funct_is_compute) {
        solitary_preload := false.B
      }
    }
  }

  def entry_sp_banks(e: UDValid[Entry]): UInt =
    Seq(e.bits.opa, e.bits.opb, e.bits.opc, e.bits.opd).map(o => Mux(o.valid, o.bits.sp_banks_mask(), 0.U)).reduce(_ | _)
  val vec_unit_ready = io.vec_unit_ready
  val vec_pending_banks = io.vec_pending_banks

  // (after the bank gate too: holding the accumulator side while a store still owes this SR's banks would deadlock)
  io.vec_sr_fp4_waiting := entries_vec.map(e => e.valid && e.bits.ready() && !e.bits.issued &&
    e.bits.cmd.cmd.inst.funct === SPAD_REQUANT && e.bits.cmd.cmd.rs2(32) &&
    (entry_sp_banks(e) & vec_pending_banks) === 0.U).reduce(_ || _)
  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld, entries_ld), (exq, io.issue.ex, entries_ex), (stq, io.issue.st, entries_st),
    (vecq, io.issue.vec, entries_vec))
    .foreach { case (q, io, entries_type) =>

    // a vector entry also needs its unit free and no queued requant rows in its spad banks
    def unit_ok(e: UDValid[Entry]): Bool = if (q != vecq) true.B else
      Mux(e.bits.cmd.cmd.inst.funct === SPAD_REQUANT, Mux(e.bits.cmd.cmd.rs2(32), vec_unit_ready(2), vec_unit_ready(1)),
        vec_unit_ready(0)) &&
        (entry_sp_banks(e) & vec_pending_banks) === 0.U
    val issue_valids = entries_type.map(e => e.valid && e.bits.ready() && !e.bits.issued && unit_ok(e))
    val issue_sel = PriorityEncoderOH(issue_valids)
    val issue_id = OHToUInt(issue_sel)
    val global_issue_id = Cat(q.U(2.W), issue_id.pad(log2Up(res_max_per_type)))
    assert(global_issue_id.getWidth == 2 + log2Up(res_max_per_type))

    val issue_entry = Mux1H(issue_sel, entries_type)

    io.valid := issue_valids.reduce(_||_)
    io.cmd := issue_entry.bits.cmd
    // use the most significant 2 bits to indicate instruction type
    io.rob_id := global_issue_id

    val complete_on_issue = entries_type(issue_id).bits.complete_on_issue
    val from_conv_fsm = entries_type(issue_id).bits.cmd.from_conv_fsm
    val from_matmul_fsm = entries_type(issue_id).bits.cmd.from_matmul_fsm

    when (io.fire) {
      entries_type.zipWithIndex.foreach { case (e, i) =>
        when (issue_sel(i)) {
          e.bits.issued := true.B
          e.valid := !e.bits.complete_on_issue
        }
      }

      // Update the "deps" vectors of all instructions which depend on the one that is being issued
      Seq((ldq, entries_ld), (exq, entries_ex), (stq, entries_st), (vecq, entries_vec))
        .foreach { case (q_, entries_type_) =>

        entries_type_.zipWithIndex.foreach { case (e, i) =>
          val deps_type = if (q == ldq) e.bits.deps_ld else if (q == exq) e.bits.deps_ex
            else if (q == stq) e.bits.deps_st else e.bits.deps_vec
          if ((q == q_) && (q_ != stq) && (q_ != vecq)) {
            deps_type(issue_id) := false.B // TODO(richard): normal mvouts should not be blocked until complete
          } else {
            when (issue_entry.bits.complete_on_issue) {
              deps_type(issue_id) := false.B
            }
          }
        }
      }

      // If the instruction completed on issue, then notify the conv/matmul FSMs that another one of their commands
      // completed
      if (q == ldq) { conv_ld_issue_completed := complete_on_issue && from_conv_fsm }
      if (q == stq) { conv_st_issue_completed := complete_on_issue && from_conv_fsm }
      if (q == exq) { conv_ex_issue_completed := complete_on_issue && from_conv_fsm }

      if (q == ldq) { matmul_ld_issue_completed := complete_on_issue && from_matmul_fsm }
      if (q == stq) { matmul_st_issue_completed := complete_on_issue && from_matmul_fsm }
      if (q == exq) { matmul_ex_issue_completed := complete_on_issue && from_matmul_fsm }
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire) {
    val type_width = log2Up(res_max_per_type)
    val queue_type = io.completed.bits(type_width + 1, type_width)
    val issue_id = io.completed.bits(type_width - 1, 0)

    when (queue_type === ldqu) {
      entries.foreach(_.bits.deps_ld(issue_id) := false.B)
      entries_ld(issue_id).valid := false.B

      conv_ld_completed := entries_ld(issue_id).bits.cmd.from_conv_fsm
      matmul_ld_completed := entries_ld(issue_id).bits.cmd.from_matmul_fsm

      assert(entries_ld(issue_id).valid)
    }.elsewhen (queue_type === exqu) {
      entries.foreach(_.bits.deps_ex(issue_id) := false.B)
      entries_ex(issue_id).valid := false.B

      conv_ex_completed := entries_ex(issue_id).bits.cmd.from_conv_fsm
      matmul_ex_completed := entries_ex(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_ex(issue_id).valid)
    }.elsewhen (queue_type === stqu) {
      entries.foreach(_.bits.deps_st(issue_id) := false.B)
      entries_st(issue_id).valid := false.B

      conv_st_completed := entries_st(issue_id).bits.cmd.from_conv_fsm
      matmul_st_completed := entries_st(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_st(issue_id).valid)
    }.otherwise {
      assert(has_vec.B, "vector-queue completion without a vector queue")
      entries.foreach(_.bits.deps_vec(issue_id) := false.B)
      entries_vec(issue_id).valid := false.B
      assert(entries_vec(issue_id).valid)
    }
  }

  // Explicitly mark "opb" in all ld/st queues entries as being invalid.
  // This helps us to reduce the total reservation table area
  // (ex_write_to_spad: a STORE_SPAD keeps its accumulator source range in opb, so a younger matmul's acc writes wait
  // for its read -- without it the WAR on the accumulator is lost)
  (if (ex_write_to_spad) Seq(entries_ld) else Seq(entries_ld, entries_st)).foreach { entries_type =>
    entries_type.foreach { e =>
      e.bits.opb.valid := false.B
      e.bits.opb.bits := DontCare
    }
  }
  // opc (src2) exists only in vector entries
  Seq(entries_ld, entries_ex, entries_st).foreach(_.foreach { e =>
    e.bits.opc.valid := false.B
    e.bits.opc.bits := DontCare
    e.bits.opd.valid := false.B
    e.bits.opd.bits := DontCare
  })
  if (!has_vec) entries_vec.foreach(_.valid := false.B)

  // val utilization = PopCount(entries.map(e => e.valid))
  val utilization_ld_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === ldqu))
  val utilization_st_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === stqu))
  val utilization_ex_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === exqu))
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

  when (io.issue.ld.fire || io.issue.st.fire || io.issue.ex.fire || io.issue.vec.fire || !io.busy || io.completed.fire) {
    cycles_since_issue := 0.U
  }.elsewhen(io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < PlusArg("gemmini_timeout", 1000000), "pipeline stall")

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

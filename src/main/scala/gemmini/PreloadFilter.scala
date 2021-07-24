package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.RoCCCommand
import GemminiISA._
import Util._

class PreloadFilter[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], cmd_t: RoCCCommand) extends Module {
  import config._

  val io = IO(new Bundle {
    val in_ld = Flipped(new ROBIssue(cmd_t, rob_entries))
    val in_ex = Flipped(new ROBIssue(cmd_t, rob_entries))

    val out_ld = new ROBIssue(cmd_t, rob_entries)
    val out_ex = new ROBIssue(cmd_t, rob_entries)
  })

  val block_cols = meshColumns * tileColumns
  val block_rows = meshRows * tileRows
  val block_size = block_rows max block_cols

  class AddressRangeT extends Bundle {
    // TODO maybe this should be merged with OpT in ROB.scala?
    val start = local_addr_t.cloneType
    val end = local_addr_t.cloneType
    val wraps_around = Bool()

    def overlaps(other: AddressRangeT): Bool = {
      ((other.start <= start && (start < other.end || other.wraps_around)) ||
        (start <= other.start && (other.start < end || wraps_around))) &&
        !(start.is_garbage() || other.start.is_garbage()) // TODO the "is_garbage" check might not really be necessary
    }

    def ===(other: AddressRangeT): Bool = {
      start === other.start && end === other.end && wraps_around === other.wraps_around
    }

    def make_this_garbage(dummy: Int=0): Unit = {
      start.make_this_garbage()
    }

    def is_garbage(dummy: Int=0): Bool = start.is_garbage()
  }

  val df = if (dataflow == Dataflow.BOTH) Reg(UInt(1.W)) else dataflow.id.U // TODO magic numbers
  val b_transposed = Reg(Bool())
  val preloaded_address = Reg(new AddressRangeT)
  val ld_block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W)))
  val last_preload_was_filtered = RegInit(false.B)

  val ex_set_only_strides = io.in_ex.cmd.rs1(7) // TODO magic numbers
  val ex_is_config = io.in_ex.cmd.inst.funct === CONFIG_CMD && io.in_ex.cmd.rs1(1,0).asUInt() === CONFIG_EX && !ex_set_only_strides // TODO magic numbers
  val ex_config_dataflow = io.in_ex.cmd.rs1(2) // TODO magic numbers
  val ex_config_b_transposed = io.in_ex.cmd.rs1(9) // TODO magic numbers
  val ex_is_preload = io.in_ex.cmd.inst.funct === PRELOAD_CMD
  val ex_is_compute = io.in_ex.cmd.inst.funct === COMPUTE_AND_STAY_CMD || io.in_ex.cmd.inst.funct === COMPUTE_AND_FLIP_CMD
  val ex_preload_rows = {
    val default_rows = io.in_ex.cmd.rs1(48 + log2Up(block_size) - 1, 48).asUInt() // TODO magic numbers
    val default_cols = io.in_ex.cmd.rs1(32 + log2Up(block_size) - 1, 32).asUInt() // TODO magic numbers
    Mux(b_transposed, default_cols, default_rows)
  }
  val ex_preload_addr = {
    val start = io.in_ex.cmd.rs1(31, 0).asTypeOf(local_addr_t) // TODO magic numbers
    val (end, wraps_around) = start.add_with_overflow(ex_preload_rows)

    val addr = Wire(new AddressRangeT)
    addr.start := start
    addr.end := end
    addr.wraps_around := wraps_around

    if (!ex_read_from_acc) {
      start.is_acc_addr := false.B
      end.is_acc_addr := false.B
    }

    addr
  }
  val should_filter_preload = ex_is_preload && df === Dataflow.WS.id.U && preloaded_address === ex_preload_addr

  val ld_is_config = io.in_ld.cmd.inst.funct === CONFIG_CMD
  val ld_id = Mux(ld_is_config, io.in_ld.cmd.rs1(4,3).asUInt(), // TODO magic numbers
    MuxCase(0.U, Seq((io.in_ld.cmd.inst.funct === LOAD2_CMD) -> 1.U,
      (io.in_ld.cmd.inst.funct === LOAD3_CMD) -> 2.U)))
  val ld_config_block_stride = io.in_ld.cmd.rs1(31, 16).asUInt() // TODO magic numbers
  val ld_total_rows = {
    val block_stride = ld_block_strides(ld_id)
    val ld_cols = io.in_ld.cmd.rs2(32 + mvin_cols_bits - 1, 32).asUInt() // TODO magic numbers
    val ld_rows = io.in_ld.cmd.rs2(48 + mvin_rows_bits - 1, 48).asUInt() // TODO magic numbers
    val ld_mats = ld_cols / block_cols.U + (ld_cols % block_cols.U =/= 0.U)
    ((ld_mats - 1.U) * block_stride) + ld_rows
  }
  val ld_addr = {
    val start = io.in_ld.cmd.rs2(31, 0).asTypeOf(local_addr_t) // TODO magic numbers
    val (end, wraps_around) = start.add_with_overflow(ld_total_rows)

    val addr = Wire(new AddressRangeT)
    addr.start := start
    addr.end := end
    addr.wraps_around := wraps_around

    addr
  }

  // Set all state registers
  when (io.in_ld.fire()) {
    when (ld_is_config) {
      ld_block_strides(ld_id) := ld_config_block_stride
    }.elsewhen(preloaded_address.overlaps(ld_addr)) {
      preloaded_address.make_this_garbage()
    }
  }

  when (io.in_ex.fire()) {
    when (ex_is_config) {
      if (dataflow == Dataflow.BOTH) {
        df := ex_config_dataflow
      }
      b_transposed := ex_config_b_transposed

      when (b_transposed =/= ex_config_b_transposed) {
        preloaded_address.make_this_garbage()
      }
    }.elsewhen(ex_is_preload && !ex_preload_addr.is_garbage()) {
      preloaded_address := ex_preload_addr
    }

    when (should_filter_preload) {
      last_preload_was_filtered := true.B
    }.elsewhen(ex_is_compute) {
      last_preload_was_filtered := false.B
    }
  }

  // Set outputs
  io.out_ld <> io.in_ld
  io.out_ex <> io.in_ex

  when (should_filter_preload) {
    io.out_ex.cmd.rs1 := (block_rows.U << 48) | (block_cols.U << 32) | GARBAGE_ADDR // TODO magic numbers
  }.elsewhen(ex_is_compute && last_preload_was_filtered) {
    io.out_ex.cmd.inst.funct := COMPUTE_AND_STAY_CMD
  }

  when (reset.toBool()) {
    preloaded_address.make_this_garbage()
  }

  assert(!(io.in_ex.valid && io.in_ld.valid && !ld_is_config && ex_is_preload && ex_preload_addr.overlaps(ld_addr)))
}

object PreloadFilter{
  def apply[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], cmd_t: RoCCCommand, ld_issue: ROBIssue[RoCCCommand], ex_issue: ROBIssue[RoCCCommand]) = {
    if (config.use_preload_filter) {
      val preload_filter = Module(new PreloadFilter(config, cmd_t))
      preload_filter.io.in_ld <> ld_issue
      preload_filter.io.in_ex <> ex_issue
      (preload_filter.io.out_ld, preload_filter.io.out_ex)
    } else {
      (ld_issue, ex_issue)
    }
  }
}


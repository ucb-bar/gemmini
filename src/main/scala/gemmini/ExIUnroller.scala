package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import chipsalliance.rocketchip.config.Parameters
import GemminiISA._
import Util._

class ExIUnroller[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])(implicit p: Parameters)  extends Module {
  import config._

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(rob_entries)))
    val out = Decoupled(new GemminiCmd(rob_entries))
  })

  object State extends ChiselEnum {
    val preload, compute = Value
  }
  import State._
  val state = RegInit(preload)

  val (q, len) = MultiHeadedQueue(io.in, entries=3, heads=2, maxpop=2)

  val first_cmd_is_preload = q.bits(0).cmd.inst.funct === PRELOAD_CMD

  val total_I = q.bits(0).cmd.rs2(63, 48).asUInt() // This is only valid if first_cmd_is_preload === true.B // TODO magic numbers
  val I_sent = RegInit(0.U(16.W)) // TODO magic number
  val last_send = total_I -& I_sent <= block_rows.U

  val must_unroll = first_cmd_is_preload && total_I > block_rows.U

  val J_blocks = Cat(q.bits(0).cmd.inst.opcode, q.bits(0).cmd.inst.rs1, q.bits(0).cmd.inst.rs2, q.bits(0).cmd.inst.rd)
  val K_blocks = Cat(q.bits(1).cmd.inst.opcode, q.bits(1).cmd.inst.rs1, q.bits(1).cmd.inst.rs2, q.bits(1).cmd.inst.rd)
  val I_block = I_sent / block_rows.U

  val preload_cmd_with_bounded_i = WireInit(q.bits(0))
  preload_cmd_with_bounded_i.cmd.rs2 := (minOf(total_I -& I_sent, block_rows.U) << 48) |
    (q.bits(0).cmd.rs2(47, 32) << 32) |
    (q.bits(0).cmd.rs2(31, 0).asTypeOf(local_addr_t) + I_block * J_blocks * block_rows.U).asUInt()
  preload_cmd_with_bounded_i.rob_id.valid := last_send && q.bits(0).rob_id.valid

  val compute_cmd_with_bounded_i = WireInit(q.bits(1))
  compute_cmd_with_bounded_i.cmd.rs1 := (minOf(total_I -& I_sent, block_rows.U) << 48) |
    (q.bits(1).cmd.rs1(47, 32) << 32) |
    (q.bits(1).cmd.rs1(31, 0).asTypeOf(local_addr_t) + I_block * K_blocks * block_rows.U).asUInt()
  compute_cmd_with_bounded_i.cmd.rs2 := (minOf(total_I -& I_sent, block_rows.U) << 48) |
    (q.bits(1).cmd.rs2(47, 32) << 32) |
    (q.bits(1).cmd.rs2(31, 0).asTypeOf(local_addr_t) + I_block * J_blocks * block_rows.U).asUInt()
  compute_cmd_with_bounded_i.rob_id.valid := last_send && q.bits(1).rob_id.valid

  when (I_sent > 0.U) {
    preload_cmd_with_bounded_i.cmd.rs1 := (block_rows.U << 48) | (block_cols.U << 32) | GARBAGE_ADDR
    compute_cmd_with_bounded_i.cmd.inst.funct := COMPUTE_AND_STAY_CMD
  }
  when (q.bits(0).cmd.rs2(31, 0).asTypeOf(local_addr_t).is_garbage()) {
    preload_cmd_with_bounded_i.cmd.rs2 := (block_rows.U << 48) | (block_cols.U << 32) | GARBAGE_ADDR
  }
  when (q.bits(1).cmd.rs1(31, 0).asTypeOf(local_addr_t).is_garbage()) {
    compute_cmd_with_bounded_i.cmd.rs1 := (block_rows.U << 48) | (block_cols.U << 32) | GARBAGE_ADDR
  }
  when (q.bits(1).cmd.rs2(31, 0).asTypeOf(local_addr_t).is_garbage() || (dataflow == Dataflow.WS && hardcode_d_to_garbage_addr).B) {
    compute_cmd_with_bounded_i.cmd.rs2 := (block_rows.U << 48) | (block_cols.U << 32) | GARBAGE_ADDR
  }

  io.out.valid := Mux(must_unroll, (q.valid(0) && state === preload) || (q.valid(1) && state === compute), q.valid(0))
  io.out.bits := Mux(must_unroll, Mux(state === preload, preload_cmd_with_bounded_i, compute_cmd_with_bounded_i), q.bits(0))

  q.pop := Mux(io.out.fire(), Mux(must_unroll, Mux(state === compute && last_send, 2.U, 0.U), 1.U), 0.U)

  // Control the state
  when (io.out.fire() && must_unroll) {
    state := state.next
  }

  // Control I_sent
  when (io.out.fire() && must_unroll && state === compute) {
    I_sent := floorAdd(I_sent, block_rows.U, total_I)
  }
}

object ExIUnroller {
  def apply[T <: Data : Arithmetic, U <: Data, V <: Data](in: ReadyValidIO[GemminiCmd], config: GemminiArrayConfig[T, U, V])(implicit p: Parameters) = {
    val mod = Module(new ExIUnroller(config))
    mod.io.in <> in
    mod.io.out
  }
}

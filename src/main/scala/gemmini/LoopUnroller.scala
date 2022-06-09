package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._
import gemmini.LocalAddr.{cast_to_acc_addr, cast_to_sp_addr}

class LoopUnroller(block_size: Int, coreMaxAddrBits: Int,
                   max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
                   mvin_rs2_t: MvinRs2, mvout_rs2_t: MvoutRs2
                  )(implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val GARBAGE_ADDR = ~0.U(32.W)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
  })

  object State extends ChiselEnum {
    val idle, load, store = Value
  }
  import State._

  val state = RegInit(idle)

  val cmd = Queue(io.in)

  val row = RegInit(0.U(15.W))
  val col = RegInit(0.U(15.W))

  val is_loop = cmd.bits.inst.funct === LOOP_ONE
  val operation = cmd.bits.rs1(63, 60) // 0: mvout, 1: mvin1, 2: mvin2
  val dram_addr_base = cmd.bits.rs1(47, 0)
  val dram_stride = cmd.bits.rs2(31, 0)
  val spad_choice = cmd.bits.rs2(33, 32)
  val cols = cmd.bits.rs2(48, 34)
  val rows = cmd.bits.rs2(63, 49)

  val max_block_len = (dma_max_bytes / (input_w / 8)) max block_size // DIM * block_len


  val a_start = 0.U
  val b_start = (max_addr / 2).U
  val c_start = (3.U << 30).asUInt()
  val d_start = (1.U << 31).asUInt()

  val sp_addr_start = MuxCase(a_start, Seq(
    (spad_choice === 1.U) -> b_start,
    (spad_choice === 2.U) -> c_start,
    (spad_choice === 3.U) -> d_start
  ))


  val max_blocks = Mux(cols <= max_block_len.U, cols, max_block_len.U)
  val last_iteration = row + block_size.U >= rows && col + max_blocks >= cols

  val dram_offset = (row * dram_stride + col) * (input_w/8).U
  val dram_addr = dram_addr_base + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = sp_addr_start + (row * cols + col)// * block_size.U
  val blocks = Mux(col + max_blocks <= cols, max_blocks, cols-col)
  val num_c = blocks - Mux(col + blocks >= cols, cols - col, 0.U)
  val num_r = block_size.U - Mux(row + block_size.U > rows, rows - row, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := Mux(operation === 1.U, LOAD_CMD, LOAD2_CMD)
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := num_r
  mvin_cmd_rs2.num_cols := num_c
  mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt()

  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := dram_addr

  val mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  mvout_cmd_rs2 := DontCare
  mvout_cmd_rs2.num_rows := num_r
  mvout_cmd_rs2.num_cols := num_c
  // full_c paramaterize?
  mvout_cmd_rs2.local_addr := cast_to_acc_addr(mvout_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = false.B)
  mvout_cmd.rs2 := mvout_cmd_rs2.asUInt()

  cmd.ready := false.B
  io.out.valid := cmd.valid
  io.out.bits := Mux(is_loop, Mux(state === load, mvin_cmd, mvout_cmd), cmd.bits)

  def increment(): Unit = {
    val next_r = floorAdd(row, block_size.U, rows)
    val next_c = Mux(row + block_size.U >= rows, floorAdd(col, max_blocks, cols), col)

    row := next_r
    col := next_c
  }

  when (cmd.valid) {
    when (is_loop && state === idle) {
      when (io.out.fire) {
        state := Mux(operation === 0.U, store, load)
      }
    }.elsewhen(is_loop && (state === store || state === load)) {
      when (io.out.fire) {
        increment()
        state := Mux(last_iteration, idle, state)
        cmd.ready := last_iteration
      }
    }.otherwise {
      cmd.ready := io.out.ready
    }
  }
}

object LoopUnroller {
  def apply(in: DecoupledIO[RoCCCommand], block_size: Int, coreMaxAddrBits: Int,
            max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
            mvin_rs2_t: MvinRs2, mvout_rs2_t: MvoutRs2)
           (implicit p: Parameters): DecoupledIO[RoCCCommand] = {
    val lu = Module(new LoopUnroller(block_size, coreMaxAddrBits,
      max_addr, max_acc_addr, input_w, acc_w, dma_max_bytes,
      mvin_rs2_t, mvout_rs2_t))
    lu.io.in <> in
    lu.io.out
  }
}

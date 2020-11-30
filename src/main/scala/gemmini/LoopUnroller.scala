package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters

import GemminiISA._
import Util._

class LoopUnroller(block_size: Int)(implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val GARBAGE_ADDR = ~0.U(32.W)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
  })

  object State extends ChiselEnum {
    val idle, preload, compute = Value
  }
  import State._

  val state = RegInit(idle)

  val cmd = Queue(io.in)

  val i = RegInit(0.U(16.W))
  val j = RegInit(0.U(16.W))
  val k = RegInit(0.U)

  val max_i = cmd.bits.rs2(iterator_bitwidth-1, 0)
  val max_j = cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
  val max_k = cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)

  val last_iteration = i === max_i - 1.U && j === max_j - 1.U && k === max_k - 1.U

  //pad I, J, K
  val pad_i = cmd.bits.rs1(9, 0)
  val pad_j = cmd.bits.rs1(19, 10)
  val pad_k = cmd.bits.rs1(29, 20)

  val last_i = i === max_i - 1.U
  val last_j = j === max_j - 1.U
  val last_k = k === max_k - 1.U

  val bias = cmd.bits.rs2(iterator_bitwidth * 3)
  val b_transpose = cmd.bits.rs2(iterator_bitwidth * 3 + 1)

  val a_start = 0.U(32.W)//cmd.bits.rs1(31, 0)
  val b_start = cmd.bits.rs1(63, 32)
  val c_start = (3.U << 30).asUInt()
  val d_start = (1.U << 31).asUInt()

  // TODO get rid of the x * max_y multiplications here
  val a_addr = a_start + (i * max_k + k) * block_size.U
  val b_addr = Mux(b_transpose, b_start + (j * max_k + k) * block_size.U, b_start + (k * max_j + j) * block_size.U)
  val c_addr = c_start + (i * max_j + j) * block_size.U
  val d_addr = d_start + (i * max_j + j) * block_size.U

  val is_loop = cmd.bits.inst.funct === LOOP_WS

  val pre_addr = Mux(i === 0.U, b_addr, GARBAGE_ADDR)
  val out_addr = Mux(bias || k =/= 0.U, c_addr, d_addr)

  val A_cols = Mux(last_k, block_size.U - pad_k, block_size.U)
  val A_rows = Mux(last_i, block_size.U - pad_i, block_size.U)
  val C_cols = Mux(last_j, block_size.U - pad_j, block_size.U)
  val B_cols = Mux(b_transpose, A_cols, C_cols)
  val B_rows = Mux(b_transpose, C_cols, A_cols)
  val C_rows = A_rows

  val preload_cmd = Wire(new RoCCCommand)
  preload_cmd := DontCare
  preload_cmd.inst.funct := PRELOAD_CMD
  preload_cmd.rs1 := ((B_rows << 48.U).asUInt() | (B_cols << 32.U).asUInt() | pre_addr.asUInt())
  preload_cmd.rs2 := ((C_rows << 48.U).asUInt() | (C_cols << 32.U).asUInt() | out_addr)

  val compute_cmd = Wire(new RoCCCommand())
  compute_cmd := DontCare
  compute_cmd.inst.funct := Mux(i === 0.U, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)
  compute_cmd.rs1 := ((A_rows << 48.U).asUInt() | (A_cols << 32.U).asUInt() | a_addr)
  compute_cmd.rs2 := GARBAGE_ADDR

  cmd.ready := false.B
  io.out.valid := cmd.valid
  io.out.bits := Mux(is_loop, Mux(state === compute, compute_cmd, preload_cmd), cmd.bits)

  def increment(): Unit = {
    val next_i = wrappingAdd(i, 1.U, max_i)
    val next_k = Mux(i === max_i - 1.U, wrappingAdd(k, 1.U, max_k), k)
    val next_j = Mux(k === max_k - 1.U && i === max_i - 1.U, wrappingAdd(j, 1.U, max_j), j)

    i := next_i
    k := next_k
    j := next_j
  }

  when (cmd.valid) {
    when (is_loop && (state === idle || state === preload)) {
      when (io.out.fire()) {
        state := compute
      }
    }.elsewhen(is_loop && state === compute) {
      when (io.out.fire()) {
        increment()
        state := Mux(last_iteration, idle, preload)
        cmd.ready := last_iteration
      }
    }.otherwise {
      cmd.ready := io.out.ready
    }
  }
}

object LoopUnroller {
  def apply(enq: ReadyValidIO[RoCCCommand], block_size: Int)(implicit p: Parameters): DecoupledIO[RoCCCommand] = {
    val lu = Module(new LoopUnroller(block_size))
    lu.io.in <> enq
    lu.io.out
  }
}
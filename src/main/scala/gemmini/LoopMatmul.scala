package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import LocalAddr._
import Util._

// LdA

class LoopMatmulLdAReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_i = UInt(iterator_bitwidth.W)
  val max_k = UInt(iterator_bitwidth.W)
  val pad_i = UInt(log2Up(block_size).W)
  val pad_k = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val transpose = Bool()
  val addr_start = UInt(log2Up(max_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopMatmulLdA(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                    max_block_len: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))
    val i = Output(UInt(iterator_bitwidth.W))
    val k = Output(UInt(iterator_bitwidth.W))
    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops))

  val i = Reg(UInt(iterator_bitwidth.W))
  val k = Reg(UInt(iterator_bitwidth.W))

  val row_iterator = Mux(req.transpose, k, i)
  val col_iterator = Mux(req.transpose, i, k)

  val max_row_iterator = Mux(req.transpose, req.max_k, req.max_i)
  val max_col_iterator = Mux(req.transpose, req.max_i, req.max_k)

  val row_pad = Mux(req.transpose, req.pad_k, req.pad_i)
  val col_pad = Mux(req.transpose, req.pad_i, req.pad_k)

  val max_col_dim = Mux(req.transpose, req.max_i, req.max_k)
  val max_blocks = Mux(max_col_dim <= max_block_len.U, max_col_dim, max_block_len.U)

  val sp_addr_start = req.addr_start

  val dram_offset = (row_iterator * req.dram_stride + col_iterator) * block_size.U * (input_w/8).U
  val dram_addr = req.dram_addr + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = sp_addr_start + (row_iterator * max_col_iterator + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(row_iterator === max_row_iterator-1.U, row_pad, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt()
  mvin_cmd_rs2.num_cols := cols.asUInt()
  mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt()

  io.req.ready := state === idle
  io.i := i
  io.k := k
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when (io.cmd.fire) {
    // The order here is k, j, i
    val i_blocks = Mux(req.transpose, max_blocks, 1.U)
    val k_blocks = Mux(req.transpose, 1.U, max_blocks)

    val next_i = floorAdd(i, i_blocks, req.max_i)
    val next_k = floorAdd(k, k_blocks, req.max_k, next_i === 0.U)

    i := next_i
    k := next_k

    when (next_i === 0.U && next_k === 0.U) {
      state := idle
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    i := 0.U
    k := 0.U
  }
}

// LdB

class LoopMatmulLdBReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val pad_k = UInt(log2Up(block_size).W)
  val pad_j = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val transpose = Bool()
  val addr_end = UInt(log2Up(max_addr+1).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopMatmulLdB(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                    max_block_len: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val k = Output(UInt(iterator_bitwidth.W))
    val j = Output(UInt(iterator_bitwidth.W))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops))

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))

  val row_iterator = Mux(req.transpose, j, k)
  val col_iterator = Mux(req.transpose, k, j)

  val max_row_iterator = Mux(req.transpose, req.max_j, req.max_k)
  val max_col_iterator = Mux(req.transpose, req.max_k, req.max_j)

  val row_pad = Mux(req.transpose, req.pad_j, req.pad_k)
  val col_pad = Mux(req.transpose, req.pad_k, req.pad_j)

  val max_col_dim = Mux(req.transpose, req.max_k, req.max_j)
  val max_blocks = Mux(max_col_dim <= max_block_len.U, max_col_dim, max_block_len.U)

  val sp_addr_start = req.addr_end - req.max_k * req.max_j * block_size.U

  val dram_offset = (row_iterator * req.dram_stride + col_iterator) * block_size.U * (input_w/8).U
  val dram_addr = req.dram_addr + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = sp_addr_start + (row_iterator * max_col_iterator + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(max_row_iterator === max_row_iterator-1.U, row_pad, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD2_CMD
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt()
  mvin_cmd_rs2.num_cols := cols.asUInt()
  mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt()

  io.req.ready := state === idle
  io.k := k
  io.j := j
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when (io.cmd.fire) {
    // The order here is k, j, i
    val j_blocks = Mux(req.transpose, 1.U, max_blocks)
    val k_blocks = Mux(req.transpose, max_blocks, 1.U)

    val next_j = floorAdd(j, j_blocks, req.max_j)
    val next_k = floorAdd(k, k_blocks, req.max_k, next_j === 0.U)

    j := next_j
    k := next_k

    when (next_j === 0.U && next_k === 0.U) {
      state := idle
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    j := 0.U
    k := 0.U
  }
}

// LdD

class LoopMatmulLdDReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val low_d = Bool()
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopMatmulLdD(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int,
                    acc_w: Int, max_block_len: Int, max_block_len_acc: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulLdDReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulLdDReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops))

  val max_blocks = Mux(req.low_d, Mux(req.max_j <= max_block_len.U, req.max_j, max_block_len.U),
    Mux(req.max_j <= max_block_len_acc.U, req.max_j, max_block_len_acc.U))

  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))

  val acc_addr_start = req.addr_start

  val dram_offset = Mux(req.low_d, (i * req.dram_stride + j) * block_size.U * (input_w/8).U,
    (i * req.dram_stride + j) * block_size.U * (acc_w/8).U)
  val dram_addr = req.dram_addr + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = acc_addr_start + (i * req.max_j + j) * block_size.U
  val blocks = Mux(j + max_blocks <= req.max_j, max_blocks, req.max_j-j)
  val cols = (blocks * block_size.U) - Mux(j + blocks >= req.max_j, req.pad_j, 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD3_CMD
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt()
  mvin_cmd_rs2.num_cols := cols.asUInt()
  mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = false.B)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt()

  io.req.ready := state === idle
  io.idle := state === idle

  // The order here is k, j, i
  io.cmd.valid := state =/= idle && !io.rob_overloaded && req.dram_addr =/= 0.U
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when (req.dram_addr === 0.U) {
    state := idle
  }.elsewhen (io.cmd.fire) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_j = floorAdd(j, max_blocks, req.max_j, next_i === 0.U)

    i := next_i
    j := next_j

    when (next_i === 0.U && next_j === 0.U) {
      state := idle
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    j := 0.U
    i := 0.U
  }
}

// Compute
class LoopMatmulExecuteReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_addr: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_j = UInt(iterator_bitwidth.W)
  val max_k = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_k = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val a_tranpose = Bool()
  val b_tranpose = Bool()
  val accumulate = Bool()
  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr+1).W)
  val c_addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopMatmulExecute(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int, max_acc_addr: Int, concurrent_loops: Int,
                        preload_rs1_t: PreloadRs, preload_rs2_t: PreloadRs,
                        compute_rs1_t: ComputeRs, compute_rs2_t: ComputeRs)
                       (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulExecuteReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val k = Output(UInt(iterator_bitwidth.W))
    val j = Output(UInt(iterator_bitwidth.W))
    val i = Output(UInt(iterator_bitwidth.W))

    val ld_ka = Input(UInt(iterator_bitwidth.W))
    val ld_kb = Input(UInt(iterator_bitwidth.W))
    val ld_j = Input(UInt(iterator_bitwidth.W))
    val ld_i = Input(UInt(iterator_bitwidth.W))
    val lda_completed = Input(Bool())
    val ldb_completed = Input(Bool())
    val ldd_completed = Input(Bool())

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, pre, comp = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulExecuteReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops))

  val c_addr_start = /*(BigInt(1) << 31).U |*/ req.c_addr_start
  val b_addr_start = req.b_addr_end - req.max_k * req.max_j * block_size.U

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))

  val a_row = Mux(req.a_tranpose, k, i)
  val a_col = Mux(req.a_tranpose, i, k)
  val b_row = Mux(req.b_tranpose, j, k)
  val b_col = Mux(req.b_tranpose, k, j)

  val a_max_col = Mux(req.a_tranpose, req.max_i, req.max_k)
  val b_max_col = Mux(req.b_tranpose, req.max_k, req.max_j)

  val a_addr = req.a_addr_start + (a_row * a_max_col + a_col) * block_size.U
  val b_addr = b_addr_start + (b_row * b_max_col + b_col) * block_size.U
  val c_addr = c_addr_start + (i * req.max_j + j) * block_size.U

  val a_cols = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)
  val a_rows = block_size.U - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)
  val b_cols = block_size.U - Mux(j === req.max_j - 1.U, req.pad_j, 0.U)
  val b_rows = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)
  val c_cols = block_size.U - Mux(j === req.max_j - 1.U, req.pad_j, 0.U)
  val c_rows = block_size.U - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)

  val pre_cmd = Wire(new RoCCCommand)
  pre_cmd := DontCare
  pre_cmd.inst.funct := PRELOAD_CMD

  val pre_cmd_rs1 = Wire(preload_rs1_t.cloneType)
  pre_cmd_rs1 := DontCare
  pre_cmd_rs1.num_rows := b_rows.asUInt()
  pre_cmd_rs1.num_cols := b_cols.asUInt()
  pre_cmd_rs1.local_addr := Mux(i === 0.U, cast_to_sp_addr(pre_cmd_rs1.local_addr, b_addr),
    garbage_addr(pre_cmd_rs1.local_addr))

  val pre_cmd_rs2 = Wire(preload_rs2_t.cloneType)
  pre_cmd_rs2 := DontCare
  pre_cmd_rs2.num_rows := c_rows.asUInt()
  pre_cmd_rs2.num_cols := c_cols.asUInt()
  pre_cmd_rs2.local_addr := cast_to_acc_addr(pre_cmd_rs2.local_addr, c_addr, accumulate = req.accumulate || k =/= 0.U, read_full = false.B)

  pre_cmd.rs1 := pre_cmd_rs1.asUInt()
  pre_cmd.rs2 := pre_cmd_rs2.asUInt()

  val comp_cmd = Wire(new RoCCCommand())
  comp_cmd := DontCare
  comp_cmd.inst.funct := Mux(i === 0.U, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)

  val comp_cmd_rs1 = Wire(compute_rs1_t.cloneType)
  comp_cmd_rs1 := DontCare
  comp_cmd_rs1.num_rows := a_rows.asUInt()
  comp_cmd_rs1.num_cols := a_cols.asUInt()
  comp_cmd_rs1.local_addr := cast_to_sp_addr(comp_cmd_rs1.local_addr, a_addr)

  val comp_cmd_rs2 = Wire(compute_rs2_t.cloneType)
  comp_cmd_rs2 := DontCare
  comp_cmd_rs2.num_rows := block_size.U
  comp_cmd_rs2.num_cols := block_size.U
  comp_cmd_rs2.local_addr := garbage_addr(comp_cmd_rs2.local_addr)

  comp_cmd.rs1 := comp_cmd_rs1.asUInt()
  comp_cmd.rs2 := comp_cmd_rs2.asUInt()

  io.req.ready := state === idle
  io.k := k
  io.j := j
  io.i := i
  io.idle := state === idle

  // The order here is k, j, i
  val lda_ahead = io.lda_completed || io.ld_ka > k || (io.ld_ka === k && io.ld_i > i)
  val ldb_ahead = io.ldb_completed || io.ld_kb > k || (io.ld_ka === k && io.ld_j > j)
  val ldd_ahead = io.ldd_completed
  val ld_ahead = lda_ahead && ldb_ahead && ldd_ahead

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ld_ahead
  io.cmd.bits := Mux(state === pre, pre_cmd, comp_cmd)

  io.loop_id := req.loop_id

  when (io.cmd.fire) {
    when (state === pre) {
      state := comp
    }.otherwise {
      val next_i = floorAdd(i, 1.U, req.max_i)
      val next_j = floorAdd(j, 1.U, req.max_j, next_i === 0.U)
      val next_k = floorAdd(k, 1.U, req.max_k, next_j === 0.U && next_i === 0.U)

      k := next_k
      j := next_j
      i := next_i

      state := Mux(next_k === 0.U && next_j === 0.U && next_i === 0.U, idle, pre)
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := pre
    j := 0.U
    k := 0.U
    i := 0.U
  }

  assert(!(state =/= idle && req.a_tranpose && req.b_tranpose))
}

// StC

class LoopMatmulStCReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val full_c = Bool()
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopMatmulStC(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, max_block_len: Int, concurrent_loops: Int, mvout_rs2_t: MvoutRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulStCReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val ex_k = Input(UInt(iterator_bitwidth.W))
    val ex_j = Input(UInt(iterator_bitwidth.W))
    val ex_i = Input(UInt(iterator_bitwidth.W))
    val ex_completed = Input(Bool())

    val j = Output(UInt(iterator_bitwidth.W))
    val i = Output(UInt(iterator_bitwidth.W))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, st = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulStCReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops))

  val max_blocks = Mux(req.full_c, 1.U, Mux(req.max_j <= max_block_len.U, req.max_j, max_block_len.U))

  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))

  val acc_addr_start = /*(BigInt(1) << 31).U | (req.full_c << 29.U).asUInt() |*/ req.addr_start

  val dram_offset = Mux(req.full_c, (i * req.dram_stride + j) * block_size.U * (acc_w/8).U,
    (i * req.dram_stride + j) * block_size.U * (input_w/8).U)
  val dram_addr = req.dram_addr + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = acc_addr_start + (i * req.max_j + j) * block_size.U
  val blocks = Mux(j + max_blocks <= req.max_j, max_blocks, req.max_j-j)
  val cols = (blocks * block_size.U) - Mux(j + blocks >= req.max_j, req.pad_j, 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)

  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := dram_addr

  val mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  mvout_cmd_rs2 := DontCare
  mvout_cmd_rs2.num_rows := rows.asUInt()
  mvout_cmd_rs2.num_cols := cols.asUInt()
  mvout_cmd_rs2.local_addr := cast_to_acc_addr(mvout_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = req.full_c)
  mvout_cmd.rs2 := mvout_cmd_rs2.asUInt()

  io.req.ready := state === idle
  io.j := j
  io.i := i
  io.idle := state === idle

  // The order here is k, j, i
  // val ex_ahead = io.ex_completed || (io.ex_k === req.max_k - 1.U && (io.ex_j > j || (io.ex_j === j && io.ex_i > i)))
  val ex_ahead = io.ex_completed ||
    (io.ex_k === req.max_k - 1.U &&
      (io.ex_j >= j + blocks ||
        ((io.ex_j === j + blocks - 1.U) && io.ex_i > i)))

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ex_ahead && req.dram_addr =/= 0.U
  io.cmd.bits := mvout_cmd

  io.loop_id := req.loop_id

  when (req.dram_addr === 0.U) {
    state := idle
  }.elsewhen (io.cmd.fire) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_j = floorAdd(j, max_blocks, req.max_j, next_i === 0.U)

    i := next_i
    j := next_j

    when (next_i === 0.U && next_j === 0.U) {
      state := idle
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := st
    j := 0.U
    i := 0.U
  }
}

// Combined loop
class LoopMatmulState(val iterator_bitwidth: Int, val coreMaxAddrBits: Int, val max_addr: Int, val max_acc_addr: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)

  val pad_k = UInt(iterator_bitwidth.W)
  val pad_j = UInt(iterator_bitwidth.W)
  val pad_i = UInt(iterator_bitwidth.W)

  val a_dram_addr = UInt(coreMaxAddrBits.W)
  val b_dram_addr = UInt(coreMaxAddrBits.W)
  val d_dram_addr = UInt(coreMaxAddrBits.W)
  val c_dram_addr = UInt(coreMaxAddrBits.W)

  val a_dram_stride = UInt(coreMaxAddrBits.W)
  val b_dram_stride = UInt(coreMaxAddrBits.W)
  val d_dram_stride = UInt(coreMaxAddrBits.W)
  val c_dram_stride = UInt(coreMaxAddrBits.W)

  val a_transpose = Bool()
  val b_transpose = Bool()

  val low_d = Bool()
  val full_c = Bool()
  val ex_accumulate = Bool()

  val weightA = UInt(8.W) // TODO magic numbers

  val configured = Bool()

  val running = Bool()

  val lda_started = Bool()
  val ldb_started = Bool()
  val ex_started = Bool()
  val ldd_started = Bool()
  val st_started = Bool()

  val lda_completed = Bool()
  val ldb_completed = Bool()
  val ex_completed = Bool()
  val ldd_completed = Bool()
  val st_completed = Bool()

  def all_completed(dummy: Int=0): Bool = lda_completed && ldb_completed && ldd_completed && ex_completed && st_completed

  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr+1).W)

  def reset(): Unit = {
    configured := false.B

    running := false.B

    lda_started := false.B
    ldb_started := false.B
    ex_started := false.B
    ldd_started := false.B
    st_started := false.B

    lda_completed := false.B
    ldb_completed := false.B
    ex_completed := false.B
    ldd_completed := false.B
    st_completed := false.B
  }
}

class LoopMatmul(block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
                 max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
                 mvin_rs2_t: MvinRs2, preload_rs1_t: PreloadRs, preload_rs2_t: PreloadRs,
                 compute_rs1_t: ComputeRs, compute_rs2_t: ComputeRs, mvout_rs2_t: MvoutRs2)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val max_block_len = (dma_max_bytes / (block_size * input_w / 8)) max 1
  val max_block_len_acc = (dma_max_bytes / (block_size * acc_w / 8)) max 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val ld_utilization = Input(UInt(log2Up(rob_size+1).W))
    val st_utilization = Input(UInt(log2Up(rob_size+1).W))
    val ex_utilization = Input(UInt(log2Up(rob_size+1).W))
    val busy = Output(Bool())
  })

  // Create states
  val concurrent_loops = 2
  val loops = Reg(Vec(concurrent_loops, new LoopMatmulState(iterator_bitwidth, coreMaxAddrBits, max_addr, max_acc_addr)))
  val head_loop_id = Reg(UInt(log2Up(concurrent_loops).W))
  val tail_loop_id = (~head_loop_id).asUInt() // This is the loop that we always try to configure if available
  val head_loop = loops(head_loop_id)
  val tail_loop = loops(tail_loop_id)

  val loop_configured = loops.map(_.configured).reduce(_ || _)

  val loop_being_configured_id = Mux(head_loop.configured, tail_loop_id, head_loop_id)
  val loop_being_configured = loops(loop_being_configured_id)

  // Create inner modules
  val ldA = Module(new LoopMatmulLdA(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops, mvin_rs2_t))
  val ldB = Module(new LoopMatmulLdB(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops, mvin_rs2_t))
  val ldD = Module(new LoopMatmulLdD(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, max_block_len, max_block_len_acc, concurrent_loops, mvin_rs2_t))
  val ex = Module(new LoopMatmulExecute(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops, preload_rs1_t, preload_rs2_t, compute_rs1_t, compute_rs2_t))
  val stC = Module(new LoopMatmulStC(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, max_block_len, concurrent_loops, mvout_rs2_t))

  // Create command queue
  val cmd = Queue(io.in)

  io.busy := cmd.valid || loop_configured

  // Create ld arbiters
  val ldab_arb = Module(new WeightedArbiter(new RoCCCommand(), maxWeightA=255, staticWeightAEnabled=true)) // TODO magic numbers
  ldab_arb.io.inA <> ldA.io.cmd
  ldab_arb.io.inB <> ldB.io.cmd
  val ab_loads_on_same_loop = ldA.io.loop_id === ldB.io.loop_id
  ldab_arb.io.forceA := !ab_loads_on_same_loop && ldA.io.loop_id === head_loop_id
  ldab_arb.io.forceB := !ab_loads_on_same_loop && ldB.io.loop_id === head_loop_id
  ldab_arb.io.weightA := head_loop.weightA
  ldab_arb.io.inA_idle := ldA.io.idle
  ldab_arb.io.inB_idle := ldB.io.idle
  ldab_arb.io.inA_k := ldA.io.k
  ldab_arb.io.inA_i := ldA.io.i
  ldab_arb.io.inB_k := ldB.io.k
  ldab_arb.io.inB_j := ldB.io.j

  // Create global arbiter
  val arb = Module(new Arbiter(new RoCCCommand(), 4))
  arb.io.in(0) <> stC.io.cmd
  arb.io.in(1) <> ex.io.cmd
  arb.io.in(2) <> ldD.io.cmd
  arb.io.in(3) <> ldab_arb.io.out
  val unrolled_cmd = arb.io.out

  // Wire up unrolled command output
  val is_loop_run_cmd = cmd.bits.inst.funct === LOOP_WS
  val is_loop_config_cmd = cmd.bits.inst.funct >= LOOP_WS_CONFIG_BOUNDS && cmd.bits.inst.funct <= LOOP_WS_CONFIG_STRIDES_DC
  val is_loop_cmd = is_loop_run_cmd || is_loop_config_cmd

  io.out.bits := Mux(loop_configured, unrolled_cmd.bits, cmd.bits)
  io.out.bits.status := cmd.bits.status // TODO This is not guaranteed to be the correct fix! We must fix this
  io.out.valid := Mux(loop_configured, unrolled_cmd.valid, cmd.valid && !is_loop_config_cmd && !is_loop_run_cmd)

  cmd.ready := Mux(is_loop_cmd, !loop_being_configured.configured, !loop_configured && io.out.ready)
  arb.io.out.ready := io.out.ready

  // Wire up overloaded signals
  ldA.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ldB.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ex.io.rob_overloaded := io.ex_utilization >= max_exs.U
  ldD.io.rob_overloaded := io.ld_utilization >= max_lds.U
  stC.io.rob_overloaded := io.st_utilization >= max_sts.U

  // Wire up iterator inputs
  ex.io.lda_completed := (ldA.io.loop_id =/= ex.io.loop_id) || ldA.io.idle
  ex.io.ldb_completed := (ldB.io.loop_id =/= ex.io.loop_id) || ldB.io.idle
  ex.io.ldd_completed := (ldD.io.loop_id =/= ex.io.loop_id) || ldD.io.idle
  ex.io.ld_ka := ldA.io.k
  ex.io.ld_kb := ldB.io.k
  ex.io.ld_j := ldB.io.j
  ex.io.ld_i := ldA.io.i

  stC.io.ex_completed := (ex.io.loop_id =/= stC.io.loop_id) || ex.io.idle
  stC.io.ex_k := ex.io.k
  stC.io.ex_j := ex.io.j
  stC.io.ex_i := ex.io.i

  val loops_configured = RegInit(0.U(16.W))
  dontTouch(loops_configured)

  // Create config registers
  when(cmd.valid && is_loop_cmd && !loop_being_configured.configured) {

    switch (cmd.bits.inst.funct) {
      is (LOOP_WS_CONFIG_BOUNDS) {
        loop_being_configured.max_k := cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        loop_being_configured.max_j := cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        loop_being_configured.max_i := cmd.bits.rs2(iterator_bitwidth-1, 0)

        loop_being_configured.pad_k := cmd.bits.rs1(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        loop_being_configured.pad_j := cmd.bits.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        loop_being_configured.pad_i := cmd.bits.rs1(iterator_bitwidth-1, 0)
      }

      is (LOOP_WS_CONFIG_ADDRS_AB) {
        loop_being_configured.a_dram_addr := cmd.bits.rs1
        loop_being_configured.b_dram_addr := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_ADDRS_DC) {
        loop_being_configured.d_dram_addr := cmd.bits.rs1
        loop_being_configured.c_dram_addr := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_AB) {
        loop_being_configured.a_dram_stride := cmd.bits.rs1
        loop_being_configured.b_dram_stride := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_DC) {
        loop_being_configured.d_dram_stride := cmd.bits.rs1
        loop_being_configured.c_dram_stride := cmd.bits.rs2
      }

      is (LOOP_WS) {
        loop_being_configured.ex_accumulate := cmd.bits.rs1(0)
        loop_being_configured.full_c := cmd.bits.rs1(1)
        loop_being_configured.low_d := cmd.bits.rs1(2)
        loop_being_configured.a_transpose := cmd.bits.rs2(0)
        loop_being_configured.b_transpose := cmd.bits.rs2(1)

        loop_being_configured.weightA := cmd.bits.rs1(15, 8) // TODO magic numbers

        loop_being_configured.configured := true.B

        loops_configured := loops_configured + 1.U
      }
    }
  }

  // Wire up request signals
  val ld_d_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val ex_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val st_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))

  val loop_requesting_ldA_id = Mux(head_loop.lda_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldA = loops(loop_requesting_ldA_id)
  ldA.io.req.bits.max_k := loop_requesting_ldA.max_k
  ldA.io.req.bits.max_i := loop_requesting_ldA.max_i
  ldA.io.req.bits.pad_k := loop_requesting_ldA.pad_k
  ldA.io.req.bits.pad_i := loop_requesting_ldA.pad_i
  ldA.io.req.bits.dram_addr := loop_requesting_ldA.a_dram_addr
  ldA.io.req.bits.dram_stride := loop_requesting_ldA.a_dram_stride
  ldA.io.req.bits.transpose := loop_requesting_ldA.a_transpose
  ldA.io.req.bits.addr_start := loop_requesting_ldA.a_addr_start
  ldA.io.req.bits.loop_id := loop_requesting_ldA_id

  ldA.io.req.valid := !loop_requesting_ldA.lda_started && loop_requesting_ldA.configured

  when (ldA.io.req.fire) {
    loop_requesting_ldA.running := true.B
    loop_requesting_ldA.lda_started := true.B
  }

  val loop_requesting_ldB_id = Mux(head_loop.ldb_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldB = loops(loop_requesting_ldB_id)
  ldB.io.req.bits.max_j := loop_requesting_ldB.max_j
  ldB.io.req.bits.max_k := loop_requesting_ldB.max_k
  ldB.io.req.bits.pad_j := loop_requesting_ldB.pad_j
  ldB.io.req.bits.pad_k := loop_requesting_ldB.pad_k
  ldB.io.req.bits.dram_addr := loop_requesting_ldB.b_dram_addr
  ldB.io.req.bits.dram_stride := loop_requesting_ldB.b_dram_stride
  ldB.io.req.bits.transpose := loop_requesting_ldB.b_transpose
  ldB.io.req.bits.addr_end := loop_requesting_ldB.b_addr_end
  ldB.io.req.bits.loop_id := loop_requesting_ldB_id

  ldB.io.req.valid := !loop_requesting_ldB.ldb_started && loop_requesting_ldB.configured

  when (ldB.io.req.fire) {
    loop_requesting_ldB.running := true.B
    loop_requesting_ldB.ldb_started := true.B
  }

  val loop_requesting_ex_id = Mux(head_loop.ex_started, tail_loop_id, head_loop_id)
  val loop_requesting_ex = loops(loop_requesting_ex_id)
  ex.io.req.bits.max_j := loop_requesting_ex.max_j
  ex.io.req.bits.max_k := loop_requesting_ex.max_k
  ex.io.req.bits.max_i := loop_requesting_ex.max_i
  ex.io.req.bits.pad_j := loop_requesting_ex.pad_j
  ex.io.req.bits.pad_k := loop_requesting_ex.pad_k
  ex.io.req.bits.pad_i := loop_requesting_ex.pad_i
  ex.io.req.bits.accumulate := loop_requesting_ex.ex_accumulate
  ex.io.req.bits.a_addr_start := loop_requesting_ex.a_addr_start
  ex.io.req.bits.b_addr_end := loop_requesting_ex.b_addr_end
  ex.io.req.bits.a_tranpose := loop_requesting_ex.a_transpose
  ex.io.req.bits.b_tranpose := loop_requesting_ex.b_transpose
  ex.io.req.bits.c_addr_start := ex_c_addr_start
  ex.io.req.bits.loop_id := loop_requesting_ex_id

  ex.io.req.valid := !loop_requesting_ex.ex_started && loop_requesting_ex.lda_started &&
    loop_requesting_ex.ldb_started && loop_requesting_ex.ldd_started && loop_requesting_ex.configured

  when (ex.io.req.fire) {
    loop_requesting_ex.running := true.B
    loop_requesting_ex.ex_started := true.B

    when (loop_requesting_ex.c_dram_addr =/= 0.U) {
      ex_c_addr_start := floorAdd(ex_c_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  val loop_requesting_ldD_id = Mux(head_loop.ldd_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldD = loops(loop_requesting_ldD_id)
  ldD.io.req.bits.max_j := loop_requesting_ldD.max_j
  ldD.io.req.bits.max_i := loop_requesting_ldD.max_i
  ldD.io.req.bits.pad_j := loop_requesting_ldD.pad_j
  ldD.io.req.bits.pad_i := loop_requesting_ldD.pad_i
  ldD.io.req.bits.dram_addr := loop_requesting_ldD.d_dram_addr
  ldD.io.req.bits.dram_stride := loop_requesting_ldD.d_dram_stride
  ldD.io.req.bits.low_d := loop_requesting_ldD.low_d
  ldD.io.req.bits.addr_start := ld_d_addr_start
  ldD.io.req.bits.loop_id := loop_requesting_ldD_id

  ldD.io.req.valid := !loop_requesting_ldD.ldd_started && loop_requesting_ldD.configured

  when (ldD.io.req.fire) {
    loop_requesting_ldD.running := true.B
    loop_requesting_ldD.ldd_started := true.B

    when (loop_requesting_ldD.c_dram_addr =/= 0.U) {
      ld_d_addr_start := floorAdd(ld_d_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  val loop_requesting_st_id = Mux(head_loop.st_started, tail_loop_id, head_loop_id)
  val loop_requesting_st = loops(loop_requesting_st_id)
  stC.io.req.bits.max_k := loop_requesting_st.max_k
  stC.io.req.bits.max_j := loop_requesting_st.max_j
  stC.io.req.bits.max_i := loop_requesting_st.max_i
  stC.io.req.bits.pad_j := loop_requesting_st.pad_j
  stC.io.req.bits.pad_i := loop_requesting_st.pad_i
  stC.io.req.bits.dram_addr := loop_requesting_st.c_dram_addr
  stC.io.req.bits.dram_stride := loop_requesting_st.c_dram_stride
  stC.io.req.bits.full_c := loop_requesting_st.full_c
  stC.io.req.bits.addr_start := st_c_addr_start
  stC.io.req.bits.loop_id := loop_requesting_st_id

  stC.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.ex_started && loop_requesting_st.configured

  when (stC.io.req.fire) {
    loop_requesting_st.running := true.B
    loop_requesting_st.st_started := true.B

    when (loop_requesting_st.c_dram_addr =/= 0.U) {
      st_c_addr_start := floorAdd(st_c_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  // Handle completed signals
  when (ldA.io.idle && loops(ldA.io.loop_id).running && loops(ldA.io.loop_id).lda_started) {
    loops(ldA.io.loop_id).lda_completed := true.B
  }

  when (ldB.io.idle && loops(ldB.io.loop_id).running && loops(ldB.io.loop_id).ldb_started) {
    loops(ldB.io.loop_id).ldb_completed := true.B
  }

  when (ex.io.idle && loops(ex.io.loop_id).running && loops(ex.io.loop_id).ex_started) {
    loops(ex.io.loop_id).ex_completed := true.B
  }

  when (ldD.io.idle && loops(ldD.io.loop_id).running && loops(ldD.io.loop_id).ldd_started) {
    loops(ldD.io.loop_id).ldd_completed := true.B
  }

  when (stC.io.idle && loops(stC.io.loop_id).running && loops(stC.io.loop_id).st_started) {
    loops(stC.io.loop_id).st_completed := true.B
  }

  when (head_loop.running && head_loop.all_completed()) {
    head_loop.reset()
    head_loop_id := ~head_loop_id
  }

  // Resets
  when (reset.asBool()) {
    loops.zipWithIndex.foreach { case (l, i) =>
      l.reset()
      l.a_addr_start := (i * (max_addr / concurrent_loops)).U
      l.b_addr_end := ((i+1) * (max_addr / concurrent_loops)).U
    }
  }
}

object LoopMatmul {
  def apply(in: DecoupledIO[RoCCCommand], ld_utilization: UInt, st_utilization: UInt, ex_utilization: UInt,
            block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
            max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
            mvin_rs2_t: MvinRs2, preload_rs1_t: PreloadRs, preload_rs2_t: PreloadRs,
            compute_rs1_t: ComputeRs, compute_rs2_t: ComputeRs, mvout_rs2_t: MvoutRs2)
           (implicit p: Parameters): Tuple2[DecoupledIO[RoCCCommand], Bool] = {
    val mod = Module(new LoopMatmul(block_size, coreMaxAddrBits, rob_size, max_lds, max_exs, max_sts,
      max_addr, max_acc_addr, input_w, acc_w, dma_max_bytes,
      mvin_rs2_t, preload_rs1_t, preload_rs2_t, compute_rs1_t, compute_rs2_t, mvout_rs2_t))
    mod.io.in <> in
    mod.io.ld_utilization := ld_utilization
    mod.io.st_utilization := st_utilization
    mod.io.ex_utilization := ex_utilization
    (mod.io.out, mod.io.busy)
  }

  def castDramOffset(dram_offset: UInt): UInt = {
    // Cast dram offsets to 32 bits max
    dram_offset & "hFFFFFFFF".U
  }
}

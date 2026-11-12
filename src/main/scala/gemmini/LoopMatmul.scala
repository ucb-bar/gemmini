
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import org.chipsalliance.cde.config.Parameters
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
  val is_resadd = Bool()
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
  mvin_cmd_rs2.num_rows := rows.asUInt
  mvin_cmd_rs2.num_cols := cols.asUInt
  mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt
  when(req.is_resadd){
    mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = false.B)
  }

  io.req.ready := state === idle
  io.i := i
  io.k := k
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded && req.dram_addr =/= 0.U
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when(req.dram_addr === 0.U){
    state := idle
  }.elsewhen(io.cmd.fire) {
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
  val is_resadd = Bool()
  val kmix = Bool()   // mixed-K loop: K-blocks with kmask 0 load MXFP8 B (alt base/stride, 2x the tiles)
  val kmask = UInt(128.W)
  val alt_addr = UInt(coreMaxAddrBits.W)
  val alt_stride = UInt(coreMaxAddrBits.W)
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

  // mixed-K: an MXFP8 K-block spans 2x the (quad) tiles; every K-block reserves that footprint (uniform layout)
  val fp8_k = req.kmix && !req.kmask(k >> log2Ceil(MxGeom(block_size).T))
  val mj = Mux(fp8_k, req.max_j << 1, req.max_j)
  val sp_cols = Mux(req.kmix, req.max_j << 1, req.max_j)
  val max_row_iterator = Mux(req.transpose, req.max_j, req.max_k)
  val max_col_iterator = Mux(req.transpose, req.max_k, mj)

  val row_pad = Mux(req.transpose, req.pad_j, req.pad_k)
  val col_pad = Mux(req.transpose, req.pad_k, req.pad_j)

  val max_col_dim = Mux(req.transpose, req.max_k, req.max_j)
  val max_blocks = Mux(max_col_dim <= max_block_len.U, max_col_dim, max_block_len.U)

  val sp_addr_start = Mux(req.is_resadd, req.addr_end, req.addr_end - req.max_k * sp_cols * block_size.U)

  val dram_offset = (row_iterator * Mux(fp8_k, req.alt_stride, req.dram_stride) + col_iterator) * block_size.U * (input_w/8).U
  val dram_addr = Mux(fp8_k, req.alt_addr, req.dram_addr) + LoopMatmul.castDramOffset(dram_offset)
  val sp_addr = sp_addr_start + (row_iterator * Mux(req.kmix, sp_cols, max_col_iterator) + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(max_row_iterator === max_row_iterator-1.U, row_pad, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := Mux(fp8_k, LOAD3_CMD, LOAD2_CMD)   // mixed-K MXFP8 B: load state 2 (its row stride)
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt
  mvin_cmd_rs2.num_cols := cols.asUInt
  mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt

  when (req.is_resadd){
    mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = true.B, read_full = false.B)
  }

  io.req.ready := state === idle
  io.k := k
  io.j := j
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded && req.dram_addr =/= 0.U
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when(req.dram_addr === 0.U){
    state := idle
  }.elsewhen(io.cmd.fire) {
    // The order here is k, j, i
    val j_blocks = Mux(req.transpose, 1.U, max_blocks)
    val k_blocks = Mux(req.transpose, max_blocks, 1.U)

    val next_j = floorAdd(j, j_blocks, mj)
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
  mvin_cmd_rs2.num_rows := rows.asUInt
  mvin_cmd_rs2.num_cols := cols.asUInt
  mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = false.B)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt

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
  val pad_j = UInt(log2Up(2*block_size).W)
  val pad_k = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(2*block_size).W)
  val a_tranpose = Bool()
  val b_tranpose = Bool()
  val accumulate = Bool()
  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr+1).W)
  val c_addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  val narrow_type = Bool()
  val narrow_act = Bool()
  val skip = Bool()
  val scale_cfg = Bool()   // loop manages its scales: open the ex stream with CONFIG_SCALE_MEM(this loop, wait)
  val scale_a_reuse = Bool()   // its A scales are already resident in its half (no A load was issued)
  val lut_cfg = Bool()     // its act-in codebooks are loop-managed: in its slot's LUT half
  val lut_cfg_w = Bool()   // its weight codebooks are loop-managed: in its slot's weight-LUT half
  val lut_gran = UInt(16.W)   // the matmul's LUT update granularity G (carried, not forced to 1)
  val shared = Bool()   // single x single on the shared (act-quad) acc layout (LOOP_WS rs2[11])
  val lut_kb = Bool()   // weight codebooks hold one set per K-block of the loop (LOOP_WS rs2[12])
  val kmix = Bool()     // mixed-K loop: per K-block LUT/low-precision (kmask 1) or MXFP8 on the shared layout
  val kmask = UInt(128.W)
  val mx = new MxCmdMode   // the loop's (low-precision) mode
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
    val mx = Output(new MxCmdMode)   // mode of the command it issues (per K-block in a mixed-K loop)
  })

  object State extends ChiselEnum {
    val idle, cfg, pre, comp = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulExecuteReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops))
  val block_ij = block_size.U
  val c_addr_start = /*(BigInt(1) << 31).U |*/ req.c_addr_start
  // mixed-K: K-block k is low precision (kmask 1: the loop's mode, max_j quad tiles) or MXFP8 (2*max_j tiles, shared
  // layout); every K-block reserves the MXFP8 B footprint

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val kb_of = (x: UInt) => x >> log2Ceil(MxGeom(block_size).T)
  val lut_k = !req.kmix || req.kmask(kb_of(k))
  val mj = Mux(lut_k, req.max_j, req.max_j << 1)
  val b_sp_cols = Mux(req.kmix, req.max_j << 1, req.max_j)
  val b_addr_start = req.b_addr_end - req.max_k * b_sp_cols * block_size.U
  val lut_ord = Reg(UInt(8.W))   // low-precision K-blocks before this one (codebook set / ring slot)

  val a_row = Mux(req.a_tranpose, k, i)
  val a_col = Mux(req.a_tranpose, i, k)
  val b_row = Mux(req.b_tranpose, j, k)
  val b_col = Mux(req.b_tranpose, k, j)

  val a_max_col = Mux(req.a_tranpose, req.max_i, req.max_k)
  val b_max_col = Mux(req.b_tranpose, req.max_k, b_sp_cols)

  val a_addr = req.a_addr_start + (a_row * a_max_col + a_col) * block_size.U
  val b_addr = b_addr_start + (b_row * b_max_col + b_col) * block_size.U
  // single: per-i C stride in tiles = max_j rounded up to whole W-group acc rows (4W tiles; 4 at DIM>=16)
  val tilesPerAccRow = 4 * MxGeom(block_size).W
  val ceil_max_j4 = ((req.max_j + (tilesPerAccRow - 1).U) >> log2Ceil(tilesPerAccRow)) << log2Ceil(tilesPerAccRow)
  // act-quad x wei-single: col-tiles j, j+1 share one quad acc row (j odd at lane offset DIM/4); odd max_j: last half full
  val pair_max_j = (req.max_j + 1.U) >> 1
  // single x quad (shared acc layout): row-tiles 2q, 2q+1 fill the two halves of quad tile q's acc rows
  val c_addr_quad = if (block_size >= 16) Mux(req.narrow_act, c_addr_start + (i * req.max_j + j) * block_size.U,
    c_addr_start + ((i >> 1) * req.max_j + j) * block_size.U + i(0) * (block_size / 2).U)
    else c_addr_start + (i * req.max_j + j) * block_size.U
  // single x single, shared layout: tile (i, j) is quadrant (i(0), j(0)) of quad tile (i>>1, j>>1)
  val c_addr_shared = c_addr_start + ((i >> 1) * ((req.max_j + 1.U) >> 1) + (j >> 1)) * block_size.U +
    i(0) * (block_size / 2).U + j(0) * (block_size / 4).U
  // mixed-K MXFP8 K-block: shared layout with 2*max_j tiles (quad tile j>>1 of the loop's max_j)
  val c_addr_kmix8 = c_addr_start + ((i >> 1) * req.max_j + (j >> 1)) * block_size.U + i(0) * (block_size / 2).U +
    j(0) * (block_size / 4).U
  val c_addr = Mux(req.kmix && !lut_k, c_addr_kmix8, Mux(req.shared, c_addr_shared, Mux(req.narrow_type, c_addr_quad,
    Mux(req.narrow_act, c_addr_start + (i * pair_max_j + (j >> 1)) * block_size.U + j(0) * (block_size / 4).U,
      c_addr_start + (i * ceil_max_j4 + j) * (block_size / 4).U))))

  val a_cols = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)
  val a_rows = block_ij - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)       
  val b_cols = block_ij - Mux(j === mj - 1.U, req.pad_j, 0.U)       
  val b_rows = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)   
  val c_cols = block_ij - Mux(j === mj - 1.U, req.pad_j, 0.U)       
  val c_rows = block_ij - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)      


  val pre_cmd = Wire(new RoCCCommand)
  pre_cmd := DontCare
  pre_cmd.inst.funct := PRELOAD_CMD

  val pre_cmd_rs1 = Wire(preload_rs1_t.cloneType)
  pre_cmd_rs1 := DontCare
  pre_cmd_rs1.num_rows := b_rows.asUInt
  pre_cmd_rs1.num_cols := b_cols.asUInt
  pre_cmd_rs1.local_addr := Mux(i === 0.U, cast_to_sp_addr(pre_cmd_rs1.local_addr, b_addr),
    garbage_addr(pre_cmd_rs1.local_addr))

  val pre_cmd_rs2 = Wire(preload_rs2_t.cloneType)
  pre_cmd_rs2 := DontCare
  pre_cmd_rs2.num_rows := c_rows.asUInt
  pre_cmd_rs2.num_cols := c_cols.asUInt
  

  pre_cmd_rs2.local_addr := cast_to_acc_addr(pre_cmd_rs2.local_addr, c_addr, accumulate = req.accumulate || k =/= 0.U, read_full = false.B)

  pre_cmd.rs1 := pre_cmd_rs1.asUInt
  pre_cmd.rs2 := pre_cmd_rs2.asUInt

  val comp_cmd = Wire(new RoCCCommand())
  comp_cmd := DontCare
  comp_cmd.inst.funct := Mux(i === 0.U, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)

  val comp_cmd_rs1 = Wire(compute_rs1_t.cloneType)
  comp_cmd_rs1 := DontCare
  comp_cmd_rs1.num_rows := a_rows.asUInt
  comp_cmd_rs1.num_cols := a_cols.asUInt
  comp_cmd_rs1.local_addr := cast_to_sp_addr(comp_cmd_rs1.local_addr, a_addr)

  val comp_cmd_rs2 = Wire(compute_rs2_t.cloneType)
  comp_cmd_rs2 := DontCare
  comp_cmd_rs2.num_rows := block_ij
  comp_cmd_rs2.num_cols := block_ij
  comp_cmd_rs2.local_addr := garbage_addr(comp_cmd_rs2.local_addr)

  comp_cmd.rs1 := comp_cmd_rs1.asUInt
  comp_cmd.rs2 := comp_cmd_rs2.asUInt

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

  // CONFIG_SCALE_MEM for this loop: bounds = loop dims, act/wgt half = loop slot, rs2[17] = managed wait.
  val cfg_cmd = Wire(new RoCCCommand)
  cfg_cmd := DontCare
  cfg_cmd.inst.funct := CONFIG_SCALE_MEM
  cfg_cmd.rs1 := Cat(0.U(2.W), req.loop_id(0), req.loop_id(0), req.max_k(8, 0), req.max_j(8, 0), req.max_i(8, 0), 0.U(33.W))
  // rs2: [15:0] G, [17] managed, [18] don't wait for the act half, [19] shared layout, [20]/[21] act-in / weight LUT
  // half = slot, [22] per-K-block weight codebooks
  // [23] mixed-K loop (QuantLut codebook set by slot; scale reads of MXFP8 K-blocks from the quad layout)
  cfg_cmd.rs2 := Cat(req.kmix, req.lut_kb, req.lut_cfg_w, req.lut_cfg, req.shared || req.kmix, req.scale_a_reuse, 1.U(1.W),
    0.U(1.W), req.lut_gran)

  io.cmd.valid := state =/= idle && !io.rob_overloaded && (state === cfg || ld_ahead) && !req.skip
  io.cmd.bits := Mux(state === cfg, cfg_cmd, Mux(state === pre, pre_cmd, comp_cmd))

  io.loop_id := req.loop_id
  // per K-block mode: low precision = the loop's tag (+ its codebook set), MXFP8 = E4M3 weight on the shared layout
  io.mx := req.mx
  when (req.kmix) {
    io.mx.slot := Mux(lut_k, lut_ord(1, 0), 0.U)
    when (!lut_k) { io.mx.wgt := 0.U; io.mx.shared := true.B }
  }

  when(req.skip) {
    state := idle
  }.elsewhen (io.cmd.fire) {
    when (state === cfg) {
      state := pre
    }.elsewhen (state === pre) {
      state := comp
    }.otherwise {
      val next_i = floorAdd(i, 1.U, req.max_i)
      val next_j = floorAdd(j, 1.U, mj, next_i === 0.U)
      val next_k = floorAdd(k, 1.U, req.max_k, next_j === 0.U && next_i === 0.U)

      k := next_k
      j := next_j
      i := next_i
      when (req.kmix && lut_k && kb_of(next_k) =/= kb_of(k)) { lut_ord := lut_ord + 1.U }

      state := Mux(next_k === 0.U && next_j === 0.U && next_i === 0.U, idle, pre)
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := Mux(io.req.bits.scale_cfg, cfg, pre)
    j := 0.U
    k := 0.U
    i := 0.U
    lut_ord := 0.U
  }

  assert(!(state =/= idle && req.a_tranpose && req.b_tranpose))
}

// StC

// Shared StC / StCSpad iteration. Single throughput: j counts 4-tile groups; at DIM<16 one acc row holds W groups,
// so j steps by W and the row is read as 2 chunks of chunkTiles tiles each.
object MxStoreIter {
  def fold(geom: MxGeom, useMx: Boolean): Bool = (useMx && geom.W > 1).B
  def jSpan(geom: MxGeom, fold: Bool, j: UInt, iterMaxJ: UInt, blocks: UInt): UInt =
    Mux(fold, Mux(j + geom.W.U <= iterMaxJ, geom.W.U, iterMaxJ - j), blocks)
  // chunks holding data at step j = ceil(tiles left in this acc row / chunkTiles)
  def chunks(geom: MxGeom, j: UInt, maxJ: UInt): UInt = {
    val rem  = maxJ - (j << 2)
    val span = Mux(rem > (4 * geom.W).U, (4 * geom.W).U, rem)
    (span +& (geom.chunkTiles - 1).U) >> log2Ceil(geom.chunkTiles)
  }
  // single throughput: acc slots (4-tile groups) per i-tile, padded to W so a W-wide acc row never spans two i-tiles
  def accGroups(geom: MxGeom, single: Bool, iterMaxJ: UInt): UInt =
    Mux(single, ((iterMaxJ + (geom.W - 1).U) >> log2Ceil(geom.W)) << log2Ceil(geom.W), iterMaxJ)
  // tiles holding data in chunk `chunk` of step j (a full chunk = chunkTiles; less only when partialChunks)
  def chunkValidTiles(geom: MxGeom, j: UInt, maxJ: UInt, chunk: UInt): UInt = {
    val rem  = maxJ - (j << 2)
    val left = Mux(rem > (4 * geom.W).U, (4 * geom.W).U, rem) - chunk * geom.chunkTiles.U
    Mux(left > geom.chunkTiles.U, geom.chunkTiles.U, left)
  }
  // this chunk ends the output row (gets pad_j)
  def chunkEndsRow(geom: MxGeom, j: UInt, maxJ: UInt, chunk: UInt): Bool =
    (j << 2) + chunk * geom.chunkTiles.U + chunkValidTiles(geom, j, maxJ, chunk) === maxJ
  // last execute tile (uncompressed ex_j) the store of groups j..lastGroup waits for
  def ejHigh(multi: Bool, lastGroup: UInt, iterMaxJ: UInt, maxJ: UInt): UInt =
    Mux(!multi,
      Mux((lastGroup === iterMaxJ - 1.U) && (maxJ % 4.U =/= 0.U), maxJ - 1.U, lastGroup * 4.U + 3.U),
      lastGroup)
}

class LoopMatmulStCReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(2 * block_size).W)   // ipair: C rows of the 2*DIM-row quad tile
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val full_c = Bool()
  val act = UInt(Activation.bitwidth.W)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  val is_resadd = Bool()
  val output_mx_format = UInt(2.W)
  val activation_mx_format = UInt(2.W)
  val mx_multi_elem = Bool()   // throughput: 2 elements/lane (datatype-independent)
  val mx_multi_elem_act = Bool()   // ACTIVATION (output-row) throughput
  val pair = Bool()   // act-quad x wei-single: max_j single col-tiles stored as ceil(max_j/2) quad pairs
  val ipair = Bool()   // single x quad: row-tile pairs stored as quad tiles, mvout rows count C rows
}

class LoopMatmulStC(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, max_block_len: Int, concurrent_loops: Int, tilesPerMxBlock: Int, mvout_rs2_t: MvoutRs2,
                    use_mx_scaling: Boolean = true)
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
    val idle, st, ln_config, ln_st = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulStCReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops))

  val iter_max_j = Mux(!req.mx_multi_elem, (req.max_j + 3.U) / 4.U, Mux(req.pair, (req.max_j + 1.U) >> 1, req.max_j))
  // quad output width in DIM-col tiles (N/DIM): 2 per quad tile, 1 per pair member
  val quad_nd = Mux(req.pair, req.max_j, req.max_j << 1)
  // an odd pair count leaves the last pair half full (DIM cols = DIM/2 quad col units)
  val pair_pad = Mux(req.pair && req.max_j(0), (block_size / 2).U, 0.U)
  val iter_max_i = req.max_i
  val ex_j_compressed = Mux(!req.mx_multi_elem, io.ex_j / 4.U, io.ex_j)
  val ex_i_compressed = io.ex_i

  when(state =/= idle && !req.mx_multi_elem) {
    assert((iter_max_i >= tilesPerMxBlock.U) && (iter_max_i % tilesPerMxBlock.U === 0.U), "tiles need to be multiples of mxBlockSize")
    assert((req.max_j >= tilesPerMxBlock.U) && (req.max_j % tilesPerMxBlock.U === 0.U), "tiles need to be multiples of mxBlockSize")
  }

  val geom = MxGeom(block_size)
  // DRAM offsets in BYTES: the StCSpad spad-row formula x DIM bytes, with its implicit row pitch (N * elem)
  // replaced by dram_stride * elem so N-chunked loops land in a wider C. One acc chunk = 2R cols.
  val out_bf16 = req.output_mx_format === 3.U || req.full_c
  val geomR = MxGeom(block_size).R
  val chunk_dram_stride = Mux(out_bf16, (4 * geomR).U, (2 * geomR).U)
  val bf16_j_bytes = 8 * block_size   // E4M3-single bf16 j-group: 4 tiles x DIM cols x 2 B

  val max_blocks = Mux(req.full_c, 1.U, Mux(iter_max_j <= max_block_len.U, iter_max_j, max_block_len.U))

  // Non-normalization-related iterators and calculations
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val chunk_id = RegInit(0.U(GemminiISA.MX_CHUNK_ID_BITS.W))

  val acc_addr_start = /*(BigInt(1) << 31).U | (req.full_c << 29.U).asUInt |*/ req.addr_start

  val dram_offset =  MuxCase((i * quad_nd) * block_size.U + j*(block_size/8).U, Seq(
    (req.full_c || (req.output_mx_format === 3.U && !req.mx_multi_elem)) ->
      (i * block_size.U * req.dram_stride * 2.U + j * Mux(req.full_c, ((block_size / 2) * block_size).U, bf16_j_bytes.U)),
    (!req.mx_multi_elem && (req.output_mx_format === 0.U)) -> (i * block_size.U * req.dram_stride + j * (4 * block_size).U),
    // Internal-spad flat layout: output row R (of N cols, bf16) -> spad rows [R*(N/8), R*(N/8)+(N/8)).
    // A row-tile of H output rows has i-stride i*H*(N/8) spad-rows. N enters via max_j = N/32, so
    // i*H*(N/8) = i*max_j*(H/2)*block_size: act-quad H=32 -> x16... = i*max_j*block_size*8; act-single
    // (mode6/7) H=16 -> i*max_j*block_size*4. The old x8-only assumed quad and doubled the act-single
    // i-stride (tiles i>=1 landed at 2x -> rows>=16 garbage). Gate the height on mx_multi_elem_act; the
    // j-term: each J-tile = 2*DIM cols (a 64-col half) -> block_size/8 spad-rows (=4), N-independent.
    // quad BF16: one tile (2*DIM rows if act quad) x 2*DIM cols per mvout, row pitch dram_stride elements
    ((req.mx_multi_elem) && (req.output_mx_format === 3.U)) -> ((i * Mux(req.mx_multi_elem_act, (2 * block_size).U, block_size.U) * req.dram_stride + j * (2 * block_size).U) * 2.U)
  ))
  // non-MX (the controller drives mx_multi_elem): upstream's offset, one DIM x DIM tile of input_w (acc_w if full_c)
  val plain_dram_offset = (i * req.dram_stride + j) * block_size.U * Mux(req.full_c, (acc_w / 8).U, (input_w / 8).U)
  val dram_addr = req.dram_addr + LoopMatmul.castDramOffset(if (use_mx_scaling) dram_offset else plain_dram_offset)
  val acc_addr_offset = (i * MxStoreIter.accGroups(geom, !req.mx_multi_elem, iter_max_j) + j) * block_size.U
  val sp_addr = acc_addr_start + acc_addr_offset
  val blocks = Mux(j + max_blocks <= iter_max_j, max_blocks, iter_max_j-j)
  val cols = Mux(!req.mx_multi_elem, {
    // this chunk's own tiles (DIM/4 acc entries each)
    MxStoreIter.chunkValidTiles(geom, j, req.max_j, chunk_id) * (block_size / 4).U -
      Mux(MxStoreIter.chunkEndsRow(geom, j, req.max_j, chunk_id), req.pad_j, 0.U)
  }, {
    blocks * block_size.U - Mux(j + blocks >= iter_max_j, req.pad_j +& pair_pad, 0.U)
  })
  val rows = Mux(req.ipair, (2 * block_size).U, block_size.U) - Mux(i === iter_max_i-1.U, req.pad_i, 0.U)

  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := Mux(!req.mx_multi_elem,
    dram_addr + LoopMatmul.castDramOffset(chunk_id * chunk_dram_stride),
    dram_addr)

  val mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  mvout_cmd_rs2 := DontCare
  mvout_cmd_rs2.num_rows := rows.asUInt
  mvout_cmd_rs2.num_cols := cols.asUInt
  mvout_cmd_rs2.local_addr := cast_to_acc_addr(mvout_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = req.full_c)
  mvout_cmd_rs2.mx_chunk_id := chunk_id
  mvout_cmd_rs2.reuse_tiled := false.B // DRAM store path never tiles (dst is not spad)
  mvout_cmd_rs2._spacer2 := 0.U
  mvout_cmd.rs2 := mvout_cmd_rs2.asUInt

  // Layernorm iterators and calculations
  val ln_row = Reg(UInt(iterator_bitwidth.W))
  val ln_cmd = Reg(UInt(iterator_bitwidth.W))
  val ln_stat_id = Reg(UInt(iterator_bitwidth.W))

  val NORM_STAT_IDS = 2 // TODO magic number

  val ln_norm_cmds = VecInit(VecInit(NormCmd.SUM, NormCmd.MEAN), VecInit(NormCmd.VARIANCE, NormCmd.INV_STDDEV),
    VecInit(NormCmd.RESET, NormCmd.RESET))

  val sm_norm_cmds = VecInit(VecInit(NormCmd.MAX, NormCmd.MAX), VecInit(NormCmd.SUM_EXP, NormCmd.INV_SUM_EXP),
    VecInit(NormCmd.RESET, NormCmd.RESET))

  val ln_stat_ids = Mux(rows -& ln_row > NORM_STAT_IDS.U, NORM_STAT_IDS.U, rows -& ln_row)

  val ln_r = ln_row +& ln_stat_id

  val ln_sp_addr = acc_addr_start +& (i * req.max_j +& j) * block_size.U +& ln_r
  val ln_norm_cmd = Mux(j +& max_blocks >= req.max_j,
    Mux(req.act === Activation.LAYERNORM, ln_norm_cmds(ln_cmd)(1), sm_norm_cmds(ln_cmd)(1)),
    Mux(req.act === Activation.LAYERNORM, ln_norm_cmds(ln_cmd)(0), sm_norm_cmds(ln_cmd)(0)))

  // TODO we assume for now that full_C and layernorm aren't true at the same
  val ln_dram_offset = ((i * req.dram_stride +& j) * block_size.U +& ln_r * req.dram_stride) * (input_w/8).U
  val ln_dram_addr = req.dram_addr + LoopMatmul.castDramOffset(ln_dram_offset)

  val ln_config_norm_rs1 = Wire(new GemminiISA.ConfigNormRs1)
  ln_config_norm_rs1 := DontCare
  ln_config_norm_rs1.set_stats_id_only := 1.U
  ln_config_norm_rs1.cmd_type := CONFIG_NORM
  ln_config_norm_rs1.norm_stats_id := ln_stat_id

  val ln_config_norm = Wire(new RoCCCommand)
  ln_config_norm := DontCare
  ln_config_norm.inst.funct := CONFIG_CMD
  ln_config_norm.rs1 := ln_config_norm_rs1.asUInt
  ln_config_norm.rs2 := DontCare

  val ln_mvout_cmd = Wire(new RoCCCommand)
  ln_mvout_cmd := DontCare
  ln_mvout_cmd.inst.funct := STORE_CMD
  ln_mvout_cmd.rs1 := ln_dram_addr

  val ln_mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  ln_mvout_cmd_rs2 := DontCare
  ln_mvout_cmd_rs2.num_rows := 1.U
  ln_mvout_cmd_rs2.num_cols := cols.asUInt
  ln_mvout_cmd_rs2.local_addr := cast_to_acc_addr(ln_mvout_cmd_rs2.local_addr, ln_sp_addr, accumulate = false.B, read_full = req.full_c)
  ln_mvout_cmd_rs2.local_addr.norm_cmd := ln_norm_cmd
  ln_mvout_cmd_rs2.reuse_tiled := false.B // layernorm store path never tiles
  ln_mvout_cmd.rs2 := ln_mvout_cmd_rs2.asUInt

  io.req.ready := state === idle
  io.j := j
  io.i := i
  io.idle := state === idle

  // The order here is k, j, i when not doing LAYERNORM or SOFTMAX. As in StCSpad: an FP8 j-group is 4 execute
  // tiles, so gate on the UNCOMPRESSED ex_j reaching the group's last tile (ej_high); the terminal group has no
  // drain margin and waits for ex_completed.
  // DIM<16: the widened acc row of j also holds j+1..j+W-1; store once per W-group, after all of them
  val fold = MxStoreIter.fold(geom, use_mx_scaling)   // non-MX acc rows are not widened
  // quad at DIM<16: a W-wide acc row is only fully written when N is a multiple of the 32-col MX block
  when(state =/= idle && req.mx_multi_elem && fold) {
    assert(iter_max_j % geom.W.U === 0.U, "quad at DIM<16: max_j must be a multiple of W (N a multiple of the MX block)")
  }
  val j_span = MxStoreIter.jSpan(geom, fold, j, iter_max_j, blocks)
  val last_group = j + j_span - 1.U
  val ej_high = MxStoreIter.ejHigh(req.mx_multi_elem, last_group, iter_max_j, req.max_j)
  val terminal_needs_drain = (last_group === iter_max_j - 1.U) && (iter_max_j > 1.U)
  val ex_ahead = WireInit(io.ex_completed ||
    ((req.act =/= Activation.LAYERNORM) && (req.act =/= Activation.SOFTMAX) &&
      (io.ex_k === req.max_k - 1.U &&
        (io.ex_j > ej_high ||
          ((io.ex_j === ej_high) && ex_i_compressed > i && !terminal_needs_drain)))))
  when(req.is_resadd){
    ex_ahead := io.ex_completed || (ex_i_compressed > i || (ex_i_compressed === i && ex_j_compressed >= j + blocks))
  }

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ex_ahead && req.dram_addr =/= 0.U
  io.cmd.bits := MuxCase(mvout_cmd, Seq(
    (state === ln_config) -> ln_config_norm,
    (state === ln_st) -> ln_mvout_cmd,
  ))

  io.loop_id := req.loop_id

  when (req.dram_addr === 0.U) {
    state := idle
  }.elsewhen (io.cmd.fire && state === st) {
    when (!req.mx_multi_elem && chunk_id < (MxStoreIter.chunks(geom, j, req.max_j) - 1.U)) {
      chunk_id := chunk_id + 1.U
    }.otherwise {
      chunk_id := 0.U
      val next_i = floorAdd(i, 1.U, iter_max_i)
      val next_j = floorAdd(j, Mux(fold, geom.W.U, max_blocks), iter_max_j, next_i === 0.U)

      i := next_i
      j := next_j

      when (next_i === 0.U && next_j === 0.U) {
        state := idle
      }
    }
  }.elsewhen (io.cmd.fire && state === ln_config) {
    state := ln_st
  }.elsewhen (io.cmd.fire && state === ln_st) {
    val next_j = floorAdd(j, max_blocks, req.max_j)
    val next_stat_id = floorAdd(ln_stat_id, 1.U, ln_stat_ids, next_j === 0.U)
    val next_cmd = floorAdd(ln_cmd, 1.U, ln_norm_cmds.size.U, next_j === 0.U && next_stat_id === 0.U)
    val next_row = floorAdd(ln_row, NORM_STAT_IDS.U, rows, next_j === 0.U && next_stat_id === 0.U && next_cmd === 0.U)
    val next_i = floorAdd(i, 1.U, req.max_i,
      next_j === 0.U && next_stat_id === 0.U && next_cmd === 0.U && next_row === 0.U)

    j := next_j
    ln_stat_id := next_stat_id
    ln_cmd := next_cmd
    ln_row := next_row
    i := next_i

    when (next_i === 0.U && next_row === 0.U && next_cmd === 0.U && next_stat_id === 0.U && next_j === 0.U) {
      state := idle
    }.elsewhen (next_j === 0.U) {
      state := ln_config
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := Mux((io.req.bits.act === Activation.LAYERNORM) || (io.req.bits.act === Activation.SOFTMAX), ln_config, st)

    j := 0.U
    i := 0.U
    chunk_id := 0.U
    ln_row := 0.U
    ln_cmd := 0.U
    ln_stat_id := 0.U
  }
}

// StCSpad
class LoopMatmulStCSpadReq(val block_size: Int, val iterator_bitwidth: Int, val max_addr: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(2 * block_size).W)   // ipair: C rows of the 2*DIM-row quad tile
  val dst_addr = UInt(log2Up(max_addr).W)
  val src_addr = UInt(log2Up(max_acc_addr).W)
  val full_c = Bool()
  val act = UInt(Activation.bitwidth.W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  val is_resadd = Bool()
  val output_mx_format = UInt(2.W)
  val activation_mx_format = UInt(2.W)
  val mx_multi_elem = Bool()   // throughput: 2 elements/lane (datatype-independent)
  val mx_multi_elem_act = Bool()   // ACTIVATION (output-row) throughput
  val pair = Bool()   // act-quad x wei-single: max_j single col-tiles stored as ceil(max_j/2) quad pairs
  val ipair = Bool()   // single x quad: row-tile pairs stored as quad tiles, mvout rows count C rows
  // Output C region overlaps an A/B operand region this loop still reads (decided at dispatch);
  // gates the store on full execute drain.
  val dst_overlaps_operands = Bool()
  // FP8 gated tiled requant->spad store: output deposited block-tiled (operand-A layout) for in-place reuse.
  val reuse_tiled = Bool()
}

class LoopMatmulStCSpad(block_size: Int, iterator_bitwidth: Int, max_addr: Int, max_acc_addr: Int,
                        input_w: Int, acc_w: Int, max_block_len: Int, concurrent_loops: Int, tilesPerMxBlock: Int,
                        mvout_spad_rs1_t: MvoutSpadRs1, mvout_rs2_t: MvoutRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulStCSpadReq(block_size, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops)))
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

  val req = Reg(new LoopMatmulStCSpadReq(block_size, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops))

  val iter_max_j = Mux(!req.mx_multi_elem, (req.max_j + 3.U) / 4.U, Mux(req.pair, (req.max_j + 1.U) >> 1, req.max_j))
  // quad output width in DIM-col tiles (N/DIM): 2 per quad tile, 1 per pair member
  val quad_nd = Mux(req.pair, req.max_j, req.max_j << 1)
  // an odd pair count leaves the last pair half full (DIM cols = DIM/2 quad col units)
  val pair_pad = Mux(req.pair && req.max_j(0), (block_size / 2).U, 0.U)
  val iter_max_i = req.max_i
  val ex_j_compressed = Mux(!req.mx_multi_elem, io.ex_j / 4.U, io.ex_j)
  val ex_i_compressed = io.ex_i

  when(state =/= idle && !req.mx_multi_elem) {
    assert((iter_max_i >= tilesPerMxBlock.U) && (iter_max_i % tilesPerMxBlock.U === 0.U), "tiles need to be multiples of mxBlockSize")
    assert((req.max_j >= tilesPerMxBlock.U) && (req.max_j % tilesPerMxBlock.U === 0.U), "tiles need to be multiples of mxBlockSize")
  }

  // DIM<16 widen: the acc row is W*block_size wide (a full block for 2 output rows) = DIM=16-shaped.
  // Mimic DIM=16's store: 2 chunks per wide read (the 2 output rows), and step col-tiles by W (W col-tiles/block).
  val geom = MxGeom(block_size)
  val accBlockSize = geom.W * block_size
  // chunk1 sits one requant output below chunk0: codeBeats (8-bit) / bf16Beats (bf16) spad rows; full_c keeps 2T
  val single_chunk_stride = geom.codeBeats
  val chunk_spad_stride = Mux(req.full_c, (2 * tilesPerMxBlock).U,
    Mux(req.output_mx_format === 3.U, geom.bf16Beats.U,
      Mux(req.reuse_tiled && req.output_mx_format === 0.U, (block_size * tilesPerMxBlock).U, single_chunk_stride.U)))

  val max_blocks = Mux(req.full_c, 1.U, Mux(iter_max_j <= max_block_len.U, iter_max_j, max_block_len.U))
  assert(max_block_len == 1, "there might be hw bugs if block length > 1, disabled for now")

  // Non-normalization-related iterators and calculations
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val chunk_id = RegInit(0.U(GemminiISA.MX_CHUNK_ID_BITS.W))

  val acc_addr_start = req.src_addr

  val dst_offset = MuxCase(
    // FP4/FP6 requant: flat = i*N + 2*j; gated tiled = i*N + 32*j (j-term x16 -> tiled operand layout).
    Mux(req.reuse_tiled,
      (i * quad_nd) * block_size.U + j * (block_size * 2).U,
      (i * quad_nd) * block_size.U + j * 2.U), Seq(
    // single fmt3: a j-group (4 tiles x DIM bf16 cols = 8*DIM B) is 8 spad rows at every DIM; full_c keeps block_size/2.
    (req.full_c || (req.output_mx_format === 3.U && !req.mx_multi_elem)) -> ((i * req.max_j) * block_size.U * 2.U + j*Mux(req.full_c, (block_size/2).U, 8.U)),
    (!req.mx_multi_elem && (req.output_mx_format === 0.U)) ->
      Mux(req.reuse_tiled,
        // GATED tiled (FP8): i*tiles_N*16 + 64*j  (j-term x16 vs flat -> within-tile-row-inner layout)
        ((i * req.max_j) * block_size.U + j * (block_size * 4).U),
        // flat
        ((i * req.max_j) * block_size.U + j * 4.U)),
    // Internal-spad flat layout: output row R (of N cols, bf16) -> spad rows [R*(N/8), R*(N/8)+(N/8)).
    // A row-tile of H output rows has i-stride i*H*(N/8) spad-rows. N enters via max_j = N/32, so
    // i*H*(N/8) = i*max_j*(H/2)*block_size: act-quad H=32 -> x16... = i*max_j*block_size*8; act-single
    // (mode6/7) H=16 -> i*max_j*block_size*4. The old x8-only assumed quad and doubled the act-single
    // i-stride (tiles i>=1 landed at 2x -> rows>=16 garbage). Gate the height on mx_multi_elem_act.
    // j-term: J-tile = 2*DIM bf16 cols = 4*DIM B = 4 flat spad rows at every DIM.
    ((req.mx_multi_elem) && (req.output_mx_format === 3.U)) -> ((i*quad_nd)*block_size.U*Mux(req.mx_multi_elem_act, 4.U, 2.U) + j * 4.U)
    ))
  val dst_addr = Mux(!req.mx_multi_elem,
    req.dst_addr + dst_offset + chunk_id * chunk_spad_stride,
    req.dst_addr + dst_offset)

  val acc_addr_offset = (i * MxStoreIter.accGroups(geom, !req.mx_multi_elem, iter_max_j) + j) * block_size.U
  val src_addr = acc_addr_start + acc_addr_offset
  val blocks = Mux(j + max_blocks <= iter_max_j, max_blocks, iter_max_j-j)
  val cols = Mux(!req.mx_multi_elem, {
    // this chunk's own tiles (DIM/4 acc entries each)
    MxStoreIter.chunkValidTiles(geom, j, req.max_j, chunk_id) * (block_size / 4).U -
      Mux(MxStoreIter.chunkEndsRow(geom, j, req.max_j, chunk_id), req.pad_j, 0.U)
  }, {
    // DIM=8 widen: one read covers the wide row = 2 col-tiles = accBlockSize (16) cols, mimicking DIM=16.
    blocks * accBlockSize.U - Mux(j + blocks >= iter_max_j, req.pad_j +& pair_pad, 0.U)
  })
  val rows = Mux(req.ipair, (2 * block_size).U, block_size.U) - Mux(i === iter_max_i-1.U, req.pad_i, 0.U)

  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_SPAD_CMD

  val mvout_cmd_rs1 = Wire(mvout_spad_rs1_t.cloneType)
  mvout_cmd_rs1 := DontCare
  mvout_cmd_rs1.local_addr := cast_to_sp_addr(mvout_cmd_rs1.local_addr, dst_addr)
  // Absolute destination row step (bit 31 set), from THIS loop's own J and format captured at request time. The
  // store controller used to derive it from the live CONFIG_SCALE_MEM bounds, which a younger loop's config can
  // replace while this loop's last stores are still queued. Non-MX builds read a flagged stride as the old 1.
  val st_j = req.max_j
  val st_wide = req.output_mx_format === 3.U
  val st_row_step = Mux(req.reuse_tiled, 1.U,
    Mux(st_wide && !req.mx_multi_elem, st_j * 2.U,   // single row pitch: N*2/DIM (BF16), N/DIM (FP8); odd max_j at DIM32
      Mux(st_wide, quad_nd * Mux(req.mx_multi_elem_act, 4.U, 2.U),
        Mux(!req.mx_multi_elem, st_j, quad_nd))))
  mvout_cmd_rs1.stride := (BigInt(1) << 31).U | st_row_step.pad(31)
  mvout_cmd.rs1 := mvout_cmd_rs1.asUInt

  val mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  mvout_cmd_rs2 := DontCare
  mvout_cmd_rs2.num_rows := rows.asUInt
  mvout_cmd_rs2.num_cols := cols.asUInt
  mvout_cmd_rs2.local_addr := cast_to_acc_addr(mvout_cmd_rs2.local_addr, src_addr, accumulate = false.B, read_full = req.full_c)
  mvout_cmd_rs2.mx_chunk_id := chunk_id
  mvout_cmd_rs2.reuse_tiled := req.reuse_tiled
  // rs2[63] (rows spacer): BF16 store -- its beats pass the requantizer unquantized, so the RS lets it overlap
  // SPAD_REQUANT (which then shares the requantizer beat by beat)
  mvout_cmd.rs2 := Cat(st_wide, mvout_cmd_rs2.asUInt(62, 0))

  io.req.ready := state === idle
  io.j := j
  io.i := i
  io.idle := state === idle

  // FP8 store j-group = 4 execute tiles; gate on the UNCOMPRESSED ex_j reaching the
  // group's last execute-tile (ej_high) so we don't read before all tiles' final-k accumulate.
  // DIM<16: the widened acc row of tile j also holds j+1..j+W-1 (jStep W below), so wait for all their final k
  val fold = MxStoreIter.fold(geom, true)
  // quad at DIM<16: a W-wide acc row is only fully written when N is a multiple of the 32-col MX block
  when(state =/= idle && req.mx_multi_elem && fold) {
    assert(iter_max_j % geom.W.U === 0.U, "quad at DIM<16: max_j must be a multiple of W (N a multiple of the MX block)")
  }
  val j_span = MxStoreIter.jSpan(geom, fold, j, iter_max_j, blocks)
  val last_group = j + j_span - 1.U
  val ej_high = MxStoreIter.ejHigh(req.mx_multi_elem, last_group, iter_max_j, req.max_j)
  // terminal j-group has no drain margin: route it through drain-gated ex_completed, not the early ex_i window.
  val terminal_needs_drain = (last_group === iter_max_j - 1.U) && (iter_max_j > 1.U)
  // WAR hazard: if the output aliases an A/B row a later execute tile still reads, releasing the store on
  // the early window lets the DMA overwrite the live operand. On overlap, release only via io.ex_completed
  // (all operand reads done); otherwise keep the fast early-release window.
  val dest_overlaps_live_operands = req.dst_overlaps_operands
  dontTouch(ej_high)
  dontTouch(terminal_needs_drain)
  dontTouch(dest_overlaps_live_operands)
  val ex_ahead = WireInit(io.ex_completed ||
    ((req.act =/= Activation.LAYERNORM) && (req.act =/= Activation.SOFTMAX) &&
      !dest_overlaps_live_operands &&
      (io.ex_k === req.max_k - 1.U &&
        (io.ex_j > ej_high ||
          ((io.ex_j === ej_high) && ex_i_compressed > i && !terminal_needs_drain)))))
  when(req.is_resadd){
    ex_ahead := io.ex_completed || (ex_i_compressed > i || (ex_i_compressed === i && ex_j_compressed >= j + blocks))
  }
  dontTouch(ex_ahead)

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ex_ahead
  io.cmd.bits := mvout_cmd
  io.loop_id := req.loop_id

  when (io.cmd.fire && state === st) {
    when (!req.mx_multi_elem && chunk_id < (MxStoreIter.chunks(geom, j, req.max_j) - 1.U)) {
      chunk_id := chunk_id + 1.U
    }.otherwise {
      chunk_id := 0.U
      val next_i = floorAdd(i, 1.U, iter_max_i)
      // DIM<16 widen: step j by W (j+1..j+W-1 fold onto the same physical row). Both quad and single.
      val next_j = floorAdd(j, Mux(fold, geom.W.U, max_blocks), iter_max_j, next_i === 0.U)

      i := next_i
      j := next_j

      when(next_i === 0.U && next_j === 0.U) {
        state := idle
      }
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := st
    j := 0.U
    i := 0.U
    chunk_id := 0.U
  }
}

// Combined loop
// Last A-scale slice loaded into a loop slot's half (R3c reuse check).
class LoopMatmulAScaleSlice(val coreMaxAddrBits: Int, val iterator_bitwidth: Int) extends Bundle {
  val addr = UInt(coreMaxAddrBits.W)
  val stride = UInt(coreMaxAddrBits.W)
  val max_i = UInt(iterator_bitwidth.W)
  val max_k = UInt(iterator_bitwidth.W)
}

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

  val c_spad_addr = UInt(max_acc_addr.W)
  val inc_acc_addr = Bool()
  val spad_only = Bool()
  val reuse_tiled = Bool()  // LOOP_WS rs2 bit 10: gated tiled requant->spad store (FP8)

  val a_transpose = Bool()
  val b_transpose = Bool()

  val act = UInt(Activation.bitwidth.W)

  val low_d = Bool()
  val full_c = Bool()
  val ex_accumulate = Bool()

  val a_ex_spad_id = UInt(2.W)
  val b_ex_spad_id = UInt(2.W)
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
  val ex_skipped = Bool()   // LOOP_WS rs2[6]: store-only loop (no EX of its own to order its stores behind)
  val narrow_type = Bool()
  val narrow_act = Bool()
  val lut_kb = Bool()   // LOOP_WS rs2[12]: weight codebooks hold one set per K-block of the loop
  val kmix = Bool()     // LOOP_WS_CONFIG_KMIX seen: mixed-K loop (per-K-block low precision / MXFP8 by kmask)
  val kmask = UInt(128.W)
  val b_alt_addr = UInt(coreMaxAddrBits.W)   // MXFP8 B of a mixed-K loop
  val b_alt_stride = UInt(coreMaxAddrBits.W)
  val nlut = UInt(8.W)  // its low-precision K-blocks (codebook sets loaded)
  val shared_acc = Bool()   // LOOP_WS rs2[11]: single x single on the shared acc layout (DIM>=16, managed)
  val mx = new MxCmdMode   // the loop's mode; every command it generates carries it

  def all_completed(dummy: Int=0): Bool = lda_completed && ldb_completed && ldd_completed && ex_completed && st_completed

  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr+1).W)
  val resadd_addr_start = UInt(log2Up(max_acc_addr).W)

  // Loop-integrated MX scales (LOOP_WS_CONFIG_SCALES/_STRIDES); a_scale_addr == 0 -> legacy (not managed).
  val a_scale_addr = UInt(coreMaxAddrBits.W)
  val b_scale_addr = UInt(coreMaxAddrBits.W)
  val a_scale_stride = UInt(coreMaxAddrBits.W)
  val b_scale_stride = UInt(coreMaxAddrBits.W)
  val lds_started = Bool()
  val lds_done = Bool()
  val lds_a_reuse = Bool()
  val a_lut_addr = UInt(coreMaxAddrBits.W)   // LOOP_WS_CONFIG_LUTS act-in codebooks (0 = not loop-managed)
  val b_lut_addr = UInt(coreMaxAddrBits.W)   // LOOP_WS_CONFIG_LUTS weight codebooks (0 = not loop-managed)
  val ldl_a_issued = Bool()                  // its act-in codebook load was issued
  val ldl_done = Bool()                      // its codebook loads were issued (or there are none)
  val reads_act0 = Bool()   // its scale config (the last CONFIG_SCALE_MEM ahead of its LOOP_WS) may read act half 0

  def reset(): Unit = {
    configured := false.B
    a_scale_addr := 0.U
    b_scale_addr := 0.U
    lds_started := false.B
    lds_done := false.B
    lds_a_reuse := false.B
    a_lut_addr := 0.U
    b_lut_addr := 0.U
    ldl_a_issued := false.B
    ldl_done := false.B

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
    ex_skipped := false.B

    spad_only := false.B
    reuse_tiled := false.B
    narrow_type := false.B
    narrow_act := false.B
    shared_acc := false.B
    lut_kb := false.B
    kmix := false.B
    kmask := 0.U
    //is_resadd := false.B
  }
}

class LoopMatmul(block_size: Int, coreMaxAddrBits: Int, reservation_station_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
                 max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
                 mvin_rs2_t: MvinRs2, preload_rs1_t: PreloadRs, preload_rs2_t: PreloadRs,
                 compute_rs1_t: ComputeRs, compute_rs2_t: ComputeRs, mvout_spad_rs1_t: MvoutSpadRs1, mvout_rs2_t: MvoutRs2,
                 vec_bypass: Boolean = false, ld_bypass: Boolean = false, use_mx_scaling: Boolean = true, lut_quad: Boolean = false)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val tilesPerMxBlock = MxGeom(block_size).T
  val max_block_len = (dma_max_bytes / (block_size * input_w / 8)) max 1
  val max_block_len_acc = (dma_max_bytes / (block_size * acc_w / 8)) max 1
  val max_block_size = 2 * block_size  // 32, for sizing pad fields
  val max_block_len_ij = (dma_max_bytes / (2 * block_size * input_w / 8)) max 1
  val max_block_len_acc_nonfp8 = (dma_max_bytes / (2 * block_size * acc_w / 8)) max 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(reservation_station_size)))
    val out = Decoupled(new GemminiCmd(reservation_station_size))
    val ld_completed = Input(UInt(log2Up(reservation_station_size+1).W))
    val st_completed = Input(UInt(log2Up(reservation_station_size+1).W))
    val ex_completed = Input(UInt(log2Up(reservation_station_size+1).W))
    val busy = Output(Bool())
    val completed = Output(Vec(2, Bool()))
    val counter_i = Output(UInt(16.W))
    val counter_j = Output(UInt(16.W))
    val counter_k = Output(UInt(16.W))
    val i = Output(UInt(16.W))
    val j = Output(UInt(16.W))
    val k = Output(UInt(16.W))
    val activation_mx_format = Input(UInt(2.W))
    val weight_mx_format = Input(UInt(2.W))
    val output_mx_format = Input(UInt(2.W))
    val mx_multi_elem = Input(Bool())   // throughput: 2 elements/lane (datatype-independent)
    val mx_multi_elem_act = Input(Bool())   // ACTIVATION (output-row) throughput
    val vec_space = Input(Bool())   // RS vector queue can take one more VPU_EXEC
    val ld_space = Input(Bool())    // RS load queue can take one more mvin / config_ld
    val lut_granularity = Input(UInt(16.W))   // live LUT update granularity G (codebooks per 2^G rows)
  })

  // Create states
  

  val concurrent_loops = 2
  val loops = Reg(Vec(concurrent_loops, new LoopMatmulState(iterator_bitwidth, coreMaxAddrBits, max_addr, max_acc_addr)))
  val head_loop_id = Reg(UInt(log2Up(concurrent_loops).W))
  val tail_loop_id = (~head_loop_id).asUInt // This is the loop that we always try to configure if available
  val head_loop = loops(head_loop_id)
  val tail_loop = loops(tail_loop_id)

  val loop_configured = loops.map(_.configured).reduce(_ || _)

  val loop_being_configured_id = Mux(head_loop.configured, tail_loop_id, head_loop_id)
  val loop_being_configured = loops(loop_being_configured_id)

  val is_resadd = RegInit(false.B)

  val max_all_addr = if(max_addr > max_acc_addr) max_addr else max_acc_addr 
  // Create inner modules
  val ldA = Module(new LoopMatmulLdA(block_size, coreMaxAddrBits, iterator_bitwidth, max_all_addr, input_w, max_block_len, concurrent_loops, mvin_rs2_t))
  val ldB = Module(new LoopMatmulLdB(block_size, coreMaxAddrBits, iterator_bitwidth, max_all_addr, input_w, max_block_len, concurrent_loops, mvin_rs2_t))
  val ldD = Module(new LoopMatmulLdD(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, max_block_len, max_block_len_acc, concurrent_loops, mvin_rs2_t))
  val ex = Module(new LoopMatmulExecute(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops, preload_rs1_t, preload_rs2_t, compute_rs1_t, compute_rs2_t))
  // max_block_len 1 (as stC_spad): an MX mvout covers one 4-tile j-group; wider would skip groups.
  val stC = Module(new LoopMatmulStC(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, 1, concurrent_loops, tilesPerMxBlock, mvout_rs2_t,
    use_mx_scaling))
  val stC_spad = Module(new LoopMatmulStCSpad(block_size, iterator_bitwidth, max_addr, max_acc_addr, input_w, acc_w, 1, concurrent_loops, tilesPerMxBlock, mvout_spad_rs1_t, mvout_rs2_t))

  // Create command queue
  val cmd = Queue(io.in)
  io.counter_i := ex.io.i
  io.counter_j := ex.io.j
  io.counter_k := ex.io.k
  io.i := loop_being_configured.max_i
  io.j := loop_being_configured.max_j
  io.k := loop_being_configured.max_k
  io.busy := cmd.valid || loop_configured

  io.completed := 0.U.asTypeOf(io.completed.cloneType)

  // Create ld arbiters
  val ldab_arb = Module(new WeightedArbiter(new RoCCCommand(), maxWeightA=255, staticWeightAEnabled=true)) // TODO magic numbers
  ldab_arb.io.inA <> ldA.io.cmd
  ldab_arb.io.inB <> ldB.io.cmd
  val ab_loads_on_same_loop = ldA.io.loop_id === ldB.io.loop_id
  // Force only a loader with head-loop work left: an idle loader keeps the loop_id of the last loop it served (e.g. a
  // head matmul that skips its A/B loads), and forcing it would starve the other loader's next-loop loads until the
  // head loop completes.
  val forceA = !ab_loads_on_same_loop && ldA.io.loop_id === head_loop_id && !ldA.io.idle
  val forceB = !ab_loads_on_same_loop && ldB.io.loop_id === head_loop_id && !ldB.io.idle
  ldab_arb.io.forceA := Mux(is_resadd, ab_loads_on_same_loop && !ldA.io.idle, forceA)
  ldab_arb.io.forceB := Mux(is_resadd, forceB || ldA.io.idle, forceB) 
  ldab_arb.io.weightA := 0.U
  ldab_arb.io.inA_idle := ldA.io.idle
  ldab_arb.io.inB_idle := ldB.io.idle
  ldab_arb.io.inA_k := ldA.io.k
  ldab_arb.io.inA_i := ldA.io.i
  ldab_arb.io.inB_k := ldB.io.k
  ldab_arb.io.inB_j := ldB.io.j

  // Create global arbiter
  val lds_cmd = Wire(Decoupled(new RoCCCommand))
  val ldl_cmd = Wire(Decoupled(new RoCCCommand))
  val arb = Module(new Arbiter(new RoCCCommand(), 7))
  arb.io.in(0) <> lds_cmd    // scale loads first: they run ahead of the loop like operand loads
  arb.io.in(1) <> ldl_cmd    // then codebook loads, likewise
  arb.io.in(2) <> stC.io.cmd
  arb.io.in(3) <> ex.io.cmd
  arb.io.in(4) <> ldD.io.cmd
  arb.io.in(5) <> ldab_arb.io.out
  arb.io.in(6) <> stC_spad.io.cmd
  val unrolled_cmd = arb.io.out

  // Create reservation station utilization counters
  val ld_utilization = RegInit(0.U(log2Up(max_lds+1).W))
  val st_utilization = RegInit(0.U(log2Up(max_sts+1).W))
  val ex_utilization = RegInit(0.U(log2Up(max_exs+1).W))

  ld_utilization := ld_utilization +& (ldA.io.cmd.fire || ldB.io.cmd.fire || ldD.io.cmd.fire) -& io.ld_completed
  st_utilization := st_utilization +& (stC.io.cmd.fire || stC_spad.io.cmd.fire) -& io.st_completed
  ex_utilization := ex_utilization +& ex.io.cmd.fire -& io.ex_completed

  assert(ld_utilization >= io.ld_completed, "ld utilization underflow")
  assert(st_utilization >= io.st_completed, "st utilization underflow")
  assert(ex_utilization >= io.ex_completed, "ex utilization underflow")

  // Wire up unrolled command output
  val is_loop_run_cmd = cmd.bits.cmd.inst.funct === LOOP_WS
  val is_loop_config_cmd = (cmd.bits.cmd.inst.funct >= LOOP_WS_CONFIG_BOUNDS && cmd.bits.cmd.inst.funct <= LOOP_WS_CONFIG_STRIDES_DC) ||
    (cmd.bits.cmd.inst.funct >= LOOP_WS_CONFIG_SPAD_AB && cmd.bits.cmd.inst.funct <= LOOP_WS_CONFIG_SPAD_C) ||
    cmd.bits.cmd.inst.funct === LOOP_WS_CONFIG_SCALES || cmd.bits.cmd.inst.funct === LOOP_WS_CONFIG_SCALE_STRIDES ||
    cmd.bits.cmd.inst.funct === LOOP_WS_CONFIG_LUTS || cmd.bits.cmd.inst.funct === LOOP_WS_CONFIG_KMIX ||
    cmd.bits.cmd.inst.funct === LOOP_WS_CONFIG_KMASK
  val is_loop_cmd = is_loop_run_cmd || is_loop_config_cmd

  io.out.bits.cmd := Mux(loop_configured, unrolled_cmd.bits, cmd.bits.cmd)
  io.out.bits.cmd.status := cmd.bits.cmd.status // TODO This is not guaranteed to be the correct fix! We must fix this
  io.out.bits.rob_id := DontCare
  io.out.bits.from_matmul_fsm := Mux(loop_configured, true.B, cmd.bits.from_matmul_fsm)
  io.out.bits.from_conv_fsm := Mux(loop_configured, false.B, cmd.bits.from_conv_fsm)
  val gen_mx = Wire(new MxCmdMode)
  io.out.bits.mx := Mux(loop_configured, gen_mx, cmd.bits.mx)
  io.out.valid := Mux(loop_configured, unrolled_cmd.valid, cmd.valid && !is_loop_config_cmd && !is_loop_run_cmd)

  cmd.ready := Mux(is_loop_cmd, !loop_being_configured.configured, !loop_configured && io.out.ready)
  arb.io.out.ready := io.out.ready

  // Wire up overloaded signals
  ldA.io.rob_overloaded := ld_utilization >= max_lds.U
  ldB.io.rob_overloaded := ld_utilization >= max_lds.U
  ex.io.rob_overloaded := ex_utilization >= max_exs.U
  ldD.io.rob_overloaded := ld_utilization >= max_lds.U
  stC.io.rob_overloaded := st_utilization >= max_sts.U
  stC_spad.io.rob_overloaded := st_utilization >= max_sts.U

  // Wire up iterator inputs
  ex.io.lda_completed := (ldA.io.loop_id =/= ex.io.loop_id) || ldA.io.idle
  ex.io.ldb_completed := (ldB.io.loop_id =/= ex.io.loop_id) || ldB.io.idle
  ex.io.ldd_completed := (ldD.io.loop_id =/= ex.io.loop_id) || ldD.io.idle
  ex.io.ld_ka := ldA.io.k
  ex.io.ld_kb := ldB.io.k
  ex.io.ld_j := ldB.io.j
  ex.io.ld_i := ldA.io.i

  // act-quad x wei-single: stores see col-tile pairs as quad tiles. ex_j = 2q+1: pair q done for rows < ex_i;
  // ex_j = 2q: pairs < q done, pair q not started (-> pair q, ex_i 0)
  val st_run = loops(stC.io.loop_id)
  // per store unit, from its own loop
  def st_ex_ji(l: LoopMatmulState): (UInt, UInt) = {
    val pair = l.narrow_act && !l.narrow_type
    // single x quad: stores see row-tile pairs as quad tiles (pair q done once ex_i > 2q+1)
    val ipair = (block_size >= 16).B && l.narrow_type && !l.narrow_act
    // single x single, shared layout: both pairings (2x2 tiles per quad tile)
    val sh = l.shared_acc
    (Mux(pair || sh, ex.io.j >> 1, ex.io.j),
     Mux((pair || sh) && !ex.io.j(0), 0.U, Mux(ipair || sh, ex.io.i >> 1, ex.io.i)))
  }
  val (st_ex_j, st_ex_i) = st_ex_ji(st_run)
  val (sts_ex_j, sts_ex_i) = st_ex_ji(loops(stC_spad.io.loop_id))

  stC.io.ex_completed := (ex.io.loop_id =/= stC.io.loop_id) || ex.io.idle
  stC.io.ex_k := ex.io.k
  stC.io.ex_j := st_ex_j
  stC.io.ex_i := st_ex_i

  stC_spad.io.ex_completed := (ex.io.loop_id =/= stC_spad.io.loop_id) || ex.io.idle
  stC_spad.io.ex_k := ex.io.k
  stC_spad.io.ex_j := sts_ex_j
  stC_spad.io.ex_i := sts_ex_i
  // mixed-K loop: tile progress mixes quad / MXFP8 units, so its stores wait for all of its ex commands
  when (st_run.kmix) { stC.io.ex_k := 0.U; stC.io.ex_j := 0.U; stC.io.ex_i := 0.U }
  when (loops(stC_spad.io.loop_id).kmix) { stC_spad.io.ex_k := 0.U; stC_spad.io.ex_j := 0.U; stC_spad.io.ex_i := 0.U }

  // when loop matmul is used as resadd unroller
  // skip ex
  // track ldB instead of ex
  when(is_resadd){
    stC.io.ex_completed := (ldA.io.loop_id =/= stC.io.loop_id || ldA.io.idle) && (ldB.io.loop_id =/= stC.io.loop_id || ldB.io.idle)
    stC.io.ex_k := 0.U // req.max_k shall be 1
    stC.io.ex_j := ldB.io.j
    stC.io.ex_i := ldB.io.k
    //ldB.io.rob_overloaded := ld_utilization >= max_lds.U || !((ldA.io.loop_id =/= ldB.io.loop_id) || ldA.io.idle)
    // TODO(richard): does store to spad also support resadd?
  }

  val loops_configured = RegInit(0.U(16.W))
  dontTouch(loops_configured)

  // act half of the last scale config seen (same test as the RS): a resident SPAD_REQUANT rewrites act half 0
  val cfg_reads_act0 = RegInit(true.B)
  when (cmd.fire && cmd.bits.cmd.inst.funct === CONFIG_SCALE_MEM) {
    cfg_reads_act0 := !(cmd.bits.cmd.rs2(17) && cmd.bits.cmd.rs1(60) && !cmd.bits.cmd.rs1(63))
  }

  // Create config registers
  when(cmd.valid && is_loop_cmd && !loop_being_configured.configured) {

    switch (cmd.bits.cmd.inst.funct) {
      is (LOOP_WS_CONFIG_BOUNDS) {

        loop_being_configured.max_k := cmd.bits.cmd.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        loop_being_configured.max_j := cmd.bits.cmd.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        loop_being_configured.max_i := cmd.bits.cmd.rs2(iterator_bitwidth-1, 0)

        loop_being_configured.pad_k := cmd.bits.cmd.rs1(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        loop_being_configured.pad_j := cmd.bits.cmd.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        loop_being_configured.pad_i := cmd.bits.cmd.rs1(iterator_bitwidth-1, 0)
      }

      is (LOOP_WS_CONFIG_ADDRS_AB) {
        loop_being_configured.spad_only := false.B
        loop_being_configured.a_dram_addr := cmd.bits.cmd.rs1
        loop_being_configured.b_dram_addr := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_ADDRS_DC) {
        loop_being_configured.spad_only := false.B
        loop_being_configured.d_dram_addr := cmd.bits.cmd.rs1
        loop_being_configured.c_dram_addr := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_AB) {
        loop_being_configured.a_dram_stride := cmd.bits.cmd.rs1
        loop_being_configured.b_dram_stride := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_DC) {
        loop_being_configured.d_dram_stride := cmd.bits.cmd.rs1
        loop_being_configured.c_dram_stride := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_SPAD_AB) {
        loop_being_configured.a_addr_start := cmd.bits.cmd.rs1
        loop_being_configured.b_addr_end := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_SCALES) {
        loop_being_configured.a_scale_addr := cmd.bits.cmd.rs1
        loop_being_configured.b_scale_addr := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_SCALE_STRIDES) {
        loop_being_configured.a_scale_stride := cmd.bits.cmd.rs1
        loop_being_configured.b_scale_stride := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_KMIX) {
        loop_being_configured.kmix := true.B
        loop_being_configured.b_alt_addr := cmd.bits.cmd.rs1
        loop_being_configured.b_alt_stride := cmd.bits.cmd.rs2
      }

      is (LOOP_WS_CONFIG_KMASK) {
        loop_being_configured.kmask := Cat(cmd.bits.cmd.rs2, cmd.bits.cmd.rs1)
      }

      is (LOOP_WS_CONFIG_LUTS) {
        loop_being_configured.a_lut_addr := cmd.bits.cmd.rs1
        loop_being_configured.b_lut_addr := cmd.bits.cmd.rs2
      }

      is (LOOP_WS) {

        loop_being_configured.ex_accumulate := cmd.bits.cmd.rs1(0)
        loop_being_configured.full_c := cmd.bits.cmd.rs1(1)
        loop_being_configured.low_d := cmd.bits.cmd.rs1(2)
        loop_being_configured.act := cmd.bits.cmd.rs1(8+Activation.bitwidth-1, 8) // TODO magic numbers
        // the LOOP_WS command's own mode tag; loop-managed codebooks => LUT operand, always 2/lane
        val bl = loop_being_configured.b_lut_addr =/= 0.U
        val al = loop_being_configured.a_lut_addr =/= 0.U
        val tw = MxCmdMode.withLut(cmd.bits.mx.wgt, lut_quad.B && bl)
        val ta = MxCmdMode.withLut(cmd.bits.mx.act, lut_quad.B && al)
        loop_being_configured.mx.wgt := tw
        loop_being_configured.mx.act := ta
        val nw = MxCmdMode.quad(tw) || bl
        val na = MxCmdMode.quad(ta) || al
        loop_being_configured.narrow_type := nw
        loop_being_configured.narrow_act := na
        loop_being_configured.lut_kb := cmd.bits.cmd.rs2(12)
        loop_being_configured.nlut := PopCount(loop_being_configured.kmask)
        val sh = (use_mx_scaling && block_size >= 16).B && cmd.bits.cmd.rs2(11) && !nw && !na
        loop_being_configured.shared_acc := sh
        loop_being_configured.mx.shared := sh
        loop_being_configured.mx.slot := 0.U
        loop_being_configured.lda_started := cmd.bits.cmd.rs2(3)
        loop_being_configured.ldb_started := cmd.bits.cmd.rs2(4)
        loop_being_configured.ldd_started := cmd.bits.cmd.rs2(5)
        loop_being_configured.ex_started := cmd.bits.cmd.rs2(6)
        loop_being_configured.st_started := cmd.bits.cmd.rs2(7)
        loop_being_configured.lda_completed := cmd.bits.cmd.rs2(3)
        loop_being_configured.ldb_completed := cmd.bits.cmd.rs2(4)
        loop_being_configured.ldd_completed := cmd.bits.cmd.rs2(5)
        loop_being_configured.ex_completed := cmd.bits.cmd.rs2(6)
        loop_being_configured.st_completed := cmd.bits.cmd.rs2(7)
        loop_being_configured.ex_skipped := cmd.bits.cmd.rs2(6)
        loop_being_configured.inc_acc_addr := cmd.bits.cmd.rs2(8)
        loop_being_configured.spad_only := cmd.bits.cmd.rs2(9)
        loop_being_configured.reuse_tiled := cmd.bits.cmd.rs2(10) // LOOP_WS_REQUANT_TILED (gated tiled requant->spad)

        loop_being_configured.c_spad_addr := cmd.bits.cmd.rs2(63, 32)

        loop_being_configured.a_ex_spad_id := cmd.bits.cmd.rs1(19, 18)
        loop_being_configured.b_ex_spad_id := cmd.bits.cmd.rs1(17, 16) 
        loop_being_configured.a_transpose := cmd.bits.cmd.rs2(0)
        loop_being_configured.b_transpose := cmd.bits.cmd.rs2(1)
        is_resadd := cmd.bits.cmd.rs2(2)

        loop_being_configured.configured := true.B
        loop_being_configured.reads_act0 := cfg_reads_act0

        loops_configured := loops_configured + 1.U
      }
    }
  }

  // Wire up request signals
  val ld_d_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val ex_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val st_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))

  // Cross-loop WAR guard. With explicit spad ids both concurrent loops can own the same rows, and the newer
  // loop's mvins would enter the RS ahead of the older loop's remaining computes (RS then sees RAW, not WAR).
  // Hold a newer loop's A/B load while its rows overlap the older (head) loop's A or B rows and that loop has
  // not issued all its ex commands; after that the RS orders each mvin behind the reads it overlaps.
  val half_rows = (max_addr / concurrent_loops).U
  def a_rows(l: LoopMatmulState): (UInt, UInt) = {
    val s = Mux(l.spad_only || l.a_ex_spad_id === 0.U, l.a_addr_start, (l.a_ex_spad_id - 1.U) * half_rows)
    (s, s +& l.max_i * l.max_k * block_size.U)
  }
  def b_rows(l: LoopMatmulState): (UInt, UInt) = {
    val e = Mux(l.spad_only || l.b_ex_spad_id === 0.U, l.b_addr_end, l.b_ex_spad_id * half_rows)
    val n = l.max_k * (l.max_j << l.kmix) * block_size.U
    (Mux(e > n, e - n, 0.U), e)
  }
  def rows_overlap(x: (UInt, UInt), y: (UInt, UInt)): Bool = x._1 < y._2 && y._1 < x._2
  def ld_blocked(id: UInt, rows: (UInt, UInt)): Bool = !is_resadd && id =/= head_loop_id && head_loop.configured &&
    !head_loop.ex_completed && (rows_overlap(rows, a_rows(head_loop)) || rows_overlap(rows, b_rows(head_loop)))

  // VPU_EXEC passes configured loops whose A, B and C spad rows it misses, so vector work overlaps the loop's
  // unrolling (raw commands stay in program order). SPAD_REQUANT does too, but only while the output is BF16: then
  // the loop's stores are BF16-marked and do not wait for an older SR (they share the requantizer beat by beat);
  // a quantized store would, so with a quantized output SR stays in order.
  // ld_bypass (MX builds): the config_ld / mvin and next-matmul scale-config passes below do not need a VPU, so a
  // VPU-less build can still start the next matmul's loads under the previous loop's stores; vec_pass needs vec_bypass
  if (vec_bypass || ld_bypass) {
    val f = cmd.bits.cmd.inst.funct
    val r1 = cmd.bits.cmd.rs1
    val r2 = cmd.bits.cmd.rs2
    val is_vpu = f === VPU_EXEC
    val vc = gemmini.vpu.VpuCmd.decode(r1, r2, log2Up(max_addr))
    def span(base: UInt, n: UInt): (UInt, UInt) = (base, base +& n)
    val is_sr = f === SPAD_REQUANT && io.output_mx_format === 3.U
    // exact footprints, as the RS computes them: BF16 source M*N/8 rows, E4M3 destination M*N/16 (M padded to 16 if tiled)
    val sc = SpadRequantCmd.decode(r1, r2, log2Up(max_addr))
    val sr_src_rows = SpadRequantRows.src(sc, block_size)   // spad rows are block_size (DIM) bytes
    val sr_dst_rows = SpadRequantRows.dst(sc, block_size)
    val ranges = Seq(
      (is_vpu, span(vc.src1, vc.rows)),
      (is_vpu && gemmini.vpu.VpuOp.usesSrc2(vc.op), span(vc.src2, gemmini.vpu.VpuCmd.src2Rows(vc))),
      (is_vpu, span(vc.dst, gemmini.vpu.VpuCmd.dstRows(vc))),
      (is_vpu && gemmini.vpu.VpuOp.hasDst2(vc.op), span(vc.dst2, gemmini.vpu.VpuCmd.dst2Rows(vc))),
      (is_sr, span(sc.src, sr_src_rows)),
      (is_sr, span(sc.dst, sr_dst_rows)))
    // BF16 C: a tile is 2*DIM rows, x2 per quad operand (2*DIM output rows / cols)
    def c_rows(l: LoopMatmulState): (UInt, UInt) = span(l.c_spad_addr(log2Up(max_addr) - 1, 0),
      ((2.U * l.max_i * l.max_j * block_size.U) << l.narrow_type) << l.narrow_act)
    // a resident SR also waits for a loop reading act half 0 to issue all its computes (the RS then orders it after them)
    def hits(l: LoopMatmulState): Bool = l.configured && (ranges.map { case (v, r) =>
      v && (rows_overlap(r, a_rows(l)) || rows_overlap(r, b_rows(l)) || (l.spad_only && rows_overlap(r, c_rows(l))))
    }.reduce(_ || _) || (is_sr && sc.resident && l.reads_act0 && !l.ex_completed))
    val vec_pass = vec_bypass.B && cmd.valid && loop_configured && (is_vpu || is_sr) && io.vec_space && !loops.map(hits).reduce(_ || _)
    // The next matmul's scale config and gated scale load may pass loops that have issued every load and ex command
    // (only stores left) when the output is BF16 and every such loop moves the accumulator base by half: the next
    // loop then accumulates into the other half while these stores drain. The RS orders the config after the loops'
    // ex commands; a gated load waits for a free scale half on its own.
    val acc_alternates = (l: LoopMatmulState) => Mux(l.spad_only, l.inc_acc_addr, l.c_dram_addr =/= 0.U)
    val only_stores_left = loops.map(l => !l.configured || (l.ex_completed && l.lda_completed && l.ldb_completed &&
      l.ldd_completed && acc_alternates(l))).reduce(_ && _)
    val is_scale_cmd = f === CONFIG_SCALE_MEM || (f === MX_LOAD_SCALES && r2(54))
    // a CONFIG_SCALE_MEM also needs a free RS ex slot, or it would itself wait at the head of the unrolled queue
    val cfg_pass = cmd.valid && loop_configured && is_scale_cmd && only_stores_left && io.output_mx_format === 3.U &&
      (f =/= CONFIG_SCALE_MEM || ex_utilization < max_exs.U)
    // A raw mvin (or the config_ld that sets its block stride) may pass loops that have issued all their own loads
    // when its destination rows miss every configured loop's A, B and C rows: the next block's K/V loads then enter
    // while the previous loop's stores drain. The RS orders it against the loop's queued commands by row range.
    val is_cfg_ld = f === CONFIG_CMD && r1(1, 0) === CONFIG_LOAD
    val ld_stride = RegInit(0.U(16.W))   // block stride of load state 0 (as the RS computes the mvin's rows)
    when (cmd.fire && is_cfg_ld && r1(4, 3) === 0.U) { ld_stride := r1(31, 16) }
    val mv = r2.asTypeOf(mvin_rs2_t)
    val mv_mats = mv.num_cols / block_size.U + (mv.num_cols % block_size.U =/= 0.U)
    val mv_dst = mv.local_addr.asUInt(log2Up(max_addr) - 1, 0)
    val mv_range = span(mv_dst, (mv_mats - 1.U) * ld_stride +& mv.num_rows)
    val loads_done = loops.map(l => !l.configured || (l.lda_completed && l.ldb_completed && l.ldd_completed)).reduce(_ && _)
    def mv_hits(l: LoopMatmulState): Bool = l.configured && (rows_overlap(mv_range, a_rows(l)) ||
      rows_overlap(mv_range, b_rows(l)) || (l.spad_only && rows_overlap(mv_range, c_rows(l))))
    val is_spad_mvin = f === LOAD_CMD && !mv.local_addr.is_acc_addr && !mv.local_addr.is_garbage()
    val ld_pass = cmd.valid && loop_configured && io.ld_space && loads_done &&
      (is_cfg_ld || (is_spad_mvin && !loops.map(mv_hits).reduce(_ || _)))
    // config_ex / config_st may pass loops that have issued all their ex / store commands: the RS queue keeps it after
    // them and before the next loop's (ex applies a datatype-only config_ex without a drain); it takes an RS slot
    val is_cfg_ex = f === CONFIG_CMD && r1(1, 0) === CONFIG_EX
    val is_cfg_st = f === CONFIG_CMD && r1(1, 0) === CONFIG_STORE
    val exs_issued = loops.map(l => !l.configured || l.ex_completed).reduce(_ && _)
    val sts_issued = loops.map(l => !l.configured || l.st_completed).reduce(_ && _)
    val cfgx_pass = cmd.valid && loop_configured &&
      ((is_cfg_ex && exs_issued && ex_utilization < max_exs.U) || (is_cfg_st && sts_issued && st_utilization < max_sts.U))
    when (vec_pass || cfg_pass || ld_pass || cfgx_pass) {
      io.out.valid := true.B
      io.out.bits.cmd := cmd.bits.cmd
      io.out.bits.from_matmul_fsm := cmd.bits.from_matmul_fsm
      io.out.bits.from_conv_fsm := cmd.bits.from_conv_fsm
      io.out.bits.mx := cmd.bits.mx
      cmd.ready := io.out.ready
      arb.io.out.ready := false.B
    }
    // A passed CONFIG_SCALE_MEM takes an RS ex slot: count it in ex_utilization (and report its completion as a
    // matmul one) so the ex unroller never sends a command the full ex queue cannot take, which would sit at the head
    // of the unrolled queue and block the loops' stores behind it.
    val cfg_ex_pass = cfg_pass && f === CONFIG_SCALE_MEM
    when (cfg_ex_pass) { io.out.bits.from_matmul_fsm := true.B }
    when (cfg_ex_pass && io.out.fire) { ex_utilization := ex_utilization +& 1.U -& io.ex_completed }
    when (cfgx_pass) { io.out.bits.from_matmul_fsm := true.B }
    when (cfgx_pass && is_cfg_ex && io.out.fire) { ex_utilization := ex_utilization +& 1.U -& io.ex_completed }
    when (cfgx_pass && is_cfg_st && io.out.fire) { st_utilization := st_utilization +& 1.U -& io.st_completed }
  }

  val loop_requesting_ldA_id = Mux(head_loop.lda_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldA = loops(loop_requesting_ldA_id)
  ldA.io.req.bits.max_k := Mux(is_resadd, loop_requesting_ldA.max_j, loop_requesting_ldA.max_k)
  ldA.io.req.bits.max_i := loop_requesting_ldA.max_i
  ldA.io.req.bits.pad_k := Mux(is_resadd, loop_requesting_ldA.pad_j, loop_requesting_ldA.pad_k)
  ldA.io.req.bits.pad_i := loop_requesting_ldA.pad_i
  ldA.io.req.bits.dram_addr := loop_requesting_ldA.a_dram_addr
  ldA.io.req.bits.dram_stride := loop_requesting_ldA.a_dram_stride
  ldA.io.req.bits.transpose := loop_requesting_ldA.a_transpose
  ldA.io.req.bits.addr_start := Mux(loop_requesting_ldA.a_ex_spad_id === 0.U, loop_requesting_ldA.a_addr_start, (loop_requesting_ldA.a_ex_spad_id - 1.U) * (max_addr / concurrent_loops).U)
  ldA.io.req.bits.loop_id := loop_requesting_ldA_id
  ldA.io.req.bits.is_resadd := is_resadd

  ldA.io.req.valid := !loop_requesting_ldA.lda_started && loop_requesting_ldA.configured &&
    !(loop_requesting_ldA.a_dram_addr =/= 0.U && ld_blocked(loop_requesting_ldA_id, a_rows(loop_requesting_ldA)))

  when (ldA.io.req.fire) {
    loop_requesting_ldA.running := true.B
    loop_requesting_ldA.lda_started := true.B
  }

  val loop_requesting_ldB_id = Mux(head_loop.ldb_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldB = loops(loop_requesting_ldB_id)
  ldB.io.req.bits.max_j := loop_requesting_ldB.max_j
  ldB.io.req.bits.max_k := Mux(is_resadd, loop_requesting_ldB.max_i, loop_requesting_ldB.max_k)
  ldB.io.req.bits.pad_j := loop_requesting_ldB.pad_j
  ldB.io.req.bits.pad_k := Mux(is_resadd, loop_requesting_ldB.pad_i, loop_requesting_ldB.pad_k)
  ldB.io.req.bits.dram_addr := loop_requesting_ldB.b_dram_addr
  ldB.io.req.bits.dram_stride := loop_requesting_ldB.b_dram_stride
  ldB.io.req.bits.transpose := loop_requesting_ldB.b_transpose
  ldB.io.req.bits.addr_end := Mux(loop_requesting_ldB.b_ex_spad_id === 0.U, loop_requesting_ldB.b_addr_end, (loop_requesting_ldB.b_ex_spad_id) * (max_addr / concurrent_loops).U)
  ldB.io.req.bits.loop_id := loop_requesting_ldB_id
  ldB.io.req.bits.is_resadd := is_resadd
  ldB.io.req.bits.kmix := loop_requesting_ldB.kmix
  ldB.io.req.bits.kmask := loop_requesting_ldB.kmask
  ldB.io.req.bits.alt_addr := loop_requesting_ldB.b_alt_addr
  ldB.io.req.bits.alt_stride := loop_requesting_ldB.b_alt_stride

  ldB.io.req.valid := !loop_requesting_ldB.ldb_started && loop_requesting_ldB.configured &&
    !(loop_requesting_ldB.b_dram_addr =/= 0.U && ld_blocked(loop_requesting_ldB_id, b_rows(loop_requesting_ldB)))

  when (ldB.io.req.fire) {
    loop_requesting_ldB.running := true.B
    loop_requesting_ldB.ldb_started := true.B
  }

  // LdS: per loop, as soon as it is configured, issue its A and B scale slices as 2-D MX_LOAD_SCALES into scale
  // half = loop slot (rows = E8M0 k-blocks = max_k / tilesPerMxBlock; row = max_i / max_j tiles of DIM bytes).
  // They go straight to the scale loader (not the RS), so they are outside the ld/ex/st utilization counts;
  // the loop's CONFIG_SCALE_MEM waits for them in the execute controller.
  val loop_requesting_lds_id = Mux(head_loop.lds_started, tail_loop_id, head_loop_id)
  val loop_requesting_lds = loops(loop_requesting_lds_id)
  val lds_state = RegInit(0.U(2.W))   // 0 idle, 1 issue A, 2 issue B
  val lds_loop_id = Reg(UInt(log2Up(concurrent_loops).W))
  val lds_loop = loops(lds_loop_id)
  // A-scale reuse: a loop with A = NULL (A resident) skips its A-scale load when its half still holds the
  // same slice from the last loop in this slot. Any non-loop command (manual load/config, requant) invalidates.
  val asc_valid = RegInit(VecInit(Seq.fill(concurrent_loops)(false.B)))
  val asc = Reg(Vec(concurrent_loops, new LoopMatmulAScaleSlice(coreMaxAddrBits, iterator_bitwidth)))
  val lrl = loop_requesting_lds
  val lds_a_hit = lrl.a_dram_addr === 0.U && asc_valid(loop_requesting_lds_id) &&
    asc(loop_requesting_lds_id).addr === lrl.a_scale_addr && asc(loop_requesting_lds_id).stride === lrl.a_scale_stride &&
    asc(loop_requesting_lds_id).max_i === lrl.max_i && asc(loop_requesting_lds_id).max_k === lrl.max_k
  when (lds_state === 0.U && lrl.configured && !lrl.lds_started) {
    lrl.lds_started := true.B
    lrl.lds_a_reuse := false.B
    when (lrl.a_scale_addr === 0.U) {
      lrl.lds_done := true.B
    }.otherwise {
      lrl.lds_a_reuse := lds_a_hit
      lds_state := Mux(lds_a_hit, 2.U, 1.U)
      lds_loop_id := loop_requesting_lds_id
    }
  }
  val lds_is_b = lds_state === 2.U
  val lds_rows = lds_loop.max_k / tilesPerMxBlock.U
  // a quad operand tile is 2*DIM rows (A) / cols (B): twice the scale bytes per k-block row
  val lds_row_bytes = Mux(lds_is_b, lds_loop.max_j << lds_loop.narrow_type, lds_loop.max_i << lds_loop.narrow_act) * block_size.U
  val lds_dest = Cat(lds_loop_id(0), 0.U(12.W))
  lds_cmd.valid := lds_state =/= 0.U
  lds_cmd.bits := DontCare
  lds_cmd.bits.inst.funct := MX_LOAD_SCALES
  lds_cmd.bits.rs1 := Cat(Mux(lds_is_b, lds_loop.b_scale_stride, lds_loop.a_scale_stride).pad(24)(23, 0),
                          Mux(lds_is_b, lds_loop.b_scale_addr, lds_loop.a_scale_addr).pad(40)(39, 0))
  lds_cmd.bits.rs2 := Cat(0.U(9.W), true.B, lds_rows(7, 0), lds_dest, lds_is_b, lds_row_bytes.pad(32)(31, 0))
  when (lds_cmd.fire) {
    when (lds_is_b) {
      lds_state := 0.U
      lds_loop.lds_done := true.B
    }.otherwise {
      lds_state := 2.U
      asc_valid(lds_loop_id) := true.B
      asc(lds_loop_id).addr := lds_loop.a_scale_addr
      asc(lds_loop_id).stride := lds_loop.a_scale_stride
      asc(lds_loop_id).max_i := lds_loop.max_i
      asc(lds_loop_id).max_k := lds_loop.max_k
    }
  }
  when (cmd.fire && !is_loop_cmd) { asc_valid.foreach(_ := false.B) }

  // LdL: per loop, once configured, gated MX_LOAD_LUTs of its act-in codebooks (rows >> G) and then its weight
  // codebooks (cols >> G), each into that memory's LUT half = loop slot (rs2: [31:0] count, [33:32] sel 1 = act-in /
  // 0 = weight, [39:34] entry bits, [40] half mode, [41] half, [42] gated). They go to the LUT loader, not the RS; the
  // loop's CONFIG_SCALE_MEM (rs2[20] / rs2[21]) waits for them to land.
  val loop_requesting_ldl_id = Mux(head_loop.ldl_done, tail_loop_id, head_loop_id)
  val ldl_loop = loops(loop_requesting_ldl_id)
  val ldl_need_a = ldl_loop.a_lut_addr =/= 0.U && !ldl_loop.ldl_a_issued
  val ldl_need_b = ldl_loop.b_lut_addr =/= 0.U
  val ldl_is_b = !ldl_need_a
  val ldl_rows = (ldl_loop.max_i * block_size.U) << ldl_loop.narrow_act
  // kblock codebooks: one set per K-block of the loop, contiguous in DRAM (loop column chunk = full N)
  val ldl_cols = ((ldl_loop.max_j * block_size.U) << ldl_loop.narrow_type) *
    Mux(ldl_loop.kmix, ldl_loop.nlut, Mux(ldl_loop.lut_kb, ldl_loop.max_k / tilesPerMxBlock.U, 1.U))
  val ldl_num  = Mux(ldl_is_b, ldl_cols, ldl_rows) >> io.lut_granularity(4, 0)
  val ldl_fmt  = MxCmdMode.fmt(Mux(ldl_is_b, ldl_loop.mx.wgt, ldl_loop.mx.act))
  val ldl_ebits = Mux(ldl_fmt === 1.U, 6.U(6.W), 8.U(6.W))   // FP6 codebooks 6 b, FP8 8 b
  val ldl_go = ldl_loop.configured && !ldl_loop.ldl_done
  when (ldl_go && !ldl_need_a && !ldl_need_b) { ldl_loop.ldl_done := true.B }
  ldl_cmd.valid := ldl_go && (ldl_need_a || ldl_need_b)
  ldl_cmd.bits := DontCare
  ldl_cmd.bits.inst.funct := MX_LOAD_LUT
  ldl_cmd.bits.rs1 := Mux(ldl_is_b, ldl_loop.b_lut_addr, ldl_loop.a_lut_addr)
  ldl_cmd.bits.rs2 := Cat(true.B, loop_requesting_ldl_id(0), true.B, ldl_ebits, Mux(ldl_is_b, 0.U(2.W), 1.U(2.W)),
                          ldl_num.pad(32)(31, 0))
  when (ldl_cmd.fire) {
    when (ldl_is_b) { ldl_loop.ldl_done := true.B }
    .otherwise { ldl_loop.ldl_a_issued := true.B; when (!ldl_need_b) { ldl_loop.ldl_done := true.B } }
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
  ex.io.req.bits.a_addr_start := Mux(loop_requesting_ex.spad_only || loop_requesting_ex.a_ex_spad_id === 0.U,
    loop_requesting_ex.a_addr_start, (loop_requesting_ex.a_ex_spad_id - 1.U) * (max_addr / concurrent_loops).U)
  ex.io.req.bits.b_addr_end := Mux(loop_requesting_ex.spad_only || loop_requesting_ex.b_ex_spad_id === 0.U,
    loop_requesting_ex.b_addr_end, (loop_requesting_ex.b_ex_spad_id) * (max_addr / concurrent_loops).U)
  ex.io.req.bits.a_tranpose := loop_requesting_ex.a_transpose
  ex.io.req.bits.b_tranpose := loop_requesting_ex.b_transpose
  ex.io.req.bits.c_addr_start := ex_c_addr_start
  ex.io.req.bits.loop_id := loop_requesting_ex_id
  ex.io.req.bits.narrow_type := loop_requesting_ex.narrow_type
  ex.io.req.bits.narrow_act := loop_requesting_ex.narrow_act
  ex.io.req.bits.shared := loop_requesting_ex.shared_acc
  ex.io.req.bits.lut_kb := loop_requesting_ex.lut_kb
  ex.io.req.bits.kmix := loop_requesting_ex.kmix
  ex.io.req.bits.kmask := loop_requesting_ex.kmask
  ex.io.req.bits.mx := loop_requesting_ex.mx
  ex.io.req.bits.skip := is_resadd
  ex.io.req.bits.scale_cfg := loop_requesting_ex.a_scale_addr =/= 0.U
  ex.io.req.bits.scale_a_reuse := loop_requesting_ex.lds_a_reuse
  ex.io.req.bits.lut_cfg := loop_requesting_ex.a_lut_addr =/= 0.U
  ex.io.req.bits.lut_cfg_w := loop_requesting_ex.b_lut_addr =/= 0.U
  ex.io.req.bits.lut_gran := io.lut_granularity
  assert(!(loop_requesting_ex.configured && (loop_requesting_ex.a_lut_addr =/= 0.U || loop_requesting_ex.b_lut_addr =/= 0.U) &&
           loop_requesting_ex.a_scale_addr === 0.U),
    "LOOP_WS_CONFIG_LUTS needs loop-managed scales (LOOP_WS_CONFIG_SCALES): the in-stream config carries the LUT half")
  assert(!(loop_requesting_ex.configured && loop_requesting_ex.shared_acc &&
           (loop_requesting_ex.a_scale_addr === 0.U || loop_requesting_ex.spad_only)),
    "shared acc layout needs loop-managed scales and a DRAM C store")

  ex.io.req.valid := !loop_requesting_ex.ex_started && loop_requesting_ex.lds_done && loop_requesting_ex.ldl_done &&
    loop_requesting_ex.lda_started &&
    loop_requesting_ex.ldb_started && loop_requesting_ex.ldd_started && loop_requesting_ex.configured 

  when (ex.io.req.fire) {
    loop_requesting_ex.running := true.B
    loop_requesting_ex.ex_started := true.B

    when (Mux(loop_requesting_ex.spad_only, loop_requesting_ex.inc_acc_addr, loop_requesting_ex.c_dram_addr =/= 0.U)) {
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
  stC.io.req.bits.max_k := Mux(is_resadd, 1.U, loop_requesting_st.max_k)
  stC.io.req.bits.max_j := loop_requesting_st.max_j
  // single x quad: row-tile pairs as quad tiles; an odd pair count leaves the last pair's upper half empty
  val st_shared = loop_requesting_st.shared_acc
  val st_ipair_r = ((block_size >= 16).B && loop_requesting_st.narrow_type && !loop_requesting_st.narrow_act) || st_shared
  val st_pair_r = (loop_requesting_st.narrow_act && !loop_requesting_st.narrow_type) || st_shared
  val st_max_i = Mux(st_ipair_r, (loop_requesting_st.max_i +& 1.U) >> 1, loop_requesting_st.max_i)
  val st_pad_i = Mux(st_ipair_r, loop_requesting_st.pad_i +& Mux(loop_requesting_st.max_i(0), block_size.U, 0.U),
    loop_requesting_st.pad_i)
  stC.io.req.bits.max_i := st_max_i
  stC.io.req.bits.pad_j := loop_requesting_st.pad_j
  stC.io.req.bits.pad_i := st_pad_i
  stC.io.req.bits.dram_addr := loop_requesting_st.c_dram_addr
  stC.io.req.bits.dram_stride := loop_requesting_st.c_dram_stride
  stC.io.req.bits.full_c := loop_requesting_st.full_c
  stC.io.req.bits.act := loop_requesting_st.act
  stC.io.req.bits.addr_start := st_c_addr_start
  stC.io.req.bits.loop_id := loop_requesting_st_id
  stC.io.req.bits.is_resadd := is_resadd
  stC.io.req.bits.output_mx_format := io.output_mx_format
  stC.io.req.bits.activation_mx_format := io.activation_mx_format
  stC.io.req.bits.mx_multi_elem := loop_requesting_st.narrow_type || st_pair_r
  stC.io.req.bits.pair := st_pair_r
  stC.io.req.bits.ipair := st_ipair_r
  stC.io.req.bits.mx_multi_elem_act := loop_requesting_st.narrow_act || st_ipair_r

  stC_spad.io.req.bits.max_k := Mux(is_resadd, 1.U, loop_requesting_st.max_k)
  stC_spad.io.req.bits.max_j := loop_requesting_st.max_j
  stC_spad.io.req.bits.max_i := st_max_i
  stC_spad.io.req.bits.pad_j := loop_requesting_st.pad_j
  stC_spad.io.req.bits.pad_i := st_pad_i
  stC_spad.io.req.bits.src_addr := st_c_addr_start
  stC_spad.io.req.bits.dst_addr := loop_requesting_st.c_spad_addr
  stC_spad.io.req.bits.full_c := loop_requesting_st.full_c
  stC_spad.io.req.bits.act := loop_requesting_st.act
  stC_spad.io.req.bits.loop_id := loop_requesting_st_id
  stC_spad.io.req.bits.is_resadd := is_resadd
  stC_spad.io.req.bits.output_mx_format := io.output_mx_format
  stC_spad.io.req.bits.activation_mx_format := io.activation_mx_format
  stC_spad.io.req.bits.mx_multi_elem := loop_requesting_st.narrow_type || st_pair_r
  stC_spad.io.req.bits.pair := st_pair_r
  stC_spad.io.req.bits.ipair := st_ipair_r
  stC_spad.io.req.bits.mx_multi_elem_act := loop_requesting_st.narrow_act || st_ipair_r
  stC_spad.io.req.bits.reuse_tiled := loop_requesting_st.reuse_tiled

  // WAR overlap: with all A/B operand and C output spad regions known, test whether C overlaps A or B
  // (operand bases mirror the execute spad_id remap). A = max_i*max_k tiles, B = max_k*max_j tiles,
  // each block_size rows; C span = FP8 output width (BF16/full is 2x). Overlap gates the WAR drain below.
  val st_a_lo = Mux(loop_requesting_st.spad_only || loop_requesting_st.a_ex_spad_id === 0.U,
    loop_requesting_st.a_addr_start, (loop_requesting_st.a_ex_spad_id - 1.U) * (max_addr / concurrent_loops).U)
  val st_a_hi = st_a_lo +& (loop_requesting_st.max_i * loop_requesting_st.max_k * block_size.U)
  val st_b_hi = Mux(loop_requesting_st.spad_only || loop_requesting_st.b_ex_spad_id === 0.U,
    loop_requesting_st.b_addr_end, loop_requesting_st.b_ex_spad_id * (max_addr / concurrent_loops).U)
  val st_b_lo = st_b_hi -& (loop_requesting_st.max_k * (loop_requesting_st.max_j << loop_requesting_st.kmix) * block_size.U)
  val st_c_lo = loop_requesting_st.c_spad_addr
  // quad tiles are 2*DIM rows (act) / cols (weight): x2 each
  val st_c_span = (Mux(io.output_mx_format === 3.U || loop_requesting_st.full_c,
    2.U * loop_requesting_st.max_i * loop_requesting_st.max_j * block_size.U,
    loop_requesting_st.max_i * loop_requesting_st.max_j * block_size.U) << (loop_requesting_st.narrow_type.asUInt +& loop_requesting_st.narrow_act.asUInt)).asUInt
  val st_c_hi = st_c_lo +& st_c_span
  val st_c_overlaps_a = (st_c_lo < st_a_hi) && (st_a_lo < st_c_hi)
  val st_c_overlaps_b = (st_c_lo < st_b_hi) && (st_b_lo < st_c_hi)
  stC_spad.io.req.bits.dst_overlaps_operands := st_c_overlaps_a || st_c_overlaps_b
  dontTouch(stC_spad.io.req.bits.dst_overlaps_operands)

  // A store-only loop (EX skipped) must not start while the older loop's EX is still unrolling. A loop's stores are
  // otherwise ordered only against its OWN EX (stC*.io.ex_completed above): for a store-only loop that check is
  // vacuous, and the reservation station's st<-ex accumulator RAW only sees EX commands already enqueued, so the
  // stores can read the accumulator while the older loop's remaining computes are still to come (compute-only
  // LOOP_WS followed by a store-only LOOP_WS over the same accumulator rows). Once the older loop's ex_completed is
  // set, all its EX commands are in the reservation station and that RAW orders the stores. Loops with their own
  // EX are unaffected: the EX unroller serves loops in order, so the older loop's EX is always complete by then.
  val st_blocked = !is_resadd && loop_requesting_st.ex_skipped && loop_requesting_st_id =/= head_loop_id &&
    head_loop.configured && !head_loop.ex_completed
  stC.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.ex_started &&
    loop_requesting_st.configured && !loop_requesting_st.spad_only && !st_blocked
  stC_spad.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.ex_started &&
    loop_requesting_st.configured && loop_requesting_st.spad_only && !st_blocked

  when (stC.io.req.fire || stC_spad.io.req.fire) {
    loop_requesting_st.running := true.B
    loop_requesting_st.st_started := true.B

    when (Mux(loop_requesting_st.spad_only, loop_requesting_st.inc_acc_addr, loop_requesting_st.c_dram_addr =/= 0.U)) {
      st_c_addr_start := floorAdd(st_c_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  when(is_resadd){
    ldA.io.req.bits.addr_start := loop_requesting_ldA.resadd_addr_start
    ldB.io.req.bits.addr_end := loop_requesting_ldB.resadd_addr_start
    stC.io.req.bits.addr_start := loop_requesting_st.resadd_addr_start
    stC.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.configured
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

  when (stC.io.idle && loops(stC.io.loop_id).running && loops(stC.io.loop_id).st_started && !loops(stC.io.loop_id).spad_only) {
    loops(stC.io.loop_id).st_completed := true.B
  }

  when (stC_spad.io.idle && loops(stC_spad.io.loop_id).running &&
    loops(stC_spad.io.loop_id).st_started && loops(stC_spad.io.loop_id).spad_only) {
    loops(stC_spad.io.loop_id).st_completed := true.B
  }

  when (head_loop.running && head_loop.all_completed()) {
    head_loop.reset()
    io.completed(head_loop_id) := true.B
    head_loop_id := ~head_loop_id
  }

  // Resets
  when (reset.asBool) {
    loops.zipWithIndex.foreach { case (l, i) =>
      l.reset()
      l.a_addr_start := (i * (max_addr / concurrent_loops)).U
      l.b_addr_end := ((i+1) * (max_addr / concurrent_loops)).U
      l.resadd_addr_start := (i * (max_acc_addr / concurrent_loops)).U
    }
  }
  // generated commands: the mode of the loop that issued each one
  val ldab_id = Mux(ldA.io.cmd.valid && ldA.io.cmd.ready, ldA.io.loop_id, ldB.io.loop_id)
  val gen_ids = VecInit(Seq(lds_loop_id, loop_requesting_ldl_id, stC.io.loop_id, ex.io.loop_id, ldD.io.loop_id, ldab_id,
    stC_spad.io.loop_id))
  gen_mx := Mux(arb.io.chosen === 3.U, ex.io.mx, loops(gen_ids(arb.io.chosen)).mx)

}

object LoopMatmul {
  def apply(in: DecoupledIO[GemminiCmd], ld_completed: UInt, st_completed: UInt, ex_completed: UInt,
            block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
            max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
            mvin_rs2_t: MvinRs2, preload_rs1_t: PreloadRs, preload_rs2_t: PreloadRs,
            compute_rs1_t: ComputeRs, compute_rs2_t: ComputeRs, mvout_spad_rs1_t: MvoutSpadRs1, mvout_rs2_t: MvoutRs2,
            vec_bypass: Boolean = false, ld_bypass: Boolean = false, use_mx_scaling: Boolean = true, lut_quad: Boolean = false)
           (implicit p: Parameters): (DecoupledIO[GemminiCmd], Bool, Vec[Bool], LoopMatmul) = {
    val mod = Module(new LoopMatmul(block_size, coreMaxAddrBits, rob_size, max_lds, max_exs, max_sts,
      max_addr, max_acc_addr, input_w, acc_w, dma_max_bytes,
      mvin_rs2_t, preload_rs1_t, preload_rs2_t, compute_rs1_t, compute_rs2_t, mvout_spad_rs1_t, mvout_rs2_t, vec_bypass, ld_bypass,
      use_mx_scaling, lut_quad))
    mod.io.in <> in
    mod.io.ld_completed := ld_completed
    mod.io.st_completed := st_completed
    mod.io.ex_completed := ex_completed
    (mod.io.out, mod.io.busy, mod.io.completed, mod)
  }

  def castDramOffset(dram_offset: UInt): UInt = {
    // Cast dram offsets to 32 bits max
    dram_offset & "hFFFFFFFF".U
  }
}


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

class LoopMataddLdAReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int) extends Bundle {
  //val max_i = UInt(iterator_bitwidth.W)
  val max_k = Vec(4, UInt(iterator_bitwidth.W))
  val max_i = UInt(iterator_bitwidth.W)
  //val pad_i = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val pad_k = Vec(4, UInt(log2Up(block_size).W))
  val num_blk = UInt(3.W)
  val dram_addr_offset = Vec(4, UInt(iterator_bitwidth.W))
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  //val transpose = Bool()
  val addr_start = UInt(log2Up(max_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  //val is_resadd = Bool()
}

class LoopMataddLdA(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                    max_block_len: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMataddLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))
    val i = Output(UInt(iterator_bitwidth.W))
    //val k = Output(UInt(iterator_bitwidth.W))
    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMataddLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops))

  val b = Reg(UInt(3.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val k = Reg(UInt(iterator_bitwidth.W))
  val accumulated_i = Reg(UInt((iterator_bitwidth).W))

  val accumulated_sp = Reg(UInt((iterator_bitwidth+2).W))
  val blk_iterator = b
  val row_iterator = i //Mux(req.transpose, k, i)
  val col_iterator = k //Mux(req.transpose, i, k)

  val max_blk_iterator = req.num_blk
  val max_row_iterator = req.max_i //Mux(req.transpose, req.max_k, req.max_i)
  val max_col_iterator = req.max_k(b) //Mux(req.transpose, req.max_i, req.max_k)

  val row_pad = req.pad_i //Mux(req.transpose, req.pad_k, req.pad_i)
  val col_pad = req.pad_k(b) //Mux(req.transpose, req.pad_i, req.pad_k)

  val max_col_dim = req.max_k(b) //Mux(req.transpose, req.max_i, req.max_k)
  val max_blocks = Mux(max_col_dim <= max_block_len.U, max_col_dim, max_block_len.U)

  val sp_addr_start = req.addr_start

  val col_offset = req.dram_addr_offset(b)
  val dram_offset = ((row_iterator * req.dram_stride + col_iterator) * block_size.U + col_offset)* (input_w/8).U
  val dram_addr = req.dram_addr + LoopMatadd.castDramOffset(dram_offset)
  val sp_addr = sp_addr_start + (row_iterator * max_col_iterator + accumulated_sp + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(row_iterator === max_row_iterator-1.U, row_pad, 0.U)
  dontTouch(rows)
  dontTouch(cols)
  dontTouch(sp_addr)
  dontTouch(dram_offset)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt
  mvin_cmd_rs2.num_cols := cols.asUInt
  //mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt
  //when(req.is_resadd){
    mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = true.B, read_full = false.B)
  //}

  io.req.ready := state === idle
  io.i := i + accumulated_i //i
  //io.k := k
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded && req.dram_addr =/= 0.U
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when(req.dram_addr === 0.U){
    state := idle
  }.elsewhen(io.cmd.fire) {
    // The order here is k, j, i
    //val i_blocks = Mux(req.transpose, max_blocks, 1.U)
    //val k_blocks = Mux(req.transpose, 1.U, max_blocks)

    //val next_i = floorAdd(i, 1.U, max_row_iterator)
    //val next_k = floorAdd(k, max_blocks, max_col_iterator, next_i === 0.U)
    val next_k = floorAdd(k, max_blocks, max_col_iterator)
    val next_i = floorAdd(i, 1.U, max_row_iterator, next_k === 0.U)
    val next_blk = floorAdd(b, 1.U, req.num_blk, next_i === 0.U && next_k === 0.U)

    when (next_i === 0.U && next_k === 0.U && next_blk === 0.U) {
      state := idle
    }
    .elsewhen (next_i === 0.U && next_k === 0.U) {
      accumulated_i := accumulated_i + max_row_iterator
      accumulated_sp := accumulated_sp + max_col_iterator * max_row_iterator
    }
    i := next_i
    k := next_k
    b := next_blk
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    i := 0.U
    k := 0.U
    b := 0.U
    accumulated_i := 0.U
    accumulated_sp := 0.U
  }
}

// LdB

class LoopMataddLdBReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int) extends Bundle {
  //val max_k = UInt(iterator_bitwidth.W)
  val max_j = Vec(4, UInt(iterator_bitwidth.W))
  val max_k = UInt(iterator_bitwidth.W)
  //val pad_k = UInt(log2Up(block_size).W)
  val pad_j = Vec(4, UInt(log2Up(block_size).W))
  val pad_k = UInt(log2Up(block_size).W)
  val num_blk = UInt(3.W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  val dram_addr_offset = Vec(4, UInt(iterator_bitwidth.W))
  //val transpose = Bool()
  val addr_end = UInt(log2Up(max_addr+1).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  //val is_resadd = Bool()
}

class LoopMataddLdB(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                    max_block_len: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMataddLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops)))
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

  val req = Reg(new LoopMataddLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, concurrent_loops))

  val b = Reg(UInt(3.W))
  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val accumulated_k = Reg(UInt((iterator_bitwidth).W))
  val accumulated_sp = Reg(UInt((2+iterator_bitwidth).W))

  val row_iterator = k// Mux(req.transpose, j, k)
  val col_iterator = j//Mux(req.transpose, k, j)
  val blk_iterator = b

  val max_blk_iterator = req.num_blk
  val max_row_iterator = req.max_k //Mux(req.transpose, req.max_j, req.max_k)
  val max_col_iterator = req.max_j(b) //Mux(req.transpose, req.max_k, req.max_j)

  val row_pad = req.pad_k //Mux(req.transpose, req.pad_j, req.pad_k)
  val col_pad = req.pad_j(b) //Mux(req.transpose, req.pad_k, req.pad_j)

  val max_col_dim = req.max_j(b) //Mux(req.transpose, req.max_k, req.max_j)
  val max_blocks = Mux(max_col_dim <= max_block_len.U, max_col_dim, max_block_len.U)

  val sp_addr_start = req.addr_end 

  val col_offset = req.dram_addr_offset(b)
  val dram_offset = ((row_iterator * req.dram_stride + col_iterator) * block_size.U + col_offset)* (input_w/8).U
  val dram_addr = req.dram_addr + LoopMatadd.castDramOffset(dram_offset)
  //val sp_addr = sp_addr_start + (total_row_iterator * max_col_iterator + col_iterator) * block_size.U
  val sp_addr = sp_addr_start + (row_iterator * max_col_iterator + accumulated_sp + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(row_iterator === max_row_iterator-1.U, row_pad, 0.U)
  dontTouch(rows)
  dontTouch(cols)
  dontTouch(sp_addr)
  dontTouch(dram_offset)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD2_CMD
  mvin_cmd.rs1 := dram_addr

  val mvin_cmd_rs2 = Wire(mvin_rs2_t.cloneType)
  mvin_cmd_rs2 := DontCare
  mvin_cmd_rs2.num_rows := rows.asUInt
  mvin_cmd_rs2.num_cols := cols.asUInt
  //mvin_cmd_rs2.local_addr := cast_to_sp_addr(mvin_cmd_rs2.local_addr, sp_addr)
  mvin_cmd.rs2 := mvin_cmd_rs2.asUInt

  //when (req.is_resadd){
    mvin_cmd_rs2.local_addr := cast_to_acc_addr(mvin_cmd_rs2.local_addr, sp_addr, accumulate = true.B, read_full = false.B)
  //}

  io.req.ready := state === idle
  io.k := k + accumulated_k //k
  io.j := j
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded && req.dram_addr =/= 0.U
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  when(req.dram_addr === 0.U){
    state := idle
  }.elsewhen(io.cmd.fire) {
    // The order here is k, j, i
    //val j_blocks = Mux(req.transpose, 1.U, max_blocks)
    //val k_blocks = Mux(req.transpose, max_blocks, 1.U)

    val next_j = floorAdd(j, max_blocks, req.max_j(b))
    val next_k = floorAdd(k, 1.U, req.max_k, next_j === 0.U)
    val next_blk = floorAdd(b, 1.U, req.num_blk, next_j=== 0.U && next_k === 0.U)

    when (next_j === 0.U && next_k === 0.U && next_blk === 0.U) {
      state := idle
    }
    .elsewhen(next_j === 0.U && next_k === 0.U){
      accumulated_k := accumulated_k + max_row_iterator
      accumulated_sp := accumulated_sp + max_col_iterator * max_row_iterator
    }
    j := next_j
    k := next_k
    b := next_blk
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    j := 0.U
    k := 0.U
    b := 0.U
    accumulated_k := 0.U
    accumulated_sp := 0.U
  }
}

// LdD

class LoopMataddLdDReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_i = UInt(iterator_bitwidth.W)
  //val max_i = UInt(iterator_bitwidth.W)
  val max_j = Vec(4, UInt(iterator_bitwidth.W))
  val pad_i = UInt(log2Up(block_size).W)
  //val pad_i = UInt(log2Up(block_size).W)
  val pad_j = Vec(4, UInt(log2Up(block_size).W))
  val num_blk = UInt(3.W)
  //val dram_addr = UInt(coreMaxAddrBits.W)
  //val dram_stride = UInt(coreMaxAddrBits.W)
  //val low_d = Bool()
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  //val is_resadd = Bool()
}

class LoopMataddLdD(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int,
                    acc_w: Int, max_block_len: Int, max_block_len_acc: Int, concurrent_loops: Int, mvin_rs2_t: MvinRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMataddLdDReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops)))
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

  val req = Reg(new LoopMataddLdDReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops))

  val b = Reg(UInt(3.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val accumulated_i = Reg(UInt((iterator_bitwidth).W))
  val accumulated_sp = Reg(UInt((2+iterator_bitwidth).W))

  //val max_blocks = Mux(req.low_d, Mux(req.max_j <= max_block_len.U, req.max_j, max_block_len.U),
  //  Mux(req.max_j <= max_block_len_acc.U, req.max_j, max_block_len_acc.U))
  val max_blocks = Mux(req.max_j(b) <= max_block_len_acc.U, req.max_j(b), max_block_len_acc.U)

  val acc_addr_start = req.addr_start

  //val dram_offset = Mux(req.low_d, (i * req.dram_stride + j) * block_size.U * (input_w/8).U,
  //  (i * req.dram_stride + j) * block_size.U * (acc_w/8).U)
  val dram_addr = 0.U //Mux(req.is_resadd, 0.U, req.dram_addr + LoopMatadd.castDramOffset(dram_offset))
  //val sp_addr = acc_addr_start + ((i + accumulated_i) * req.max_j + j) * block_size.U
  val sp_addr = acc_addr_start + (i * req.max_j(b) + accumulated_sp + j) * block_size.U
  val blocks = Mux(j + max_blocks <= req.max_j(b), max_blocks, req.max_j(b)-j)
  val cols = (blocks * block_size.U) - Mux(j + blocks >= req.max_j(b), req.pad_j(b), 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)

  dontTouch(rows)
  dontTouch(cols)
  dontTouch(sp_addr)

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
  io.cmd.valid := state =/= idle && !io.rob_overloaded //&& !(req.dram_addr === 0.U && !req.is_resadd)
  io.cmd.bits := mvin_cmd

  io.loop_id := req.loop_id

  //when (req.dram_addr === 0.U && !req.is_resadd) {
  //  state := idle
  //}.else
  when (io.cmd.fire) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_j = floorAdd(j, max_blocks, req.max_j(b), next_i === 0.U)
    val next_blk = floorAdd(b, 1.U, req.num_blk, next_i === 0.U && next_j === 0.U)

    i := next_i
    j := next_j
    b := next_blk

    when (next_i === 0.U && next_j === 0.U && next_blk === 0.U) {
      state := idle
    }
    .elsewhen(next_i === 0.U && next_j === 0.U){
      accumulated_i := accumulated_i + req.max_i
      accumulated_sp := accumulated_sp + req.max_i * req.max_j(b)
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    state := ld
    j := 0.U
    i := 0.U
    b := 0.U
    accumulated_i := 0.U
    accumulated_sp := 0.U
  }
}
// StC

class LoopMataddStCReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int) extends Bundle {
  val max_i = UInt(iterator_bitwidth.W)
  //val max_i = UInt(iterator_bitwidth.W)
  val max_j = Vec(4, UInt(iterator_bitwidth.W))
  val pad_i = UInt(log2Up(block_size).W)
  //val pad_i = UInt(log2Up(block_size).W)
  val pad_j = Vec(4, UInt(log2Up(block_size).W))
  val dram_addr_offset = Vec(4, UInt(iterator_bitwidth.W))
  val num_blk = UInt(3.W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
  //val full_c = Bool()
  //val act = UInt(Activation.bitwidth.W)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
  //val is_resadd = Bool()
}

class LoopMataddStC(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, max_block_len: Int, concurrent_loops: Int, mvout_rs2_t: MvoutRs2)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMataddStCReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops)))
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

  val req = Reg(new LoopMataddStCReq(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, concurrent_loops))

  // Non-normalization-related iterators and calculations
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))
  val b = Reg(UInt(3.W))
  val accumulated_sp = Reg(UInt((2+iterator_bitwidth).W))
  val accumulated_i = Reg(UInt((iterator_bitwidth).W))

  //val max_blocks = Mux(req.full_c, 1.U, Mux(req.max_j <= max_block_len.U, req.max_j, max_block_len.U))
  val max_blocks = Mux(req.max_j(b) <= max_block_len.U, req.max_j(b), max_block_len.U)
  val acc_addr_start = /*(BigInt(1) << 31).U | (req.full_c << 29.U).asUInt |*/ req.addr_start

  val col_offset = req.dram_addr_offset(b)
  //val dram_offset = Mux(req.full_c, (row_offset * req.dram_stride + j) * block_size.U * (acc_w/8).U,
  //  (row_offset * req.dram_stride + j) * block_size.U * (input_w/8).U)
  val dram_offset = ((i * req.dram_stride + j) * block_size.U + col_offset) * (acc_w/8).U
  val dram_addr = req.dram_addr + LoopMatadd.castDramOffset(dram_offset)
  val sp_addr = acc_addr_start + (i * req.max_j(b) + accumulated_sp + j) * block_size.U
  val blocks = Mux(j + max_blocks <= req.max_j(b), max_blocks, req.max_j(b)-j)
  val cols = (blocks * block_size.U) - Mux(j + blocks >= req.max_j(b), req.pad_j(b), 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)
  dontTouch(rows)
  dontTouch(cols)
  dontTouch(sp_addr)
  dontTouch(dram_offset)

  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := dram_addr

  val mvout_cmd_rs2 = Wire(mvout_rs2_t.cloneType)
  mvout_cmd_rs2 := DontCare
  mvout_cmd_rs2.num_rows := rows.asUInt
  mvout_cmd_rs2.num_cols := cols.asUInt
  mvout_cmd_rs2.local_addr := cast_to_acc_addr(mvout_cmd_rs2.local_addr, sp_addr, accumulate = false.B, read_full = false.B) //req.full_c)
  mvout_cmd.rs2 := mvout_cmd_rs2.asUInt

  io.req.ready := state === idle
  io.j := j
  io.i := i + accumulated_i
  io.idle := state === idle

  // The order here is k, j, i when not doing LAYERNORM or SOFTMAX
  /*
  val ex_ahead = WireInit(io.ex_completed ||
    ((req.act =/= Activation.LAYERNORM) && (req.act =/= Activation.SOFTMAX) &&
      (io.ex_k === req.max_k - 1.U &&
        (io.ex_j >= j + blocks ||
          ((io.ex_j === j + blocks - 1.U) && io.ex_i > i)))))
  when(req.is_resadd){
    ex_ahead := io.ex_completed || (io.ex_i > i || (io.ex_i === i && io.ex_j >= j + blocks))
  }
   */
  val total_row_iterator = i + accumulated_i
  val ex_ahead = io.ex_completed || (io.ex_i > total_row_iterator || (io.ex_i === total_row_iterator && io.ex_j >= j + blocks))

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ex_ahead && req.dram_addr =/= 0.U
  io.cmd.bits := mvout_cmd

  io.loop_id := req.loop_id

  when (req.dram_addr === 0.U) {
    state := idle
  }.elsewhen (io.cmd.fire() && state === st) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_j = floorAdd(j, max_blocks, req.max_j(b), next_i === 0.U)
    val next_blk = floorAdd(b, 1.U, req.num_blk, next_i === 0.U && next_j === 0.U)

    i := next_i
    j := next_j
    b := next_blk

    when (next_i === 0.U && next_j === 0.U && next_blk === 0.U) {
      state := idle
    }
    .elsewhen (next_i === 0.U && next_j === 0.U) {
      accumulated_i := accumulated_i + req.max_i
      accumulated_sp := accumulated_sp + req.max_i * req.max_j(b)
    }
  }

  when (io.req.fire) {
    req := io.req.bits
    //state := Mux((io.req.bits.act === Activation.LAYERNORM) || (io.req.bits.act === Activation.SOFTMAX), ln_config, st)
    state := st

    j := 0.U
    i := 0.U
    b := 0.U
    accumulated_i := 0.U
    accumulated_sp := 0.U
  }
}

// Combined loop
class LoopMataddState(val iterator_bitwidth: Int, val coreMaxAddrBits: Int, val max_acc_addr: Int) extends Bundle {
  //val max_k = UInt(iterator_bitwidth.W)
  val max_j = Vec(4, UInt(iterator_bitwidth.W))
  val max_i = UInt(iterator_bitwidth.W)
  val a_row_offset = Vec(4, UInt(iterator_bitwidth.W))
  val b_row_offset = Vec(4, UInt(iterator_bitwidth.W))

  //val pad_k = UInt(iterator_bitwidth.W)
  val pad_j = Vec(4, UInt(4.W))
  val pad_i = UInt(4.W)

  val a_dram_addr = UInt(coreMaxAddrBits.W)
  val b_dram_addr = UInt(coreMaxAddrBits.W)
  //val d_dram_addr = UInt(coreMaxAddrBits.W)
  val c_dram_addr = UInt(coreMaxAddrBits.W)

  val a_dram_stride = UInt(coreMaxAddrBits.W)
  val b_dram_stride = UInt(coreMaxAddrBits.W)
  //val d_dram_stride = UInt(coreMaxAddrBits.W)
  val c_dram_stride = UInt(coreMaxAddrBits.W)

  //val a_transpose = Bool()
  //val b_transpose = Bool()
  //val act = UInt(Activation.bitwidth.W)
  val num_blk = UInt(3.W)

  //val low_d = Bool()
  //val full_c = Bool()
  //val ex_accumulate = Bool()

  //val a_ex_spad_id = UInt(2.W)
  //val b_ex_spad_id = UInt(2.W)
  val configured = Bool()

  val running = Bool()

  val lda_started = Bool()
  val ldb_started = Bool()
  //val ex_started = Bool()
  val ldd_started = Bool()
  val st_started = Bool()

  val lda_completed = Bool()
  val ldb_completed = Bool()
  //val ex_completed = Bool()
  val ldd_completed = Bool()
  val st_completed = Bool()

  def all_completed(dummy: Int=0): Bool = lda_completed && ldb_completed && ldd_completed && st_completed

  //val a_addr_start = UInt(log2Up(max_acc_addr).W)
  //val b_addr_end = UInt(log2Up(max_acc_addr+1).W)
  val resadd_addr_start = UInt(log2Up(max_acc_addr).W)

  def reset(): Unit = {
    configured := false.B

    running := false.B

    lda_started := false.B
    ldb_started := false.B
    //ex_started := false.B
    ldd_started := false.B
    st_started := false.B

    lda_completed := false.B
    ldb_completed := false.B
    //ex_completed := false.B
    ldd_completed := false.B
    st_completed := false.B

    //is_resadd := false.B
  }
}

class LoopMatadd(block_size: Int, coreMaxAddrBits: Int, reservation_station_size: Int, max_lds: Int, max_sts: Int,
                 max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
                 mvin_rs2_t: MvinRs2, mvout_rs2_t: MvoutRs2)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 12// 16
  val offset_bitwidth = 12
  val small_bitwidth = 4
  //val max_block_len = (dma_max_bytes / (block_size * input_w / 8)) max 1
  val max_block_len_acc = (dma_max_bytes / (block_size * acc_w / 8)) max 1
  val config_bitwidth = 16

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(reservation_station_size)))
    val out = Decoupled(new GemminiCmd(reservation_station_size))
    val ld_completed = Input(UInt(log2Up(reservation_station_size + 1).W))
    val st_completed = Input(UInt(log2Up(reservation_station_size + 1).W))
    //val ex_completed = Input(UInt(log2Up(reservation_station_size+1).W))
    val busy = Output(Bool())
  })

  // Create states
  val concurrent_loops = 2
  val loops = Reg(Vec(concurrent_loops, new LoopMataddState(iterator_bitwidth, coreMaxAddrBits, max_acc_addr)))
  val head_loop_id = Reg(UInt(log2Up(concurrent_loops).W))
  val tail_loop_id = (~head_loop_id).asUInt // This is the loop that we always try to configure if available
  val head_loop = loops(head_loop_id)
  val tail_loop = loops(tail_loop_id)

  val loop_configured = loops.map(_.configured).reduce(_ || _)

  val loop_being_configured_id = Mux(head_loop.configured, tail_loop_id, head_loop_id)
  val loop_being_configured = loops(loop_being_configured_id)

  //val is_resadd = true.B

  val max_all_addr = max_acc_addr
  // Create inner modules
  val ldA = Module(new LoopMataddLdA(block_size, coreMaxAddrBits, iterator_bitwidth, max_all_addr, input_w, max_block_len_acc, concurrent_loops, mvin_rs2_t))
  val ldB = Module(new LoopMataddLdB(block_size, coreMaxAddrBits, iterator_bitwidth, max_all_addr, input_w, max_block_len_acc, concurrent_loops, mvin_rs2_t))
  val ldD = Module(new LoopMataddLdD(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, max_block_len_acc, max_block_len_acc, concurrent_loops, mvin_rs2_t))
  //val ex = Module(new LoopMataddExecute(block_size, coreMaxAddrBits, iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops, preload_rs1_t, preload_rs2_t, compute_rs1_t, compute_rs2_t))
  val stC = Module(new LoopMataddStC(block_size, coreMaxAddrBits, iterator_bitwidth, max_acc_addr, input_w, acc_w, max_block_len_acc, concurrent_loops, mvout_rs2_t))

  // Create command queue
  val cmd = Queue(io.in)

  io.busy := cmd.valid || loop_configured

  // Create reservation station utilization counters
  val ld_utilization = RegInit(0.U(log2Up(max_lds + 1).W))
  val st_utilization = RegInit(0.U(log2Up(max_sts + 1).W))
  //val ex_utilization = RegInit(0.U(log2Up(max_exs+1).W))
  // Create ld arbiters
  val ldab_arb = Module(new WeightedMataddArbiter(new RoCCCommand(), maxWeightA = 255, staticWeightAEnabled = true)) // TODO magic numbers
  ldab_arb.io.inA <> ldA.io.cmd
  ldab_arb.io.inB <> ldB.io.cmd
  val ab_loads_on_same_loop = ldA.io.loop_id === ldB.io.loop_id
  val forceA = !ab_loads_on_same_loop && ldA.io.loop_id === head_loop_id
  val forceB = !ab_loads_on_same_loop && ldB.io.loop_id === head_loop_id
  //ldab_arb.io.forceA := Mux(is_resadd, ab_loads_on_same_loop && !ldA.io.idle, forceA)
  //ldab_arb.io.forceB := Mux(is_resadd, forceB || ldA.io.idle, forceB)
  ldab_arb.io.forceA := ab_loads_on_same_loop && !ldA.io.idle
  ldab_arb.io.forceB := forceB || ldA.io.idle
  //ldab_arb.io.forceB := Mux(is_resadd, (forceB || ldA.io.idle) && (ld_utilization === 0.U), forceB)
  ldab_arb.io.weightA := 0.U
  ldab_arb.io.inA_idle := ldA.io.idle
  ldab_arb.io.inB_idle := ldB.io.idle
  //ldab_arb.io.inA_j := ldA.io.k
  ldab_arb.io.inA_i := ldA.io.i
  ldab_arb.io.inB_i := ldB.io.k
  //ldab_arb.io.inB_j := ldB.io.j

  // Create global arbiter
  val arb = Module(new Arbiter(new RoCCCommand(), 3))
  arb.io.in(0) <> stC.io.cmd
  //arb.io.in(1) <> ex.io.cmd
  arb.io.in(1) <> ldD.io.cmd
  arb.io.in(2) <> ldab_arb.io.out
  val unrolled_cmd = arb.io.out

  ld_utilization := ld_utilization +& (ldA.io.cmd.fire || ldB.io.cmd.fire || ldD.io.cmd.fire) -& io.ld_completed
  st_utilization := st_utilization +& stC.io.cmd.fire -& io.st_completed
  //ex_utilization := ex_utilization +& ex.io.cmd.fire -& io.ex_completed

  assert(ld_utilization >= io.ld_completed, "ld utilization underflow")
  assert(st_utilization >= io.st_completed, "st utilization underflow")
  //assert(ex_utilization >= io.ex_completed, "ex utilization underflow")

  // Wire up unrolled command output
  //val is_loop_run_cmd = cmd.bits.cmd.inst.funct === LOOP_WS
  val is_loop_config_cmd = (cmd.bits.cmd.inst.funct >= LOOP_MATADD_CONFIG_BOUNDS && cmd.bits.cmd.inst.funct <= LOOP_MATADD_CONFIG_PACK)
  val is_loop_cmd = is_loop_config_cmd //is_loop_run_cmd || is_loop_config_cmd

  io.out.bits.cmd := Mux(loop_configured, unrolled_cmd.bits, cmd.bits.cmd)
  io.out.bits.cmd.status := cmd.bits.cmd.status // TODO This is not guaranteed to be the correct fix! We must fix this
  io.out.bits.rob_id := DontCare
  io.out.bits.from_matadd_fsm := Mux(loop_configured, true.B, cmd.bits.from_matadd_fsm)
  io.out.bits.from_matmul_fsm := Mux(loop_configured, false.B, cmd.bits.from_matmul_fsm)
  io.out.bits.from_conv_fsm := Mux(loop_configured, false.B, cmd.bits.from_conv_fsm)
  io.out.valid := Mux(loop_configured, unrolled_cmd.valid, cmd.valid && !is_loop_config_cmd) // && !is_loop_run_cmd)

  //cmd.ready := Mux(is_loop_cmd, !loop_being_configured.configured && !(is_resadd && ld_utilization > 0.U), !loop_configured && io.out.ready)
  cmd.ready := Mux(is_loop_cmd, !loop_being_configured.configured && !(ld_utilization > 0.U), !loop_configured && io.out.ready)
  arb.io.out.ready := io.out.ready

  // Wire up overloaded signals
  ldA.io.rob_overloaded := ld_utilization >= max_lds.U
  ldB.io.rob_overloaded := ld_utilization >= max_lds.U
  //ex.io.rob_overloaded := ex_utilization >= max_exs.U
  ldD.io.rob_overloaded := ld_utilization >= max_lds.U
  stC.io.rob_overloaded := st_utilization >= max_sts.U

  // when loop matmul is used as resadd unroller
  // skip ex
  // track ldB instead of ex
  stC.io.ex_completed := (ldA.io.loop_id =/= stC.io.loop_id || ldA.io.idle) && (ldB.io.loop_id =/= stC.io.loop_id || ldB.io.idle)
  stC.io.ex_k := 0.U // req.max_k shall be 1
  stC.io.ex_j := ldB.io.j
  stC.io.ex_i := ldB.io.k
  //ldB.io.rob_overloaded := ld_utilization >= max_lds.U || !((ldA.io.loop_id =/= ldB.io.loop_id) || ldA.io.idle)


  val loops_configured = RegInit(0.U(16.W))
  dontTouch(loops_configured)

  val packed = RegInit(false.B)
  // Create config registers
  when(cmd.valid && is_loop_cmd && !loop_being_configured.configured) {

    switch(cmd.bits.cmd.inst.funct) {
      is(LOOP_MATADD_CONFIG_PACK){
        loop_being_configured.pad_j
        packed := true.B
        val dim_j0 = cmd.bits.cmd.rs2(39, 32)
        val dim_j1 = cmd.bits.cmd.rs2(47, 40)
        val dim_j2 = cmd.bits.cmd.rs2(55, 48) 
        val dim_j3 = cmd.bits.cmd.rs2(63, 56)
        loop_being_configured.a_row_offset(3) := dim_j0 + dim_j1 + dim_j2 //cmd.bits.cmd.rs2(offset_bitwidth - 2, 0)
        loop_being_configured.a_row_offset(2) := dim_j0 + dim_j1 //cmd.bits.cmd.rs2(offset_bitwidth * 2 - 3, offset_bitwidth - 1)
        loop_being_configured.a_row_offset(1) := dim_j0 //cmd.bits.cmd.rs2(offset_bitwidth * 3 - 4, offset_bitwidth * 2 - 2)
        loop_being_configured.a_row_offset(0) := 0.U //cmd.bits.cmd.rs2(offset_bitwidth * 4 - 5, offset_bitwidth * 3 - 3)
        loop_being_configured.max_j(0) := (dim_j0 >> 2).asUInt + ((dim_j0(1,0) =/= 0.U).asUInt)
        loop_being_configured.max_j(1) := (dim_j1 >> 2).asUInt + ((dim_j1(1,0) =/= 0.U).asUInt)
        loop_being_configured.max_j(2) := (dim_j2 >> 2).asUInt + ((dim_j2(1,0) =/= 0.U).asUInt)
        loop_being_configured.max_j(3) := (dim_j3 >> 2).asUInt + ((dim_j3(1,0) =/= 0.U).asUInt) 


        loop_being_configured.b_row_offset(3) := cmd.bits.cmd.rs1(offset_bitwidth - 1, 0)
        loop_being_configured.b_row_offset(2) := cmd.bits.cmd.rs1(offset_bitwidth * 2 - 1, offset_bitwidth)
        loop_being_configured.b_row_offset(1) := cmd.bits.cmd.rs1(offset_bitwidth * 3 - 1, offset_bitwidth * 2)
        loop_being_configured.b_row_offset(0) := cmd.bits.cmd.rs1(offset_bitwidth * 4 - 1, offset_bitwidth * 3)
        loop_being_configured.pad_j(0) := Mux(dim_j0(1,0) === 0.U, 0.U, (4.U - dim_j0(1,0)).asUInt) 
        loop_being_configured.pad_j(1) := Mux(dim_j1(1,0) === 0.U, 0.U, (4.U - dim_j1(1,0)).asUInt)
        loop_being_configured.pad_j(2) := Mux(dim_j2(1,0) === 0.U, 0.U, (4.U - dim_j2(1,0)).asUInt)
        loop_being_configured.pad_j(3) := Mux(dim_j3(1,0) === 0.U, 0.U, (4.U - dim_j3(1,0)).asUInt)
      }
      is(LOOP_MATADD_CONFIG_BOUNDS) {
        //loop_being_configured.max_k := cmd.bits.cmd.rs2(config_bitwidth * 3 - 1, config_bitwidth * 2)
        loop_being_configured.max_i := cmd.bits.cmd.rs2(config_bitwidth * 2 - 1, config_bitwidth)


        //loop_being_configured.pad_k := cmd.bits.cmd.rs1(config_bitwidth * 3 - 1, config_bitwidth * 2)
        loop_being_configured.pad_i := cmd.bits.cmd.rs1(config_bitwidth * 2 - 1, config_bitwidth)

        loop_being_configured.num_blk := cmd.bits.cmd.rs1(35, 32)
        when(!packed) {
          loop_being_configured.max_j(0) := cmd.bits.cmd.rs2(config_bitwidth - 1, 0)

          loop_being_configured.pad_j(0) := cmd.bits.cmd.rs1(config_bitwidth - 1, 0)

          loop_being_configured.a_row_offset(0) := 0.U
          loop_being_configured.b_row_offset(0) := 0.U

        }
      }

      is(LOOP_MATADD_CONFIG_ADDRS_AB) {
        loop_being_configured.a_dram_addr := cmd.bits.cmd.rs1
        loop_being_configured.b_dram_addr := cmd.bits.cmd.rs2
      }

      is(LOOP_MATADD_CONFIG_ADDRS_DC) {
        //loop_being_configured.d_dram_addr := cmd.bits.cmd.rs1
        loop_being_configured.c_dram_stride := cmd.bits.cmd.rs1
        loop_being_configured.c_dram_addr := cmd.bits.cmd.rs2
      }

      is(LOOP_MATADD_CONFIG_STRIDES_AB) {
        loop_being_configured.a_dram_stride := cmd.bits.cmd.rs1
        loop_being_configured.b_dram_stride := cmd.bits.cmd.rs2

        loop_being_configured.configured := true.B

        loops_configured := loops_configured + 1.U
        packed := false.B
      }

      //is(LOOP_MATADD_CONFIG_STRIDES_DC) {
        //loop_being_configured.d_dram_stride := cmd.bits.cmd.rs1
        //loop_being_configured.c_dram_stride := cmd.bits.cmd.rs2
      //}

      /*
      is(LOOP_MATADD) {
        loop_being_configured.ex_accumulate := cmd.bits.cmd.rs1(0)
        loop_being_configured.full_c := cmd.bits.cmd.rs1(1)
        loop_being_configured.low_d := cmd.bits.cmd.rs1(2)
        loop_being_configured.act := cmd.bits.cmd.rs1(8 + Activation.bitwidth - 1, 8) // TODO magic numbers

        loop_being_configured.a_ex_spad_id := cmd.bits.cmd.rs1(19, 18)
        loop_being_configured.b_ex_spad_id := cmd.bits.cmd.rs1(17, 16)
        //loop_being_configured.a_transpose := cmd.bits.cmd.rs2(0)
        //loop_being_configured.b_transpose := cmd.bits.cmd.rs2(1)
        //is_resadd := cmd.bits.cmd.rs2(2)

        loop_being_configured.configured := true.B

        loops_configured := loops_configured + 1.U
        packed := false.B
      }

       */
    }
  }

  // Wire up request signals
  val ld_d_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  //val ex_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val st_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))

  val loop_requesting_ldA_id = Mux(head_loop.lda_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldA = loops(loop_requesting_ldA_id)
  ldA.io.req.bits.max_k := loop_requesting_ldA.max_j
  ldA.io.req.bits.max_i := loop_requesting_ldA.max_i
  ldA.io.req.bits.pad_k := loop_requesting_ldA.pad_j
  ldA.io.req.bits.pad_i := loop_requesting_ldA.pad_i
  ldA.io.req.bits.dram_addr_offset := loop_requesting_ldA.a_row_offset
  ldA.io.req.bits.dram_addr := loop_requesting_ldA.a_dram_addr
  ldA.io.req.bits.dram_stride := loop_requesting_ldA.a_dram_stride
  ldA.io.req.bits.num_blk := loop_requesting_ldA.num_blk
  //ldA.io.req.bits.transpose := loop_requesting_ldA.a_transpose
  //ldA.io.req.bits.addr_start := Mux(loop_requesting_ldA.a_ex_spad_id === 0.U, loop_requesting_ldA.a_addr_start, (loop_requesting_ldA.a_ex_spad_id - 1.U) * (max_addr / concurrent_loops).U)
  ldA.io.req.bits.loop_id := loop_requesting_ldA_id
  //ldA.io.req.bits.is_resadd := is_resadd

  ldA.io.req.valid := !loop_requesting_ldA.lda_started && loop_requesting_ldA.configured

  when(ldA.io.req.fire) {
    loop_requesting_ldA.running := true.B
    loop_requesting_ldA.lda_started := true.B
  }

  val loop_requesting_ldB_id = Mux(head_loop.ldb_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldB = loops(loop_requesting_ldB_id)
  ldB.io.req.bits.max_j := loop_requesting_ldB.max_j
  ldB.io.req.bits.max_k := loop_requesting_ldB.max_i
  ldB.io.req.bits.pad_j := loop_requesting_ldB.pad_j
  ldB.io.req.bits.pad_k := loop_requesting_ldB.pad_i
  ldB.io.req.bits.dram_addr := loop_requesting_ldB.b_dram_addr
  ldB.io.req.bits.dram_stride := loop_requesting_ldB.b_dram_stride
  ldB.io.req.bits.dram_addr_offset := loop_requesting_ldB.b_row_offset
  ldB.io.req.bits.num_blk := loop_requesting_ldB.num_blk
  //ldB.io.req.bits.transpose := loop_requesting_ldB.b_transpose
  //ldB.io.req.bits.addr_end := Mux(loop_requesting_ldB.b_ex_spad_id === 0.U, loop_requesting_ldB.b_addr_end, (loop_requesting_ldB.b_ex_spad_id) * (max_addr / concurrent_loops).U)
  ldB.io.req.bits.loop_id := loop_requesting_ldB_id
  //ldB.io.req.bits.is_resadd := is_resadd

  ldB.io.req.valid := !loop_requesting_ldB.ldb_started && loop_requesting_ldB.configured

  when(ldB.io.req.fire) {
    loop_requesting_ldB.running := true.B
    loop_requesting_ldB.ldb_started := true.B
  }


  val loop_requesting_ldD_id = Mux(head_loop.ldd_started, tail_loop_id, head_loop_id)
  val loop_requesting_ldD = loops(loop_requesting_ldD_id)
  ldD.io.req.bits.max_j := loop_requesting_ldD.max_j
  ldD.io.req.bits.max_i := loop_requesting_ldD.max_i
  ldD.io.req.bits.pad_j := loop_requesting_ldD.pad_j
  ldD.io.req.bits.pad_i := loop_requesting_ldD.pad_i
  //ldD.io.req.bits.dram_addr := loop_requesting_ldD.d_dram_addr
  //ldD.io.req.bits.dram_stride := loop_requesting_ldD.d_dram_stride
  //ldD.io.req.bits.low_d := loop_requesting_ldD.low_d
  //ldD.io.req.bits.addr_start := ld_d_addr_start
  ldD.io.req.bits.loop_id := loop_requesting_ldD_id
  //ldD.io.req.bits.is_resadd := is_resadd
  ldD.io.req.bits.num_blk := loop_requesting_ldD.num_blk

  ldD.io.req.valid := !loop_requesting_ldD.ldd_started && loop_requesting_ldD.configured

  when(ldD.io.req.fire) {
    loop_requesting_ldD.running := true.B
    loop_requesting_ldD.ldd_started := true.B

    when(loop_requesting_ldD.c_dram_addr =/= 0.U) {
      ld_d_addr_start := floorAdd(ld_d_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  val loop_requesting_st_id = Mux(head_loop.st_started, tail_loop_id, head_loop_id)
  val loop_requesting_st = loops(loop_requesting_st_id)
  stC.io.req.bits.max_j := loop_requesting_st.max_j
  stC.io.req.bits.max_i := loop_requesting_st.max_i
  stC.io.req.bits.pad_j := loop_requesting_st.pad_j
  stC.io.req.bits.pad_i := loop_requesting_st.pad_i
  stC.io.req.bits.dram_addr := loop_requesting_st.c_dram_addr
  stC.io.req.bits.dram_stride := loop_requesting_st.c_dram_stride
  stC.io.req.bits.dram_addr_offset := loop_requesting_st.b_row_offset
  stC.io.req.bits.num_blk := loop_requesting_st.num_blk
  //stC.io.req.bits.full_c := loop_requesting_st.full_c
  //stC.io.req.bits.act := loop_requesting_st.act
  //stC.io.req.bits.addr_start := st_c_addr_start
  stC.io.req.bits.loop_id := loop_requesting_st_id
  //stC.io.req.bits.is_resadd := is_resadd


  //stC.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.ex_started && loop_requesting_st.configured

  when(stC.io.req.fire) {
    loop_requesting_st.running := true.B
    loop_requesting_st.st_started := true.B

    when(loop_requesting_st.c_dram_addr =/= 0.U) {
      st_c_addr_start := floorAdd(st_c_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  //when(is_resadd){
  ldA.io.req.bits.addr_start := loop_requesting_ldA.resadd_addr_start
  ldB.io.req.bits.addr_end := loop_requesting_ldB.resadd_addr_start
  ldD.io.req.bits.addr_start := loop_requesting_ldD.resadd_addr_start
  stC.io.req.bits.addr_start := loop_requesting_st.resadd_addr_start
  stC.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.configured
  //}
  // Handle completed signals
  when (ldA.io.idle && loops(ldA.io.loop_id).running && loops(ldA.io.loop_id).lda_started) {
    loops(ldA.io.loop_id).lda_completed := true.B
  }

  when (ldB.io.idle && loops(ldB.io.loop_id).running && loops(ldB.io.loop_id).ldb_started) {
    loops(ldB.io.loop_id).ldb_completed := true.B
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
  when (reset.asBool) {
    loops.zipWithIndex.foreach { case (l, i) =>
      l.reset()
      //l.a_addr_start := (i * (max_acc_addr / concurrent_loops)).U
      //l.b_addr_end := ((i+1) * (max_acc_addr / concurrent_loops)).U
      l.resadd_addr_start := (i * (max_acc_addr / concurrent_loops)).U
    }
  }
}

object LoopMatadd {
  def apply(in: DecoupledIO[GemminiCmd], ld_completed: UInt, st_completed: UInt,
            block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_sts: Int,
            max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int,
            mvin_rs2_t: MvinRs2, mvout_rs2_t: MvoutRs2)
           (implicit p: Parameters): (DecoupledIO[GemminiCmd], Bool) = {
    val mod = Module(new LoopMatadd(block_size, coreMaxAddrBits, rob_size, max_lds, max_sts,
      max_acc_addr, input_w, acc_w, dma_max_bytes,
      mvin_rs2_t, mvout_rs2_t))
    mod.io.in <> in
    mod.io.ld_completed := ld_completed
    mod.io.st_completed := st_completed
    //mod.io.ex_completed := ex_completed
    (mod.io.out, mod.io.busy)
  }

  def castDramOffset(dram_offset: UInt): UInt = {
    // Cast dram offsets to 32 bits max
    dram_offset & "hFFFFFFFF".U
  }
}

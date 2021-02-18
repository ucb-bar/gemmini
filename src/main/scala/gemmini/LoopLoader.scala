package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._

class LoopLoader(block_size: Int, coreMaxAddrBits:Int, max_addr: Int, input_w: Int, dma_max_bytes: Int)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val max_block_len = (dma_max_bytes / (block_size * input_w / 8)) max 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val busy = Output(Bool())
  })
  //queue for cmd
  val cmd = Queue(io.in)
  val is_ldloop = cmd.bits.inst.funct === LOOP_LD
  val is_ldconfig = cmd.bits.inst.funct === LOOP_LD_CONFIG_ADDRS || cmd.bits.inst.funct === Loop_LD_CONFIG_BOUNDS
  //ToDo: add these in software

  // config states
  val max_k = RegInit(0.U(iterator_bitwidth.W))
  val max_j = RegInit(0.U(iterator_bitwidth.W))
  val max_i = RegInit(0.U(iterator_bitwidth.W))

  val pad_k = RegInit(0.U(iterator_bitwidth.W))
  val pad_j = RegInit(0.U(iterator_bitwidth.W))
  val pad_i = RegInit(0.U(iterator_bitwidth.W))

  val dram_base_addr = RegInit(0.U(coreMaxAddrBits.W))
  val row_stride = RegInit(0.U(coreMaxAddrBits.W))
  //ToDo: add transpose

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val row_iterator = k //Mux(req.transpose, j, k)
  val col_iterator = j //Mux(req.transpose, k, j)
  val max_row_iterator = max_k //Mux(req.transpose, max_j, max_k)
  val max_col_iterator = max_j //Mux(req.transpose, max_k, max_j)

  val row_pad = pad_k //Mux(req.transpose, pad_j, pad_k)
  val col_pad = pad_j //Mux(req.transpose, pad_k, pad_j)

  val max_blocks = max_block_len.asUInt()
  val sp_addr_start = (max_addr/2).U // need mux with 0 (skip A)
  val dram_addr = dram_base_addr + (row_iterator * row_stride + col_iterator) * block_size.U * (input_w/8).U
  val sp_addr = sp_addr_start + (row_iterator * max_col_iterator + col_iterator) * block_size.U
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(max_row_iterator === max_row_iterator-1.U, row_pad, 0.U)

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)
  val configured = RegInit(false.B)

  //ToDo: either load A or B (for now just do with B)
  val load_cmd = Wire(new RoCCCommand())
  load_cmd := DontCare
  load_cmd.inst.funct := LOAD2_CMD
  load_cmd.rs1 := dram_addr
  load_cmd.rs2 := (rows << 48).asUInt() | (cols << 32).asUInt() | sp_addr
  io.out.valid := cmd.valid

  val is_ldloop_cmd = is_ldloop || is_ldconfig
  io.out.bits := Mux(configured, load_cmd, cmd.bits)
  io.out.bits.status := cmd.bits.status
  io.out.valid := Mux(configured, state =/= idle, cmd.valid && !is_ldloop_cmd)
  cmd.ready := Mux(is_ldloop_cmd, !configured, !io.out.ready)

  when(cmd.valid && is_ldconfig && state === idle){
    switch(cmd.bits.inst.funct){
      is(LOOP_LD_CONFIG_ADDRS){
        dram_base_addr := cmd.bits.rs1
        row_stride := cmd.bits.rs2 //Todo: add these in software
      }
      is(Loop_LD_CONFIG_BOUNDS){
        max_k := cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        max_j := cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        max_i := cmd.bits.rs2(iterator_bitwidth-1, 0)

        pad_k := cmd.bits.rs1(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        pad_j := cmd.bits.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        pad_i := cmd.bits.rs1(iterator_bitwidth-1, 0)
        configured := true.B
        state := ld
      }
    }
  }
  when(io.out.fire() && state === ld){
    // The order here is k, j, i
    // Todo: do A as well
    val j_blocks = max_blocks// Mux(req.transpose, 1.U, max_blocks)
    val k_blocks = 1.U //Mux(req.transpose, max_blocks, 1.U)

    val next_j = floorAdd(j, j_blocks, max_j)
    val next_k = floorAdd(k, k_blocks, max_k, next_j === 0.U)

    j := next_j
    k := next_k

    when (next_j === 0.U && next_k === 0.U) { //finished loading
      state := idle
      configured := false.B
    }
  }


}

object LoopLoader{
  def apply(in: DecoupledIO[RoCCCommand], block_size: Int, coreMaxAddrBits: Int, max_addr: Int, input_w: Int, dma_max_bytes: Int)
           (implicit p: Parameters): Tuple2[DecoupledIO[RoCCCommand], Bool] = {
    val lld = Module(new LoopLoader(block_size, coreMaxAddrBits, max_addr, input_w, dma_max_bytes))
    lld.io.in <> in
    (lld.io.out, lld.io.busy)
  }
}
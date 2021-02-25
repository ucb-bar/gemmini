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
    val latency = Output(UInt(iterator_bitwidth.W))
    val alert_cycle = Output(UInt(6.W))
    val pause_turn = Output(UInt(3.W))
    val pause_monitor = Input(Bool())
  })
  //queue for cmd
  val cmd = Queue(io.in)
  //val is_ldloop = cmd.bits.inst.funct === LOOP_LD
  val is_ldconfig = cmd.bits.inst.funct === LOOP_LD_CONFIG_ADDRS || cmd.bits.inst.funct === LOOP_LD_CONFIG_BOUNDS

  val pause_req = RegInit(false.B)
  val loop_tag = RegInit(false.B)
  val lock_tag = RegInit(false.B)
  when(cmd.bits.inst.funct === LOOP_LD_CONFIG_ADDRS){
    lock_tag := true.B
  } // no need to force flip once seen LOOP_LD
  when(cmd.bits.inst.funct === LOOP_WS){
    when(lock_tag){
      lock_tag := false.B
    }.otherwise{
      loop_tag := ~loop_tag //force to flip to sync with loop matmul afterwards
    }
  }
  // config states
  val latency = RegInit(0.U(iterator_bitwidth.W)) //how many cycles to push
  val alert_cycle = RegInit(0.U(6.W)) //raise flag after how much cycles?
  val pause_turn = RegInit(1.U(3.W)) // how many turns to wait to pause monitoring TL ports
  val dram_base_addr = RegInit(0.U(coreMaxAddrBits.W))
  val row_stride = RegInit(0.U(coreMaxAddrBits.W))

  val row_iterator =  RegInit(0.U(iterator_bitwidth.W))//Mux(req.transpose, j, k) //k
  val col_iterator =  RegInit(0.U(iterator_bitwidth.W))//Mux(req.transpose, k, j) //j
  val max_row_iterator = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, max_j, max_k)
  val max_col_iterator = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, max_k, max_j)

  val row_pad = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, pad_j, pad_k)
  val col_pad = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, pad_k, pad_j)

  val max_blocks = max_block_len.asUInt()
  val AB = RegInit(false.B) //false if B, true if A
  //ToDo: rotate starting address like LoopMatmul.scala
  val A_sp_addr_start = Mux(loop_tag, (max_addr/2).U, 0.U)//RegInit(0.U(log2Up(max_addr).W))
  val B_sp_addr_end = Mux(loop_tag, (max_addr - block_size).U, (max_addr/2 - block_size).U)//RegInit((max_addr/2).U(log2Up(max_addr).W))
  val sp_addr_start = Mux(AB, A_sp_addr_start, B_sp_addr_end - max_row_iterator * max_col_iterator * block_size.U + block_size.U) // Todo: need mux with 0 (skip A)
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

  val conflict_monitor = !((alert_cycle === 0.U) || (latency === 0.U))

  //ToDo: either load A or B (for now just do with B)
  val load_cmd = Wire(new RoCCCommand())
  load_cmd := DontCare
  load_cmd.inst.funct := Mux(AB, LOAD_CMD, LOAD2_CMD)
  load_cmd.rs1 := dram_addr
  load_cmd.rs2 :=  (conflict_monitor << 63).asUInt() | (rows << 48).asUInt() | (cols << 32).asUInt() | sp_addr
  io.busy := cmd.valid || configured
  io.alert_cycle := alert_cycle
  io.latency := latency
  io.pause_turn := pause_turn

  // fix loop_ws command
  val loop_ws_state = RegInit(idle)
  val is_loop_ws_addr = cmd.bits.inst.funct === LOOP_WS_CONFIG_ADDRS_AB
  val fixed_loop_cmd = Wire(new RoCCCommand())
  fixed_loop_cmd := DontCare
  fixed_loop_cmd.inst.funct := LOOP_WS_CONFIG_ADDRS_AB
  fixed_loop_cmd.rs1 := Mux(AB, 0.U, cmd.bits.rs1)
  fixed_loop_cmd.rs2 := Mux(AB, cmd.bits.rs2, 0.U)

  val unlock_monitor = RegInit(0.U(4.W))
  val unlock_cycle = RegInit(3.U(4.W))
  unlock_monitor := floorAdd(unlock_monitor, 1.U, unlock_cycle, pause_req && is_loop_ws_addr & lock_tag && cmd.fire())
  when(!pause_req){
    unlock_monitor := 0.U
  }
  when(!configured){
    pause_req := io.pause_monitor
  }

  val unlock = unlock_monitor === unlock_cycle - 1.U // ToDo: change this number

  io.out.bits := Mux(configured, load_cmd, Mux(lock_tag && is_loop_ws_addr && (!pause_req || unlock) && conflict_monitor, fixed_loop_cmd, cmd.bits))
  io.out.bits.status := cmd.bits.status
  io.out.valid := Mux(configured, state =/= idle, cmd.valid && !is_ldconfig)
  cmd.ready := Mux(is_ldconfig, !configured, !configured && io.out.ready)

  when(cmd.valid && is_ldconfig && state === idle && (!pause_req || unlock)){
    switch(cmd.bits.inst.funct){
      is(LOOP_LD_CONFIG_BOUNDS){
        pause_turn := cmd.bits.rs2(iterator_bitwidth * 3 + 12, iterator_bitwidth * 3 + 10)
        alert_cycle := cmd.bits.rs2(iterator_bitwidth * 3 + 5, iterator_bitwidth * 3)
        latency := cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2) //ToDo: give this to DMA
        unlock_cycle := cmd.bits.rs2(iterator_bitwidth * 3 + 9, iterator_bitwidth * 3 + 6)
        max_col_iterator := cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        max_row_iterator := cmd.bits.rs2(iterator_bitwidth-1, 0)

        AB := cmd.bits.rs1(63)
        col_pad := cmd.bits.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        row_pad := cmd.bits.rs1(iterator_bitwidth-1, 0)
      }
      is(LOOP_LD_CONFIG_ADDRS){
        dram_base_addr := cmd.bits.rs1
        row_stride := cmd.bits.rs2
        when (conflict_monitor){ // if latency == 0, don't unroll
          configured := true.B
          state := ld
        }
      }
    }
  }
  when(io.out.fire() && state === ld){
    val row_blocks = 1.U
    val col_blocks = max_blocks

    val next_col = floorAdd(col_iterator, col_blocks, max_col_iterator)
    val next_row = floorAdd(row_iterator, row_blocks, max_row_iterator, next_col === 0.U)

    row_iterator := next_row
    col_iterator := next_col

    when (next_row === 0.U && next_col === 0.U) { //finished loading
      state := idle
      configured := false.B
      loop_tag := ~loop_tag
    }
  }

}

object LoopLoader{
  def apply(in: DecoupledIO[RoCCCommand], pause_monitor: Bool, block_size: Int, coreMaxAddrBits: Int, max_addr: Int, input_w: Int, dma_max_bytes: Int)
           (implicit p: Parameters): Tuple5[DecoupledIO[RoCCCommand], Bool, UInt, UInt, UInt] = {
    val lld = Module(new LoopLoader(block_size, coreMaxAddrBits, max_addr, input_w, dma_max_bytes))
    lld.io.in <> in
    lld.io.pause_monitor <> pause_monitor
    (lld.io.out, lld.io.busy, lld.io.latency, lld.io.alert_cycle, lld.io.pause_turn)
  }
}
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
    val alert_cycle = Output(UInt(7.W))
    val pause_turn = Output(UInt(3.W))
    val pause_monitor = Input(Bool())
  })
  //queue for cmd
  val cmd = Queue(io.in)
  //val is_ldloop = cmd.bits.inst.funct === LOOP_LD
  val is_matmul_ldconfig = cmd.bits.inst.funct === LOOP_LD_CONFIG_ADDRS || cmd.bits.inst.funct === LOOP_LD_CONFIG_BOUNDS
  val is_conv_ldconfig = cmd.bits.inst.funct === LOOP_CONV_LD_CONFIG_ADDRS || cmd.bits.inst.funct === LOOP_CONV_LD_CONFIG_BOUNDS

  val pause_req = RegInit(false.B)
  val lock_tag = RegInit(false.B)
  val is_conv = RegInit(false.B)
  // for switching between conv and matmul
  val loop_tag_conv = RegInit(false.B)
  val loop_tag_matmul = RegInit(false.B)
  val loop_tag = Mux(is_conv, loop_tag_conv, loop_tag_matmul)

  when(cmd.bits.inst.funct === LOOP_LD_CONFIG_ADDRS || cmd.bits.inst.funct === LOOP_CONV_LD_CONFIG_ADDRS){
    lock_tag := true.B
  } // no need to force flip once seen LOOP_LD
  when(cmd.bits.inst.funct === LOOP_WS || cmd.bits.inst.funct === LOOP_CONV_WS){
    when(lock_tag){
      lock_tag := false.B
    }.otherwise{
      when(is_conv){
        loop_tag_conv := ~loop_tag_conv
      }.otherwise{
        loop_tag_matmul := ~loop_tag_matmul
      } //force to flip to sync with loop matmul afterwards
    }
  }
  // config states
  val latency = RegInit(0.U(iterator_bitwidth.W)) //how many cycles to push
  val multiply_factor = RegInit(1.U(2.W))
  val alert_cycle = RegInit(0.U(7.W)) //raise flag after how much cycles?
  val pause_turn = RegInit(1.U(3.W)) // how many turns to wait to pause monitoring TL ports
  val dram_base_addr = RegInit(0.U(coreMaxAddrBits.W))
  val row_stride = RegInit(0.U(coreMaxAddrBits.W))

  val row_iterator =  RegInit(0.U(iterator_bitwidth.W))//Mux(req.transpose, j, k) //k
  val col_iterator =  RegInit(0.U(iterator_bitwidth.W))//Mux(req.transpose, k, j) //j
  val max_row_iterator = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, max_j, max_k)
  val max_col_iterator = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, max_k, max_j)

  val row_pad = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, pad_j, pad_k)
  val col_pad = Reg(UInt(iterator_bitwidth.W)) //Mux(req.transpose, pad_k, pad_j)

  //conv parameters
  val out_channels = RegInit(0.U(16.W))
  val in_channels = RegInit(0.U(16.W))
  val kernel_dim = RegInit(0.U(4.W))
  val krows = RegInit(0.U(4.W))
  val kcols = RegInit(0.U(4.W))
  val kchs = RegInit(0.U(16.W))
  val ochs = RegInit(0.U(16.W))

  // conv Iterators
  val och = RegInit(0.U(16.W))
  val krow = RegInit(0.U(4.W))
  val kcol = RegInit(0.U(4.W))
  val kch = RegInit(0.U(16.W))

  val max_blocks = max_block_len.asUInt()
  val AB = RegInit(false.B) //false if B, true if A
  val profile = RegInit(false.B)
  //ToDo: rotate starting address like LoopMatmul.scala
  val A_sp_addr_start = Mux(loop_tag, (max_addr/2).U, 0.U)//RegInit(0.U(log2Up(max_addr).W))
  val B_sp_addr_end = Mux(loop_tag, (max_addr - block_size).U, (max_addr/2 - block_size).U)//RegInit((max_addr/2).U(log2Up(max_addr).W))
  //for conv
  val depthwise = RegInit(false.B)
  val out_channel_stride = RegInit(0.U(coreMaxAddrBits.W))
  val max_ochs_per_mvin = Mux(ochs < (max_block_len * block_size).U, ochs, (max_block_len * block_size).U)
  val out_channels_per_bank = WireInit(0.U(8.W))
  out_channels_per_bank := ochs / block_size.U +& (ochs % block_size.U =/= 0.U)
  val B_rows = out_channels_per_bank * kcols * krows * kchs
  //val addr_start = B_sp_addr_end - B_rows + block_size.U

  val sp_addr_start = Mux(is_conv, B_sp_addr_end - B_rows + block_size.U,
    Mux(AB, A_sp_addr_start, B_sp_addr_end - max_row_iterator * max_col_iterator * block_size.U + block_size.U)) // Todo: need mux with 0 (skip A)
  val conv_dram_addr = Mux(depthwise, dram_base_addr +& ((krow*kernel_dim +& kcol +& kch) * out_channel_stride +& och) * (input_w/8).U, dram_base_addr +& ((krow*kernel_dim*in_channels +& kcol*in_channels +& kch) * out_channel_stride +& och) * (input_w/8).U)
  val dram_addr = Mux(!is_conv, dram_base_addr + (row_iterator * row_stride + col_iterator) * block_size.U * (input_w/8).U,
    conv_dram_addr)
  val sp_addr = sp_addr_start + Mux(is_conv, (och / block_size.U) * krows * kcols * kchs + krow * kcols * kchs + kcol * kchs + kch,
    (row_iterator * max_col_iterator + col_iterator) * block_size.U)
  val blocks = Mux(col_iterator + max_blocks <= max_col_iterator, max_blocks, max_col_iterator-col_iterator)
  val cols = (blocks * block_size.U) - Mux(col_iterator + blocks >= max_col_iterator, col_pad, 0.U)
  val rows = block_size.U - Mux(max_row_iterator === max_row_iterator-1.U, row_pad, 0.U)
  // for conv rows and cols
  val J = Mux(ochs - och > max_ochs_per_mvin, max_ochs_per_mvin, ochs - och)
  val K = Mux(kchs - kch > block_size.U, block_size.U, kchs - kch)

  object State extends ChiselEnum {
    val idle, config, ld = Value //added config for conv
  }
  import State._
  val state = RegInit(idle)
  val configured = RegInit(false.B)

  val unlock_monitor = RegInit(0.U(4.W))
  val unlock_cycle = RegInit(0.U(4.W))
  val enable_bubble = RegInit(false.B) // enable monitoring for cache hits
  val conflict_monitor = !(unlock_cycle === 0.U)//!((alert_cycle === 0.U) || (latency === 0.U))
  val conflict_monitor_start = conflict_monitor && Mux(is_conv, (och === 0.U && kch === 0.U && kcol === 0.U && krow === 0.U), (row_iterator === 0.U && col_iterator === 0.U)) && (state === ld) //ToDo: with conv
  val conflict_monitor_end = conflict_monitor && Mux(is_conv, (kch + block_size.U >= kchs && kcol === kcols - 1.U && krow === krows - 1.U && och + max_ochs_per_mvin >= ochs),
    (row_iterator === max_row_iterator - 1.U && col_iterator >= max_col_iterator - max_blocks)) && (state === ld)


  val profile_hit = profile && (pause_turn =/= 0.U)
  val profile_start = profile_hit && (row_iterator === 0.U && col_iterator === 0.U)
  val profile_end = profile_hit && (row_iterator === max_row_iterator - 1.U && col_iterator >= max_col_iterator - max_blocks)
  //ToDo: either load A or B (for now just do with B)
  val load_cmd = Wire(new RoCCCommand())
  load_cmd := DontCare
  load_cmd.inst.funct := Mux(AB, LOAD_CMD, LOAD2_CMD)
  load_cmd.rs1 := dram_addr
  load_cmd.rs2 :=  ((conflict_monitor && enable_bubble) << 63).asUInt() | (conflict_monitor_end << 62).asUInt() | (conflict_monitor_start << 61).asUInt() | (rows << 48).asUInt() | (profile_hit << 47).asUInt() | (profile_end << 46).asUInt() | (profile_start << 45).asUInt() | (cols << 32).asUInt() | sp_addr

  //for conv
  val MVIN_SCALE_IDENTITY = 0x3f800000.U // TODO get this from configs somehow
  val weight_spad_stride = krows * kcols * kchs
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U).asUInt() | (weight_spad_stride << 16.U).asUInt() | (1.U << 3).asUInt() | 1.U
  config_cmd.rs2 := out_channel_stride * (input_w/8).U
  //for conv
  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD2_CMD // for now, only weight
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := ((conflict_monitor && enable_bubble) << 63).asUInt() | (conflict_monitor_end << 62).asUInt() | (conflict_monitor_start << 61).asUInt() | (K << 48.U).asUInt() | (J << 32.U).asUInt() | sp_addr

  //val expected_tl_req = (max_addr / (2*2*max_block_len)).asUInt()
  io.busy := cmd.valid || configured
  io.alert_cycle := Mux(alert_cycle === 0.U, multiply_factor + 1.U, alert_cycle * (multiply_factor + 1.U)) //multiply for longer alert cycle when cache miss
  io.latency := latency//Mux(enable_bubble, latency, 1.U) // latency
  //enable_bubble := (latency =/= 0.U) //if latency == 0, disable bubble
  // not enable bubble (loopld+FSM without bubble)
  io.pause_turn := pause_turn
  // fix loop_ws command
  val loop_ws_state = RegInit(idle)
  val is_loop_ws_addr = (cmd.bits.inst.funct === LOOP_WS_CONFIG_ADDRS_AB || cmd.bits.inst.funct === LOOP_CONV_WS_CONFIG_5) // for now, only weight for conv
  val fixed_loop_cmd = Wire(new RoCCCommand())
  fixed_loop_cmd := DontCare
  fixed_loop_cmd.inst.funct := cmd.bits.inst.funct//LOOP_WS_CONFIG_ADDRS_AB
  fixed_loop_cmd.rs1 := Mux(cmd.bits.inst.funct === LOOP_CONV_WS_CONFIG_5, 0.U, Mux(AB, 0.U, cmd.bits.rs1)) //if conv, weight
  fixed_loop_cmd.rs2 := Mux(is_conv, cmd.bits.rs2, Mux(AB, cmd.bits.rs2, 0.U)) //for now, not do input for conv

  unlock_monitor := floorAdd(unlock_monitor, 1.U, unlock_cycle + pause_turn - 1.U, pause_req && is_loop_ws_addr & lock_tag && cmd.fire())
  when(!pause_req){
    unlock_monitor := 0.U
  }
  //when(!configured){
  when((cmd.bits.inst.funct === LOOP_LD_CONFIG_BOUNDS || cmd.bits.inst.funct === LOOP_CONV_LD_CONFIG_BOUNDS) && cmd.valid){
    pause_req := io.pause_monitor
  }

  val unlock = unlock_monitor + 1.U >= unlock_cycle // ToDo: change this number

  io.out.bits := Mux(configured, Mux(is_conv, Mux(state === config, config_cmd, mvin_cmd), load_cmd),
    Mux(lock_tag && is_loop_ws_addr && (!pause_req || unlock) && (conflict_monitor || profile), fixed_loop_cmd, cmd.bits))
  io.out.bits.status := cmd.bits.status
  io.out.valid := Mux(configured, state =/= idle, cmd.valid && !is_matmul_ldconfig && !is_conv_ldconfig)
  cmd.ready := Mux(is_matmul_ldconfig || is_conv_ldconfig, !configured, !configured && io.out.ready)

//  when(cmd.valid && is_ldconfig && state === idle && (!pause_req || unlock)){
  when(cmd.valid && is_matmul_ldconfig && state === idle){
    switch(cmd.bits.inst.funct){
      is(LOOP_LD_CONFIG_BOUNDS){
        enable_bubble := cmd.bits.rs2(63) //diable: just loop B without bubble insertion
        multiply_factor := cmd.bits.rs2(62, 61)
        pause_turn := cmd.bits.rs2(iterator_bitwidth * 3 + 12, iterator_bitwidth * 3 + 10)
        alert_cycle := cmd.bits.rs2(iterator_bitwidth * 3 + 5, iterator_bitwidth * 3 - 1)
        latency := cmd.bits.rs2(iterator_bitwidth * 3 - 2, iterator_bitwidth * 2) //ToDo: give this to DMA
        unlock_cycle := cmd.bits.rs2(iterator_bitwidth * 3 + 9, iterator_bitwidth * 3 + 6)
        max_col_iterator := cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        max_row_iterator := cmd.bits.rs2(iterator_bitwidth-1, 0)

        AB := cmd.bits.rs1(63)
        profile := cmd.bits.rs1(62) //added for profiling cache behavior
        col_pad := cmd.bits.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        row_pad := cmd.bits.rs1(iterator_bitwidth-1, 0)
        is_conv := false.B
      }
      is(LOOP_LD_CONFIG_ADDRS){
        when(!pause_req || unlock) {
          dram_base_addr := cmd.bits.rs1
          row_stride := cmd.bits.rs2
          when(conflict_monitor || profile) { // if latency == 0, don't unroll
            configured := true.B
            state := ld
          }.otherwise {
            loop_tag_matmul := ~loop_tag_matmul
          }
        }
      }
    }
  }.elsewhen(cmd.valid && is_conv_ldconfig && state === idle){
    switch(cmd.bits.inst.funct){
      is(LOOP_CONV_LD_CONFIG_BOUNDS){
        enable_bubble := cmd.bits.rs2(63) //diable: just loop B without bubble insertion
        multiply_facor := cmd.bits.rs2(62, 61)
        pause_turn := cmd.bits.rs2(60, 58)
        unlock_cycle := cmd.bits.rs2(57, 54)
        alert_cycle := cmd.bits.rs2(53, 47)
        latency := cmd.bits.rs2(46, 32) //ToDo: give this to DMA
        kernel_dim := cmd.bits.rs2(15, 0)//can code more if needed

        krows := cmd.bits.rs1(63, 48)
        kcols := cmd.bits.rs1(47, 32)
        kchs := cmd.bits.rs1(31, 16)
        ochs := cmd.bits.rs1(15, 0)
        is_conv := true.B

        // initialize for safety
        krow := 0.U
        kcol := 0.U
        kch := 0.U
        och := 0.U
      }
      is(LOOP_CONV_LD_CONFIG_ADDRS){
        when(!pause_req || unlock) {
          dram_base_addr := cmd.bits.rs1
          out_channel_stride := cmd.bits.rs2(47, 32)
          depthwise := cmd.bits.rs2(63)
          out_channels := cmd.bits.rs2(31, 16)
          in_channels := cmd.bits.rs2(15, 0)
          //can code more
          when(conflict_monitor) {
            configured := true.B
            state := config // for conv, idle -> config -> ld
          }.otherwise{
            loop_tag_conv := ~loop_tag_conv
          }
        }
      }
    }
  }


  when(io.out.fire() && state === ld) {
    when(!is_conv) {
      //matmul loop
      val row_blocks = 1.U
      val col_blocks = max_blocks

      val next_col = floorAdd(col_iterator, col_blocks, max_col_iterator)
      val next_row = floorAdd(row_iterator, row_blocks, max_row_iterator, next_col === 0.U)

      row_iterator := next_row
      col_iterator := next_col

      when(next_row === 0.U && next_col === 0.U) { //finished loading
        state := idle
        configured := false.B
        loop_tag_matmul := ~loop_tag_matmul
      }
    }.otherwise{
      //conv loop
      val next_kch = floorAdd(kch, block_size.U, kchs)
      val next_kcol = floorAdd(kcol, 1.U, kcols, next_kch === 0.U)
      val next_krow = floorAdd(krow, 1.U, krows, next_kcol === 0.U && next_kch === 0.U)
      val next_och = floorAdd(och, max_ochs_per_mvin, ochs, next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)

      kch := next_kch
      kcol := next_kcol
      krow := next_krow
      och := next_och

      when(next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U){ //finished loading
        state := idle
        configured := false.B
        loop_tag_conv := ~loop_tag_conv
      }
    }
  }.elsewhen(io.out.fire() && state === config){ //for conv config
    state := ld
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

package gemmini

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.ClockGate
import freechips.rocketchip.tilelink.TLIdentityNode
import GemminiISA._
import Util._

class GemminiCmd(rob_entries: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val rob_id = UDValid(UInt(log2Up(rob_entries).W))
  val from_matmul_fsm = Bool()
  val from_conv_fsm = Bool()
}

class Gemmini[T <: Data : Arithmetic, U <: Data, V <: Data](val config: GemminiArrayConfig[T, U, V])
                                     (implicit p: Parameters)
  extends LazyRoCC (
    opcodes = config.opcodes,
    nPTWPorts = if (config.use_shared_tlb) 1 else 2) {

  Files.write(Paths.get(config.headerFilePath), config.generateHeader().getBytes(StandardCharsets.UTF_8))
  if (System.getenv("GEMMINI_ONLY_GENERATE_GEMMINI_H") == "1") {
    System.exit(1)
  }

  val xLen = p(XLen)
  val spad = LazyModule(new Scratchpad(config))

  override lazy val module = new GemminiModule(this)
  override lazy val desiredName = if (config.inputType.isInstanceOf[gemmini.Float]) "GemminiFP" else "Gemmini"
  override val tlNode = if (config.use_dedicated_tl_port) spad.id_node else TLIdentityNode()
  override val atlNode = if (config.use_dedicated_tl_port) TLIdentityNode() else spad.id_node

  val node = if (config.use_dedicated_tl_port) tlNode else atlNode
}

class GemminiModule[T <: Data: Arithmetic, U <: Data, V <: Data]
    (outer: Gemmini[T, U, V])
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val ext_mem_io = if (use_shared_ext_mem) Some(IO(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))) else None
  ext_mem_io.foreach(_ <> outer.spad.module.io.ext_mem.get)

  val tagWidth = 32

  // Counters
  /*
  val counters = Module(new CounterController(outer.config.num_counter, outer.xLen))
  io.resp <> counters.io.out  // Counter access command will be committed immediately
  counters.io.event_io.external_values(0) := 0.U
  counters.io.event_io.event_signal(0) := false.B
  counters.io.in.valid := false.B
  counters.io.in.bits := DontCare
  counters.io.event_io.collect(spad.module.io.counter)

   */

  // TLB
  implicit val edge = outer.spad.id_node.edges.out.head
  val tlb = Module(new FrontendTLB(2, tlb_size, dma_maxbytes, use_tlb_register_filter, use_firesim_simulation_counters, use_shared_tlb))
  (tlb.io.clients zip outer.spad.module.io.tlb).foreach(t => t._1 <> t._2)

  tlb.io.exp.foreach(_.flush_skip := false.B)
  tlb.io.exp.foreach(_.flush_retry := false.B)

  io.ptw <> tlb.io.ptw

  //counters.io.event_io.collect(tlb.io.counter)

  spad.module.io.flush := tlb.io.exp.map(_.flush()).reduce(_ || _)

  val clock_en_reg = RegInit(true.B) // clock gating entire gemmini
  val gated_clock = if (clock_gate) ClockGate(clock, clock_en_reg, "gemmini_clock_gate") else clock
  outer.spad.module.clock := gated_clock

  // gated clock for gemv + gemm
  val vega_clock_en = RegInit(true.B)
  val gemmini_clock_en = RegInit(true.B)
  val vega_gated_clock = if(vega_clock_gate) ClockGate(gated_clock, vega_clock_en, "vega_clock_gate") else gated_clock
  val gemmini_gated_clock = if(vega_clock_gate) ClockGate(gated_clock, gemmini_clock_en, "vega_gemmini_clock_gate") else gated_clock


  val reservation_station = withClock (gated_clock) { Module(new ReservationStation(outer.config, new GemminiCmd(reservation_station_entries))) }
  //counters.io.event_io.collect(reservation_station.io.counter)

  when (io.cmd.valid && io.cmd.bits.inst.funct === CLKGATE_EN && !io.busy) {
    clock_en_reg := io.cmd.bits.rs1(0)
    gemmini_clock_en := io.cmd.bits.rs1(1)
    vega_clock_en := io.cmd.bits.rs1(2)
  }

  val raw_cmd_q = Module(new Queue(new GemminiCmd(reservation_station_entries), entries = 2))
  raw_cmd_q.io.enq.valid := io.cmd.valid
  io.cmd.ready := raw_cmd_q.io.enq.ready
  raw_cmd_q.io.enq.bits.cmd := io.cmd.bits
  raw_cmd_q.io.enq.bits.rob_id := DontCare
  raw_cmd_q.io.enq.bits.from_conv_fsm := false.B
  raw_cmd_q.io.enq.bits.from_matmul_fsm := false.B

  val raw_cmd = raw_cmd_q.io.deq

  val max_lds = reservation_station_entries_ld
  val max_exs = reservation_station_entries_ex
  val max_sts = reservation_station_entries_st

  val (conv_cmd, loop_conv_unroller_busy) = if (has_loop_conv) withClock (gated_clock) { LoopConv(raw_cmd, reservation_station.io.conv_ld_completed, reservation_station.io.conv_st_completed, reservation_station.io.conv_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes,
    new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits), new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new ConfigMvoutRs2(acc_scale_t_bits, 32), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ConfigExRs1(acc_scale_t_bits), new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    has_training_convs, has_max_pool, has_first_layer_optimizations, has_dw_convs) }
  else (raw_cmd, false.B)

  val loop_matmul_ld_completed = Mux(reservation_station.io.is_vega, false.B, reservation_station.io.matmul_ld_completed)
  val loop_matmul_ex_completed = Mux(reservation_station.io.is_vega, false.B, reservation_station.io.matmul_ex_completed)
  val loop_matmul_st_completed = Mux(reservation_station.io.is_vega, false.B, reservation_station.io.matmul_st_completed)

  val (loop_ws_cmd, loop_matmul_unroller_busy) = withClock (gemmini_gated_clock) { LoopMatmul(if (has_loop_conv) conv_cmd else raw_cmd, loop_matmul_ld_completed, loop_matmul_st_completed, loop_matmul_ex_completed,
    gemmini_clock_en, meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes, has_vega && vega_clock_gate, new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)) }

  val loop_vega_ld_completed = Mux(!reservation_station.io.is_vega, false.B, reservation_station.io.matmul_ld_completed)
  val loop_vega_ex_completed = Mux(!reservation_station.io.is_vega, false.B, reservation_station.io.matmul_ex_completed)
  val loop_vega_st_completed = Mux(!reservation_station.io.is_vega, false.B, reservation_station.io.matmul_st_completed)

  // only allow accessing these banks
  //val vega_loop_ws_cmd = Mux(gemmini_clock_en, loop_ws_cmd, (if (has_loop_conv) conv_cmd else raw_cmd))

  /*
  val vega_loop_ws_cmd = WireInit(loop_ws_cmd)
  if(has_vega && vega_clock_gate) {
    val loop_ws_arb = Module(new Arbiter(new GemminiCmd(reservation_station_entries), 2))
    loop_ws_arb.io.in(0) <> loop_ws_cmd
    loop_ws_arb.io.in(1) <> (if (has_loop_conv) conv_cmd else raw_cmd)
    when(gemmini_clock_en && !vega_clock_en) {
      loop_ws_arb.io.in(0).valid := false.B
    }.otherwise{
      loop_ws_arb.io.in(1).valid := false.B
    }
    vega_loop_ws_cmd <> loop_ws_arb.io.out
  }

   */

  val (loop_cmd, loop_vega_unroller_busy) = if (has_vega) withClock (vega_gated_clock) { VegaLoopMatmul(loop_ws_cmd, loop_vega_ld_completed, loop_vega_st_completed, loop_vega_ex_completed,
    vega_clock_en, meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, vega_sp_banks * sp_bank_entries, vega_acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes, has_vega && vega_clock_gate, new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)) } else (loop_ws_cmd, false.B)

  /*
  val final_loop_cmd = WireInit(loop_cmd)
  if(has_vega && vega_clock_gate){
    val loop_cmd_arb = Module(new Arbiter(new GemminiCmd(reservation_station_entries), 2))
    loop_cmd_arb.io.in(0) <> loop_cmd
    loop_cmd_arb.io.in(1) <> loop_ws_cmd
    when(gemmini_clock_en && !vega_clock_en) {
      loop_cmd_arb.io.in(0).valid := false.B
    }.otherwise{
      loop_cmd_arb.io.in(1).valid := false.B
    }
    final_loop_cmd <> loop_cmd_arb.io.out
  }
  dontTouch(final_loop_cmd)

   */
  //dontTouch(vega_loop_ws_cmd)
  //dontTouch(loop_ws_cmd)
  //dontTouch(loop_cmd)
  //dontTouch(raw_cmd)
  val unrolled_cmd = Queue(loop_cmd)
  unrolled_cmd.ready := false.B
  //counters.io.event_io.connectEventSignal(CounterEvent.LOOP_MATMUL_ACTIVE_CYCLES, loop_matmul_unroller_busy)

  // Wire up controllers to ROB
  reservation_station.io.alloc.valid := false.B
  reservation_station.io.alloc.bits := unrolled_cmd.bits

  /*
  //-------------------------------------------------------------------------
  // finish muxing control signals to rob (risc) or tiler (cisc)
  when (raw_cmd.valid && is_cisc_funct && !rob.io.busy) {
    is_cisc_mode       := true.B
    raw_cisc_cmd.valid := true.B
    raw_cmd.ready      := raw_cisc_cmd.ready
  }
  .elsewhen (raw_cmd.valid && !is_cisc_funct && !tiler.io.busy) {
    is_cisc_mode       := false.B
    raw_risc_cmd.valid := true.B
    raw_cmd.ready      := raw_risc_cmd.ready
  }
  */

  //=========================================================================
  // Controllers
  //=========================================================================
  val load_controller = withClock (gated_clock) { Module(new LoadController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val store_controller = withClock (gated_clock) { Module(new StoreController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val ex_controller = withClock (gemmini_gated_clock) { Module(new ExecuteController(xLen, tagWidth, outer.config)) }

  //val vega_ex_controller = if (has_vega) {withClock(gated_clock) { Module(new VegaExecuteController(xLen, tagWidth, outer.config))}} else None
  //val vega_ex_controller = withClock(gated_clock) { Module(new VegaExecuteController(xLen, tagWidth, outer.config))}

  //counters.io.event_io.collect(load_controller.io.counter)
  //counters.io.event_io.collect(store_controller.io.counter)
  //counters.io.event_io.collect(ex_controller.io.counter)
  //counters.io.event_io.collect(vega_ex_controller.io.counter)

  /*
  tiler.io.issue.load.ready := false.B
  tiler.io.issue.store.ready := false.B
  tiler.io.issue.exec.ready := false.B
  */

  reservation_station.io.issue.ld.ready := false.B
  reservation_station.io.issue.st.ready := false.B
  reservation_station.io.issue.ex.ready := false.B

  /*
  when (is_cisc_mode) {
    load_controller.io.cmd  <> tiler.io.issue.load
    store_controller.io.cmd <> tiler.io.issue.store
    ex_controller.io.cmd  <> tiler.io.issue.exec
  }
  .otherwise {
    load_controller.io.cmd.valid := rob.io.issue.ld.valid
    rob.io.issue.ld.ready := load_controller.io.cmd.ready
    load_controller.io.cmd.bits.cmd := rob.io.issue.ld.cmd
    load_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.ld.cmd.inst.funct
    load_controller.io.cmd.bits.rob_id.push(rob.io.issue.ld.rob_id)

    store_controller.io.cmd.valid := rob.io.issue.st.valid
    rob.io.issue.st.ready := store_controller.io.cmd.ready
    store_controller.io.cmd.bits.cmd := rob.io.issue.st.cmd
    store_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.st.cmd.inst.funct
    store_controller.io.cmd.bits.rob_id.push(rob.io.issue.st.rob_id)

    ex_controller.io.cmd.valid := rob.io.issue.ex.valid
    rob.io.issue.ex.ready := ex_controller.io.cmd.ready
    ex_controller.io.cmd.bits.cmd := rob.io.issue.ex.cmd
    ex_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.ex.cmd.inst.funct
    ex_controller.io.cmd.bits.rob_id.push(rob.io.issue.ex.rob_id)
  }
  */

  load_controller.io.cmd.valid := reservation_station.io.issue.ld.valid
  reservation_station.io.issue.ld.ready := load_controller.io.cmd.ready
  load_controller.io.cmd.bits := reservation_station.io.issue.ld.cmd
  load_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ld.rob_id)

  store_controller.io.cmd.valid := reservation_station.io.issue.st.valid
  reservation_station.io.issue.st.ready := store_controller.io.cmd.ready
  store_controller.io.cmd.bits := reservation_station.io.issue.st.cmd
  store_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.st.rob_id)


  spad.module.io.dma.read <> load_controller.io.dma
  spad.module.io.dma.write <> store_controller.io.dma
  ex_controller.io.cmd.bits := reservation_station.io.issue.ex.cmd
  ex_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ex.rob_id)

  val ex_controller_bits = WireInit(ex_controller.io.completed.bits)
  val ex_controller_valid = WireInit(ex_controller.io.completed.valid)
  val ex_controller_busy = WireInit(ex_controller.io.busy)

  if(has_vega) {
    val vega_ex_controller = withClock(vega_gated_clock) { Module(new VegaExecuteController(xLen, tagWidth, outer.config))}
    vega_ex_controller.io.cmd.bits := reservation_station.io.issue.ex.cmd
    vega_ex_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ex.rob_id)
    when(reservation_station.io.is_vega){
      ex_controller.io.cmd.valid := false.B
      vega_ex_controller.io.cmd.valid :=  reservation_station.io.issue.ex.valid
      reservation_station.io.issue.ex.ready := vega_ex_controller.io.cmd.ready

      for (i <- 0 until vega_sp_banks) {
        vega_ex_controller.io.srams.read(i) <> spad.module.io.srams.read(i)
        vega_ex_controller.io.srams.write(i) <> spad.module.io.srams.write(i)
      }
      for (i <- 0 until vega_acc_banks){
        spad.module.io.acc.read_req(i) <> vega_ex_controller.io.acc.read_req(i)
        vega_ex_controller.io.acc.read_resp(i) <> spad.module.io.acc.read_resp(i)
        vega_ex_controller.io.acc.write(i) <> spad.module.io.acc.write(i)
      }
      for(i <- vega_acc_banks until acc_banks){
        spad.module.io.acc.read_req(i) := DontCare
        spad.module.io.acc.read_req(i).valid := false.B
        spad.module.io.acc.write(i) := DontCare
        spad.module.io.acc.write(i).valid := false.B
	spad.module.io.acc.read_resp(i).ready := false.B
      }
      for(i <- vega_sp_banks until sp_banks){
        spad.module.io.srams.write(i) := DontCare
        spad.module.io.srams.read(i) := DontCare
        spad.module.io.srams.read(i).req.valid := false.B
        spad.module.io.srams.write(i).en := false.B
      }

      ex_controller.io.srams.read := DontCare
      ex_controller.io.srams.write := DontCare
      ex_controller.io.acc.read_resp := DontCare
      ex_controller.io.acc.write := DontCare
      for (i <- 0 until sp_banks) {
        ex_controller.io.srams.read(i).resp.valid := false.B
        ex_controller.io.srams.read(i).req.ready := false.B
      }
      for(i <- 0 until acc_banks) {
        ex_controller.io.acc.read_resp(i).valid := false.B
        ex_controller.io.acc.read_req(i).ready := false.B
        ex_controller.io.acc.write(i).ready := false.B
      }
      ex_controller_bits := vega_ex_controller.io.completed.bits
      ex_controller_valid := vega_ex_controller.io.completed.valid
      ex_controller_busy := vega_ex_controller.io.busy
    }.otherwise{
      ex_controller.io.cmd.valid :=  reservation_station.io.issue.ex.valid
      vega_ex_controller.io.cmd.valid := false.B
      reservation_station.io.issue.ex.ready := ex_controller.io.cmd.ready

      ex_controller.io.srams.read <> spad.module.io.srams.read
      ex_controller.io.srams.write <> spad.module.io.srams.write
      spad.module.io.acc.read_req <> ex_controller.io.acc.read_req
      ex_controller.io.acc.read_resp <> spad.module.io.acc.read_resp
      ex_controller.io.acc.write <> spad.module.io.acc.write

      vega_ex_controller.io.srams.read := DontCare
      vega_ex_controller.io.srams.write := DontCare
      vega_ex_controller.io.acc.read_resp := DontCare
      vega_ex_controller.io.acc.write := DontCare
      for (i <- 0 until vega_sp_banks) {
        vega_ex_controller.io.srams.read(i).resp.valid := false.B
        vega_ex_controller.io.srams.read(i).req.ready := false.B
      }
      for(i <- 0 until vega_acc_banks) {
        vega_ex_controller.io.acc.read_resp(i).valid := false.B
        vega_ex_controller.io.acc.read_req(i).ready := false.B
        vega_ex_controller.io.acc.write(i).ready := false.B
      }
    }
    // Wire arbiter for ExecuteController and Im2Col scratchpad reads
    for(i <- 0 until vega_sp_banks) {
      val req_arb = Module(new Arbiter(new ScratchpadReadReq(n = sp_bank_entries), 2))

      req_arb.io.in(0) <> ex_controller.io.srams.read(i).req
      req_arb.io.in(1) <> vega_ex_controller.io.srams.read(i).req

      spad.module.io.srams.read(i).req <> req_arb.io.out


      ex_controller.io.srams.read(i).resp.valid := spad.module.io.srams.read(i).resp.valid
      vega_ex_controller.io.srams.read(i).resp.valid := spad.module.io.srams.read(i).resp.valid

      ex_controller.io.srams.read(i).resp.bits := spad.module.io.srams.read(i).resp.bits
      vega_ex_controller.io.srams.read(i).resp.bits := spad.module.io.srams.read(i).resp.bits

      spad.module.io.srams.read(i).resp.ready := ex_controller.io.srams.read(i).resp.ready || vega_ex_controller.io.srams.read(i).resp.ready
    }
    for(i <- vega_sp_banks until sp_banks) {
      val req_arb = Module(new Arbiter(new ScratchpadReadReq(n = sp_bank_entries), 1))

      req_arb.io.in(0) <> ex_controller.io.srams.read(i).req

      spad.module.io.srams.read(i).req <> req_arb.io.out
      ex_controller.io.srams.read(i).resp.valid := spad.module.io.srams.read(i).resp.valid
      ex_controller.io.srams.read(i).resp.bits := spad.module.io.srams.read(i).resp.bits
      spad.module.io.srams.read(i).resp.ready := ex_controller.io.srams.read(i).resp.ready
    }
  }
  else {
    ex_controller.io.cmd.valid := reservation_station.io.issue.ex.valid
    reservation_station.io.issue.ex.ready := ex_controller.io.cmd.ready

    // Wire up scratchpad to controllers
    ex_controller.io.srams.read <> spad.module.io.srams.read
    ex_controller.io.srams.write <> spad.module.io.srams.write
    spad.module.io.acc.read_req <> ex_controller.io.acc.read_req
    ex_controller.io.acc.read_resp <> spad.module.io.acc.read_resp
    ex_controller.io.acc.write <> spad.module.io.acc.write

    // Wire arbiter for ExecuteController and Im2Col scratchpad reads
    (ex_controller.io.srams.read, spad.module.io.srams.read).zipped.foreach { case (ex_read, spad_read) =>
      val req_arb = Module(new Arbiter(new ScratchpadReadReq(n=sp_bank_entries), 1))

      req_arb.io.in(0) <> ex_read.req

      spad_read.req <> req_arb.io.out

      ex_read.resp.valid := spad_read.resp.valid

      ex_read.resp.bits := spad_read.resp.bits

      spad_read.resp.ready := ex_read.resp.ready
    }
  }

  /*
  // Im2Col unit
  val im2col = withClock (gated_clock) { Module(new Im2Col(outer.config)) }

  // Wire up Im2col
  counters.io.event_io.collect(im2col.io.counter)
  // im2col.io.sram_reads <> spad.module.io.srams.read
  im2col.io.req <> ex_controller.io.im2col.req
  ex_controller.io.im2col.resp <> im2col.io.resp

   */


  // Wire up controllers to ROB
  reservation_station.io.alloc.valid := false.B
  // rob.io.alloc.bits := compressed_cmd.bits
  reservation_station.io.alloc.bits := unrolled_cmd.bits

  /*
  //=========================================================================
  // committed insn return path to frontends
  //=========================================================================

  //-------------------------------------------------------------------------
  // cisc
  tiler.io.completed.exec.valid := ex_controller.io.completed.valid
  tiler.io.completed.exec.bits := ex_controller.io.completed.bits

  tiler.io.completed.load <> load_controller.io.completed
  tiler.io.completed.store <> store_controller.io.completed

  // mux with cisc frontend arbiter
  tiler.io.completed.exec.valid  := ex_controller.io.completed.valid && is_cisc_mode
  tiler.io.completed.load.valid  := load_controller.io.completed.valid && is_cisc_mode
  tiler.io.completed.store.valid := store_controller.io.completed.valid && is_cisc_mode
  */

  //-------------------------------------------------------------------------
  // risc
  val reservation_station_completed_arb = Module(new Arbiter(UInt(log2Up(reservation_station_entries).W), 3))

  reservation_station_completed_arb.io.in(0).valid := ex_controller_valid//ex_controller.io.completed.valid
  reservation_station_completed_arb.io.in(0).bits := ex_controller_bits//ex_controller.io.completed.bits

  reservation_station_completed_arb.io.in(1) <> load_controller.io.completed
  reservation_station_completed_arb.io.in(2) <> store_controller.io.completed

  // mux with cisc frontend arbiter
  reservation_station_completed_arb.io.in(0).valid := ex_controller_valid //ex_controller.io.completed.valid // && !is_cisc_mode
  reservation_station_completed_arb.io.in(1).valid := load_controller.io.completed.valid // && !is_cisc_mode
  reservation_station_completed_arb.io.in(2).valid := store_controller.io.completed.valid // && !is_cisc_mode

  reservation_station.io.completed.valid := reservation_station_completed_arb.io.out.valid
  reservation_station.io.completed.bits := reservation_station_completed_arb.io.out.bits
  reservation_station_completed_arb.io.out.ready := true.B

  // Wire up global RoCC signals
  io.busy := raw_cmd.valid || loop_conv_unroller_busy || loop_matmul_unroller_busy || loop_vega_unroller_busy || reservation_station.io.busy || spad.module.io.busy || unrolled_cmd.valid || loop_cmd.valid || conv_cmd.valid

  io.interrupt := tlb.io.exp.map(_.interrupt).reduce(_ || _)

  // assert(!io.interrupt, "Interrupt handlers have not been written yet")

  /*
  // Cycle counters
  val incr_ld_cycles = load_controller.io.busy && !store_controller.io.busy && !ex_controller_busy //!ex_controller.io.busy
  val incr_st_cycles = !load_controller.io.busy && store_controller.io.busy && !ex_controller_busy //!ex_controller.io.busy
  val incr_ex_cycles = !load_controller.io.busy && !store_controller.io.busy && ex_controller_busy //ex_controller.io.busy

  val incr_ld_st_cycles = load_controller.io.busy && store_controller.io.busy && !ex_controller_busy //!ex_controller.io.busy
  val incr_ld_ex_cycles = load_controller.io.busy && !store_controller.io.busy && ex_controller_busy //ex_controller.io.busy
  val incr_st_ex_cycles = !load_controller.io.busy && store_controller.io.busy && ex_controller_busy//ex_controller.io.busy

  val incr_ld_st_ex_cycles = load_controller.io.busy && store_controller.io.busy && ex_controller_busy //ex_controller.io.busy

  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_CYCLES, incr_ld_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_CYCLES, incr_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_EX_CYCLES, incr_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_CYCLES, incr_ld_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_EX_CYCLES, incr_ld_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_EX_CYCLES, incr_st_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_EX_CYCLES, incr_ld_st_ex_cycles)

   */

  // Issue commands to controllers
  // TODO we combinationally couple cmd.ready and cmd.valid signals here
  // when (compressed_cmd.valid) {
  when (unrolled_cmd.valid) {
    // val config_cmd_type = cmd.bits.rs1(1,0) // TODO magic numbers

    //val funct = unrolled_cmd.bits.inst.funct
    val risc_funct = unrolled_cmd.bits.cmd.inst.funct

    val is_flush = risc_funct === FLUSH_CMD
    val is_counter_op = risc_funct === COUNTER_OP
    val is_clock_gate_en = risc_funct === CLKGATE_EN

    /*
    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = (funct === COMPUTE_AND_FLIP_CMD || funct === COMPUTE_AND_STAY_CMD || funct === PRELOAD_CMD) ||
    (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    */

    when (is_flush) {
      val skip = unrolled_cmd.bits.cmd.rs1(0)
      tlb.io.exp.foreach(_.flush_skip := skip)
      tlb.io.exp.foreach(_.flush_retry := !skip)

      unrolled_cmd.ready := true.B // TODO should we wait for an acknowledgement from the TLB?
    }

    .elsewhen (is_counter_op) {
      // If this is a counter access/configuration command, execute immediately
      //counters.io.in.valid := unrolled_cmd.valid
      //unrolled_cmd.ready := counters.io.in.ready
      //counters.io.in.bits := unrolled_cmd.bits.cmd
    }

    .elsewhen (is_clock_gate_en) {
      unrolled_cmd.ready := true.B
    }

    .otherwise {
      reservation_station.io.alloc.valid := true.B

      when(reservation_station.io.alloc.fire) {
        // compressed_cmd.ready := true.B
        unrolled_cmd.ready := true.B
      }
    }
  }

  // Debugging signals
  /*
  val pipeline_stall_counter = RegInit(0.U(32.W))
  when (io.cmd.fire()) {
    pipeline_stall_counter := 0.U
  }.elsewhen(io.busy) {
    pipeline_stall_counter := pipeline_stall_counter + 1.U
  }
  assert(pipeline_stall_counter < 10000000.U, "pipeline stall")
  */
  /*
  //=========================================================================
  // Wire up global RoCC signals
  //=========================================================================
  io.busy := raw_cmd.valid || unrolled_cmd.valid || rob.io.busy || spad.module.io.busy || tiler.io.busy
  io.interrupt := tlb.io.exp.interrupt

  // hack
  when(is_cisc_mode || !(unrolled_cmd.valid || rob.io.busy || tiler.io.busy)){
    tlb.io.exp.flush_retry := cmd_fsm.io.flush_retry
    tlb.io.exp.flush_skip  := cmd_fsm.io.flush_skip
  }
  */

  //=========================================================================
  // Performance Counters Access
  //=========================================================================

}

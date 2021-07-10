
package gemmini

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.TLIdentityNode
import GemminiISA._
import Util._

class GemminiCmd(rob_entries: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val rob_id = UDValid(UInt(log2Up(rob_entries).W))

  val i = UInt(16.W) // TODO magic numbers
  val j = UInt(16.W) // TODO magic numbers
  val k = UInt(16.W) // TODO magic numbers
  val max_i = UInt(16.W) // TODO magic numbers
  val max_j = UInt(16.W) // TODO magic numbers
  val max_k = UInt(16.W) // TODO magic numbers
  val use_iterators = Bool()

  val ex_k_portion = UInt(8.W) // TODO magic numbers

  override def cloneType: this.type = new GemminiCmd(rob_entries).asInstanceOf[this.type]
}

class Gemmini[T <: Data : Arithmetic, U <: Data, V <: Data](val config: GemminiArrayConfig[T, U, V])
                                     (implicit p: Parameters)
  extends LazyRoCC (
    opcodes = config.opcodes,
    nPTWPorts = 1) {

  Files.write(Paths.get(config.headerFilePath), config.generateHeader().getBytes(StandardCharsets.UTF_8))

  val xLen = p(XLen)
  val spad = LazyModule(new Scratchpad(config))

  override lazy val module = new GemminiModule(this)
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

  val tagWidth = 32

  // TLB
  implicit val edge = outer.node.edges.out.head
  val tlb = Module(new FrontendTLB(2, tlb_size, dma_maxbytes))
  (tlb.io.clients zip outer.spad.module.io.tlb).foreach(t => t._1 <> t._2)
  tlb.io.exp.flush_skip := false.B
  tlb.io.exp.flush_retry := false.B

  io.ptw.head <> tlb.io.ptw
  /*io.ptw.head.req <> tlb.io.ptw.req
  tlb.io.ptw.resp <> io.ptw.head.resp
  tlb.io.ptw.ptbr := io.ptw.head.ptbr
  tlb.io.ptw.status := outer.spad.module.io.mstatus
  tlb.io.ptw.pmp := io.ptw.head.pmp
  tlb.io.ptw.customCSRs := io.ptw.head.customCSRs*/

  spad.module.io.flush := tlb.io.exp.flush()

  /*
  //=========================================================================
  // Frontends: Incoming commands and ROB
  //=========================================================================

  // forward cmd to correct frontend. if the rob is busy, do not forward new
  // commands to tiler, and vice versa
  val is_cisc_mode = RegInit(false.B)

  val raw_cmd = Queue(io.cmd)
  val funct = raw_cmd.bits.inst.funct

  val is_cisc_funct = (funct === CISC_CONFIG) ||
                      (funct === ADDR_AB) ||
                      (funct === ADDR_CD) ||
                      (funct === SIZE_MN) ||
                      (funct === SIZE_K) ||
                      (funct === RPT_BIAS) ||
                      (funct === RESET) ||
                      (funct === COMPUTE_CISC)

  val raw_cisc_cmd = WireInit(raw_cmd)
  val raw_risc_cmd = WireInit(raw_cmd)
  raw_cisc_cmd.valid := false.B
  raw_risc_cmd.valid := false.B
  raw_cmd.ready := false.B

  //-------------------------------------------------------------------------
  // cisc
  val cmd_fsm = CmdFSM(outer.config)
  cmd_fsm.io.cmd <> raw_cisc_cmd
  val tiler = TilerController(outer.config)
  tiler.io.cmd_in <> cmd_fsm.io.tiler

  //-------------------------------------------------------------------------
  // risc
  val unrolled_cmd = LoopUnroller(raw_risc_cmd, outer.config.meshRows * outer.config.tileRows)
  */

  // Incoming commands and ROB
  val rob = Module(new ROB(outer.config, new RoCCCommand, new GemminiCmd(rob_entries)))

  val raw_cmd = Queue(io.cmd)

  val max_lds = rob_partial_entries
  val max_exs = rob_full_entries
  val max_sts = rob_partial_entries / 2

  // TODO replace 4,12,2 with parameters based on ROB size
  val (conv_cmd, loop_conv_unroller_busy) = LoopConv(raw_cmd, rob.io.ld_utilization, rob.io.st_utilization, rob.io.ex_utilization,
    meshRows*tileRows, coreMaxAddrBits, rob_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes)

  // val (compressed_cmd, compressor_busy) = InstCompressor(unrolled_cmd)
  // compressed_cmd.ready := false.B

  // val (unrolled_cmd, loop_matmul_unroller_busy) = LoopMatmul(unrolled_cmd_after_conv, rob.io.ld_utilization, rob.io.st_utilization, rob.io.ex_utilization,

  val (loop_cmd, loop_matmul_unroller_busy) = LoopMatmul(conv_cmd, rob.io.ld_utilization, rob.io.st_utilization, rob.io.ex_utilization, rob.io.ex_k_portion_utilizations,
    meshRows*tileRows, coreMaxAddrBits, rob_entries, rob_full_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes, new GemminiCmd(rob_entries), ex_total_k_portions, ex_fine_grained_interleaving)

  val unrolled_cmd = Queue(loop_cmd)
  unrolled_cmd.ready := false.B

  // val cmd_decompressor = Module(new InstDecompressor(rob_entries))

  // cmd_decompressor.io.in.valid := rob.io.issue.ex.valid
  // cmd_decompressor.io.in.bits.cmd := rob.io.issue.ex.cmd
  // cmd_decompressor.io.in.bits.rob_id := rob.io.issue.ex.rob_id
  // rob.io.issue.ex.ready := cmd_decompressor.io.in.ready

  // val decompressed_cmd = cmd_decompressor.io.out

  // Wire up controllers to ROB
  rob.io.alloc.valid := false.B
  // rob.io.alloc.bits := compressed_cmd.bits
  rob.io.alloc.bits := unrolled_cmd.bits

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
  val load_controller = Module(new LoadController(outer.config, coreMaxAddrBits, local_addr_t))
  val store_controller = Module(new StoreController(outer.config, coreMaxAddrBits, local_addr_t))
  val ex_controller = Module(new ExecuteController(xLen, tagWidth, outer.config))

  /*
  tiler.io.issue.load.ready := false.B
  tiler.io.issue.store.ready := false.B
  tiler.io.issue.exec.ready := false.B

  rob.io.issue.ld.ready := false.B
  rob.io.issue.st.ready := false.B
  rob.io.issue.ex.ready := false.B

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

  val (rob_issue_ld, rob_issue_ex) = PreloadFilter(outer.config, new RoCCCommand, rob.io.issue.ld, rob.io.issue.ex)

  load_controller.io.cmd.valid := rob_issue_ld.valid
  rob_issue_ld.ready := load_controller.io.cmd.ready
  load_controller.io.cmd.bits := DontCare
  load_controller.io.cmd.bits.cmd := rob_issue_ld.cmd
  load_controller.io.cmd.bits.cmd.inst.funct := rob_issue_ld.cmd.inst.funct
  load_controller.io.cmd.bits.rob_id.push(rob_issue_ld.rob_id)

  store_controller.io.cmd.valid := rob.io.issue.st.valid
  rob.io.issue.st.ready := store_controller.io.cmd.ready
  store_controller.io.cmd.bits := DontCare
  store_controller.io.cmd.bits.cmd := rob.io.issue.st.cmd
  store_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.st.cmd.inst.funct
  store_controller.io.cmd.bits.rob_id.push(rob.io.issue.st.rob_id)

  ex_controller.io.cmd.valid := rob_issue_ex.valid
  rob_issue_ex.ready := ex_controller.io.cmd.ready
  ex_controller.io.cmd.bits := DontCare
  ex_controller.io.cmd.bits.cmd := rob_issue_ex.cmd
  ex_controller.io.cmd.bits.cmd.inst.funct := rob_issue_ex.cmd.inst.funct
  ex_controller.io.cmd.bits.rob_id.push(rob_issue_ex.rob_id)

  // Wire up scratchpad to controllers
  spad.module.io.dma.read <> load_controller.io.dma
  spad.module.io.dma.write <> store_controller.io.dma
  ex_controller.io.srams.read <> spad.module.io.srams.read
  ex_controller.io.srams.write <> spad.module.io.srams.write
  spad.module.io.acc.read_req <> ex_controller.io.acc.read_req
  ex_controller.io.acc.read_resp <> spad.module.io.acc.read_resp
  ex_controller.io.acc.write <> spad.module.io.acc.write

  // Im2Col unit
  val im2col = Module(new Im2Col(outer.config))

  // Wire up Im2col
  // im2col.io.sram_reads <> spad.module.io.srams.read
  im2col.io.req <> ex_controller.io.im2col.req
  ex_controller.io.im2col.resp <> im2col.io.resp

  // Wire arbiter for ExecuteController and Im2Col scratchpad reads
  (ex_controller.io.srams.read, im2col.io.sram_reads, spad.module.io.srams.read).zipped.foreach { case (ex_read, im2col_read, spad_read) =>
    val req_arb = Module(new Arbiter(new ScratchpadReadReq(n=sp_bank_entries), 2))

    req_arb.io.in(0) <> ex_read.req
    req_arb.io.in(1) <> im2col_read.req

    spad_read.req <> req_arb.io.out

    // TODO if necessary, change how the responses are handled when fromIm2Col is added to spad read interface

    ex_read.resp.valid := spad_read.resp.valid
    im2col_read.resp.valid := spad_read.resp.valid

    ex_read.resp.bits := spad_read.resp.bits
    im2col_read.resp.bits := spad_read.resp.bits

    spad_read.resp.ready := ex_read.resp.ready || im2col_read.resp.ready
  }

  // Wire up controllers to ROB
  rob.io.alloc.valid := false.B
  // rob.io.alloc.bits := compressed_cmd.bits
  rob.io.alloc.bits := unrolled_cmd.bits

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
  val rob_completed_arb = Module(new Arbiter(UInt(log2Up(rob_entries).W), 3))

  rob_completed_arb.io.in(0).valid := ex_controller.io.completed.valid
  rob_completed_arb.io.in(0).bits := ex_controller.io.completed.bits

  rob_completed_arb.io.in(1) <> load_controller.io.completed
  rob_completed_arb.io.in(2) <> store_controller.io.completed

  // mux with cisc frontend arbiter
  rob_completed_arb.io.in(0).valid := ex_controller.io.completed.valid // && !is_cisc_mode
  rob_completed_arb.io.in(1).valid := load_controller.io.completed.valid // && !is_cisc_mode
  rob_completed_arb.io.in(2).valid := store_controller.io.completed.valid // && !is_cisc_mode

  rob.io.completed.valid := rob_completed_arb.io.out.valid
  rob.io.completed.bits := rob_completed_arb.io.out.bits
  rob_completed_arb.io.out.ready := true.B

  // Wire up global RoCC signals
  io.busy := raw_cmd.valid || loop_conv_unroller_busy || loop_matmul_unroller_busy || rob.io.busy || spad.module.io.busy || unrolled_cmd.valid || loop_cmd.valid || conv_cmd.valid
  io.interrupt := tlb.io.exp.interrupt

  rob.io.solitary_preload := ex_controller.io.solitary_preload

  // assert(!io.interrupt, "Interrupt handlers have not been written yet")

  // Cycle counters
  val ld_cycles = RegInit(0.U(34.W))
  val st_cycles = RegInit(0.U(34.W))
  val ex_cycles = RegInit(0.U(34.W))

  val ld_st_cycles = RegInit(0.U(34.W))
  val ld_ex_cycles = RegInit(0.U(34.W))
  val st_ex_cycles = RegInit(0.U(34.W))

  val ld_st_ex_cycles = RegInit(0.U(34.W))

  val incr_ld_cycles = load_controller.io.busy && !store_controller.io.busy && !ex_controller.io.busy
  val incr_st_cycles = !load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ex_cycles = !load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_cycles = load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ld_ex_cycles = load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy
  val incr_st_ex_cycles = !load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_ex_cycles = load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  when (incr_ld_cycles) {
    ld_cycles := ld_cycles + 1.U
  }.elsewhen (incr_st_cycles) {
    st_cycles := st_cycles + 1.U
  }.elsewhen (incr_ex_cycles) {
    ex_cycles := ex_cycles + 1.U
  }.elsewhen (incr_ld_st_cycles) {
    ld_st_cycles := ld_st_cycles + 1.U
  }.elsewhen (incr_ld_ex_cycles) {
    ld_ex_cycles := ld_ex_cycles + 1.U
  }.elsewhen (incr_st_ex_cycles) {
    st_ex_cycles := st_ex_cycles + 1.U
  }.elsewhen (incr_ld_st_ex_cycles) {
    ld_st_ex_cycles := ld_st_ex_cycles + 1.U
  }

  // Issue commands to controllers
  // TODO we combinationally couple cmd.ready and cmd.valid signals here
  // when (compressed_cmd.valid) {
  when (unrolled_cmd.valid) {
    // val config_cmd_type = cmd.bits.rs1(1,0) // TODO magic numbers

    //val funct = unrolled_cmd.bits.inst.funct
    val risc_funct = unrolled_cmd.bits.cmd.inst.funct

    val is_flush = risc_funct === FLUSH_CMD
    /*
    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = (funct === COMPUTE_AND_FLIP_CMD || funct === COMPUTE_AND_STAY_CMD || funct === PRELOAD_CMD) ||
    (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    */

    when (is_flush) {
      // val skip = compressed_cmd.bits.rs1(0)
      val skip = unrolled_cmd.bits.cmd.rs1(0)
      tlb.io.exp.flush_skip := skip
      tlb.io.exp.flush_retry := !skip

      // compressed_cmd.ready := true.B // TODO should we wait for an acknowledgement from the TLB?
      unrolled_cmd.ready := true.B // TODO should we wait for an acknowledgement from the TLB?
    }

    .otherwise {
      rob.io.alloc.valid := true.B

      when(rob.io.alloc.fire()) {
        // compressed_cmd.ready := true.B
        unrolled_cmd.ready := true.B
      }
    }

  }

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
}

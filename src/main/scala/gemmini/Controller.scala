
package gemmini

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.{BundleField, ClockGate}
import freechips.rocketchip.tilelink._
import GemminiISA._
import Util._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, SimpleDevice, TransferSizes}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.regmapper.{RegField, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink.TLRegisterNode
import org.chipsalliance.diplomacy.lazymodule.LazyModule

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

  val xLen = p(TileKey).core.xLen
  val spad = LazyModule(new Scratchpad(config))

  val use_ext_tl_mem = config.use_shared_ext_mem && config.use_tl_ext_mem
  val num_ids = 32 // TODO (richard): move to config
  val spad_base = config.tl_ext_mem_base
  val spad_data_len = config.sp_width_projected / 8
  val acc_data_len = config.meshColumns * config.tileColumns * config.accType.getWidth / 8
  val max_data_len = spad_data_len // max acc_data_len

  val mem_depth = config.sp_bank_entries * spad_data_len / max_data_len
  val mem_width = max_data_len

  println(f"unified shared memory size: ${mem_depth}x${mem_width}x${config.sp_banks}")

  // make scratchpad read and write clients, per bank
  //    _____  ________  _______  ___   ___
  //   / __/ |/_/_  __/ / __/ _ \/ _ | / _ \
  //  / _/_>  <  / /   _\ \/ ___/ __ |/ // /
  // /___/_/|_| /_/   /___/_/  /_/ |_/____/
  // ***************************************
  // HOW TO USE EXTERNAL SCRATCHPAD:
  // the scratchpad MUST BE INSTANTIATED ELSEWHERE if use_ext_tl_mem is enabled,
  // else elaboration will not pass. the scratchpad needs to be dual ported
  // and must be able to serve the entire scratchpad row (config.sp_width) in 1 cycle.
  // three nodes must be hooked up correctly: spad_read_nodes, spad_write_nodes, and spad.spad_writer.node
  // for deadlock avoidance, read and write should not be sharing a single channel anywhere until the SRAMs.
  // see RadianceCluster.scala for an example
  val spad_read_nodes = if (use_ext_tl_mem) TLClientNode(Seq.tabulate(config.sp_banks) {i =>
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = s"spad_read_node_$i",
      sourceId = IdRange(0, num_ids),
      supportsProbe = TransferSizes(mem_width, mem_width),
      supportsGet = TransferSizes(mem_width, mem_width)
    )))
  }) else TLIdentityNode()

  val spad_write_nodes = if (use_ext_tl_mem) TLClientNode(Seq.tabulate(config.sp_banks) { i =>
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = s"spad_write_node_$i",
      sourceId = IdRange(0, num_ids),
      supportsProbe = TransferSizes(mem_width, mem_width),
      supportsPutFull = TransferSizes(mem_width, mem_width),
      supportsPutPartial = TransferSizes(mem_width, mem_width)
    )))
  }) else TLIdentityNode()

  // val acc_read_nodes = if (create_tl_mem) TLClientNode(Seq.tabulate(config.acc_banks) { i =>
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = s"acc_read_node_$i", sourceId = IdRange(0, numIDs))))
  // }) else TLIdentityNode()
  // val acc_write_nodes = if (create_tl_mem) TLClientNode(Seq.tabulate(config.acc_banks) { i =>
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = s"acc_write_node_$i", sourceId = IdRange(0, numIDs))))
  // }) else TLIdentityNode()

  override lazy val module = new GemminiModule(this)
  override val tlNode = if (config.use_dedicated_tl_port) spad.id_node else TLIdentityNode()
  override val atlNode = if (config.use_dedicated_tl_port) TLIdentityNode() else spad.id_node

  val node = if (config.use_dedicated_tl_port) tlNode else atlNode

  // Standalone MMIO path to mx_io endpoints; parent config attaches to pbus.
  // Disabled in radiance builds (use_shared_ext_mem=true), which use mx_io.
  val use_mx_mmio = config.use_mx_scaling && !config.use_shared_ext_mem
  val mx_mmio_node = Option.when(use_mx_mmio) {
    TLRegisterNode(
      address     = Seq(AddressSet(config.mx_mmio_base.getOrElse(config.tl_ext_mem_base + 0x100000L), 0xfffL)),
      device      = new SimpleDevice("gemmini-mx-mmio", Seq("ucbbar,gemmini-mx-mmio")),
      beatBytes   = 8,
      concurrency = 1)
  }
  // Push-side master for requantizer output: issues a TL Put to out.bits.address on each result.
  val mx_requant_out_client = Option.when(use_mx_mmio) {
    val q = config.requantizer.get
    val minBytes = q.numOutputLanes * q.minOutputBits / 8
    val maxBytes = q.numOutputLanes * q.maxOutputBits / 8
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v2(
        name     = "gemmini-mx-requant-out",
        sourceId = IdRange(0, 1 << q.outputIdBits),
        emits    = TLMasterToSlaveTransferSizes(
          putFull    = TransferSizes(minBytes, maxBytes),
          putPartial = TransferSizes(minBytes, maxBytes)
        )
      ))
    )))
  }
  // Push-side master for output scaling factors: gemmini issues fixed-width
  // TL Puts whenever mx_requantizer.io.scaleMem_write fires.
  val mx_scale_fac_out_client = Option.when(use_mx_mmio) {
    val s = config.scale_mem.get
    val dataBytes = s.ScaleMemWriteDataWidth / 8
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v2(
        name     = "gemmini-mx-scale-fac-out",
        sourceId = IdRange(0, 1 << 4),
        emits    = TLMasterToSlaveTransferSizes(
          putFull    = TransferSizes(dataBytes, dataBytes),
          putPartial = TransferSizes(dataBytes, dataBytes)
        )
      ))
    )))
  }

  // Pull-side master for funct-27 MX_LOAD_SCALES: issues 8-byte TL Gets from DRAM (addr in rs1)
  // and replays them as scale-mem write beats. Replaces the CPU flat window as the SW scale front-end.
  val mx_scale_loader_client = Option.when(use_mx_mmio) {
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name     = "gemmini-mx-scale-loader",
        sourceId = IdRange(0, 1))))))
  }

  // Pull-side master for funct-29 MX_LOAD_LUT: issues 8-byte TL Gets from DRAM (addr in rs1),
  // assembles num_luts*96b into the LUT buffer, and fires the selected requantizer lut port once.
  val mx_lut_loader_client = Option.when(use_mx_mmio) {
    TLClientNode(Seq(TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name     = "gemmini-mx-lut-loader",
        sourceId = IdRange(0, 1))))))
  }

  // Attach the standalone MX nodes via the standard LazyRoCC hooks: slaves (LUT/requant regmap +
  // scale window) <- stlNode (tile slave port); out clients -> tlNode -> sbus.
  if (use_mx_mmio) {
    val mx_slave_xbar = TLXbar()
    mx_slave_xbar := stlNode
    // Fragmenter (not just a width widget) so the regmap advertises [1,64] upstream and stays
    // homogeneous with DRAM for the gemmini DMA's TLB page lookup; it still sees <=8B beats.
    mx_mmio_node.foreach      { _ := TLFragmenter(8, p(CacheBlockBytes)) := mx_slave_xbar }
  }
  mx_requant_out_client.foreach   { client => tlNode := TLBuffer() := client }
  mx_scale_fac_out_client.foreach { client => tlNode := TLBuffer() := client }
  mx_scale_loader_client.foreach  { client => tlNode := TLBuffer() := client }
  mx_lut_loader_client.foreach    { client => tlNode := TLBuffer() := client }
}

class GemminiModule[T <: Data: Arithmetic, U <: Data, V <: Data]
    (outer: Gemmini[T, U, V])
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val ext_mem_io = if (use_shared_ext_mem && !use_tl_ext_mem)
    Some(IO(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))) else None

  if (outer.use_ext_tl_mem) {
    val ext_mem_spad = outer.spad.module.io.ext_mem.get.spad
    val ext_mem_acc = outer.spad.module.io.ext_mem.get.acc
    val source_counters = Seq.fill(4)(Counter(outer.num_ids))

    def connect(ext_mem: ExtMemIO, bank_base: Int, req_size: Int, r_node: TLBundle, r_edge: TLEdgeOut, r_source: Counter,
                w_node: TLBundle, w_edge: TLEdgeOut, w_source: Counter): Unit = {
      r_node.a.valid := ext_mem.read_req.valid
      r_node.a.bits := r_edge.Get(r_source.value,
        (ext_mem.read_req.bits << req_size.U).asUInt | bank_base.U | outer.spad_base.U,
        req_size.U)._2
      ext_mem.read_req.ready := r_node.a.ready

      val w_shifted_addr = (ext_mem.write_req.bits.addr << req_size.U).asUInt
      val w_mask = (ext_mem.write_req.bits.mask << (w_shifted_addr & (w_edge.manager.beatBytes - 1).U)).asUInt

      w_node.a.valid := ext_mem.write_req.valid
      w_node.a.bits := w_edge.Put(w_source.value,
        w_shifted_addr | bank_base.U | outer.spad_base.U,
        req_size.U, ext_mem.write_req.bits.data, w_mask)._2
      ext_mem.write_req.ready := w_node.a.ready

      ext_mem.read_resp.valid := r_node.d.valid
      ext_mem.read_resp.bits := r_node.d.bits.data
      r_node.d.ready := ext_mem.read_resp.ready

      w_node.d.ready := true.B // writes are not acknowledged in gemmini

      when(ext_mem.read_req.fire) { r_source.inc() }
      when(ext_mem.write_req.fire) { w_source.inc() }
    }
    (outer.spad_read_nodes.out zip outer.spad_write_nodes.out)
      .zipWithIndex.foreach{ case (((r_node, r_edge), (w_node, w_edge)), i) =>
        connect(ext_mem_spad(i), i * outer.mem_depth * outer.mem_width, log2Up(outer.spad_data_len),
          r_node, r_edge, source_counters(0), w_node, w_edge, source_counters(1))
    }


    ext_mem_acc.foreach(_.foreach(x => {
      x.read_resp.bits := 0.U(1.W)
      x.read_resp.valid := false.B
      x.read_req.ready := false.B
      x.write_req.ready := false.B
    }))
    // (outer.acc_read_nodes.out zip outer.acc_write_nodes.out)
    //   .zipWithIndex.foreach { case (((r_node, r_edge), (w_node, w_edge)), i) =>
    //     // TODO (richard): one subbank only for now
    //     connect(ext_mem_acc(i)(0), log2Up(outer.acc_data_len),
    //       r_node, r_edge, source_counters(2), w_node, w_edge, source_counters(3))
    // }
  } else if (use_shared_ext_mem) {
    ext_mem_io.foreach(_ <> outer.spad.module.io.ext_mem.get)
  }

  val tagWidth = 32

  // Counters
  val counters = Module(new CounterController(outer.config.num_counter, outer.xLen))
  io.resp <> counters.io.out  // Counter access command will be committed immediately
  counters.io.event_io.external_values(0) := 0.U
  counters.io.event_io.event_signal(0) := false.B
  counters.io.in.valid := false.B
  counters.io.in.bits := DontCare
  counters.io.event_io.collect(spad.module.io.counter)

  // TLB
  implicit val edge = outer.spad.id_node.edges.out.head
  val tlb = Module(new FrontendTLB(if (outer.config.use_tl_ext_mem) 3 else 2,
    tlb_size, dma_maxbytes, use_tlb_register_filter, use_firesim_simulation_counters, use_shared_tlb))
  (tlb.io.clients zip outer.spad.module.io.tlb).foreach(t => t._1 <> t._2)

  tlb.io.exp.foreach(_.flush_skip := false.B)
  tlb.io.exp.foreach(_.flush_retry := false.B)

  io.ptw <> tlb.io.ptw

  counters.io.event_io.collect(tlb.io.counter)

  spad.module.io.flush := tlb.io.exp.map(_.flush()).reduce(_ || _)

  val clock_en_reg = RegInit(true.B)
  val gated_clock = if (clock_gate) ClockGate(clock, clock_en_reg, "gemmini_clock_gate") else clock
  outer.spad.module.clock := gated_clock

  //=========================================================================
  // Controllers
  //=========================================================================
  val load_controller = withClock (gated_clock) { Module(new LoadController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val store_controller = withClock (gated_clock) { Module(new StoreController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val ex_controller = withClock (gated_clock) { Module(new ExecuteController(xLen, tagWidth, outer.config)) }
  val mx_requantizer = Option.when(outer.config.use_mx_scaling && outer.config.requantizer.isDefined && outer.config.lut.isDefined) {
    val q = outer.config.requantizer.get
    val l = outer.config.lut.get
    
    Module(new MxRequantizer(
      sp_data_width = outer.config.sp_width,
      sp_addr_width = log2Ceil(outer.config.sp_bank_entries),
      scaleMem_data_width = outer.config.scale_mem.get.ScaleMemWriteDataWidth,
      scaleMem_addr_width = outer.config.scale_mem.get.ScaleMemWriteAddrWidth,
      // on-chip act-scale write-port byte-addr width (same as scale_loader_act uses)
      scaleMemActWriteAddrWidth = outer.config.scale_mem.get.addrBits - 1,
      scaleSize = outer.config.scaleSize,
      scaleMembasewrite = 0, // TODO: add this into the instruction
      lutConfig = l,
      sp_bank_entries = outer.config.sp_bank_entries,
      sp_banks = outer.config.sp_banks,
      sp_width = outer.config.sp_width,
      sp_width_projected = outer.config.sp_width_projected,
      iterator_bitwidth = 16,
      meshColumns = outer.config.meshColumns,
      tileColumns = outer.config.tileColumns,
      accType = outer.config.accType,
      weightTypeProjected = outer.config.weightTypeProjected,
      config = q  
    ))
  }

  mx_requantizer.foreach { req =>
    require(outer.config.use_mx_scaling, "use_mx_scaling needs to be true if mx_requantizer is defined")
    req.io.scaleMem_write.ready := false.B
    req.io.scale_mem_mvout_base_addr_act := ex_controller.io.mx.get.scale_mem_mvout_base_addr_act
    req.io.quant_lut_update_granularity := ex_controller.io.mx.get.quant_lut_update_granularity
    // Thread the residency flag; the transposed act-write port defaults idle (the standalone
    // use_mx_mmio branch below drives its ready via the scale_mem_write_act mux).
    req.io.scale_resident := ex_controller.io.mx.get.scale_resident
    req.io.scaleMem_write_act_resident.ready := false.B
    // E4M3-quad requant output (4-bit LUT) vs E4M3-single (8-bit): both output format0/altfmt0, split by lut_en.
    req.io.lut_en := ex_controller.io.mx.get.lut_en_out
  }


  val mx_io = Option.when(outer.config.use_mx_scaling && outer.config.use_shared_ext_mem) {
    require(outer.config.requantizer.isDefined && outer.config.lut.isDefined, "requantizer and lut need to be defined if using mx_scaling")
    val q = outer.config.requantizer.get
    val l = outer.config.lut.get
    val s = outer.config.scale_mem.get

    IO(new Bundle {
      val scale_mem_write_w = Flipped(Decoupled(new ScalingFactorWriteReq(s)))
      val scale_mem_write_act = Flipped(Decoupled(new ScalingFactorWriteReq(s)))
      val requant_in_gpu = Flipped(Decoupled(new RequantizerInBundle(q.numGPUInputLanes, q.inputBits)))
      val requant_out = Decoupled(new RequantizerOutBundle(q.numOutputLanes, q.maxOutputBits))
      val lut0 = Flipped(Decoupled(new QuantLutWriteBundle(l(0))))
      val lut1 = Flipped(Decoupled(new QuantLutWriteBundle(l(1))))
      val lut2 = Flipped(Decoupled(new QuantLutWriteBundle(l(2))))
      val scale_factor_out = Decoupled(new ScalingFactorWriteReq(s.ScaleMemWriteAddrWidth, s.ScaleMemWriteDataWidth))
    })
  }

  val (mmio_requant_in_gpu, mmio_requant_out, mmio_scale_factor_out) =
    if (outer.use_mx_mmio) {
      val s = outer.config.scale_mem.get
      val q = outer.config.requantizer.get
      val l = outer.config.lut.get

      // ------ helper: latch a flat dataW-bit reg from N×64-bit MMIO writes ------
      def dataRegFields(base: Int, data_reg: UInt, dataW: Int, offset: Int)
        : Seq[(Int, Seq[RegField])] = {
        val data_words = (dataW + 63) / 64
        Seq.tabulate(data_words) { i =>
          val hi = math.min(dataW - 1, 64 * (i + 1) - 1)
          val lo = 64 * i
          val width = hi - lo + 1
          (base + offset + 8 * i) -> Seq(RegField(width,
            RegReadFn(data_reg(hi, lo)),
            RegWriteFn((wvalid, wdata) => {
              when (wvalid) {
                data_reg := (data_reg & ~(((BigInt(1) << width) - 1).U << lo.U).asUInt) |
                            (wdata(width - 1, 0) << lo.U).asUInt
              }
              true.B
            })))
        }
      }

      def goField(base: Int, offset: Int, staged: Bool): (Int, Seq[RegField]) = {
        (base + offset) -> Seq(RegField(1,
          RegReadFn(staged),
          RegWriteFn((wvalid, _) => {
            when (wvalid && !staged) { staged := true.B }
            !staged
          })))
      }

      // ------ requantizer-in port (data Vec + address + dataType + go) ------
      def requantInPort(base: Int): (DecoupledIO[RequantizerInBundle], Seq[(Int, Seq[RegField])]) = {
        val numLanes = q.numGPUInputLanes
        val laneBits = q.inputBits
        val dataW    = numLanes * laneBits
        val w = Wire(Decoupled(new RequantizerInBundle(numLanes, laneBits)))

        val staged    = RegInit(false.B)
        val addr_reg  = RegInit(0.U(32.W))
        val dtype_reg = RegInit(0.U(2.W))
        val data_reg  = RegInit(0.U(dataW.W))

        w.valid         := staged
        w.bits.address  := addr_reg
        w.bits.dataType := dtype_reg.asTypeOf(RequantizerDataType())
        w.bits.data     := data_reg.asTypeOf(Vec(numLanes, UInt(laneBits.W)))
        when (w.fire) { staged := false.B }

        val fields = Seq(
          (base + 0x000) -> Seq(RegField(32, addr_reg)),
          (base + 0x008) -> Seq(RegField(2, dtype_reg)),
          goField(base, 0x030, staged)
        ) ++ dataRegFields(base, data_reg, dataW, 0x010)

        (w, fields)
      }

      // ------ LUT write port (data Vec only, no addr/dtype) ------
      def lutPort(base: Int, idx: Int): (DecoupledIO[QuantLutWriteBundle], Seq[(Int, Seq[RegField])]) = {
        val (numEntries, numBits) = l(idx)
        val dataW = numEntries * numBits
        val w = Wire(Decoupled(new QuantLutWriteBundle(numEntries, numBits)))

        val staged   = RegInit(false.B)
        val data_reg = RegInit(0.U(dataW.W))

        w.valid     := staged
        w.bits.data := data_reg.asTypeOf(Vec(numEntries, UInt(numBits.W)))
        when (w.fire) { staged := false.B }

        val data_words   = (dataW + 63) / 64
        val go_offset = data_words * 8  // immediately after the data block

        val fields = Seq(
          goField(base, go_offset, staged)
        ) ++ dataRegFields(base, data_reg, dataW, 0x000)

        (w, fields)
      }

      // ------ requant-out (push: gemmini issues TL Puts via mx_requant_out_client) ------
      val rout_wire = {
        val numLanes = q.numOutputLanes
        val laneBits = q.maxOutputBits
        val w = Wire(Flipped(Decoupled(new RequantizerOutBundle(numLanes, laneBits))))

        val (node, edge) = outer.mx_requant_out_client.get.out.head
        val fullWidth = q.numOutputLanes
        val halfWidth = q.numOutputLanes / 2
        val beatBytes  = node.params.dataBits / 8
        val nBeatsFull = (fullWidth + beatBytes - 1) / beatBytes
        val nBeatsHalf = (halfWidth + beatBytes - 1) / beatBytes

        // Accept-then-burst: latch the request, then drive the (possibly multibeat) Put from the latched
        // copy, holding one source across all beats (TL forbids the source changing mid-burst) and slicing
        // data per beat. Latching also keeps w.ready off w.bits, avoiding a combinational loop.
        val busy   = RegInit(false.B)
        val req    = Reg(new RequantizerOutBundle(numLanes, laneBits))
        val beat   = RegInit(0.U(log2Ceil(nBeatsFull + 1).W))
        val source = RegInit(0.U(q.outputIdBits.W))
        w.ready := !busy
        when (w.fire) { req := w.bits; busy := true.B; beat := 0.U }

        val isFP4 = req.dataType === RequantizerDataType.FP4 || req.dataType === RequantizerDataType.FP6
        val putData = Mux(isFP4,
          Mux(req.address(log2Ceil(halfWidth)),
            (req.data(halfWidth - 1, 0) << halfWidth).asTypeOf(UInt(fullWidth.W)),
            req.data),
          req.data)
        val dataVec = putData.asTypeOf(Vec(nBeatsFull, UInt((beatBytes * 8).W)))
        val last = beat === Mux(isFP4, (nBeatsHalf - 1).U, (nBeatsFull - 1).U)
        node.a.valid     := busy
        node.a.bits      := edge.Put(fromSource = source, toAddress = req.address,
                              lgSize = Mux(isFP4, log2Ceil(halfWidth).U, log2Ceil(fullWidth).U),
                              data = dataVec(0))._2
        node.a.bits.data := dataVec(beat)
        node.d.ready     := true.B
        when (busy && node.a.fire) {
          beat := beat + 1.U
          when (last) { busy := false.B; beat := 0.U; source := source + 1.U }
        }
        w
      }

      // ------ scale-factor-out (push: gemmini issues fixed-size TL Puts) ------
      // Fires a TL Put on every w.fire from mx_requantizer.io.scaleMem_write.
      val sfout_wire = {
        val w = Wire(Flipped(Decoupled(new ScalingFactorWriteReq(
          s.ScaleMemWriteAddrWidth, s.ScaleMemWriteDataWidth))))

        val (node, edge) = outer.mx_scale_fac_out_client.get.out.head
        val dataBytes = s.ScaleMemWriteDataWidth / 8
        val beatBytes = node.params.dataBits / 8
        val nBeats    = (dataBytes + beatBytes - 1) / beatBytes

        // Accept-then-burst (see rout_wire).
        val busy   = RegInit(false.B)
        val req    = Reg(new ScalingFactorWriteReq(s.ScaleMemWriteAddrWidth, s.ScaleMemWriteDataWidth))
        val beat   = RegInit(0.U(log2Ceil(nBeats + 1).W))
        val source = RegInit(0.U(4.W))
        w.ready := !busy
        when (w.fire) { req := w.bits; busy := true.B; beat := 0.U }

        val dataVec = req.data.asTypeOf(Vec(nBeats, UInt((beatBytes * 8).W)))
        val last = beat === (nBeats - 1).U
        node.a.valid     := busy
        node.a.bits      := edge.Put(fromSource = source, toAddress = req.addr,
                              lgSize = log2Ceil(dataBytes).U, data = dataVec(0))._2
        node.a.bits.data := dataVec(beat)
        node.d.ready     := true.B
        when (busy && node.a.fire) {
          beat := beat + 1.U
          when (last) { busy := false.B; beat := 0.U; source := source + 1.U }
        }
        w
      }

      val (rin_wire, rin_fields) = requantInPort(0x080)

      outer.mx_mmio_node.get.regmap(rin_fields: _*)

      (Some(rin_wire), Some(rout_wire), Some(sfout_wire))
    } else {
      (None, None, None)
    }
  // ---- funct-27 MX_LOAD_SCALES: DMA loader (8-byte Gets from DRAM -> scale-mem write beats) ----
  // Replays the (addr = i*8, data = little-endian 64b) beat sequence byte-identically to the CPU flat
  // window. sel=1 -> weight port, sel=0 -> activation port (Spike polarity). Physical-only addressing.
  val scale_loader_busy = WireDefault(false.B)
  val (scale_loader_w, scale_loader_act, scale_loader_start) = if (outer.use_mx_mmio) {
    val s = outer.config.scale_mem.get
    val (gnode, gedge) = outer.mx_scale_loader_client.get.out.head
    val beatBytes = gnode.params.dataBits / 8
    val nLanes    = math.max(beatBytes / 8, 1)
    val laneRegW  = if (nLanes > 1) log2Ceil(nLanes) else 1

    val start = Wire(Decoupled(new Bundle {
      val addr = UInt(coreMaxAddrBits.W)
      val len  = UInt(32.W)   // bytes
      val sel  = Bool()       // 1 = weight, 0 = activation
    }))
    start.valid := false.B
    start.bits  := DontCare

    val w_out   = Wire(Decoupled(new ScalingFactorWriteReq(s.addrBits - 1, 8 * 8)))
    val act_out = Wire(Decoupled(new ScalingFactorWriteReq(s.addrBits - 1, 8 * 8)))
    w_out.valid   := false.B; w_out.bits   := DontCare
    act_out.valid := false.B; act_out.bits := DontCare

    val sIdle :: sReq :: sResp :: Nil = Enum(3)
    val state       = RegInit(sIdle)
    val base        = Reg(UInt(coreMaxAddrBits.W))
    val total_words = Reg(UInt(29.W))   // len bytes / 8
    val word_idx    = Reg(UInt(29.W))
    val sel_r       = Reg(Bool())
    val lane_r      = Reg(UInt(laneRegW.W))

    // TL-A/D defaults (overridden per-state below)
    gnode.a.valid := false.B
    gnode.a.bits  := DontCare
    gnode.d.ready := false.B

    start.ready := state === sIdle
    when (state === sIdle && start.fire) {
      base        := start.bits.addr
      total_words := start.bits.len(31, 3)
      word_idx    := 0.U
      sel_r       := start.bits.sel
      state       := Mux(start.bits.len(31, 3) === 0.U, sIdle, sReq)
    }

    val get_addr = base + (word_idx << 3)
    when (state === sReq) {
      gnode.a.valid := true.B
      gnode.a.bits  := gedge.Get(fromSource = 0.U, toAddress = get_addr, lgSize = 3.U)._2
      when (gnode.a.fire) {
        lane_r := (if (nLanes > 1) get_addr(log2Ceil(beatBytes) - 1, 3) else 0.U)
        state  := sResp
      }
    }

    val dataLanes = gnode.d.bits.data.asTypeOf(Vec(nLanes, UInt(64.W)))
    val word_data = if (nLanes > 1) dataLanes(lane_r) else gnode.d.bits.data(63, 0)
    when (state === sResp) {
      when (sel_r) {
        w_out.valid     := gnode.d.valid
        w_out.bits.addr := (word_idx << 3)
        w_out.bits.data := word_data
        gnode.d.ready   := w_out.ready
      } .otherwise {
        act_out.valid     := gnode.d.valid
        act_out.bits.addr := (word_idx << 3)
        act_out.bits.data := word_data
        gnode.d.ready     := act_out.ready
      }
      when (gnode.d.fire) {
        word_idx := word_idx + 1.U
        state    := Mux(word_idx + 1.U === total_words, sIdle, sReq)
      }
    }

    scale_loader_busy := state =/= sIdle
    (Some(w_out), Some(act_out), Some(start))
  } else (None, None, None)

  // ---- funct-29 MX_LOAD_LUT: DMA loader (8-byte Gets from DRAM -> one lutN_write bundle fire) ----
  // Assembles num_luts*96b into the LUT buffer, then fires the selected requantizer lut port once.
  // sel: 0 = B/weight -> lut0, 1 = A/act-in -> lut1, 2 = C/act-out -> lut2 (Spike polarity).
  val lut_loader_busy = WireDefault(false.B)

  // Runtime LUT-usage flag: MX_LOAD_LUT sets it, MX_LUT_DISABLE clears it, default off.
  val mx_lut_en = RegInit(false.B)
  ex_controller.io.lut_en := mx_lut_en
  // Per-operand LUT-usage (MX_LOAD_LUT sel: 1=act, 0=weight): each operand's single-vs-quad, vs global.
  val mx_lut_a_en = RegInit(false.B)
  val mx_lut_b_en = RegInit(false.B)
  ex_controller.io.act_lut_en := mx_lut_a_en
  ex_controller.io.weight_lut_en := mx_lut_b_en

  // True on the E4M3-quad build (either operand lane is E4M3), which routes E4M3 (code0) operands through
  // the LUT deproject (4-bit -> 2x8b E4M3) when lut_en is set.
  private def isE4M3Lane(t: Any): Boolean = t match {
    case mf: MxFloat => mf.sigWidth >= 4 && mf.expWidth < 5
    case _ => false
  }
  val e4m3QuadThroughput = isE4M3Lane(outer.config.spatialArrayInputType) ||
    isE4M3Lane(outer.config.spatialArrayWeightType)

  val (lut_out, lut_out_sel, lut_loader_start) = if (outer.use_mx_mmio) {
    val (gnode, gedge) = outer.mx_lut_loader_client.get.out.head
    val beatBytes = gnode.params.dataBits / 8
    val nLanes    = math.max(beatBytes / 8, 1)
    val laneRegW  = if (nLanes > 1) log2Ceil(nLanes) else 1
    val (numEntries, numBits) = outer.config.lut.get(0)
    val bufWords  = (numEntries * numBits + 63) / 64   // 6144/64 = 96

    val start = Wire(Decoupled(new Bundle {
      val addr = UInt(coreMaxAddrBits.W)
      val num  = UInt(32.W)   // num_luts
      val sel  = UInt(2.W)    // 0=B/weight, 1=A/act-in, 2=C/act-out
      val entry_bits = UInt(6.W)   // DRAM codebook entry width (datatype): FP6/E2M3=6, E5M2=8
    }))
    start.valid := false.B
    start.bits  := DontCare

    val out = Wire(Decoupled(new QuantLutWriteBundle(numEntries, numBits)))
    out.valid := false.B
    out.bits  := DontCare
    val sel_out = RegInit(0.U(2.W))

    val sIdle :: sReq :: sResp :: sFire :: Nil = Enum(4)
    val state     = RegInit(sIdle)
    val base      = Reg(UInt(coreMaxAddrBits.W))
    val num_words = Reg(UInt(log2Ceil(bufWords + 1).W))
    val word_idx  = Reg(UInt(log2Ceil(bufWords + 1).W))
    val lane_r    = Reg(UInt(laneRegW.W))
    val buffer    = Reg(Vec(bufWords, UInt(64.W)))
    val ent_bits  = Reg(UInt(6.W))   // latched DRAM codebook entry width

    gnode.a.valid := false.B
    gnode.a.bits  := DontCare
    gnode.d.ready := false.B

    start.ready := state === sIdle
    when (state === sIdle && start.fire) {
      base      := start.bits.addr
      // native DRAM codebook = num_luts * 16 entries * entry_bits bits -> u64 words. FP6 6b=12B, E5M2 8b=16B.
      num_words := ((start.bits.num * (start.bits.entry_bits << 1).asUInt) + 7.U) >> 3
      word_idx  := 0.U
      sel_out   := start.bits.sel
      ent_bits  := start.bits.entry_bits
      state     := Mux(start.bits.num === 0.U, sIdle, sReq)
    }

    val get_addr = base + (word_idx << 3)
    when (state === sReq) {
      gnode.a.valid := true.B
      gnode.a.bits  := gedge.Get(fromSource = 0.U, toAddress = get_addr, lgSize = 3.U)._2
      when (gnode.a.fire) {
        lane_r := (if (nLanes > 1) get_addr(log2Ceil(beatBytes) - 1, 3) else 0.U)
        state  := sResp
      }
    }

    val dataLanes = gnode.d.bits.data.asTypeOf(Vec(nLanes, UInt(64.W)))
    val word_data = if (nLanes > 1) dataLanes(lane_r) else gnode.d.bits.data(63, 0)
    when (state === sResp) {
      gnode.d.ready := true.B
      when (gnode.d.fire) {
        buffer(word_idx) := word_data
        word_idx := word_idx + 1.U
        state    := Mux(word_idx + 1.U === num_words, sFire, sReq)
      }
    }

    // Unpack the native codebook (16 entries of ent_bits each per table) into the physical LUT slots
    // (rdataW = numBits/16), low-aligning narrow formats so storage is config-independent.
    val rdataW  = numBits / 16
    val bufBits = buffer.asUInt
    def buildTables(ew: Int): UInt =
      Cat((0 until numEntries).reverse.map { t =>
        Cat((0 until 16).reverse.map { e =>
          val lo = t * (ew * 16) + e * ew
          bufBits(lo + ew - 1, lo).pad(rdataW)
        })
      })
    when (state === sFire) {
      out.valid     := true.B
      out.bits.data := (if (rdataW >= 8) Mux(ent_bits === 8.U, buildTables(8), buildTables(6))
                        else buildTables(rdataW)).asTypeOf(Vec(numEntries, UInt(numBits.W)))
      when (out.fire) { state := sIdle }
    }

    lut_loader_busy := state =/= sIdle
    (Some(out), Some(sel_out), Some(start))
  } else (None, None, None)

  if (outer.config.use_mx_scaling) {
    if (outer.config.use_shared_ext_mem) {
      val b = mx_io.get
      spad.module.io.scale_mem_write_w.get   <> b.scale_mem_write_w
      spad.module.io.scale_mem_write_act.get <> b.scale_mem_write_act
      mx_requantizer.get.io.requant_data_in_gpu <> b.requant_in_gpu
      mx_requantizer.get.io.requant_data_out    <> b.requant_out
      b.scale_factor_out                        <> mx_requantizer.get.io.scaleMem_write
      mx_requantizer.get.io.lut0_write <> b.lut0
      mx_requantizer.get.io.lut1_write <> b.lut1
      mx_requantizer.get.io.lut2_write <> b.lut2
    } else {
      // scale-mem write source is the funct-27 DMA loader (idle -> valid=false, no spurious writes).
      spad.module.io.scale_mem_write_w.get   <> scale_loader_w.get
      // Act-scale write source mux, arbitrated by activity: the requantizer's transposed act flush drives
      // the port only while it has data (scale_resident && flush.valid); otherwise the funct-27 loader does,
      // so an A-scale load is never blocked. The two are temporally disjoint in a chain.
      locally {
        val sc_act_dst = spad.module.io.scale_mem_write_act.get
        val sc_act_res = mx_requantizer.get.io.scaleMem_write_act_resident
        val sc_act_ld  = scale_loader_act.get
        when (ex_controller.io.mx.get.scale_resident && sc_act_res.valid) {
          sc_act_dst.valid := sc_act_res.valid
          sc_act_dst.bits  := sc_act_res.bits
          sc_act_res.ready := sc_act_dst.ready
          sc_act_ld.ready  := false.B
        } .otherwise {
          sc_act_dst.valid := sc_act_ld.valid
          sc_act_dst.bits  := sc_act_ld.bits
          sc_act_ld.ready  := sc_act_dst.ready
          sc_act_res.ready := false.B
        }
      }
      mx_requantizer.get.io.requant_data_in_gpu <> mmio_requant_in_gpu.get
      mx_requantizer.get.io.requant_data_out    <> mmio_requant_out.get
      mmio_scale_factor_out.get                 <> mx_requantizer.get.io.scaleMem_write
      // LUT write source: the funct-29 DMA loader fires the sel'd port; the other two stay idle.
      val r0 = mx_requantizer.get.io.lut0_write
      val r1 = mx_requantizer.get.io.lut1_write
      val r2 = mx_requantizer.get.io.lut2_write
      Seq((r0, 0), (r1, 1), (r2, 2)).foreach { case (port, n) =>
        port.valid := lut_out.get.valid && (lut_out_sel.get === n.U)
        port.bits  := lut_out.get.bits
      }
      lut_out.get.ready := Mux(lut_out_sel.get === 0.U, r0.ready,
                           Mux(lut_out_sel.get === 1.U, r1.ready, r2.ready))
    }

    spad.module.io.enable_MXQuant := ex_controller.io.mx.get.enable_MXQuant
    store_controller.io.enable_wide_spad_write := !ex_controller.io.mx.get.enable_MXQuant

    spad.module.io.weight_mx_format := ex_controller.io.mx.get.weight_mx_format_out
    spad.module.io.act_mx_format := ex_controller.io.mx.get.activation_mx_format_out
    spad.module.io.mx_multi_elem := ex_controller.io.mx.get.mx_multi_elem
  } else {
    // Non-MX build: drive the (unconditional) MX ports to inert defaults.
    spad.module.io.enable_MXQuant := false.B
    store_controller.io.enable_wide_spad_write := false.B

    spad.module.io.weight_mx_format := 0.U
    spad.module.io.act_mx_format := 0.U
    spad.module.io.mx_multi_elem := false.B
  }


  /* 
    Execute Controller - Controller - Spad
                            |
                      (MxRequantizer)

    read_projected : receives read request from execute controller (sp_width full)
    sram_read_buffer : buffers the reads from spad (sp_width projected). 
                       
    If !mx_sel(bank) then wire sram_read_buffer to read_projected. Otherwise wire 
    through mxrequantizer.
   */

  // read assignments

  val spad_read = if (outer.config.use_mx_scaling) {
    val read_projected = Wire(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected)))
    val mx_sel = Wire(Vec(sp_banks, Bool()))
    val sram_read_buffer = Wire(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)))

    for (b <- 0 until sp_banks) {
      mx_sel(b) := false.B
      when(read_projected(b).resp.valid) {
        // Per-operand routing: act reads gate on act fmt, weight/other reads on weight fmt.
        val opfmt = Mux(read_projected(b).resp.bits.read_a,
          ex_controller.io.mx.get.activation_mx_format_out,
          ex_controller.io.mx.get.weight_mx_format_out)
        val walt = Mux(read_projected(b).resp.bits.read_a,
          ex_controller.io.mx.get.mx_fp8_altfmt_out,
          ex_controller.io.mx.get.weight_mx_altfmt_out)
        val oplut = Mux(read_projected(b).resp.bits.read_a, mx_lut_a_en, mx_lut_b_en)
        mx_sel(b) := (opfmt === 1.U) ||
          (opfmt === 0.U && walt) ||
          (e4m3QuadThroughput.B && oplut && opfmt === 0.U && !walt)
      }

      // req to srams
      read_projected(b).req <> sram_read_buffer(b).req
      spad.module.io.srams.read(b).req <> read_projected(b).req

      // resp from srams fp4/8
      read_projected(b).resp.bits.fromDMA := spad.module.io.srams.read(b).resp.bits.fromDMA
      read_projected(b).resp.bits.weight_mx_format := spad.module.io.srams.read(b).resp.bits.weight_mx_format
      read_projected(b).resp.bits.input_mx_format := spad.module.io.srams.read(b).resp.bits.input_mx_format
      // carry the operand-kind tag (aligned with the read data) to the requantizer/QuantLut
      read_projected(b).resp.bits.read_a := spad.module.io.srams.read(b).resp.bits.read_a
      read_projected(b).resp.bits.read_d := spad.module.io.srams.read(b).resp.bits.read_d
      read_projected(b).resp.valid := spad.module.io.srams.read(b).resp.valid
      read_projected(b).resp.bits.data := spad.module.io.srams.read(b).resp.bits.data
      spad.module.io.srams.read(b).resp.ready := read_projected(b).resp.ready

      // resp from srams fp6
      mx_requantizer.get.io.spad_deprojected_data(b).req <> DontCare
      mx_requantizer.get.io.spad_projected_data(b).req <> DontCare

      // resp to ex
      val useMxB = mx_requantizer.isDefined.B && !mx_sel(b)

      // FP8 mode: bypass requantizer
      when (useMxB) {
        // Direct-path expansion, per operand: act uses inputType width + act fmt, weight uses weightType
        // width + weight fmt. E4M3 -> low 8 of the lane; FP4 (dual 4-bit) -> low 4 of each half-lane.
        val proj = read_projected(b).resp
        val spad_data_vec = proj.bits.data.asTypeOf(Vec(16, UInt(8.W)))
        def expand(laneW: Int, fmt: UInt): UInt = {
          val slotW = laneW / 2
          val out = WireInit(0.U(sp_width.W))
          when(fmt === 0.U) {
            out := VecInit(spad_data_vec.map(_.pad(laneW))).asUInt
          }.elsewhen(fmt === 2.U) {
            out := VecInit(spad_data_vec.map(byte => Cat(byte(7, 4).pad(slotW), byte(3, 0).pad(slotW)))).asUInt
          }
          out
        }
        val padded_data = Mux(proj.bits.read_a,
          expand(outer.config.inputType.getWidth,  ex_controller.io.mx.get.activation_mx_format_out),
          expand(outer.config.weightType.getWidth, ex_controller.io.mx.get.weight_mx_format_out))
        sram_read_buffer(b).resp.valid := read_projected(b).resp.valid
        sram_read_buffer(b).resp.bits.data := padded_data
        sram_read_buffer(b).resp.bits.fromDMA := read_projected(b).resp.bits.fromDMA
        sram_read_buffer(b).resp.bits.weight_mx_format := read_projected(b).resp.bits.weight_mx_format
        sram_read_buffer(b).resp.bits.input_mx_format := read_projected(b).resp.bits.input_mx_format
        sram_read_buffer(b).resp.bits.read_a := read_projected(b).resp.bits.read_a
        sram_read_buffer(b).resp.bits.read_d := read_projected(b).resp.bits.read_d
        read_projected(b).resp.ready := sram_read_buffer(b).resp.ready
        mx_requantizer.get.io.spad_projected_data(b).resp.valid := false.B
        mx_requantizer.get.io.spad_projected_data(b).resp.bits := DontCare
        mx_requantizer.get.io.spad_deprojected_data(b).resp.ready := false.B
      }.otherwise {
        mx_requantizer.get.io.spad_projected_data(b).resp <> read_projected(b).resp
        sram_read_buffer(b).resp <> mx_requantizer.get.io.spad_deprojected_data(b).resp
      }
    }
    sram_read_buffer
  } else {
    spad.module.io.srams.read
  }


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

  val reservation_station = withClock (gated_clock) { Module(new ReservationStation(outer.config, new GemminiCmd(reservation_station_entries))) }
  counters.io.event_io.collect(reservation_station.io.counter)

  when (io.cmd.valid && io.cmd.bits.inst.funct === CLKGATE_EN && !io.busy) {
    clock_en_reg := io.cmd.bits.rs1(0)
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
    inputTypeProjected.getWidth, accType.getWidth, dma_maxbytes,
    new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits), new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new ConfigMvoutRs2(acc_scale_t_bits, 32), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ConfigExRs1(acc_scale_t_bits), new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    has_training_convs, has_max_pool, has_first_layer_optimizations, has_dw_convs) }
  else (raw_cmd, false.B)

  val (loop_cmd, loop_matmul_unroller_busy, loop_completed, loop_matmul) = withClock (gated_clock) { LoopMatmul(if (has_loop_conv) conv_cmd else raw_cmd, reservation_station.io.matmul_ld_completed, reservation_station.io.matmul_st_completed, reservation_station.io.matmul_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputTypeProjected.getWidth, accType.getWidth, dma_maxbytes, new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutSpadRs1(32, local_addr_t), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)) }
  
  if (use_mx_scaling) {
    loop_matmul.io.activation_mx_format := ex_controller.io.mx.get.activation_mx_format_out
    loop_matmul.io.mx_multi_elem := ex_controller.io.mx.get.mx_multi_elem
    loop_matmul.io.weight_mx_format := ex_controller.io.mx.get.weight_mx_format_out
    loop_matmul.io.output_mx_format := ex_controller.io.mx.get.output_MxFormat

    mx_requantizer.get.io.loop_bound_i := ex_controller.io.mx.get.scaleMemCntl.loop_bound_i
    mx_requantizer.get.io.loop_bound_j := ex_controller.io.mx.get.scaleMemCntl.loop_bound_j
    mx_requantizer.get.io.loop_bound_k := ex_controller.io.mx.get.scaleMemCntl.loop_bound_k

    store_controller.io.loop_bound_j := ex_controller.io.mx.get.loop_bounds.j
    store_controller.io.activation_mx_type := ex_controller.io.mx.get.activation_mx_format_out
    store_controller.io.mx_multi_elem := ex_controller.io.mx.get.mx_multi_elem
    store_controller.io.output_mx_type := ex_controller.io.mx.get.output_MxFormat

    mx_requantizer.get.io.read_a := ex_controller.io.read_a
    mx_requantizer.get.io.read_d := ex_controller.io.read_d
    mx_requantizer.get.io.scale_mem_counter_reset_flag := ex_controller.io.mx.get.scaleMemCntl.scale_mem_counter_reset_flag

    spad.module.io.scaleMemCntl.foreach { spadCnlt =>
    spadCnlt <> ex_controller.io.mx.get.scaleMemCntl
    }
    spad.module.io.counter_i := loop_matmul.io.counter_i
    spad.module.io.counter_j := loop_matmul.io.counter_j
    spad.module.io.counter_k := loop_matmul.io.counter_k
    spad.module.io.i := loop_matmul.io.i
    spad.module.io.j := loop_matmul.io.j
    spad.module.io.k := loop_matmul.io.k
    spad.module.io.output_mx_format := ex_controller.io.mx.get.output_MxFormat
    spad.module.io.mx_fp8_altfmt := ex_controller.io.mx.get.mx_fp8_altfmt_out
    spad.module.io.mx_lut_en := ex_controller.io.mx.get.lut_en_out
  } else {
    // Non-MX build: ex_controller.io.mx and mx_requantizer are absent, so drive
    // the (unconditional) MX ports on loop_matmul/store_controller/spad to inert
    // defaults. The loop counters still come from loop_matmul, which always has them.
    loop_matmul.io.activation_mx_format := 0.U
    loop_matmul.io.mx_multi_elem := false.B
    loop_matmul.io.weight_mx_format := 0.U
    loop_matmul.io.output_mx_format := 0.U

    store_controller.io.loop_bound_j := 0.U
    store_controller.io.activation_mx_type := 0.U
    store_controller.io.mx_multi_elem := false.B
    store_controller.io.output_mx_type := 0.U

    spad.module.io.counter_i := loop_matmul.io.counter_i
    spad.module.io.counter_j := loop_matmul.io.counter_j
    spad.module.io.counter_k := loop_matmul.io.counter_k
    spad.module.io.i := loop_matmul.io.i
    spad.module.io.j := loop_matmul.io.j
    spad.module.io.k := loop_matmul.io.k
    spad.module.io.output_mx_format := 0.U
    spad.module.io.mx_fp8_altfmt := false.B
    spad.module.io.mx_lut_en := false.B
  }
  val unrolled_cmd = Queue(loop_cmd)
  unrolled_cmd.ready := false.B
  counters.io.event_io.connectEventSignal(CounterEvent.LOOP_MATMUL_ACTIVE_CYCLES, loop_matmul_unroller_busy)

  // Wire up controllers to ROB
  reservation_station.io.alloc.valid := false.B
  reservation_station.io.alloc.bits := unrolled_cmd.bits

  val completion_io = IO(new Bundle {
    val completed = Output(loop_completed.cloneType)
  })

  completion_io.completed := loop_completed

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

  counters.io.event_io.collect(load_controller.io.counter)
  counters.io.event_io.collect(store_controller.io.counter)
  counters.io.event_io.collect(ex_controller.io.counter)

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

  ex_controller.io.cmd.valid := reservation_station.io.issue.ex.valid
  reservation_station.io.issue.ex.ready := ex_controller.io.cmd.ready
  ex_controller.io.cmd.bits := reservation_station.io.issue.ex.cmd
  ex_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ex.rob_id)

  // Wire up scratchpad to controllers
  spad.module.io.dma.read <> load_controller.io.dma
  spad.module.io.dma.write <> store_controller.io.dma

  if (outer.config.use_mx_scaling) {
    spad.module.io.loop_bounds := ex_controller.io.mx.get.loop_bounds

    // Connect accumulator memory to mxrequantizer
    // default
    spad.module.io.mx_req_io <> mx_requantizer.get.io.mxacc_req

    // Writing to spad from ex
    for (i <- 0 until outer.config.sp_banks) {
      spad.module.io.srams.write(i).addr := ex_controller.io.srams.write(i).addr  // Default assignments
      spad.module.io.srams.write(i).mask := ex_controller.io.srams.write(i).mask.take(16)

      ex_controller.io.srams.write(i).ready := spad.module.io.srams.write(i).ready
      spad.module.io.srams.write(i).valid := ex_controller.io.srams.write(i).valid

      val spad_data_vec = ex_controller.io.srams.write(i).data.asTypeOf(Vec(16, UInt(12.W)))
      spad.module.io.srams.write(i).data := VecInit(spad_data_vec.map(a => a(7, 0))).asUInt
    }

  } else {
    ex_controller.io.srams.read <> spad.module.io.srams.read
    ex_controller.io.srams.write <> spad.module.io.srams.write

    spad.module.io.loop_bounds := DontCare
    spad.module.io.mx_req_io.mx_data_out.valid := false.B
    spad.module.io.mx_req_io.mx_data_out.bits := DontCare
    spad.module.io.mx_req_io.mx_data_in.ready := false.B
  }
  spad.module.io.acc.read_req <> ex_controller.io.acc.read_req
  ex_controller.io.acc.read_resp <> spad.module.io.acc.read_resp
  ex_controller.io.acc.write <> spad.module.io.acc.write

  // Im2Col unit
  val im2col = withClock (gated_clock) { Module(new Im2Col(outer.config)) }

  // Wire up Im2col
  counters.io.event_io.collect(im2col.io.counter)
  // im2col.io.sram_reads <> spad.module.io.srams.read
  im2col.io.req <> ex_controller.io.im2col.req
  ex_controller.io.im2col.resp <> im2col.io.resp

  // Wire arbiter for ExecuteController and Im2Col scratchpad reads
  (ex_controller.io.srams.read, im2col.io.sram_reads, spad_read).zipped.foreach { case (ex_read, im2col_read, spad_read) =>
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

  reservation_station_completed_arb.io.in(0).valid := ex_controller.io.completed.valid
  reservation_station_completed_arb.io.in(0).bits := ex_controller.io.completed.bits

  reservation_station_completed_arb.io.in(1) <> load_controller.io.completed
  reservation_station_completed_arb.io.in(2) <> store_controller.io.completed

  // mux with cisc frontend arbiter
  reservation_station_completed_arb.io.in(0).valid := ex_controller.io.completed.valid // && !is_cisc_mode
  reservation_station_completed_arb.io.in(1).valid := load_controller.io.completed.valid // && !is_cisc_mode
  reservation_station_completed_arb.io.in(2).valid := store_controller.io.completed.valid // && !is_cisc_mode

  reservation_station.io.completed.valid := reservation_station_completed_arb.io.out.valid
  reservation_station.io.completed.bits := reservation_station_completed_arb.io.out.bits
  reservation_station_completed_arb.io.out.ready := true.B

  // Wire up global RoCC signals
  io.busy := raw_cmd.valid || loop_conv_unroller_busy || loop_matmul_unroller_busy || reservation_station.io.busy || spad.module.io.busy || unrolled_cmd.valid || loop_cmd.valid || conv_cmd.valid || scale_loader_busy || lut_loader_busy

  io.interrupt := tlb.io.exp.map(_.interrupt).reduce(_ || _)

  // assert(!io.interrupt, "Interrupt handlers have not been written yet")

  // Cycle counters
  val incr_ld_cycles = load_controller.io.busy && !store_controller.io.busy && !ex_controller.io.busy
  val incr_st_cycles = !load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ex_cycles = !load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_cycles = load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ld_ex_cycles = load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy
  val incr_st_ex_cycles = !load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_ex_cycles = load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_CYCLES, incr_ld_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_CYCLES, incr_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_EX_CYCLES, incr_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_CYCLES, incr_ld_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_EX_CYCLES, incr_ld_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_EX_CYCLES, incr_st_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_EX_CYCLES, incr_ld_st_ex_cycles)

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
    val is_mx_load_scales = risc_funct === MX_LOAD_SCALES
    val is_mx_load_lut = risc_funct === MX_LOAD_LUT
    val is_mx_lut_disable = risc_funct === MX_LUT_DISABLE

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
      counters.io.in.valid := unrolled_cmd.valid
      unrolled_cmd.ready := counters.io.in.ready
      counters.io.in.bits := unrolled_cmd.bits.cmd
    }

    .elsewhen (is_clock_gate_en) {
      unrolled_cmd.ready := true.B
    }

    .elsewhen (is_mx_lut_disable) {
      mx_lut_en := false.B
      mx_lut_a_en := false.B
      mx_lut_b_en := false.B
      unrolled_cmd.ready := true.B
    }

    .otherwise {
      if (outer.use_mx_mmio) {
        // funct-27 MX_LOAD_SCALES kicks the DMA loader instead of the reservation station; io.busy holds
        // via scale_loader_busy until the load finishes, so gemmini_fence orders it.
        when (is_mx_load_scales) {
          scale_loader_start.get.valid     := unrolled_cmd.valid
          scale_loader_start.get.bits.addr := unrolled_cmd.bits.cmd.rs1(coreMaxAddrBits - 1, 0)
          scale_loader_start.get.bits.len  := unrolled_cmd.bits.cmd.rs2(31, 0)
          scale_loader_start.get.bits.sel  := unrolled_cmd.bits.cmd.rs2(32).asBool
          unrolled_cmd.ready := scale_loader_start.get.ready
        } .elsewhen (is_mx_load_lut) {
          lut_loader_start.get.valid     := unrolled_cmd.valid
          lut_loader_start.get.bits.addr := unrolled_cmd.bits.cmd.rs1(coreMaxAddrBits - 1, 0)
          lut_loader_start.get.bits.num  := unrolled_cmd.bits.cmd.rs2(31, 0)
          lut_loader_start.get.bits.sel  := unrolled_cmd.bits.cmd.rs2(33, 32)
          lut_loader_start.get.bits.entry_bits := unrolled_cmd.bits.cmd.rs2(39, 34)
          unrolled_cmd.ready := lut_loader_start.get.ready
          when (unrolled_cmd.valid && lut_loader_start.get.ready) {
            mx_lut_en := true.B
            val lsel = unrolled_cmd.bits.cmd.rs2(33, 32)   // 1=act, 0=weight, 2=output
            when (lsel === 1.U) { mx_lut_a_en := true.B }
            .elsewhen (lsel === 0.U) { mx_lut_b_en := true.B }
          }
        } .otherwise {
          reservation_station.io.alloc.valid := true.B
          when(reservation_station.io.alloc.fire) { unrolled_cmd.ready := true.B }
        }
      } else {
        reservation_station.io.alloc.valid := true.B
        when(reservation_station.io.alloc.fire) {
          unrolled_cmd.ready := true.B
        }
      }
    }
  }

  // Debugging signals
  val pipeline_stall_counter = RegInit(0.U(32.W))
  when (io.cmd.fire) {
    pipeline_stall_counter := 0.U
  }.elsewhen(io.busy) {
    pipeline_stall_counter := pipeline_stall_counter + 1.U
  }
  assert(pipeline_stall_counter < 10000000.U, "pipeline stall")

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

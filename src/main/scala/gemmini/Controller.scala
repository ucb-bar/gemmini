
package gemmini

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.{BundleField, ClockGate}
import freechips.rocketchip.tilelink._
import GemminiISA._
import Util._

class GemminiCmd(rob_entries: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val rob_id = UDValid(UInt(log2Up(rob_entries).W))
  val from_matmul_fsm = Bool()
  val from_conv_fsm = Bool()
}

class DPTLRAM(address: AddressSet, beatBytes: Int)
             (implicit p: Parameters) extends LazyModule {
  val r = TLManagerNode(Seq(
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v2(
      name = Some(s"spad_read_mgr"),
      address = Seq(address),
      supports = TLMasterToSlaveTransferSizes(
        get = TransferSizes(1, beatBytes),
      ),
      fifoId = Some(0)
    )), beatBytes = beatBytes)
  ))

  val w = TLManagerNode(Seq(
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v2(
      name = Some(s"spad_write_mgr"),
      address = Seq(address),
      supports = TLMasterToSlaveTransferSizes(
        putFull = TransferSizes(1, beatBytes),
        putPartial = TransferSizes(1, beatBytes),
      ),
      fifoId = Some(0)
    )), beatBytes = beatBytes)
  ))

  lazy val module = new LazyModuleImp(this) {
    def make_buffer[T <: Data](mem: TwoPortSyncMem[T], r_node: TLBundle, r_edge: TLEdgeIn,
                               w_node: TLBundle, w_edge: TLEdgeIn): Unit = {
      mem.io.ren := r_node.a.fire

      val data_pipe_in = Wire(DecoupledIO(mem.io.rdata.cloneType))
      data_pipe_in.valid := RegNext(mem.io.ren)
      data_pipe_in.bits := mem.io.rdata

      val metadata_pipe_in = Wire(DecoupledIO(new Bundle {
        val source = r_node.a.bits.source.cloneType
        val size = r_node.a.bits.size.cloneType
      }))
      metadata_pipe_in.valid := mem.io.ren
      metadata_pipe_in.bits.source := r_node.a.bits.source
      metadata_pipe_in.bits.size := r_node.a.bits.size

      val sram_read_backup_reg = RegInit(0.U.asTypeOf(Valid(mem.io.rdata.cloneType)))

      val data_pipe_inst = Module(new Pipeline(data_pipe_in.bits.cloneType, 1)())
      data_pipe_inst.io.in <> data_pipe_in
      val data_pipe = data_pipe_inst.io.out
      val metadata_pipe = Pipeline(metadata_pipe_in, 2)
      assert((data_pipe.valid || sram_read_backup_reg.valid) === metadata_pipe.valid)

      // data pipe is filled, but D is not ready and SRAM read came back
      when (data_pipe.valid && !r_node.d.ready && data_pipe_in.valid) {
        assert(!data_pipe_in.ready) // we should fill backup reg only if data pipe is not enqueueing
        assert(!sram_read_backup_reg.valid) // backup reg should be empty
        assert(!metadata_pipe_in.ready) // metadata should be filled previous cycle
        sram_read_backup_reg.valid := true.B
        sram_read_backup_reg.bits := mem.io.rdata
      }.otherwise {
        assert(data_pipe_in.ready || !data_pipe_in.valid) // do not skip any response
      }

      assert(metadata_pipe_in.fire || !mem.io.ren) // when requesting sram, metadata needs to be ready
      assert(r_node.d.fire === metadata_pipe.fire) // metadata dequeues iff D fires

      // when D becomes ready, and data pipe has emptied, time for backup to empty
      when (r_node.d.ready && sram_read_backup_reg.valid && !data_pipe.valid) {
        sram_read_backup_reg.valid := false.B
      }
      // must empty backup before filling data pipe
      assert(!(sram_read_backup_reg.valid && data_pipe.valid && data_pipe_in.fire))

      r_node.d.bits := r_edge.AccessAck(
        Mux(r_node.d.valid, metadata_pipe.bits.source, 0.U),
        Mux(r_node.d.valid, metadata_pipe.bits.size, 0.U),
        Mux(!data_pipe.valid, sram_read_backup_reg.bits, data_pipe.bits).asUInt)
      r_node.d.valid := data_pipe.valid || sram_read_backup_reg.valid
      // r node A is not ready only if D is not ready and both slots filled
      r_node.a.ready := r_node.d.ready && !(data_pipe.valid && sram_read_backup_reg.valid)
      data_pipe.ready := r_node.d.ready
      metadata_pipe.ready := r_node.d.ready

      // WRITE
      mem.io.wen := RegNext(w_node.a.fire)
      mem.io.wdata := RegNext(w_node.a.bits.data)
      mem.io.mask := RegNext(VecInit(w_node.a.bits.mask.asBools))

      val write_resp = Wire(Flipped(w_node.d.cloneType))
      write_resp.bits := w_edge.AccessAck(w_node.a.bits)
      write_resp.valid := w_node.a.valid
      w_node.a.ready := write_resp.ready
      w_node.d <> Queue(write_resp, 2)
    }

    val mem = TwoPortSyncMem(
      n = ((address.mask + 1) / beatBytes).toInt,
      t = UInt((beatBytes * 8).W),
      mask_len = beatBytes
    )

    val (r_node, r_edge) = r.in.head
    val (w_node, w_edge) = w.in.head

    // address format is
    // [ smem_base | bank_id | line_id | word_id | byte_offset ]
    // line_id is used to index into the SRAMs
    mem.io.raddr := (r_node.a.bits.address & address.mask.U) >> log2Ceil(beatBytes).U
    mem.io.waddr := RegNext((w_node.a.bits.address & address.mask.U) >> log2Ceil(beatBytes).U)

    make_buffer(mem, r_node, r_edge, w_node, w_edge)
  }
}

object DPTLRAM {
  def apply(address: AddressSet, beatBytes: Int)
           (implicit p: Parameters): (TLManagerNode, TLManagerNode) = {
    val dpram = LazyModule(new DPTLRAM(address, beatBytes))
    (dpram.r, dpram.w)
  }
}

class RWSplitterNode(visibility: Option[AddressSet], override val name: String = "rw_splitter")
                    (implicit p: Parameters) extends LazyModule {
  // this node accepts both read and write requests,
  // splits & arbitrates them into one client node per type of operation;
  // there will be N incoming edges, two outgoing edges, with two N:1 muxes;
  // it keeps the read and write channels fully separate to allow parallel processing.
  val node = TLNexusNode(
    clientFn = { seq =>
      val in_mapping = TLXbar.mapInputIds(seq)
      val read_src_range = IdRange(in_mapping.map(_.start).min, in_mapping.map(_.end).max)
      assert((read_src_range.start == 0) && isPow2(read_src_range.end))
      val write_src_range = read_src_range.shift(read_src_range.size)
      val visibilities = seq.flatMap(_.masters.flatMap(_.visibility))
      val unified_vis = if (visibilities.map(_ == AddressSet.everything).reduce(_ || _)) Seq(AddressSet.everything)
      else AddressSet.unify(visibilities)
      println(s"$name has input visibilities $visibilities, unified to $unified_vis")

      seq.head.v1copy(
        echoFields = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = Seq(
          TLMasterParameters.v1(
            name = s"${name}_read_client",
            sourceId = read_src_range,
            visibility = visibility.map(Seq(_)).getOrElse(unified_vis),
            supportsProbe = TransferSizes.mincover(seq.map(_.anyEmitClaims.get)),
            supportsGet = TransferSizes.mincover(seq.map(_.anyEmitClaims.get)),
            supportsPutFull = TransferSizes.none,
            supportsPutPartial = TransferSizes.none
          ),
          TLMasterParameters.v1(
            name = s"${name}_write_client",
            sourceId = write_src_range,
            visibility = visibility.map(Seq(_)).getOrElse(unified_vis),
            supportsProbe = TransferSizes.mincover(
              seq.map(_.anyEmitClaims.putFull) ++seq.map(_.anyEmitClaims.putPartial)),
            supportsGet = TransferSizes.none,
            supportsPutFull = TransferSizes.mincover(seq.map(_.anyEmitClaims.putFull)),
            supportsPutPartial = TransferSizes.mincover(seq.map(_.anyEmitClaims.putPartial))
          )
        )
      )
    },
    managerFn = { seq =>
      // val fifoIdFactory = TLXbar.relabeler()
      println(f"combined address range of $name managers: " +
        f"${AddressSet.unify(seq.flatMap(_.slaves.flatMap(_.address)))}, supports:" +
        f"${seq.map(_.anySupportClaims).reduce(_ mincover _)}")

      seq.head.v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = Seq(TLSlaveParameters.v2(
          name = Some(s"${name}_manager"),
          address = AddressSet.unify(seq.flatMap(_.slaves.flatMap(_.address))),
          supports = seq.map(_.anySupportClaims).reduce(_ mincover _),
          fifoId = Some(0),
        ))
      )
    }
  )

  lazy val module = new LazyModuleImp(this) {
    val u_out = node.out
    val u_in = node.in
    assert(u_out.length == 2, s"$name should have 2 outgoing edges but has ${u_out.length}")

    val r_out = u_out.head
    val w_out = u_out.last

    val in_src = TLXbar.mapInputIds(u_in.map(_._2.client))
    val in_src_size = in_src.map(_.end).max
    assert(isPow2(in_src_size)) // should be checked already, but just to be sure

    // arbitrate all reads into one read while assigning source prefix, same for write
    val a_arbiter_in = (u_in zip in_src).map { case ((in_node, _), src_range) =>
      val in_r: DecoupledIO[TLBundleA] =
        WireDefault(0.U.asTypeOf(Decoupled(new TLBundleA(in_node.a.bits.params.copy(
          sourceBits = log2Up(in_src_size) + 1
        )))))
      val in_w: DecoupledIO[TLBundleA] = WireDefault(0.U.asTypeOf(in_r.cloneType))

      val req_is_read = in_node.a.bits.opcode === TLMessages.Get

      (Seq(in_r.bits.user, in_r.bits.address, in_r.bits.opcode, in_r.bits.size,
        in_r.bits.mask, in_r.bits.param, in_r.bits.data)
        zip Seq(in_node.a.bits.user, in_node.a.bits.address, in_node.a.bits.opcode, in_node.a.bits.size,
        in_node.a.bits.mask, in_node.a.bits.param, in_node.a.bits.data))
        .foreach { case (x, y) => x := y }
      in_r.bits.source := in_node.a.bits.source | src_range.start.U | Mux(req_is_read, 0.U, in_src_size.U)
      in_w.bits := in_r.bits

      in_r.valid := in_node.a.valid && req_is_read
      in_w.valid := in_node.a.valid && !req_is_read
      in_node.a.ready := Mux(req_is_read, in_r.ready, in_w.ready)

      (in_r, in_w)
    }
    // we cannot use round robin because it might reorder requests, even from the same client
    val (a_arbiter_in_r_nodes, a_arbiter_in_w_nodes) = a_arbiter_in.unzip
    TLArbiter.lowest(r_out._2, r_out._1.a, a_arbiter_in_r_nodes:_*)
    TLArbiter.lowest(w_out._2, w_out._1.a, a_arbiter_in_w_nodes:_*)

    def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0) // from Xbar
    // for each unified mem node client, arbitrate read/write responses on d channel
    (u_in zip in_src).zipWithIndex.foreach { case (((in_node, in_edge), src_range), i) =>
      // assign d channel back based on source, invalid if source prefix mismatch
      val resp = Seq(r_out._1.d, w_out._1.d)
      val source_match = resp.zipWithIndex.map { case (r, i) =>
        (r.bits.source(r.bits.source.getWidth - 1) === i.U(1.W)) && // MSB indicates read(0)/write(1)
          src_range.contains(trim(r.bits.source, in_src_size))
      }
      val d_arbiter_in = resp.map(r => WireDefault(
        0.U.asTypeOf(Decoupled(new TLBundleD(r.bits.params.copy(
          sourceBits = in_node.d.bits.source.getWidth,
          sizeBits = in_node.d.bits.size.getWidth
        ))))
      ))

      (d_arbiter_in lazyZip resp lazyZip source_match).foreach { case (arb_in: DecoupledIO[TLBundleD], r, sm) =>
        (Seq(arb_in.bits.user, arb_in.bits.opcode, arb_in.bits.data, arb_in.bits.param,
          arb_in.bits.sink, arb_in.bits.denied, arb_in.bits.corrupt)
          zip Seq(r.bits.user, r.bits.opcode, r.bits.data, r.bits.param,
          r.bits.sink, r.bits.denied, r.bits.corrupt))
          .foreach { case (x, y) => x := y }
        arb_in.bits.source := trim(r.bits.source, 1 << in_node.d.bits.source.getWidth) // we can trim b/c isPow2(prefix)
        arb_in.bits.size := trim(r.bits.size, 1 << in_node.d.bits.size.getWidth) // FIXME: check truncation

        arb_in.valid := r.valid && sm
        r.ready := arb_in.ready
      }

      TLArbiter.robin(in_edge, in_node.d, d_arbiter_in:_*)
    }
  }
}

object RWSplitterNode {
  def apply()(implicit p: Parameters, valName: ValName): TLNexusNode = {
    LazyModule(new RWSplitterNode(None, name = valName.name)).node
  }
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

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()
  val xbar_client_node = TLXbar()

  val use_ext_tl_mem = config.use_shared_ext_mem && config.use_tl_ext_mem
  val num_ids = 32 // TODO (richard): move to config
  val spad_base = config.tl_ext_mem_base
  val spad_data_len = config.sp_width / 8
  val acc_data_len = config.sp_width / config.inputType.getWidth * config.accType.getWidth / 8
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
      visibility = Seq(AddressSet(spad_base + i * mem_width * mem_depth, mem_width * mem_depth - 1)),
      supportsProbe = TransferSizes(mem_width, mem_width),
      supportsGet = TransferSizes(mem_width, mem_width)
    )))
  }) else TLIdentityNode()

  val spad_write_nodes = if (use_ext_tl_mem) TLClientNode(Seq.tabulate(config.sp_banks) { i =>
    TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = s"spad_write_node_$i",
      sourceId = IdRange(0, num_ids),
      visibility = Seq(AddressSet(spad_base + i * mem_width * mem_depth, mem_width * mem_depth - 1)),
      supportsProbe = TransferSizes(mem_width, mem_width),
      supportsPutFull = TransferSizes(mem_width, mem_width),
      supportsPutPartial = TransferSizes(mem_width, mem_width)
    )))
  }) else TLIdentityNode()

  val spad_read_mgrs = if (false) TLManagerNode(Seq.tabulate(config.sp_banks) {i =>
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v2(
      name = Some(s"spad_read_mgr_$i"),
      address = Seq(AddressSet(spad_base + i * mem_width * mem_depth, mem_width * mem_depth - 1)),
      supports = TLMasterToSlaveTransferSizes(
        get = TransferSizes(1, 64)),
      fifoId = Some(0)
    )),
    beatBytes = mem_width)
  }) else TLIdentityNode()

  val spad_rw_mgrs = if (false) TLManagerNode(Seq.tabulate(config.sp_banks) { i =>
    TLSlavePortParameters.v1(Seq(TLSlaveParameters.v2(
      name = Some(s"spad_rw_mgr_$i"),
      address = Seq(AddressSet(spad_base + i * mem_width * mem_depth, mem_width * mem_depth - 1)),
      supports = TLMasterToSlaveTransferSizes(
        get = TransferSizes(1, 64),
        putFull = TransferSizes(1, 64),
        putPartial = TransferSizes(1, 64)),
      fifoId = Some(0)
    )),
    beatBytes = mem_width)
  }) else TLIdentityNode()

  (0 until config.sp_banks).map { i =>
    val (ram_r, ram_w) = DPTLRAM(
      address = AddressSet(spad_base + i * mem_width * mem_depth, mem_width * mem_depth - 1),
      beatBytes = mem_width,
    )
    val r_xbar = TLXbar(TLArbiter.lowestIndexFirst)
    val w_xbar = TLXbar(TLArbiter.lowestIndexFirst)
    r_xbar := spad_read_nodes
    w_xbar := spad_write_nodes
    val rw_splitter = RWSplitterNode()
    rw_splitter := TLFragmenter(32, 64) := stlNode
    r_xbar := rw_splitter
    w_xbar := rw_splitter

    ram_r := r_xbar
    ram_w := w_xbar
  }

  // val acc_read_nodes = if (create_tl_mem) TLClientNode(Seq.tabulate(config.acc_banks) { i =>
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = s"acc_read_node_$i", sourceId = IdRange(0, numIDs))))
  // }) else TLIdentityNode()
  // val acc_write_nodes = if (create_tl_mem) TLClientNode(Seq.tabulate(config.acc_banks) { i =>
  //   TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name = s"acc_write_node_$i", sourceId = IdRange(0, numIDs))))
  // }) else TLIdentityNode()

  // spad.xbar_node :=* TLBuffer() :=* spad_read_nodes
  // spad.xbar_node :=* TLBuffer() :=* spad_write_nodes

  // spad_read_mgrs :*= TLBuffer() :*= xbar_node
  // spad_rw_mgrs :*= TLBuffer() :*= xbar_node
  // xbar_node := TLBuffer() := TLWidthWidget(config.dma_buswidth/8) := id_node

  override lazy val module = new GemminiModule(this)
  override val tlNode = if (config.use_dedicated_tl_port) spad.id_node else TLIdentityNode()
  override val atlNode = if (config.use_dedicated_tl_port) TLIdentityNode() else spad.id_node
  // id_node := stlNode

  val node = if (config.use_dedicated_tl_port) tlNode else atlNode
}

class GemminiModule[T <: Data: Arithmetic, U <: Data, V <: Data]
    (outer: Gemmini[T, U, V])
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val ext_mem_io = if (use_shared_ext_mem && !use_tl_ext_mem)
    Some(IO(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))) else None

  // we need these 2 separate signals because ext_mem_io is not writable in this module
  val ext_mem_spad = outer.spad.module.io.ext_mem.get.spad
  val ext_mem_acc = outer.spad.module.io.ext_mem.get.acc

  // connecting to unified TL interface
  val source_counters = Seq.fill(4)(Counter(outer.num_ids))

  if (outer.use_ext_tl_mem) {
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
  // TODO(richard): bypass TLB
  val tlb = Module(new FrontendTLB(3, tlb_size, dma_maxbytes, use_tlb_register_filter, use_firesim_simulation_counters, use_shared_tlb))
  (tlb.io.clients zip outer.spad.module.io.tlb).foreach(t => t._1 <> t._2)

  tlb.io.exp.foreach(_.flush_skip := false.B)
  tlb.io.exp.foreach(_.flush_retry := false.B)

  io.ptw <> tlb.io.ptw

  counters.io.event_io.collect(tlb.io.counter)

  spad.module.io.flush := tlb.io.exp.map(_.flush()).reduce(_ || _)

  val clock_en_reg = RegInit(true.B)
  val gated_clock = if (clock_gate) ClockGate(clock, clock_en_reg, "gemmini_clock_gate") else clock
  outer.spad.module.clock := gated_clock

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

  val (conv_cmd, loop_conv_unroller_busy) = withClock (gated_clock) { LoopConv(raw_cmd, reservation_station.io.conv_ld_completed, reservation_station.io.conv_st_completed, reservation_station.io.conv_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes,
    new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits), new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new ConfigMvoutRs2(acc_scale_t_bits, 32), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ConfigExRs1(acc_scale_t_bits), new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    has_training_convs, has_max_pool, has_first_layer_optimizations, has_dw_convs) }

  val (loop_cmd, loop_matmul_unroller_busy) = withClock (gated_clock) { LoopMatmul(conv_cmd, reservation_station.io.matmul_ld_completed, reservation_station.io.matmul_st_completed, reservation_station.io.matmul_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes, new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutSpadRs1(32, local_addr_t), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)) }

  val unrolled_cmd = Queue(loop_cmd)
  unrolled_cmd.ready := false.B
  counters.io.event_io.connectEventSignal(CounterEvent.LOOP_MATMUL_ACTIVE_CYCLES, loop_matmul_unroller_busy)

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
  val ex_controller = withClock (gated_clock) { Module(new ExecuteController(xLen, tagWidth, outer.config)) }

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
  ex_controller.io.srams.read <> spad.module.io.srams.read
  ex_controller.io.srams.write <> spad.module.io.srams.write
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
  io.busy := raw_cmd.valid || loop_conv_unroller_busy || loop_matmul_unroller_busy || reservation_station.io.busy || spad.module.io.busy || unrolled_cmd.valid || loop_cmd.valid || conv_cmd.valid

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

    .otherwise {
      reservation_station.io.alloc.valid := true.B

      when(reservation_station.io.alloc.fire) {
        // compressed_cmd.ready := true.B
        unrolled_cmd.ready := true.B
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

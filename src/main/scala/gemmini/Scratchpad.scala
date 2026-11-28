package gemmini

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import Util._

class ScratchpadMemReadRequest[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val cols = UInt(16.W) // TODO don't use a magic number for the width here
  val repeats = UInt(16.W) // TODO don't use a magic number for the width here
  val scale = UInt(scale_t_bits.W)
  val has_acc_bitwidth = Bool()
  val all_zeros = Bool()
  val block_stride = UInt(16.W) // TODO magic numbers
  val pixel_repeats = UInt(8.W) // TODO magic numbers
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
  val status = new MStatus
}

class ScratchpadMemWriteRequest(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int)
                               (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val dest = UInt(1.W)

  val acc_act = UInt(Activation.bitwidth.W) // TODO don't use a magic number for the width here
  val acc_scale = UInt(scale_t_bits.W)
  val acc_igelu_qb = UInt(acc_t_bits.W)
  val acc_igelu_qc = UInt(acc_t_bits.W)
  val acc_iexp_qln2 = UInt(acc_t_bits.W)
  val acc_iexp_qln2_inv = UInt(acc_t_bits.W)
  val acc_norm_stats_id = UInt(8.W) // TODO magic number
  val max_j = UInt(8.W)

  val len = UInt(16.W) // TODO don't use a magic number for the width here
  val block = UInt(8.W) // TODO don't use a magic number for the width here

  val cmd_id = UInt(8.W) // TODO don't use a magic number here
  val status = new MStatus

  // Pooling variables
  val pool_en = Bool()
  val store_en = Bool()

  val chunk_id = UInt(GemminiISA.MX_CHUNK_ID_BITS.W)

  val activation_mx_type = UInt(2.W)
  val mx_multi_elem = Bool()   // WEIGHT (output-column) throughput: 2 cols/lane iff quad weight
  val mx_multi_elem_act = Bool()   // ACTIVATION (output-row) throughput: 2 rows/lane iff quad act
  val output_mx_type = UInt(2.W)

  // When set, a requant->spad beat lands within-tile-row-inner (beat << 4) instead of flat +beat.
  val reuse_tiled = Bool()
  // half the store's row step (DRAM: bytes, spad: rows) = the output row pitch when an act-quad acc row holds 2 rows
  val half_pitch = UInt(coreMaxAddrBits.W)
  val skip_second = Bool()   // act-quad split: this acc row's second C row is past the end (odd C rows)
}

class WriteReqExpander(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int, fullRowLen: Int = 16,
                       quadRowLen: Int = 16, chunkEntries: Int = 8, partialChunks: Boolean = false)
                      (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits)))
    val out = Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits))
    val byte_codes = Input(Bool())   // 8-bit requant codes: rows 2r, 2r+1 of an acc row are separate spad rows
  })

  val second_half = RegInit(false.B)
  val is_acc_write = io.in.bits.laddr.is_acc_addr && !io.in.bits.laddr.is_garbage()
  // split the acc write into 2 output rows only when the activation is quad (mode6/7 dual tiles have 1 row)
  val is_non_fp8_acc_write = is_acc_write && io.in.bits.mx_multi_elem_act
  // act-quad stores arrive as quad; BF16: 2nd row at the half pitch, 4-bit outputs pack rows 2r, 2r+1 (same address)
  val address_second_half = Mux(io.in.bits.output_mx_type === 3.U || io.byte_codes, io.in.bits.vaddr + io.in.bits.half_pitch,
    io.in.bits.vaddr)
  // a row shorter than the mesh width (fullRowLen 4-B units) has no second row
  val second_half_invalid = io.in.bits.len < fullRowLen.U
  assert(!(io.in.valid && is_non_fp8_acc_write && !io.in.bits.mx_multi_elem), "act-quad acc store must arrive as quad")
  val out_chunk_id = Mux(is_non_fp8_acc_write, second_half.asUInt, io.in.bits.chunk_id)

  io.out.valid := io.in.valid
  io.out.bits := io.in.bits
  // acc/garbage keep 16; spad-source requests keep the real row length. Quad x quad and E4M3-single BF16 chunks carry
  // one output row (quadRowLen); a DIM32 single-weight partial chunk uses the store's own length
  val partial_chunk = partialChunks.B && is_acc_write && !io.in.bits.mx_multi_elem && !io.in.bits.mx_multi_elem_act &&
    io.in.bits.len < chunkEntries.U
  // act-quad x wei-single, odd col-tile count (DIM32 only): the last pair is half full -> the store's own length
  val partial_pair = partialChunks.B && is_non_fp8_acc_write && io.in.bits.mx_multi_elem && io.in.bits.len < quadRowLen.U
  io.out.bits.len := Mux(partial_chunk, Mux(io.in.bits.output_mx_type === 3.U, io.in.bits.len << 1, io.in.bits.len),
    Mux(partial_pair, io.in.bits.len,
    Mux(io.in.bits.laddr.is_acc_addr,
      Mux((is_non_fp8_acc_write && io.in.bits.mx_multi_elem) ||
          (!io.in.bits.mx_multi_elem_act && io.in.bits.output_mx_type === 3.U), quadRowLen.U, 16.U), io.in.bits.len)))
  io.out.bits.chunk_id := out_chunk_id
  io.out.bits.store_en := Mux(is_non_fp8_acc_write, !second_half || !(second_half_invalid || io.in.bits.skip_second), io.in.bits.store_en)
  io.out.bits.vaddr   := Mux(is_non_fp8_acc_write && second_half,
    address_second_half,
    io.in.bits.vaddr)

  io.in.ready := io.out.ready && (!is_non_fp8_acc_write || second_half)

  when (io.out.fire) {
    when (is_non_fp8_acc_write && !second_half) {
      second_half := true.B
    } .otherwise {
      second_half := false.B
    }
  }

}

class ScratchpadMemWriteResponse extends Bundle {
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadMemReadResponse extends Bundle {
  val bytesRead = UInt(16.W) // TODO magic number here
  val cmd_id = UInt(8.W) // TODO don't use a magic number here

}

class ScratchpadReadMemIO[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)(implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemReadRequest(local_addr_t, scale_t_bits))
  val resp = Flipped(Valid(new ScratchpadMemReadResponse))
}

class ScratchpadWriteMemIO(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int)
                          (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits))
  val resp = Flipped(Valid(new ScratchpadMemWriteResponse))
}

class ScratchpadReadReq(val n: Int) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val fromDMA = Bool()
  // read-operand kind travels with the request so FP6 deprojection advances with the data
  val read_a = Bool()
  val read_d = Bool()
  // [1:0] {wgt, act} codebook half, [2] deproject, [3] [2] valid (ex-issued), [5:4] fmt, [7:6] slot, [9:8] direct FP6
  val lut_halves = UInt(12.W)
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val fromDMA = Bool()
  // Read-operand kind, carried through the read pipeline aligned with `data`.
  val read_a = Bool()
  val read_d = Bool()
  val lut_halves = UInt(12.W)
}

class ScratchpadReadIO(val n: Int, val w: Int) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(n))
  val resp = Flipped(Decoupled(new ScratchpadReadResp(w)))
}

class ScratchpadWriteIO(val n: Int, val w: Int, val mask_len: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val mask = Output(Vec(mask_len, Bool()))
  val data = Output(UInt(w.W))
  def fire = valid && ready
}

class MxRequantizerAccMemDataOut[T <: Data: Arithmetic](rDataType: Vec[Vec[T]], chunk_t: Vec[Vec[T]]) extends Bundle {
  val quant_mx_data_out = rDataType.cloneType
  val full_mx_data_out = chunk_t.cloneType
  val fromDMA = Bool()
  val chunk_id = UInt(GemminiISA.MX_CHUNK_ID_BITS.W)
  val acc_bank_id = UInt(2.W)
  val is_garbage = Bool()
}

class MxRequantizerAccMemDataIn[T <: Data: Arithmetic](rDataType: Vec[Vec[T]], chunk_t: Vec[Vec[T]]) extends Bundle {
  val full_mx_data_in = chunk_t.cloneType
  val fromDMA = Bool()
  val chunk_id = UInt(GemminiISA.MX_CHUNK_ID_BITS.W)
  val acc_bank_id = UInt(2.W)
}

class MxRequantizerAccMemIO[T <: Data: Arithmetic](fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]], chunk_t: Vec[Vec[T]]) extends Bundle {
  val mx_data_out = Flipped(Decoupled(new MxRequantizerAccMemDataOut[T](rDataType, chunk_t)))
  val mx_data_in = Decoupled(new MxRequantizerAccMemDataIn[T](rDataType, chunk_t))
  val mx_mode = Output(UInt(2.W))
  val mx_fp8_altfmt = Output(Bool())   // code1 requant output: 1 = E5M2, 0 = FP6
}

class ScratchpadBank(n: Int, w: Int, aligned_to: Int, single_ported: Boolean, use_shared_ext_mem: Boolean, is_dummy: Boolean,
                     has_vpu: Boolean = false) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages

  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // How many mask bits are there?
  val mask_elem = UInt((w min (aligned_to * 8)).W) // What datatype does each mask bit correspond to?

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))
    val ext_mem = if (use_shared_ext_mem) Some(new ExtMemIO) else None
    // VPU read: owns the SRAM read port this cycle, data valid exactly one cycle later (vpu_rdata)
    val vpu_rd = Option.when(has_vpu)(Flipped(Valid(UInt(log2Ceil(n).W))))
    val vpu_rdata = Option.when(has_vpu)(Output(UInt(w.W)))
  })
  require(!has_vpu || (!is_dummy && !use_shared_ext_mem && !single_ported), "VPU needs a dual-ported SyncReadMem bank")
  val vpu_ren = io.vpu_rd.map(_.valid).getOrElse(false.B)

  val ren = io.read.req.fire
  val fromDMA = io.read.req.bits.fromDMA
  val read_a_tag = io.read.req.bits.read_a
  val read_d_tag = io.read.req.bits.read_d
  val lut_halves = io.read.req.bits.lut_halves


  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new ScratchpadReadResp(w), 1, true, true))
  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U
  // When the scratchpad is single-ported, the writes take precedence
  val singleport_busy_with_write = single_ported.B && io.write.fire

  if (is_dummy) {
    q.io.enq.valid := RegNext(ren)
    q.io.enq.bits.data := 0.U
    q.io.enq.bits.fromDMA := RegNext(fromDMA)
    q.io.enq.bits.read_a := RegNext(read_a_tag)
    q.io.enq.bits.read_d := RegNext(read_d_tag)
    q.io.enq.bits.lut_halves := RegNext(lut_halves)
    io.read.req.ready := q_will_be_empty && !singleport_busy_with_write
  } else if (use_shared_ext_mem) { // use ready-valid interface
    val ext_mem = io.ext_mem.get

    /* READ */
    // TODO (richard): the number of entries here should be configurable
    // fromDMA and the read_a/read_d tag ride the latency-tracking queue so they stay aligned with the data
    class ReadMeta extends Bundle { val fromDMA = Bool(); val read_a = Bool(); val read_d = Bool(); val lut_halves = UInt(12.W) }
    val dma_q = Module(new Queue(new ReadMeta, 4, false, true))

    // Hold reads while dma_q is full: more than 4 outstanding reads would overflow it and misroute responses.
    ext_mem.read_req.valid := q_will_be_empty && io.read.req.valid && dma_q.io.enq.ready
    ext_mem.read_req.bits := io.read.req.bits.addr
    io.read.req.ready := q_will_be_empty && ext_mem.read_req.ready && dma_q.io.enq.ready

    dma_q.io.enq.valid := ren
    dma_q.io.enq.bits.fromDMA := fromDMA
    dma_q.io.enq.bits.read_a := read_a_tag
    dma_q.io.enq.bits.read_d := read_d_tag
    dma_q.io.enq.bits.lut_halves := lut_halves
    dma_q.io.deq.ready := q.io.enq.fire
    assert(dma_q.io.enq.fire === ren, "DMA queue does not have enough entries")
    assert(dma_q.io.deq.fire === q.io.enq.fire, "fromDMA should be dequeued only when read resp comes back")

    q.io.enq.valid := ext_mem.read_resp.valid
    q.io.enq.bits.data := ext_mem.read_resp.bits
    q.io.enq.bits.fromDMA := dma_q.io.deq.bits.fromDMA
    q.io.enq.bits.read_a := dma_q.io.deq.bits.read_a
    q.io.enq.bits.read_d := dma_q.io.deq.bits.read_d
    q.io.enq.bits.lut_halves := dma_q.io.deq.bits.lut_halves
    ext_mem.read_resp.ready := q.io.enq.ready

    /* WRITE */
    // registered both ways to cut the comb path to shared memory; 2 entries keep full throughput
    val wq = Module(new Queue(ext_mem.write_req.bits.cloneType, 2))
    ext_mem.write_req <> wq.io.deq

    wq.io.enq.valid := io.write.valid
    io.write.ready := wq.io.enq.ready
    wq.io.enq.bits.addr := io.write.addr
    wq.io.enq.bits.data := io.write.data
    if (aligned_to >= w) {
      wq.io.enq.bits.mask := VecInit((~(0.U(mask_len.W))).asBools).asUInt
    } else {
      wq.io.enq.bits.mask := io.write.mask.asUInt
    }
    // assert(wq.io.enq.ready || (!io.write.en), "TODO (richard): fix this if triggered")
  } else { // use valid only interface
    val mem = SyncReadMem(n, Vec(mask_len, mask_elem))

    val raddr = io.vpu_rd.map(v => Mux(v.valid, v.bits, io.read.req.bits.addr)).getOrElse(io.read.req.bits.addr)
    val rdata = if (single_ported) {
      assert(!(ren && io.write.fire))
      mem.read(raddr, ren && !io.write.fire).asUInt
    } else {
      mem.read(raddr, ren || vpu_ren).asUInt
    }
    io.vpu_rdata.foreach(_ := rdata)
    q.io.enq.valid := RegNext(ren)
    q.io.enq.bits.data := rdata
    q.io.enq.bits.fromDMA := RegNext(fromDMA)
    q.io.enq.bits.read_a := RegNext(read_a_tag)
    q.io.enq.bits.read_d := RegNext(read_d_tag)
    q.io.enq.bits.lut_halves := RegNext(lut_halves)

    io.read.req.ready := q_will_be_empty && !singleport_busy_with_write && !vpu_ren

    io.write.ready := true.B
    when(io.write.fire) {
      if (aligned_to >= w)
        mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), VecInit((~(0.U(mask_len.W))).asBools))
      else
        mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), io.write.mask)
    }
  }

  io.read.resp <> q.io.deq
}


class Scratchpad[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
                                                 (implicit p: Parameters, ev: Arithmetic[T]) extends LazyModule {

  import config._
  import ev._

  val maxBytes = dma_maxbytes
  val dataBits = dma_buswidth
  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val spad_w = (inputStorageType.getWidth *  block_cols)
  val acc_w = (accType.getWidth * block_cols)

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new StreamReader(config, max_in_flight_mem_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_banks * acc_bank_entries, block_rows, use_tlb_register_filter,
    use_firesim_simulation_counters))
  val fp8_chunk_bits = 8 * accType.getWidth
  // MX acc read chunk = one quad output row, drained as maxBytes Puts (DIM32 quad: one 128-B BF16 row = two 64-B Puts)
  val acc_chunk_w = {
    val cols = MxGeom(block_cols).W * meshColumns
    (accType.getWidth * cols * tileColumns) / (if (cols * tileColumns < 16) 1 else 2)
  }
  val writer_data_width = if (use_mx_scaling) { if (acc_read_full_width) fp8_chunk_bits max acc_chunk_w else 2*spad_w }
                          else                { if (acc_read_full_width) acc_w           else spad_w   }
  val writer = LazyModule(new StreamWriter(max_in_flight_mem_reqs, dataBits, maxBytes,
    writer_data_width, aligned_to, inputStorageType, block_cols, use_tlb_register_filter,
    use_firesim_simulation_counters))
  val spad_writer = Option.when(config.use_tl_ext_mem)(LazyModule(new StreamWriter(max_in_flight_mem_reqs, spad_writer_dma_width, max_spad_writer_bytes,
    writer_data_width, aligned_to, inputStorageType, block_cols, use_tlb_register_filter,
    use_firesim_simulation_counters)))

  // TODO make a cross-bar vs two separate ports a config option
  // id_node :=* reader.node
  // id_node :=* writer.node

  xbar_node := TLBuffer() := reader.node // TODO
  xbar_node := TLBuffer() := writer.node
  id_node := TLWidthWidget(config.dma_buswidth/8) := TLBuffer() := xbar_node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasCoreParameters {
    // acc_row_t = mesh-side write width; accCols = widened acc row (2*meshColumns at DIM=8), read as 2 blocks
    val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
    val accCols = if (use_mx_scaling) MxGeom(meshColumns*tileColumns).R else meshColumns
    val spad_row_t = if (use_mx_scaling) Vec(config.mxGeom.lanesPerBeat/tileColumns, Vec(tileColumns, weightStorageType))
                     else Vec(meshColumns, Vec(tileColumns, inputType))
    val numChunks = MxGeom.accChunks(use_mx_scaling, accCols*tileColumns)
    val chunk_t = if (use_mx_scaling) Vec((accCols*tileColumns)/numChunks, Vec(tileColumns, accType))
                  else Vec(meshColumns, Vec(tileColumns, accType))

    val io = IO(new Bundle {
      // DMA ports
      val dma = new Bundle {
        val read = Flipped(new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits))
        val write = Flipped(new ScratchpadWriteMemIO(local_addr_t, accType.getWidth, acc_scale_t_bits))
      }

      // SRAM ports
      val srams = new Bundle {
        val read = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w)))
        val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1)))
      }

      // Accumulator ports
      val acc = new Bundle {
        val read_req = Flipped(Vec(acc_banks, Decoupled(new AccumulatorReadReq(
          acc_bank_entries, accType, acc_scale_t.asInstanceOf[V]
        ))))
        val read_resp = Vec(acc_banks, Decoupled(new AccumulatorScaleResp(
          Vec(accCols, Vec(tileColumns, accType)),
          if (config.use_mx_scaling) Vec(config.mxGeom.lanesPerBeat/tileColumns, Vec(tileColumns, weightType)) else Vec(meshColumns, Vec(tileColumns, inputType)),
          if (config.use_mx_scaling) Vec((accCols*tileColumns)/numChunks, Vec(tileColumns, accType)) else Vec(meshColumns, Vec(tileColumns, accType))
        )))
        val write = Flipped(Vec(acc_banks, Decoupled(new AccumulatorWriteReq(
          acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType)), config.use_mx_scaling
        ))))
      }

      val ext_mem = if (use_shared_ext_mem) {
        Some(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))
      } else {
        None
      }
      val scaleMemCntl = if (config.scale_mem.isDefined) {Some(Input(new ScalingFactorCntl(meshRows*tileRows)))
      } else {
        None
      }
      val counter_i = Input(UInt(16.W))
      val counter_j = Input(UInt(16.W))
      val counter_k = Input(UInt(16.W))
      val i = Input(UInt(16.W))
      val j = Input(UInt(16.W))
      val k = Input(UInt(16.W))
      val scale_mem_write_w = config.scale_mem.map(sm => Flipped(Decoupled(new ScalingFactorWriteReq(sm))))
      val scale_mem_write_act = config.scale_mem.map(sm => Flipped(Decoupled(new ScalingFactorWriteReq(sm))))
      val scale_wr_layout_w = config.scale_mem.map(_ => Input(Valid(Bool())))   // per-load layout (valid: quad)
      val scale_fmt = Option.when(config.mx_flex_scales)(Input(new MxScaleFmtCfg))   // MX_SCALE_FMT registers
      val scale_wr_layout_act = config.scale_mem.map(_ => Input(Valid(Bool())))
      // TLB ports
      val tlb = Vec(2 + spad_writer.map(_ => 1).getOrElse(0), new FrontendTLBIO)

      // Misc. ports
      val busy = Output(Bool())
      val flush = Input(Bool())
      val counter = new CounterEventIO()
      val mx_req_io = new MxRequantizerAccMemIO(
        fullDataType = acc_row_t,
        rDataType = spad_row_t,
        chunk_t = chunk_t
      )
      val act_mx_format = Input(UInt(2.W))
      val output_mx_format = Input(UInt(2.W))
      val mx_fp8_altfmt = Input(Bool())
      val mx_lut_en = Input(Bool())   // format0/altfmt0 output: 4-bit LUT (E4M3-quad) vs 8-bit (E4M3-single)
      val mx_multi_elem = Input(Bool())   // WEIGHT (output-column) throughput: 2 cols/lane iff quad weight
      val mx_multi_elem_act = Input(Bool())   // ACTIVATION (output-row) throughput: 2 rows/lane iff quad act
      val enable_MXQuant = Input(Bool()) //determines if mxrequantizer gets used
      val loop_bounds = Input(new MaxBounds())
      // VPU: command in, busy out, banks with store data not yet written (requant drain / store pipeline)
      val vpu_cmd = Option.when(has_vpu)(Vec(vpu_params.units, Flipped(Decoupled(new gemmini.vpu.VpuCmd(log2Ceil(sp_banks * sp_bank_entries))))))
      val vpu_busy = Output(Bool())
      val vpu_unit_busy = Output(Vec(vpu_params.units, Bool()))
      val vpu_pending_banks = Output(UInt(sp_banks.W))
      // SPAD_REQUANT code rows: lowest write priority, backpressured
      val sr_write = Option.when(spad_requant_en)(Flipped(Decoupled(new SpadRowWrite(log2Ceil(sp_banks * sp_bank_entries), spad_w))))
      // has_loop_retire_counter: store data still on its way out (queues, unacked writes, unwritten rows); loads not counted
      val store_drain_busy = Option.when(has_loop_retire_counter)(Output(Bool()))
    })

    val write_dispatch_q = if (use_mx_scaling) {
      // row lengths in 4-B units: quad-weight row min(DIM, 16); quad x quad output row R (one acc chunk)
      val write_req_expander = Module(new WriteReqExpander(local_addr_t, accType.getWidth, acc_scale_t_bits,
        config.mxGeom.dim min 16, config.mxGeom.R, config.mxGeom.chunkEntries, config.mxGeom.partialChunks))
      write_req_expander.io.in <> io.dma.write.req
      write_req_expander.io.byte_codes := (if (config.use_mx_scaling)
        io.dma.write.req.bits.output_mx_type === 0.U && !io.mx_lut_en && !io.mx_fp8_altfmt else false.B)
      Queue(write_req_expander.io.out)
    } else {
      val single_write = Wire(Decoupled(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits)))
      single_write.valid := io.dma.write.req.valid
      io.dma.write.req.ready := single_write.ready
      single_write.bits := io.dma.write.req.bits
      single_write.bits.chunk_id := (numChunks - 1).U
      Queue(single_write)
    }

    // Write norm/scale queues are necessary to maintain in-order requests to accumulator norm/scale units
    // Writes from main SPAD just flow directly between scale_q and issue_q, while writes
    // From acc are ordered
    val write_norm_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+2))
    val write_scale_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+2))
    val write_issue_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+1, pipe=true))
    val read_issue_q = Module(new Queue(new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), spad_read_delay+1, pipe=true)) // TODO can't this just be a normal queue?

    write_dispatch_q.ready := false.B

    write_norm_q.io.enq.valid := false.B
    write_norm_q.io.enq.bits := write_dispatch_q.bits
    write_norm_q.io.deq.ready := false.B

    write_scale_q.io.enq.valid := false.B
    write_scale_q.io.enq.bits  := write_norm_q.io.deq.bits
    write_scale_q.io.deq.ready := false.B

    write_issue_q.io.enq.valid := false.B
    write_issue_q.io.enq.bits := write_scale_q.io.deq.bits

    // Garbage can immediately fire from dispatch_q -> norm_q
    when (write_dispatch_q.bits.laddr.is_garbage()) {
      write_norm_q.io.enq <> write_dispatch_q
    }

    // Non-acc or garbage can immediately fire between norm_q and scale_q
    when (write_norm_q.io.deq.bits.laddr.is_garbage() || !write_norm_q.io.deq.bits.laddr.is_acc_addr) {
      write_scale_q.io.enq <> write_norm_q.io.deq
    }

    // Non-acc or garbage can immediately fire between scale_q and issue_q
    when (write_scale_q.io.deq.bits.laddr.is_garbage() || !write_scale_q.io.deq.bits.laddr.is_acc_addr) {
      write_issue_q.io.enq <> write_scale_q.io.deq
    }
     val acc_scale_unit = Module(new AccumulatorScale(
      Vec(accCols, Vec(tileColumns, accType)),
      spad_row_t,
      acc_scale_t.asInstanceOf[V],
      acc_read_small_width,
      acc_read_full_width,
      acc_scale_func,
      acc_scale_num_units,
      acc_scale_latency,
      has_nonlinear_activations,
      has_normalizations, config.use_mx_scaling
    ))
    // write width from accCols (the widened row), so DIM=8 BF16 stores walk all 8 beats; same as acc_w at DIM16/32
    val acc_write_w = if (use_mx_scaling) acc_chunk_w else acc_w
    val requant_fp8_beats = (config.requantizer.map(_ => config.mxGeom.lanesPerBeat * 8).getOrElse(2*spad_w) / spad_w) max 1
    // spad rows per acc row on the non-subbyte acc -> spad store path (non-MX: the scaled row is exactly one spad row)
    val acc_spad_beats = if (use_mx_scaling) requant_fp8_beats else 1
    val writeData = Wire(Valid(UInt((spad_w max acc_write_w).W)))
    // FP4/FP6 first beat of a pair: dequeue its store entry without writing
    val out_garbage = acc_scale_unit.io.out.valid && acc_scale_unit.io.out.bits.fromDMA && acc_scale_unit.io.out.bits.is_garbage
    writeData.valid := write_issue_q.io.deq.bits.laddr.is_garbage()
    writeData.bits := DontCare
    val fullAccWriteData = Wire(UInt(acc_write_w.W))
    fullAccWriteData := DontCare

    // requant FP8 output -> spad banks via a bio.write source (2-beat write, consume on beat 2); non-MX STORE_SPAD
    // uses the same path, one acc row per spad row
    val requant_to_spad = if (ex_write_to_spad && !config.use_tl_ext_mem) {
      write_issue_q.io.deq.valid && write_issue_q.io.deq.bits.dest.asBool &&
        write_issue_q.io.deq.bits.laddr.is_acc_addr && !write_issue_q.io.deq.bits.laddr.is_garbage()
    } else false.B
    val requant_spad_consume = WireDefault(false.B)
    // Set inside spad_mems (where requant_q lives); read in the acc_mems read loop to throttle the requant read.
    val requant_read_stall = WireDefault(false.B)
    // FP4/FP6 emit a garbage 1st beat per 2-fire result: skip the bank write but still drain+dequeue.
    val requant_garbage = requant_to_spad && acc_scale_unit.io.out.valid &&
      acc_scale_unit.io.out.bits.fromDMA && acc_scale_unit.io.out.bits.is_garbage

    val writeData_is_full_width = if (use_mx_scaling) {
      !write_issue_q.io.deq.bits.laddr.is_garbage() && (!io.enable_MXQuant)
    } else {
      !write_issue_q.io.deq.bits.laddr.is_garbage() &&
        write_issue_q.io.deq.bits.laddr.is_acc_addr &&
        write_issue_q.io.deq.bits.laddr.read_full_acc_row
    }
    val writeData_is_all_zeros = write_issue_q.io.deq.bits.laddr.is_garbage()

    writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid && !write_issue_q.io.deq.bits.dest.asBool && !out_garbage
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.physical := write_issue_q.io.deq.bits.dest
    writer.module.io.req.bits.len := (if (use_mx_scaling) {
      // Spad-source (bf16 drain or requant codes): exactly len spad elements, no overrun.
      Mux(!write_issue_q.io.deq.bits.laddr.is_acc_addr,
        write_issue_q.io.deq.bits.len * (inputStorageType.getWidth / 8).U,
          Mux( writeData_is_full_width,
            write_issue_q.io.deq.bits.len * (accType.getWidth / 16).U,
            write_issue_q.io.deq.bits.len * (weightStorageType.getWidth / 4).U))
    } else {
      Mux(writeData_is_full_width,
        write_issue_q.io.deq.bits.len * (accType.getWidth / 8).U,
        write_issue_q.io.deq.bits.len * (inputType.getWidth / 8).U)
    })

    writer.module.io.req.bits.data := MuxCase(writeData.bits, Seq(
      writeData_is_all_zeros -> 0.U,
      (writeData_is_full_width && write_issue_q.io.deq.bits.laddr.is_acc_addr) -> fullAccWriteData
    ))
    writer.module.io.req.bits.block := write_issue_q.io.deq.bits.block
    writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status
    writer.module.io.req.bits.pool_en := write_issue_q.io.deq.bits.pool_en
    writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en && !out_garbage

    write_issue_q.io.deq.ready := Mux(requant_to_spad,
      requant_spad_consume,
      writer.module.io.req.ready && spad_writer.map(_.module.io.req.ready).getOrElse(true.B)) && writeData.valid

    val vaddr_offset = (write_issue_q.io.deq.bits.vaddr.asUInt << log2Ceil(config.DIM * config.weightStorageType.getWidth / 8).U).asUInt
    spad_writer.foreach { spad_writer =>
      spad_writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid && write_issue_q.io.deq.bits.dest.asBool && !out_garbage
      spad_writer.module.io.req.bits.vaddr := config.tl_ext_mem_base.U | vaddr_offset
      spad_writer.module.io.req.bits.physical := write_issue_q.io.deq.bits.dest
      spad_writer.module.io.req.bits.len := (if (use_mx_scaling) {
        Mux(writeData_is_full_width,
          write_issue_q.io.deq.bits.len * (accType.getWidth / 16).U, write_issue_q.io.deq.bits.len * (weightStorageType.getWidth / 4).U)
      } else {
        Mux(writeData_is_full_width,
          write_issue_q.io.deq.bits.len * (accType.getWidth / 8).U, write_issue_q.io.deq.bits.len * (inputType.getWidth / 8).U)
      })
      spad_writer.module.io.req.bits.data := MuxCase(writeData.bits, Seq(
        writeData_is_all_zeros -> 0.U,
        writeData_is_full_width -> fullAccWriteData
      ))
      spad_writer.module.io.req.bits.block := write_issue_q.io.deq.bits.block
      spad_writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status
      spad_writer.module.io.req.bits.pool_en := write_issue_q.io.deq.bits.pool_en
      spad_writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en && !out_garbage
    }

    io.dma.write.resp.valid := false.B
    io.dma.write.resp.bits.cmd_id := write_dispatch_q.bits.cmd_id
    when (write_dispatch_q.bits.laddr.is_garbage() && write_dispatch_q.fire) {
      io.dma.write.resp.valid := true.B
    }

    read_issue_q.io.enq <> io.dma.read.req

    val zero_writer = Module(new ZeroWriter(config, new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits)))

    when (io.dma.read.req.bits.all_zeros) {
      read_issue_q.io.enq.valid := false.B
      io.dma.read.req.ready := zero_writer.io.req.ready
    }

    zero_writer.io.req.valid := io.dma.read.req.valid && io.dma.read.req.bits.all_zeros
    zero_writer.io.req.bits.laddr := io.dma.read.req.bits.laddr
    zero_writer.io.req.bits.cols := io.dma.read.req.bits.cols
    zero_writer.io.req.bits.block_stride := io.dma.read.req.bits.block_stride
    zero_writer.io.req.bits.tag := io.dma.read.req.bits

    val zero_writer_pixel_repeater = Module(new PixelRepeater(weightStorageType, local_addr_t, block_cols, aligned_to, new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), passthrough = !has_first_layer_optimizations))
    zero_writer_pixel_repeater.io.req.valid := zero_writer.io.resp.valid
    zero_writer_pixel_repeater.io.req.bits.in := 0.U.asTypeOf(Vec(block_cols, weightStorageType))
    zero_writer_pixel_repeater.io.req.bits.laddr := zero_writer.io.resp.bits.laddr
    zero_writer_pixel_repeater.io.req.bits.len := zero_writer.io.resp.bits.tag.cols
    zero_writer_pixel_repeater.io.req.bits.pixel_repeats := zero_writer.io.resp.bits.tag.pixel_repeats
    zero_writer_pixel_repeater.io.req.bits.last := zero_writer.io.resp.bits.last
    zero_writer_pixel_repeater.io.req.bits.tag := zero_writer.io.resp.bits.tag
    zero_writer_pixel_repeater.io.req.bits.mask := {
      val n = weightStorageType.getWidth/ 8
      val mask = zero_writer.io.resp.bits.mask
      val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
      expanded
    }

    zero_writer.io.resp.ready := zero_writer_pixel_repeater.io.req.ready
    zero_writer_pixel_repeater.io.resp.ready := false.B

    reader.module.io.req.valid := read_issue_q.io.deq.valid
    read_issue_q.io.deq.ready := reader.module.io.req.ready
    reader.module.io.req.bits.vaddr := read_issue_q.io.deq.bits.vaddr
    reader.module.io.req.bits.spaddr := Mux(read_issue_q.io.deq.bits.laddr.is_acc_addr,
      read_issue_q.io.deq.bits.laddr.full_acc_addr(), read_issue_q.io.deq.bits.laddr.full_sp_addr())
    reader.module.io.req.bits.len := read_issue_q.io.deq.bits.cols
    reader.module.io.req.bits.repeats := read_issue_q.io.deq.bits.repeats
    reader.module.io.req.bits.pixel_repeats := read_issue_q.io.deq.bits.pixel_repeats
    reader.module.io.req.bits.scale := read_issue_q.io.deq.bits.scale
    reader.module.io.req.bits.is_acc := read_issue_q.io.deq.bits.laddr.is_acc_addr
    reader.module.io.req.bits.accumulate := read_issue_q.io.deq.bits.laddr.accumulate
    reader.module.io.req.bits.has_acc_bitwidth := read_issue_q.io.deq.bits.has_acc_bitwidth
    reader.module.io.req.bits.block_stride := read_issue_q.io.deq.bits.block_stride
    reader.module.io.req.bits.status := read_issue_q.io.deq.bits.status
    reader.module.io.req.bits.cmd_id := read_issue_q.io.deq.bits.cmd_id

    val (mvin_scale_in, mvin_scale_out) = VectorScalarMultiplier(
      config.mvin_scale_args,
      config.weightType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits),
      is_acc = false
    )
    val (mvin_scale_acc_in, mvin_scale_acc_out) = if (mvin_scale_shared) (mvin_scale_in, mvin_scale_out)
      else if (!has_acc_mvin) {
        // has_acc_mvin = false: no mvin into the accumulator (no bias path) -> inert stubs, no VSM/queue.
        require(config.mvin_scale_acc_args.isEmpty, "has_acc_mvin = false needs mvin_scale_acc_args = None")
        val tag_t = chiselTypeOf(reader.module.io.resp.bits)
        val cols = config.meshColumns * config.tileColumns
        val in_stub = Wire(Decoupled(new VectorScalarMultiplierReq(cols, config.accType, Bool(), tag_t)))
        val out_stub = Wire(Decoupled(new VectorScalarMultiplierResp(cols, config.accType, tag_t)))
        in_stub.ready := false.B
        out_stub.valid := false.B
        out_stub.bits := DontCare
        (in_stub, out_stub)
      } else (
      VectorScalarMultiplier(
        config.mvin_scale_acc_args,
        config.accType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits),
        is_acc = true
      )
      )
    assert(has_acc_mvin.B || !(reader.module.io.resp.valid && reader.module.io.resp.bits.is_acc),
      "mvin into the accumulator, but has_acc_mvin = false")

    mvin_scale_in.valid := reader.module.io.resp.valid && (mvin_scale_shared.B || !reader.module.io.resp.bits.is_acc ||
      (reader.module.io.resp.bits.is_acc && !reader.module.io.resp.bits.has_acc_bitwidth))

    mvin_scale_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_in.bits.in))
    mvin_scale_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_t)
    mvin_scale_in.bits.repeats := reader.module.io.resp.bits.repeats
    mvin_scale_in.bits.pixel_repeats := reader.module.io.resp.bits.pixel_repeats
    mvin_scale_in.bits.last := reader.module.io.resp.bits.last
    mvin_scale_in.bits.tag := reader.module.io.resp.bits

    val mvin_scale_pixel_repeater = Module(new PixelRepeater(weightType, local_addr_t, block_cols, aligned_to, mvin_scale_out.bits.tag.cloneType, passthrough = !has_first_layer_optimizations))
    mvin_scale_pixel_repeater.io.req.valid := mvin_scale_out.valid
    mvin_scale_pixel_repeater.io.req.bits.in := mvin_scale_out.bits.out
    mvin_scale_pixel_repeater.io.req.bits.mask := mvin_scale_out.bits.tag.mask take mvin_scale_pixel_repeater.io.req.bits.mask.size
    mvin_scale_pixel_repeater.io.req.bits.laddr := mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
    mvin_scale_pixel_repeater.io.req.bits.len := mvin_scale_out.bits.tag.len
    mvin_scale_pixel_repeater.io.req.bits.pixel_repeats := mvin_scale_out.bits.tag.pixel_repeats
    mvin_scale_pixel_repeater.io.req.bits.last := mvin_scale_out.bits.last
    mvin_scale_pixel_repeater.io.req.bits.tag := mvin_scale_out.bits.tag

    mvin_scale_out.ready := mvin_scale_pixel_repeater.io.req.ready
    mvin_scale_pixel_repeater.io.resp.ready := false.B

    if (!mvin_scale_shared) {
      mvin_scale_acc_in.valid := reader.module.io.resp.valid &&
        (reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth)
      mvin_scale_acc_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_acc_in.bits.in))
      mvin_scale_acc_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_acc_t)
      mvin_scale_acc_in.bits.repeats := reader.module.io.resp.bits.repeats
      mvin_scale_acc_in.bits.pixel_repeats := 1.U
      mvin_scale_acc_in.bits.last := reader.module.io.resp.bits.last
      mvin_scale_acc_in.bits.tag := reader.module.io.resp.bits

      mvin_scale_acc_out.ready := false.B
    }

    reader.module.io.resp.ready := Mux(reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth,
      mvin_scale_acc_in.ready, mvin_scale_in.ready)

    val mvin_scale_finished = mvin_scale_pixel_repeater.io.resp.fire && mvin_scale_pixel_repeater.io.resp.bits.last
    val mvin_scale_acc_finished = mvin_scale_acc_out.fire && mvin_scale_acc_out.bits.last
    val zero_writer_finished = zero_writer_pixel_repeater.io.resp.fire && zero_writer_pixel_repeater.io.resp.bits.last

    val zero_writer_bytes_read = Mux(zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr,
      zero_writer_pixel_repeater.io.resp.bits.tag.cols * (accType.getWidth / 8).U,
      zero_writer_pixel_repeater.io.resp.bits.tag.cols * (weightStorageType.getWidth / 8).U)

    // For DMA read responses, mvin_scale gets first priority, then mvin_scale_acc, and then zero_writer
    io.dma.read.resp.valid := mvin_scale_finished || mvin_scale_acc_finished || zero_writer_finished

    // io.dma.read.resp.bits.cmd_id := MuxCase(zero_writer.io.resp.bits.tag.cmd_id, Seq(
    io.dma.read.resp.bits.cmd_id := MuxCase(zero_writer_pixel_repeater.io.resp.bits.tag.cmd_id, Seq(
      // mvin_scale_finished -> mvin_scale_out.bits.tag.cmd_id,
      mvin_scale_finished -> mvin_scale_pixel_repeater.io.resp.bits.tag.cmd_id,
      mvin_scale_acc_finished -> mvin_scale_acc_out.bits.tag.cmd_id))

    io.dma.read.resp.bits.bytesRead := MuxCase(zero_writer_bytes_read, Seq(
      // mvin_scale_finished -> mvin_scale_out.bits.tag.bytes_read,
      mvin_scale_finished -> mvin_scale_pixel_repeater.io.resp.bits.tag.bytes_read,
      mvin_scale_acc_finished -> mvin_scale_acc_out.bits.tag.bytes_read))

    io.tlb(0) <> writer.module.io.tlb
    io.tlb(1) <> reader.module.io.tlb
    spad_writer match {
      case Some(sw) => {
        io.tlb(2) <> sw.module.io.tlb
        sw.module.io.flush := io.flush
      }
      case None => {}
    }

    writer.module.io.flush := io.flush
    reader.module.io.flush := io.flush

    io.busy := writer.module.io.busy || spad_writer.map(_.module.io.busy).getOrElse(false.B) || reader.module.io.busy ||
      write_issue_q.io.deq.valid || write_norm_q.io.deq.valid || write_scale_q.io.deq.valid || write_dispatch_q.valid
    io.store_drain_busy.foreach(_ := writer.module.io.busy || spad_writer.map(_.module.io.busy).getOrElse(false.B) ||
      write_issue_q.io.deq.valid || write_norm_q.io.deq.valid || write_scale_q.io.deq.valid || write_dispatch_q.valid ||
      io.vpu_pending_banks =/= 0.U)

    val spad_mems = {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(
        sp_bank_entries, spad_w,
        aligned_to, config.sp_singleported,
        use_shared_ext_mem, is_dummy, has_vpu
      )) }
      val bank_ios = VecInit(banks.map(_.io))

      // VPUs: reads use the bank's SRAM read port (the RS keeps concurrent VPU reads on disjoint banks); writes go
      // through a per-VPU queue and win their bank; ordering vs other spad traffic is the RS's
      val vpu_wr_bank = WireDefault(VecInit(Seq.fill(sp_banks)(false.B)))
      val vpu_wr_row = WireDefault(VecInit(Seq.fill(sp_banks)(0.U(log2Ceil(sp_bank_entries).W))))
      val vpu_wr_data = WireDefault(VecInit(Seq.fill(sp_banks)(0.U(spad_w.W))))
      val vpu_wq_busy = WireDefault(VecInit(Seq.fill(vpu_params.units)(false.B)))
      val vpus = if (!has_vpu) Seq() else {
        require(spad_w == vpu_params.lanes * 16 && isPow2(sp_banks) && log2Ceil(sp_banks * sp_bank_entries) <= 15,
          "VPU: one spad row = lanes x BF16 (8 at DIM16, 4 at DIM8), <= 15-bit spad row addresses")
        val vs = Seq.tabulate(vpu_params.units)(_ => Module(new gemmini.vpu.Vpu(log2Ceil(sp_banks * sp_bank_entries), log2Ceil(sp_bank_entries), vpu_params)))
        vs.zip(io.vpu_cmd.get).foreach { case (v, c) => v.io.cmd <> c }
        val bankOf = (a: UInt) => if (sp_banks == 1) 0.U else a(log2Ceil(sp_banks * sp_bank_entries) - 1, log2Ceil(sp_bank_entries))
        val rowOf = (a: UInt) => a(log2Ceil(sp_bank_entries) - 1, 0)
        val rds = vs.flatMap(_.io.spad.rd)
        // write queues: depth 8; issue stops at 4 queued so the <= 4 writes already in the VPU pipeline always fit
        val wqs = vs.map { v =>
          val q = Module(new Queue(new gemmini.vpu.VpuWrite(log2Ceil(sp_banks * sp_bank_entries), spad_w), 8))
          q.io.enq.valid := v.io.spad.wr.valid
          q.io.enq.bits := v.io.spad.wr.bits
          assert(!v.io.spad.wr.valid || q.io.enq.ready, "VPU write queue overflow")
          v.io.stall := q.io.count >= 4.U
          q.io.deq.ready := false.B
          q
        }
        wqs.zipWithIndex.foreach { case (q, u) => vpu_wq_busy(u) := q.io.deq.valid }
        bank_ios.zipWithIndex.foreach { case (bio, i) =>
          val hit = rds.map(r => r.valid && bankOf(r.bits) === i.U)
          bio.vpu_rd.get.valid := hit.reduce(_ || _)
          bio.vpu_rd.get.bits := rowOf(Mux1H(hit, rds.map(_.bits)))
          assert(PopCount(hit) <= 1.U, "VPU: two reads to one bank in a cycle")
          val want = wqs.map(q => q.io.deq.valid && bankOf(q.io.deq.bits.addr) === i.U)
          val grant = PriorityEncoderOH(want)
          vpu_wr_bank(i) := want.reduce(_ || _)
          vpu_wr_row(i) := rowOf(Mux1H(grant, wqs.map(_.io.deq.bits.addr)))
          vpu_wr_data(i) := Mux1H(grant, wqs.map(_.io.deq.bits.data))
          wqs.zip(grant).foreach { case (q, g) => when (g) { q.io.deq.ready := true.B } }
        }
        vs.foreach(v => v.io.spad.rd.zip(v.io.spad.rdata).foreach { case (r, d) =>
          d := VecInit(bank_ios.map(_.vpu_rdata.get))(RegNext(bankOf(r.bits)))
        })
        vs
      }
      // a VPU is busy until its last write has landed
      io.vpu_unit_busy := VecInit(Seq.tabulate(vpu_params.units)(i => if (has_vpu) vpus(i).io.busy || vpu_wq_busy(i) else false.B))
      io.vpu_busy := io.vpu_unit_busy.asUInt.orR
      // Reading from the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        if (use_shared_ext_mem) {
          io.ext_mem.get.spad(i) <> bio.ext_mem.get
        }

        val ex_read_req = io.srams.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          !(bio.write.fire && config.sp_singleported.B) &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.fromDMA := false.B
          // Carry the operand-kind tag from the ExecuteController read request.
          bio.read.req.bits.read_a := ex_read_req.bits.read_a
          bio.read.req.bits.read_d := ex_read_req.bits.read_d
          bio.read.req.bits.lut_halves := ex_read_req.bits.lut_halves
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B
          bio.read.req.bits.read_a := false.B  // DMA reads are not deproj operand reads
          bio.read.req.bits.read_d := false.B
          bio.read.req.bits.lut_halves := 0.U
          when (bio.read.req.fire) {
            write_dispatch_q.ready := true.B
            write_norm_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val dma_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        dma_read_resp.valid := bio.read.resp.valid && bio.read.resp.bits.fromDMA
        dma_read_resp.bits := bio.read.resp.bits
        val ex_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        ex_read_resp.valid := bio.read.resp.valid && !bio.read.resp.bits.fromDMA
        ex_read_resp.bits := bio.read.resp.bits

        val dma_read_pipe = Module(new Queue(dma_read_resp.bits.cloneType, spad_read_delay, flow = false, pipe = true))
        val ex_read_pipe = Module(new Queue(ex_read_resp.bits.cloneType, spad_read_delay, flow = false, pipe = true))

        dma_read_pipe.io.enq <> dma_read_resp
        ex_read_pipe.io.enq <> ex_read_resp

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_read_resp.ready, ex_read_resp.ready)

        dma_read_pipe.io.deq.ready := writer.module.io.req.ready &&
          spad_writer.map(_.module.io.req.ready).getOrElse(true.B) &&
          (!write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U && // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid
            write_issue_q.io.deq.valid) && !write_issue_q.io.deq.bits.laddr.is_garbage()
        when (dma_read_pipe.io.deq.fire) {
          writeData.valid := true.B
          writeData.bits := dma_read_pipe.io.deq.bits.data
        }

        io.srams.read(i).resp <> ex_read_pipe.io.deq
      }

      // requant output -> consecutive bank rows; two write slots co-issue to different banks (DIM16/32), DIM=8 walks
      // 4 beats over slot0. Beat counter covers max(bf16 beats, requant_fp8_beats) - 1
      val requant_half = RegInit(0.U(log2Ceil((acc_write_w/spad_w) max requant_fp8_beats).W))
      val requant_dst  = WireInit(0.U.asTypeOf(local_addr_t))
      requant_dst.data := write_issue_q.io.deq.bits.vaddr
      val requant_dst_bank = requant_dst.sp_bank()
      val requant_dst_row  = requant_dst.sp_row()

      val requant_subbyte = io.output_mx_format === 1.U || io.output_mx_format === 2.U ||
                            (io.output_mx_format === 0.U && (io.mx_fp8_altfmt || io.mx_lut_en))
      val requant_bf16 = io.output_mx_format === 3.U   // non-requant BF16 out: full-width, 4 beats
      val requant_valid_fire = requant_to_spad && acc_scale_unit.io.out.valid &&
        acc_scale_unit.io.out.bits.fromDMA && !acc_scale_unit.io.out.bits.is_garbage

      // Subbyte bank-write slot
      val sbw0_valid = WireDefault(false.B); val sbw0_bank = WireDefault(requant_dst_bank)
      val sbw0_addr  = WireDefault(requant_dst_row); val sbw0_data = WireDefault(0.U(spad_w.W))
      // subbyte store: the requant output free-runs faster than the N-beat drain, so rows queue here; a full queue
      // backpressures the requantizer and the acc read
      val nbeats = requant_fp8_beats
      val sb_tiled = write_issue_q.io.deq.bits.reuse_tiled   // all subbyte formats: beat b lands b tile-rows (16) below
      // Depth gives headroom for the read->enqueue pipeline in flight when the read throttle trips (below).
      val requant_q_depth = 16
      val requant_q = Module(new Queue(new Bundle {
        val codes = UInt((nbeats * spad_w).W)
        val row   = UInt(requant_dst_row.getWidth.W)
        val bank  = UInt(requant_dst_bank.getWidth.W)
        val tiled = Bool()
      }, requant_q_depth))
      // rows within one store command get dst = base + row_idx*nbeats; row_idx resets on a new base
      val enq_cmd_base = RegInit(0.U(requant_dst_row.getWidth.W))
      val enq_row_idx  = RegInit(0.U(8.W))
      val is_new_cmd   = requant_dst_row =/= enq_cmd_base
      val this_row_idx = Mux(is_new_cmd, 0.U, enq_row_idx)
      when (requant_q.io.enq.fire) {
        enq_cmd_base := requant_dst_row
        enq_row_idx  := Mux(is_new_cmd, 1.U, enq_row_idx + 1.U)
      }
      requant_q.io.enq.valid      := requant_subbyte && requant_valid_fire
      requant_q.io.enq.bits.codes := acc_scale_unit.io.out.bits.data.asUInt
      requant_q.io.enq.bits.row   := requant_dst_row + this_row_idx * nbeats.U
      requant_q.io.enq.bits.bank  := requant_dst_bank
      requant_q.io.enq.bits.tiled := sb_tiled
      // consume acc_scale out only once the row is queued (backpressure); garbage consumes via requant_garbage
      when (requant_subbyte && requant_valid_fire) { requant_spad_consume := requant_q.io.enq.ready }
      // Throttle the acc read while the Queue lacks headroom for the read->enqueue pipeline in flight.
      requant_read_stall := requant_to_spad && requant_subbyte && (requant_q.io.count >= (requant_q_depth/2).U)

      val requant_drain = RegInit(0.U(log2Ceil(nbeats+1).W))
      val sbw0_grant = WireDefault(false.B)   // the beat won its bank's write port this cycle
      val sr_grant = WireDefault(false.B)     // the SPAD_REQUANT row won its bank's write port this cycle
      io.sr_write.foreach(_.ready := sr_grant)
      val deqVec = requant_q.io.deq.bits.codes.asTypeOf(Vec(nbeats, UInt(spad_w.W)))
      // reload on the last beat when another entry is queued: no dead cycle between entries
      val sb_next = requant_drain === 1.U && sbw0_grant && requant_q.io.count > 1.U
      when ((requant_q.io.deq.valid && requant_drain === 0.U) || sb_next) {
        requant_drain := nbeats.U
      } .elsewhen (requant_drain =/= 0.U && sbw0_grant) {
        requant_drain := requant_drain - 1.U
      }
      requant_q.io.deq.ready := requant_drain === 1.U && sbw0_grant   // pop the head after its last beat
      // rows buffered in requant_q per bank (written after the store command may have completed)
      val requant_pending = RegInit(VecInit(Seq.fill(sp_banks)(0.U(log2Ceil(requant_q_depth + 1).W))))
      requant_pending.zipWithIndex.foreach { case (c, i) =>
        val inc = requant_q.io.enq.fire && requant_q.io.enq.bits.bank === i.U
        val dec = requant_q.io.deq.fire && requant_q.io.deq.bits.bank === i.U
        c := c + inc.asUInt - dec.asUInt
      }
      // + the destination bank of every spad store between its acc read and its last bank write (write_norm_q in to
      // write_issue_q out); DRAM stores never land in the scratchpad
      def store_bank_oh(r: ScratchpadMemWriteRequest): UInt = {
        val la = WireInit(0.U.asTypeOf(local_addr_t))
        la.data := r.vaddr
        UIntToOH(la.sp_bank(), sp_banks)
      }
      val st_in = Mux(write_norm_q.io.enq.fire && write_norm_q.io.enq.bits.dest.asBool,
        store_bank_oh(write_norm_q.io.enq.bits), 0.U(sp_banks.W))
      val st_out = Mux(write_issue_q.io.deq.fire && write_issue_q.io.deq.bits.dest.asBool,
        store_bank_oh(write_issue_q.io.deq.bits), 0.U(sp_banks.W))
      val store_pending = RegInit(VecInit(Seq.fill(sp_banks)(0.U(6.W))))
      store_pending.zipWithIndex.foreach { case (c, i) => c := c + st_in(i).asUInt - st_out(i).asUInt }
      val store_banks_in_flight = VecInit(store_pending.map(_ =/= 0.U)).asUInt
      io.vpu_pending_banks := VecInit(requant_pending.map(_ =/= 0.U)).asUInt | store_banks_in_flight
      val drain_beat = nbeats.U - requant_drain
      sbw0_valid := requant_drain =/= 0.U
      sbw0_bank  := requant_q.io.deq.bits.bank
      // tiled: beat b = K-tile b of the operand-A layout, b K-tile heights below
      sbw0_addr  := requant_q.io.deq.bits.row + Mux(requant_q.io.deq.bits.tiled, (drain_beat << log2Ceil(config.mxGeom.kTileRows)).asUInt, drain_beat)
      sbw0_data  := deqVec(drain_beat)

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val vpuwrite = vpu_wr_bank(i)
        val exwrite = io.srams.write(i).valid
        io.srams.write(i).ready := bio.write.ready && !vpuwrite
        assert(!(vpuwrite && exwrite), "VPU and ExecuteController write the same spad bank")

        // val laddr = mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
        val laddr = mvin_scale_pixel_repeater.io.resp.bits.laddr

        // val dmaread = mvin_scale_out.valid && !mvin_scale_out.bits.tag.is_acc &&
        val dmaread = mvin_scale_pixel_repeater.io.resp.valid && !mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc &&
          (laddr.sp_bank() === i.U) && bio.write.ready

        // We need to make sure that we don't try to return a dma read resp from both zero_writer and either mvin_scale
        // or mvin_acc_scale at the same time. The scalers always get priority in those cases
        /* val zerowrite = zero_writer.io.resp.valid && !zero_writer.io.resp.bits.laddr.is_acc_addr &&
          zero_writer.io.resp.bits.laddr.sp_bank() === i.U && */
        val zerowrite = zero_writer_pixel_repeater.io.resp.valid && !zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr &&
          zero_writer_pixel_repeater.io.resp.bits.laddr.sp_bank() === i.U &&
          // !((mvin_scale_out.valid && mvin_scale_out.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))
          !((mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last)) &&
          bio.write.ready

        // source acc_scale_unit.io.out (registered) to avoid a comb loop through requant_spad_consume; BF16 and
        // non-subbyte FP8 drain here, subbyte codes use the queued drain above
        val requantwrite = requant_to_spad && !requant_subbyte && acc_scale_unit.io.out.valid &&
          acc_scale_unit.io.out.bits.fromDMA && !acc_scale_unit.io.out.bits.is_garbage &&
          (requant_dst_bank === i.U) && bio.write.ready
        val requant_beat0 = sbw0_valid && (sbw0_bank === i.U) && bio.write.ready
        val srwrite = io.sr_write.map(w => w.valid && (if (sp_banks == 1) true.B
          else w.bits.addr(log2Ceil(sp_banks * sp_bank_entries) - 1, log2Ceil(sp_bank_entries)) === i.U)).getOrElse(false.B)

        bio.write.valid := vpuwrite || exwrite || dmaread || zerowrite || requantwrite || requant_beat0 || srwrite

        when (vpuwrite) {
          bio.write.addr := vpu_wr_row(i)
          bio.write.data := vpu_wr_data(i)
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
        }.elsewhen (exwrite) {
          bio.write.addr := io.srams.write(i).addr
          bio.write.data := io.srams.write(i).data
          bio.write.mask := io.srams.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := laddr.sp_row()
          bio.write.data := mvin_scale_pixel_repeater.io.resp.bits.out.asUInt
          bio.write.mask := mvin_scale_pixel_repeater.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)

          mvin_scale_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.elsewhen (zerowrite) {
          bio.write.addr := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_row()
          bio.write.data := 0.U
          bio.write.mask := zero_writer_pixel_repeater.io.resp.bits.mask

          zero_writer_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.elsewhen (requantwrite) {
          // FP8 (2 beats) or non-requant BF16 (4 beats): drain into consecutive bank rows, consume last.
          val narrowVec = acc_scale_unit.io.out.bits.data.asUInt.asTypeOf(Vec(acc_spad_beats, UInt(spad_w.W)))
          val wideVec   = acc_scale_unit.io.out.bits.full_data.asUInt.asTypeOf(Vec(acc_write_w / spad_w, UInt(spad_w.W)))
          val full_last = Mux(requant_bf16, (acc_write_w / spad_w - 1).U, (acc_spad_beats - 1).U)
          // DIM32 single-weight: beats = the expander's len (4-B units) in DIM-byte spad rows (half for a partial chunk)
          val len_last  = ((write_issue_q.io.deq.bits.len << 2) >> log2Ceil(block_cols)).asUInt - 1.U
          val single_w  = !write_issue_q.io.deq.bits.mx_multi_elem && !write_issue_q.io.deq.bits.mx_multi_elem_act
          // act-quad x wei-single half-full last pair (and paired-row 8-bit codes): row shorter than the quad row
          val byte_codes_w = io.output_mx_format === 0.U && !io.mx_fp8_altfmt && !io.mx_lut_en
          val half_pair = (requant_bf16 || byte_codes_w) && write_issue_q.io.deq.bits.mx_multi_elem &&
                          write_issue_q.io.deq.bits.mx_multi_elem_act && write_issue_q.io.deq.bits.len < config.mxGeom.R.U
          // a half-full pair's len counts BF16-width units (2 cols / 4 B): 8-bit codes need half the beats
          val pair_last = Mux(requant_bf16, len_last,
            ((write_issue_q.io.deq.bits.len << 1) >> log2Ceil(block_cols)).asUInt - 1.U)
          val last_beat = if (config.mxGeom.partialChunks) Mux(single_w, len_last, Mux(half_pair, pair_last, full_last))
                          else full_last
          // reuse_tiled (8-bit E4M3): beat b lands b K-tile heights below instead of flat.
          val tiled_beat = write_issue_q.io.deq.bits.reuse_tiled && (io.output_mx_format === 0.U) && !io.mx_fp8_altfmt && !io.mx_lut_en
          bio.write.addr := requant_dst_row + Mux(tiled_beat, (requant_half << log2Ceil(config.mxGeom.kTileRows)).asUInt, requant_half)
          bio.write.data := Mux(requant_bf16, wideVec(requant_half), narrowVec(requant_half))
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
          requant_half := Mux(requant_half === last_beat, 0.U, requant_half + 1.U)
          when (requant_half === last_beat) { requant_spad_consume := true.B }
        }.elsewhen (requant_beat0) {
          sbw0_grant := true.B
          bio.write.addr := sbw0_addr
          bio.write.data := sbw0_data
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
        }.elsewhen (srwrite) {
          sr_grant := true.B
          bio.write.addr := io.sr_write.map(_.bits.addr(log2Ceil(sp_bank_entries) - 1, 0)).getOrElse(0.U)
          bio.write.data := io.sr_write.map(_.bits.data).getOrElse(0.U)
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
        }
      }
      // garbage cycle: consume and dequeue (subbyte consume is asserted per path above)
      when (requant_garbage) { requant_spad_consume := true.B }
      banks
    }

    val (acc_norm_unit_in, acc_norm_unit_out) = Normalizer(
      is_passthru = !config.has_normalizations,
      max_len = block_cols,
      num_reduce_lanes = -1,
      num_stats = 2,
      latency = 4,
      fullDataType = acc_row_t,
      scale_t = acc_scale_t,
      use_mx_scaling = config.use_mx_scaling
    )

    acc_norm_unit_in.valid := false.B
    acc_norm_unit_in.bits.len := write_norm_q.io.deq.bits.len
    acc_norm_unit_in.bits.stats_id := write_norm_q.io.deq.bits.acc_norm_stats_id
    acc_norm_unit_in.bits.cmd := write_norm_q.io.deq.bits.laddr.norm_cmd
    acc_norm_unit_in.bits.acc_read_resp := DontCare



    acc_scale_unit.io.mx_req_io <> io.mx_req_io

    val acc_waiting_to_be_scaled = write_scale_q.io.deq.valid &&
      !write_scale_q.io.deq.bits.laddr.is_garbage() &&
      write_scale_q.io.deq.bits.laddr.is_acc_addr &&
      write_issue_q.io.enq.ready

    acc_scale_unit.io.output_mx_format :=  io.output_mx_format
    acc_scale_unit.io.mx_fp8_altfmt := io.mx_fp8_altfmt
    acc_norm_unit_out.ready := acc_scale_unit.io.in.ready && acc_waiting_to_be_scaled
    acc_scale_unit.io.in.valid := acc_norm_unit_out.valid && acc_waiting_to_be_scaled
    acc_scale_unit.io.in.bits  := acc_norm_unit_out.bits

    when (acc_scale_unit.io.in.fire) {
      write_issue_q.io.enq <> write_scale_q.io.deq
    }

    acc_scale_unit.io.out.ready := false.B

    val dma_resp_ready =
      Mux(requant_to_spad,
        requant_spad_consume,
        writer.module.io.req.ready && spad_writer.map(_.module.io.req.ready).getOrElse(true.B)) &&
        write_issue_q.io.deq.bits.laddr.is_acc_addr &&
        !write_issue_q.io.deq.bits.laddr.is_garbage()
    when (acc_scale_unit.io.out.bits.fromDMA && dma_resp_ready) {
      // acc-scale result -> DMA; for requant_to_spad, consumed on the bank write's 2nd beat
      acc_scale_unit.io.out.ready := true.B
      writeData.valid := acc_scale_unit.io.out.valid
      writeData.bits  := acc_scale_unit.io.out.bits.data.asUInt
      fullAccWriteData := acc_scale_unit.io.out.bits.full_data.asUInt
    }
    for (i <- 0 until acc_banks) {
      // Send the acc-sccale result to the ExController
      io.acc.read_resp(i).valid := false.B
      io.acc.read_resp(i).bits  := acc_scale_unit.io.out.bits
      when (!acc_scale_unit.io.out.bits.fromDMA && acc_scale_unit.io.out.bits.acc_bank_id === i.U) {
        acc_scale_unit.io.out.ready := io.acc.read_resp(i).ready
        io.acc.read_resp(i).valid := acc_scale_unit.io.out.valid
      }
    }

    val acc_adders = Module(new AccPipeShared(acc_latency-1, acc_row_t, acc_banks, config.acc_clock_gating))

    val acc_mems = {
      val banks = Seq.fill(acc_banks) { Module(new AccumulatorMem(
        acc_bank_entries, acc_row_t, acc_scale_func, acc_scale_t.asInstanceOf[V],
        acc_singleported, acc_sub_banks,
        use_shared_ext_mem, use_tl_ext_mem,
        acc_latency, accType, is_dummy, config.use_mx_scaling,
        config.testConfig,
        config.scale_mem,
        meshRows,
        tileRows,
        config.acc_clock_gating,
        config.mx_flex_scales,
      )) }
      val bank_ios = VecInit(banks.map(_.io))


      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.acc_bank())

      // one ScalingFactorMem for all acc banks: counters follow global compute order
      val shared_scale_mem = config.scale_mem.filter(_ => config.use_mx_scaling).map { conf =>
        val sm = Module(new ScalingFactorMem(
          depth = conf.depth,
          sramWidth = conf.subbankLineSizeInBytes*8,
          actOutputScalingWidth = 8,
          numBanks = conf.numBanks,
          testConfig = config.testConfig,
          meshRows = meshRows,
          tileRows = tileRows,
          flexScales = config.mx_flex_scales
        ))
        sm.flex_io.foreach(_ := io.scale_fmt.get)
        sm.io.wr_layout_w := io.scale_wr_layout_w.get
        sm.io.wr_layout_act := io.scale_wr_layout_act.get
        Seq((sm.io.scale_mem_write_w, io.scale_mem_write_w.get), (sm.io.scale_mem_write_act, io.scale_mem_write_act.get)).foreach {
          case (dst, src) =>
            dst.valid := src.valid
            dst.bits  := src.bits
            dst.bits.addr := src.bits.addr(12, 0)   // per-bank port takes a 13-bit address
            src.ready := dst.ready
        }
        sm.io.dataType := io.act_mx_format
        sm.io.mx_multi_elem := io.mx_multi_elem
        sm.io.mx_multi_elem_act := io.mx_multi_elem_act
        sm.io.mx_fp8_altfmt := io.mx_fp8_altfmt
        sm.io.counter_i := io.counter_i
        sm.io.counter_j := io.counter_j
        sm.io.counter_k := io.counter_k
        sm.io.i := io.i
        sm.io.j := io.j
        sm.io.k := io.k
        sm.io.scaleMemCntl := io.scaleMemCntl.get
        val reqs = bank_ios.map(_.scale_rd_req.get)
        assert(PopCount(reqs.map(_.valid)) <= 1.U, "shared scale mem: two acc banks wrote in the same cycle")
        sm.io.read_req.valid := reqs.map(_.valid).reduce(_ || _)
        sm.io.read_req.bits  := Mux1H(reqs.map(_.valid), reqs.map(_.bits))
        sm.io.read_resp.ready := true.B
        sm
      }

      // Reading from the Accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        shared_scale_mem.foreach { sm =>
          bio.scale_rd_resp.get.valid := sm.io.read_resp.valid
          bio.scale_rd_resp.get.bits  := sm.io.read_resp.bits
        }
        bio.mx_multi_elem := io.mx_multi_elem
        bio.mx_multi_elem_act := io.mx_multi_elem_act
        bio.mx_fp8_altfmt := io.mx_fp8_altfmt

        if (use_shared_ext_mem) {
          io.ext_mem.get.acc(i) <> bio.ext_mem.get
        }

        acc_adders.io.in_sel(i) := bio.adder.valid
        acc_adders.io.ina(i) := bio.adder.op1
        acc_adders.io.inb(i) := bio.adder.op2
        bio.adder.sum := acc_adders.io.out

        val ex_read_req = io.acc.read_req(i)
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.acc_bank() === i.U &&
          // subbyte requant: throttle the acc read to the drain rate (the pipeline cannot stall; matters at DIM=8)
          !requant_read_stall

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from accumulator banks
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.act := ex_read_req.bits.act
          bio.read.req.bits.igelu_qb := ex_read_req.bits.igelu_qb
          bio.read.req.bits.igelu_qc := ex_read_req.bits.igelu_qc
          bio.read.req.bits.iexp_qln2 := ex_read_req.bits.iexp_qln2
          bio.read.req.bits.iexp_qln2_inv := ex_read_req.bits.iexp_qln2_inv
          bio.read.req.bits.scale := ex_read_req.bits.scale
          bio.read.req.bits.full := false.B
          bio.read.req.bits.fromDMA := false.B
          bio.read.req.bits.chunk_id := DontCare
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          bio.read.req.bits.full := write_dispatch_q.bits.laddr.read_full_acc_row
          bio.read.req.bits.act := write_dispatch_q.bits.acc_act
          bio.read.req.bits.igelu_qb := write_dispatch_q.bits.acc_igelu_qb.asTypeOf(bio.read.req.bits.igelu_qb)
          bio.read.req.bits.igelu_qc := write_dispatch_q.bits.acc_igelu_qc.asTypeOf(bio.read.req.bits.igelu_qc)
          bio.read.req.bits.iexp_qln2 := write_dispatch_q.bits.acc_iexp_qln2.asTypeOf(bio.read.req.bits.iexp_qln2)
          bio.read.req.bits.iexp_qln2_inv := write_dispatch_q.bits.acc_iexp_qln2_inv.asTypeOf(bio.read.req.bits.iexp_qln2_inv)
          bio.read.req.bits.scale := write_dispatch_q.bits.acc_scale.asTypeOf(bio.read.req.bits.scale)
          bio.read.req.bits.fromDMA := true.B
          bio.read.req.bits.chunk_id := write_dispatch_q.bits.chunk_id


          when (bio.read.req.fire) {
            write_norm_q.io.enq.valid := true.B
            write_norm_q.io.enq.bits := write_dispatch_q.bits
            write_dispatch_q.ready := true.B
            // FP8: one resp per StC chunk command; FP4/FP6: two sub-commands per mvout, resp on the last
            when (!write_dispatch_q.bits.mx_multi_elem_act || write_dispatch_q.bits.chunk_id === (numChunks - 1).U) {
              io.dma.write.resp.valid := true.B
            }
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }
        bio.read.resp.ready := false.B

        when (write_norm_q.io.deq.valid &&
          acc_norm_unit_in.ready &&
          bio.read.resp.valid &&
          write_scale_q.io.enq.ready &&
          write_norm_q.io.deq.bits.laddr.is_acc_addr &&
          !write_norm_q.io.deq.bits.laddr.is_garbage() &&
          write_norm_q.io.deq.bits.laddr.acc_bank() === i.U)
        {
          write_norm_q.io.deq.ready := true.B
          acc_norm_unit_in.valid := true.B
          bio.read.resp.ready := true.B

          // Some normalizer commands don't write to main memory, so they don't need to be passed on to the scaling units
          write_scale_q.io.enq.valid := NormCmd.writes_to_main_memory(write_norm_q.io.deq.bits.laddr.norm_cmd)

          acc_norm_unit_in.bits.acc_read_resp := bio.read.resp.bits
          acc_norm_unit_in.bits.acc_read_resp.acc_bank_id := i.U
        }
      }

      // Writing to the accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        // Order of precedence during writes is ExecuteController, and then mvin_scale, and then mvin_scale_acc, and
        // then zero_writer

        val exwrite = io.acc.write(i).valid
        io.acc.write(i).ready := true.B
        assert(!(exwrite && !bio.write.ready), "Execute controller write to AccumulatorMem was skipped")

        // val from_mvin_scale = mvin_scale_out.valid && mvin_scale_out.bits.tag.is_acc
        val from_mvin_scale = has_acc_mvin.B &&
          mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc
        val from_mvin_scale_acc = has_acc_mvin.B && mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.tag.is_acc

        // val mvin_scale_laddr = mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
        val mvin_scale_laddr = mvin_scale_pixel_repeater.io.resp.bits.laddr
        val mvin_scale_acc_laddr = mvin_scale_acc_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_acc_out.bits.row

        val dmaread_bank = Mux(from_mvin_scale, mvin_scale_laddr.acc_bank(),
          mvin_scale_acc_laddr.acc_bank())
        val dmaread_row = Mux(from_mvin_scale, mvin_scale_laddr.acc_row(), mvin_scale_acc_laddr.acc_row())

        // We need to make sure that we don't try to return a dma read resp from both mvin_scale and mvin_scale_acc
        // at the same time. mvin_scale always gets priority in this cases
        val spad_last = mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last && !mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc

        val dmaread = (from_mvin_scale || from_mvin_scale_acc) &&
          dmaread_bank === i.U /* &&
          (mvin_scale_same.B || from_mvin_scale || !spad_dmaread_last) */

        // We need to make sure that we don't try to return a dma read resp from both zero_writer and either mvin_scale
        // or mvin_acc_scale at the same time. The scalers always get priority in those cases
        /* val zerowrite = zero_writer.io.resp.valid && zero_writer.io.resp.bits.laddr.is_acc_addr &&
          zero_writer.io.resp.bits.laddr.acc_bank() === i.U && */
        val zerowrite = zero_writer_pixel_repeater.io.resp.valid && zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr &&
          zero_writer_pixel_repeater.io.resp.bits.laddr.acc_bank() === i.U &&
          // !((mvin_scale_out.valid && mvin_scale_out.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))
          !((mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))

        val consecutive_write_block = RegInit(false.B)
        if (acc_singleported) {
          val consecutive_write_sub_bank = RegInit(0.U((1 max log2Ceil(acc_sub_banks)).W))
          when (bio.write.fire && bio.write.bits.acc &&
            (bio.write.bits.addr(log2Ceil(acc_sub_banks)-1,0) === consecutive_write_sub_bank)) {
            consecutive_write_block := true.B
          } .elsewhen (bio.write.fire && bio.write.bits.acc) {
            consecutive_write_block := false.B
            consecutive_write_sub_bank := bio.write.bits.addr(log2Ceil(acc_sub_banks)-1,0)
          } .otherwise {
            consecutive_write_block := false.B
          }
        }
        bio.write.valid := false.B
        bio.write.bits.offset := DontCare
        bio.write.bits.scale_halves := 0.U
        bio.write.bits.scale_pos.foreach(_ := 0.U.asTypeOf(new MxScalePos))
        bio.write.bits.scale_en.foreach(_ := false.B)
        bio.write.bits.wm.multi := io.mx_multi_elem   // non-ex writes: the live mode
        bio.write.bits.wm.multi_act := io.mx_multi_elem_act
        bio.write.bits.wm.dtype := io.act_mx_format
        bio.write.bits.wm.altfmt := io.mx_fp8_altfmt

        // bio.write.bits.acc := MuxCase(zero_writer.io.resp.bits.laddr.accumulate,
        bio.write.bits.acc := MuxCase(zero_writer_pixel_repeater.io.resp.bits.laddr.accumulate,
          Seq(exwrite -> io.acc.write(i).bits.acc,
            // from_mvin_scale -> mvin_scale_out.bits.tag.accumulate,
            from_mvin_scale -> mvin_scale_pixel_repeater.io.resp.bits.tag.accumulate,
            from_mvin_scale_acc -> mvin_scale_acc_out.bits.tag.accumulate))

        // bio.write.bits.addr := MuxCase(zero_writer.io.resp.bits.laddr.acc_row(),
        bio.write.bits.addr := MuxCase(zero_writer_pixel_repeater.io.resp.bits.laddr.acc_row(),
          Seq(exwrite -> io.acc.write(i).bits.addr,
            (from_mvin_scale || from_mvin_scale_acc) -> dmaread_row))

        when (exwrite) {
          bio.write.valid := true.B
          bio.write.bits.data := io.acc.write(i).bits.data
          bio.write.bits.mask := io.acc.write(i).bits.mask
          bio.write.bits.offset := io.acc.write(i).bits.offset
          bio.write.bits.scale_halves := io.acc.write(i).bits.scale_halves
          (bio.write.bits.scale_pos zip io.acc.write(i).bits.scale_pos).foreach { case (o, p) => o := p }
          (bio.write.bits.scale_en zip io.acc.write(i).bits.scale_en).foreach { case (o, p) => o := p }
          bio.write.bits.wm := io.acc.write(i).bits.wm
        }.elsewhen (dmaread && !spad_last && !consecutive_write_block) {
          bio.write.valid := true.B
          // has_acc_mvin = false: this branch is unreachable; skip the spad->acc width converters entirely.
          bio.write.bits.data := (if (has_acc_mvin) Mux(from_mvin_scale,
            // VecInit(mvin_scale_out.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            VecInit(mvin_scale_pixel_repeater.io.resp.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            mvin_scale_acc_out.bits.out.asTypeOf(acc_row_t)) else 0.U.asTypeOf(acc_row_t))
          bio.write.bits.mask :=
            Mux(from_mvin_scale,
              {
                val n = accType.getWidth / weightStorageType.getWidth
                // val mask = mvin_scale_out.bits.tag.mask take ((spad_w / (aligned_to * 8)) max 1)
                val mask = mvin_scale_pixel_repeater.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)
                val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
                expanded
              },
              mvin_scale_acc_out.bits.tag.mask)

          when(from_mvin_scale) {
            mvin_scale_pixel_repeater.io.resp.ready := bio.write.ready
          }.otherwise {
            mvin_scale_acc_out.ready := bio.write.ready
          }
        }.elsewhen (zerowrite && !spad_last && !consecutive_write_block) {
          bio.write.valid := true.B
          bio.write.bits.data := 0.U.asTypeOf(acc_row_t)
          bio.write.bits.mask := {
            val n = accType.getWidth / weightStorageType.getWidth
            val mask = zero_writer_pixel_repeater.io.resp.bits.mask
            val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
            expanded
          }

          zero_writer_pixel_repeater.io.resp.ready := bio.write.ready
        }.otherwise {
          bio.write.bits.data := DontCare
          bio.write.bits.mask := DontCare
        }
      }
      banks
    }

    // Counter connection
    io.counter := DontCare
    io.counter.collect(reader.module.io.counter)
    io.counter.collect(writer.module.io.counter)
    spad_writer.foreach(_.module.io.counter := DontCare)
  }
}

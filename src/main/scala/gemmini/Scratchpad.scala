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

  val input_mx_format = UInt(2.W)
  val weight_mx_format = UInt(2.W)
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
  val output_mx_type = UInt(2.W)

}

class WriteReqExpander(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits)))
    val out = Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits))
  })

  val second_half = RegInit(false.B)
  val is_acc_write = io.in.bits.laddr.is_acc_addr && !io.in.bits.laddr.is_garbage()
  val is_non_fp8_acc_write = is_acc_write && io.in.bits.activation_mx_type =/= 0.U
  val gmem_multiplier = Mux(io.in.bits.dest =/= 0.U, 1.U, 16.U)

  val address_second_half_wide = io.in.bits.vaddr + (gmem_multiplier * MuxCase(4.U, Seq(
    (io.in.bits.max_j <= 2.U && io.in.bits.activation_mx_type === 0.U) -> ((acc_t_bits/16).U * 16.U),
    (io.in.bits.activation_mx_type === 1.U || io.in.bits.activation_mx_type === 2.U) -> (4.U * io.in.bits.max_j)
    )))

  val address_second_half_narrow = io.in.bits.vaddr + (gmem_multiplier * MuxCase(2.U, Seq(
    (io.in.bits.max_j <= 2.U && io.in.bits.activation_mx_type === 0.U) -> ((acc_t_bits/16).U * 16.U / 2.U),
    (io.in.bits.activation_mx_type === 1.U || io.in.bits.activation_mx_type === 2.U) -> 0.U
  )))
  val address_second_half  = Mux(io.in.bits.output_mx_type === 3.U, address_second_half_wide, address_second_half_narrow)
  val second_half_invalid = io.in.bits.len < 16.U
  val out_chunk_id = Mux(is_non_fp8_acc_write, second_half.asUInt, io.in.bits.chunk_id)

  io.out.valid := io.in.valid
  io.out.bits := io.in.bits
  io.out.bits.len := 16.U
  io.out.bits.chunk_id := out_chunk_id
  io.out.bits.store_en := Mux(is_non_fp8_acc_write, !second_half || !second_half_invalid, io.in.bits.store_en)
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
  val weight_mx_format = UInt(2.W)
  val input_mx_format = UInt(2.W)
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val fromDMA = Bool()
  val weight_mx_format = UInt(2.W)
  val input_mx_format = UInt(2.W)

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
}

class ScratchpadBank(n: Int, w: Int, aligned_to: Int, single_ported: Boolean, use_shared_ext_mem: Boolean, is_dummy: Boolean) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages

  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // How many mask bits are there?
  val mask_elem = UInt((w min (aligned_to * 8)).W) // What datatype does each mask bit correspond to?

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))
    val ext_mem = if (use_shared_ext_mem) Some(new ExtMemIO) else None
  })

  val ren = io.read.req.fire
  val fromDMA = io.read.req.bits.fromDMA
  val weight_mx_format = io.read.req.bits.weight_mx_format
  val input_mx_format = io.read.req.bits.input_mx_format


  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new ScratchpadReadResp(w), 1, true, true))
  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U
  // When the scratchpad is single-ported, the writes take precedence
  val singleport_busy_with_write = single_ported.B && io.write.fire

  if (is_dummy) {
    q.io.enq.valid := RegNext(ren)
    q.io.enq.bits.data := 0.U
    q.io.enq.bits.fromDMA := RegNext(fromDMA)
    q.io.enq.bits.weight_mx_format := RegNext(weight_mx_format)
    q.io.enq.bits.input_mx_format := RegNext(input_mx_format)
    io.read.req.ready := q_will_be_empty && !singleport_busy_with_write
  } else if (use_shared_ext_mem) { // use ready-valid interface
    val ext_mem = io.ext_mem.get

    /* READ */
    ext_mem.read_req.valid := q_will_be_empty && io.read.req.valid
    ext_mem.read_req.bits := io.read.req.bits.addr
    io.read.req.ready := q_will_be_empty && ext_mem.read_req.ready

    // TODO (richard): the number of entries here should be configurable
    val dma_q = Module(new Queue(Bool(), 4, false, true))
    dma_q.io.enq.valid := ren
    dma_q.io.enq.bits := fromDMA
    dma_q.io.deq.ready := q.io.enq.fire
    assert(dma_q.io.enq.fire === ren, "DMA queue does not have enough entries") // TODO (richard): do backpressure
    assert(dma_q.io.deq.fire === q.io.enq.fire, "fromDMA should be dequeued only when read resp comes back")

    q.io.enq.valid := ext_mem.read_resp.valid
    q.io.enq.bits.data := ext_mem.read_resp.bits
    q.io.enq.bits.fromDMA := dma_q.io.deq.bits
    q.io.enq.bits.weight_mx_format := RegNext(weight_mx_format)
    q.io.enq.bits.input_mx_format := RegNext(input_mx_format)
    ext_mem.read_resp.ready := q.io.enq.ready

    /* WRITE */
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

    val raddr = io.read.req.bits.addr
    val rdata = if (single_ported) {
      assert(!(ren && io.write.fire))
      mem.read(raddr, ren && !io.write.fire).asUInt
    } else {
      mem.read(raddr, ren).asUInt
    }
    q.io.enq.valid := RegNext(ren)
    q.io.enq.bits.data := rdata
    q.io.enq.bits.fromDMA := RegNext(fromDMA)
    q.io.enq.bits.weight_mx_format := RegNext(weight_mx_format)
    q.io.enq.bits.input_mx_format := RegNext(input_mx_format)

    io.read.req.ready := q_will_be_empty && !singleport_busy_with_write

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
  val spad_w = (inputTypeProjected.getWidth *  block_cols)
  val spad_w_deprojected = (inputType.getWidth *  block_cols)
  val acc_w = (accType.getWidth * block_cols)

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  println("maxBytes: " + maxBytes + "\n")
  println("dataBits: " + dataBits + "\n")
  println("acc_w: " + acc_w + "\n")

  val reader = LazyModule(new StreamReader(config, max_in_flight_mem_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_banks * acc_bank_entries, block_rows, use_tlb_register_filter,
    use_firesim_simulation_counters))
  val fp8_chunk_bits = 8 * accType.getWidth
  val writer_data_width = if (use_mx_scaling) { if (acc_read_full_width) fp8_chunk_bits else 2*spad_w }
                          else                { if (acc_read_full_width) acc_w           else spad_w   }
  val writer = LazyModule(new StreamWriter(max_in_flight_mem_reqs, dataBits, maxBytes,
    writer_data_width, aligned_to, inputTypeProjected, block_cols, use_tlb_register_filter,
    use_firesim_simulation_counters))
  val spad_writer = Option.when(config.use_tl_ext_mem)(LazyModule(new StreamWriter(max_in_flight_mem_reqs, spad_writer_dma_width, max_spad_writer_bytes,
    writer_data_width, aligned_to, inputTypeProjected, block_cols, use_tlb_register_filter,
    use_firesim_simulation_counters)))

  // TODO make a cross-bar vs two separate ports a config option
  // id_node :=* reader.node
  // id_node :=* writer.node

  xbar_node := TLBuffer() := reader.node // TODO
  xbar_node := TLBuffer() := writer.node
  id_node := TLWidthWidget(config.dma_buswidth/8) := TLBuffer() := xbar_node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasCoreParameters {
    val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
    val spad_row_t = if (use_mx_scaling) Vec(2*meshColumns, Vec(tileColumns, weightTypeProjected))
                     else Vec(meshColumns, Vec(tileColumns, inputType))
    val numChunks = (meshColumns * tileColumns) / 8
    val chunk_t = if (use_mx_scaling) Vec(meshColumns / numChunks, Vec(tileColumns, accType))
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
      val acc_scale_t_size = 1
      val acc = new Bundle {
        val read_req = Flipped(Vec(acc_banks, Decoupled(new AccumulatorReadReq(
          acc_bank_entries, accType, acc_scale_t.asInstanceOf[V]
        ))))
        val read_resp = Vec(acc_banks, Decoupled(new AccumulatorScaleResp(
          Vec(meshColumns, Vec(tileColumns, accType)),
          if (config.use_mx_scaling) Vec(2*meshColumns, Vec(tileColumns, weightType)) else Vec(meshColumns, Vec(tileColumns, inputType)),
          if (config.use_mx_scaling) Vec(meshColumns/2, Vec(tileColumns, accType)) else Vec(meshColumns, Vec(tileColumns, accType))
        )))
        val write = Flipped(Vec(acc_banks, Decoupled(new AccumulatorWriteReq(
          acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))
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
      val weight_mx_format = Input(UInt(2.W))
      val act_mx_format = Input(UInt(2.W))
      val output_mx_format = Input(UInt(2.W))
      val enable_MXQuant = Input(Bool()) //determines if mxrequantizer gets used
      val loop_bounds = Input(new MaxBounds())
    })

    val write_dispatch_q = if (use_mx_scaling) {
      val write_req_expander = Module(new WriteReqExpander(local_addr_t, accType.getWidth, acc_scale_t_bits))
      write_req_expander.io.in <> io.dma.write.req
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
      acc_row_t,
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
    val acc_write_w = if (use_mx_scaling) acc_w/2 else acc_w
    val writeData = Wire(Valid(UInt((spad_w max acc_write_w).W)))
    writeData.valid := write_issue_q.io.deq.bits.laddr.is_garbage() || (acc_scale_unit.io.out.bits.is_garbage)
    writeData.bits := DontCare
    val fullAccWriteData = Wire(UInt(acc_write_w.W))
    fullAccWriteData := DontCare

    // V1 (MX ex_write_to_spad): write the requant FP8 output (writeData, 256b = 2 bank rows) into
    // the INTERNAL scratchpad banks via a new bio.write source (driven in spad_mems below), the
    // standalone replacement for the absent spad_writer. requant_to_spad selects this mode; the
    // 2-beat bank write asserts requant_spad_consume on its 2nd beat to consume the requantizer out.
    val requant_to_spad = if (use_mx_scaling && ex_write_to_spad) {
      write_issue_q.io.deq.valid && write_issue_q.io.deq.bits.dest.asBool &&
        write_issue_q.io.deq.bits.laddr.is_acc_addr && !write_issue_q.io.deq.bits.laddr.is_garbage()
    } else false.B
    val requant_spad_consume = WireDefault(false.B)
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
    val writeData_is_fp8 = !write_issue_q.io.deq.bits.laddr.is_garbage() && (io.output_mx_format === 0.U)
    val writeData_is_fp4orfp6 = !write_issue_q.io.deq.bits.laddr.is_garbage() && (io.output_mx_format === 2.U || io.output_mx_format === 1.U)
    val writeData_is_all_zeros = write_issue_q.io.deq.bits.laddr.is_garbage()

    writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid && !write_issue_q.io.deq.bits.dest.asBool && (!acc_scale_unit.io.out.bits.is_garbage)
    // write_issue_q.io.deq.ready := writer.module.io.req.ready && writeData.valid
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.physical := write_issue_q.io.deq.bits.dest
    writer.module.io.req.bits.len := (if (use_mx_scaling) {
      Mux(writeData_is_full_width && !write_issue_q.io.deq.bits.laddr.is_acc_addr,
        write_issue_q.io.deq.bits.len * (weightTypeProjected.getWidth / 8).U,
          Mux( writeData_is_full_width,
            write_issue_q.io.deq.bits.len * (accType.getWidth / 16).U,
            write_issue_q.io.deq.bits.len * (weightTypeProjected.getWidth / 4).U))
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
    writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en && (!acc_scale_unit.io.out.bits.is_garbage)

    write_issue_q.io.deq.ready := Mux(requant_to_spad,
      requant_spad_consume,
      writer.module.io.req.ready && spad_writer.map(_.module.io.req.ready).getOrElse(true.B)) && writeData.valid
    
    // when (acc_scale_unit.io.out.valid && acc_scale_unit.io.out.bits.is_garbage) {
    //   acc_scale_unit.io.out.ready    := true.B   // drain scale unit
    //   write_issue_q.io.deq.ready     := true.B   // drain queue, no writer needed
    //   //spad_writer.module.io.req.valid  := false.B  // suppress writer
    //   spad_writer.foreach { sw => sw.module.io.req.valid := false.B }
    // }
    val vaddr_offset = (write_issue_q.io.deq.bits.vaddr.asUInt << log2Ceil(config.DIM * config.weightTypeProjected.getWidth / 8).U).asUInt
    spad_writer.foreach { spad_writer =>
      spad_writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid && write_issue_q.io.deq.bits.dest.asBool && (!acc_scale_unit.io.out.bits.is_garbage)
      spad_writer.module.io.req.bits.vaddr := config.tl_ext_mem_base.U | vaddr_offset
      spad_writer.module.io.req.bits.physical := write_issue_q.io.deq.bits.dest
      spad_writer.module.io.req.bits.len := (if (use_mx_scaling) {
        Mux(writeData_is_full_width,
          write_issue_q.io.deq.bits.len * (accType.getWidth / 16).U, write_issue_q.io.deq.bits.len * (weightTypeProjected.getWidth / 4).U)
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
      spad_writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en && (!acc_scale_unit.io.out.bits.is_garbage)
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

    val zero_writer_pixel_repeater = Module(new PixelRepeater(weightTypeProjected, local_addr_t, block_cols, aligned_to, new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), passthrough = !has_first_layer_optimizations))
    zero_writer_pixel_repeater.io.req.valid := zero_writer.io.resp.valid
    zero_writer_pixel_repeater.io.req.bits.in := 0.U.asTypeOf(Vec(block_cols, weightTypeProjected))
    zero_writer_pixel_repeater.io.req.bits.laddr := zero_writer.io.resp.bits.laddr
    zero_writer_pixel_repeater.io.req.bits.len := zero_writer.io.resp.bits.tag.cols
    zero_writer_pixel_repeater.io.req.bits.pixel_repeats := zero_writer.io.resp.bits.tag.pixel_repeats
    zero_writer_pixel_repeater.io.req.bits.last := zero_writer.io.resp.bits.last
    zero_writer_pixel_repeater.io.req.bits.tag := zero_writer.io.resp.bits.tag
    zero_writer_pixel_repeater.io.req.bits.mask := {
      val n = weightTypeProjected.getWidth/ 8
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
    val (mvin_scale_acc_in, mvin_scale_acc_out) = if (mvin_scale_shared) (mvin_scale_in, mvin_scale_out) else (
      VectorScalarMultiplier(
        config.mvin_scale_acc_args,
        config.accType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits),
        is_acc = true
      )
      )

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
      zero_writer_pixel_repeater.io.resp.bits.tag.cols * (weightTypeProjected.getWidth / 8).U)

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

    val spad_mems = {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(
        sp_bank_entries, spad_w,
        aligned_to, config.sp_singleported,
        use_shared_ext_mem, is_dummy
      )) }
      val bank_ios = VecInit(banks.map(_.io))
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
          bio.read.req.bits.weight_mx_format := ex_read_req.bits.weight_mx_format
          bio.read.req.bits.input_mx_format := ex_read_req.bits.input_mx_format
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B
          bio.read.req.bits.weight_mx_format := ex_read_req.bits.weight_mx_format  // Default FP8 for DMA
          bio.read.req.bits.input_mx_format := ex_read_req.bits.input_mx_format
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

      // V1 requant-to-spad: 2-beat write of the 256b FP8 requant output into 2 consecutive bank
      // rows (writeData[127:0] -> row r, writeData[255:128] -> row r+1). Destination bank/row come
      // from the store's vaddr (= the spad dest addr the loop store passes in, StoreController:186).
      val requant_half = RegInit(0.U(2.W))   // beat counter: FP8 uses beats 0-1, BF16 uses 0-3
      val requant_dst  = WireInit(0.U.asTypeOf(local_addr_t))
      requant_dst.data := write_issue_q.io.deq.bits.vaddr
      val requant_dst_bank = requant_dst.sp_bank()
      val requant_dst_row  = requant_dst.sp_row()

      // FP4/FP6 (sub-byte): the requantizer emits each combined 256b VALID FOR ONE CYCLE (its 2-fire
      // counter is input-driven, so unlike FP8 the output does NOT hold under back-pressure). The 128b
      // bank port can't take 256b in one cycle, so latch the high half on the valid cycle and drain it
      // the next cycle (the interleaved garbage cycle). beat0 -> dst_row, beat1 -> dst_row+1; the store
      // dst rows are stride-2, so this lands row-major-contiguous. FP8 keeps the requant_half scheme.
      val requant_subbyte = io.output_mx_format === 1.U || io.output_mx_format === 2.U
      val requant_bf16 = io.output_mx_format === 3.U   // non-requant BF16 out: full-width, 4 beats
      val requant_valid_fire = requant_to_spad && acc_scale_unit.io.out.valid &&
        acc_scale_unit.io.out.bits.fromDMA && !acc_scale_unit.io.out.bits.is_garbage
      val requant_pend    = RegInit(false.B)
      val requant_hi_data = Reg(UInt(spad_w.W))
      val requant_hi_row  = Reg(chiselTypeOf(requant_dst_row))
      val requant_hi_bank = Reg(chiselTypeOf(requant_dst_bank))
      when (requant_subbyte && requant_valid_fire && !requant_pend) {
        requant_hi_data := acc_scale_unit.io.out.bits.data.asUInt(2*spad_w - 1, spad_w)
        requant_hi_row  := requant_dst_row + 1.U
        requant_hi_bank := requant_dst_bank
        requant_pend    := true.B
      }.elsewhen (requant_pend) {
        requant_pend := false.B
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.srams.write(i).valid
        io.srams.write(i).ready := bio.write.ready

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

        // Source directly from acc_scale_unit.io.out (registered valid), NOT writeData, so this does
        // not depend on dma_resp_ready (which depends back on requant_spad_consume) -> no comb cycle.
        // FP8: 2-beat held scheme (output holds under back-pressure). FP4/FP6: beat0 on the valid
        // cycle, beat1 drained from the latch next cycle (output does not hold). Data written verbatim
        // -> spad keeps the mesh-input interleaved i0j0 i1j0 i0j1... format.
        val requantwrite = requant_to_spad && !requant_subbyte && acc_scale_unit.io.out.valid &&
          acc_scale_unit.io.out.bits.fromDMA && !acc_scale_unit.io.out.bits.is_garbage &&
          (requant_dst_bank === i.U) && bio.write.ready
        val requant_beat0 = requant_subbyte && requant_valid_fire &&
          (requant_dst_bank === i.U) && bio.write.ready
        val requant_beat1 = requant_subbyte && requant_pend &&
          (requant_hi_bank === i.U) && bio.write.ready

        bio.write.valid := exwrite || dmaread || zerowrite || requantwrite || requant_beat0 || requant_beat1

        when (exwrite) {
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
          // FP8 (narrow 256b -> 2 beats) or non-requant BF16 (full_data 512b -> 4 beats). Both hold
          // under back-pressure (no 2-fire counter), so drain into consecutive bank rows and consume on
          // the last beat. BF16 sources full_data directly (registered) -> no comb cycle, same 128b port.
          val narrowVec = acc_scale_unit.io.out.bits.data.asUInt.asTypeOf(Vec(2, UInt(spad_w.W)))
          val wideVec   = acc_scale_unit.io.out.bits.full_data.asUInt.asTypeOf(Vec(acc_write_w / spad_w, UInt(spad_w.W)))
          val last_beat = Mux(requant_bf16, (acc_write_w / spad_w - 1).U, 1.U)
          bio.write.addr := requant_dst_row + requant_half
          bio.write.data := Mux(requant_bf16, wideVec(requant_half), narrowVec(requant_half(0)))
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
          requant_half := Mux(requant_half === last_beat, 0.U, requant_half + 1.U)
          when (requant_half === last_beat) { requant_spad_consume := true.B }
        }.elsewhen (requant_beat0) {
          // FP4/FP6 beat0: low 128b -> dst_row (high 128b latched for beat1 next cycle).
          bio.write.addr := requant_dst_row
          bio.write.data := acc_scale_unit.io.out.bits.data.asUInt(spad_w - 1, 0)
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
        }.elsewhen (requant_beat1) {
          // FP4/FP6 beat1: latched high 128b -> dst_row+1.
          bio.write.addr := requant_hi_row
          bio.write.data := requant_hi_data
          bio.write.mask := VecInit(Seq.fill((spad_w / (aligned_to * 8)) max 1)(true.B))
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
        }
      }
      // FP4/FP6: consume the valid out on the beat0 cycle (high half already latched for beat1).
      when (requant_subbyte && requant_valid_fire) { requant_spad_consume := true.B }
      // Garbage cycle: consume the garbage out (+dequeue). For FP4/FP6 the beat1 drain write happens
      // in the bank loop (requant_beat1) this same cycle; for FP8 no bank write here.
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
    val dma_read_resp_wire = WireDefault(dma_resp_ready)
    dontTouch(dma_read_resp_wire)
    when (acc_scale_unit.io.out.bits.fromDMA && dma_resp_ready) {
      // Send the acc-scale result into the DMA. For requant_to_spad, dma_resp_ready = the bank
      // write's 2nd-beat consume, so out is consumed only once both bank rows are written.
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

    val acc_adders = Module(new AccPipeShared(acc_latency-1, acc_row_t, acc_banks))
    //val fp8_mode = io.srams.read(0).req.bits.input_mx_format === 2.U

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
      )) }
      val bank_ios = VecInit(banks.map(_.io))


      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.acc_bank())

      // Reading from the Accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        bio.scale_mem_write_w.foreach { w =>
          w.valid := io.scale_mem_write_w.get.valid
          w.bits := io.scale_mem_write_w.get.bits
          io.scale_mem_write_w.get.ready := w.ready
        }

        bio.scale_mem_write_act.foreach { w =>
          w.valid := io.scale_mem_write_act.get.valid
          w.bits := io.scale_mem_write_act.get.bits
          io.scale_mem_write_act.get.ready := w.ready
        }
        bio.i := io.i
        bio.j := io.j
        bio.k := io.k
        bio.counter_i := io.counter_i
        bio.counter_j := io.counter_j
        bio.counter_k := io.counter_k
        bio.dataType_out := io.act_mx_format
        // bio.scaleMemCntl <> io.scaleMemCntl.get
        bio.scaleMemCntl.foreach { bioCnlt =>          
          io.scaleMemCntl.foreach { ioCnlt =>
            bioCnlt <> ioCnlt
          }
        }
      
        bio.read.req.bits.activation_mx_format := io.act_mx_format
        bio.read.req.bits.weight_mx_format := io.weight_mx_format

        if (use_shared_ext_mem) {
          io.ext_mem.get.acc(i) <> bio.ext_mem.get
        }

        acc_adders.io.in_sel(i) := bio.adder.valid
        acc_adders.io.ina(i) := bio.adder.op1
        acc_adders.io.inb(i) := bio.adder.op2
        bio.adder.sum := acc_adders.io.out

        val ex_read_req = io.acc.read_req(i)
        val exread = ex_read_req.valid

//        val dispatch_first_half_sent = RegInit(false.B)


        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.acc_bank() === i.U

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
            // FP8: StC fires one command per chunk, so every command gets a resp.
            // FP4/FP6: WRE fires two sub-commands per mvout; resp fires only on the last.
            when (write_dispatch_q.bits.activation_mx_type === 0.U || write_dispatch_q.bits.chunk_id === (numChunks - 1).U) {
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
        val from_mvin_scale = mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc
        val from_mvin_scale_acc = mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.tag.is_acc

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
        }.elsewhen (dmaread && !spad_last && !consecutive_write_block) {
          bio.write.valid := true.B
          bio.write.bits.data := Mux(from_mvin_scale,
            // VecInit(mvin_scale_out.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            VecInit(mvin_scale_pixel_repeater.io.resp.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            mvin_scale_acc_out.bits.out.asTypeOf(acc_row_t))
          bio.write.bits.mask :=
            Mux(from_mvin_scale,
              {
                val n = accType.getWidth / weightTypeProjected.getWidth
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
            val n = accType.getWidth / weightTypeProjected.getWidth
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
    //    io.counter.collect(spad_writer.module.io.counter)
  }
}

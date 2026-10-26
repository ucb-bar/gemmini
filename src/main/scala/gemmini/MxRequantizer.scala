package gemmini

import chisel3._
import chisel3.util._

object MxFloatFormat {
  // Format encoding
  val FP8 = 0.U(2.W)
  val FP6 = 1.U(2.W)
  val FP4 = 2.U(2.W)
  val BF16 = 3.U(2.W)

  def apply(bits: UInt): (UInt, UInt, UInt, UInt) = {
    val exp_bits = MuxLookup(bits, 4.U)(Seq(
      FP4 -> 2.U,
      FP6 -> 3.U,
      FP8 -> 4.U,
      BF16 -> 7.U
    ))
    val mant_bits = MuxLookup(bits, 3.U)(Seq(
      FP4 -> 1.U,
      FP6 -> 2.U,
      FP8 -> 3.U,
      BF16 -> 8.U
    ))
    
    val pmax = MuxLookup(bits, 448.U)(Seq(
      FP4 -> 6.U,     
      FP6 -> 28.U,   
      FP8 -> 448.U,   
      BF16 -> 65024.U
    ))

    // Block-scale floor = 0 for every format (_po2 convention): a block max normalizes into [1,2) so a
    // chained requant output (which becomes the next matmul's A operand) never overflows the acc. Non-zero
    // floors (old FP4=2, FP6=4) pushed chained values past the accumulator/finder range.
    val log2_pmax_floor = 0.U
    
    (exp_bits, mant_bits, pmax, log2_pmax_floor)
  }
}

class MxRequantizerAccResp[T <: Data: Arithmetic](fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]]) extends Bundle {
  val out = new MxRequantizerAccMemDataOut[T] (rDataType, fullDataType)
  val mx_mode = UInt(2.W)
  val is_gpu = Bool()
  val gpu_addr = UInt(32.W)
  val from_sr = Bool()   // owner tag: a SPAD_REQUANT beat (vs an accumulator beat), returned with the output
}

class MxRequantizerIO[T <: Data: Arithmetic](
  sp_data_width: Int,
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleMemActWriteAddrWidth: Int,   // byte-addr width of the on-chip act-scale write port
  scaleSize: Int,
  scaleMembasewrite: Int,
  lutConfig: Option[GemminiLUTConfig],
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  acc_row_t: Vec[Vec[T]],
  spad_row_t: Vec[Vec[T]],
  half_t: Vec[Vec[T]],
  iterator_bitwidth: Int,
  config: GemminiRequantizerConfig
) extends Bundle {
  val inputnumLanes = config.numInputLanes
  val outputnumLanes = config.numOutputLanes
  val inputdataWidth = config.inputBits
  val mxacc_req = Flipped(new MxRequantizerAccMemIO[T](acc_row_t, spad_row_t, half_t))
  val requant_data_in_gpu = Flipped(Decoupled(new RequantizerInBundle(config.numGPUInputLanes, inputdataWidth)))
  val requant_data_out = Decoupled(new RequantizerOutBundle(outputnumLanes))
  val scaleMem_write = Decoupled(new ScalingFactorPutReq(scaleMem_addr_width, scaleMem_data_width))   // byte-masked
  // LUT write ports exist only when the QuantLut is built (lutConfig defined).
  val lut0_write = lutConfig.map(l => Flipped(Decoupled(new QuantLutWriteBundle(l(0)))))
  val lut1_write = lutConfig.map(l => Flipped(Decoupled(new QuantLutWriteBundle(l(1)))))
  val lut2_write = lutConfig.map(l => Flipped(Decoupled(new QuantLutWriteBundle(l(2)))))
  val spad_projected_data   = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected))
  val spad_deprojected_data = Vec(sp_banks, Flipped(new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val read_a = Input(Bool())
  val read_d = Input(Bool())
  val scale_mem_mvout_base_addr_act = Input(UInt(scaleMem_addr_width.W)) // from execute controller
  val quant_lut_update_granularity = Input(UInt(lutConfig.map(_.lutUpdateRegularityWidth).getOrElse(16).W))
  val act_lut_halved = Input(Bool())   // act-in lookups use LUT half act_lut_half (loop-managed codebooks)
  val act_lut_half = Input(UInt(1.W))
  val wgt_lut_halved = Input(Bool())   // weight lookups use LUT half wgt_lut_half (loop-managed codebooks)
  val wgt_lut_half = Input(UInt(1.W))
  val loop_bound_i = Input(UInt(iterator_bitwidth.W)) // from  controller
  val loop_bound_j = Input(UInt(iterator_bitwidth.W)) // from  controller
  val loop_bound_k = Input(UInt(iterator_bitwidth.W)) // from  controller
  val scale_mem_counter_reset_flag = Input(Bool())
  // When set, the coalescer also flushes act block-scales transposed to [GN][M] onto
  // scaleMem_write_act_resident (64b/beat); otherwise that port is idle.
  val scale_resident = Input(Bool())
  val scale_read_act_sel = Input(UInt(1.W))   // act-window half this matmul reads; its resident scales go to the other
  val scaleMem_write_act_resident = Decoupled(new ScalingFactorWriteReq(scaleMemActWriteAddrWidth, 64))
  // Splits E4M3-quad (4-bit LUT index) from E4M3-single (8-bit code): both are format0/altfmt0.
  val lut_en = Input(Bool())
  // SPAD_REQUANT: scales filed in arrival order (byte k = block k, M x GN row-major), not by the matmul walk
  val linear_scales = Input(Bool())
  val linear_gn = Input(UInt(16.W))
  val linear_m = Input(UInt(16.W))
  val linear_base = Input(UInt(scaleMem_addr_width.W))   // overrides scale_mem_mvout_base_addr_act / scale_resident
  val linear_resident = Input(Bool())
  val linear_pairs = Input(Bool())   // beats arrive in row pairs (m, b), (m+1, b) (FP4 / LUT SPAD_REQUANT)
  val sr_lut_rows = Input(Bool())    // LUT SPAD_REQUANT: act-out codebook = the beat's row (pair order) >> G
  val scale_flush_busy = Output(Bool())
  // Shared use (SPAD_REQUANT beats interleaved with BF16 accumulator beats): each beat's own mx_mode decides
  // quantize vs pass-through; quant_mode, when valid, sets the quantizer's format instead of mxacc_req.mx_mode.
  // Interleaving is only valid with an 8-bit direct-coded quant format (no two-beat FP4/LUT output packing).
  val quant_mode = Input(Valid(UInt(2.W)))
  val sr_tag_in = Input(Bool())    // tags the beat entering on mxacc_req.mx_data_in
  val sr_tag_out = Output(Bool())  // tag of the beat on mxacc_req.mx_data_out
}
   
class MxRequantizer[T <: Data](
  sp_data_width: Int,
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleMemActWriteAddrWidth: Int,   // byte-addr width of the on-chip act-scale write port
  scaleSize: Int,
  scaleMembasewrite: Int,
  // None -> no QuantLut (no LUT caches / nearest finders): only direct-coded outputs (E4M3-single, FP4).
  lutConfig: Option[GemminiLUTConfig],
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  iterator_bitwidth: Int,
  meshColumns: Int, tileColumns: Int,
  accType: T, weightTypeProjected: T,
  config: GemminiRequantizerConfig
)(implicit ev: Arithmetic[T]) extends Module {
  
  import ev._
  val pipelineLatency = config.pipelineLatency
  // Widened acc SRAM row (2*meshColumns at DIM=8) delivered to the requant as one wide row = 2 blocks.
  val accCols = MxAccWiden(meshColumns*tileColumns)*meshColumns
  val acc_row_t = Vec(accCols, Vec(tileColumns, accType))
  // spad_row_t holds one requant beat's output = numOutputLanes codes.
  val spad_row_t = Vec(config.numOutputLanes/tileColumns, Vec(tileColumns, weightTypeProjected))
  val numChunks = if (accCols*tileColumns < 16) 1 else 2
  val tilesPerMxBlock = scaleSize / (meshColumns*tileColumns)
  val half_acc_row_t = Vec((accCols*tileColumns)/numChunks, Vec(tileColumns, accType))
  val inputdataWidth = config.inputBits
  val io = IO(new MxRequantizerIO[T](
    sp_data_width,
    sp_addr_width,
    scaleMem_data_width,
    scaleMem_addr_width,
    scaleMemActWriteAddrWidth,
    scaleSize,
    scaleMembasewrite,
    lutConfig,
    sp_bank_entries,
    sp_banks,
    sp_width,
    sp_width_projected,
    acc_row_t,
    spad_row_t,
    half_acc_row_t,
    iterator_bitwidth,
    config
  ))
  dontTouch(io)
  val scale_mem_mvout_base_addr_act = io.scale_mem_mvout_base_addr_act

  val scales_per_write = scaleMem_data_width / 8
  val scale_write_addr_counter = RegInit(0.U(16.W))

  val scale_buffer = RegInit(VecInit(Seq.fill(scaleSize)(0.U(8.W))))
  val quant_dataType = io.mxacc_req.mx_mode  //output data fromat
  val format_reg = RegInit(0.U(2.W))
  format_reg := Mux(io.quant_mode.valid, io.quant_mode.bits, quant_dataType.asUInt)(1, 0)
  val altfmt_reg = RegInit(false.B)          // code1 output sub-format: 1 = E5M2, 0 = FP6 (aligned w/ format_reg)
  altfmt_reg := io.mxacc_req.mx_fp8_altfmt
  // lut_en is quasi-static; latch it alongside format_reg/altfmt_reg.
  val lut_en_reg = RegInit(false.B)
  lut_en_reg := io.lut_en

  io.scaleMem_write.valid := false.B
  io.scaleMem_write.bits := DontCare

  def abs(x: UInt): UInt = {  
    x & 0x7FFF.U  
  }
 
  def log2_floor(x: UInt): UInt = {
    val width = x.getWidth
    val result = Wire(UInt(log2Ceil(width).W))
    result := 0.U
    
    for (i <- (width - 1) to 0 by -1) {
      when(x(i)) {
        result := i.U
      }
    }
    result
  }
  
  val e5m2Lut = lutConfig.exists(_.projFormat == LutFP8E5M2)
  val (exp_bits, mant_bits, pmax, log2_pmax_floor_raw) = MxFloatFormat(format_reg)
  // Block-scale floor = 0 for every (format, altfmt) so chained requant outputs stay in acc/finder range.
  val log2_pmax_floor = log2_pmax_floor_raw
  val data_buffer_counter = RegInit(0.U(1.W))
  //buffer twice for 16-lane mode
  val half_lanes = config.numGPUInputLanes
  val input_32_buffer = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(inputdataWidth.W))))
  val input_16_buffer_gpu = RegInit(VecInit(Seq.fill(io.outputnumLanes/2)(0.U(inputdataWidth.W))))
  val should_compute = Wire(Bool())
  val quantize_valid = RegNext(should_compute)
  val quantized_buffer = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(8.W)))) 
  dontTouch(quantized_buffer)
  dontTouch( should_compute)
  should_compute := false.B
  val quant_half_counter = RegInit(false.B)
  val first_half_buf     = RegInit(0.U((io.outputnumLanes*4).W))

  // This is the datapath pack structure, not the literal element width: E5M2 and E4M3-quad have wider
  // codes but ride the 6-bit LUT datapath (buffering, packing, coalescer cadence) like FP6.
  val total_bits_raw = 1.U +& exp_bits +& mant_bits
  val datapath_pack_mode = WireDefault(0.U(5.W))
  datapath_pack_mode := Mux(format_reg === 0.U && (altfmt_reg || lut_en_reg), 6.U, total_bits_raw)

  // Output packing: E4M3-single -> 8-bit direct codes; E4M3-quad/E5M2/E3M2/E2M3 -> 4-bit LUT indices;
  // FP4 -> 4-bit direct. E4M3-single vs -quad share format0/altfmt0, split only by latched lut_en.
  val out_is_fp4  = format_reg === 2.U
  val out_is_lut4 = lutConfig.isDefined.B && ((format_reg === 1.U) || (format_reg === 0.U && (altfmt_reg || lut_en_reg)))
  val out_is_8bit = (format_reg === 0.U && !altfmt_reg && !lut_en_reg)

  val extracted_data = WireDefault((0.U((io.outputnumLanes*8).W))) // 256bits / 128bits
  val quant_data_held = RegInit(0.U((io.outputnumLanes*8).W))

  val pipe_in = Wire(Decoupled(new MxRequantizerAccResp[T](half_acc_row_t, spad_row_t)(ev)))
  pipe_in.valid := false.B
  pipe_in.bits := DontCare
  val pipelined_out_0 = Pipeline(pipe_in, 1)
  val oldest_pipe_out = Pipeline(pipelined_out_0, pipelineLatency - 1)
  

  val final_pipe_out = Wire(Decoupled(new MxRequantizerAccResp[T](half_acc_row_t, spad_row_t)(ev)))
  dontTouch(final_pipe_out)
  val quantLut = lutConfig.map { l => Module(new QuantLut(
    l,
    outputnumLanes = io.outputnumLanes ,
    sp_bank_entries = sp_bank_entries,
    sp_banks = sp_banks,
    sp_width = sp_width,
    sp_width_projected = sp_width_projected,
    lut_update_regularity_w = config.lutUpdateRegularityW,
    lut_update_regularity_act_in = config.lutUpdateRegularityActIn,
    lut_update_regularity_act_out = config.lutUpdateRegularityActOut,
    iterator_bitwidth = iterator_bitwidth
  )) }

  when(io.requant_data_in_gpu.fire) {
     for (i <- 0 until half_lanes) {
      input_16_buffer_gpu(i) := io.requant_data_in_gpu.bits.data(i)
    }
    data_buffer_counter := ~data_buffer_counter
  }
  
  val gpu_addr = RegInit(0.U((32).W))
  when(io.requant_data_in_gpu.fire && data_buffer_counter === 0.U && datapath_pack_mode === 8.U){
    gpu_addr := io.requant_data_in_gpu.bits.address 
  }.elsewhen(io.requant_data_in_gpu.fire && data_buffer_counter === 0.U && datapath_pack_mode =/= 8.U && !quant_half_counter){
    gpu_addr := io.requant_data_in_gpu.bits.address >> 1
  }

  when(io.mxacc_req.mx_data_in.fire) {
    pipe_in.valid := true.B
    pipe_in.bits.mx_mode := io.mxacc_req.mx_mode
    pipe_in.bits.out.full_mx_data_out := io.mxacc_req.mx_data_in.bits.full_mx_data_in
    pipe_in.bits.out.fromDMA := io.mxacc_req.mx_data_in.bits.fromDMA
    pipe_in.bits.out.chunk_id := io.mxacc_req.mx_data_in.bits.chunk_id
    pipe_in.bits.out.acc_bank_id := io.mxacc_req.mx_data_in.bits.acc_bank_id
    pipe_in.bits.is_gpu := false.B
    pipe_in.bits.gpu_addr := 0.U
    pipe_in.bits.from_sr := io.sr_tag_in

  }.elsewhen(io.requant_data_in_gpu.fire && data_buffer_counter === 1.U) {
    pipe_in.valid := true.B
    pipe_in.bits.mx_mode := format_reg
    val combined = input_16_buffer_gpu ++ io.requant_data_in_gpu.bits.data
    pipe_in.bits.out.full_mx_data_out := VecInit(combined).asTypeOf(half_acc_row_t)
    pipe_in.bits.out.fromDMA := false.B
    pipe_in.bits.is_gpu := true.B
    pipe_in.bits.gpu_addr := gpu_addr
    pipe_in.bits.from_sr := false.B
  }


  final_pipe_out := oldest_pipe_out

  // Two-cycle accumulation registers for FP4 / FP6:
  val fp6_lut_out     = quantLut.map(q => Cat(q.io.projected_data.bits.reverse)).getOrElse(0.U((io.outputnumLanes*4).W))
  val fp6_row0        = (0 until io.outputnumLanes).map(k => first_half_buf(4*k+3, 4*k))    
  val fp6_row1        = (0 until io.outputnumLanes).map(k => fp6_lut_out(4*k+3, 4*k))      
  val fp6_interleaved = (0 until io.outputnumLanes/2).flatMap { j => Seq(fp6_row0(2*j), fp6_row1(2*j), fp6_row0(2*j+1), fp6_row1(2*j+1)) }
  val fp6_combined    = Cat(fp6_interleaved.reverse)   
  val fp6_combined_wire = WireDefault(fp6_combined)                        
  val fp6_lut_out_wire = WireDefault(fp6_lut_out)
  dontTouch(fp6_combined_wire)
  dontTouch(fp6_lut_out_wire)

  val fp4_row0        = (0 until io.outputnumLanes).map(k => first_half_buf(4*k+3, 4*k))
  val fp4_row1        = (0 until io.outputnumLanes).map(k => extracted_data(4*k+3, 4*k))
  val fp4_row0_wire = WireDefault(VecInit(fp4_row0))
  val fp4_row1_wire = WireDefault(VecInit(fp4_row1))
  dontTouch(fp4_row0_wire)
  dontTouch(fp4_row1_wire)
  val fp4_interleaved = (0 until io.outputnumLanes/2).flatMap { j => Seq(fp4_row0(2*j), fp4_row1(2*j), fp4_row0(2*j+1), fp4_row1(2*j+1)) }
  val fp4_combined    = Cat(fp4_interleaved.reverse)                       
  val fp4_combined_wire = WireDefault(fp4_combined)                        
  dontTouch(fp4_combined_wire)

  final_pipe_out.bits.out.quant_mx_data_out := 0.U.asTypeOf(spad_row_t)
  final_pipe_out.bits.out.is_garbage := false.B
  // a two-beat output's garbage flag belongs to its beat: latched at the beat's quantize pulse and held, like its data,
  // while the output waits (the live pair counter has already moved on to the next beat)
  val garbage_held = RegInit(false.B)
  val lut_valid = quantLut.map(_.io.projected_data.valid).getOrElse(false.B)
  when(out_is_8bit) {
    final_pipe_out.valid := oldest_pipe_out.valid
    final_pipe_out.bits.out.quant_mx_data_out := Mux(quantize_valid, extracted_data, quant_data_held).asTypeOf(spad_row_t)
    final_pipe_out.bits.out.is_garbage := false.B
  }.elsewhen(out_is_lut4) {
    when(lut_valid) {
      garbage_held := !quant_half_counter
      when(!quant_half_counter) {
        first_half_buf     := fp6_lut_out
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.bits.out.is_garbage := Mux(lut_valid, !quant_half_counter, garbage_held) && oldest_pipe_out.valid
    final_pipe_out.valid :=  (oldest_pipe_out.valid)  
    final_pipe_out.bits.out.quant_mx_data_out := Mux(lut_valid, fp6_combined, quant_data_held).asTypeOf(spad_row_t)

  }.elsewhen(out_is_fp4) {
    when(quantize_valid) {
      garbage_held := !quant_half_counter
      when(!quant_half_counter) {
        first_half_buf     := extracted_data(io.outputnumLanes*4-1, 0)
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.valid := (oldest_pipe_out.valid)
    final_pipe_out.bits.out.is_garbage := Mux(quantize_valid, !quant_half_counter, garbage_held) && oldest_pipe_out.valid
    final_pipe_out.bits.out.quant_mx_data_out := Mux(quantize_valid, fp4_combined, quant_data_held).asTypeOf(spad_row_t)
  }

  when(quantize_valid) {
    when(out_is_fp4){
      extracted_data := Cat((0 until io.outputnumLanes).map(i => quantized_buffer(i)(3, 0)).reverse)
    }.elsewhen(out_is_8bit){
      extracted_data := Cat(quantized_buffer.reverse)
    }.otherwise{
      extracted_data := 0.U((io.outputnumLanes*8).W)
    }
  }

  when(quantize_valid && out_is_8bit) {
    quant_data_held := extracted_data
  }.elsewhen(lut_valid && out_is_lut4){
    quant_data_held := (fp6_combined)
  }.elsewhen(quantize_valid && out_is_fp4){
    quant_data_held := (fp4_combined)
  }

  val scale_room = WireDefault(true.B)   // the scale word queue can take this beat's words (driven below)
  val can_enqueue = pipe_in.ready && scale_room
  val can_enqueue_wire = WireDefault(can_enqueue)
  // per beat: a BF16 beat passes through unquantized (same as datapath_pack_mode === 16 while the mode is static)
  val beat_full = pipelined_out_0.bits.mx_mode === MxFloatFormat.BF16
  val full_precision_valid = RegNext(can_enqueue && pipelined_out_0.valid && beat_full)

  dontTouch(can_enqueue_wire)
    // Only allow input handshake when queue has space
  io.mxacc_req.mx_data_in.ready := can_enqueue
  io.requant_data_in_gpu.ready := can_enqueue && !io.mxacc_req.mx_data_in.fire 
  
  io.mxacc_req.mx_data_out.bits.is_garbage := final_pipe_out.bits.out.is_garbage
  io.mxacc_req.mx_data_out.bits.full_mx_data_out := final_pipe_out.bits.out.full_mx_data_out
  io.mxacc_req.mx_data_out.bits.quant_mx_data_out  := final_pipe_out.bits.out.quant_mx_data_out
  io.mxacc_req.mx_data_out.valid := (final_pipe_out.valid || full_precision_valid) && !final_pipe_out.bits.is_gpu
  io.mxacc_req.mx_data_out.bits.fromDMA := final_pipe_out.bits.out.fromDMA
  io.mxacc_req.mx_data_out.bits.acc_bank_id := final_pipe_out.bits.out.acc_bank_id
  io.mxacc_req.mx_data_out.bits.chunk_id := final_pipe_out.bits.out.chunk_id
  io.sr_tag_out := final_pipe_out.bits.from_sr


  final_pipe_out.ready := Mux(final_pipe_out.bits.is_gpu,
    io.requant_data_out.ready  || final_pipe_out.bits.out.is_garbage,
    io.mxacc_req.mx_data_out.ready)
  oldest_pipe_out.ready := final_pipe_out.ready
  

  val gpu_out_held = RegInit(0.U(256.W))
  val helding_flag = RegInit(0.U)
  when (io.requant_data_out.fire){
    helding_flag := 0.U
  }.elsewhen(final_pipe_out.bits.is_gpu && final_pipe_out.valid && !io.requant_data_out.ready && !final_pipe_out.bits.out.is_garbage){
    helding_flag := 1.U
  }

  when(final_pipe_out.bits.is_gpu && final_pipe_out.valid && !final_pipe_out.bits.out.is_garbage){
    when(helding_flag === 1.U && io.requant_data_out.ready){
      io.requant_data_out.bits.data := gpu_out_held
      io.requant_data_out.valid := true.B
      io.requant_data_out.bits.dataType := RequantizerDataType(format_reg)
      io.requant_data_out.bits.address :=  final_pipe_out.bits.gpu_addr
    }.elsewhen(helding_flag === 0.U && io.requant_data_out.ready){
      io.requant_data_out.bits.data := final_pipe_out.bits.out.quant_mx_data_out.asUInt
      io.requant_data_out.valid := true.B
      io.requant_data_out.bits.dataType := RequantizerDataType(format_reg)
      io.requant_data_out.bits.address :=  final_pipe_out.bits.gpu_addr
    }.otherwise{
      gpu_out_held := final_pipe_out.bits.out.quant_mx_data_out.asUInt
      io.requant_data_out.bits.data := 0.U
      io.requant_data_out.valid := false.B
      io.requant_data_out.bits.dataType := RequantizerDataType(format_reg)
      io.requant_data_out.bits.address := final_pipe_out.bits.gpu_addr
    }
  }.otherwise{
    io.requant_data_out.bits.data := 0.U
    io.requant_data_out.valid := false.B
    io.requant_data_out.bits.dataType := RequantizerDataType(format_reg)
    io.requant_data_out.bits.address := final_pipe_out.bits.gpu_addr
  }

  should_compute := false.B
  when (can_enqueue) {
    when(pipelined_out_0.valid && !beat_full) {
      should_compute := true.B
    }
  }
  
  val flat64 = pipelined_out_0.bits.out.full_mx_data_out.flatten.map(_.asUInt)
  val flat64_wire = WireDefault(VecInit(flat64))
  dontTouch(flat64_wire)
  val reshaped_pipelined_out_0 = VecInit(
    flat64.flatMap(
      x => (0 until 4).map(i => x(16*(i+1)-1, 16*i))
    )
  )

  val mxBlockSize    = 32
  val numOutBlocks   = io.outputnumLanes / mxBlockSize
  val EPS_BIASED_EXP = 104.U(8.W)
  val scale_e8m0_vec    = Wire(Vec(numOutBlocks, UInt(8.W)))
  val neg_e8m0_vec      = Wire(Vec(numOutBlocks, UInt(8.W)))
  val block_has_nan_vec = Wire(Vec(numOutBlocks, Bool()))
  val block_has_inf_vec = Wire(Vec(numOutBlocks, Bool()))
  val quantized_blocks  = Wire(Vec(io.outputnumLanes, UInt(8.W)))
  dontTouch(scale_e8m0_vec); dontTouch(neg_e8m0_vec)

  for (blk <- 0 until numOutBlocks) {
    val elems = (0 until mxBlockSize).map(k => reshaped_pipelined_out_0(blk*mxBlockSize + k))
    val block_max = WireDefault(0.U(inputdataWidth.W))
    val bhn = WireDefault(false.B)
    val bhi = WireDefault(false.B)
    when(should_compute) {
      val mags = elems.map { e =>
        val mag     = abs(e.asUInt)
        val expOnes = mag(14, 7).andR     // BF16: exp field all-ones -> NaN or Inf
        val isNan   = expOnes && mag(6, 0).orR
        val isInf   = expOnes && !mag(6, 0).orR
        (Mux(expOnes, 0.U, mag), isNan, isInf)
      }
      block_max := mags.map(_._1).reduce { (a, b) => Mux(a > b, a, b) }
      bhn := mags.map(_._2).reduce(_ || _)
      bhi := mags.map(_._3).reduce(_ || _)
    }
    block_has_nan_vec(blk) := bhn
    block_has_inf_vec(blk) := bhi
    val block_nonfinite = bhn || bhi

    val scale_exponent   = Wire(SInt(9.W))
    val scale_e8m0       = WireDefault(0.U(8.W))
    val neg_e8m0_clamped = WireDefault(0.U(8.W))
    scale_exponent := 0.S
    val max_biased_exp = block_max(14, 7)
    when (should_compute) {
      val clamped_exp = Mux(max_biased_exp < EPS_BIASED_EXP, EPS_BIASED_EXP, max_biased_exp)
      scale_exponent := clamped_exp.zext.asSInt - 127.S - log2_pmax_floor.zext.asSInt
      val std_e8m0 = scale_exponent + 127.S
      val std_clamped = Mux(std_e8m0 < 0.S, 0.U(8.W),
                        Mux(std_e8m0 > 254.S, 254.U(8.W), std_e8m0.asUInt(7, 0)))
      // A non-finite anywhere in the block -> E8M0 255 (NaN); valid codes stop at 254.
      scale_e8m0 := Mux(block_nonfinite, 255.U, std_clamped)
      val neg_e8m0 = 127.S(9.W) - scale_exponent
      neg_e8m0_clamped := Mux(neg_e8m0 < 0.S, 0.U(8.W),
                          Mux(neg_e8m0 > 254.S, 254.U(8.W), neg_e8m0.asUInt(7, 0)))
    } .otherwise {
      scale_exponent := 0.S
      scale_e8m0 := 127.U
    }
    scale_e8m0_vec(blk) := scale_e8m0
    neg_e8m0_vec(blk)   := neg_e8m0_clamped

    val brt = Module(new BF16ScaleRoundToTiny(outputnumLanes = mxBlockSize, e5m2Lut = e5m2Lut))
    brt.io.in_bf16       := VecInit(elems)
    brt.io.scale_e8m0    := neg_e8m0_clamped
    brt.io.dataType      := format_reg
    brt.io.mx_fp8_altfmt := altfmt_reg
    brt.io.block_has_nan := bhn
    brt.io.block_has_inf := bhi
    for (k <- 0 until mxBlockSize) { quantized_blocks(blk*mxBlockSize + k) := brt.io.out(k) }
  }
  quantized_buffer := RegNext(quantized_blocks)

  val scale_e8m0    = scale_e8m0_vec(0)
  val block_has_nan = block_has_nan_vec.reduce(_ || _)
  val block_has_inf = block_has_inf_vec.reduce(_ || _)

  val sr_row = WireDefault(0.U(16.W))   // LUT SPAD_REQUANT beat row at the act-out lookup (driven below, by the pair walk)
  quantLut match {
    case Some(lut) =>
      // LUT projection source: low rdataW bits of the quantized code (full 8-bit for E5M2, low-6 for FP6).
      val rdataW = lutConfig.get.rdataWidth
      val quant_fp6 = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(rdataW.W))))
      dontTouch(quant_fp6)
      quant_fp6 := Mux(out_is_lut4, VecInit((0 until io.outputnumLanes).map(i => quantized_buffer(i)(rdataW - 1, 0))),
      VecInit(Seq.fill(io.outputnumLanes)(0.U(rdataW.W))))

      lut.io.spad_projected_data <> io.spad_projected_data
      lut.io.spad_deprojected_data <> io.spad_deprojected_data
      lut.io.quant_lut_update_granularity := io.quant_lut_update_granularity
      lut.io.act_lut_halved := io.act_lut_halved
      lut.io.act_lut_half := io.act_lut_half
      lut.io.wgt_lut_halved := io.wgt_lut_halved
      lut.io.wgt_lut_half := io.wgt_lut_half
      lut.io.read_a := io.read_a
      lut.io.read_d := io.read_d
      lut.io.mx_fp8_altfmt := altfmt_reg
      lut.io.output_mx_format := format_reg
      lut.io.loop_bound_i := io.loop_bound_i
      lut.io.loop_bound_j := io.loop_bound_j
      lut.io.loop_bound_k := io.loop_bound_k
      lut.io.quant_fp6.valid := false.B
      lut.io.quant_fp6.bits := DontCare
      lut.io.act_out_row.valid := io.sr_lut_rows
      lut.io.act_out_row.bits := sr_row
      lut.io.lut_write_weight <> io.lut0_write.get
      lut.io.lut_write_act_in <> io.lut1_write.get
      lut.io.lut_write_act_out <> io.lut2_write.get

      when(quantize_valid && out_is_lut4) {
          lut.io.quant_fp6.valid := true.B
          lut.io.quant_fp6.bits := quant_fp6
      }
    case None =>
      // No QuantLut: the Controller never routes reads through the deproject path; tie it off inert.
      for (b <- 0 until sp_banks) {
        io.spad_projected_data(b).req.valid := false.B
        io.spad_projected_data(b).req.bits := DontCare
        io.spad_projected_data(b).resp.ready := true.B
        io.spad_deprojected_data(b).req.ready := false.B
        io.spad_deprojected_data(b).resp.valid := false.B
        io.spad_deprojected_data(b).resp.bits := DontCare
      }
  }

  
  // Scale-factor stream: the E8M0 block scales leave block-major [GN][M] (byte b*M + m) -- the layout A-scales are read
  // in -- instead of being buffered row-major. Within each block's column the scales arrive in ascending m (tile rows,
  // then the next tile row-group), so each block in flight keeps one scales_per_write-byte staging word and emits it
  // when full or at its column end. A column end that is not word-aligned (M % scales_per_write != 0) leaves a word
  // shared with the next column: each side is written as a byte-masked partial Put.
  //   fp8:    sb=super-block, bib=block-in-sb, m=16*g+row,        b=2*sb+bib
  //   nibble: sb=block(b),    bib=row-half,    m=32*g+16*bib+row, b=sb   (2 rows/beat)
  val ROWS_PER_HALF = meshColumns * tileColumns
  val isNibble      = out_is_fp4 || out_is_lut4
  // nibble GN = blocks-per-output-row = N/32 = loop_bound_j * (cols-per-tile) / 32 = loop_bound_j*block_cols/16.
  // == loop_bound_j*numOutBlocks at DIM16/32 (block_cols/16 == numOutBlocks); differs only at DIM=8 (tile<block).
  val nibbleGN      = if (meshColumns*tileColumns < 16) ((io.loop_bound_j * (meshColumns*tileColumns).U) >> 4).asUInt
                      else (io.loop_bound_j * numOutBlocks.U)
  val GN            = Mux(io.linear_scales, io.linear_gn,
                        Mux(isNibble, nibbleGN, (io.loop_bound_j >> log2Ceil(tilesPerMxBlock)).asUInt))
  val tiles_I       = io.loop_bound_i
  val num_super     = (GN + 1.U) >> 1
  val gn_odd        = GN(0)
  val sbPerJg       = 2.U
  val n_jgroups     = Mux(num_super <= sbPerJg, 1.U, (num_super + 1.U) >> 1)
  val chunks_per_jg = Mux(num_super < sbPerJg, num_super, sbPerJg)

  val ag_sb  = RegInit(0.U(9.W))
  val ag_g   = RegInit(0.U(9.W))
  val ag_jg  = RegInit(0.U(9.W))
  val ag_bib = RegInit(0.U(1.W))
  val ag_row = RegInit(0.U(log2Ceil(ROWS_PER_HALF).W))
  val blocks_in_sb = Mux(isNibble, 2.U,
                       Mux((ag_sb === (num_super - 1.U)) && gn_odd, 1.U, 2.U))
  // Super-block arrivals = GN blocks / numOutBlocks blocks-per-arrival. At DIM16/32 GN==loop_bound_j*numOutBlocks
  // so this equals loop_bound_j (unchanged); at DIM=8 (tile<block) loop_bound_j overcounts -> use GN/numOutBlocks.
  val sb_count = Mux(isNibble, (GN / numOutBlocks.U), num_super)
  val super_block = Mux(isNibble, ag_sb, ag_jg * sbPerJg + ag_sb)
  val cur_b    = Mux(isNibble, ag_sb * numOutBlocks.U, (super_block << 1).asUInt + ag_bib)
  val cur_m    = Mux(isNibble, ag_g * (2*ROWS_PER_HALF).U + ag_bib * ROWS_PER_HALF.U + ag_row,
                               ag_g * ROWS_PER_HALF.U + ag_row)

  // SPAD_REQUANT walk: block-beat outer, row inner (m ascending; FP4 / LUT row pairs are consecutive rows)
  val lin_m = RegInit(0.U(16.W)); val lin_b = RegInit(0.U(16.W))
  sr_row := RegNext(lin_m)   // aligned with quantize_valid (= RegNext(should_compute))
  when(should_compute && io.linear_scales) {
    when (lin_m === io.linear_m - 1.U) {
      lin_m := 0.U
      lin_b := Mux(lin_b + numOutBlocks.U >= io.linear_gn, 0.U, lin_b + numOutBlocks.U)
    } .otherwise { lin_m := lin_m + 1.U }
  }
  when(should_compute && !io.linear_scales) {
    when(ag_row === (ROWS_PER_HALF - 1).U) {
      ag_row := 0.U
      when (isNibble) {
        when(ag_bib =/= (blocks_in_sb - 1.U)) {
          ag_bib := ag_bib + 1.U
        } .otherwise {
          ag_bib := 0.U
          when(ag_g === (tiles_I - 1.U)) {
            ag_g := 0.U
            when(ag_sb === (sb_count - 1.U)) {
              ag_sb := 0.U
            }.otherwise { ag_sb := ag_sb + 1.U }
          }.otherwise { ag_g := ag_g + 1.U }
        }
      } .otherwise {
        if (numOutBlocks >= 2) {
          // DIM>=32: one arrival carries all blocks of a super-block; bib unused, walk super-blocks via j-groups.
          ag_bib := 0.U
          when(ag_sb === (chunks_per_jg - 1.U)) {
            ag_sb := 0.U
            when(ag_g === (tiles_I - 1.U)) {
              ag_g := 0.U
              when(ag_jg === (n_jgroups - 1.U)) {
                ag_jg := 0.U
              }.otherwise { ag_jg := ag_jg + 1.U }
            }.otherwise { ag_g := ag_g + 1.U }
          }.otherwise { ag_sb := ag_sb + 1.U }
        } else {
          // DIM=16 (numOutBlocks==1): a super-block's 2 blocks arrive on separate beats; step bib 0->1 or cur_b stays even and block 1 is never written.
          when(ag_bib === (blocks_in_sb - 1.U)) {
            ag_bib := 0.U
            when(ag_g === (tiles_I - 1.U)) {
              ag_g := 0.U
              when(ag_sb === (sb_count - 1.U)) {
                ag_sb := 0.U
              }.otherwise { ag_sb := ag_sb + 1.U }
            }.otherwise { ag_g := ag_g + 1.U }
          }.otherwise { ag_bib := ag_bib + 1.U }
        }
      }
    }.otherwise { ag_row := ag_row + 1.U }
  }


  // ---- staging: one word per block in flight (a super-block's 2 blocks interleave on the fp8 path, a j-group's 4 at
  // DIM32), keyed by block index mod nStreams; each cycle files numOutBlocks consecutive blocks of one row ----
  val sW       = scales_per_write
  val lgSW     = log2Ceil(sW)
  val nStreams = 4
  require(isPow2(sW) && sW >= 8 && numOutBlocks <= nStreams)
  val M_rows   = Mux(io.linear_scales, io.linear_m,
                   tiles_I * Mux(isNibble, (2*ROWS_PER_HALF).U, ROWS_PER_HALF.U))
  val sc_m     = Mux(io.linear_scales, lin_m, cur_m)
  val sc_b0    = Mux(io.linear_scales, lin_b, cur_b)
  val sc_fire  = should_compute
  val st_data  = Reg(Vec(nStreams, Vec(sW, UInt(8.W))))
  val st_mask  = RegInit(VecInit(Seq.fill(nStreams)(0.U(sW.W))))
  val st_word  = Reg(Vec(nStreams, UInt(24.W)))
  class ScaleWord extends Bundle { val data = UInt((8 * sW).W); val mask = UInt(sW.W); val off = UInt(24.W) }
  class ScaleEntry extends Bundle {
    val w = Vec(numOutBlocks, new ScaleWord); val v = Vec(numOutBlocks, Bool())
    val base = UInt(scaleMem_addr_width.W); val resident = Bool(); val act_off = UInt(scaleMemActWriteAddrWidth.W)
  }
  val scale_q = Module(new Queue(new ScaleEntry, 4))
  scale_room := scale_q.io.enq.ready
  val enq = Wire(new ScaleEntry)
  enq.base     := Mux(io.linear_scales, io.linear_base, scale_mem_mvout_base_addr_act)
  enq.resident := Mux(io.linear_scales, io.linear_resident, io.scale_resident)
  // matmul: resident scales go to the act-window half this matmul does not read; SPAD_REQUANT: half 0
  enq.act_off  := Mux(io.linear_scales, 0.U, (~io.scale_read_act_sel).asUInt << (scaleMemActWriteAddrWidth - 1))
  for (k <- 0 until numOutBlocks) {
    val b    = sc_b0 + k.U
    val a    = (b * M_rows + sc_m)(23, 0)          // byte offset in [GN][M]
    val sidx = b(log2Ceil(nStreams) - 1, 0)
    val lane = a(lgSW - 1, 0)
    val word = a >> lgSW
    val ok   = sc_fire && (b < GN)
    val last = lane === (sW - 1).U || sc_m === M_rows - 1.U
    val mask_now = st_mask(sidx) | UIntToOH(lane, sW)
    assert(!(ok && st_mask(sidx) =/= 0.U && st_word(sidx) =/= word), "scale stream: block column arrived out of order")
    enq.v(k)      := ok && last
    enq.w(k).data := VecInit((0 until sW).map(j => Mux(lane === j.U, scale_e8m0_vec(k), st_data(sidx)(j)))).asUInt
    enq.w(k).mask := mask_now
    enq.w(k).off  := word << lgSW
    when (ok) {
      st_data(sidx)(lane) := scale_e8m0_vec(k)
      st_mask(sidx)       := Mux(last, 0.U, mask_now)
      st_word(sidx)       := word
    }
  }
  scale_q.io.enq.valid := enq.v.asUInt.orR
  scale_q.io.enq.bits  := enq

  // ---- drain: per word one (masked) Put to the scale DRAM address, then, if resident, its 8-byte granules into
  // the act-scale window (whole granules: M % 8 == 0) ----
  val head    = scale_q.io.deq.bits
  val dq_k    = RegInit(0.U((log2Ceil(numOutBlocks) max 1).W))
  val dq_put  = RegInit(false.B)                         // this word's Put done
  val gran    = sW / 8
  val dq_g    = RegInit(0.U((log2Ceil(gran) max 1).W))   // next act granule
  val dq_gend = RegInit(false.B)                         // all act granules done
  val cur     = head.w(dq_k)
  val cur_v   = scale_q.io.deq.valid && head.v(dq_k)
  io.scaleMem_write.valid     := cur_v && !dq_put
  io.scaleMem_write.bits.addr := head.base + cur.off
  io.scaleMem_write.bits.data := cur.data
  io.scaleMem_write.bits.mask := cur.mask
  val g_mask  = cur.mask.asTypeOf(Vec(gran, UInt(8.W)))(dq_g)
  val act_on  = cur_v && head.resident && !dq_gend
  io.scaleMem_write_act_resident.valid     := act_on && g_mask =/= 0.U
  io.scaleMem_write_act_resident.bits.addr := head.act_off + cur.off + (dq_g << 3)
  io.scaleMem_write_act_resident.bits.data := cur.data.asTypeOf(Vec(gran, UInt(64.W)))(dq_g)
  assert(!(act_on && g_mask =/= 0.U && g_mask =/= "hff".U), "resident scales: partial 8-byte granule (M not a multiple of 8)")
  val g_step  = act_on && (g_mask === 0.U || io.scaleMem_write_act_resident.fire)
  val put_ok  = dq_put || io.scaleMem_write.fire
  val act_ok  = !head.resident || dq_gend || (g_step && dq_g === (gran - 1).U)
  val slot_ok = scale_q.io.deq.valid && (!head.v(dq_k) || (put_ok && act_ok))
  when (io.scaleMem_write.fire) { dq_put := true.B }
  when (g_step) { dq_g := dq_g + 1.U; when (dq_g === (gran - 1).U) { dq_gend := true.B } }
  when (slot_ok) {
    dq_put := false.B; dq_g := 0.U; dq_gend := false.B
    dq_k := Mux(dq_k === (numOutBlocks - 1).U, 0.U, dq_k + 1.U)
  }
  scale_q.io.deq.ready := slot_ok && dq_k === (numOutBlocks - 1).U

  io.scale_flush_busy := scale_q.io.deq.valid || st_mask.map(_ =/= 0.U).reduce(_ || _)

  when(io.scale_mem_counter_reset_flag) {
    lin_m := 0.U; lin_b := 0.U
    ag_sb := 0.U; ag_g := 0.U; ag_jg := 0.U; ag_bib := 0.U; ag_row := 0.U
  }
}

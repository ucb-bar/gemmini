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

    // Block-scale floor: FP8 uses the _po2 convention (pmax=0); FP4/FP6 use OCP emax (FP4=2, FP6=4).
    val log2_pmax_floor = MuxLookup(bits, 0.U)(Seq(
      FP4 -> 2.U,
      FP6 -> 4.U,
      FP8 -> 0.U,
      BF16 -> 0.U
    ))
    
    (exp_bits, mant_bits, pmax, log2_pmax_floor)
  }
}

class MxRequantizerAccResp[T <: Data: Arithmetic](fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]]) extends Bundle {
  val out = new MxRequantizerAccMemDataOut[T] (rDataType, fullDataType)
  val mx_mode = UInt(2.W)
  val is_gpu = Bool()
  val gpu_addr = UInt(32.W)
}

class MxRequantizerIO[T <: Data: Arithmetic](
  sp_data_width: Int,
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleMemActWriteAddrWidth: Int,   // byte-addr width of the on-chip act-scale write port
  scaleSize: Int,
  scaleMembasewrite: Int,
  lutConfig: GemminiLUTConfig,
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
  val scaleMem_write = Decoupled(new ScalingFactorWriteReq(scaleMem_addr_width, scaleMem_data_width)) 
  val lut0_write = Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(0))))
  val lut1_write = Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(1))))
  val lut2_write = Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(2))))
  val spad_projected_data   = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected))
  val spad_deprojected_data = Vec(sp_banks, Flipped(new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val read_a = Input(Bool())
  val read_d = Input(Bool())
  val scale_mem_mvout_base_addr_act = Input(UInt(scaleMem_addr_width.W)) // from execute controller
  val quant_lut_update_granularity = Input(UInt(lutConfig.lutUpdateRegularityWidth.W))
  val loop_bound_i = Input(UInt(iterator_bitwidth.W)) // from  controller
  val loop_bound_j = Input(UInt(iterator_bitwidth.W)) // from  controller
  val loop_bound_k = Input(UInt(iterator_bitwidth.W)) // from  controller
  val scale_mem_counter_reset_flag = Input(Bool())
  // When set, the coalescer also flushes act block-scales transposed to [GN][M] onto
  // scaleMem_write_act_resident (64b/beat); otherwise that port is idle.
  val scale_resident = Input(Bool())
  val scaleMem_write_act_resident = Decoupled(new ScalingFactorWriteReq(scaleMemActWriteAddrWidth, 64))
  // Splits E4M3-quad (4-bit LUT index) from E4M3-single (8-bit code): both are format0/altfmt0.
  val lut_en = Input(Bool())
}
   
class MxRequantizer[T <: Data](
  sp_data_width: Int,
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleMemActWriteAddrWidth: Int,   // byte-addr width of the on-chip act-scale write port
  scaleSize: Int,
  scaleMembasewrite: Int,
  lutConfig: GemminiLUTConfig,
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
  val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
  val spad_row_t = Vec(2*meshColumns, Vec(tileColumns, weightTypeProjected))
  val half_acc_row_t = Vec(meshColumns/2, Vec(tileColumns, accType))
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
  format_reg := quant_dataType.asUInt(1, 0)
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
  
  val e5m2Lut = lutConfig.projFormat == LutFP8E5M2
  val (exp_bits, mant_bits, pmax, log2_pmax_floor_raw) = MxFloatFormat(format_reg)
  // Block-scale floor overrides by (format code, altfmt): E5M2 -> 16, E4M3-quad -> 8, E2M3 -> 2;
  // everything else uses the MxFloatFormat default.
  val log2_pmax_floor = MuxCase(log2_pmax_floor_raw, Seq(
    (format_reg === 0.U && altfmt_reg) -> 16.U,                    // E5M2
    (format_reg === 0.U && !altfmt_reg && lut_en_reg) -> 8.U,      // E4M3-quad (LUT nibble)
    (format_reg === 1.U && altfmt_reg) ->  2.U                     // E2M3
  ))
  val data_buffer_counter = RegInit(0.U(1.W))
  //buffer twice for 16-lane mode
  val half_lanes = 16
  val input_32_buffer = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(inputdataWidth.W))))
  val input_16_buffer_gpu = RegInit(VecInit(Seq.fill(io.outputnumLanes/2)(0.U(inputdataWidth.W))))
  val should_compute = Wire(Bool())
  val quantize_valid = RegNext(should_compute)
  val quantized_buffer = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(8.W)))) 
  dontTouch(quantized_buffer)
  dontTouch( should_compute)
  should_compute := false.B
  val quant_half_counter = RegInit(false.B)
  val first_half_buf     = RegInit(0.U(128.W))

  // This is the datapath pack structure, not the literal element width: E5M2 and E4M3-quad have wider
  // codes but ride the 6-bit LUT datapath (buffering, packing, coalescer cadence) like FP6.
  val total_bits_raw = 1.U +& exp_bits +& mant_bits
  val datapath_pack_mode = WireDefault(0.U(5.W))
  datapath_pack_mode := Mux(format_reg === 0.U && (altfmt_reg || lut_en_reg), 6.U, total_bits_raw)

  // Output packing: E4M3-single -> 8-bit direct codes; E4M3-quad/E5M2/E3M2/E2M3 -> 4-bit LUT indices;
  // FP4 -> 4-bit direct. E4M3-single vs -quad share format0/altfmt0, split only by latched lut_en.
  val out_is_fp4  = format_reg === 2.U
  val out_is_lut4 = (format_reg === 1.U) || (format_reg === 0.U && (altfmt_reg || lut_en_reg))
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
  val quantLut = Module(new QuantLut(
    lutConfig,
    outputnumLanes = io.outputnumLanes ,
    sp_bank_entries = sp_bank_entries,
    sp_banks = sp_banks,
    sp_width = sp_width,
    sp_width_projected = sp_width_projected,
    lut_update_regularity_w = config.lutUpdateRegularityW,
    lut_update_regularity_act_in = config.lutUpdateRegularityActIn,
    lut_update_regularity_act_out = config.lutUpdateRegularityActOut,
    iterator_bitwidth = iterator_bitwidth
  ))

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

  }.elsewhen(io.requant_data_in_gpu.fire && data_buffer_counter === 1.U) {
    pipe_in.valid := true.B
    pipe_in.bits.mx_mode := format_reg
    val combined = input_16_buffer_gpu ++ io.requant_data_in_gpu.bits.data
    pipe_in.bits.out.full_mx_data_out := VecInit(combined).asTypeOf(half_acc_row_t)
    pipe_in.bits.out.fromDMA := false.B
    pipe_in.bits.is_gpu := true.B
    pipe_in.bits.gpu_addr := gpu_addr
  }


  final_pipe_out := oldest_pipe_out

  // Two-cycle accumulation registers for FP4 / FP6:
  val fp6_lut_out     = Cat(quantLut.io.projected_data.bits.reverse)
  val fp6_row0        = (0 until 32).map(k => first_half_buf(4*k+3, 4*k))    
  val fp6_row1        = (0 until 32).map(k => fp6_lut_out(4*k+3, 4*k))      
  val fp6_interleaved = (0 until 16).flatMap { j => Seq(fp6_row0(2*j), fp6_row1(2*j), fp6_row0(2*j+1), fp6_row1(2*j+1)) }
  val fp6_combined    = Cat(fp6_interleaved.reverse)   
  val fp6_combined_wire = WireDefault(fp6_combined)                        
  val fp6_lut_out_wire = WireDefault(fp6_lut_out)
  dontTouch(fp6_combined_wire)
  dontTouch(fp6_lut_out_wire)

  val fp4_row0        = (0 until 32).map(k => first_half_buf(4*k+3, 4*k))
  val fp4_row1        = (0 until 32).map(k => extracted_data(4*k+3, 4*k))
  val fp4_row0_wire = WireDefault(VecInit(fp4_row0))
  val fp4_row1_wire = WireDefault(VecInit(fp4_row1))
  dontTouch(fp4_row0_wire)
  dontTouch(fp4_row1_wire)
  val fp4_interleaved = (0 until 16).flatMap { j => Seq(fp4_row0(2*j), fp4_row1(2*j), fp4_row0(2*j+1), fp4_row1(2*j+1)) }
  val fp4_combined    = Cat(fp4_interleaved.reverse)                       
  val fp4_combined_wire = WireDefault(fp4_combined)                        
  dontTouch(fp4_combined_wire)

  final_pipe_out.bits.out.quant_mx_data_out := 0.U.asTypeOf(spad_row_t)
  final_pipe_out.bits.out.is_garbage := false.B
  val lut_valid = quantLut.io.projected_data.valid
  when(out_is_8bit) {
    final_pipe_out.valid := oldest_pipe_out.valid
    final_pipe_out.bits.out.quant_mx_data_out := Mux(quantize_valid, extracted_data, quant_data_held).asTypeOf(spad_row_t)
    final_pipe_out.bits.out.is_garbage := false.B
  }.elsewhen(out_is_lut4) {
    when(lut_valid) {
      when(!quant_half_counter) {
        first_half_buf     := fp6_lut_out
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.bits.out.is_garbage := !quant_half_counter &&  (oldest_pipe_out.valid) 
    final_pipe_out.valid :=  (oldest_pipe_out.valid)  
    final_pipe_out.bits.out.quant_mx_data_out := Mux(lut_valid, fp6_combined, quant_data_held).asTypeOf(spad_row_t)

  }.elsewhen(out_is_fp4) {
    when(quantize_valid) {
      when(!quant_half_counter) {
        first_half_buf     := extracted_data(127, 0)
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.valid := (oldest_pipe_out.valid)
    final_pipe_out.bits.out.is_garbage := !quant_half_counter && oldest_pipe_out.valid
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

  val can_enqueue = pipe_in.ready
  val can_enqueue_wire = WireDefault(can_enqueue)
  val full_precision_valid = RegNext(can_enqueue && pipelined_out_0.valid && (datapath_pack_mode === 16.U))

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
    when(pipelined_out_0.valid && (datapath_pack_mode =/= 16.U)) {
      should_compute := true.B
    }
  }
  
  val block_max = Wire(UInt(inputdataWidth.W))
  block_max := 0.U
  val flat64 = pipelined_out_0.bits.out.full_mx_data_out.flatten.map(_.asUInt)
  val flat64_wire = WireDefault(VecInit(flat64))
  dontTouch(flat64_wire)
  val reshaped_pipelined_out_0 = VecInit(
    flat64.flatMap(
      x => (0 until 4).map(i => x(16*(i+1)-1, 16*i))
    )
  )
  // NaN and Inf are tracked separately because the reference divides the block by its own max:
  // /nan sends every element to NaN, /inf sends the finite ones to zero. See BF16ToE4M3's caller.
  val block_has_nan = WireDefault(false.B)
  val block_has_inf = WireDefault(false.B)
  dontTouch(block_has_nan)
  dontTouch(block_has_inf)
  when(should_compute) {
    val mags = reshaped_pipelined_out_0.map { e =>
      val mag   = abs(e.asUInt)
      val expOnes = mag(14, 7).andR     // BF16: exp field all-ones → NaN or Inf
      val isNan = expOnes && mag(6, 0).orR
      val isInf = expOnes && !mag(6, 0).orR
      (Mux(expOnes, 0.U, mag), isNan, isInf)
    }
    block_max     := mags.map(_._1).reduce { (a, b) => Mux(a > b, a, b) }
    block_has_nan := mags.map(_._2).reduce(_ || _)
    block_has_inf := mags.map(_._3).reduce(_ || _)
  }
  val block_nonfinite = block_has_nan || block_has_inf
  
  val block_max_uint = block_max.asUInt
  val block_max_uint_wire = WireDefault(block_max_uint)
  dontTouch(block_max_uint_wire)
  val scale_exponent = Wire(SInt(9.W))
  val scale_e8m0 = WireDefault(0.U(8.W))
  val neg_e8m0_clamped = WireDefault(0.U(8.W))
  dontTouch(scale_e8m0)
  dontTouch(neg_e8m0_clamped)
  scale_exponent := 0.S
  
  // _po2 floors amax at fp32 eps (2^-23), BF16 biased exponent 104, subsuming the all-zero/subnormal
  // cases. Gated on should_compute: this module is shared with the BF16 path, whose idle values must
  // stay bit-identical.
  val EPS_BIASED_EXP = 104.U(8.W)
  val max_biased_exp = block_max_uint(14, 7)
  when (should_compute) {
    val clamped_exp = Mux(max_biased_exp < EPS_BIASED_EXP, EPS_BIASED_EXP, max_biased_exp)
    scale_exponent := clamped_exp.zext.asSInt - 127.S - log2_pmax_floor.zext.asSInt

    val std_e8m0 = scale_exponent + 127.S
    val std_clamped = Mux(std_e8m0 < 0.S, 0.U(8.W),
                      Mux(std_e8m0 > 254.S, 254.U(8.W), std_e8m0.asUInt(7, 0)))
    // A non-finite anywhere in the block propagates as E8M0 255 (NaN). Valid codes stop at 254 so
    // that 255 is reserved; it used to be a reachable clamp value.
    scale_e8m0 := Mux(block_nonfinite, 255.U, std_clamped)

    val neg_e8m0 = 127.S(9.W) - scale_exponent
    neg_e8m0_clamped := Mux(neg_e8m0 < 0.S, 0.U(8.W),
                        Mux(neg_e8m0 > 254.S, 254.U(8.W), neg_e8m0.asUInt(7, 0)))
  } .otherwise {
    scale_exponent := 0.S
    scale_e8m0 := 127.U
    // neg_e8m0_clamped deliberately left at its WireDefault(0), as before.
  }
  
  val BF16ScaleRoundToTiny = Module(new BF16ScaleRoundToTiny(outputnumLanes = io.outputnumLanes, e5m2Lut = e5m2Lut))
  BF16ScaleRoundToTiny.io.in_bf16 := reshaped_pipelined_out_0
  BF16ScaleRoundToTiny.io.scale_e8m0 := neg_e8m0_clamped
  BF16ScaleRoundToTiny.io.dataType := format_reg
  BF16ScaleRoundToTiny.io.mx_fp8_altfmt := altfmt_reg
  BF16ScaleRoundToTiny.io.block_has_nan := block_has_nan
  BF16ScaleRoundToTiny.io.block_has_inf := block_has_inf
  quantized_buffer := RegNext(BF16ScaleRoundToTiny.io.out)

  // LUT projection source: low rdataW bits of the quantized code (full 8-bit for E5M2, low-6 for FP6).
  val rdataW = lutConfig.rdataWidth
  val quant_fp6 = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(rdataW.W))))
  dontTouch(quant_fp6)
  quant_fp6 := Mux(out_is_lut4, VecInit((0 until io.outputnumLanes).map(i => quantized_buffer(i)(rdataW - 1, 0))),
  VecInit(Seq.fill(io.outputnumLanes)(0.U(rdataW.W))))

  quantLut.io.spad_projected_data <> io.spad_projected_data
  quantLut.io.spad_deprojected_data <> io.spad_deprojected_data
  quantLut.io.quant_lut_update_granularity := io.quant_lut_update_granularity
  quantLut.io.read_a := io.read_a
  quantLut.io.read_d := io.read_d
  quantLut.io.mx_fp8_altfmt := altfmt_reg
  quantLut.io.output_mx_format := format_reg
  quantLut.io.loop_bound_i := io.loop_bound_i
  quantLut.io.loop_bound_j := io.loop_bound_j
  quantLut.io.loop_bound_k := io.loop_bound_k
  quantLut.io.quant_fp6.valid := false.B
  quantLut.io.quant_fp6.bits := DontCare
  quantLut.io.lut_write_weight <> io.lut0_write
  quantLut.io.lut_write_act_in <> io.lut1_write
  quantLut.io.lut_write_act_out <> io.lut2_write

  when(quantize_valid && out_is_lut4) {
      quantLut.io.quant_fp6.valid := true.B
      quantLut.io.quant_fp6.bits := quant_fp6
  }

  
  // Scale-factor coalescer: buffer the tile's E8M0 block scales row-major (byte m*GN+b) via an address
  // generator mirroring the mvout iteration, then flush contiguous 32-byte chunks. Arrival decode:
  //   fp8:    sb=super-block, bib=block-in-sb, m=16*g+row,        b=2*sb+bib
  //   nibble: sb=block(b),    bib=row-half,    m=32*g+16*bib+row, b=sb   (2 rows/beat)
  val ROWS_PER_HALF = meshColumns * tileColumns            // 16 rows per (bib) row-half
  val isNibble      = out_is_fp4 || out_is_lut4   // 4-bit-packed outputs: FP4, E3M2/E2M3, and E5M2 (code0/altfmt)
  val GN            = Mux(isNibble, io.loop_bound_j, (io.loop_bound_j >> 1).asUInt)
  val tiles_I       = io.loop_bound_i
  val num_super     = (GN + 1.U) >> 1                      // fp8: ceil(GN/2) super-blocks
  val gn_odd        = GN(0)

  val coalesceSize  = 2048                                 // >= max M*GN for supported MX matmuls
  val coalescer     = RegInit(VecInit(Seq.fill(coalesceSize)(0.U(8.W))))

  // address-generator counters (advance one per should_compute beat). Nesting sb > g > bib > row.
  val ag_sb  = RegInit(0.U(9.W))
  val ag_g   = RegInit(0.U(9.W))
  val ag_bib = RegInit(0.U(1.W))
  val ag_row = RegInit(0.U(log2Ceil(ROWS_PER_HALF).W))
  val flushing = RegInit(false.B)

  // Transposed act-scale residency flush state (gated on io.scale_resident; idle otherwise).
  val flushing_act = RegInit(false.B)   // never set unless scale_resident
  val flush_act_bi = RegInit(0.U(9.W))  // block-column index bi, 0..GN-1  (outer)
  val flush_act_wm = RegInit(0.U(16.W)) // 64b word within a bi's M-row span, 0..(M/8 - 1)  (inner)

  val blocks_in_sb = Mux(isNibble, 2.U,
                       Mux((ag_sb === (num_super - 1.U)) && gn_odd, 1.U, 2.U))
  val sb_count = Mux(isNibble, GN, num_super)
  val cur_b    = Mux(isNibble, ag_sb, (ag_sb << 1).asUInt + ag_bib)
  val cur_m    = Mux(isNibble, ag_g * 32.U + ag_bib * ROWS_PER_HALF.U + ag_row,
                               ag_g * ROWS_PER_HALF.U + ag_row)
  val cur_byte = cur_m * GN + cur_b

  // Hold off overwriting the coalescer until both flushes finish.
  when(should_compute && !flushing && !flushing_act) {
    coalescer(cur_byte) := scale_e8m0
    // increment nested counters: row -> bib -> i-tile -> sb (block for nibble, super-block for fp8)
    when(ag_row === (ROWS_PER_HALF - 1).U) {
      ag_row := 0.U
      when(ag_bib === (blocks_in_sb - 1.U)) {
        ag_bib := 0.U
        when(ag_g === (tiles_I - 1.U)) {
          ag_g := 0.U
          when(ag_sb === (sb_count - 1.U)) {
            ag_sb := 0.U
            flushing := true.B                              // full tile collected -> flush
            flushing_act := io.scale_resident               // also start the transposed act flush
          }.otherwise { ag_sb := ag_sb + 1.U }
        }.otherwise { ag_g := ag_g + 1.U }
      }.otherwise { ag_bib := ag_bib + 1.U }
    }.otherwise { ag_row := ag_row + 1.U }
  }

  // Flush: stream the coalescer out as contiguous 32-byte row-major chunks.
  val total_scales   = tiles_I * Mux(isNibble, 32.U, ROWS_PER_HALF.U) * GN   // M*GN
  val num_flush_rows = total_scales >> log2Ceil(scales_per_write)
  val flush_row      = RegInit(0.U(16.W))
  val flush_base     = flush_row << log2Ceil(scales_per_write)

  io.scaleMem_write.valid     := false.B
  io.scaleMem_write.bits.addr := scale_mem_mvout_base_addr_act + (flush_row << log2Ceil(scaleMem_data_width/8))
  io.scaleMem_write.bits.data := Cat((0 until scales_per_write).map(i => coalescer(flush_base + i.U)).reverse)
  when(flushing) {
    io.scaleMem_write.valid := true.B
    when(io.scaleMem_write.fire) {
      when(flush_row === (num_flush_rows - 1.U)) {
        flush_row := 0.U
        flushing  := false.B
      }.otherwise { flush_row := flush_row + 1.U }
    }
  }

  // Transposed act-scale residency flush: re-read the row-major coalescer and stream it in the
  // A-scale operand layout [GN][M] (byte bi*M+m) as 64b beats, bi outer / wm inner.
  //   M = tiles_I * (32 nibble | 16 fp8); words_per_bi = M/8; word w = bi*words_per_bi + wm; addr = w*8.
  val M_rows       = tiles_I * Mux(isNibble, 32.U, ROWS_PER_HALF.U)   // == total_scales / GN
  val words_per_bi = M_rows >> 3                                       // M/8 (M is a multiple of 8)
  val act_m0       = flush_act_wm << 3
  val act_word     = flush_act_bi * words_per_bi + flush_act_wm
  val act_data     = Cat((0 until 8).reverse.map(j => coalescer((act_m0 + j.U) * GN + flush_act_bi)))

  io.scaleMem_write_act_resident.valid     := false.B
  io.scaleMem_write_act_resident.bits.addr := (act_word << 3)         // byte addr = w*8, 0-based
  io.scaleMem_write_act_resident.bits.data := act_data
  when(flushing_act) {
    io.scaleMem_write_act_resident.valid := true.B
    when(io.scaleMem_write_act_resident.fire) {
      when(flush_act_wm === (words_per_bi - 1.U)) {
        flush_act_wm := 0.U
        when(flush_act_bi === (GN - 1.U)) {
          flush_act_bi := 0.U
          flushing_act := false.B
        }.otherwise { flush_act_bi := flush_act_bi + 1.U }
      }.otherwise { flush_act_wm := flush_act_wm + 1.U }
    }
  }

  when(io.scale_mem_counter_reset_flag) {
    ag_sb := 0.U; ag_g := 0.U; ag_bib := 0.U; ag_row := 0.U
    flush_row := 0.U; flushing := false.B
    flush_act_bi := 0.U; flush_act_wm := 0.U; flushing_act := false.B
  }
}

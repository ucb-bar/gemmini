package gemmini

import chisel3._
import chisel3.util._
import javax.swing.plaf.synth.Region

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

    val log2_pmax_floor = MuxLookup(bits, 8.U)(Seq(
      FP4 -> 2.U,
      FP6 -> 4.U,
      FP8 -> 8.U,
      BF16 -> 16.U
    ))
    
    (exp_bits, mant_bits, pmax, log2_pmax_floor)
  }
}

class MxRequantizerAccResp[T <: Data: Arithmetic](fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]]) extends Bundle {
  val out = new MxRequantizerAccMemDataOut[T] (rDataType, fullDataType)
  val mx_mode = UInt(2.W)
}

class MxRequantizerIO[T <: Data: Arithmetic](
  sp_data_width: Int,  
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
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
}
   
class MxRequantizer[T <: Data](
  sp_data_width: Int, 
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
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
  val format_reg = RegNext(quant_dataType.asUInt, 0.U)
  
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
  
  val (exp_bits, mant_bits, pmax, log2_pmax_floor) = MxFloatFormat(format_reg)
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


  val total_bits_per_element = WireDefault(0.U(5.W))
  total_bits_per_element := 1.U  +&  exp_bits  +&  mant_bits 

  val extracted_data = WireDefault((0.U((io.outputnumLanes*8).W))) // 256bits / 128bits


  val pipe_in = Wire(Decoupled(new MxRequantizerAccResp[T](half_acc_row_t, spad_row_t)(ev)))
  pipe_in.valid := false.B
  pipe_in.bits := DontCare
  //val pipelined_out = Reg(Vec(pipelineLatency, Decoupled(new MxRequantizerAccResp[T](half_acc_row_t, spad_row_t)(ev))))
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
  
  
  when(io.mxacc_req.mx_data_in.fire) {
    pipe_in.valid := true.B
    pipe_in.bits.mx_mode := io.mxacc_req.mx_mode
    pipe_in.bits.out.full_mx_data_out := io.mxacc_req.mx_data_in.bits.full_mx_data_in
    pipe_in.bits.out.fromDMA := io.mxacc_req.mx_data_in.bits.fromDMA
    pipe_in.bits.out.is_last_half := io.mxacc_req.mx_data_in.bits.is_last_half
    pipe_in.bits.out.acc_bank_id := io.mxacc_req.mx_data_in.bits.acc_bank_id

  //}
  }.elsewhen(io.requant_data_in_gpu.fire && data_buffer_counter === 1.U) {
    pipe_in.valid := true.B
    pipe_in.bits.mx_mode := format_reg
    val combined = input_16_buffer_gpu ++ io.requant_data_in_gpu.bits.data
    pipe_in.bits.out.full_mx_data_out := VecInit(combined.reverse).asTypeOf(half_acc_row_t)
    pipe_in.bits.out.fromDMA := true.B
  }

  // for (i <- 1 until pipelineLatency) {
  //   pipelined_out(i) := pipelined_out(i-1)
  // }
  // pipelined_out(0).valid := pipe_in.valid
  // pipelined_out(0).bits  := pipe_in.bits
  final_pipe_out := oldest_pipe_out
  // val packed_quant_data = RegInit((0.U((128).W)))
  // val packed_quant_data_counter = RegInit(0.U(1.W)) 

  // when(total_bits_per_element === 6.U || total_bits_per_element === 4.U){ 
  //   when(should_compute){
  //     packed_quant_data_counter := ~packed_quant_data_counter
  //     when(packed_quant_data_counter === 1.U){
  //       when(total_bits_per_element === 6.U ){
  //         packed_quant_data := Cat(quantLut.io.projected_data.bits.reverse)   
  //       }.otherwise{
  //         packed_quant_data :=  extracted_data(127,0)                
  //       }        
  //     }
  //   }
  // }
  
  // when(total_bits_per_element === 8.U){ 
  //   final_pipe_out.valid := quantize_valid
  //   final_pipe_out.bits.out.quant_mx_data_out := extracted_data.asTypeOf(spad_row_t)
  // }.elsewhen(total_bits_per_element === 6.U){ 
  //   final_pipe_out.valid := quantLut.io.projected_data.valid && (packed_quant_data_counter === 1.U)
  //   val lut_quant_data = Cat(quantLut.io.projected_data.bits.reverse)
  //   val row0 = (0 until 32).map(k => packed_quant_data(4*k+3,  4*k))  // r0_c0..c31
  //   val row1 = (0 until 32).map(k => lut_quant_data(4*k+3,    4*k))  // r1_c0..c31
  //   val interleaved = (0 until 16).flatMap { j => Seq(row0(2*j), row0(2*j+1), row1(2*j), row1(2*j+1)) }
  //   final_pipe_out.bits.out.quant_mx_data_out := Cat(interleaved.reverse).asTypeOf(spad_row_t)
  // }.elsewhen(total_bits_per_element === 4.U){
  //   final_pipe_out.valid := quantize_valid && (packed_quant_data_counter === 1.U)
  //   val row0 = (0 until 32).map(k => packed_quant_data(4*k+3,  4*k))  // r0_c0..c31
  //   val row1 = (0 until 32).map(k => extracted_data(4*k+3,    4*k))  // r1_c0..c31
  //   val interleaved = (0 until 16).flatMap { j => Seq(row0(2*j), row0(2*j+1), row1(2*j), row1(2*j+1)) }
  //   final_pipe_out.bits.out.quant_mx_data_out := Cat(interleaved.reverse).asTypeOf(spad_row_t)
  // }
 
  // Two-cycle accumulation registers for FP4 / FP6:
  val quant_half_counter = RegInit(false.B)
  val first_half_buf     = RegInit(0.U(128.W))

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

  final_pipe_out.valid := false.B
  final_pipe_out.bits.out.quant_mx_data_out := 0.U.asTypeOf(spad_row_t)
  final_pipe_out.bits.out.is_garbage := false.B

  when(total_bits_per_element === 8.U) {
    final_pipe_out.valid := quantize_valid
    final_pipe_out.bits.out.quant_mx_data_out := extracted_data.asTypeOf(spad_row_t)
    final_pipe_out.bits.out.is_garbage := false.B
  }.elsewhen(total_bits_per_element === 6.U) {
    val lut_valid = quantLut.io.projected_data.valid
    when(lut_valid) {
      when(!quant_half_counter) {
        first_half_buf     := fp6_lut_out
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.bits.out.is_garbage := !quant_half_counter && lut_valid
    final_pipe_out.valid := lut_valid 
    final_pipe_out.bits.out.quant_mx_data_out := (fp6_combined).asTypeOf(spad_row_t)

  }.elsewhen(total_bits_per_element === 4.U) {
    when(quantize_valid) {
      when(!quant_half_counter) {
        first_half_buf     := extracted_data(127, 0)
        quant_half_counter := true.B
      }.otherwise {
        quant_half_counter := false.B
      }
    }
    final_pipe_out.valid := (quantize_valid) 
    final_pipe_out.bits.out.is_garbage := !quant_half_counter && quantize_valid
    final_pipe_out.bits.out.quant_mx_data_out := (fp4_combined).asTypeOf(spad_row_t)
  }
  
  when(quantize_valid) {
    when(total_bits_per_element === 4.U){
      extracted_data := Cat((0 until io.outputnumLanes).map(i => quantized_buffer(i)(3, 0)).reverse)
    }.elsewhen(total_bits_per_element === 8.U){
      extracted_data := Cat(quantized_buffer.reverse)
    }.otherwise{
      extracted_data := 0.U((io.outputnumLanes*8).W)
    }
  }
  

  val can_enqueue = pipe_in.ready
  val can_enqueue_wire = WireDefault(can_enqueue)
  val full_precision_valid = RegNext(can_enqueue && pipelined_out_0.valid && (total_bits_per_element === 16.U))

  dontTouch(can_enqueue_wire)
    // Only allow input handshake when queue has space
  io.mxacc_req.mx_data_in.ready := can_enqueue
  io.requant_data_in_gpu.ready := can_enqueue && !io.mxacc_req.mx_data_in.fire
  
  io.mxacc_req.mx_data_out.bits.is_garbage := final_pipe_out.bits.out.is_garbage
  io.mxacc_req.mx_data_out.bits.full_mx_data_out := final_pipe_out.bits.out.full_mx_data_out
  io.mxacc_req.mx_data_out.bits.quant_mx_data_out  := final_pipe_out.bits.out.quant_mx_data_out
  io.mxacc_req.mx_data_out.valid := final_pipe_out.valid || full_precision_valid
  io.mxacc_req.mx_data_out.bits.fromDMA := final_pipe_out.bits.out.fromDMA
  io.mxacc_req.mx_data_out.bits.acc_bank_id := final_pipe_out.bits.out.acc_bank_id
  io.mxacc_req.mx_data_out.bits.is_last_half := final_pipe_out.bits.out.is_last_half


  oldest_pipe_out.ready := io.mxacc_req.mx_data_out.ready
  final_pipe_out.ready := io.mxacc_req.mx_data_out.ready
  
  should_compute := false.B
  when (can_enqueue) {
    when(pipelined_out_0.valid && (total_bits_per_element =/= 16.U)) {
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
  when(should_compute) {
    block_max := reshaped_pipelined_out_0.map { e =>
      val mag        = abs(e.asUInt)
      val isNanOrInf = mag(14, 7).andR   // BF16: exp field all-ones → NaN or Inf
      Mux(isNanOrInf, 0.U, mag)
    }.reduce { (a, b) => Mux(a > b, a, b) }
  }
  
  val block_max_uint = block_max.asUInt
  val block_max_uint_wire = WireDefault(block_max_uint)
  dontTouch(block_max_uint_wire)
  val scale_exponent = Wire(SInt(9.W))
  val scale_e8m0 = WireDefault(0.U(8.W))
  val neg_e8m0_clamped = WireDefault(0.U(8.W))
  dontTouch(scale_e8m0)
  dontTouch(neg_e8m0_clamped)
  scale_exponent := 0.S
  
  when(block_max_uint === 0.U || block_max_uint(14, 7) === 0.U) {
    scale_exponent := 0.S
    scale_e8m0 := 127.U 
  }.otherwise {
    val max_biased_exp = block_max_uint(14, 7)
    scale_exponent := max_biased_exp.zext.asSInt - 127.S - log2_pmax_floor.zext.asSInt
    val std_e8m0 = scale_exponent + 127.S
    when(std_e8m0 < 0.S){
      scale_e8m0 := 0.U
    }.elsewhen(std_e8m0 > 255.S){ 
      scale_e8m0 := 255.U
    }.otherwise{ 
      scale_e8m0 := std_e8m0.asUInt(7, 0)}

    val neg_e8m0 = 127.S(9.W) - scale_exponent
    
    when(neg_e8m0 < 0.S) {
      neg_e8m0_clamped := 0.U
    }.elsewhen(neg_e8m0 > 254.S) {
      neg_e8m0_clamped := 254.U
    }.otherwise {
      neg_e8m0_clamped := neg_e8m0.asUInt(7, 0)
    }
  }
  
  val BF16ScaleRoundToTiny = Module(new BF16ScaleRoundToTiny(outputnumLanes = io.outputnumLanes))
  BF16ScaleRoundToTiny.io.in_bf16 := reshaped_pipelined_out_0
  BF16ScaleRoundToTiny.io.scale_e8m0 := neg_e8m0_clamped 
  BF16ScaleRoundToTiny.io.dataType := format_reg
  quantized_buffer := RegNext(BF16ScaleRoundToTiny.io.out)
  

  
  // when(quantize_valid) {
  //   when(total_bits_per_element === 4.U){
  //     extracted_data := Cat((0 until io.outputnumLanes).map(i => quantized_buffer(i)(3, 0)).reverse)
  //   }.elsewhen(total_bits_per_element === 8.U){
  //     extracted_data := Cat(quantized_buffer.reverse)
  //   }.otherwise{
  //     extracted_data := 0.U((io.outputnumLanes*8).W)
  //   }
  // }
  
 

  val quant_fp6 = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  dontTouch(quant_fp6)
  quant_fp6 := Mux(total_bits_per_element === 6.U, VecInit((0 until io.outputnumLanes).map(i => quantized_buffer(i)(5, 0))), 
  VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  //val quant_projected_data = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(4.W))))

 
  
  quantLut.io.spad_projected_data <> io.spad_projected_data
  quantLut.io.spad_deprojected_data <> io.spad_deprojected_data
  quantLut.io.quant_lut_update_granularity := io.quant_lut_update_granularity
  quantLut.io.read_a := io.read_a
  quantLut.io.read_d := io.read_d
  quantLut.io.loop_bound_i := io.loop_bound_i
  quantLut.io.loop_bound_j := io.loop_bound_j
  quantLut.io.loop_bound_k := io.loop_bound_k
  //quantLut.io.quant_lut_update_granularity := io.quant_lut_update_granularity
  quantLut.io.quant_fp6.valid := false.B
  quantLut.io.quant_fp6.bits := DontCare
  quantLut.io.lut_write_weight <> io.lut0_write
  quantLut.io.lut_write_act_in <> io.lut1_write
  quantLut.io.lut_write_act_out <> io.lut2_write

  when(quantize_valid && (total_bits_per_element === 6.U)) {
      quantLut.io.quant_fp6.valid := true.B 
      quantLut.io.quant_fp6.bits := quant_fp6
  }

  
  val scale_write_counter = RegInit(0.U(log2Ceil(scaleSize).W))
  val scale_buffer_full = RegInit(false.B)
  when(should_compute) {
    for (i <- 0 until scaleSize) {
      when(i.U === scale_write_counter) {
        scale_buffer(i) := scale_e8m0
      }
    }
    when((scale_write_counter === (scaleSize - 1).U)) {
      scale_write_counter := 0.U
      scale_buffer_full := true.B
    }.otherwise {
      scale_write_counter := scale_write_counter + 1.U
      when(io.scaleMem_write.fire){
        scale_buffer_full := false.B
      }
    }
  }.elsewhen(io.scaleMem_write.fire){
      scale_buffer_full := false.B
  }
  
  when(io.scaleMem_write.fire) {
      when(scale_write_addr_counter === ((1 << 16) - 1).U) {
        scale_write_addr_counter := 0.U
      }.otherwise {
        scale_write_addr_counter := scale_write_addr_counter + 1.U
      }
  }

  when(io.scale_mem_counter_reset_flag) {
    scale_write_addr_counter := 0.U
  }

  when(scale_buffer_full) {
    io.scaleMem_write.valid := true.B
    io.scaleMem_write.bits.addr := scale_mem_mvout_base_addr_act + (scale_write_addr_counter << (log2Ceil(scaleMem_data_width/8))) //byte address, scale 32B per write
    io.scaleMem_write.bits.data := Cat(scale_buffer.reverse)
  }
}
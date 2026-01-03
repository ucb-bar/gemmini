package gemmini

import chisel3._
import chisel3.util._

object MxFloatFormat {
  // Format encoding
  val FP4 = 0.U(2.W)
  val FP6 = 1.U(2.W)
  val FP8 = 2.U(2.W)
  
  def apply(bits: UInt): (UInt, UInt, UInt, UInt) = {
    val exp_bits = MuxLookup(bits, 4.U)(Seq(
      FP4 -> 2.U,
      FP6 -> 3.U,
      FP8 -> 4.U
    ))
    val mant_bits = MuxLookup(bits, 3.U)(Seq(
      FP4 -> 1.U,
      FP6 -> 2.U,
      FP8 -> 3.U
    ))
    
    val pmax = MuxLookup(bits, 448.U)(Seq(
      FP4 -> 6.U,     
      FP6 -> 28.U,   
      FP8 -> 448.U   
    ))

    val log2_pmax_floor = MuxLookup(bits, 8.U)(Seq(
      FP4 -> 2.U,
      FP6 -> 4.U,
      FP8 -> 8.U
    ))
    
    (exp_bits, mant_bits, pmax, log2_pmax_floor)
  }
}

class MxRequantizerIO(
  sp_data_width: Int,  
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleSize: Int,
  scaleMembasewrite: Int,
  quantWdataWidth: Int,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  config: GemminiRequantizerConfig 
) extends Bundle {
  val inputnumLanes = config.numInputLanes
  val outputnumLanes = config.numOutputLanes
  val inputdataWidth = config.inputBits
  val requant_data_in = Flipped(Decoupled(new RequantizerInBundle(inputnumLanes, inputdataWidth)))
  val scaleMem_write = Decoupled(new ScalingFactorWriteReq(scaleMem_addr_width, scaleMem_data_width)) 
  val requant_data_out = Decoupled(new RequantizerOutBundle(outputnumLanes))
  val lut_write = Flipped(Decoupled(new QuantLutWriteBundle(quantWdataWidth)))
  val spad_projected_data = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected)))
  val spad_deprojected_data = Decoupled(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val fp8_mode = Input(Bool())  // true for 64-lane mode, false for 16-lane mode
}
   
class MxRequantizer[T <: Data: Arithmetic](
  sp_data_width: Int, 
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleSize: Int,
  scaleMembasewrite: Int,
  quantWdataWidth: Int,
  quantRdataWidth: Int,
  quantRaddrWidth: Int,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  config: GemminiRequantizerConfig 
)(implicit ev: Arithmetic[T]) extends Module {
  
  import ev._
  
  val io = IO(new MxRequantizerIO(
    sp_data_width, 
    sp_addr_width, 
    scaleMem_data_width,
    scaleMem_addr_width,
    scaleSize, 
    scaleMembasewrite,
    quantWdataWidth,
    sp_bank_entries,
    sp_banks,
    sp_width,
    sp_width_projected,
    config
  ))
   
  val scales_per_write = scaleMem_data_width / 8
  val scale_write_addr_counter = RegInit(0.U(log2Ceil(scaleMem_addr_width).W))


  val scale_buffer = RegInit(VecInit(Seq.fill(scaleSize)(0.U(8.W))))
  val quant_dataType = io.requant_data_in.bits.dataType   //output data fromat
  val format_reg = RegNext(quant_dataType.asUInt, 2.U)
  

  io.scaleMem_write.valid := false.B
  io.scaleMem_write.bits := DontCare
  io.requant_data_out.valid := false.B
  io.requant_data_out.bits := DontCare
  

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

  val data_buffer = WireInit(VecInit(Seq.fill(io.outputnumLanes)(0.U(io.inputdataWidth.W))))
  val data_buffer_counter = RegInit(0.U(1.W))

  //buffer twice for 16-lane mode
  val half_lanes = 16
  val input_32_buffer = RegInit(VecInit(Seq.fill(io.outputnumLanes)(0.U(io.inputdataWidth.W))))
  
  val input_64_buffer = RegInit(VecInit(Seq.fill(io.inputnumLanes)(0.U(io.inputdataWidth.W))))
  val batch_counter = RegInit(0.U(1.W))
  val processing_64lane = RegInit(false.B)
  
  val requant_data_in_valid_d = RegNext(io.requant_data_in.valid, false.B)
  val should_compute = Wire(Bool())
  val quantize_valid = RegNext(should_compute, false.B)

  should_compute := false.B
  io.requant_data_in.ready := !processing_64lane
  
  when(io.requant_data_in.fire) {
    when(io.fp8_mode) { //16 lanes at a time
      for (i <- 0 until half_lanes) {
        val idx = Mux(data_buffer_counter === 0.U, i.U, (half_lanes + i).U)
        input_32_buffer(idx) := io.requant_data_in.bits.asUInt.asTypeOf(Vec(half_lanes, UInt(io.inputdataWidth.W)))(i) 
      }
      data_buffer_counter := data_buffer_counter ^ 1.U
    }.otherwise {
      for (i <- 0 until 64) {
        input_64_buffer(i) := io.requant_data_in.bits.data(i)
      }
      data_buffer_counter := 1.U
    }
  }
  
  when(io.fp8_mode) { //16 lanes at a time
      should_compute := data_buffer_counter === 0.U && requant_data_in_valid_d
      for (i <- 0 until io.outputnumLanes) {
        data_buffer(i) := input_32_buffer(i) 
      }
  }.otherwise {
      processing_64lane := true.B
      should_compute := processing_64lane
      for (i <- 0 until io.outputnumLanes) {
        val idx = Mux(batch_counter === 0.U, i.U, (io.outputnumLanes + i).U)
        data_buffer(i) := input_64_buffer(i) 
      }
      when(quantize_valid){
        batch_counter := 1.U
      }.otherwise {
        processing_64lane := false.B
        data_buffer_counter := 0.U
        batch_counter := 0.U 
    }
  }
  
  
  val block_max = Wire(UInt(io.inputdataWidth.W))
  block_max := 0.U 
  
  when(should_compute) {
    block_max := data_buffer.map(abs).reduce { (a, b) =>
      Mux(a > b, a, b)
    }
  }
  
  val block_max_uint = block_max.asUInt
  val scale_exponent = Wire(SInt(9.W))
  val scale_e8m0 = Wire(UInt(8.W))
  
  scale_exponent := 0.S
  scale_e8m0 := 0.U
  
  when(block_max_uint === 0.U) {
    scale_exponent := 0.S
    scale_e8m0 := 0.U
  }.otherwise {
    val log2_m = log2_floor(block_max_uint)
    scale_exponent := log2_m.zext - log2_pmax_floor.zext
    val biased_exp = scale_exponent + 127.S
    when(biased_exp < 0.S) {
      scale_e8m0 := 0.U
    }.elsewhen(biased_exp > 255.S) {
      scale_e8m0 := 255.U
    }.otherwise {
      scale_e8m0 := biased_exp.asUInt
    }
  }
  
  val quantized_buffer = Wire(Vec(io.outputnumLanes, UInt(8.W)))
  val BF16ScaleRoundToTiny = Module(new BF16ScaleRoundToTiny(outputnumLanes = io.outputnumLanes))
  
  BF16ScaleRoundToTiny.io.in_bf16 := data_buffer
  BF16ScaleRoundToTiny.io.scale_e8m0 := scale_e8m0
  BF16ScaleRoundToTiny.io.dataType := format_reg
  quantized_buffer := BF16ScaleRoundToTiny.io.out

  val total_bits_per_element = WireDefault(0.U(4.W))
  total_bits_per_element := 1.U  +&  exp_bits  +&  mant_bits 

  val quant_fp6 = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  quant_fp6 := Mux(total_bits_per_element === 6.U, VecInit((0 until io.outputnumLanes).map(i => quantized_buffer(i)(5, 0))), 
    VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  val extracted_data = WireDefault((0.U((io.outputnumLanes*8).W))) // 256bits / 128bits
  
  when(quantize_valid) {
    when(total_bits_per_element === 4.U){
      extracted_data := Cat((0 until io.outputnumLanes).map(i => quantized_buffer(i)(3, 0)).reverse)
    }.elsewhen(total_bits_per_element === 8.U){
      extracted_data := Cat(quantized_buffer.reverse)
    }.otherwise{
      extracted_data := 0.U((io.outputnumLanes*8).W)
    }
  }
  
  val projected_data = RegInit(VecInit(Seq.fill(io.outputnumLanes)(0.U(4.W))))

  val quantLut = Module(new QuantLut(
    wdataWidth = quantWdataWidth,
    raddrWidth = quantRaddrWidth,
    rdataWidth = quantRdataWidth,
    outputnumLanes = io.outputnumLanes ,
    sp_bank_entries = sp_bank_entries,
    sp_banks = sp_banks,
    sp_width = sp_width,
    sp_width_projected = sp_width_projected
  ))
  
  quantLut.io.spad_projected_data <> io.spad_projected_data
  quantLut.io.spad_deprojected_data <> io.spad_deprojected_data
  
  quantLut.io.lut_write.valid := false.B
  quantLut.io.lut_write.bits := DontCare
  quantLut.io.quant_fp6.valid := false.B
  quantLut.io.quant_fp6.bits := DontCare
  // quantLut.io.lut_read_req.valid := false.B
  // quantLut.io.lut_read_req.bits := DontCare
  // quantLut.io.lut_read_resp.ready := false.B
  
  when(quantLut.io.lut_write.valid) {
    quantLut.io.lut_write.ready := true.B
    quantLut.io.lut_write.bits.data := io.lut_write.bits.data
  }.otherwise {
    quantLut.io.lut_write.ready := false.B
  }

  when(quantize_valid && (total_bits_per_element === 6.U)) {
    quantLut.io.quant_fp6.valid := true.B
    quantLut.io.quant_fp6.bits := quant_fp6
  }
  
  when(quantLut.io.projected_data.valid && (total_bits_per_element === 6.U)) {
    io.requant_data_out.valid := true.B
    io.requant_data_out.bits.dataType := quant_dataType
    io.requant_data_out.bits.address := io.requant_data_in.bits.address  +& config.baseAddr.U //todo: the address generated for only 256bits write
    io.requant_data_out.bits.data := Cat(quantLut.io.projected_data.bits.reverse)
  }.elsewhen(quantize_valid && ((total_bits_per_element === 4.U) || (total_bits_per_element === 8.U))){
    io.requant_data_out.valid := true.B
    io.requant_data_out.bits.dataType := quant_dataType
    io.requant_data_out.bits.address := io.requant_data_in.bits.address +& config.baseAddr.U
    io.requant_data_out.bits.data := extracted_data
  }.otherwise {
    io.requant_data_out.bits.data := 0.U
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
      scale_buffer_full := false.B
    }
  }
  
  when(scale_buffer_full) {
    io.scaleMem_write.valid := true.B
    io.scaleMem_write.bits.addr := (scaleMembasewrite.U +& scale_write_addr_counter) << 1.U
    io.scaleMem_write.bits.data := Cat(scale_buffer.reverse)
    
    when(io.scaleMem_write.fire) {
      val scale_buffer_packed = Cat(scale_buffer.reverse)
      printf(p"[MxScaleGen]: addr=${scale_write_addr_counter}, data=0x${Hexadecimal(scale_buffer_packed)}\n")
      
      when(scale_write_addr_counter === ((1 << scaleMem_addr_width) - 1).U) {
        scale_write_addr_counter := 0.U
      }.otherwise {
        scale_write_addr_counter := scale_write_addr_counter + 1.U
      }
    }
  }
}
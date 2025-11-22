package gemmini

import chisel3._
import chisel3.util._
import Util._
import scala.util.matching.Regex


object MxFloatFormat {
  // Format encoding
  val FP4 = 0.U(2.W)
  val FP6 = 1.U(2.W)
  val FP8 = 2.U(2.W)
  
  def apply(bits: UInt): (UInt, UInt, UInt, UInt) = {
    val exp_bits = MuxLookup(bits, 4.U)(Seq(
      FP4 -> 2.U,  // E2M1
      FP6 -> 3.U,  // E3M2
      FP8 -> 4.U   // E4M3
    ))
    val mant_bits = MuxLookup(bits, 3.U)(Seq(
      FP4 -> 1.U,  // E2M1
      FP6 -> 2.U,  // E3M2
      FP8 -> 3.U   // E4M3
    ))
    
    val pmax = MuxLookup(bits, 448.U)(Seq(
      FP4 -> 6.U,     
      FP6 -> 28.U,   
      FP8 -> 448.U   
    ))

    val log2_pmax_floor = MuxLookup(bits, 8.U)(Seq(
      FP4 -> 2.U,   // floor(log2(6))
      FP6 -> 4.U,   // floor(log2(28))
      FP8 -> 8.U    // floor(log2(448))
    ))
    
    (exp_bits, mant_bits, pmax, log2_pmax_floor)
  }
}

object RequantizerDataType extends ChiselEnum {
  val FP4, FP6, FP8 = Value
}

class RequantizerInBundle(numLanes: Int, dataWidth: Int = 16) extends Bundle {
  val data = Vec(numLanes, UInt(dataWidth.W))
  val address = UInt(32.W) // in bytes
  val dataType = RequantizerDataType()
}

class RequantizerOutBundle(numLanes: Int) extends Bundle {
  val data = UInt((numLanes * 8).W) // maximum data type is fp8 (1 byte/lane), valid from lsb
  val address = UInt(32.W)
  val dataType = RequantizerDataType() // data type determines response size
}

case class GemminiRequantizerConfig(
  baseAddr: BigInt,
  numInputLanes: Int = 16,
  numOutputLanes: Int = 32,
  gpuMaxFactor: Int = 2, // maximum fp16->fp8 for gpus, determines address space size
  gpuWordSize: Int = 4,
  inputBits: Int = 16,
  minOutputBits: Int = 4,
  maxOutputBits: Int = 8,
  outputIdBits: Int = 3,
)


class MxRequantizerIO(
  sp_data_width: Int,  
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleSize: Int, //scale size in byte
  scaleMembasewrite: Int,
  config: GemminiRequantizerConfig 
) extends Bundle {
  val inputnumLanes = config.numInputLanes
  val outputnumLanes = config.numOutputLanes
  val inputdataWidth = config.inputBits
  val requnat_data_in = Flipped(Decoupled(new RequantizerInBundle(inputnumLanes, inputdataWidth)))
  val scaleMem_write = Decoupled(new ScalingFactorWriteReq(scaleMem_addr_width, scaleMem_data_width)) 
  val requant_data_out = Decoupled(new RequantizerOutBundle(outputnumLanes))
}
   
class MxRequantizer[T <: Data: Arithmetic](
  sp_data_width: Int, 
  sp_addr_width: Int,
  scaleMem_data_width: Int,
  scaleMem_addr_width: Int,
  scaleSize: Int,
  scaleMembasewrite: Int,
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
    config
  ))
   
  io.requnat_data_in.ready := true.B 
  val scales_per_write = scaleMem_data_width / 8
  val scale_write_addr_counter = RegInit(0.U(log2Ceil(scaleMem_addr_width).W))

  val scale_buffer = RegInit(VecInit(Seq.fill(scaleSize)(0.U(8.W))))
  val quant_dataType = io.requnat_data_in.bits.dataType  
 
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

  val data_buffer =  RegInit(VecInit(Seq.fill(io.outputnumLanes)(0.U(io.inputdataWidth.W))))
  val data_buffer_counter = RegInit(0.U(1.W))
  val half_lanes = io.inputnumLanes 
  val requant_data_in_valid_d = RegNext(io.requnat_data_in.valid, false.B) 
  
  when(io.requnat_data_in.valid) {
    for (i <- 0 until half_lanes) {
      val idx = Mux(data_buffer_counter === 0.U, i.U, (half_lanes + i).U)
      data_buffer(idx) := io.requnat_data_in.bits.data(i) 
    }
    data_buffer_counter := data_buffer_counter ^ 1.U
  }


  val block_max = Wire(UInt(io.inputdataWidth.W))
  block_max := 0.U 
  
  when(data_buffer_counter === 0.U && requant_data_in_valid_d) {
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
  
  val quantized_buffer = Wire(Vec(io.outputnumLanes, UInt(8.W))) //single data output width can be 4,6,8 bits
 
  val BF16ScaleRoundToFP6 = Module(new BF16ScaleRoundToFP6(outputnumLanes = io.outputnumLanes))
  
  BF16ScaleRoundToFP6.io.in_bf16 := data_buffer
  BF16ScaleRoundToFP6.io.scale_e8m0 := scale_e8m0
  quantized_buffer := BF16ScaleRoundToFP6.io.out_fp6


  val quantize_valid = RegNext(data_buffer_counter === 0.U && requant_data_in_valid_d, false.B)
  //val quantize_valid_d = RegNext(quantize_valid, false.B)

  val total_bits_per_element = WireDefault(0.U(4.W))
  total_bits_per_element := 1.U  +&  exp_bits  +&  mant_bits 

  val quant_fp6 = WireDefault(VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  quant_fp6 := Mux(total_bits_per_element === 6.U, VecInit((0 until io.outputnumLanes).map(i => quantized_buffer(i)(5, 0))), 
    VecInit(Seq.fill(io.outputnumLanes)(0.U(6.W))))
  val extracted_data = WireDefault((0.U((io.outputnumLanes*8).W)))
  
  when(quantize_valid) {
    when(total_bits_per_element === 4.U){
      extracted_data := Cat((0 until io.outputnumLanes).map(i => quantized_buffer(i)(3, 0)).reverse)
    }.elsewhen(total_bits_per_element === 8.U){
      extracted_data := Cat(quantized_buffer.reverse)
    }.otherwise{
      extracted_data := 0.U((io.outputnumLanes*8).W)
    }
    //printf(p"[MxQuantize] extracted_data=0x${Hexadecimal(extracted_data)}\n")
  }
  
  //do projecttion for fp6 to Int4
  val projected_data = RegInit(VecInit(Seq.fill(io.outputnumLanes)(0.U(4.W))))

  val quantLut = Module(new QuantLut(
  wdataWidth = 96,
  raddrWidth = 4,
  rdataWidth = 6,
  outputnumLanes = io.outputnumLanes
))

quantLut.io.lut_write.valid := false.B
quantLut.io.lut_write.bits := DontCare
quantLut.io.quant_fp6.valid := false.B
quantLut.io.quant_fp6.bits := DontCare
quantLut.io.lut_read_req.valid := false.B
quantLut.io.lut_read_req.bits := DontCare
quantLut.io.lut_read_resp.ready := false.B

when(quantize_valid && (total_bits_per_element === 6.U)) {
      quantLut.io.quant_fp6.valid := true.B
      quantLut.io.quant_fp6.bits := quant_fp6
}
  
when(quantLut.io.projected_data.valid && (total_bits_per_element === 6.U)) {
    io.requant_data_out.valid := true.B
    io.requant_data_out.bits.dataType := quant_dataType
    io.requant_data_out.bits.address := io.requnat_data_in.bits.address  +& config.baseAddr.U
    io.requant_data_out.bits.data := Cat(quantLut.io.projected_data.bits.reverse)
    //printf(p"[MxQuantize] Quantized block with scale=0x${Hexadecimal(scale_e8m0)}, exp=${scale_exponent}\n")
  }.elsewhen(quantize_valid && ((total_bits_per_element === 4.U) || (total_bits_per_element === 8.U))){
    io.requant_data_out.valid := true.B
    io.requant_data_out.bits.dataType := quant_dataType
    io.requant_data_out.bits.address := io.requnat_data_in.bits.address +& config.baseAddr.U
    io.requant_data_out.bits.data := extracted_data
    //printf(p"[MxQuantize] Quantized block with scale=0x${Hexadecimal(scale_e8m0)}, exp=${scale_exponent}\n")
    //printf(p"[MxQuantize] io.requant_data_out.bits.data=0x${Hexadecimal(io.requant_data_out.bits.data)}\n")
  }.otherwise {
     io.requant_data_out.bits.data := 0.U
  }

  val scale_write_counter = RegInit(0.U(log2Ceil(scaleSize).W))
  val scale_buffer_full = RegInit(false.B)

  when(data_buffer_counter === 0.U && requant_data_in_valid_d) {
    for (i <- 0 until scaleSize) {
      when(i.U === scale_write_counter) {
       scale_buffer(i) := scale_e8m0
      }
    }
    when((scale_write_counter === (scaleSize - 1).U) ) {
      scale_write_counter := 0.U
      scale_buffer_full := true.B
    }.otherwise {
      scale_write_counter := scale_write_counter + 1.U
      scale_buffer_full := false.B
    }
  }
  
  when(scale_buffer_full) {
    io.scaleMem_write.valid := true.B
    io.scaleMem_write.bits.addr := (scaleMembasewrite.U +& scale_write_addr_counter) << 1.U //todo, isa determines the scaleMembasewrite for initial addr of scaling memory write rows 
    io.scaleMem_write.bits.data := Cat(scale_buffer.reverse)
    
    when(io.scaleMem_write.fire) {
      val scale_buffer_packed = Cat(scale_buffer.reverse)
      printf(p"[MxScaleGen]: addr=${scale_write_addr_counter}, data=0x${Hexadecimal(scale_buffer_packed)}\n")
      
      when(scale_write_addr_counter === ((1 << scaleMem_addr_width) - 1).U) { //TODO: do we need to make scale_write_addr_counter be resetable?
        scale_write_addr_counter := 0.U
      }.otherwise {
        scale_write_addr_counter := scale_write_addr_counter + 1.U
      }
    }
  }// }.otherwise{
  //   io.scaleMem_write.valid := false.B
  // }
}
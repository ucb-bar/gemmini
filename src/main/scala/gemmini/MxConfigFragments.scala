package gemmini

import chisel3._
import chisel3.util._

case class GemminiScalingFactorMemConfig(
  baseAddr: BigInt = 0x10000000L,
  sizeInBytes: BigInt = 16 << 10,
  subbankLineSizeInBytes: Int = 16,
  subbanksPerBank: Int = 2,
  gpuInputWidthBytes: Int = 8,
  numBanks: Int = 8,
  ScaleMemWriteDataWidth: Int = 256,
  ScaleMemWriteAddrWidth: Int = 33,
) {
  def depth: Int = (sizeInBytes / (subbankLineSizeInBytes) / numBanks).toInt
  def bankWidthBytes = subbankLineSizeInBytes * subbanksPerBank
  def bankWidthBits = bankWidthBytes * 8
  def addrBits = log2Ceil(sizeInBytes)
  def lineOffsetBits = log2Ceil(bankWidthBytes) 
}

case class GemminiRequantizerConfig(
  baseAddr: BigInt = 0x10000000L + 0x8000,
  numGPUInputLanes: Int = 16,
  numInputLanes: Int = 64, // 16 for fp8, 64 for fp6/fp4
  numOutputLanes: Int = 32,
  gpuMaxFactor: Int = 2, // maximum fp16->fp8 for gpus, determines address space size
  gpuWordSize: Int = 4,
  inputBits: Int = 16,
  minOutputBits: Int = 4,
  maxOutputBits: Int = 8,
  outputIdBits: Int = 3,
  lutUpdateRegularityW : Int = 128,  // how many elements the LUT updates at once
  lutUpdateRegularityActIn : Int = 128,
  lutUpdateRegularityActOut : Int = 128,
  pipelineLatency: Int = 2,
)

// Source floating-point format that the LUT projects into 4-bit indices.
// Fixed at elaboration; defaults to FP6 so existing configs are unchanged.
sealed trait LutProjFormat
case object LutFP6E3M2 extends LutProjFormat
case object LutFP6E2M3 extends LutProjFormat
case object LutFP8E4M3 extends LutProjFormat
case object LutFP8E5M2 extends LutProjFormat

case class GemminiLUTConfig(
  numBits: Seq[Int] = Seq(96, 96, 96),
  numEntries: Seq[Int] = Seq(64, 64, 64),
  rdataWidth: Int = 6,
  raddrWidth: Int = 4,
  lutUpdateRegularityWidth: Int = 16,
  projFormat: LutProjFormat = LutFP6E3M2,
) {
  def isFp8Proj = projFormat == LutFP8E4M3 || projFormat == LutFP8E5M2

  require(!isFp8Proj || rdataWidth == 8,
    "FP8 LUT projection requires rdataWidth == 8")

  // Each LUT holds 16 entries (2^raddrWidth) of rdataWidth bits, so every write
  // word must be exactly 16 * rdataWidth wide (96 for FP6, 128 for FP8).
  require(numBits.forall(_ == 16 * rdataWidth),
    s"GemminiLUTConfig.numBits must each equal 16 * rdataWidth (= ${16 * rdataWidth}); got $numBits")

  def apply(table: Int) = {
    (numEntries(table), numBits(table))
  }

  def numTables = numBits.length
}

object RequantizerDataType extends ChiselEnum {
  val FP8, FP6, FP4 = Value

  def widthBits(x: Type): UInt = {
    Mux(x === FP4, 4.U(4.W), 8.U(4.W))
  }

  def toUInt(x: Type): UInt = {
    Mux(x === FP8, 0.U(2.W), Mux(x === FP6, 1.U(2.W), 2.U(2.W)))
  }
}


class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  def this(config: GemminiScalingFactorMemConfig) = {
    // writes two interleaved banks at once
    this(config.addrBits, config.ScaleMemWriteDataWidth)
  }
}


class ScalingFactorCntl(max_block: Int) extends Bundle {
  val counter_a = UInt(log2Up(max_block).W)
  val counter_b = UInt(log2Up(max_block).W)
  val fire_a = Bool()
  val fire_b = Bool()
  val baseAddress_act = UInt(32.W)
  val baseAddress_w = UInt(32.W)
  val scale_mem_read_w_sel = UInt(1.W)
  val scale_mem_read_act_sel = UInt(1.W)
  val loop_bound_i = UInt(9.W)
  val loop_bound_j = UInt(9.W)
  val loop_bound_k = UInt(9.W)
  val scale_mem_counter_reset_flag = Bool()
}

class RequantizerInBundle(numLanes: Int, dataWidth: Int = 16) extends Bundle {
  val data = Vec(numLanes, UInt(dataWidth.W))
  val address = UInt(32.W) // in bytes
  val dataType = RequantizerDataType()
}

class RequantizerOutBundle(numLanes: Int, dataWidth: Int = 8) extends Bundle {
  val data = UInt((numLanes * dataWidth).W) // no active byte lanes (valid from lsb)
  val address = UInt(32.W)
  val dataType = RequantizerDataType() // data type determines response size
}

class QuantLutWriteBundle(numEntries: Int, numBits: Int) extends Bundle {
  val data = Vec(numEntries, UInt(numBits.W))
  def this(config: (Int, Int)) = {
    this(config._1, config._2)
  }
}

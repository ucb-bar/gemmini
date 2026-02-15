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
  numInputLanes: Int = 64, // TODO: note 16 only for fp8, 64 for fp6/fp4
  numOutputLanes: Int = 32,
  gpuMaxFactor: Int = 2, // maximum fp16->fp8 for gpus, determines address space size
  gpuWordSize: Int = 4,
  inputBits: Int = 16,
  minOutputBits: Int = 4,
  maxOutputBits: Int = 8,
  outputIdBits: Int = 3,
  lutUpdateRegularityW : Int = 128,  // means how many elements updatScalingFactorCntle once the lut
  lutUpdateRegularityActIn : Int = 128,
  lutUpdateRegularityActOut : Int = 128,
  pipelineLatency: Int = 0,
)

case class GemminiLUTConfig(
  numBits: Seq[Int] = Seq(96, 96, 96),
  numEntries: Seq[Int] = Seq(16, 16, 32),
  rdataWidth: Int = 6,
  raddrWidth: Int = 4, 
  lutUpdateRegularityWidth: Int = 16,
) {
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

package gemmini

import chisel3._
import chisel3.util._

case class GemminiScalingFactorMemConfig(
  baseAddr: BigInt,
  sizeInBytes: BigInt = 32 << 10,
  sramLineSizeInBytes: Int = 32,
  numBanks: Int = 8,
) {
  def depth: Int = (sizeInBytes / sramLineSizeInBytes / numBanks).toInt
  def bankWidthBits = sramLineSizeInBytes * 8
  def addrBits = log2Ceil(sizeInBytes)
  def lineOffsetBits = log2Ceil(sramLineSizeInBytes)
}

case class GemminiRequantizerConfig(
  baseAddr: BigInt,
  numGPUInputLanes: Int = 16,
  numInputLanes: Int = 64, // TODO: note 16 only for fp8, 64 for fp6/fp4
  numOutputLanes: Int = 32,
  gpuMaxFactor: Int = 2, // maximum fp16->fp8 for gpus, determines address space size
  gpuWordSize: Int = 4,
  inputBits: Int = 16,
  minOutputBits: Int = 4,
  maxOutputBits: Int = 8,
  outputIdBits: Int = 3,
)

case class GemminiLUTConfig(
  numBits: Int = 96,
  numEntries: Int = 32,
  numTables: Int = 3,
  rdataWidth: Int = 6,
  raddrWidth: Int = 4, 
)

object RequantizerDataType extends ChiselEnum {
  val FP4, FP6, FP8 = Value

  def widthBits(x: Type): UInt = {
    Mux(x === FP4, 4.U(4.W), 8.U(4.W))
  }
}

class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  def this(config: GemminiScalingFactorMemConfig) = {
    // writes two interleaved banks at once
    this(config.addrBits, config.bankWidthBits * 2)
  }
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
  def this(config: GemminiLUTConfig) = {
    this(config.numEntries, config.numBits)
  }
}

class QuantLutReadReq(raddrWidth: Int) extends Bundle {
  val lutaddr = UInt(raddrWidth.W)
}

class QuantLutReadResp(rdataWidth: Int) extends Bundle {
  val lutdata = UInt(rdataWidth.W)
}

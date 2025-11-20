package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config.Parameters

import scala.math.{pow}

class QuantLutIO(
  wdataWidth: Int,
  raddrWidth: Int,
  rdataWidth: Int,
  outputnumLanes: Int = 32
) extends Bundle {
  val lutReadEnable = Output(Bool()) 
  val lut_write =  Flipped(Decoupled(new QuantLutWriteBundle(wdataWidth))) //input
  val lut_read_req = Flipped(Decoupled(new QuantLutReadReq(raddrWidth))) //input
  val lut_read_resp = Decoupled(new QuantLutReadResp(rdataWidth)) //output
  val quant_fp6 = Flipped(Valid(Vec(outputnumLanes, UInt(rdataWidth.W))))
  val projected_data = Valid(Vec(outputnumLanes, UInt(raddrWidth.W)))
}

class QuantLut(
  wdataWidth: Int,
  raddrWidth: Int, 
  rdataWidth: Int,
  outputnumLanes: Int = 32
) extends Module {
  val QuantLutEnable = Input(Bool()) 
  val io = IO(new QuantLutIO(wdataWidth, raddrWidth, rdataWidth, outputnumLanes))
  val lutSize = pow(2, raddrWidth).toInt
  val lutCache = RegInit(VecInit(Seq.fill(lutSize)(0.U(rdataWidth.W))))

  io.lutReadEnable := false.B
  io.lut_write.ready := false.B
  io.lut_read_req.ready := false.B
  io.lut_read_resp.valid := false.B
  io.lut_read_resp.bits := DontCare

  when(io.lut_write.valid) {
    io.lut_write.ready := true.B
    io.lutReadEnable := false.B
    when(io.lut_write.fire) {
    for (j <- 0 until lutSize) {
      lutCache(j) := io.lut_write.bits.data((j+1)*rdataWidth-1, j*rdataWidth)
      printf(p"[QuantLut] Wrote LUT index $j with value=0x${Hexadecimal(io.lut_write.bits.data((j+1)*rdataWidth-1, j*rdataWidth))}\n")
    }
  }
  }.otherwise {
    io.lut_write.ready := false.B
    io.lutReadEnable := true.B
  }
 
  val projectedIndices = RegInit(VecInit(Seq.fill(outputnumLanes)(0.U(raddrWidth.W))))
  val projectedDataValid = RegInit(false.B)

  //TODO: double check if this is the correct way to do nearest neighbor search, aligning with the algorithm implementation
  when(io.lutReadEnable && io.quant_fp6.valid) {
  for (i <- 0 until outputnumLanes) {
    val inputFp6 = io.quant_fp6.bits(i)
    val distances = VecInit((0 until lutSize).map { j =>
    val diff = Mux(inputFp6 > lutCache(j), 
                    inputFp6 - lutCache(j), 
                    lutCache(j) - inputFp6)
      diff
   }
  )
    
  val minIdx = (0 until lutSize).map(_.U(raddrWidth.W)).reduce { (idxA, idxB) =>
  Mux(distances(idxA) <= distances(idxB), idxA, idxB)}
  printf(p"[QuantLut] Input FP6: 0x${Hexadecimal(inputFp6)} => Nearest LUT index: $minIdx with value=0x${Hexadecimal(lutCache(minIdx))}\n")
  projectedIndices(i) := minIdx
  projectedDataValid := true.B
  }
  } .otherwise {
    // for (i <- 0 until outputnumLanes) {
    //   projectedIndices(i) := 0.U
    // }
    projectedDataValid := false.B
  }

  //TODO: read the data from shared memory by 4 bits index and use the data to do projection
  io.projected_data.valid := projectedDataValid
  io.projected_data.bits := projectedIndices
  
  // io.lut_read_req.valid := false.B  
  // io.lut_read_req.bits := DontCare
  // io.lut_read_resp.ready := false.B
}
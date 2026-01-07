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
  outputnumLanes: Int = 32 ,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
) extends Bundle {
  val lutReadEnable = Output(Bool()) 
  val lut_write =  Flipped(Decoupled(new QuantLutWriteBundle(wdataWidth))) //input
  val quant_fp6 = Flipped(Valid(Vec(outputnumLanes, UInt(rdataWidth.W)))) //input
  val projected_data = Valid(Vec(outputnumLanes, UInt(raddrWidth.W))) //output
  // val spad_projected_data = Flipped(Decoupled(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected)))) 
  // val spad_deprojected_data = Decoupled(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val spad_projected_data   = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected))
  val spad_deprojected_data = Vec(sp_banks, Flipped(new ScratchpadReadIO(sp_bank_entries, sp_width)))
}

class QuantLut(
  wdataWidth: Int,
  raddrWidth: Int, 
  rdataWidth: Int,
  outputnumLanes: Int = 32 ,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
) extends Module {
  val QuantLutEnable = Input(Bool()) 
  val io = IO(new QuantLutIO(wdataWidth, raddrWidth, rdataWidth, outputnumLanes, sp_bank_entries, sp_banks, sp_width, sp_width_projected))
  val lutSize = pow(2, raddrWidth).toInt
  val lutCache = RegInit(VecInit(Seq.fill(lutSize)(0.U(rdataWidth.W))))

  io.lutReadEnable := false.B
  io.lut_write.ready := true.B

  when(io.lut_write.valid) {
    when(io.lut_write.fire) {
      for (j <- 0 until lutSize) {
        lutCache(j) := io.lut_write.bits.data((j+1)*rdataWidth-1, j*rdataWidth)
        printf(p"[QuantLut] Wrote LUT index $j with value=0x${Hexadecimal(io.lut_write.bits.data((j+1)*rdataWidth-1, j*rdataWidth))}\n")
      }
    }
  }
  io.lutReadEnable := !io.lut_write.valid

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
    projectedDataValid := false.B
  }

  io.projected_data.valid := projectedDataValid
  io.projected_data.bits := projectedIndices

  for (i <- 0 until sp_banks) {
    io.spad_deprojected_data(i).req <> io.spad_projected_data(i).req

    io.spad_deprojected_data(i).resp.valid := false.B
    io.spad_deprojected_data(i).resp.bits  := 0.U.asTypeOf(new ScratchpadReadResp(sp_width))
    io.spad_projected_data(i).resp.ready   := io.spad_deprojected_data(i).resp.ready

    when(io.spad_projected_data(i).resp.valid) {
      val num_4bit_chunks = sp_width_projected / 4
      val deprojected_bits = Wire(Vec(num_4bit_chunks, UInt(6.W)))
      
      for (k <- 0 until num_4bit_chunks) {
        val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
        deprojected_bits(k) := lutCache(chunk_4bit)
      }
      
      io.spad_deprojected_data(i).resp.bits.data := deprojected_bits.asUInt
      io.spad_deprojected_data(i).resp.valid := true.B
      
      io.spad_deprojected_data(i).resp.bits.fromDMA := io.spad_projected_data(i).resp.bits.fromDMA
      io.spad_deprojected_data(i).resp.bits.weight_mx_format := io.spad_projected_data(i).resp.bits.weight_mx_format
      io.spad_deprojected_data(i).resp.bits.input_mx_format := io.spad_projected_data(i).resp.bits.input_mx_format
    }
  }
}
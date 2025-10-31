package gemmini

import chisel3._
import chisel3.util._


// 2x2 Multiplier Unit with hardwired LUT and optimization control
class Multiplier2x2(lut: Boolean) extends Module {
  val io = IO(new Bundle {
    val weight = Input(UInt(2.W))      // 2-bit weight
    val activation = Input(UInt(2.W))  // 2-bit activation
    val result = Output(UInt(4.W))     // 4-bit multiplication result
  })
 
  if (lut) {
    // Lookup table for 4x4 matrix as shown in diagram
    // Values from the truth table: rows indexed by activation, columns by weight
    val lut = VecInit(Seq(
      VecInit(Seq("b0000".U(4.W), "b0000".U(4.W), "b0000".U(4.W), "b0000".U(4.W))),  // Row 00
      VecInit(Seq("b0000".U(4.W), "b0001".U(4.W), "b0010".U(4.W), "b0011".U(4.W))),  // Row 01    
      VecInit(Seq("b0000".U(4.W), "b0010".U(4.W), "b0100".U(4.W), "b0110".U(4.W))),  // Row 10
      VecInit(Seq("b0000".U(4.W), "b0011".U(4.W), "b0110".U(4.W), "b1001".U(4.W)))   // Row 11
    ))
    
    // Output both the specific result and the column
    io.result := lut(io.activation)(io.weight)
  } else {
    io.result := io.weight * io.activation  
  }
}


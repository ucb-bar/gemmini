package gemmini 

import chisel3._
import chisel3.util._


// MACU - Multi-Mode Accumulation Compute Unit
// Lowest level compute unit with single 2x2 multiplier
class MACU(lut: Boolean) extends Module {
  val io = IO(new Bundle {
    val w = Input(UInt(3.W))           // 2 or 3-bit weight input
    val act = Input(UInt(3.W))         // 2 or 3-bit activation input
    val enable = Input(Bool())         // Enable signal for compute unit
    val w_mode = Input(UInt(1.W))      // Weight mode: 0=M1, 1=M2
    val act_mode = Input(UInt(1.W))    // Activation mode: 0=M1, 1=M2
    val output = Output(UInt(6.W))
  })

  // Instantiate single 2x2 multiplier
  val mult = Module(new Multiplier2x2(lut))
  
  // Connect weight and activation to multiplier
  mult.io.weight := io.w(1,0)
  mult.io.activation := io.act(1,0)

  // Mode definitions
  val w_m1_mode = io.w_mode === 0.U
  val w_m2_mode = io.w_mode === 1.U
  val act_m1_mode = io.act_mode === 0.U
  val act_m2_mode = io.act_mode === 1.U
  
  // Store multiplier result when enabled
  val mult_result = Mux(io.enable, mult.io.result, 0.U(4.W))

  // Activation M2 mode processing
  // When activation is in M2 mode, we need additional processing for higher precision
  val act_m2_path = Wire(UInt(5.W))
  when(act_m2_mode) {
      when(io.act(2) === 1.U) {
          act_m2_path := io.w(1,0) << 2
      }.otherwise {
         act_m2_path := 0.U
      }      
  }.otherwise {
    // not enable this path in M1 mode
    act_m2_path := 0.U
  }

  // Weight M2 mode processing
  // When weight is in M2 mode, we need additional weight-based computations
  val w_m2_path = Wire(UInt(5.W))
  when(w_m2_mode) {
    when(io.w(2) === 1.U) {
          w_m2_path := io.act << 2
      }.otherwise {
          w_m2_path := 0.U
      }      
  }.otherwise {
    // In weight M1 mode, minimal processing
    w_m2_path := 0.U
  }

  // Combined processing based on both modes
  val combined_result = Wire(UInt(6.W))

  // If both modes are enabled, we need to ensure the output is valid
  when(io.enable) {
    combined_result := act_m2_path +& w_m2_path +& mult_result  
  }.otherwise {
    combined_result := 0.U
  }

  // Output assignment with enable control
  io.output := combined_result
}


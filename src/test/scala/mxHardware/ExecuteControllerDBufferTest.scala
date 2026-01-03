package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters

class ExecuteControllerDBufferTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ExecuteController FP8 Buffer Logic for Operand D"
  
  //implicit val p: Parameters = GemminiConfigs.defaultConfig
  implicit val p: Parameters = (new DefaultGemminiConfig).toInstance
  it should "buffer FP8 weight data (operand D) and split into two cycles" in {
    test(new ExecuteControllerDBufferWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("=" * 60)
      println("Test: FP8 Weight Buffering for Operand D")
      println("=" * 60)
      

      dut.clock.step(5)
      
      // ===== Configure FP8 weight mode =====
      dut.io.config_weight_mx_format.poke(2.U)  // FP8
      dut.io.config_enable.poke(true.B)
      dut.clock.step(1)
      dut.io.config_enable.poke(false.B)
      println("✓ Configured weight format to FP8")
      
      // ===== Prepare test data =====
      // 256b SRAM data = [high 128b][low 128b]
      val weight_row0_128b = BigInt("0011223344556677" + "8899AABBCCDDEEFF", 16)  // low 128b - row 0
      val weight_row1_128b = BigInt("FFEEDDCCBBAA9988" + "7766554433221100", 16)  // high 128b - row 1
      val sram_full_256b = (weight_row1_128b << 128) | weight_row0_128b
      
      println(f"SRAM data (256b) = 0x${sram_full_256b.toString(16)}")
      println(f"  Low  128b (row0) = 0x${weight_row0_128b.toString(16)}")
      println(f"  High 128b (row1) = 0x${weight_row1_128b.toString(16)}")
      
      // ===== Start PRELOAD operation =====
      dut.io.start_preload.poke(true.B)
      dut.io.d_address.poke(0x200.U)  // D address
      dut.io.d_rows.poke(2.U)         // Read 2 rows
      dut.io.d_cols.poke(16.U)
      dut.clock.step(1)
      dut.io.start_preload.poke(false.B)
      
      println("\n--- Cycle 1: Issue SRAM Read Request ---")
      
      // ===== Cycle 1: Check SRAM read request =====
      dut.io.sram_read_valid.expect(true.B, "FAIL: Should issue SRAM read request")
      dut.io.sram_read_bank.expect(0.U)  // Assume bank 0
      dut.io.sram_read_addr.expect(0x200.U, "FAIL: SRAM address mismatch")
      
      // Buffer should be empty
      dut.io.debug_d_buffer_valid.expect(false.B, "FAIL: Buffer should be invalid initially")
      dut.io.debug_d_buffer_half.expect(false.B)
      
      println("✓ SRAM read request issued correctly")
      println(s"  - Address: 0x${dut.io.sram_read_addr.peek().litValue.toString(16)}")
      println(s"  - Buffer valid: ${dut.io.debug_d_buffer_valid.peek().litToBoolean}")
      
      // ===== Cycle 2: SRAM response ready (simulate pipeline delay) =====
      dut.io.sram_read_ready.poke(true.B)
      dut.clock.step(1)
      
      println("\n--- Cycle 2: SRAM Pipeline Delay ---")
      
      // ===== Cycle 3: SRAM data returned =====
      dut.io.sram_resp_valid.poke(true.B)
      dut.io.sram_resp_data.poke(sram_full_256b.U)
      dut.io.mesh_d_ready.poke(true.B)  // Mesh ready to receive
      dut.clock.step(1)
      
      println("\n--- Cycle 3: SRAM Data Returned (256b) ---")
      
      // Check: should save to buffer and use low 128b
      dut.io.debug_d_buffer_valid.expect(true.B, "FAIL: Buffer should be valid after SRAM read")
      dut.io.debug_d_buffer_half.expect(false.B, "FAIL: Should be using LOW half (row 0)")
      
      // Check if output data is low 128b
      val actual_data_cycle3 = dut.io.d_data_to_mesh.peek().litValue
      if (actual_data_cycle3 != weight_row0_128b) {
        println(f"FAIL: Expected 0x${weight_row0_128b.toString(16)}")
        println(f"      Got      0x${actual_data_cycle3.toString(16)}")
        assert(false)
      }
      
      // Check if data is sent to mesh
      dut.io.mesh_d_valid.expect(true.B, "FAIL: Should send data to mesh")
      
      println("✓ First half (low 128b) sent to mesh")
      println(f"  - Data: 0x${actual_data_cycle3.toString(16)}")
      println(f"  - Buffer valid: ${dut.io.debug_d_buffer_valid.peek().litToBoolean}")
      println(f"  - Using half: ${if (dut.io.debug_d_buffer_half.peek().litToBoolean) "HIGH" else "LOW"}")
      
      dut.io.sram_resp_valid.poke(false.B)
      
      // ===== Cycle 4: Use buffered high 128b =====
      dut.clock.step(1)
      
      println("\n--- Cycle 4: Use Buffered High 128b ---")
      
      // Check: should NOT issue new SRAM read
      dut.io.sram_read_valid.expect(false.B, "FAIL: Should NOT issue new SRAM read (using buffer)")
      
      // Check: should switch to high 128b
      dut.io.debug_d_buffer_valid.expect(true.B, "FAIL: Buffer should still be valid")
      dut.io.debug_d_buffer_half.expect(true.B, "FAIL: Should be using HIGH half (row 1)")
      
      // Check if output data is high 128b
      val actual_data_cycle4 = dut.io.d_data_to_mesh.peek().litValue
      if (actual_data_cycle4 != weight_row1_128b) {
        println(f"FAIL: Expected 0x${weight_row1_128b.toString(16)}")
        println(f"      Got      0x${actual_data_cycle4.toString(16)}")
        assert(false)
      }
      
      // Check if data is sent to mesh
      dut.io.mesh_d_valid.expect(true.B, "FAIL: Should send data to mesh")
      
      println("✓ Second half (high 128b) sent to mesh from buffer")
      println(f"  - Data: 0x${actual_data_cycle4.toString(16)}")
      println(f"  - Buffer valid: ${dut.io.debug_d_buffer_valid.peek().litToBoolean}")
      println(f"  - Using half: ${if (dut.io.debug_d_buffer_half.peek().litToBoolean) "HIGH" else "LOW"}")
      println("✓ NO new SRAM read issued (as expected)")
      
      // ===== Cycle 5: Buffer exhausted =====
      dut.clock.step(1)
      
      println("\n--- Cycle 5: Buffer Exhausted ---")
      
      // Check: buffer should be invalid
      dut.io.debug_d_buffer_valid.expect(false.B, "FAIL: Buffer should be invalid after both halves used")
      dut.io.debug_d_buffer_half.expect(false.B, "FAIL: Buffer half flag should reset")
      
      println("✓ Buffer cleared after both halves consumed")
      println(f"  - Buffer valid: ${dut.io.debug_d_buffer_valid.peek().litToBoolean}")
      
      // If more rows need to be read, should issue new SRAM read
      // (Here we only configured 2 rows, so reading is complete)
      
      println("\n" + "=" * 60)
      println("✓✓✓ All tests PASSED! ✓✓✓")
      println("=" * 60)
    }
  }
  
  it should "NOT buffer for non-FP8 formats (FP4/FP6)" in {
    test(new ExecuteControllerDBufferWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("=" * 60)
      println("Test: FP4 Should NOT Use Buffering")
      println("=" * 60)
      
      dut.clock.step(3)
      
      // Configure as FP4 (should not use buffering)
      dut.io.config_weight_mx_format.poke(0.U)  // FP4
      dut.io.config_enable.poke(true.B)
      dut.clock.step(1)
      dut.io.config_enable.poke(false.B)
      
      val sram_data_fp4 = BigInt("AAAA" * 16, 16)
      
      dut.io.start_preload.poke(true.B)
      dut.io.d_address.poke(0x100.U)
      dut.io.d_rows.poke(4.U)
      dut.clock.step(1)
      
      // Should issue SRAM read
      dut.io.sram_read_valid.expect(true.B)
      
      dut.io.sram_read_ready.poke(true.B)
      dut.clock.step(2)
      
      dut.io.sram_resp_valid.poke(true.B)
      dut.io.sram_resp_data.poke(sram_data_fp4.U)
      dut.io.mesh_d_ready.poke(true.B)
      dut.clock.step(1)
      
      // FP4 should not use buffer
      dut.io.debug_d_buffer_valid.expect(false.B, "FAIL: FP4 should NOT use buffering")
      println("✓ FP4 correctly does NOT use buffering")
      
      dut.io.sram_resp_valid.poke(false.B)
      dut.clock.step(1)
      
      // Next row should immediately issue new SRAM read
      dut.io.sram_read_valid.expect(true.B, "FAIL: FP4 should issue new SRAM read for each row")
      println("✓ FP4 issues new SRAM read for each row (as expected)")
      
      println("\n✓✓✓ FP4 non-buffering test PASSED! ✓✓✓")
    }
  }
}

// ===== Wrapper Module for Testing =====
class ExecuteControllerDBufferWrapper(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // Configuration interface
    val config_weight_mx_format = Input(UInt(2.W))
    val config_enable = Input(Bool())
    
    // Control signals
    val start_preload = Input(Bool())
    val d_address = Input(UInt(32.W))
    val d_rows = Input(UInt(8.W))
    val d_cols = Input(UInt(8.W))
    
    // SRAM read interface
    val sram_read_valid = Output(Bool())
    val sram_read_ready = Input(Bool())
    val sram_read_bank = Output(UInt(4.W))
    val sram_read_addr = Output(UInt(16.W))
    
    // SRAM response interface
    val sram_resp_valid = Input(Bool())
    val sram_resp_data = Input(UInt(256.W))
    
    // Mesh interface
    val mesh_d_valid = Output(Bool())
    val mesh_d_ready = Input(Bool())
    val d_data_to_mesh = Output(UInt(128.W))
    
    // Debug signals
    val debug_d_buffer_valid = Output(Bool())
    val debug_d_buffer_half = Output(Bool())
  })
  
  // Simplified state machine
  val d_data_buffer = Reg(UInt(256.W))
  val d_buffer_valid = RegInit(false.B)
  val d_buffer_half = RegInit(false.B)
  
  val weight_mx_format = RegInit(3.U(2.W))
  when(io.config_enable) {
    weight_mx_format := io.config_weight_mx_format
  }
  
  def needsBuffering(mx_format: UInt): Bool = mx_format === 2.U
  def extractHalf(data: UInt, use_high: Bool): UInt = {
    Mux(use_high, data(255, 128), data(127, 0))
  }
  
  // Simplified SRAM read logic
  val reading = RegInit(false.B)
  val row_counter = RegInit(0.U(8.W))
  
  when(io.start_preload) {
    reading := true.B
    row_counter := 0.U
  }
  
  val needs_sram_read = reading && 
     !(needsBuffering(weight_mx_format) && d_buffer_valid)
  
  io.sram_read_valid := needs_sram_read
  io.sram_read_bank := 0.U
  io.sram_read_addr := io.d_address + row_counter
  
  // Data processing
  val data_from_buffer = extractHalf(d_data_buffer, d_buffer_half)
  val data_from_sram = extractHalf(io.sram_resp_data, false.B)
  
  io.d_data_to_mesh := Mux(d_buffer_valid, data_from_buffer, data_from_sram)
  io.mesh_d_valid := (d_buffer_valid || io.sram_resp_valid) && reading
  
  // Buffer management
  when(io.mesh_d_valid && io.mesh_d_ready) {
    when(needsBuffering(weight_mx_format)) {
      when(!d_buffer_valid && io.sram_resp_valid) {
        // Save to buffer
        d_data_buffer := io.sram_resp_data
        d_buffer_valid := true.B
        d_buffer_half := false.B
      }.elsewhen(d_buffer_valid && !d_buffer_half) {
        // Switch to high half
        d_buffer_half := true.B
      }.elsewhen(d_buffer_valid && d_buffer_half) {
        // Clear buffer
        d_buffer_valid := false.B
        d_buffer_half := false.B
        row_counter := row_counter + 2.U
      }
    }.otherwise {
      // Non-FP8: increment counter directly after each read
      when(io.sram_resp_valid) {
        row_counter := row_counter + 1.U
      }
    }
    
    when(row_counter >= io.d_rows) {
      reading := false.B
    }
    
  }
  
  when(!reading) {
     d_buffer_valid := false.B
     d_buffer_half := false.B
   }
   
  // Debug output
  io.debug_d_buffer_valid := d_buffer_valid
  io.debug_d_buffer_half := d_buffer_half
}
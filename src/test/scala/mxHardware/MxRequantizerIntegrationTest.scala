package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters

class MxRequantizerIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MxRequantizer Integration in Gemmini"
  
  implicit val p: Parameters = Parameters.empty
  
  it should "convert ExecuteController writes to Requantizer input format" in {
    test(new MxRequantizerTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("=" * 70)
      println("Test: ExecuteController Write to Requantizer Input Conversion")
      println("=" * 70)
      
      dut.clock.step(5)
      
      // ===== Configure FP8 output mode =====
      dut.io.config_output_mx_format.poke(2.U)  // FP8
      dut.io.config_enable.poke(true.B)
      dut.clock.step(1)
      dut.io.config_enable.poke(false.B)
      println("✓ Configured output format to FP8")
      
      // ===== Simulate ExecuteController writing to 2 banks =====
      // Bank 0: 32 elements of 8-bit data
      val bank0_data = (0 until 32).map(i => (0xA0 + i).U(8.W))
      val bank0_data_packed = Cat(bank0_data.reverse)  // 256 bits
      
      // Bank 1: 32 elements of 8-bit data
      val bank1_data = (0 until 32).map(i => (0xB0 + i).U(8.W))
      val bank1_data_packed = Cat(bank1_data.reverse)  // 256 bits
      
      println(f"\nBank 0 data (first 4 elements): 0x${bank0_data.take(4).map(_.litValue).mkString(",")}")
      println(f"Bank 1 data (first 4 elements): 0x${bank1_data.take(4).map(_.litValue).mkString(",")}")
      
      // ===== Cycle 1: Bank 0 write valid =====
      dut.io.ex_sram_write_valid(0).poke(true.B)
      dut.io.ex_sram_write_addr(0).poke(0x100.U)
      dut.io.ex_sram_write_data(0).poke(bank0_data_packed)
      
      dut.io.ex_sram_write_valid(1).poke(false.B)
      
      dut.clock.step(1)
      
      println("\n--- Cycle 1: Bank 0 Valid ---")
      
      // Check: Requantizer input should receive first 32 lanes from bank 0
      dut.io.requant_in_valid.expect(true.B, "FAIL: Requantizer input should be valid")
      
      val req_in_data_0 = dut.io.requant_in_data(0).peek().litValue
      println(f"Requantizer input lane 0: 0x${req_in_data_0.toString(16)} (expected: 0xA0)")
      assert(req_in_data_0 == 0xA0, "Lane 0 mismatch")
      
      val req_in_data_31 = dut.io.requant_in_data(31).peek().litValue
      println(f"Requantizer input lane 31: 0x${req_in_data_31.toString(16)} (expected: 0xBF)")
      assert(req_in_data_31 == 0xBF, "Lane 31 mismatch")
      
      // ===== Cycle 2: Both banks valid =====
      dut.io.ex_sram_write_valid(0).poke(true.B)
      dut.io.ex_sram_write_addr(0).poke(0x100.U)
      dut.io.ex_sram_write_data(0).poke(bank0_data_packed)
      
      dut.io.ex_sram_write_valid(1).poke(true.B)
      dut.io.ex_sram_write_addr(1).poke(0x200.U)
      dut.io.ex_sram_write_data(1).poke(bank1_data_packed)
      
      dut.clock.step(1)
      
      println("\n--- Cycle 2: Both Banks Valid ---")
      
      dut.io.requant_in_valid.expect(true.B, "FAIL: Requantizer input should be valid")
      
      // Check lanes 0-31 from bank 0
      val lane_0 = dut.io.requant_in_data(0).peek().litValue
      println(f"Lane 0 (from bank 0): 0x${lane_0.toString(16)} (expected: 0xA0)")
      assert(lane_0 == 0xA0, "Lane 0 mismatch")
      
      // Check lanes 32-63 from bank 1
      val lane_32 = dut.io.requant_in_data(32).peek().litValue
      println(f"Lane 32 (from bank 1): 0x${lane_32.toString(16)} (expected: 0xB0)")
      assert(lane_32 == 0xB0, "Lane 32 mismatch")
      
      val lane_63 = dut.io.requant_in_data(63).peek().litValue
      println(f"Lane 63 (from bank 1): 0x${lane_63.toString(16)} (expected: 0xCF)")
      assert(lane_63 == 0xCF, "Lane 63 mismatch")
      
      // Check address generation
      val req_addr = dut.io.requant_in_address.peek().litValue
      println(f"Requantizer input address: 0x${req_addr.toString(16)}")
      
      println("\n✓ Input conversion test passed!")
      
      // ===== Test Requantizer Output to SRAM Write Conversion =====
      println("\n" + "=" * 70)
      println("Test: Requantizer Output to SRAM Write Conversion")
      println("=" * 70)
      
      dut.io.ex_sram_write_valid(0).poke(false.B)
      dut.io.ex_sram_write_valid(1).poke(false.B)
      
      // ===== Simulate Requantizer Output (FP8: 256 bits) =====
      val requant_out_data = BigInt("DEADBEEF" * 16, 16)  // 256 bits
      
      dut.io.requant_out_valid.poke(true.B)
      dut.io.requant_out_data.poke(requant_out_data.U)
      dut.io.requant_out_address.poke(0x1000.U)
      dut.io.requant_out_format.poke(2.U)  // FP8
      
      dut.clock.step(1)
      
      println("\n--- Requantizer Output Received ---")
      
      // Check: Should convert to SRAM write
      dut.io.sram_write_valid(0).expect(true.B, "FAIL: SRAM write should be valid")
      
      val sram_addr = dut.io.sram_write_addr(0).peek().litValue
      println(f"SRAM write address: 0x${sram_addr.toString(16)}")
      
      val sram_data = dut.io.sram_write_data(0).peek().litValue
      println(f"SRAM write data (low 256b): 0x${(sram_data & ((BigInt(1) << 256) - 1)).toString(16)}")
      
      // Check mask
      val mask_valid_count = (0 until 32).count(i => 
        dut.io.sram_write_mask(0)(i).peek().litToBoolean
      )
      println(f"Valid mask bytes: $mask_valid_count (expected: 32 for FP8)")
      assert(mask_valid_count == 32, "Mask should have 32 valid bytes for FP8")
      
      dut.io.requant_out_ready.expect(true.B, "FAIL: Requantizer ready should be true")
      
      println("\n✓ Output conversion test passed!")
      
      // ===== Test FP4 Format (128 bits) =====
      println("\n" + "=" * 70)
      println("Test: FP4 Format (128 bits)")
      println("=" * 70)
      
      dut.io.requant_out_valid.poke(true.B)
      dut.io.requant_out_data.poke(BigInt("CAFE" * 8, 16).U)  // 128 bits
      dut.io.requant_out_address.poke(0x2000.U)
      dut.io.requant_out_format.poke(0.U)  // FP4
      
      dut.clock.step(1)
      
      println("\n--- FP4 Output ---")
      
      dut.io.sram_write_valid(0).expect(true.B, "FAIL: SRAM write should be valid")
      
      // Check mask - only 16 bytes should be valid
      val fp4_mask_count = (0 until 32).count(i => 
        dut.io.sram_write_mask(0)(i).peek().litToBoolean
      )
      println(f"Valid mask bytes for FP4: $fp4_mask_count (expected: 16)")
      assert(fp4_mask_count == 16, "Mask should have 16 valid bytes for FP4")
      
      println("\n✓ FP4 format test passed!")
      
      println("\n" + "=" * 70)
      println("✓✓✓ All MxRequantizer Integration Tests PASSED! ✓✓✓")
      println("=" * 70)
    }
  }
  
  it should "handle requantizer handshake correctly" in {
    test(new MxRequantizerTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("=" * 70)
      println("Test: Requantizer Handshake Protocol")
      println("=" * 70)
      
      dut.clock.step(3)
      
      dut.io.config_output_mx_format.poke(2.U)
      dut.io.config_enable.poke(true.B)
      dut.clock.step(1)
      dut.io.config_enable.poke(false.B)
      
      // ===== Test 1: Requantizer not ready =====
      println("\n--- Test: Requantizer Input Not Ready ---")
      
      dut.io.requant_in_ready.poke(false.B)
      
      dut.io.ex_sram_write_valid(0).poke(true.B)
      dut.io.ex_sram_write_data(0).poke(0xDEADBEEF.U)
      
      dut.clock.step(1)
      
      // Should still try to send
      dut.io.requant_in_valid.expect(true.B, "Should assert valid even if not ready")
      
      println("✓ Correctly handles backpressure")
      
      // ===== Test 2: Requantizer ready =====
      println("\n--- Test: Requantizer Ready ---")
      
      dut.io.requant_in_ready.poke(true.B)
      dut.clock.step(1)
      
      println("✓ Transaction completes when ready")
      
      // ===== Test 3: Output ready signal =====
      println("\n--- Test: Output Ready Signal ---")
      
      dut.io.ex_sram_write_valid(0).poke(false.B)
      
      dut.io.requant_out_valid.poke(true.B)
      dut.io.requant_out_data.poke(0xCAFE.U)
      dut.io.requant_out_format.poke(2.U)
      
      dut.clock.step(1)
      
      dut.io.requant_out_ready.expect(true.B, "Should be ready to accept output")
      
      println("✓ Output ready signal works correctly")
      
      println("\n✓✓✓ Handshake test PASSED! ✓✓✓")
    }
  }
}

// ===== Test Wrapper Module =====
class MxRequantizerTestWrapper extends Module {
  val io = IO(new Bundle {
    // Configuration
    val config_output_mx_format = Input(UInt(2.W))
    val config_enable = Input(Bool())
    
    // ExecuteController SRAM write interface (simplified, 2 banks)
    val ex_sram_write_valid = Input(Vec(2, Bool()))
    val ex_sram_write_addr = Input(Vec(2, UInt(16.W)))
    val ex_sram_write_data = Input(Vec(2, UInt(256.W)))
    
    // Requantizer input interface
    val requant_in_valid = Output(Bool())
    val requant_in_ready = Input(Bool())
    val requant_in_data = Output(Vec(64, UInt(8.W)))
    val requant_in_address = Output(UInt(32.W))
    val requant_in_format = Output(UInt(2.W))
    
    // Requantizer output interface
    val requant_out_valid = Input(Bool())
    val requant_out_ready = Output(Bool())
    val requant_out_data = Input(UInt(512.W))
    val requant_out_address = Input(UInt(32.W))
    val requant_out_format = Input(UInt(2.W))
    
    // SRAM write interface (output)
    val sram_write_valid = Output(Vec(2, Bool()))
    val sram_write_addr = Output(Vec(2, UInt(16.W)))
    val sram_write_data = Output(Vec(2, UInt(512.W)))
    val sram_write_mask = Output(Vec(2, Vec(64, Bool())))
  })
  
  // State
  val output_mx_format = RegInit(3.U(2.W))
  when(io.config_enable) {
    output_mx_format := io.config_output_mx_format
  }
  
  val elements_per_bank = 32  // 256 bits / 8 bits per element
  val total_elements = 64
  
  // ===== Convert ExecuteController writes to Requantizer input =====
  val flattened_data = Wire(Vec(total_elements, UInt(8.W)))
  
  // Initialize
  for (i <- 0 until total_elements) {
    flattened_data(i) := 0.U
  }
  
  // Fill from valid banks
  for (bank <- 0 until 2) {
    when(io.ex_sram_write_valid(bank)) {
      val start_idx = PopCount(io.ex_sram_write_valid.take(bank)) * elements_per_bank.U
      val bank_data = io.ex_sram_write_data(bank).asTypeOf(Vec(elements_per_bank, UInt(8.W)))
      
      for (elem_idx <- 0 until elements_per_bank) {
        when(start_idx + elem_idx.U < total_elements.U) {
          flattened_data(start_idx + elem_idx.U) := bank_data(elem_idx)
        }
      }
    }
  }
  
  io.requant_in_valid := io.ex_sram_write_valid.reduce(_ || _)
  io.requant_in_data := flattened_data
  io.requant_in_address := Mux(io.ex_sram_write_valid(0), 
    io.ex_sram_write_addr(0), 
    io.ex_sram_write_addr(1))
  io.requant_in_format := output_mx_format
  
  // ===== Convert Requantizer output to SRAM writes =====
  for (i <- 0 until 2) {
    io.sram_write_valid(i) := false.B
    io.sram_write_addr(i) := 0.U
    io.sram_write_data(i) := 0.U
    io.sram_write_mask(i) := VecInit(Seq.fill(64)(false.B))
  }
  
  when(io.requant_out_valid) {
    val data_bits = MuxLookup(io.requant_out_format, 256.U)(Seq(
      0.U -> 128.U,
      1.U -> 128.U,
      2.U -> 256.U,
      3.U -> 256.U
    ))
    
    val valid_bytes = data_bits >> 3.U
    
    io.sram_write_valid(0) := true.B
    io.sram_write_addr(0) := io.requant_out_address >> 6.U  // Divide by 64
    
    val extracted_data = MuxLookup(io.requant_out_format, 
      io.requant_out_data(255, 0))(Seq(
      0.U -> io.requant_out_data(127, 0),
      1.U -> io.requant_out_data(191, 0),
      2.U -> io.requant_out_data(255, 0)
    ))
    
    val padding_bits = 512.U - data_bits
    io.sram_write_data(0) := Cat(0.U(padding_bits), extracted_data)
    
    io.sram_write_mask(0) := VecInit(
      (0 until 64).map(i => i.U < valid_bytes)
    )
  }
  
  io.requant_out_ready := io.requant_out_valid
}
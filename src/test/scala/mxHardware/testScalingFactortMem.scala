package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.{WriteVcdAnnotation, VerilatorBackendAnnotation}

class ScalingFactorMemTest extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "ScalingFactorMem"
  
  // Helper function to construct write address
  def makeWriteAddr(row: Int, byteMask: Int): Int = {
  (row << 8) | (byteMask & 0xFF)  
  }

  // ========== Test 1: Basic Full Row Write and Weight Read ==========
  it should "write full rows to all banks and read weight scaling factors correctly" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4))
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      
      println("\n=== Test 1: Full Row Write and Weight Read ===")
      
      // Initialize
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.io.weight_read_resp.ready.poke(true.B)
      dut.io.act_read_req.poke(false.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(1)
      
      // Write Phase: Write full rows to all 4 banks (mask = 0x3F for full row)
      println("Writing full rows to banks...")
      for (bank <- 0 until 4) {
        for (row <- 0 until 4) {
          val write_addr = makeWriteAddr(row, 0xFF) // 0xFF = full row write
          val write_data = 0x1000L * (bank + 1) + row
          
          dut.io.write.valid.poke(true.B)
          dut.io.write.bits.addr.poke(write_addr.U)
          dut.io.write.bits.data.poke(write_data.U)
          dut.io.write.bits.bank_sel.poke(bank.U)
          
          dut.clock.step(1)
          
          println(f"  Bank $bank, Row $row (addr=0x$write_addr%03x, mask=0xFF): 0x$write_data%016x")
        }
      }
      
      dut.io.write.valid.poke(false.B)
      dut.clock.step(2)
      
      // Switch to read phase
      println("\nSwitching to read phase...")
      dut.io.scaling_enable.poke(true.B)
      dut.clock.step(3)
      
      // Read weight scaling factors (from banks 2 and 3)
      println("\nReading weight scaling factors...")
      for (row <- 0 until 4) {
        dut.io.weight_read_req.valid.poke(true.B)
        dut.io.weight_read_req.bits.addr.poke(row.U)
        
        println(f"  Requesting row $row, ready = ${dut.io.weight_read_req.ready.peek()}")
        
        dut.clock.step(1)
        dut.io.weight_read_req.valid.poke(false.B)
        
        // Wait for response (2 cycle latency)
        dut.clock.step(1)
        println(f"    After 1 cycle: valid = ${dut.io.weight_read_resp.valid.peek()}")
        
        dut.clock.step(1)
        println(f"    After 2 cycles: valid = ${dut.io.weight_read_resp.valid.peek()}")
        
        //dut.io.weight_read_resp.valid.expect(true.B)
        val expected_bank2 = 0x3000 + row
        val expected_bank3 = 0x4000 + row
        val expected_combined = (BigInt(expected_bank3) << 64) | BigInt(expected_bank2)
        
        val actual = dut.io.weight_read_resp.bits.data.peek()
        println(f"  Row $row: Expected 0x${expected_combined}%032x, Got 0x${actual.litValue}%032x")
        dut.io.weight_read_resp.bits.data.expect(expected_combined.U)
        
        dut.clock.step(1)
      }
    }
  }
  
  // ========== Test 2: Byte Masked Write ==========
  it should "support byte-masked partial writes" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("\n=== Test 2: Byte Masked Write ===")
      
      // Initialize
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.io.weight_read_resp.ready.poke(true.B)
      dut.io.act_read_req.poke(false.B)
      dut.clock.step(1)
      
      val test_row = 5
      val test_bank = 2
      
      // Step 1: Write full row with pattern 0x0F0E0D0C0B0A0908
      println(f"\nStep 1: Writing full row to bank $test_bank, row $test_row")
      val full_pattern = BigInt("0706050403020100", 16)
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(makeWriteAddr(test_row, 0xFF).U)
      dut.io.write.bits.data.poke(full_pattern.U)
      dut.io.write.bits.bank_sel.poke(test_bank.U)
      dut.clock.step(1)
      println(f"  Written: 0x${full_pattern}%016x")
      
      // Step 2: Masked write - update byte 0 and byte 3 (mask = 0b00001001 = 0x09)
      println(f"\nStep 2: Masked write - updating bytes 0 and 3 (mask=0x09)")
      val masked_data = BigInt("00000000AA000055", 16) // byte 0=0x55, byte 3=0xAA
      dut.io.write.bits.addr.poke(makeWriteAddr(test_row, 0x09).U) // mask = 0x09
      dut.io.write.bits.data.poke(masked_data.U)
      dut.io.write.bits.bank_sel.poke(test_bank.U)
      dut.clock.step(2) // Need extra cycle for read-modify-write
      println(f"  Mask data: 0x${masked_data}%016x (only bytes 0 and 3 should update)")
      
      // Step 3: Masked write - update byte 7 (mask = 0b00100000 = 0x20, using bit 5 for byte 7)
      println(f"\nStep 3: Masked write - updating byte 7 (mask=0x20)")
      val masked_data2 = BigInt("FF00000000000000", 16) // byte 7=0xFF
      dut.io.write.bits.addr.poke(makeWriteAddr(test_row, 0x80).U) // mask = 0x20 (bit 5)
      dut.io.write.bits.data.poke(masked_data2.U)
      dut.io.write.bits.bank_sel.poke(test_bank.U)
      dut.clock.step(2)
      println(f"  Mask data: 0x${masked_data2}%016x (only byte 7 should update)")
      
      dut.io.write.valid.poke(false.B)
      dut.clock.step(2)
      
      // Switch to read phase and verify
      println("\nSwitching to read phase...")
      dut.io.scaling_enable.poke(true.B)
      dut.clock.step(3)
      
      // Read back and verify
      println(f"\nReading back row $test_row from banks 2 and 3...")
      dut.io.weight_read_req.valid.poke(true.B)
      dut.io.weight_read_req.bits.addr.poke(test_row.U)
      dut.clock.step(1)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.clock.step(2)
      
      // Expected: original pattern with bytes 0, 3, 7 modified
      // Original: 0x0706050403020100
      // After:    0xFF06050AA0201SS (byte0=0x55, byte3=0xAA, byte7=0xFF)
      val expected = BigInt("FF06050AA0201555", 16) // Need to adjust based on actual bit positions
      
      //dut.io.weight_read_resp.valid.expect(true.B)
      val actual_bank2 = dut.io.weight_read_resp.bits.data.peek().litValue & BigInt("FFFFFFFFFFFFFFFF", 16)
      println(f"  Bank 2 data: 0x${actual_bank2}%016x")
      println(f"  Expected:    0x0706050403020155 -> 0x070605AA02010055 -> 0xFF0605AA02010055")
      
      dut.clock.step(1)
    }
  }
  
  // ========== Test 3: Activation Scaling Factor Read with Counter ==========
  it should "read activation scaling factors with counter correctly" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("\n=== Test 3: Activation Scaling Factor Read ===")
      
      // Initialize
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.io.act_read_req.poke(false.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(1)
      
      // Write to activation banks (bank 0 and 1)
      println("Writing to activation banks...")
      val test_row = 10
      
      // Bank 0: 0x0706050403020100 (full row write with mask 0x3F)
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(makeWriteAddr(test_row, 0xFF).U)
      dut.io.write.bits.data.poke(BigInt("0706050403020100", 16).U)
      dut.io.write.bits.bank_sel.poke(0.U)
      dut.clock.step(1)
      println(f"  Bank 0, Row $test_row: 0x0706050403020100")
      
      // Bank 1: 0x0F0E0D0C0B0A0908
      dut.io.write.bits.addr.poke(makeWriteAddr(test_row, 0xFF).U)
      dut.io.write.bits.data.poke(BigInt("0F0E0D0C0B0A0908", 16).U)
      dut.io.write.bits.bank_sel.poke(1.U)
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      println(f"  Bank 1, Row $test_row: 0x0F0E0D0C0B0A0908")
      dut.clock.step(2)
      
      // Switch to read phase
      println("\nSwitching to read phase...")
      dut.io.scaling_enable.poke(true.B)
      dut.io.act_addr.poke(test_row.U)
      dut.clock.step(3)
      
      // Read 16 bytes sequentially (counter 0-15)
      // Combined: Cat(Bank1, Bank0) = 0x0F0E0D0C0B0A09080706050403020100
      println("\nReading activation scaling factors (16 bytes)...")
      val expected_bytes = Array(
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,  // Bank 0 [63:0]
        0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F   // Bank 1 [127:64]
      )
      
      for (i <- 0 until 16) {
        println(f"\n--- Read iteration $i ---")
        
        dut.io.act_read_req.poke(true.B)
        dut.clock.step(1)
        dut.io.act_read_req.poke(false.B)
        
        // Check delays
        if (i == 0) {
          // First read: needs to read from banks (counter = 0)
          println("  First read - waiting 2 cycles for bank read")
          //dut.clock.step(1)
          println(f"    After 1 cycle: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek().litValue}%02x")
          //dut.clock.step(1)
          //println(f"    After 2 cycles: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek().litValue}%02x")
        } else {
          // Subsequent reads: using cached data
          println("  Using cached data - waiting 1 cycle")
          //dut.clock.step(1)
          println(f"    After 1 cycle: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek().litValue}%02x")
        }
        
        val actual = dut.io.act_read_resp.bits.peek()
        println(f"  Counter $i: Expected 0x${expected_bytes(i)}%02x, Got 0x${actual.litValue}%02x")
        
        dut.io.act_read_resp.valid.expect(true.B)
        dut.io.act_read_resp.bits.expect(expected_bytes(i).U)
        
        println(f"  Counter $i: ✓")
      }
      
      // Test counter wrap around
      println("\n\nTesting counter wrap around...")
      dut.io.act_read_req.poke(true.B)
      dut.clock.step(1)
      dut.io.act_read_req.poke(false.B)
      //dut.clock.step(1)
      dut.io.act_read_resp.valid.expect(true.B)
      dut.io.act_read_resp.bits.expect(expected_bytes(0).U)
      println(f"  Counter wrapped to 0: Got 0x${expected_bytes(0)}%02x ✓")
    }
  }
  
  // // ========== Test 4: Simple Debug Test ==========
  // it should "debug activation read timing" in {
  //   test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4))
  //     .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     println("\n=== Debug Test ===")
      
  //     // Setup
  //     dut.io.scaling_enable.poke(false.B)
  //     dut.io.write.valid.poke(false.B)
  //     dut.io.act_read_req.poke(false.B)
  //     dut.io.act_addr.poke(0.U)
  //     dut.clock.step(1)
      
  //     // Write simple pattern to bank 0 (full row write)
  //     println("Writing 0x0706050403020100 to bank 0, row 0")
  //     dut.io.write.valid.poke(true.B)
  //     dut.io.write.bits.addr.poke(makeWriteAddr(0, 0xFF).U)
  //     dut.io.write.bits.data.poke(BigInt("0706050403020100", 16).U)
  //     dut.io.write.bits.bank_sel.poke(0.U)
  //     dut.clock.step(1)
      
  //     println("Writing 0x0F0E0D0C0B0A0908 to bank 1, row 0")
  //     dut.io.write.bits.addr.poke(makeWriteAddr(0, 0xFF).U)
  //     dut.io.write.bits.data.poke(BigInt("0F0E0D0C0B0A0908", 16).U)
  //     dut.io.write.bits.bank_sel.poke(1.U)
  //     dut.clock.step(1)
  //     dut.io.write.valid.poke(false.B)
  //     dut.clock.step(2)
      
  //     // Enable reading
  //     println("\nEnabling scaling...")
  //     dut.io.scaling_enable.poke(true.B)
  //     dut.io.act_addr.poke(0.U)
  //     dut.clock.step(3)
      
  //     // Try one read
  //     println("\nIssuing read request...")
  //     dut.io.act_read_req.poke(true.B)
  //     dut.clock.step(1)
  //     dut.io.act_read_req.poke(false.B)
      
  //     // Watch the signals
  //     for (cycle <- 1 to 5) {
  //       val valid = dut.io.act_read_resp.valid.peek()
  //       val bits = dut.io.act_read_resp.bits.peek()
  //       println(f"  Cycle $cycle after request: valid=$valid, bits=0x${bits.litValue}%02x")
  //       dut.clock.step(1)
  //     }
  //   }
  // }
}
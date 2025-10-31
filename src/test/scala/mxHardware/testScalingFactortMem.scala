package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ScalingFactorMemTest extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "ScalingFactorMem"
  
  // ========== Test 1: Basic Write and Weight Read ==========
  it should "write to all banks and read weight scaling factors correctly" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4)) { dut =>
      
      println("\n=== Test 1: Write and Weight Read ===")
      
      // Initialize
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.io.weight_read_resp.ready.poke(true.B)
      dut.io.act_read_req.poke(false.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(1)
      
      // Write Phase: Write to all 4 banks
      println("Writing to banks...")
      for (bank <- 0 until 4) {
        for (addr <- 0 until 4) {
          dut.io.write.valid.poke(true.B)
          dut.io.write.bits.addr.poke(addr.U)
          dut.io.write.bits.data.poke((0x1000 * (bank + 1) + addr).U)
          dut.io.write.bits.bank_sel.poke(bank.U)
          
          dut.clock.step(1)
          
          println(f"  Bank $bank, Addr $addr: 0x${(0x1000 * (bank + 1) + addr)}%04x")
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
      for (addr <- 0 until 4) {
        dut.io.weight_read_req.valid.poke(true.B)
        dut.io.weight_read_req.bits.addr.poke(addr.U)
        
        // Check if ready
        println(f"  Requesting addr $addr, ready = ${dut.io.weight_read_req.ready.peek()}")
        
        dut.clock.step(1)
        dut.io.weight_read_req.valid.poke(false.B)
        
        // SyncReadMem has 1 cycle latency, then RegNext adds another
        // So total latency is 2 cycles
        dut.clock.step(1)
        println(f"    After 1 cycle: valid = ${dut.io.weight_read_resp.valid.peek()}")
        
        dut.clock.step(1)
        println(f"    After 2 cycles: valid = ${dut.io.weight_read_resp.valid.peek()}")
        
        dut.io.weight_read_resp.valid.expect(true.B)
        val expected_bank2 = 0x3000 + addr
        val expected_bank3 = 0x4000 + addr
        val expected_combined = (BigInt(expected_bank3) << 64) | BigInt(expected_bank2)
        
        val actual = dut.io.weight_read_resp.bits.data.peek()
        println(f"  Addr $addr: Expected 0x${expected_combined}%032x, Got 0x${actual}%032x")
        dut.io.weight_read_resp.bits.data.expect(expected_combined.U)
        
        dut.clock.step(1)
      }
    }
  }
  
  // ========== Test 2: Activation Scaling Factor Read with Counter ==========
  it should "read activation scaling factors with counter correctly" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4)) { dut =>
      
      println("\n=== Test 2: Activation Scaling Factor Read ===")
      
      // Initialize
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.weight_read_req.valid.poke(false.B)
      dut.io.act_read_req.poke(false.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(1)
      
      // Write to activation banks (bank 0 and 1)
      println("Writing to activation banks...")
      val test_addr = 10
      
      // Bank 0: 0x0706050403020100
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(test_addr.U)
      dut.io.write.bits.data.poke(BigInt("0706050403020100", 16).U)
      dut.io.write.bits.bank_sel.poke(0.U)
      dut.clock.step(1)
      println(f"  Bank 0, Addr $test_addr: 0x0706050403020100")
      
      // Bank 1: 0x0F0E0D0C0B0A0908
      dut.io.write.bits.addr.poke(test_addr.U)
      dut.io.write.bits.data.poke(BigInt("0F0E0D0C0B0A0908", 16).U)
      dut.io.write.bits.bank_sel.poke(1.U)
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      println(f"  Bank 1, Addr $test_addr: 0x0F0E0D0C0B0A0908")
      dut.clock.step(2)
      
      // Switch to read phase
      println("\nSwitching to read phase...")
      dut.io.scaling_enable.poke(true.B)
      dut.io.act_addr.poke(test_addr.U)
      dut.clock.step(3)
      
      // Read 16 bytes sequentially (counter 0-15)
      // Combined: Cat(Bank1, Bank0) = 0x0F0E0D0C0B0A09080706050403020100
      // Byte 0 is bits [7:0] = 0x00
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
          dut.clock.step(1)
          println(f"    After 1 cycle: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek()}%02x")
          dut.clock.step(1)
          println(f"    After 2 cycles: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek()}%02x")
        } else {
          // Subsequent reads: using cached data
          println("  Using cached data - waiting 1 cycle")
          dut.clock.step(1)
          println(f"    After 1 cycle: valid = ${dut.io.act_read_resp.valid.peek()}, bits = 0x${dut.io.act_read_resp.bits.peek()}%02x")
        }
        
        val actual = dut.io.act_read_resp.bits.peek()
        println(f"  Counter $i: Expected 0x${expected_bytes(i)}%02x, Got 0x${actual}%02x")
        
        dut.io.act_read_resp.valid.expect(true.B)
        dut.io.act_read_resp.bits.expect(expected_bytes(i).U)
        
        println(f"  Counter $i: ✓")
      }
      
      // Test counter wrap around
      println("\n\nTesting counter wrap around...")
      dut.io.act_read_req.poke(true.B)
      dut.clock.step(1)
      dut.io.act_read_req.poke(false.B)
      dut.clock.step(2) // Counter wrapped, triggers new bank read
      dut.io.act_read_resp.valid.expect(true.B)
      dut.io.act_read_resp.bits.expect(expected_bytes(0).U)
      println(f"  Counter wrapped to 0: Got 0x${expected_bytes(0)}%02x ✓")
    }
  }
  
  // ========== Test 3: Simple Debug Test ==========
  it should "debug activation read timing" in {
    test(new ScalingFactorMem(depth = 256, bankWidth = 64, actOutputScalingWidth = 8, numBanks = 4)) { dut =>
      
      println("\n=== Debug Test ===")
      
      // Setup
      dut.io.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.act_read_req.poke(false.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(1)
      
      // Write simple pattern to bank 0
      println("Writing 0x0706050403020100 to bank 0, addr 0")
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0.U)
      dut.io.write.bits.data.poke(BigInt("0706050403020100", 16).U)
      dut.io.write.bits.bank_sel.poke(0.U)
      dut.clock.step(1)
      
      println("Writing 0x0F0E0D0C0B0A0908 to bank 1, addr 0")
      dut.io.write.bits.addr.poke(0.U)
      dut.io.write.bits.data.poke(BigInt("0F0E0D0C0B0A0908", 16).U)
      dut.io.write.bits.bank_sel.poke(1.U)
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      dut.clock.step(2)
      
      // Enable reading
      println("\nEnabling scaling...")
      dut.io.scaling_enable.poke(true.B)
      dut.io.act_addr.poke(0.U)
      dut.clock.step(3)
      
      // Try one read
      println("\nIssuing read request...")
      dut.io.act_read_req.poke(true.B)
      dut.clock.step(1)
      dut.io.act_read_req.poke(false.B)
      
      // Watch the signals
      for (cycle <- 1 to 5) {
        val valid = dut.io.act_read_resp.valid.peek()
        val bits = dut.io.act_read_resp.bits.peek()
        println(f"  Cycle $cycle after request: valid=$valid, bits=0x${bits}%02x")
        dut.clock.step(1)
      }
    }
  }
}
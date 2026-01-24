package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ScalingFactorMemSpec extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "ScalingFactorMem Double Buffer"
  
  it should "test concurrent read and write in FP8 mode" in {
    test(new ScalingFactorMem(
      depth = 128,
      bankWidth = 128,
      actOutputScalingWidth = 8,
      numBanks = 8,
      testConfig = false,
      meshRows = 16,
      tileRows = 1
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      val meshRows = 16
      val tileRows = 1
      val depth = 4
      val max_block_fp8 = meshRows * tileRows  // 16
      
      println("=" * 80)
      println(s"Testing FP8 Mode Concurrent Read/Write (depth=$depth, max_block_fp8=$max_block_fp8)")
      println("=" * 80)
      
      // Set FP8 mode
      dut.io.dataType.poke(2.U)
      
      // Initialize control signals
      dut.io.scaleMemCnlt.counter_a.poke(0.U)
      dut.io.scaleMemCnlt.counter_b.poke(0.U)
      dut.io.scaleMemCnlt.fire_a.poke(false.B)
      dut.io.scaleMemCnlt.fire_b.poke(false.B)
      dut.io.scaleMemCnlt.baseAddress_act.poke(0.U)
      dut.io.scaleMemCnlt.baseAddress_w.poke(0.U)
      
      // Initialize counters
      dut.io.counter_i.poke(0.U)
      dut.io.counter_j.poke(0.U)
      dut.io.counter_k.poke(0.U)
      
      // Initialize write signals
      dut.io.scale_mem_write_act.valid.poke(false.B)
      dut.io.scale_mem_write_w.valid.poke(false.B)
      
      // Initialize read request
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_req.bits.addr.poke(0.U)
      dut.io.read_req.bits.scaling_enable.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      
      dut.clock.step(5)
      
      // Helper function to create test data with pattern
      def createTestData(baseValue: Int): BigInt = {
        val pattern = baseValue & 0xFF
        var lowData = BigInt(0)
        var highData = BigInt(0)
        
        for (i <- 0 until 16) {
          lowData = lowData | (BigInt(pattern) << (i * 8))
        }
        for (i <- 0 until 16) {
          highData = highData | (BigInt((pattern + 0x80) & 0xFF) << (i * 8))
        }
        
        (highData << 128) | lowData
      }
      
      // // ========================================================================
      // // Phase 1: Write initial data to Buffer 0
      // // ========================================================================
      // println("\n--- Phase 1: Writing initial data to Buffer 0 (Act & Weight) ---")
      
      // // Write activation buffer 0
      // for (addr <- 0 until depth) {
      //   val fullData = createTestData(addr)
      //   dut.io.scale_mem_write_act.valid.poke(true.B)
      //   dut.io.scale_mem_write_act.bits.addr.poke(addr.U)
      //   dut.io.scale_mem_write_act.bits.data.poke(fullData.U)
      //   dut.clock.step(1)
      //   while (!dut.io.scale_mem_write_act.ready.peek().litToBoolean) {
      //     dut.clock.step(1)
      //   }
      // }
      // dut.io.scale_mem_write_act.valid.poke(false.B)
      // println(s"  Activation buffer 0 initialized ($depth entries)")
      
      // // Write weight buffer 0
      // for (addr <- 0 until depth) {
      //   val fullData = createTestData(addr + 100)
      //   dut.io.scale_mem_write_w.valid.poke(true.B)
      //   dut.io.scale_mem_write_w.bits.addr.poke(addr.U)
      //   dut.io.scale_mem_write_w.bits.data.poke(fullData.U)
      //   dut.clock.step(1)
      //   while (!dut.io.scale_mem_write_w.ready.peek().litToBoolean) {
      //     dut.clock.step(1)
      //   }
      // }
      // dut.io.scale_mem_write_w.valid.poke(false.B)
      // println(s"  Weight buffer 0 initialized ($depth entries)")
      
      // dut.clock.step(10)
      
      // ========================================================================
      // Phase 2: Concurrent Read from Buffer 0 and Write to Buffer 1
      // ========================================================================
      println("\n--- Phase 2: Concurrent Read (Buffer 0) and Write (Buffer 1) ---")
      
      // State variables for reading
      var counter_i = 0
      var counter_j = 0
      var counter_k = 0
      var i_cycle_count = 0
      var read_cycle_count = 0
      val actBuffer0 = Array.fill(10)(BigInt(0))
      val weightBuffer0 = Array.fill(10)(BigInt(0))
      // State variables for writing
      var act_write_addr = 0
      var act_write_addr_1 = 0
      var weight_write_addr = 0
      var weight_write_addr_1 = 0
      var act_writes_completed = 0
      var weight_writes_completed = 0
      
      // Track read responses
      var read_responses = 0
      var expected_scale_sum = 0
      var correct_reads = 0
      var incorrect_reads = 0
      
      // Enable reading
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_req.bits.scaling_enable.poke(true.B)
      
      // Run concurrent read/write test
      val max_cycles = 5000
      var cycle = 0
      
      while (cycle < max_cycles && (counter_k < depth || act_writes_completed < depth || weight_writes_completed < depth)) {
        
        // ==================== WRITE SIDE ====================
        // Write activation data to buffer 1 whenever ready
        if (act_writes_completed < 10 && dut.io.scale_mem_write_act.ready.peek().litToBoolean) {
          val fullData = createTestData(act_write_addr + 200)  // Different pattern
          actBuffer0(act_writes_completed) = fullData
          dut.io.scale_mem_write_act.valid.poke(true.B)
          dut.io.scale_mem_write_act.bits.addr.poke(act_write_addr_1.U)
          dut.io.scale_mem_write_act.bits.data.poke(fullData.U)
          act_write_addr += 1
          if(act_write_addr % 2 == 1){
            act_write_addr_1 += 1
          }
          act_writes_completed += 1
          println(s"  [Write] act_write_addr: $act_write_addr")
          if (act_writes_completed % 32 == 0) {
            println(s"  [Write] Activation: $act_writes_completed/$depth entries written to buffer 1")
          }
        } else {
          dut.io.scale_mem_write_act.valid.poke(false.B)
        }
        
        // Write weight data to buffer 1 whenever ready
        if (weight_writes_completed < 10 && dut.io.scale_mem_write_w.ready.peek().litToBoolean) {
          val fullData = createTestData(weight_write_addr + 250)  // Different pattern
          weightBuffer0(weight_write_addr) = fullData
          dut.io.scale_mem_write_w.valid.poke(true.B)
          dut.io.scale_mem_write_w.bits.addr.poke(weight_write_addr_1.U)
          dut.io.scale_mem_write_w.bits.data.poke(fullData.U)
          weight_write_addr += 1
          if(weight_write_addr % 2 == 0){
            weight_write_addr_1 += 1
          }

          weight_writes_completed += 1
          println(s"  [Write] w_write_addr: $act_write_addr")
          if (weight_writes_completed % 32 == 0) {
            println(s"  [Write] Weight: $weight_writes_completed/$depth entries written to buffer 1")
          }
        } else {
          dut.io.scale_mem_write_w.valid.poke(false.B)
        }
        
        dut.io.read_req.valid.poke(true.B)
        // ==================== READ SIDE ====================
        if (counter_k < depth) {
          // Update counter_i: increment by 16 every 4 cycles, reset at max_block_fp8
          if (i_cycle_count ==16) {
            counter_i = counter_i + 16
            i_cycle_count = 0
            println(f"  [Read] counter_i = $counter_i%2d")
            if (counter_i == 64) {
              counter_i = 0
              counter_j = counter_j + 16
              println(f"  [Read] counter_j = $counter_j%2d")
              if (counter_j == 64) {
                counter_j = 0
                counter_k = counter_k + 1
                println(f"  [Read] counter_k = $counter_k%3d")
                // if (counter_k % 16 == 0 && counter_k < depth) {
                //   println(f"  [Read] counter_k = $counter_k%3d (i=$counter_i%2d, j=$counter_j%2d)")
                // }
              }
            }
          }
          //println("\n--- set input counters ---")
          // Poke counters
          dut.clock.step(1)
          dut.io.counter_i.poke(counter_i.U)
          dut.io.counter_j.poke(counter_j.U)
          dut.io.counter_k.poke(counter_k.U)
          dut.io.read_req.bits.addr.poke(counter_k.U)
          
          // Check read response
          if (dut.io.read_resp.valid.peek().litToBoolean) {
            read_responses += 1
            
            // Verify scale values (act_scale[0] + weight_scale[0])
            val received_scale = dut.io.read_resp.bits.combined_scales(0).peek().litValue.toInt
            
            // Expected: act_pattern = counter_k, weight_pattern = counter_k + 100
            // E8M0 format: just add the exponents
            // val expected_act = (actBuffer0(counter_k) & 0xFF).toInt
            // val expected_weight = (weightBuffer0(counter_k) & 0xFF).toInt
            // expected_scale_sum = (expected_act + expected_weight) & 0x1FF
            
            val expected_act = counter_k & 0xFF
            val expected_weight = (counter_k + 100) & 0xFF
            expected_scale_sum = (expected_act + expected_weight) & 0x1FF
            println(f"k=$counter_k: Expected scale=$expected_scale_sum%03d, Got=$received_scale%03d")
            if (received_scale == expected_scale_sum) {
              correct_reads += 1
            } else {
              incorrect_reads += 1
              if (incorrect_reads <= 5) {  // Print first 5 errors
                println(f"  [ERROR] k=$counter_k: Expected scale=$expected_scale_sum%03d, Got=$received_scale%03d")
              }
            }
            
            if (read_responses % 50 == 0) {
              println(f"  [Read] Responses: $read_responses, Correct: $correct_reads, Incorrect: $incorrect_reads")
            }
          }
          
          i_cycle_count += 1
        } else {
          dut.io.read_req.valid.poke(false.B)
        }
        
        dut.clock.step(1)
        cycle += 1
        if (counter_k == 4) {
          fail("Force stop at counter_k = 4") 
        }
      }
      
      
      // Final statistics
      println("\n" + "=" * 80)
      println("Test Results:")
      println("=" * 80)
      println(f"Total cycles: $cycle")
      println(f"Activation writes completed: $act_writes_completed/$depth")
      println(f"Weight writes completed: $weight_writes_completed/$depth")
      println(f"Read responses received: $read_responses")
      println(f"Correct reads: $correct_reads")
      println(f"Incorrect reads: $incorrect_reads")
      
      if (incorrect_reads == 0 && read_responses > 0) {
        println("\n✓ All read values matched expected values!")
      } else if (incorrect_reads > 0) {
        println(s"\n✗ Found $incorrect_reads incorrect read values")
      }
      
      println("=" * 80)
      
      dut.clock.step(10)
    }
  }
}
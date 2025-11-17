package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class ScalingFactorMemTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ScalingFactorMem"

  it should "write to both banks and read combined scales" in {
    test(new ScalingFactorMem(
      depth = 256,
      bankWidth = 256,
      actOutputScalingWidth = 8,
      numBanks = 2
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Initialize
      dut.io.write.valid.poke(false.B)
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      dut.clock.step(5)

      println("=== Test: Write and Read Scaling Factors ===")
      
      // Step 1: Write to Bank 0 (Activation scales) at row 0
      println("\nStep 1: Writing to Bank 0 (Activation)")
      val actScales = Seq(127, 127, 127, 127, 127, 127, 127, 127,
                          127, 127, 127, 127, 127, 127, 127, 127,
                          127, 127, 127, 127, 127, 127, 127, 127,
                          127, 127, 127, 127, 127, 127, 127, 127)
      // val actScales_1 = Seq(1, 1, 1, 1, 1, 1, 1, 1,
      //                     1, 1, 1, 1, 1, 1, 1, 1,
      //                     1, 1, 1, 1, 1, 1, 1, 1,
      //                     1, 1, 1, 1, 1, 1, 1, 1)
      // val actScales_2 = Seq(2, 2, 2, 2, 2, 2, 2, 2,
      //                     2, 2, 2, 2, 2, 2, 2, 2,
      //                     2, 2, 2, 2, 2, 2, 2, 2,
      //                     2, 2, 2, 2, 2, 2, 2, 2)                    
      val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { 
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      // val actData_1 = actScales_1.zipWithIndex.foldLeft(BigInt(0)) { 
      //   case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      // }
      // val actData_2 = actScales_2.zipWithIndex.foldLeft(BigInt(0)) { 
      //   case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      // }
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0.U)  // addr[0]=0 -> Bank 0, row 0
      dut.io.write.bits.data.poke(actData.U)
      dut.io.write.bits.scaling_enable.poke(false.B)
      dut.clock.step(1)
      
      assert(dut.io.write.ready.peek().litToBoolean, "Write should be ready when scaling_enable=false")
      dut.io.write.valid.poke(false.B)
      dut.clock.step(1)
      
      // Step 2: Write to Bank 1 (Weight scales) at row 0
      println("Step 2: Writing to Bank 1 (Weight)")
      val weightScales = (0 until 32).map(i => 128 + i)  // 128, 129, 130, ... 159
      val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) {
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      // val weightScales_1 = (0 until 32).map(i => 150 + i)  // 128, 129, 130, ... 159
      // val weightData_1 = weightScales_1.zipWithIndex.foldLeft(BigInt(0)) {
      //   case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      // }
      // val weightScales_2 = (0 until 32).map(i => 160 + i)  // 128, 129, 130, ... 159
      // val weightData_2 = weightScales_2.zipWithIndex.foldLeft(BigInt(0)) {
      //   case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      // }
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(1.U)  // addr[0]=1 -> Bank 1, row 0
      dut.io.write.bits.data.poke(weightData.U)
      dut.io.write.bits.scaling_enable.poke(false.B)
      dut.clock.step(1)
      

      dut.io.write.valid.poke(false.B)
      dut.clock.step(1)
      
      // Step 3: Enable scaling mode and read
      println("\nStep 3: Enabling scaling mode and reading")
      
      dut.io.write.bits.scaling_enable.poke(true.B)
      dut.clock.step(1)
      
      // Send read request
      dut.io.read_req.valid.poke(true.B)
      dut.io.read_req.bits.addr.poke(0.U)  // Read row 0, bank 0
      dut.clock.step(1)
      // dut.io.read_req.valid.poke(true.B)
      // dut.io.read_req.bits.addr.poke(1.U)  // Read row 0
      // dut.clock.step(1)
      // dut.io.read_req.valid.poke(true.B)
      // dut.io.read_req.bits.addr.poke(2.U)  // Read row 0
      // dut.clock.step(1)
      // dut.io.read_req.valid.poke(true.B)
      // dut.io.read_req.bits.addr.poke(3.U)  // Read row 0
      // dut.clock.step(1)

      assert(dut.io.read_req.ready.peek().litToBoolean, "Read should be ready when scaling_enable=true")
      
      dut.io.read_req.valid.poke(false.B)
      
      // Wait for response
      println("\nWaiting for response...")
      var cycles = 0
      var gotResponse = false
      while (cycles < 50 && !gotResponse) {
        if (dut.io.read_resp.valid.peek().litToBoolean) {
          gotResponse = true
          println(s"\n✓ Response received after $cycles cycles")
          println("\nFirst 8 combined scales:")
          for (i <- 0 until 8) {
            val combined = dut.io.read_resp.bits.combined_scales(i).peek().litValue.toInt
            val expected = {
              val sum = actScales(cycles) + weightScales(i)
              if (sum >= 255) sum - 255 else 0
            }
            println(f"  Scale[$i]: expected=$expected%3d, combined=$combined%3d (act=${actScales(0)}%3d + weight=${weightScales(i)}%3d - 255 = $expected%3d)")
          }
        }
        dut.clock.step(1)
        cycles += 1
      }
      
      assert(gotResponse, "Should receive read response within 50 cycles")
      dut.clock.step(31)
    }
  }

  // it should "handle multiple sequential reads" in {
  //   test(new ScalingFactorMem()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.write.valid.poke(false.B)
  //     dut.io.read_req.valid.poke(false.B)
  //     dut.io.read_resp.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test: Multiple Sequential Reads ===")
      
  //     // Write 3 rows to both banks
  //     for (row <- 0 until 3) {
  //       println(s"\nWriting row $row...")
        
  //       // Bank 0 - different activation scale for each row
  //       val actScale = 120 + row * 5
  //       val actData = (0 until 32).map(_ => actScale).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke((row * 2).U)
  //       dut.io.write.bits.data.poke(actData.U)
  //       dut.io.write.bits.scaling_enable.poke(false.B)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       // Bank 1
  //       val weightData = (0 until 32).map(i => 130 + i).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke((row * 2 + 1).U)
  //       dut.io.write.bits.data.poke(weightData.U)
  //       dut.io.write.bits.scaling_enable.poke(false.B)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
  //     }
      
  //     // Enable scaling and read
  //     dut.io.write.bits.scaling_enable.poke(true.B)
  //     dut.clock.step(2)
      
  //     for (row <- 0 until 3) {
  //       println(s"\nReading row $row...")
        
  //       dut.io.read_req.valid.poke(true.B)
  //       dut.io.read_req.bits.addr.poke(row.U)
  //       dut.clock.step(1)
  //       dut.io.read_req.valid.poke(false.B)
        
  //       var gotResp = false
  //       for (i <- 0 until 40) {
  //         if (dut.io.read_resp.valid.peek().litToBoolean && !gotResp) {
  //           gotResp = true
  //           val scale0 = dut.io.read_resp.bits.combined_scales(0).peek().litValue
  //           println(s"  ✓ Row $row responded, Scale[0] = $scale0")
  //         }
  //         dut.clock.step(1)
  //       }
        
  //       assert(gotResp, s"Row $row should respond")
  //     }
  //   }
  // }

  // it should "verify E8M0 multiplication logic" in {
  //   test(new ScalingFactorMem()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.write.valid.poke(false.B)
  //     dut.io.read_req.valid.poke(false.B)
  //     dut.io.read_resp.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test: E8M0 Multiplication ===")
      
  //     // Test cases: (act, weight, expected_result)
  //     val testCases = Seq(
  //       (127, 128, 0),    // 127 + 128 = 255, 255 - 255 = 0
  //       (127, 129, 1),    // 127 + 129 = 256, 256 - 255 = 1
  //       (100, 150, 0),    // 100 + 150 = 250, < 255 -> 0
  //       (200, 100, 45),   // 200 + 100 = 300, 300 - 255 = 45
  //       (255, 255, 255)   // 255 + 255 = 510, 510 - 255 = 255
  //     )
      
  //     for ((actVal, weightVal, expected) <- testCases) {
  //       println(f"\nTesting: act=$actVal%3d, weight=$weightVal%3d, expected=$expected%3d")
        
  //       // Write activation (all same value)
  //       val actData = (0 until 32).map(_ => actVal).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(0.U)
  //       dut.io.write.bits.data.poke(actData.U)
  //       dut.io.write.bits.scaling_enable.poke(false.B)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       // Write weight (first element is test value)
  //       val weightData = (0 until 32).map(i => if (i == 0) weightVal else 0).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(1.U)
  //       dut.io.write.bits.data.poke(weightData.U)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       // Read
  //       dut.io.write.bits.scaling_enable.poke(true.B)
  //       dut.io.read_req.valid.poke(true.B)
  //       dut.io.read_req.bits.addr.poke(0.U)
  //       dut.clock.step(1)
  //       dut.io.read_req.valid.poke(false.B)
        
  //       // Check result
  //       var foundResult = false
  //       for (_ <- 0 until 40) {
  //         if (dut.io.read_resp.valid.peek().litToBoolean && !foundResult) {
  //           foundResult = true
  //           val result = dut.io.read_resp.bits.combined_scales(0).peek().litValue.toInt
  //           println(f"  Result: $result%3d ${if (result == expected) "✓" else "✗ MISMATCH"}")
  //           assert(result == expected, s"Expected $expected but got $result")
  //         }
  //         dut.clock.step(1)
  //       }
        
  //       assert(foundResult, "Should get result")
  //       dut.clock.step(2)
  //     }
  //   }
  // }
}
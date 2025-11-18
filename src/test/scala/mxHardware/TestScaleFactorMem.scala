package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation


class ScalingFactorMemTest extends AnyFlatSpec with ChiselScalatestTester {
  
  it should "write to 4 banks and read 16×16 E9M0 scale matrix" in {
    test(new ScalingFactorMem(
      depth = 256,
      bankWidth = 128,
      actOutputScalingWidth = 8,
      numBanks = 4
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.write.valid.poke(false.B)
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      dut.clock.step(5)

      println("=== Test: 4-Bank Write and Read 16×16 E9M0 Scale Matrix ===")
      
      // Step 1: Write activation scales
      println("\nStep 1: Writing to Banks 0&1 (Activation)")
      val actScales = Seq.fill(32)(127)  
      val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { 
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0.U)
      dut.io.write.bits.data.poke(actData.U)
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      dut.clock.step(1)
      
      // Step 2: Write weight scales
      println("\nStep 2: Writing to Banks 2&3 (Weight)")
      val weightScales = (0 until 32).map(i => 128 + i)  
      val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) {
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(1.U)
      dut.io.write.bits.data.poke(weightData.U)
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      dut.clock.step(1)
      
      // Step 3: Read and verify E9M0 results
      println("\nStep 3: Reading 16×16 E9M0 scale matrix")
      
      dut.io.read_req.valid.poke(true.B)
      dut.io.read_req.bits.addr.poke(0.U)
      dut.io.read_req.bits.scaling_enable.poke(true.B)
      dut.clock.step(1)
      dut.io.read_req.valid.poke(false.B)
      
      var cycles = 0
      var gotResponse = false
      while (cycles < 50 && !gotResponse) {
        if (dut.io.read_resp.valid.peek().litToBoolean) {
          gotResponse = true
          println(s"✓ Response received after $cycles cycles")
          
          println("\nE9M0 Outer Product Result (first 4×4):")
          println("       Weight[0]  Weight[1]  Weight[2]  Weight[3]")
          println("       (E8M0=128) (E8M0=129) (E8M0=130) (E8M0=131)")
          for (i <- 0 until 4) {
            val actE8M0 = actScales(i)
            print(f"Act[$i%2d]")
            print(f" (E8M0=$actE8M0%3d)  ")
            for (j <- 0 until 4) {
              val combinedE9M0 = dut.io.read_resp.bits.combined_scales(i)(j).peek().litValue.toInt
              print(f"$combinedE9M0%9d  ")

              val act = actScales(i)
              val weight = weightScales(j)
              val expectedE9M0 = act + weight  
              
              assert(combinedE9M0 == expectedE9M0, 
                f"Mismatch at [$i,$j]: got $combinedE9M0, expected $expectedE9M0(E8M0: $act + $weight)")
            }
            println()
          }
          
   
          println("\nE9M0 Format Verification:")
          println("E8M0(127) × E8M0(128) = E9M0(255) means:")
          println("  2^(127-127) × 2^(128-127) = 2^0 × 2^1 = 2")
          println("  E9M0(255) = 2^(255-255) = 2^0 = 1")
          println("  Note: E9M0 needs bias adjustment for actual value")
          

          val checkPositions = Seq((0,0), (5,5), (10,10), (15,15))
          println("\nSpot checks:")
          for ((i, j) <- checkPositions) {
            val combined = dut.io.read_resp.bits.combined_scales(i)(j).peek().litValue.toInt
            val act = actScales(i)
            val weight = weightScales(j)
            val expected = act + weight
            println(f"  [$i%2d,$j%2d]: E9M0=$combined%3d (E8M0: $act%3d + $weight%3d = $expected%3d)")
            assert(combined == expected, f"Mismatch at [$i,$j]")
          }
        }
        dut.clock.step(1)
        cycles += 1
      }
      
      assert(gotResponse, "Should receive 16×16 E9M0 scale matrix")
      dut.clock.step(10)
    }
  }

  // it should "verify E8M0 × E8M0 = E9M0 arithmetic" in {
  //   test(new ScalingFactorMem()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.write.valid.poke(false.B)
  //     dut.io.read_req.valid.poke(false.B)
  //     dut.io.read_resp.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test: E8M0 × E8M0 = E9M0 Arithmetic ===")
      
  //     val testCases = Seq(
  //       (127, 127, 254),  // 2^0 × 2^0 = 2^0     (E9M0: 254 = 2^(254-255) ≈ 0.5, needs adjustment)
  //       (127, 128, 255),  // 2^0 × 2^1 = 2^1     (E9M0: 255 = 2^0)
  //       (128, 128, 256),  // 2^1 × 2^1 = 2^2     (E9M0: 256 = 2^1)
  //       (0, 127, 127),    // 2^-127 × 2^0 = ...
  //       (255, 255, 510)   // Max values
  //     )
      
  //     for ((actVal, weightVal, expectedE9M0) <- testCases) {
  //       println(f"\nTest: E8M0($actVal%3d) × E8M0($weightVal%3d) = E9M0($expectedE9M0%3d)")
        
  //       // Write scales
  //       val actData = (0 until 32).map(_ => actVal).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(0.U)
  //       dut.io.write.bits.data.poke(actData.U)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       val weightData = (0 until 32).map(_ => weightVal).zipWithIndex
  //         .foldLeft(BigInt(0)) { case (acc, (v, i)) => acc | (BigInt(v) << (i * 8)) }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(1.U)
  //       dut.io.write.bits.data.poke(weightData.U)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       // Read and verify
  //       dut.io.read_req.valid.poke(true.B)
  //       dut.io.read_req.bits.addr.poke(0.U)
  //       dut.io.read_req.bits.scaling_enable.poke(true.B)
  //       dut.clock.step(1)
  //       dut.io.read_req.valid.poke(false.B)
        
  //       var gotResult = false
  //       for (_ <- 0 until 40) {
  //         if (dut.io.read_resp.valid.peek().litToBoolean && !gotResult) {
  //           gotResult = true
  //           val result = dut.io.read_resp.bits.combined_scales(0)(0).peek().litValue.toInt
  //           val clampedExpected = expectedE9M0 & 0x1FF  // 只保留 9 bits
  //           println(f"  Result: E9M0 = $result%3d (expected: $clampedExpected%3d)")
  //           assert(result == clampedExpected, f"E8M0 multiplication error")
  //         }
  //         dut.clock.step(1)
  //       }
        
  //       assert(gotResult, "Should get result")
  //       dut.clock.step(2)
  //     }
  //   }
  // }

  // it should "repeat read/write 3 times" in {
  //   test(new ScalingFactorMem()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.write.valid.poke(false.B)
  //     dut.io.read_req.valid.poke(false.B)
  //     dut.io.read_resp.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test: Repeat 3 Times with E9M0 Output ===")
      
  //     for (round <- 0 until 3) {
  //       println(s"\n========== Round $round ==========")
        
  //       val actScales = Seq.fill(32)((120 + round * 5) % 256)
  //       val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { 
  //         case (acc, (v, i)) => acc | (BigInt(v) << (i * 8))
  //       }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(0.U)
  //       dut.io.write.bits.data.poke(actData.U)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       val weightScales = (0 until 32).map(i => (130 + round * 3 + i) % 256)
  //       val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) {
  //         case (acc, (v, i)) => acc | (BigInt(v) << (i * 8))
  //       }
        
  //       dut.io.write.valid.poke(true.B)
  //       dut.io.write.bits.addr.poke(1.U)
  //       dut.io.write.bits.data.poke(weightData.U)
  //       dut.clock.step(1)
  //       dut.io.write.valid.poke(false.B)
  //       dut.clock.step(1)
        
  //       dut.io.read_req.valid.poke(true.B)
  //       dut.io.read_req.bits.addr.poke(0.U)
  //       dut.io.read_req.bits.scaling_enable.poke(true.B)
  //       dut.clock.step(1)
  //       dut.io.read_req.valid.poke(false.B)
        
  //       var gotResp = false
  //       for (_ <- 0 until 40) {
  //         if (dut.io.read_resp.valid.peek().litToBoolean && !gotResp) {
  //           gotResp = true
  //           val scale00 = dut.io.read_resp.bits.combined_scales(0)(0).peek().litValue
  //           val scale55 = dut.io.read_resp.bits.combined_scales(5)(5).peek().litValue
  //           println(f"✓ Round $round: E9M0[0,0]=$scale00%3d, E9M0[5,5]=$scale55%3d")
  //         }
  //         dut.clock.step(1)
  //       }
        
  //       assert(gotResp, s"Round $round should respond")
  //       dut.clock.step(2)
  //     }
      
  //     println("\n=== All 3 rounds completed ===")
  //   }
  // }
}
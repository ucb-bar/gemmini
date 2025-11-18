package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class QuantLutTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "QuantLut"

  it should "write LUT and read back values" in {
    test(new QuantLut(
      wdataWidth = 96,  // 16 entries * 6 bits
      raddrWidth = 4,   // 2^4 = 16 entries
      rdataWidth = 6,   // FP6
      outputnumLanes = 32
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Initialize
      dut.io.lut_write.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.clock.step(2)

      println("=== Test 1: Write LUT ===")
      
      // Prepare LUT data: 16 FP6 values packed into 96 bits
      // Example LUT values: 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60
      val lutValues = Seq(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60)
      val packedLutData = lutValues.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (value, idx)) =>
        acc | (BigInt(value) << (idx * 6))
      }
      
      println(s"Writing LUT data: 0x${packedLutData.toString(16)}")
      
      // Write to LUT
      dut.io.lut_write.valid.poke(true.B)
      dut.io.lut_write.bits.data.poke(packedLutData.U)
      dut.clock.step(1)
      
      // Check ready signal
      assert(dut.io.lut_write.ready.peek().litToBoolean, "LUT should be ready for write")
      assert(!dut.io.lutReadEnable.peek().litToBoolean, "lutReadEnable should be low during write")
      dut.io.lut_write.valid.poke(false.B)
      // when (dut.io.lut_write.ready) {
      //   println("LUT write fired successfully")
      //   dut.io.lut_write.valid.poke(false.B)
      // }.otherwise {
      //   println("WARNING: LUT write did not fire")
      //   dut.io.lut_write.valid.poke(true.B)
      //   dut.io.lut_write.bits.data.poke(packedLutData.U)
      //   dut.clock.step(1)
      // }
      
      dut.clock.step(1)
      
      // After write, lutReadEnable should be high
      assert(dut.io.lutReadEnable.peek().litToBoolean, "lutReadEnable should be high after write")
      
      println("LUT write completed\n")
    }
  }

  it should "find nearest FP6 values and return indices" in {
    test(new QuantLut(
      wdataWidth = 96,
      raddrWidth = 4,
      rdataWidth = 6,
      outputnumLanes = 32
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.lut_write.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.clock.step(2)

      println("=== Test 2: Quantization with LUT ===")
      
      // Write LUT with evenly spaced values
      val lutValues = (0 until 16).map(_ * 4) // 0, 4, 8, 12, ..., 60
      val packedLutData = lutValues.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (value, idx)) =>
        acc | (BigInt(value) << (idx * 6))
      }
      
      println("Step 1: Writing LUT")
      dut.io.lut_write.valid.poke(true.B)
      dut.io.lut_write.bits.data.poke(packedLutData.U)
      dut.clock.step(1)
      dut.io.lut_write.valid.poke(false.B)
      dut.clock.step(1)

      println("Step 2: Sending FP6 inputs for quantization")
      
      // Test inputs: values that should map to specific LUT indices
      val testInputs = Seq(
        1,   // Should map to index 0 (LUT value 0)
        5,   // Should map to index 1 (LUT value 4)
        10,  // Should map to index 2 (LUT value 8)
        13,  // Should map to index 3 (LUT value 12)
        18,  // Should map to index 4 (LUT value 16)
        22,  // Should map to index 5 (LUT value 20)
        26,  // Should map to index 6 (LUT value 24)
        30,  // Should map to index 7 (LUT value 28)
        33,  // Should map to index 8 (LUT value 32)
        37,  // Should map to index 9 (LUT value 36)
        41,  // Should map to index 10 (LUT value 40)
        45,  // Should map to index 11 (LUT value 44)
        49,  // Should map to index 12 (LUT value 48)
        53,  // Should map to index 13 (LUT value 52)
        57,  // Should map to index 14 (LUT value 56)
        61   // Should map to index 15 (LUT value 60)
      )
      
      // Fill remaining lanes with zeros
      val fullInputs = testInputs ++ Seq.fill(32 - testInputs.length)(0)
      
      dut.io.quant_fp6.valid.poke(true.B)
      for (i <- 0 until 32) {
        dut.io.quant_fp6.bits(i).poke(fullInputs(i).U)
      }
      dut.clock.step(1)
      dut.io.quant_fp6.valid.poke(false.B)
      
      // Wait for processing
      //dut.clock.step(3)
      
      // Check output
      println("Step 3: Checking output indices")
      if (dut.io.projected_data.valid.peek().litToBoolean) {
        println("Projected data valid!")
        for (i <- 0 until 16) {
          val index = dut.io.projected_data.bits(i).peek().litValue.toInt
          val input = testInputs(i)
          val expectedLutValue = lutValues(index)
          println(f"Lane $i%2d: input=$input%2d -> index=$index%2d (LUT value=$expectedLutValue%2d)")
        }
      } else {
        println("WARNING: Projected data not valid yet")
      }
      
      dut.clock.step(5)
    }
  }

  it should "handle edge cases correctly" in {
    test(new QuantLut(
      wdataWidth = 96,
      raddrWidth = 4,
      rdataWidth = 6,
      outputnumLanes = 32
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.lut_write.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.clock.step(2)

      println("=== Test 3: Edge Cases ===")
      
      // LUT with specific edge case values
      val lutValues = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 63) // Max FP6 = 63
      val packedLutData = lutValues.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (value, idx)) =>
        acc | (BigInt(value) << (idx * 6))
      }
      
      println("Writing LUT with edge values")
      dut.io.lut_write.valid.poke(true.B)
      dut.io.lut_write.bits.data.poke(packedLutData.U)
      dut.clock.step(1)
      dut.io.lut_write.valid.poke(false.B)
      dut.clock.step(2)

      println("Testing with edge case inputs:")
      
      // Test cases
      val edgeCases = Seq(
        ("Minimum value", 0),
        ("Maximum value", 63),
        ("Mid value", 32),
        ("Between entries", 15)
      )
      
      for ((description, value) <- edgeCases) {
        println(s"\n  Testing: $description (value=$value)")
        
        dut.io.quant_fp6.valid.poke(true.B)
        for (i <- 0 until 32) {
          dut.io.quant_fp6.bits(i).poke(value.U)
        }
        dut.clock.step(1)
        dut.io.quant_fp6.valid.poke(false.B)
        //dut.clock.step(2)
        
        if (dut.io.projected_data.valid.peek().litToBoolean) {
          val index = dut.io.projected_data.bits(0).peek().litValue.toInt
          val lutValue = lutValues(index)
          println(s"    Result: index=$index, LUT value=$lutValue")
        }
        
        dut.clock.step(2)
      }
    }
  }

  it should "handle multiple sequential quantizations" in {
    test(new QuantLut(
      wdataWidth = 96,
      raddrWidth = 4,
      rdataWidth = 6,
      outputnumLanes = 32
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.lut_write.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.clock.step(2)

      println("=== Test 4: Sequential Quantizations ===")
      
      // Setup LUT
      val lutValues = (0 until 16).map(_ * 4)
      val packedLutData = lutValues.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (value, idx)) =>
        acc | (BigInt(value) << (idx * 6))
      }
      
      dut.io.lut_write.valid.poke(true.B)
      dut.io.lut_write.bits.data.poke(packedLutData.U)
      dut.clock.step(1)
      dut.io.lut_write.valid.poke(false.B)
      dut.clock.step(2)

      // Run multiple quantizations
      for (iteration <- 0 until 5) {
        println(s"\nIteration $iteration:")
        
        val baseValue = iteration * 10
        dut.io.quant_fp6.valid.poke(true.B)
        for (i <- 0 until 32) {
          val value = (baseValue + i) & 0x3F  
          dut.io.quant_fp6.bits(i).poke(value.U)
        }
        dut.clock.step(1)
        dut.io.quant_fp6.valid.poke(false.B)
        //dut.clock.step(2)
        
        if (dut.io.projected_data.valid.peek().litToBoolean) {
          val indices = (0 until 8).map(i => 
            dut.io.projected_data.bits(i).peek().litValue.toInt
          )
          println(s"  First 8 indices: ${indices.mkString(", ")}")
        }
        
        dut.clock.step(2)
      }
    }
  }

  it should "output zero when not enabled" in {
    test(new QuantLut(
      wdataWidth = 96,
      raddrWidth = 4,
      rdataWidth = 6,
      outputnumLanes = 32
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      println("=== Test 5: Disabled State ===")
      
      // Don't write LUT, don't send inputs
      dut.io.lut_write.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.clock.step(5)
      
      // Output should be invalid
      assert(!dut.io.projected_data.valid.peek().litToBoolean, 
             "Output should be invalid when no input provided")
      
      // All indices should be zero
      for (i <- 0 until 32) {
        val value = dut.io.projected_data.bits(i).peek().litValue
        assert(value == 0, s"Index $i should be 0, got $value")
      }
      
      println("Verified: outputs are zero when disabled")
    }
  }
}
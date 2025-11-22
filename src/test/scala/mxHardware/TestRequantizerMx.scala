package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class MxRequantizerTest extends AnyFlatSpec with ChiselScalatestTester {
  
  val config = GemminiRequantizerConfig(
    baseAddr = 0x10000000L,
    numInputLanes = 16,
    numOutputLanes = 32,
    gpuMaxFactor = 2,
    gpuWordSize = 4,
    inputBits = 16,
    minOutputBits = 4,
    maxOutputBits = 8,
    outputIdBits = 3
  )
  
  behavior of "MxRequantizer"

  def createDUT() = {
    new MxRequantizer[UInt](
      sp_data_width = 256,
      sp_addr_width = 32,
      scaleMem_data_width = 256,
      scaleMem_addr_width = 9,
      scaleSize = 32,
      scaleMembasewrite = 0,
      config = config
    )
  }

 it should "compute scales and quantize FP8 data" in {
  test(createDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
    
    dut.io.requnat_data_in.valid.poke(false.B)
    dut.io.scaleMem_write.ready.poke(true.B)
    dut.io.requant_data_out.ready.poke(true.B)
    dut.clock.step(5)

    println("=== Test 1: FP8 Quantization and Scale Generation ===")
    println("Need to process 32x32 = 1024 elements before scale write")
    println("With 16 lanes per cycle: 1024/16 = 64 input cycles")
    println("Each 2 cycles (32 elements) -> 1 quantization output")
    println("Expected: 64 input cycles -> 32 quantization outputs -> 1 scale write\n")
    
    // Test data: will send 64 cycles of 16 values each
    val testDataPattern = Seq(
      0x4200,  // BF16: ~8.0
      0x4100,  // BF16: ~4.0
      0x4000,  // BF16: ~2.0
      0x3F80   // BF16: ~1.0
    )
    
    var quantOutputCount = 0
    val baseAddress = 0x1000
    
    println("--- Sending 1024 elements (64 cycles of 16 values) ---\n")
    
    for (cycle <- 0 until 64) {
      if (cycle % 10 == 0 || cycle < 4) {
        println(f"Cycle ${cycle+1}/64: Sending 16 values...")
      }
      
      dut.io.requnat_data_in.valid.poke(true.B)
      dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP8)
      dut.io.requnat_data_in.bits.address.poke((baseAddress).U)
      
      for (i <- 0 until 16) {
        val dataValue = testDataPattern((cycle + i) % testDataPattern.length)
        dut.io.requnat_data_in.bits.data(i).poke(dataValue.U)
      }
      
      // Check for quantization output
      if (dut.io.requant_data_out.valid.peek().litToBoolean) {
        quantOutputCount += 1
        if (quantOutputCount % 5 == 0 || quantOutputCount <= 3) {
          println(f"  -> Received quantization output #$quantOutputCount")
        }
      }
      
      dut.clock.step(1)
    }
    
    dut.io.requnat_data_in.valid.poke(false.B)
    
    println(f"\n✓ Sent all 1024 elements in 64 cycles")
    println(f"  Received $quantOutputCount quantization outputs during input phase\n")
    
    println("Waiting for remaining quantization outputs and scale write...")
    println("(Scale write should only happen after all 32 quantization outputs)\n")
    
    var gotScaleWrite = false
    var additionalWaitCycles = 0
    var testComplete = false
    
    var cycle = 0
    while (cycle < 100 && !testComplete) {
      additionalWaitCycles += 1
      
      // Check for additional quantization outputs
      if (dut.io.requant_data_out.valid.peek().litToBoolean) {
        quantOutputCount += 1
        if (quantOutputCount % 5 == 0 || quantOutputCount >= 30) {
          println(f"  Quantization output #$quantOutputCount received")
        }
      }
      
      // Check for scale write
      if (dut.io.scaleMem_write.valid.peek().litToBoolean && !gotScaleWrite) {
        gotScaleWrite = true
        val addr = dut.io.scaleMem_write.bits.addr.peek().litValue
        val data = dut.io.scaleMem_write.bits.data.peek().litValue
        
        println(f"\n✓ Scale write received at wait cycle $additionalWaitCycles:")
        println(f"  (Total cycles from start: ${64 + additionalWaitCycles})")
        println(f"  Address: 0x$addr%04x")
        println(f"  Scale data (32 bytes): 0x$data%064x")
        
        // Parse and display first few scales (E8M0 format)
        println(f"  First 8 scales (E8M0 format):")
        for (i <- 0 until 8) {
          val scale = ((data >> (i * 8)) & 0xFF).toInt
          val exponent = scale - 127
          println(f"    Scale[$i]: $scale%3d (exponent = $exponent%4d)")
        }
        
        assert(((data & 0xFF).toInt > 0) && ((data & 0xFF).toInt < 255), 
               "Scale should be in valid E8M0 range")
      }
      
      dut.clock.step(1)
      
      if (gotScaleWrite && quantOutputCount >= 32) {
        testComplete = true
        println(f"\n✓ Test completed successfully!")
        println(f"  Total quantization outputs: $quantOutputCount")
        println(f"  Total cycles: ${64 + additionalWaitCycles}")
      }
      
      cycle += 1
    }
    
    assert(quantOutputCount == 32, 
           s"Should receive 32 quantized outputs (got $quantOutputCount)")
    assert(gotScaleWrite, 
           "Should write scales to ScaleFactorMem after processing 32 quantization outputs")
    
    println("\n✓ Test 1 passed: FP8 quantization with 32x32 block scaling working\n")
  }
}

  // it should "compute scales and quantize FP8 data" in {
  //   test(createDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.requnat_data_in.valid.poke(false.B)
  //     dut.io.scaleMem_write.ready.poke(true.B)
  //     dut.io.requant_data_out.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test 1: FP8 Quantization and Scale Generation ===")
      
  //     // Test data: 32 FP16 values (need 2 cycles of 16 lanes each)
  //     val testData1 = Seq.fill(16)(0x4200)  // BF16: ~8.0
  //     val testData2 = Seq.fill(16)(0x4100)  // BF16: ~4.0
      
  //     println("\nCycle 1: Sending first 16 values...")
  //     dut.io.requnat_data_in.valid.poke(true.B)
  //     dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP8)
  //     dut.io.requnat_data_in.bits.address.poke(0x1000.U)
      
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData1(i).U)
  //     }
      
  //     dut.clock.step(1)
      
  //     println("Cycle 2: Sending second 16 values...")
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData2(i).U)
  //     }
      
  //     dut.clock.step(1)
  //     dut.io.requnat_data_in.valid.poke(false.B)
      
  //     println("Waiting for quantization output...")
      
  //     var gotQuantOutput = false
  //     for (cycle <- 0 until 20) {
  //       if (dut.io.requant_data_out.valid.peek().litToBoolean && !gotQuantOutput) {
  //         gotQuantOutput = true
  //         val quantData = dut.io.requant_data_out.bits.data.peek().litValue
  //         val dataType = dut.io.requant_data_out.bits.dataType.peek().litValue
  //         val address = dut.io.requant_data_out.bits.address.peek().litValue
          
  //         println(f"\nQuantization output received at cycle $cycle:")
  //         println(f"  Address: 0x$address%04x")
  //         println(f"  DataType: $dataType (0=FP4, 1=FP6, 2=FP8)")
  //         println(f"  Quantized data: 0x$quantData%064x")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotQuantOutput, "Should receive quantized output")
      
  //     println("\nWaiting for scale write to ScaleFactorMem...")
      
  //     var gotScaleWrite = false
  //     for (cycle <- 0 until 20) {
  //       if (dut.io.scaleMem_write.valid.peek().litToBoolean && !gotScaleWrite) {
  //         gotScaleWrite = true
  //         val addr = dut.io.scaleMem_write.bits.addr.peek().litValue
  //         val data = dut.io.scaleMem_write.bits.data.peek().litValue
          
  //         println(f"\nScale write output at cycle $cycle:")
  //         println(f"  Address: 0x$addr%04x")
  //         println(f"  Scale data (32 bytes): 0x$data%064x")
          
  //         // Verify scale is reasonable (E8M0 format, biased exponent around 127)
  //         val scale0 = (data & 0xFF).toInt
  //         println(f"  First scale (E8M0): $scale0 (bias=127, so exponent=${scale0-127})")
          
  //         assert(scale0 > 0 && scale0 < 255, "Scale should be in valid E8M0 range")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotScaleWrite, "Should write scales to ScaleFactorMem")
  //     println("\nTest 1 passed: FP8 quantization and scale generation working\n")
  //   }


  
  
  // }

  // it should "compute scales for FP6 data and use QuantLut" in {
  //   test(createDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.requnat_data_in.valid.poke(false.B)
  //     dut.io.scaleMem_write.ready.poke(true.B)
  //     dut.io.requant_data_out.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test 2: FP6 Quantization with QuantLut ===")
      
  //     val testData1 = (0 until 16).map(i => (0x4000 + i * 0x10))  // Range of BF16 values
  //     val testData2 = (0 until 16).map(i => (0x3F00 + i * 0x10))
      
  //     println("\nSending FP6 quantization request...")
  //     dut.io.requnat_data_in.valid.poke(true.B)
  //     dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP6)
  //     dut.io.requnat_data_in.bits.address.poke(0x2000.U)
      
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData1(i).U)
  //     }
      
  //     dut.clock.step(1)
      
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData2(i).U)
  //     }
      
  //     dut.clock.step(1)
  //     dut.io.requnat_data_in.valid.poke(false.B)
      
  //     var gotQuantOutput = false
  //     for (cycle <- 0 until 30) {
  //       if (dut.io.requant_data_out.valid.peek().litToBoolean && !gotQuantOutput) {
  //         gotQuantOutput = true
  //         val quantData = dut.io.requant_data_out.bits.data.peek().litValue
          
  //         println(f"\nFP6 quantization output (after QuantLut projection):")
  //         println(f"  Quantized data: 0x$quantData%032x")
  //         println(f"  This is 32 4-bit values projected from FP6")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotQuantOutput, "Should receive FP6 quantized output")
      
  //     var gotScaleWrite = false
  //     for (cycle <- 0 until 20) {
  //       if (dut.io.scaleMem_write.valid.peek().litToBoolean && !gotScaleWrite) {
  //         gotScaleWrite = true
  //         val addr = dut.io.scaleMem_write.bits.addr.peek().litValue
  //         val data = dut.io.scaleMem_write.bits.data.peek().litValue
          
  //         println(f"\nFP6 scale write:")
  //         println(f"  Address: 0x$addr%04x")
  //         println(f"  Scale data: 0x$data%064x")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotScaleWrite, "Should write FP6 scales")
  //     println("\nTest 2 passed: FP6 quantization with QuantLut working\n")
  //   }
  // }

  // it should "compute scales for FP4 data" in {
  //   test(createDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.requnat_data_in.valid.poke(false.B)
  //     dut.io.scaleMem_write.ready.poke(true.B)
  //     dut.io.requant_data_out.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test 3: FP4 Quantization ===")
      
  //     val testData1 = Seq.fill(16)(0x3C00)  // BF16: 1.0
  //     val testData2 = Seq.fill(16)(0x4000)  // BF16: 2.0
      
  //     dut.io.requnat_data_in.valid.poke(true.B)
  //     dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP4)
  //     dut.io.requnat_data_in.bits.address.poke(0x3000.U)
      
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData1(i).U)
  //     }
      
  //     dut.clock.step(1)
      
  //     for (i <- 0 until 16) {
  //       dut.io.requnat_data_in.bits.data(i).poke(testData2(i).U)
  //     }
      
  //     dut.clock.step(1)
  //     dut.io.requnat_data_in.valid.poke(false.B)
      
  //     var gotQuantOutput = false
  //     for (cycle <- 0 until 20) {
  //       if (dut.io.requant_data_out.valid.peek().litToBoolean && !gotQuantOutput) {
  //         gotQuantOutput = true
  //         val quantData = dut.io.requant_data_out.bits.data.peek().litValue
          
  //         println(f"\nFP4 quantization output:")
  //         println(f"  Quantized data: 0x$quantData%032x")
  //         println(f"  This is 32 4-bit FP4 values")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotQuantOutput, "Should receive FP4 quantized output")
      
  //     var gotScaleWrite = false
  //     for (cycle <- 0 until 20) {
  //       if (dut.io.scaleMem_write.valid.peek().litToBoolean && !gotScaleWrite) {
  //         gotScaleWrite = true
  //         val data = dut.io.scaleMem_write.bits.data.peek().litValue
          
  //         println(f"\nFP4 scale write:")
  //         println(f"  Scale data: 0x$data%064x")
          
  //         val scale0 = (data & 0xFF).toInt
  //         println(f"  First scale (E8M0): $scale0")
          
  //         assert(scale0 >= 0 && scale0 <= 255, "Scale should be valid E8M0")
  //       }
  //       dut.clock.step(1)
  //     }
      
  //     assert(gotScaleWrite, "Should write FP4 scales")
  //     println("\nTest 3 passed: FP4 quantization working\n")
  //   }
  // }

  // it should "handle multiple blocks and generate multiple scale writes" in {
  //   test(createDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
  //     dut.io.requnat_data_in.valid.poke(false.B)
  //     dut.io.scaleMem_write.ready.poke(true.B)
  //     dut.io.requant_data_out.ready.poke(true.B)
  //     dut.clock.step(5)

  //     println("=== Test 4: Multiple Blocks ===")
      
  //     val numBlocks = 3
  //     var scaleWriteCount = 0
      
  //     for (block <- 0 until numBlocks) {
  //       println(f"\nProcessing block $block...")
        
  //       val testData1 = Seq.fill(16)((0x4000 + block * 0x100))
  //       val testData2 = Seq.fill(16)((0x3F00 + block * 0x100))
        
  //       dut.io.requnat_data_in.valid.poke(true.B)
  //       dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP8)
  //       dut.io.requnat_data_in.bits.address.poke((0x4000 + block * 0x100).U)
        
  //       for (i <- 0 until 16) {
  //         dut.io.requnat_data_in.bits.data(i).poke(testData1(i).U)
  //       }
        
  //       dut.clock.step(1)
        
  //       for (i <- 0 until 16) {
  //         dut.io.requnat_data_in.bits.data(i).poke(testData2(i).U)
  //       }
        
  //       dut.clock.step(1)
  //       dut.io.requnat_data_in.valid.poke(false.B)
        
  //       for (_ <- 0 until 20) {
  //         if (dut.io.scaleMem_write.valid.peek().litToBoolean) {
  //           scaleWriteCount += 1
  //           val addr = dut.io.scaleMem_write.bits.addr.peek().litValue
  //           println(f"  Scale write #$scaleWriteCount at address 0x$addr%04x")
  //         }
  //         dut.clock.step(1)
  //       }
  //     }
      
  //     println(f"\nTotal scale writes: $scaleWriteCount")
  //     assert(scaleWriteCount >= numBlocks, 
  //       f"Should have at least $numBlocks scale writes for $numBlocks blocks")
      
  //     println("\nTest 4 passed: Multiple block processing working\n")
  //   }
  // }
}
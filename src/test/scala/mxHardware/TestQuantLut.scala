// package gemmini

// import chisel3._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import chiseltest.simulator.WriteVcdAnnotation

// class QuantLutDoubleBufferSpec extends AnyFlatSpec with ChiselScalatestTester {
//   val lutConfig = GemminiLUTConfig()
//   val outputnumLanes = 32
//   val sp_bank_entries = 256
//   val sp_banks = 4
//   val sp_width = 192
//   val sp_width_projected = 128
//   val lut_update_regularity_w = 128
//   val lut_update_regularity_act_in = 128
//   val lut_update_regularity_act_out = 128
//   val iterator_bitwidth = 16
//   val rdataWidth = lutConfig.rdataWidth
//   val raddrWidth = lutConfig.raddrWidth

//   def createLutWriteData(baseValue: Int): Seq[BigInt] = {
//     (0 until 32).map { lane =>
//       (0 until 16).reverse.foldLeft(BigInt(0)) { (acc, entry) =>
//         (acc << rdataWidth) | BigInt((baseValue + lane + entry) % 63)
//       }
//     }
//   }
  
//   def initSignals(dut: QuantLut): Unit = {
//     dut.io.lut_write_act_in.valid.poke(false.B)
//     dut.io.lut_write_weight.valid.poke(false.B)
//     dut.io.lut_write_act_out.valid.poke(false.B)
//     dut.io.quant_fp6.valid.poke(false.B)
//     dut.io.a_fire.poke(false.B)
//     dut.io.b_fire.poke(false.B)
//     dut.io.counter_i.poke(0.U)
//     dut.io.counter_j.poke(0.U)
    
//     for (i <- 0 until outputnumLanes) {
//       dut.io.quant_fp6.bits(i).poke(0.U)
//     }
    
//     for (bank <- 0 until sp_banks) {
//       dut.io.spad_projected_data(bank).resp.valid.poke(false.B)
//       dut.io.spad_projected_data(bank).resp.bits.data.poke(0.U)
//       dut.io.spad_projected_data(bank).resp.bits.fromDMA.poke(false.B)
//       dut.io.spad_deprojected_data(bank).resp.ready.poke(false.B)
//     }
//   }

//   def testDeprojection(dut: QuantLut, testNum: Int, counterValue: Int): Unit = {
//     println(s"\n  Testing deprojection #$testNum (counter_i=$counterValue)...")
//     dut.io.a_fire.poke(true.B)
    
//     for (bank <- 0 until sp_banks) {
//       dut.io.spad_projected_data(bank).resp.valid.poke(true.B)
//       val projectedData = (0 until 32).reverse.map(k => BigInt((k + testNum) % 16))
//                             .foldLeft(BigInt(0))((acc, v) => (acc << 4) | v)
//       dut.io.spad_projected_data(bank).resp.bits.data.poke(projectedData.U)
//       dut.io.spad_deprojected_data(bank).resp.ready.poke(true.B)
//     }
    
//     dut.clock.step(1)
//     dut.io.spad_deprojected_data(0).resp.valid.expect(true.B)
//     val deprojData = dut.io.spad_deprojected_data(0).resp.bits.data.peek().litValue
//     println(s"    ✓ Deprojection successful, data: 0x${deprojData.toString(16).take(16)}...")
    
//     dut.io.a_fire.poke(false.B)
//     for (bank <- 0 until sp_banks) {
//       dut.io.spad_projected_data(bank).resp.valid.poke(false.B)
//     }
//     dut.clock.step(1)
//   }
  
//   behavior of "QuantLut act_in Double Buffer"
  
//   it should "test 8 writes with counter incrementing every 8 cycles by 32" in {
//     test(new QuantLut(lutConfig, outputnumLanes,
//                       sp_bank_entries, sp_banks, sp_width, sp_width_projected,
//                       lut_update_regularity_w, lut_update_regularity_act_in, 
//                       lut_update_regularity_act_out, iterator_bitwidth))
//       .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
//       initSignals(dut)
//       println("\n" + "="*60)
//       println("Testing act_in Double Buffer - 8 Writes with Counter Control")
//       println("Counter increments by 32 every 4 cycles")
//       println("="*60)
      
//       var currentCounterI = 0
//       var currentCounterJ = 0
//       var cycleCount = 0
//       var successfulWrites = 0
      
//       for (writeNum <- 0 until 8) {
//         println(s"\n>>> Write Operation #${writeNum + 1} <<<")
//         println(s"Current counter_i: $currentCounterI, counter_j: $currentCounterJ, cycle: $cycleCount")
        
//         var writeSuccess = false
//         var attempts = 0
//         val maxAttempts = 200
        
//         while (!writeSuccess && attempts < maxAttempts) {
//           val ready = dut.io.lut_write_act_in.ready.peek().litToBoolean
          
//           if (ready) {
//             println(s"  lut_write_act_in.ready: true (attempt ${attempts + 1})")
//             val writeData = createLutWriteData(writeNum * 10)
//             dut.io.lut_write_act_in.valid.poke(true.B)
//             for (lane <- 0 until 32) {
//               dut.io.lut_write_act_in.bits.data(lane).poke(writeData(lane).U)
//             }
//             dut.clock.step(1)
//             cycleCount += 1
//             dut.io.lut_write_act_in.valid.poke(false.B)
//             println(s"  ✓ Write #${writeNum + 1} completed with base value ${writeNum * 10}")
//             successfulWrites += 1
//             writeSuccess = true
            
//             dut.clock.step(1)
//             cycleCount += 1
            
//             testDeprojection(dut, writeNum + 1, currentCounterI)
//             cycleCount += 2
//           } else {
//             if (attempts == 0) {
//               println(s"  ⚠ Write blocked - both buffers busy")
//               println(s"  Waiting for buffer toggle...")
//             }
            
//             if ((cycleCount % 4) == 0 && cycleCount > 0) {
//               currentCounterI += 32
//               currentCounterJ += 32
//               println(s"    Cycle $cycleCount: counter_i=$currentCounterI, counter_j=$currentCounterJ")
//             }
            
//             dut.io.counter_i.poke(currentCounterI.U)
//             dut.io.counter_j.poke(currentCounterJ.U)
//             dut.clock.step(1)
//             cycleCount += 1
//             attempts += 1
//           }
//         }
        
//         if (!writeSuccess) {
//           println(s"  ✗ ERROR: Write #${writeNum + 1} failed after $maxAttempts attempts")
//         }
//       }
      
//       println("\n" + "="*60)
//       println(s"Write Summary: $successfulWrites/8 writes completed successfully")
//       println(s"Total cycles: $cycleCount")
//       println(s"Final counter_i: $currentCounterI")
//       println(s"Final counter_j: $currentCounterJ")
//       println("="*60 + "\n")
      
//       assert(successfulWrites == 8, s"Expected 8 successful writes, got $successfulWrites")
//     }
//   }
// }
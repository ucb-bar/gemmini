package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ScalingFactorMemSpec extends AnyFlatSpec with ChiselScalatestTester {
  
  behavior of "ScalingFactorMem"
  
  it should "work correctly in FP8 mode" in {
    test(new ScalingFactorMem(
      depth = 256,
      bankWidth = 128,
      actOutputScalingWidth = 8,
      numBanks = 8,
      testConfig = false
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.dataType.poke(2.U)
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_req.bits.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      dut.clock.step(5)
      
      // Write activation scales to bank 0 (addr[9:8] = 0)
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0x000.U) // bank_sel = 0, row_addr = 0
      val actScales = (0 until 16).map(i => BigInt((0x7E + i) & 0xFF))
      val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (scale, i)) =>
        acc | (scale << (i * 8))
      }
      dut.io.write.bits.data.poke(actData.U)
      dut.clock.step(1)
      
      // Write weight scales to bank 2 (addr[9:8] = 2)
      dut.io.write.bits.addr.poke(0x200.U) // bank_sel = 2, row_addr = 0
      val weightScales = (0 until 16).map(i => BigInt((0x70 + i) & 0xFF))
      val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (scale, i)) =>
        acc | (scale << (i * 8))
      }
      dut.io.write.bits.data.poke(weightData.U)
      dut.clock.step(1)
      
      dut.io.write.valid.poke(false.B)
      dut.clock.step(2)
      
      // Read request
      dut.io.read_req.valid.poke(true.B)
      dut.io.read_req.bits.scaling_enable.poke(true.B)
      dut.io.read_req.bits.addr.poke(0x000.U) // read from bank 0
      dut.clock.step(1)
      
      dut.io.read_req.valid.poke(false.B)
      
      // Wait for response
      var cycleCount = 0
      var validCount = 0
      while (cycleCount < 30 && validCount < 16) {
        if (dut.io.read_resp.valid.peek().litToBoolean) {
          println(s"Cycle $cycleCount - FP8 Mode Response $validCount:")
          for (i <- 0 until 16) {
            val scale = dut.io.read_resp.bits.combined_scales(i).peek().litValue
            println(f"  combined_scales($i) = 0x$scale%X")
          }
          validCount += 1
        }
        dut.clock.step(1)
        cycleCount += 1
      }
      
      println(s"FP8 Mode: Received $validCount valid responses")
    }
  }
  
  it should "work correctly in non-FP8 mode (FP4/FP6)" in {
    test(new ScalingFactorMem(
      depth = 256,
      bankWidth = 128,
      actOutputScalingWidth = 8,
      numBanks = 8,
      testConfig = false
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.dataType.poke(0.U) // non-FP8 mode
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_req.bits.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      dut.clock.step(5)
      
      // Write activation scales to banks 0,1,2,3
      for (bankSel <- 0 until 4) {
        dut.io.write.valid.poke(true.B)
        dut.io.write.bits.addr.poke((bankSel << 8).U)
        val scales = (0 until 16).map(i => BigInt((0x60 + bankSel * 16 + i) & 0xFF))
        val data = scales.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (scale, i)) =>
          acc | (scale << (i * 8))
        }
        dut.io.write.bits.data.poke(data.U)
        dut.clock.step(1)
      }
      
      // Write weight scales to banks 4,5,6,7
      for (bankSel <- 0 until 4) {
        dut.io.write.bits.addr.poke(((bankSel + 2) << 8).U) // bank_sel 2,3,4,5 maps to banks 4,5,6,7
        val scales = (0 until 16).map(i => BigInt((0x50 + bankSel * 16 + i) & 0xFF))
        val data = scales.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (scale, i)) =>
          acc | (scale << (i * 8))
        }
        dut.io.write.bits.data.poke(data.U)
        dut.clock.step(1)
      }
      
      dut.io.write.valid.poke(false.B)
      dut.clock.step(2)
      
      // Read request
      dut.io.read_req.valid.poke(true.B)
      dut.io.read_req.bits.scaling_enable.poke(true.B)
      dut.io.read_req.bits.addr.poke(0x000.U)
      dut.clock.step(1)
      
      dut.io.read_req.valid.poke(false.B)
      
      // Wait for response (should get 64 responses in non-FP8 mode)
      var cycleCount = 0
      var validCount = 0
      while (cycleCount < 80 && validCount < 64) {
        if (dut.io.read_resp.valid.peek().litToBoolean) {
          println(s"Cycle $cycleCount - Non-FP8 Mode Response $validCount:")
          for (i <- 0 until 64) {
            val scale = dut.io.read_resp.bits.combined_scales(i).peek().litValue
            print(f"0x$scale%03X ")
            if ((i + 1) % 8 == 0) println()
          }
          validCount += 1
        }
        dut.clock.step(1)
        cycleCount += 1
      }
      
      println(s"Non-FP8 Mode: Received $validCount valid responses (expected 64)")
    }
  }
  
  it should "handle write and read back correctly" in {
    test(new ScalingFactorMem(
      depth = 256,
      bankWidth = 128,
      actOutputScalingWidth = 8,
      numBanks = 8,
      testConfig = false
    )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.dataType.poke(2.U) 
      dut.io.read_req.valid.poke(false.B)
      dut.io.read_req.bits.scaling_enable.poke(false.B)
      dut.io.write.valid.poke(false.B)
      dut.io.read_resp.ready.poke(true.B)
      dut.clock.step(3)
      
      // Write known pattern
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0x000.U)
      dut.io.write.bits.data.poke(BigInt("0F0E0D0C0B0A09080706050403020100", 16).U)
      dut.clock.step(1)
      
      dut.io.write.bits.addr.poke(0x200.U)
      dut.io.write.bits.data.poke(BigInt("1F1E1D1C1B1A19181716151413121110", 16).U)
      dut.clock.step(1)
      
      dut.io.write.valid.poke(false.B)
      dut.clock.step(3)
      
      // Read back
      dut.io.read_req.valid.poke(true.B)
      dut.io.read_req.bits.scaling_enable.poke(true.B)
      dut.io.read_req.bits.addr.poke(0x000.U)
      dut.clock.step(1)
      dut.io.read_req.valid.poke(false.B)
      
      // Check responses
      for (cycle <- 0 until 30) {
        if (dut.io.read_resp.valid.peek().litToBoolean) {
          println(s"Read response at cycle $cycle")
        }
        dut.clock.step(1)
      }
    }
  }
}
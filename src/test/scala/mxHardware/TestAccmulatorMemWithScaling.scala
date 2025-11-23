package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.simulator.WriteVcdAnnotation

class AccumulatorMemWithScalingTest extends AnyFlatSpec with ChiselScalatestTester {
  
  val meshRows = 16
  val meshColumns = 16
  val dataType = UInt(16.W)
  val scaleType = UInt(8.W)
  val accDepth = 256
  
  behavior of "AccumulatorMem with MX Scaling"

  def createTestDUT() = {
    new AccumulatorMem(
      n = accDepth,
      t = Vec(meshRows, Vec(meshColumns, dataType)),
      scale_func = (x: UInt, y: UInt) => x,
      scale_t = scaleType,
      acc_singleported = false,
      acc_sub_banks = 1,
      use_shared_ext_mem = false,
      use_tl_ext_ram = false,
      acc_latency = 2,
      acc_type = dataType,
      is_dummy = false,
      use_mx_scaling = true,
      scale_mem = Some(GemminiScalingFactorMemConfig(
      baseAddr = 0x80000000L,
      sizeInBytes = 32 << 10,
      sramLineSizeInBytes = 32,
      numBanks = 4
    ))
    )
  }

  it should "write scales to ScaleFactorMem independently" in {
    test(createTestDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Initialize signals
      dut.io.write.valid.poke(false.B)
      dut.io.read.req.valid.poke(false.B)
      dut.io.read.resp.ready.poke(true.B)
      
      // FIX: Poke adder.sum properly
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.adder.sum(i)(j).poke(0.U)
        }
      }
      
      dut.clock.step(5)

      println("=== Test 1: Write Scales to ScaleFactorMem ===")
      
      println("\nWriting activation scales...")
      val actScales = Seq.fill(32)(127)
      val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { 
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      
      dut.io.scale_mem_write.get.valid.poke(true.B)
      dut.io.scale_mem_write.get.bits.addr.poke(0.U)
      dut.io.scale_mem_write.get.bits.data.poke(actData.U)
      dut.clock.step(1)
      
      assert(dut.io.scale_mem_write.get.ready.peek().litToBoolean, 
        "ScaleFactorMem should be ready for write")
      
      dut.io.scale_mem_write.get.valid.poke(false.B)
      dut.clock.step(2)
      
      println("Activation scales written successfully")
      
      println("\nWriting weight scales...")
      val weightScales = (0 until 32).map(i => 128 + i)
      val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) {
        case (acc, (value, idx)) => acc | (BigInt(value) << (idx * 8))
      }
      
      dut.io.scale_mem_write.get.valid.poke(true.B)
      dut.io.scale_mem_write.get.bits.addr.poke(1.U)
      dut.io.scale_mem_write.get.bits.data.poke(weightData.U)
      dut.clock.step(1)
      
      assert(dut.io.scale_mem_write.get.ready.peek().litToBoolean, 
        "ScaleFactorMem should be ready for write")
      
      dut.io.scale_mem_write.get.valid.poke(false.B)
      dut.clock.step(2)
      
      println("Weight scales written successfully")
      println("Test 1 passed: Scales written independently\n")
    }
  }

  it should "read scales and apply them to accumulator write data" in {
    test(createTestDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.write.valid.poke(false.B)
      dut.io.read.req.valid.poke(false.B)
      dut.io.read.resp.ready.poke(true.B)
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.adder.sum(i)(j).poke(0.U)
        }
      }
      
      dut.clock.step(5)

      println("=== Test 2: Read Scales and Apply to Write Data ===")
      
      println("\nStep 1: Writing scales to ScaleFactorMem...")
      val actScales = Seq.fill(32)(127)
      val actData = actScales.zipWithIndex.foldLeft(BigInt(0)) { 
        case (acc, (v, i)) => acc | (BigInt(v) << (i * 8))
      }
      
      dut.io.scale_mem_write.get.valid.poke(true.B)
      dut.io.scale_mem_write.get.bits.addr.poke(0.U)
      dut.io.scale_mem_write.get.bits.data.poke(actData.U)
      dut.clock.step(1)
      dut.io.scale_mem_write.get.valid.poke(false.B)
      dut.clock.step(2)
      
      val weightScales = (0 until 32).map(i => 128 + i)
      val weightData = weightScales.zipWithIndex.foldLeft(BigInt(0)) {
        case (acc, (v, i)) => acc | (BigInt(v) << (i * 8))
      }
      
      dut.io.scale_mem_write.get.valid.poke(true.B)
      dut.io.scale_mem_write.get.bits.addr.poke(1.U)
      dut.io.scale_mem_write.get.bits.data.poke(weightData.U)
      dut.clock.step(1)
      dut.io.scale_mem_write.get.valid.poke(false.B)
      dut.clock.step(5)
      
      println("Scales written")
      
      println("\nStep 2: Writing data to AccumulatorMem...")
      
      val testData = Seq.fill(meshRows)(Seq.fill(meshColumns)(100))
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(0.U)
      dut.io.write.bits.acc.poke(false.B)
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.write.bits.data(i)(j).poke(testData(i)(j).U)
        }
      }
      
      val maskBits = (meshRows * meshColumns * 16) / 8
      for (i <- 0 until maskBits) {
        dut.io.write.bits.mask(i).poke(true.B)
      }
      
      dut.clock.step(1)
      
      assert(dut.io.write.ready.peek().litToBoolean, 
        "AccumulatorMem should accept write")
      
      dut.io.write.valid.poke(false.B)
      
      println("Write request issued, waiting for scaling...")
      dut.clock.step(10)
      
      println("\nStep 3: Reading back data to verify scaling...")
      
      dut.io.read.req.valid.poke(true.B)
      dut.io.read.req.bits.addr.poke(0.U)
      dut.io.read.req.bits.fromDMA.poke(false.B)
      dut.io.read.req.bits.full.poke(true.B)
      dut.io.read.req.bits.scale.poke(0.U)
      dut.io.read.req.bits.igelu_qb.poke(0.U)
      dut.io.read.req.bits.igelu_qc.poke(0.U)
      dut.io.read.req.bits.iexp_qln2.poke(0.U)
      dut.io.read.req.bits.iexp_qln2_inv.poke(0.U)
      dut.io.read.req.bits.act.poke(0.U)
      
      dut.clock.step(1)
      dut.io.read.req.valid.poke(false.B)
      
      var gotResponse = false
      for (_ <- 0 until 20) {
        if (dut.io.read.resp.valid.peek().litToBoolean && !gotResponse) {
          gotResponse = true
          println("Read response received")
          
          val val00 = dut.io.read.resp.bits.data(0)(0).peek().litValue.toInt
          val val55 = dut.io.read.resp.bits.data(5)(5).peek().litValue.toInt
          
          println(f"Data[0][0] = $val00 (scaled from 100)")
          println(f"Data[5][5] = $val55 (scaled from 100)")
        }
        dut.clock.step(1)
      }
      
      assert(gotResponse, "Should receive read response")
      println("Test 2 passed\n")
    }
  }

  it should "not apply scaling to accumulate operations" in {
    test(createTestDUT()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      dut.io.write.valid.poke(false.B)
      dut.io.read.req.valid.poke(false.B)
      dut.io.read.resp.ready.poke(true.B)
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.adder.sum(i)(j).poke(0.U)
        }
      }
      
      dut.clock.step(5)

      println("=== Test 3: Accumulate Operations Bypass Scaling ===")
      
      println("\nStep 1: Writing initial data...")
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(10.U)
      dut.io.write.bits.acc.poke(false.B)
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.write.bits.data(i)(j).poke(50.U)
        }
      }
      
      val maskBits = (meshRows * meshColumns * 16) / 8
      for (i <- 0 until maskBits) {
        dut.io.write.bits.mask(i).poke(true.B)
      }
      
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      dut.clock.step(5)
      
      println("\nStep 2: Accumulating (bypass scaling)...")
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.adder.sum(i)(j).poke(100.U)
        }
      }
      
      dut.io.write.valid.poke(true.B)
      dut.io.write.bits.addr.poke(10.U)
      dut.io.write.bits.acc.poke(true.B)
      
      for (i <- 0 until meshRows) {
        for (j <- 0 until meshColumns) {
          dut.io.write.bits.data(i)(j).poke(50.U)
        }
      }
      
      dut.clock.step(1)
      dut.io.write.valid.poke(false.B)
      dut.clock.step(5)
      
      println("\nStep 3: Reading accumulated result...")
      
      dut.io.read.req.valid.poke(true.B)
      dut.io.read.req.bits.addr.poke(10.U)
      dut.io.read.req.bits.fromDMA.poke(false.B)
      dut.io.read.req.bits.full.poke(true.B)
      dut.io.read.req.bits.scale.poke(0.U)
      dut.io.read.req.bits.igelu_qb.poke(0.U)
      dut.io.read.req.bits.igelu_qc.poke(0.U)
      dut.io.read.req.bits.iexp_qln2.poke(0.U)
      dut.io.read.req.bits.iexp_qln2_inv.poke(0.U)
      dut.io.read.req.bits.act.poke(0.U)
      
      dut.clock.step(1)
      dut.io.read.req.valid.poke(false.B)
      
      var gotResponse = false
      for (_ <- 0 until 20) {
        if (dut.io.read.resp.valid.peek().litToBoolean && !gotResponse) {
          gotResponse = true
          
          val val00 = dut.io.read.resp.bits.data(0)(0).peek().litValue.toInt
          println(f"Accumulated[0][0] = $val00 (should be 100)")
          
          assert(val00 == 100, f"Expected 100, got $val00")
        }
        dut.clock.step(1)
      }
      
      assert(gotResponse, "Should receive read response")
      println("Test 3 passed\n")
    }
  }
}
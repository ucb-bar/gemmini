// package gemmini

// import chisel3._
// import chisel3.util._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import chiseltest.simulator.WriteVcdAnnotation

// class AccumulatorMemScalingTest extends AnyFlatSpec with ChiselScalatestTester {
//   it should "correctly apply element-wise scaling and accumulate" in {
//     test(new AccumulatorMemTestWrapper).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
//       println("=" * 80)
//       println("Test: Element-wise scaling and accumulation (16 columns x 1 row)")
//       println("=" * 80)
      
//       // Initialize
//       dut.io.dataType.poke(0.U)  // FP8 mode for MX scaling
//       dut.io.write.valid.poke(false.B)
//       dut.io.read_req.valid.poke(false.B)
//       dut.io.scale_write.valid.poke(false.B)
//       dut.clock.step(5)
      
//       val test_addr = 0  // Test address in accumulator
//       val meshColumns = 16  // Outer loop - 16 columns
//       val meshRows = 1      // Inner loop - 1 row
      
//       // ========================================================================
//       // Step 1: Write scales to ScalingFactorMem
//       // ========================================================================
//       println("\n[Step 1] Writing scales to ScalingFactorMem...")
      
//       // Activation scale: [124] (slightly below neutral 127)
//       val act_scales = Seq(124)
//       val act_data = BigInt(124)
      
//       dut.io.scale_write.valid.poke(true.B)
//       dut.io.scale_write.bits.addr.poke(0.U)  // bank_sel=0 (activation)
//       dut.io.scale_write.bits.data.poke(act_data.U)
//       dut.clock.step(1)
//       dut.io.scale_write.valid.poke(false.B)
//       dut.clock.step(2)
      
//       println(s"  Wrote activation scales: ${act_scales.mkString(", ")}")
      
//       // Weight scales: [120, 121, 122, ..., 135] for 16 columns (around neutral)
//       val weight_scales = (0 until meshColumns).map(i => 120 + i)
//       val weight_data = weight_scales.zipWithIndex.map { case (v, i) => 
//         BigInt(v) << (i * 8) 
//       }.reduce(_ | _)
      
//       dut.io.scale_write.valid.poke(true.B)
//       dut.io.scale_write.bits.addr.poke(1.U)  // bank_sel=1 (weight)
//       dut.io.scale_write.bits.data.poke(weight_data.U)
//       dut.clock.step(1)
//       dut.io.scale_write.valid.poke(false.B)
//       dut.clock.step(2)
      
//       println(s"  Wrote weight scales: ${weight_scales.mkString(", ")}")
      
//       // Calculate expected combined scales (E8M0 addition)
//       val combined_scales = weight_scales.map { w =>
//         (act_scales(0) + w) & 0x1FF
//       }
//       println(s"  Expected combined scales: ${combined_scales.mkString(", ")}")
//       println(s"  Scale offsets from 254: ${combined_scales.map(s => s - 254).mkString(", ")}")
      
//       // ========================================================================
//       // Step 2: Write first batch of data (BF16 value = 512.0)
//       // ========================================================================
//       println("\n[Step 2] Writing first batch of BF16 data (value=512.0)...")
      
//       val first_batch_value = 512.0  // Floating point value
      
//       dut.io.write.valid.poke(true.B)
//       dut.io.write.bits.addr.poke(test_addr.U)
//       dut.io.write.bits.acc.poke(false.B)  // Not accumulating
      
//       // data(col)(row) - 16 columns, 1 row each
//       for (col <- 0 until meshColumns) {
//         val bf16_value = floatToBF16(first_batch_value)
//         dut.io.write.bits.data(col)(0).poke(bf16_value)
//       }
      
//       // Set all mask bits to true
//       for (i <- 0 until dut.io.write.bits.mask.length) {
//         dut.io.write.bits.mask(i).poke(true.B)
//       }
      
//       dut.clock.step(1)
//       dut.io.write.valid.poke(false.B)
//       println(s"  Wrote first batch: value=$first_batch_value (BF16) for all 16 columns")
      
//       // Wait for write to complete and scaling to be applied
//       dut.clock.step(10)
      
//       // ========================================================================
//       // Step 3: Read back first batch and verify scaling
//       // ========================================================================
//       println("\n[Step 3] Reading back first batch...")
    
//       dut.io.read_req.valid.poke(true.B)
//       dut.io.read_req.bits.addr.poke(test_addr.U)
//       dut.io.read_req.bits.fromDMA.poke(false.B)
//       dut.io.read_req.bits.full.poke(false.B)
//       dut.io.read_req.bits.act.poke(0.U)
//       dut.io.read_req.bits.scale.poke(0.U)
//       dut.io.read_req.bits.igelu_qb.poke(0)
//       dut.io.read_req.bits.igelu_qc.poke(0)
//       dut.io.read_req.bits.iexp_qln2.poke(0)
//       dut.io.read_req.bits.iexp_qln2_inv.poke(0)
//       dut.clock.step(1)
//       dut.io.read_req.valid.poke(false.B)
      
//       // Wait for read response
//       var timeout = 0
//       while (!dut.io.read_resp.valid.peek().litToBoolean && timeout < 30) {
//         dut.clock.step(1)
//         timeout += 1
//       }
      
//       if (dut.io.read_resp.valid.peek().litToBoolean) {
//         println("  ✓ Read response valid!")
//         dut.io.read_resp.ready.poke(true.B)
        
//         println("\n  Verifying scaled values:")
//         var all_correct = true
        
//         for (col <- 0 until meshColumns) {
//           val actual_bits = dut.io.read_resp.bits.data(col)(0).peek().litValue
//           val actual_bf16 = bf16ToFloat(actual_bits.toInt & 0xFFFF)
//           val scale = combined_scales(col)
//           val expected_bf16 = applyE9M0ScaleSoftwareBF16(first_batch_value, scale)
          
//           val scale_offset = scale - 254
//           val matches = Math.abs(actual_bf16 - expected_bf16) < 0.1 || 
//                        (actual_bf16 == 0.0 && expected_bf16 == 0.0)
          
//           println(f"    Col $col%2d: value=$first_batch_value%.1f, scale=$scale%3d (offset=$scale_offset%+3d), " +
//                  f"expected=$expected_bf16%.2f, actual=$actual_bf16%.2f ${if(matches) "✓" else "✗"}")
          
//           if (!matches) {
//             all_correct = false
//           }
//         }
        
//         if (all_correct) {
//           println(s"  ✓✓✓ All ${meshColumns} columns first batch scaled values correct!")
//         }
        
//         dut.clock.step(1)
//         dut.io.read_resp.ready.poke(false.B)
//       } else {
//         println(s"  ✗ Read response timeout after $timeout cycles")
//       }
      
//       dut.clock.step(1)
      
//       // ========================================================================
//       // Step 4: Write second batch with accumulation (BF16 value = 256.0)
//       // ========================================================================
//       println("\n[Step 4] Writing second batch with accumulation (value=256.0)...")
      
//       val second_batch_value = 256.0  // Half of first batch
      
//       dut.io.write.valid.poke(true.B)
//       dut.io.write.bits.addr.poke(1.U)
//       dut.io.write.bits.acc.poke(true.B)  // Enable accumulation
      
//       for (col <- 0 until meshColumns) {
//         val bf16_value = floatToBF16(second_batch_value)
//         dut.io.write.bits.data(col)(0).poke(bf16_value)
//       }
      
//       for (i <- 0 until dut.io.write.bits.mask.length) {
//         dut.io.write.bits.mask(i).poke(true.B)
//       }
      
//       dut.clock.step(1)
//       dut.io.write.valid.poke(false.B)
//       println(s"  Wrote second batch: value=$second_batch_value (BF16) with acc=true")
      
//       // Wait for accumulation to complete
//       dut.clock.step(15)
      
//       // ========================================================================
//       // Step 5: Read accumulated result and verify
//       // ========================================================================
//       println("\n[Step 5] Reading accumulated result...")

//       dut.io.read_req.valid.poke(true.B)
//       dut.io.read_req.bits.addr.poke(test_addr.U)
//       dut.io.read_req.bits.fromDMA.poke(false.B)
//       dut.io.read_req.bits.full.poke(false.B)
//       dut.io.read_req.bits.act.poke(0.U)
//       dut.io.read_req.bits.scale.poke(0.U)
//       dut.io.read_req.bits.igelu_qb.poke(0)
//       dut.io.read_req.bits.igelu_qc.poke(0)
//       dut.io.read_req.bits.iexp_qln2.poke(0)
//       dut.io.read_req.bits.iexp_qln2_inv.poke(0)
//       dut.clock.step(1)
//       dut.io.read_req.valid.poke(false.B)
      
//       timeout = 0
//       while (!dut.io.read_resp.valid.peek().litToBoolean && timeout < 30) {
//         dut.clock.step(1)
//         timeout += 1
//       }
      
//       if (dut.io.read_resp.valid.peek().litToBoolean) {
//         println("  ✓ Accumulated result valid!")
//         dut.io.read_resp.ready.poke(true.B)
        
//         println("\n  Verifying accumulated values:")
//         var all_correct = true
        
//         for (col <- 0 until meshColumns) {
//           val actual_bits = dut.io.read_resp.bits.data(col)(0).peek().litValue
//           val actual_bf16 = bf16ToFloat(actual_bits.toInt & 0xFFFF)
//           val scale = combined_scales(col)
          
//           // Expected: scaled(512) + scaled(256)
//           val first_scaled = applyE9M0ScaleSoftwareBF16(first_batch_value, scale)
//           val second_scaled = applyE9M0ScaleSoftwareBF16(second_batch_value, scale)
//           val expected_bf16 = first_scaled + second_scaled
          
//           val matches = Math.abs(actual_bf16 - expected_bf16) < 0.1 || 
//                        (actual_bf16 == 0.0 && expected_bf16 == 0.0)
          
//           val scale_offset = scale - 254
//           println(f"    Col $col%2d: scale=$scale%3d (offset=$scale_offset%+3d)")
//           println(f"             first:  $first_batch_value%.1f → $first_scaled%.2f")
//           println(f"             second: $second_batch_value%.1f → $second_scaled%.2f")
//           println(f"             sum:    $expected_bf16%.2f, actual: $actual_bf16%.2f ${if(matches) "✓" else "✗"}")
          
//           if (!matches) {
//             all_correct = false
//           }
//         }
        
//         if (all_correct) {
//           println(s"\n  ✓✓✓ ALL ${meshColumns} COLUMNS ACCUMULATED VALUES CORRECT! ✓✓✓")
//         } else {
//           println(s"\n  ✗✗✗ Some accumulated values incorrect ✗✗✗")
//         }
        
//         dut.clock.step(1)
//         dut.io.read_resp.ready.poke(false.B)
//       } else {
//         println(s"  ✗ Read response timeout after $timeout cycles")
//       }
      
//       dut.clock.step(10)
      
//       println("\n" + "=" * 80)
//       println("Test completed!")
//       println("=" * 80)
//     }
//   }
  
//   // Helper: Convert float to BF16 (Brain Floating Point 16)
//   // BF16: 1 sign bit, 8 exponent bits, 7 mantissa bits
//   def floatToBF16(value: Double): BigInt = {
//     if (value == 0.0) return BigInt(0)
    
//     // Convert to Float32 bit representation
//     val floatBits = java.lang.Float.floatToIntBits(value.toFloat)
    
//     // BF16 is simply the upper 16 bits of Float32
//     val bf16 = (floatBits >> 16) & 0xFFFF
    
//     BigInt(bf16)
//   }
  
//   // Helper: Convert BF16 to float
//   def bf16ToFloat(bf16: Int): Double = {
//     // BF16 is the upper 16 bits of Float32, so shift left and fill lower bits with 0
//     val floatBits = (bf16 & 0xFFFF) << 16
    
//     java.lang.Float.intBitsToFloat(floatBits).toDouble
//   }
  
//   // // Software reference for E9M0 scaling on BF16 values
//   //   def applyE9M0ScaleSoftwareBF16(value: Double, scale_e9m0: Int): Double = {
//   //     val bf16_bits = floatToBF16(value).toInt

//   //     val sign = (bf16_bits >> 15) & 1
//   //     val exp = (bf16_bits >> 7) & 0xFF  // 8-bit exponent
//   //     val mant = bf16_bits & 0x7F        // 7-bit mantissa
//   //     println(f"  [applyE9M0Scale] Input:")
//   //     println(f"    value = $value%.2f")
//   //     println(f"    bf16_bits = 0x${bf16_bits}%04X = ${toBinaryString(bf16_bits, 16)}")
//   //     println(f"    sign = $sign (bit ${toBinaryString(sign, 1)})")
//   //     println(f"    exp  = $exp%3d (0x${exp}%02X = ${toBinaryString(exp, 8)})")
//   //     println(f"    mant = $mant%3d (0x${mant}%02X = ${toBinaryString(mant, 7)})")

//   //     // Handle zero
//   //     if (exp == 0 && mant == 0) return 0.0

//   //     // Handle infinity and NaN
//   //     if (exp == 255) {
//   //       if (mant == 0) return if (sign == 1) Double.NegativeInfinity else Double.PositiveInfinity
//   //       return Double.NaN
//   //     }

//   //     // E9M0: combined_scale = act_scale + weight_scale
//   //     // E8M0 neutral point is 127, so combined neutral is 127 + 127 = 254
//   //     // Effective scaling = 2^(combined_scale - 254)
//   //     val scaleOffset = scale_e9m0 - 254
//   //     val newExp = exp + scaleOffset

//   //     // Clamp exponent to BF16 range
//   //     val clampedExp = newExp.max(0).min(255)

//   //     // Handle overflow to infinity
//   //     if (clampedExp >= 255) {
//   //       return if (sign == 1) Double.NegativeInfinity else Double.PositiveInfinity
//   //     }

//   //     // Handle underflow to zero
//   //     if (clampedExp == 0) {
//   //       return 0.0
//   //     }

//   //     // Reconstruct BF16
//   //     val scaled_bf16 = (sign << 15) | (clampedExp << 7) | mant
//   //     println(f"  [applyE9M0Scale] Reconstruct BF16:")
//   //     println(f"    sign << 15   = 0x${sign_shifted}%04X = ${toBinaryString(sign_shifted, 16)}")
//   //     println(f"    exp  << 7    = 0x${exp_shifted}%04X = ${toBinaryString(exp_shifted, 16)}")
//   //     println(f"    mant         = 0x${mant}%04X = ${toBinaryString(mant, 16)}")
//   //     println(f"    scaled_bf16  = 0x${scaled_bf16}%04X = ${toBinaryString(scaled_bf16, 16)}")
//   //     println(f"                 = (${toBinaryString(sign, 1)}) | (${toBinaryString(clampedExp, 8)}) | (${toBinaryString(mant, 7)})")

//   //     // Convert back to float
//   //     bf16ToFloat(scaled_bf16)
//   //   }
//   def applyE9M0ScaleSoftwareBF16(value: Double, scale_e9m0: Int): Double = {
//     val bf16_bits = floatToBF16(value).toInt

//     val sign = (bf16_bits >> 15) & 1
//     val exp = (bf16_bits >> 7) & 0xFF  // 8-bit exponent
//     val mant = bf16_bits & 0x7F        // 7-bit mantissa

//     // println(f"  [applyE9M0Scale] Input:")
//     // println(f"    value = $value%.2f")
//     // println(f"    bf16_bits = 0x${bf16_bits}%04X = ${toBinaryString(bf16_bits, 16)}")
//     // println(f"    sign = $sign (bit ${toBinaryString(sign, 1)})")
//     // println(f"    exp  = $exp%3d (0x${exp}%02X = ${toBinaryString(exp, 8)})")
//     // println(f"    mant = $mant%3d (0x${mant}%02X = ${toBinaryString(mant, 7)})")

//     // Handle zero
//     if (exp == 0 && mant == 0) {
//       println(f"    Result: zero")
//       return 0.0
//     }

//     // Handle infinity and NaN
//     if (exp == 255) {
//       if (mant == 0) {
//         println(f"    Result: infinity (sign=$sign)")
//         return if (sign == 1) Double.NegativeInfinity else Double.PositiveInfinity
//       }
//       println(f"    Result: NaN")
//       return Double.NaN
//     }

//     // E9M0: combined_scale = act_scale + weight_scale
//     // E8M0 neutral point is 127, so combined neutral is 127 + 127 = 254
//     // Effective scaling = 2^(combined_scale - 254)
//     val scaleOffset = scale_e9m0 - 254
//     val newExp = exp + scaleOffset

//     // println(f"  [applyE9M0Scale] Scaling:")
//     // println(f"    scale_e9m0   = $scale_e9m0%3d (0x${scale_e9m0}%03X = ${toBinaryString(scale_e9m0, 9)})")
//     // println(f"    scaleOffset  = $scaleOffset%+4d (scale - 254)")
//     // println(f"    newExp       = $newExp%4d (exp + scaleOffset = $exp + $scaleOffset)")

//     // Clamp exponent to BF16 range
//     val clampedExp = newExp.max(0).min(255)

//     //println(f"    clampedExp   = $clampedExp%3d (clamped to [0, 255])")

//     // Handle overflow to infinity
//     if (clampedExp >= 255) {
//      // println(f"    Result: overflow to infinity (sign=$sign)")
//       return if (sign == 1) Double.NegativeInfinity else Double.PositiveInfinity
//     }

//     // Handle underflow to zero
//     if (clampedExp == 0) {
//      // println(f"    Result: underflow to zero")
//       return 0.0
//     }

//     // Reconstruct BF16
//     val sign_shifted = sign << 15
//     val exp_shifted = clampedExp << 7
//     val scaled_bf16 = sign_shifted | exp_shifted | mant

//     // println(f"  [applyE9M0Scale] Reconstruct BF16:")
//     // println(f"    sign << 15   = 0x${sign_shifted}%04X = ${toBinaryString(sign_shifted, 16)}")
//     // println(f"    exp  << 7    = 0x${exp_shifted}%04X = ${toBinaryString(exp_shifted, 16)}")
//     // println(f"    mant         = 0x${mant}%04X = ${toBinaryString(mant, 16)}")
//     // println(f"    scaled_bf16  = 0x${scaled_bf16}%04X = ${toBinaryString(scaled_bf16, 16)}")
//     // println(f"                 = (${toBinaryString(sign, 1)}) | (${toBinaryString(clampedExp, 8)}) | (${toBinaryString(mant, 7)})")

//     // Convert back to float
//     val result = bf16ToFloat(scaled_bf16)
//     //println(f"    result       = $result%.2f")
//     //println()

//     result
//   }

//   // Helper function to convert integer to binary string with specified width
//   def toBinaryString(value: Int, width: Int): String = {
//     val binary = value.toBinaryString
//     val padding = "0" * (width - binary.length)
//     padding + binary
//   }
// }

// // Wrapper module with correct dimension ordering
// class AccumulatorMemTestWrapper extends Module {
//   val meshColumns = 16  // Outer loop - 16 columns
//   val meshRows = 1      // Inner loop - 1 row
//   val accDepth = 256
  
//   val elemType = SInt(16.W)  
//   val scaleType = UInt(32.W)
//   val t = Vec(meshColumns, Vec(meshRows, elemType))  // Vec(columns, Vec(rows, ...))
  
//   val io = IO(new Bundle {
//     val dataType = Input(UInt(2.W)) 
    
//     val write = Flipped(Decoupled(new AccumulatorWriteReq(accDepth, t)))
    
//     val read_req = Flipped(Decoupled(new AccumulatorReadReq[SInt, UInt](
//       n = accDepth,
//       acc_t = elemType,
//       scale_t = scaleType
//     )))
//     val read_resp = Decoupled(new AccumulatorReadResp[SInt, UInt](
//       fullDataType = t,
//       scale_t = scaleType
//     ))
    
//     val scale_write = Flipped(Decoupled(new ScalingFactorWriteReq(9, 256)))
//   })
  
//   // Instantiate AccumulatorMem
//   val accMem = Module(new AccumulatorMem(
//     n = accDepth,
//     t = t,
//     scale_func = (x: SInt, y: UInt) => x,
//     scale_t = scaleType,
//     acc_singleported = false,
//     acc_sub_banks = 1,
//     use_shared_ext_mem = false,
//     use_tl_ext_ram = false,
//     acc_latency = 2,
//     acc_type = elemType,  
//     is_dummy = false,
//     use_mx_scaling = true,
//     scale_mem = Some(GemminiScalingFactorMemConfig(
//       baseAddr = 0x80000000L,
//       sizeInBytes = 32 << 10,
//       sramLineSizeInBytes = 32,
//       numBanks = 4
//     ))
//   ))
  
//   // Connect interfaces
//   accMem.io.dataType := io.dataType
//   accMem.io.write <> io.write
//   accMem.io.read.req <> io.read_req
//   accMem.io.read.resp <> io.read_resp
//   accMem.io.scale_mem_write.get <> io.scale_write
  
//   // Connect adder
//   accMem.io.adder.sum := VecInit((accMem.io.adder.op1 zip accMem.io.adder.op2).map { case (col1, col2) =>
//     VecInit((col1 zip col2).map { case (elem1, elem2) =>
//       val pipe = Module(new AccPipe(2, elemType))
//       pipe.io.op1 := elem1
//       pipe.io.op2 := elem2
//       pipe.io.sum
//     })
//   })
// }
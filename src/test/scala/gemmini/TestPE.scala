package gemmini

import chisel3._
import circt.stage.ChiselStage
import chiseltest._
import chiseltest.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import hardfloat._
import chisel3.util._
import scala.util.Random
import mxHardware._



// Harness wrapper for PE with MxFp support
class PEMxFpHarness(
  ts: TypeSupport,
  lut: Boolean,
  dataWidth: Int = 16
) extends Module {
  // Instantiate PE with MxFp support
  val pe = Module(new PE(
    inputType = UInt(dataWidth.W),
    weightType = UInt(dataWidth.W),
    outputType = UInt(dataWidth.W),
    accType = UInt(dataWidth.W),
    df = Dataflow.OS,
    max_simultaneous_matmuls = 1,
    useMxFp = true,
    mxfpSupportedTypes = Some(ts),
    useMxfpLUT = lut
  ))

  val io = IO(new Bundle {
    // PE standard inputs
    val in_a = Input(UInt(dataWidth.W))
    val in_b = Input(UInt(dataWidth.W))
    val in_d = Input(UInt(dataWidth.W))
    
    // PE outputs
    val out_a = Output(UInt(dataWidth.W))
    val out_b = Output(UInt(dataWidth.W))
    val out_c = Output(UInt(dataWidth.W))
    
    // Control signals
    val dataflow = Input(UInt(1.W))
    val propagate = Input(UInt(1.W))
    val shift = Input(UInt(log2Up(dataWidth).W))
    
    // MxFp control signals
    val in_a_type = Input(UInt(2.W))
    val a_altfmt = Input(Bool())
    val in_w_type = Input(UInt(2.W))
    val w_altfmt = Input(Bool())
    
    // Other PE signals
    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
    val in_id = Input(UInt(1.W))
    val out_id = Output(UInt(1.W))
    val in_last = Input(Bool())
    val out_last = Output(Bool())
  })

  // Connect PE inputs
  pe.io.in_a := io.in_a
  pe.io.in_b := io.in_b
  pe.io.in_d := io.in_d
  
  // Connect control signals
  pe.io.in_control.dataflow := io.dataflow
  pe.io.in_control.propagate := io.propagate
  pe.io.in_control.shift := io.shift
  pe.io.in_control.in_a_type := io.in_a_type
  pe.io.in_control.a_altfmt := io.a_altfmt
  pe.io.in_control.in_w_type := io.in_w_type
  pe.io.in_control.w_altfmt := io.w_altfmt
  
  pe.io.in_valid := io.in_valid
  pe.io.in_id := io.in_id
  pe.io.in_last := io.in_last
  
  // Connect PE outputs
  io.out_a := pe.io.out_a
  io.out_b := pe.io.out_b
  io.out_c := pe.io.out_c
  io.out_valid := pe.io.out_valid
  io.out_id := pe.io.out_id
  io.out_last := pe.io.out_last
}

class PE_MxFp_BasicTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  behavior of "PE with MxFp support"
  
  it should "perform basic MAC operations with MxFp formats" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
      weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
    )
    ChiselStage.emitSystemVerilogFile(
      new PEMxFpHarness(ts, lut = false, dataWidth = 16),
      args = Array("--target-dir", "generated")
    )
    test(new PEMxFpHarness(ts, lut = false, dataWidth = 16))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      
      // Helper functions from MxFpMul test
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val expMask = (1 << eBits) - 1
        val mantMask = (1 << mBits) - 1
        def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask)
      }
      
      val FP4_E2M1 = MiniFmt(2, 1, bias = 1)
      val FP6_E2M3 = MiniFmt(2, 3, bias = 1)
      val FP8_E4M3 = MiniFmt(4, 3, bias = 7)
      
      def binStr(x: BigInt, w: Int): String = {
        val s = x.toString(2)
        "b" + ("0" * (w - s.length)) + s
      }
      
      // Initialize PE control signals
      h.io.dataflow.poke(0.U)      // Output stationary
      h.io.propagate.poke(0.U)     // Compute mode
      h.io.shift.poke(0.U)
      h.io.in_valid.poke(true.B)
      h.io.in_id.poke(0.U)
      h.io.in_last.poke(false.B)
      
      println("\n=== Test Case 1: FP4 × FP4 ===")
      
      // Test FP4 activation and weight
      val a0_fp4 = FP4_E2M1.enc(2, 1) & 0xF  // exp=2, mant=1
      val a1_fp4 = FP4_E2M1.enc(1, 0) & 0xF  // exp=1, mant=0
      val in_a = (BigInt(a1_fp4) << 8) | BigInt(a0_fp4)
      
      val w0_fp4 = FP4_E2M1.enc(2, 0) & 0xF
      val w1_fp4 = FP4_E2M1.enc(1, 1) & 0xF
      val in_b = (BigInt(w1_fp4) << 8) | BigInt(w0_fp4)
      
      h.io.in_a_type.poke(0.U)      // fp4
      h.io.a_altfmt.poke(false.B)
      h.io.in_w_type.poke(0.U)      // fp4
      h.io.w_altfmt.poke(false.B)
      
      h.io.in_a.poke(in_a.U)
      h.io.in_b.poke(in_b.U)
      h.io.in_d.poke(0.U)           // accumulator input
      
      println(s"in_a (fp4×2) = ${binStr(in_a, 16)}")
      println(s"in_b (fp4×2) = ${binStr(in_b, 16)}")
      
      h.clock.step(2)
      
      val out_c = h.io.out_c.peek().litValue
      println(s"out_c = ${binStr(out_c, 16)}")
      println(s"out_valid = ${h.io.out_valid.peek().litToBoolean}")
      
      println("\n=== Test Case 2: FP6 (E2M3) × FP6 (E2M3) ===")
      
      val a_fp6 = FP6_E2M3.enc(2, 3) & 0x3F
      val w_fp6 = FP6_E2M3.enc(2, 1) & 0x3F
      
      h.io.in_a_type.poke(1.U)      // fp6
      h.io.a_altfmt.poke(false.B)   // E2M3
      h.io.in_w_type.poke(1.U)      // fp6
      h.io.w_altfmt.poke(false.B)   // E2M3
      
      h.io.in_a.poke(a_fp6.U)
      h.io.in_b.poke(w_fp6.U)
      h.io.in_d.poke(0.U)
      
      println(s"in_a (fp6 E2M3) = ${binStr(a_fp6, 16)}")
      println(s"in_b (fp6 E2M3) = ${binStr(w_fp6, 16)}")
      
      h.clock.step(2)
      
      val out_c2 = h.io.out_c.peek().litValue
      println(s"out_c = ${binStr(out_c2, 16)}")
      
      println("\n=== Test Case 3: FP8 (E4M3) × FP8 (E4M3) ===")
      
      val a_fp8 = FP8_E4M3.enc(8, 4) & 0xFF
      val w_fp8 = FP8_E4M3.enc(7, 2) & 0xFF
      
      h.io.in_a_type.poke(2.U)      // fp8
      h.io.a_altfmt.poke(false.B)   // E4M3
      h.io.in_w_type.poke(2.U)      // fp8
      h.io.w_altfmt.poke(false.B)   // E4M3
      
      h.io.in_a.poke(a_fp8.U)
      h.io.in_b.poke(w_fp8.U)
      h.io.in_d.poke(0.U)
      
      println(s"in_a (fp8 E4M3) = ${binStr(a_fp8, 16)}")
      println(s"in_b (fp8 E4M3) = ${binStr(w_fp8, 16)}")
      
      h.clock.step(2)
      
      val out_c3 = h.io.out_c.peek().litValue
      println(s"out_c = ${binStr(out_c3, 16)}")
      
      h.io.in_valid.poke(false.B)
      h.clock.step(1)
    }
  }
}

class PE_MxFp_DataflowTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  behavior of "PE MxFp dataflow modes"
  
  it should "test output-stationary and weight-stationary modes" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
      weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
    )
    
    test(new PEMxFpHarness(ts, lut = false, dataWidth = 16))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      
      // Define case class first before using it
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val expMask = (1 << eBits) - 1
        val mantMask = (1 << mBits) - 1
        def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask)
      }
      
      val FP4_E2M1 = MiniFmt(2, 1, bias = 1)
      
      println("\n=== Output Stationary Mode Test ===")
      
      h.io.dataflow.poke(0.U)      // OS mode
      h.io.propagate.poke(0.U)     // COMPUTE
      h.io.shift.poke(0.U)
      h.io.in_valid.poke(true.B)
      h.io.in_id.poke(0.U)
      h.io.in_last.poke(false.B)
      
      // FP4 inputs
      h.io.in_a_type.poke(0.U)
      h.io.a_altfmt.poke(false.B)
      h.io.in_w_type.poke(0.U)
      h.io.w_altfmt.poke(false.B)
      
      val test_a = FP4_E2M1.enc(2, 1) & 0xF
      val test_w = FP4_E2M1.enc(2, 0) & 0xF
      
      h.io.in_a.poke(test_a.U)
      h.io.in_b.poke(test_w.U)
      h.io.in_d.poke(0.U)
      
      h.clock.step(3)
      
      println(s"OS Mode: out_c = 0x${h.io.out_c.peek().litValue.toString(16)}")
      
      // Test PROPAGATE mode
      println("\n=== Propagate Mode Test ===")
      h.io.propagate.poke(1.U)     // PROPAGATE
      h.clock.step(2)
      
      println(s"Propagate: out_c = 0x${h.io.out_c.peek().litValue.toString(16)}")
      println(s"Propagate: out_b = 0x${h.io.out_b.peek().litValue.toString(16)}")
      
      h.io.in_valid.poke(false.B)
      h.clock.step(1)
    }
  }
}

class PE_MxFp_RandomizedTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  
  behavior of "PE MxFp with randomized inputs"
  
  it should "generate RTL for randomized test configuration" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
      weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
    )
    
    println("\n=== Generating SystemVerilog RTL for Randomized Test ===")
     ChiselStage.emitSystemVerilogFile(
     new PEMxFpHarness(ts, lut = false, dataWidth = 16),
     Array("--target-dir", "generated/pe_mxfp_basic")
   )
    println("RTL generated in: generated/pe_mxfp_random/")
  }

  it should "handle various random MxFp format combinations" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
      weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
    )
    
    test(new PEMxFpHarness(ts, lut = false, dataWidth = 32))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val expMask = (1 << eBits) - 1
        val mantMask = (1 << mBits) - 1
        def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask)
      }
      
      val FP4_E2M1 = MiniFmt(2, 1, bias = 1)
      val FP6_E2M3 = MiniFmt(2, 3, bias = 1)
      val FP6_E3M2 = MiniFmt(3, 2, bias = 3)
      val FP8_E4M3 = MiniFmt(4, 3, bias = 7)
      val FP8_E5M2 = MiniFmt(5, 2, bias = 15)
      
      val rng = new Random(0xDEADBEEF)
      
      def genSmall(fmt: MiniFmt): Int = {
        val r = rng.nextFloat()
        if (r < 0.10f) fmt.enc(0, 0)  // zero
        else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))  // subnormal
        else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1), 
                     rng.nextInt(fmt.mantMask + 1))  // normal
      }
      
      h.io.dataflow.poke(0.U)
      h.io.propagate.poke(0.U)
      h.io.shift.poke(0.U)
      h.io.in_valid.poke(true.B)
      h.io.in_id.poke(0.U)
      h.io.in_last.poke(false.B)
      
      val testConfigs = Seq(
        ("FP4×FP4", 0, false, 0, false, FP4_E2M1),
        ("FP6(E2M3)×FP6(E2M3)", 1, false, 1, false, FP6_E2M3),
        ("FP6(E3M2)×FP6(E3M2)", 1, true, 1, true, FP6_E3M2),
        ("FP8(E4M3)×FP8(E4M3)", 2, false, 2, false, FP8_E4M3),
        ("FP8(E5M2)×FP8(E5M2)", 2, true, 2, true, FP8_E5M2)
      )
      
      for ((name, aType, aAlt, wType, wAlt, fmt) <- testConfigs) {
        println(s"\n=== Testing $name ===")
        
        for (trial <- 0 until 5) {
          val a_val = genSmall(fmt)
          val w_val = genSmall(fmt)
          
          h.io.in_a_type.poke(aType.U)
          h.io.a_altfmt.poke(aAlt.B)
          h.io.in_w_type.poke(wType.U)
          h.io.w_altfmt.poke(wAlt.B)
          
          h.io.in_a.poke(a_val.U)
          h.io.in_b.poke(w_val.U)
          h.io.in_d.poke(0.U)
          
          h.clock.step(2)
          
          val out = h.io.out_c.peek().litValue
          println(f"  Trial $trial: a=0x$a_val%X w=0x$w_val%X -> out=0x$out%X")
        }
      }
      
      h.io.in_valid.poke(false.B)
      h.clock.step(1)
    }
  }
}
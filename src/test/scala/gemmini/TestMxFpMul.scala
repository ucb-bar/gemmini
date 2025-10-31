package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import hardfloat._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import circt.stage.ChiselStage
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import chisel3.util._
import scala.{Float => ScalaFloat}

class MxFpMulHarness(ts: TypeSupport, lut: Boolean) extends Module {
  val dut = Module(new MxFpMul(ts, lut))

  // Mirror DUT IO; change C to raw E8M7 (16b) and expose OUT as packed raw E8M7x4 (64b)
  val io = IO(new Bundle {
    val in_activation = Input(UInt(dut.io.in_activation.getWidth.W))
    val in_a_type     = Input(UInt(2.W))
    val a_altfmt      = Input(Bool())
    val in_weights    = Input(UInt(dut.io.in_weights.getWidth.W))
    val in_w_type     = Input(UInt(2.W))
    val w_altfmt      = Input(Bool())
    val enable        = Input(Bool())

    // Raw (non-recoded) E8M7: 1|8|7 = 16 bits
    val c_raw         = Input(UInt(16.W))

    // 4 lanes of raw E8M7 packed LSB..MSB (lane0 lowest 16b)
    val out           = Output(UInt((4*16).W))

    // Observe the recoded C actually driven into DUT
    val rec_c_applied = Output(UInt(dut.io.rec_c.getWidth.W))
  })

  // Pass-through
  dut.io.in_activation := io.in_activation
  dut.io.in_a_type     := io.in_a_type
  dut.io.a_altfmt      := io.a_altfmt
  dut.io.in_weights    := io.in_weights
  dut.io.in_w_type     := io.in_w_type
  dut.io.w_altfmt      := io.w_altfmt
  dut.io.enable        := io.enable

  // ---- Recode raw E8M7 -> recFN (exp=8, sig=8 (7 frac + hidden 1)) ----
  // recFull width = 1 + exp + sig = 1 + 8 + 8 = 17
  private val recFull = hardfloat.recFNFromFN(8, 8, io.c_raw) // UInt(17.W)

  // Size-match to DUT's rec_c width
  private val wantW = dut.io.rec_c.getWidth
  private val haveW = recFull.getWidth
  private val recSized =
    if (haveW == wantW) recFull
    else if (haveW > wantW) recFull(wantW-1, 0)
    else Cat(0.U((wantW - haveW).W), recFull)

  dut.io.rec_c := recSized
  io.rec_c_applied := dut.io.rec_c

  // ---- Convert DUT recFN lanes to raw E8M7 (16b) and pack into io.out ----
  // Assume dut.io.out packs 4 equal-width recFN lanes (commonly 17b each).
  private val recOutW   = dut.io.out.getWidth
  private val lanes     = 4
  require(recOutW % lanes == 0, s"DUT out width ($recOutW) not divisible by $lanes")
  private val recLaneW  = recOutW / lanes

  // Extract recFN lanes
  private val recLanes  = Wire(Vec(lanes, UInt(recLaneW.W)))
  for (i <- 0 until lanes) {
    val hi = (i+1)*recLaneW - 1
    val lo = i*recLaneW
    recLanes(i) := dut.io.out(hi, lo)
  }

  // Convert each recFN lane to raw E8M7 (16b)
  private val rawLanes16 = Wire(Vec(lanes, UInt(16.W)))
  for (i <- 0 until lanes) {
    rawLanes16(i) := hardfloat.fNFromRecFN(8, 8, recLanes(i)) // 16-bit IEEE: 1|8|7
  }

  // Pack LSB-first: lane0 in [15:0], lane1 in [31:16], ...
  io.out := Cat(rawLanes16.reverse)
}


class MxFpMulHarnessBf16Out(ts: TypeSupport, lut: Boolean) extends Module {
  val dut = Module(new MxFpMul(ts, lut))

  val io = IO(new Bundle {
    val in_activation = Input(UInt(dut.io.in_activation.getWidth.W))
    val in_a_type     = Input(UInt(2.W))
    val a_altfmt      = Input(Bool())
    val in_weights    = Input(UInt(dut.io.in_weights.getWidth.W))
    val in_w_type     = Input(UInt(2.W))
    val w_altfmt      = Input(Bool())
    val enable        = Input(Bool())
    val c_raw         = Input(UInt(16.W))      // BF16 (E8M7) 1|8|7

    val out_bf16      = Output(UInt(64.W))     // 4 × BF16 packed LSB-first
    val rec_c_applied = Output(UInt(dut.io.rec_c.getWidth.W))
  })

  // Pass-through
  dut.io.in_activation := io.in_activation
  dut.io.in_a_type     := io.in_a_type
  dut.io.a_altfmt      := io.a_altfmt
  dut.io.in_weights    := io.in_weights
  dut.io.in_w_type     := io.in_w_type
  dut.io.w_altfmt      := io.w_altfmt
  dut.io.enable        := io.enable

  // BF16 → recFN(8,8)
  private val recC = hardfloat.recFNFromFN(8, 8, io.c_raw) // 17b
  // Size-match to DUT width (some designs keep 16 here)
  private val wantW = dut.io.rec_c.getWidth
  private val haveW = recC.getWidth
  private val recSized =
    if (haveW == wantW) recC
    else if (haveW > wantW) recC(wantW-1, 0)
    else Cat(0.U((wantW - haveW).W), recC)

  dut.io.rec_c := recSized
  io.rec_c_applied := dut.io.rec_c

  // DUT out → always 4×BF16
  private val dutW = dut.io.out.getWidth
  require(dutW % 4 == 0, s"DUT out width ($dutW) must be divisible by 4")
  private val laneW = dutW / 4

  val lanes = Wire(Vec(4, UInt(laneW.W)))
  for (i <- 0 until 4) {
    val hi = (i + 1) * laneW - 1
    val lo = i * laneW
    lanes(i) := dut.io.out(hi, lo)
  }

  // If DUT lanes are recFN(17), convert to BF16; if already BF16(16), pass-through
  val lanesBF16 = Wire(Vec(4, UInt(16.W)))
  if (laneW == 17) {
    for (i <- 0 until 4) lanesBF16(i) := hardfloat.fNFromRecFN(8, 8, lanes(i)) // 16b
  } else {
    require(laneW == 16, s"Expected lane width 16 (BF16) or 17 (recFN), got $laneW")
    lanesBF16 := lanes.map(_.pad(16)) // ensure 16b width even if laneW=16
  }

  io.out_bf16 := Cat(lanesBF16.reverse) // lane0 at [15:0]
}


class MxFpMul_AllATypes_BF16Out_SelfChecking_Spec
  extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {

  behavior of "MxFpMul — BF16 output; 12b activations / 24b weights; fp6_0 & fp8_1 disabled"

  it should "print inputs/expectations before, actual outputs before asserts, and PASS after" in {
    val ts = TypeSupport(
      actSupportFp4 = true,  actSupportFp6_1 = true, actSupportFp8_0 = true,
      weiSupportFp4 = true,  weiSupportFp6_1 = true, weiSupportFp8_0 = true
    )

    test(new MxFpMulHarnessBf16Out(ts, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      // ---------- small-format helpers (positive-only) ----------
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val expMask = (1 << eBits) - 1
        val mantMask= (1 << mBits) - 1
        def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask) // sign=0
      }
      val FP4_E2M1 = MiniFmt(2,1, bias=1)
      val FP6_E3M2 = MiniFmt(3,2, bias=3)   // altfmt = 1
      val FP8_E4M3 = MiniFmt(4,3, bias=7)   // altfmt = 0

      def decodeSmall(fmt: MiniFmt, raw: Int): ScalaFloat = {
        val e = (raw >> fmt.mBits) & fmt.expMask
        val m = raw & fmt.mantMask
        if (e == 0) {
          if (m == 0) 0.0f
          else (m.toFloat / (1 << fmt.mBits).toFloat) * math.pow(2.0, 1 - fmt.bias).toFloat
        } else {
          val frac = 1.0f + m.toFloat / (1 << fmt.mBits).toFloat
          (frac * math.pow(2.0, e - fmt.bias)).toFloat
        }
      }

      // ---------- BF16 helpers (IEEE) ----------
      def bf16ToFloat(raw16: Int): ScalaFloat = java.lang.Float.intBitsToFloat(raw16 << 16)
      def floatToBf16Raw(f: ScalaFloat): Int = {
        val bits = java.lang.Float.floatToRawIntBits(f)
        val lsb  = (bits >>> 16) & 1
        val rnd  = bits + (0x7FFF + lsb)   // RNE to 16 MSBs
        (rnd >>> 16) & 0xFFFF
      }

      // Pretty printers
      def binStr(x: BigInt, w: Int): String = {
        val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
      }
      def lane(bits: BigInt, idx: Int, laneW: Int): Int =
        ((bits >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)).toInt
      def showBF16(tag: String, v: Int): Unit = {
        val s = (v >>> 15) & 1
        val e = (v >>> 7)  & 0xFF
        val f = v & 0x7F
        println(f"$tag: 0x$v%04X  s=$s e=0x$e%02X f=0x$f%02X  (~=${bf16ToFloat(v)}%g)")
      }

      val rng = new Random(0xA11CA11) // fixed seed for repeatability
      def genSmall(fmt: MiniFmt): Int = {
        val r = rng.nextFloat()
        if (r < 0.10f) fmt.enc(0, 0)                                  // +0
        else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))  // subnormal
        else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),         // normal
                     rng.nextInt(fmt.mantMask + 1))
      }
      def genBF16(): Int = {
        val r = rng.nextFloat()
        if (r < 0.10f) 0x0000                                   // +0
        else if (r < 0.20f) (0x0001 + rng.nextInt(0x7F))        // subnormal
        else {
          val mag = math.pow(2.0, rng.nextInt(8) - 4).toFloat
          val base= rng.nextFloat() * mag
          floatToBf16Raw(base)
        }
      }

      // Widths (from DUT)
      val aW    = h.io.in_activation.getWidth   // expect 12
      val wW    = h.io.in_weights.getWidth      // expect 24
      val laneW = 16                             // BF16 lanes on harness out

      // ---- Activation pack/desc for 12-bit port ----
      // - fp4: 2 lanes (nibbles at bit 0 and 6)
      // - fp6_1 E3M2 (alt=1): 2 lanes (6b at bit 0 and 6)
      // - fp8_0 E4M3 (alt=0): 1 lane (bits [7:0])
      def packActs(aType: Int, aAlt: Boolean, raws: Seq[Int]): (BigInt, Int, Seq[ScalaFloat], String) = aType match {
        case 0 => // two fp4
          require(raws.length == 2)
          val a0 = raws(0) & 0xF; val a1 = raws(1) & 0xF
          val packed = (BigInt(a1) << 6) | BigInt(a0)
          val vals = raws.map(r => decodeSmall(FP4_E2M1, r))
          (packed, 2, vals, f"fp4 lanes: ${raws.map(r => f"0x$r%X").mkString(", ")} -> ${vals.mkString(", ")}")

        case 1 => // fp6_1 → E3M2 only (alt MUST be 1)
          require(aAlt, "fp6_1 requires altfmt=1 (E3M2)")
          require(raws.length == 2)
          val a0 = raws(0) & 0x3F; val a1 = raws(1) & 0x3F
          val packed = (BigInt(a1) << 6) | BigInt(a0)
          val vals = raws.map(r => decodeSmall(FP6_E3M2, r))
          (packed, 2, vals, f"fp6 E3M2 lanes: ${raws.map(r => f"0x$r%02X").mkString(", ")} -> ${vals.mkString(", ")}")

        case 2 => // fp8_0 → E4M3 only (alt MUST be 0)
          require(!aAlt, "fp8_0 requires altfmt=0 (E4M3)")
          require(raws.length == 1)
          val a0 = raws.head & 0xFF
          val vals = raws.map(r => decodeSmall(FP8_E4M3, r))
          (BigInt(a0), 1, vals, f"fp8 E4M3 lane: 0x$a0%02X -> ${vals.head}")
      }

      // ---- Weight pack/desc for 24-bit port ----
      // - fp4: 4 lanes (nibbles at bit 0,6,12,18)
      // - fp6_1 E3M2(alt=1): 4 lanes (6b at bit 0,6,12,18)
      // - fp8_0 E4M3(alt=0): 2 lanes (bytes at [7:0] and [15:8])
      def packWeis(wType: Int, wAlt: Boolean, raws: Seq[Int]): (BigInt, Int, Seq[ScalaFloat], String) = wType match {
        case 0 => // fp4×4
          require(raws.length == 4)
          val packed = (0 until 4).map(i => BigInt(raws(i) & 0xF) << (6*i)).reduce(_|_)
          val vals = raws.map(r => decodeSmall(FP4_E2M1, r))
          (packed, 4, vals, f"fp4x4: ${raws.map(r => f"0x$r%X").mkString(" ")} -> ${vals.mkString(", ")}")

        case 1 => // fp6_1 → E3M2×4 (alt MUST be 1)
          require(wAlt, "fp6_1 requires altfmt=1 (E3M2)")
          require(raws.length == 4)
          val packed = (0 until 4).map(i => BigInt(raws(i) & 0x3F) << (6*i)).reduce(_|_)
          val vals = raws.map(r => decodeSmall(FP6_E3M2, r))
          (packed, 4, vals, f"fp6 E3M2 x4: ${raws.map(r => f"0x$r%02X").mkString(" ")} -> ${vals.mkString(", ")}")

        case 2 => // fp8_0 → E4M3×1 (alt MUST be 0). Pack only ONE byte at [7:0].
          require(!wAlt, "fp8_0 requires altfmt=0 (E4M3)")
          require(raws.length == 1, "Only one fp8 E4M3 weight is supported")
          val w0 = raws.head & 0xFF
          val packed = BigInt(w0) // bits [7:0]; upper bits zero
          val vals = raws.map(r => decodeSmall(FP8_E4M3, r))
          (packed, 1, vals, f"fp8 E4M3 x1: 0x$w0%02X -> ${vals.head}")
      }

      // ---- Expected lane mapping (reflects 12/24b config & lane counts) ----
      def expectedBF16Lane(
        aVals: Seq[ScalaFloat], wVals: Seq[ScalaFloat], c: ScalaFloat, lane: Int,
        aLanes: Int, wLanes: Int, aType: Int, aAlt: Boolean, wType: Int, wAlt: Boolean
      ): Option[Int] = {
        def toBF16(x: ScalaFloat) = floatToBf16Raw(x)

        (aLanes, wLanes) match {
          case (2,4) =>
            // [0]=a0*w0, [1]=a0*w1, [2]=a1*w2, [3]=a1*w3
            val pairs = Array((0,0),(0,1),(1,2),(1,3))
            val (ai, wi) = pairs(lane)
            Some(toBF16(aVals(ai) * wVals(wi) + c))

          case (2,2) =>
            // 2 outputs: lane0=a0*w0, lane2=a1*w1
            lane match {
              case 0 => //Some(toBF16(decodeSmall(aVals(0).fmt, aVals(0).bits) * decodeSmall(wVals(0).fmt, wVals(0).bits) + c))
                if (aVals.nonEmpty && wVals.nonEmpty)
                  Some(toBF16(aVals(0) * wVals(0) + c))
                else None
              case 2 => //Some(toBF16(aVals(1) * wVals(1) + c))
                if (aVals.length > 1 && wVals.length > 1)
                  Some(toBF16(aVals(1) * wVals(1) + c))
                else None
              case _ => None
            }

          case (2,1) =>  // ← NEW: two-lane activations × single-lane weight (fp8 E4M3)
            lane match {
              case 0 => Some(toBF16(aVals(0) * wVals(0) + c))
              case 2 => Some(toBF16(aVals(1) * wVals(0) + c))
              case _ => None
            }

          case (1,4) =>
            // 2 outputs: lane0=a0*w0, lane2=a0*w2
            lane match {
              case 0 => Some(toBF16(aVals(0) * wVals(0) + c))
              case 2 => Some(toBF16(aVals(0) * wVals(2) + c))
              case _ => None
            }

          case (1,2) =>
            // 2 outputs: lane0=a0*w0, lane2=a0*w1
            lane match {
              case 0 => Some(toBF16(aVals(0) * wVals(0) + c))
              case 2 => Some(toBF16(aVals(0) * wVals(1) + c))
              case _ => None
            }

          case (1,1) =>                     // ← NEW: E4M3 act × E4M3 weight (single × single)
            if (lane == 0) Some(toBF16(aVals(0) * wVals(0) + c)) else None
          case _ =>
            None
        }
      }

      // ---------- Variants (only supported types) ----------
      // a_type: 0=fp4, 1=fp6_1(E3M2 alt=1), 2=fp8_0(E4M3 alt=0)
      val aVariants = Seq(
        //("A: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3)))
     // )
      //   ("A: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
       ("A: 2×fp6 (E3M2 alt1)", 1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2)))
      //   ("A: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3)))
       )

      // w_type: 0=fp4×4, 1=fp6_1(E3M2 alt=1 ×4), 2=fp8_0(E4M3 alt=0 ×2)
      val wVariants = Seq(
        //("W: 4×fp4",             0, false, () => Seq.fill(4)(genSmall(FP4_E2M1)))
       ("W: 4×fp6 (E3M2 alt1)", 1, true,  () => Seq.fill(4)(genSmall(FP6_E3M2)))
        // ("W: 2×fp8 (E4M3 alt0)", 2, false, () => Seq.fill(1)(genSmall(FP8_E4M3)))
      )

      val trialsPerCombo = 100
      h.io.enable.poke(true.B)

      for ((aName, aType, aAlt, aGen) <- aVariants) {
        for ((wName, wType, wAlt, wGen) <- wVariants) {
          println(s"\n==== Combo: $aName  vs  $wName  (a_type=$aType alt=${if(aAlt)1 else 0}; w_type=$wType alt=${if(wAlt)1 else 0}) ====")

          for (t <- 0 until trialsPerCombo) {
            // --------------------- Generate stimuli ---------------------
            val aRaws = aGen()
            val (aPacked, aLanes, aVals, aDesc) = packActs(aType, aAlt, aRaws)

            val wRaws = wGen()
            val (wPacked, wLanes, wVals, wDesc) = packWeis(wType, wAlt, wRaws)

            val cRaw  = genBF16()
            val cVal  = bf16ToFloat(cRaw)

            // --------------------- PRE-TEST PRINTS ----------------------
            println(f"-- trial #$t%02d  PRE")
            println(s"  in_activation  (${aW}b) = ${binStr(aPacked, aW)}   $aDesc")
            println(s"  in_weights     (${wW}b) = ${binStr(wPacked, wW)}   $wDesc")
            showBF16("  c_raw (BF16)           ", cRaw)

            // Pre-compute expected lanes and print them
            val expOpt = Array.tabulate(4)(i =>
              expectedBF16Lane(aVals, wVals, cVal, i,
                aLanes = aLanes, wLanes = wLanes,
                aType = aType, aAlt = aAlt, wType = wType, wAlt = wAlt)
            )
            (0 until 4).foreach { i =>
              expOpt(i) match {
                case Some(e) => showBF16(f"  exp lane[$i]", e)
                case None    => println(f"  exp lane[$i]: (n/a)")
              }
            }

            // --------------------- Drive DUT ----------------------------
            h.io.in_a_type.poke(aType.U)
            h.io.a_altfmt.poke(aAlt.B)
            h.io.in_activation.poke(aPacked.U(aW.W))
            h.io.in_w_type.poke(wType.U)
            h.io.w_altfmt.poke(wAlt.B)
            h.io.in_weights.poke(wPacked.U(wW.W))
            h.io.c_raw.poke(cRaw.U(16.W))

            // latency cushion (tune if deeper)
            h.clock.step(2)

            // --------------------- POST-STEP PRINTS (before asserts) ----
            val outBits = h.io.out_bf16.peek().litValue
            println(s"  out_bf16 (${4*laneW}b)      = ${binStr(outBits, 4*laneW)}")
            val got = Array.tabulate(4)(i => lane(outBits, i, laneW))
            (0 until 4).foreach(i => showBF16(f"  got lane[$i]", got(i)))

            // --------------------- ASSERTIONS ---------------------------
            // Fill don't-care lanes with DUT values, assert defined lanes strictly
            val filledExp = Array.tabulate(4)(i => expOpt(i).getOrElse(got(i)))
            expOpt.zipWithIndex.foreach {
              case (Some(e), i) => assert(got(i) == e, f"lane[$i] mismatch: got 0x${got(i)}%04X exp 0x$e%04X")
              case _ => ()
            }
            val packedExp =
              (BigInt(filledExp(3) & 0xFFFF) << 48) |
              (BigInt(filledExp(2) & 0xFFFF) << 32) |
              (BigInt(filledExp(1) & 0xFFFF) << 16) |
               BigInt(filledExp(0) & 0xFFFF)
            h.io.out_bf16.expect(packedExp.U, s"trial $t packed expect mismatch")

            // --------------------- PASS PRINT ---------------------------
            println(s"  RESULT: PASS (trial $t)")
          }
        }
      }

      h.io.enable.poke(false.B)
      h.clock.step(1)

      //println(ChiselStage.emitSystemVerilog(new MxFpMulHarnessBf16Out(ts, lut = false)))
      //ChiselStage.emitSystemVerilog(new MxFpMulHarnessBf16Out(ts, lut = false))
    }
  }
}

// ----------------------------- Test -----------------------------
class MxFpMul_Fp4_WithRecC_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  behavior of "MxFpMul — fp4 activations; rec_c via E8M7; OUT converted to raw E8M7 per lane"

  it should "run two fp4-mode cases and print inputs/outputs as raw E8M7" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6_1 = true, actSupportFp8_0 = true,
      weiSupportFp4 = true, weiSupportFp6_1 = true, weiSupportFp8_0 = true
    )

    test(new MxFpMulHarness(ts, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      // ---- helpers ----
      def mask(w: Int) = (BigInt(1) << w) - 1
      def binStr(x: BigInt, w: Int): String = {
        val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
      }
      def lane(x: BigInt, idx: Int, laneW: Int): BigInt =
        (x >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)

      // Decode raw E8M7 lane (16b)
      def showLane16(tag: String, bits16: BigInt): Unit = {
        val s   = ((bits16 >> 15) & 0x1).toInt
        val exp = ((bits16 >> 7)  & 0xFF).toInt
        val frac= (bits16 & 0x7F).toInt
        println(f"$tag: raw16=0x${bits16}%04x  s=$s exp=0x$exp%02x frac=0x$frac%02x")
      }

      // Pack raw E8M7: s(1) | exp(8) | frac(7)
      def e8m7Raw(s: Int, e: Int, f: Int): BigInt = {
        require(s == 0 || s == 1); require(e >= 0 && e < 256); require(f >= 0 && f < 128)
        (BigInt(s) << 15) | (BigInt(e) << 7) | BigInt(f)
      }

      // Mini encoders for small formats (just for making fp4/fp6 stimuli)
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val sBits = 1
        def expMask = (1 << eBits) - 1
        def mantMask = (1 << mBits) - 1
      }
      val FP4_E2M1 = MiniFmt(2,1, bias=1)
      val FP6_E2M3 = MiniFmt(2,3, bias=1)

      def enc(fmt: MiniFmt, s: Int, e: Int, m: Int): Int = {
        require((s == 0 || s == 1) && (e & ~fmt.expMask) == 0 && (m & ~fmt.mantMask) == 0)
        (s << (fmt.eBits + fmt.mBits)) | (e << fmt.mBits) | m
      }

      // ---- widths (note: io.out is now 64b = 4×16) ----
      val aW    = h.io.in_activation.getWidth
      val wW    = h.io.in_weights.getWidth
      val outW  = h.io.out.getWidth           // expect 64 (4×16)
      val laneW = outW / 4                    // 16

      // ---- fixed fp4 activations (2 lanes) ----
      val a0 = enc(FP4_E2M1, 0, 0, 1) & 0xF   // 0_10_1 = 0x5
      val a1 = enc(FP4_E2M1, 0, 0, 1) & 0xF   // 0_01_0 = 0x2
      val inA: BigInt = (BigInt(a1) << 8) | BigInt(a0)

      // Common settings
      h.io.in_a_type.poke(0.U)     // fp4
      h.io.a_altfmt.poke(false.B)
      h.io.in_activation.poke(inA.U(aW.W))
      h.io.enable.poke(true.B)

      // ======================================================
      // Case A: fp4 × fp4, c_raw = 0.0  → expect 4 lanes used
      // ======================================================
      {
        val w0 = 0x2; val w1 = 0x3; val w2 = 0x4; val w3 = 0x1
        val inW = (BigInt(w0 & 0xF) << 0)  |
                  (BigInt(w1 & 0xF) << 8)  |
                  (BigInt(w2 & 0xF) << 16) |
                  (BigInt(w3 & 0xF) << 24)
        h.io.in_w_type.poke(0.U)     // fp4
        h.io.w_altfmt.poke(false.B)
        h.io.in_weights.poke(inW.U(wW.W))

        val cRaw = e8m7Raw(s = 0, e = 0x00, f = 0x00) // 0.0 in E8M7 raw
        h.io.c_raw.poke(cRaw.U(16.W))

        println("\n=== Case A: fp4(act) × fp4(wei), alt=0; c_raw(E8M7)=0.0 ===")
        println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=0x$a0  a1=0x$a1")
        println(s"in_weights    (fp4x4) = ${binStr(inW, wW)}   w=0x$w0 0x$w1 0x$w2 0x$w3")
        println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}")

        h.clock.step(1)

        val recApplied = h.io.rec_c_applied.peek().litValue
        val outBits    = h.io.out.peek().litValue
        println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
        println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
        (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
      }

      // ==================================================================
      // Case B: fp4 × fp6 (E2M3 single-weight), c_raw = 1.0 → 2 lanes used
      // ==================================================================
      {
        val w_single = enc(FP6_E2M3, s=0, e=2, m=1) & 0x3F
        val inW = BigInt(w_single) // low 6 bits of lowest slot
        h.io.in_w_type.poke(1.U)    // fp6
        h.io.w_altfmt.poke(false.B)  // E2M3 single-weight mode
        h.io.in_weights.poke(inW.U(wW.W))

        // val cRaw = e8m7Raw(s = 0, e = 0x7F, f = 0x00) // 1.0 in E8M7 raw
        val cRaw = e8m7Raw(s = 0, e = 0x00, f = 0x00) // 0.0 in E8M7 raw
        h.io.c_raw.poke(cRaw.U(16.W))

        println("\n=== Case B: fp4(act) × fp6(E2M3 single), alt=1; c_raw(E8M7)=1.0 ===")
        println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=0x$a0  a1=0x$a1")
        println(s"in_weights    (fp6 1)  = ${binStr(inW, wW)}   w0(E2M3)=0x${w_single}")
        println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}")

        h.clock.step(1)

        val recApplied = h.io.rec_c_applied.peek().litValue
        val outBits    = h.io.out.peek().litValue
        println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
        println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
        (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
      }

      h.io.enable.poke(false.B)
      h.clock.step(1)
    }
  }
}

class MxFpMul_Fp4_WithRecC_MoreCoverage_Spec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "MxFpMul — fp4 activations; print-heavy coverage across fp4/fp6/fp8 weights"

  it should "exercise multiple stimuli per case and print lane-wise raw E8M7" in {
    val ts = TypeSupport(
      actSupportFp4 = true, actSupportFp6_1 = true, actSupportFp8_0 = true,
      weiSupportFp4 = true, weiSupportFp6_1 = true, weiSupportFp8_0 = true
    )

    test(new MxFpMulHarness(ts, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { h =>
      // ---------- helpers ----------
      def mask(w: Int) = (BigInt(1) << w) - 1
      def binStr(x: BigInt, w: Int): String = {
        val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
      }
      def lane(x: BigInt, idx: Int, laneW: Int): BigInt =
        (x >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)

      def showLane16(tag: String, bits16: BigInt): Unit = {
        val s   = ((bits16 >> 15) & 0x1).toInt
        val exp = ((bits16 >> 7)  & 0xFF).toInt
        val frac= (bits16 & 0x7F).toInt
        println(f"$tag: raw16=0x${bits16}%04x  s=$s exp=0x$exp%02x frac=0x$frac%02x")
      }

      // Pack raw E8M7: s(1) | exp(8) | frac(7)
      def e8m7Raw(s: Int, e: Int, f: Int): BigInt = {
        require(s == 0 || s == 1); require(e >= 0 && e < 256); require(f >= 0 && f < 128)
        (BigInt(s) << 15) | (BigInt(e) << 7) | BigInt(f)
      }

      // Tiny encoders for mini formats to craft stimuli (sign=0 unless noted)
      case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
        val sBits = 1
        def expMask = (1 << eBits) - 1
        def mantMask = (1 << mBits) - 1
      }
      val FP4_E2M1 = MiniFmt(2,1, bias=1)
      val FP6_E2M3 = MiniFmt(2,3, bias=1)   // “single-weight” style vectors
      val FP6_E3M2 = MiniFmt(3,2, bias=3)   // 4-lane vectors
      val FP8_E5M2 = MiniFmt(5,2, bias=15)  // 4-lane vectors
      val FP8_E4M3 = MiniFmt(4,3, bias=7)   // “single-weight” style vectors

      def enc(fmt: MiniFmt, s: Int, e: Int, m: Int): Int = {
        require((s == 0 || s == 1) && (e & ~fmt.expMask) == 0 && (m & ~fmt.mantMask) == 0)
        (s << (fmt.eBits + fmt.mBits)) | (e << fmt.mBits) | m
      }

      // Widths
      val aW    = h.io.in_activation.getWidth
      val wW    = h.io.in_weights.getWidth
      val outW  = h.io.out.getWidth           // expect 64 (4×16)
      val laneW = outW / 4                    // 16

      // -------- Activation packers (we keep fp4 activations) --------
      // Two fp4 lanes in 16b port: lane0 nibble -> [3:0], lane1 nibble -> [11:8]
      def packActsFp4(a0_nib: Int, a1_nib: Int): BigInt =
        (BigInt(a1_nib & 0xF) << 8) | BigInt(a0_nib & 0xF)

      // fp4 activation sets (normalized, positive)
      val aSet = Seq(
        ("Aset0", enc(FP4_E2M1, 0, 2, 1) & 0xF, enc(FP4_E2M1, 0, 1, 0) & 0xF), // a0=0x5, a1=0x2
        ("Aset1", enc(FP4_E2M1, 0, 2, 0) & 0xF, enc(FP4_E2M1, 0, 1, 1) & 0xF)  // a0=0x4, a1=0x3
      )

      // -------- Weight packers --------
      // 4 slots in 32b bus, each 8b wide; we use low bits per type.
      def packW_fp4x4(w: Seq[Int]): BigInt =
        (0 until 4).map(i => BigInt(w(i) & 0xF)  << (8*i)).reduce(_ | _)

      def packW_fp6_e3m2_x4(w: Seq[Int]): BigInt =
        (0 until 4).map(i => BigInt(w(i) & 0x3F) << (8*i)).reduce(_ | _)

      def packW_fp6_e2m3_single(w: Int): BigInt = BigInt(w & 0x3F)

      def packW_fp8_x4(w: Seq[Int]): BigInt =
        (0 until 4).map(i => BigInt(w(i) & 0xFF) << (8*i)).reduce(_ | _)

      def packW_fp8_e4m3_single(w: Int): BigInt = BigInt(w & 0xFF)

      // -------- Common driver/printing --------
      def driveAndPrintCase(
        label: String,
        a0_nib: Int, a1_nib: Int,
        wType: Int, wAlt: Boolean, wPacked: BigInt, wDesc: String,
        cRaw: BigInt, cDesc: String
      ): Unit = {
        val inA = packActsFp4(a0_nib, a1_nib) & mask(aW)

        h.io.in_a_type.poke(0.U)        // fp4 activations
        h.io.a_altfmt.poke(false.B)
        h.io.in_activation.poke(inA.U(aW.W))
        h.io.enable.poke(true.B)

        h.io.in_w_type.poke(wType.U)
        h.io.w_altfmt.poke(wAlt.B)
        h.io.in_weights.poke(wPacked.U(wW.W))

        h.io.c_raw.poke(cRaw.U(16.W))

        val a0s = f"0x$a0_nib%X"; val a1s = f"0x$a1_nib%X"
        println(s"\n=== $label ===")
        println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=$a0s  a1=$a1s")
        println(s"in_weights            = ${binStr(wPacked, wW)}   $wDesc")
        println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}   $cDesc")
        println(s"(a_type=fp4, w_type=$wType, w_altfmt=${if (wAlt) 1 else 0})")

        h.clock.step(1)

        val recApplied = h.io.rec_c_applied.peek().litValue
        val outBits    = h.io.out.peek().litValue
        println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
        println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
        (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
      }

      // -------- C values we'll reuse --------
      val C_ZERO  = e8m7Raw(0, 0x00, 0x00) // +0.0
      val C_ONE   = e8m7Raw(0, 0x7F, 0x00) // +1.0
      val C_NEG1  = e8m7Raw(1, 0x7F, 0x00) // -1.0

      // ===================================================================================
      // CASE A: fp4 × fp4 (4 outputs). Two activation sets × two C values × different Ws.
      // ===================================================================================
      val wA_0 = Seq(0x2, 0x3, 0x4, 0x1) // simple ascending
      val wA_1 = Seq(0x5, 0x4, 0x3, 0x1) // different mix

      aSet.foreach { case (aname, a0, a1) =>
        driveAndPrintCase(
          label = s"Case A1 ($aname): fp4×fp4; C=0.0",
          a0_nib = a0, a1_nib = a1,
          wType = 0, wAlt = false, wPacked = packW_fp4x4(wA_0),
          wDesc = f"fp4x4 w0..3=${wA_0.map(x => f"0x$x%X").mkString(" ")}",
          cRaw = C_ZERO, cDesc = "C = +0.0"
        )
        driveAndPrintCase(
          label = s"Case A2 ($aname): fp4×fp4; C=+1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 0, wAlt = false, wPacked = packW_fp4x4(wA_1),
          wDesc = f"fp4x4 w0..3=${wA_1.map(x => f"0x$x%X").mkString(" ")}",
          cRaw = C_ONE, cDesc = "C = +1.0"
        )
      }

      // ===================================================================================
      // CASE B: fp4 × fp6 (E2M3 single-weight). Two weights + two C values.
      // (Here we set w_altfmt=true to indicate single-weight style.)
      // ===================================================================================
      val wB_single0 = enc(FP6_E2M3, 0, 2, 1) & 0x3F  // e=2,m=1
      val wB_single1 = enc(FP6_E2M3, 0, 1, 3) & 0x3F  // e=1,m=3 (smaller)
      aSet.foreach { case (aname, a0, a1) =>
        driveAndPrintCase(
          label = s"Case B1 ($aname): fp4×fp6(E2M3 single); C=0.0",
          a0_nib = a0, a1_nib = a1,
          wType = 1, wAlt = false, wPacked = packW_fp6_e2m3_single(wB_single0),
          wDesc = f"fp6(E2M3) single w0=0x$wB_single0%02X",
          cRaw = C_ZERO, cDesc = "C = +0.0"
        )
        driveAndPrintCase(
          label = s"Case B2 ($aname): fp4×fp6(E2M3 single); C=+1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 1, wAlt = false, wPacked = packW_fp6_e2m3_single(wB_single1),
          wDesc = f"fp6(E2M3) single w0=0x$wB_single1%02X",
          cRaw = C_ONE, cDesc = "C = +1.0"
        )
      }

      // ===================================================================================
      // CASE C: fp4 × fp6 (E3M2 4-lane). One vector + two C values.
      // (Here we set w_altfmt=false for E3M2 vector mode.)
      // ===================================================================================
      val wC_vec = Seq(
        enc(FP6_E3M2, 0, 4, 1) & 0x3F, // unbiased +1
        enc(FP6_E3M2, 0, 3, 2) & 0x3F, // unbiased  0
        enc(FP6_E3M2, 0, 2, 0) & 0x3F, // unbiased -1
        enc(FP6_E3M2, 0, 5, 3) & 0x3F  // unbiased +2
      )
      aSet.foreach { case (aname, a0, a1) =>
        driveAndPrintCase(
          label = s"Case C1 ($aname): fp4×fp6(E3M2 vec4); C=0.0",
          a0_nib = a0, a1_nib = a1,
          wType = 1, wAlt = true, wPacked = packW_fp6_e3m2_x4(wC_vec),
          wDesc = f"fp6(E3M2) vec4 w=${wC_vec.map(x => f"0x$x%02X").mkString(" ")}",
          cRaw = C_ZERO, cDesc = "C = +0.0"
        )
        driveAndPrintCase(
          label = s"Case C2 ($aname): fp4×fp6(E3M2 vec4); C=+1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 1, wAlt = true, wPacked = packW_fp6_e3m2_x4(wC_vec.reverse),
          wDesc = f"fp6(E3M2) vec4 w=${wC_vec.reverse.map(x => f"0x$x%02X").mkString(" ")}",
          cRaw = C_ONE, cDesc = "C = +1.0"
        )
      }

      // ===================================================================================
      // CASE D: fp4 × fp8 (E5M2 4-lane).
      // ===================================================================================
      val wD_vec = Seq(
        enc(FP8_E5M2, 0, 16, 0) & 0xFF, // +1
        enc(FP8_E5M2, 0, 15, 1) & 0xFF, //  0
        enc(FP8_E5M2, 0, 17, 2) & 0xFF, // +2
        enc(FP8_E5M2, 0, 14, 3) & 0xFF  // -1
      )
      aSet.foreach { case (aname, a0, a1) =>
        driveAndPrintCase(
          label = s"Case D1 ($aname): fp4×fp8(E5M2 vec4); C=+1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 2, wAlt = true, wPacked = packW_fp8_x4(wD_vec),
          wDesc = f"fp8(E5M2) vec4 w=${wD_vec.map(x => f"0x$x%02X").mkString(" ")}",
          cRaw = C_ONE, cDesc = "C = +1.0"
        )
        driveAndPrintCase(
          label = s"Case D2 ($aname): fp4×fp8(E5M2 vec4); C=-1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 2, wAlt = true, wPacked = packW_fp8_x4(wD_vec.reverse),
          wDesc = f"fp8(E5M2) vec4 w=${wD_vec.reverse.map(x => f"0x$x%02X").mkString(" ")}",
          cRaw = C_NEG1, cDesc = "C = -1.0"
        )
      }

      // ===================================================================================
      // CASE E: fp4 × fp8 (E4M3 single-weight).
      // ===================================================================================
      val wE_single0 = enc(FP8_E4M3, 0, 8, 1) & 0xFF
      val wE_single1 = enc(FP8_E4M3, 0, 7, 3) & 0xFF
      aSet.foreach { case (aname, a0, a1) =>
        driveAndPrintCase(
          label = s"Case E1 ($aname): fp4×fp8(E4M3 single); C=0.0",
          a0_nib = a0, a1_nib = a1,
          wType = 2, wAlt = false, wPacked = packW_fp8_e4m3_single(wE_single0),
          wDesc = f"fp8(E4M3) single w0=0x$wE_single0%02X",
          cRaw = C_ZERO, cDesc = "C = +0.0"
        )
        driveAndPrintCase(
          label = s"Case E2 ($aname): fp4×fp8(E4M3 single); C=+1.0",
          a0_nib = a0, a1_nib = a1,
          wType = 2, wAlt = false, wPacked = packW_fp8_e4m3_single(wE_single1),
          wDesc = f"fp8(E4M3) single w0=0x$wE_single1%02X",
          cRaw = C_ONE, cDesc = "C = +1.0"
        )
      }

      // Done
      h.io.enable.poke(false.B)
      h.clock.step(1)
    }
  }
}
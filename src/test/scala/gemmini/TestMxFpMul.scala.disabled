package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

import gemmini.MxFpMul

/** Harness that mirrors *new* MxFpMul IO exactly, but:
  *  - Accepts BF16 raw C (E8M7, 16b) and recodes to recFN(8,8) to drive dut.io.rec_c
  *  - Exposes dut.io.out as 4×BF16 raw packed (64b) for easy checking/printing
  */
class MxFpMulHarnessBf16Out_NewIO(lut: Boolean, fpProductPrecision: (Int, Int), fpAccPrecision: gemmini.MxFloat)
    extends Module {

  val dut = Module(new MxFpMul(lut)(fpProductPrecision, fpAccPrecision))

  val io = IO(new Bundle {
    val in_activation = Input(UInt(dut.io.in_activation.getWidth.W))
    val type_a        = Input(chiselTypeOf(dut.io.type_a))
    val in_weights    = Input(UInt(dut.io.in_weights.getWidth.W))
    val type_w        = Input(chiselTypeOf(dut.io.type_w))
    val mode          = Input(chiselTypeOf(dut.io.mode))
    val enable        = Input(Bool())

    // BF16 raw C (E8M7): 1|8|7 = 16b
    val c_raw         = Input(UInt(16.W))

    // 4 × BF16 packed LSB-first (lane0 in [15:0])
    val out_bf16      = Output(UInt(64.W))

    // Raw recFN(8,8) output from DUT: 4 × 17b = 68b, lane0 at [16:0]
    val out_recfn     = Output(UInt(dut.io.out.getWidth.W))

    // Observe what rec_c actually got applied
    val rec_c_applied = Output(UInt(dut.io.rec_c.getWidth.W))
  })

  // printf(p"type_a: ${io.type_a}, type_w: ${io.type_w}, mode\n")
  val computedMode = requiredPEMode(io.type_a, io.type_w)
  dut.io.mode := computedMode

  // Pass-through
  dut.io.in_activation := io.in_activation
  dut.io.type_a        := io.type_a
  dut.io.in_weights    := io.in_weights
  dut.io.type_w        := io.type_w
//   dut.io.mode          := io.mode
  dut.io.enable        := io.enable

  // BF16 -> recFN(8,8) (17b)
  private val recC = hardfloat.recFNFromFN(8, 8, io.c_raw)

  // Size-match to dut.io.rec_c width
  private val wantW = dut.io.rec_c.getWidth
  private val haveW = recC.getWidth
  private val recSized =
    if (haveW == wantW) recC
    else if (haveW > wantW) recC(wantW - 1, 0)
    else Cat(recC, recC, recC, recC)

  dut.io.rec_c := recSized
  io.rec_c_applied := dut.io.rec_c

  // Convert dut.io.out lanes to BF16 raw
  private val dutW = dut.io.out.getWidth
  require(dutW % 4 == 0, s"DUT out width ($dutW) must be divisible by 4")
  private val laneW = dutW / 4

  val lanes = Wire(Vec(4, UInt(laneW.W)))
  for (i <- 0 until 4) {
    val hi = (i + 1) * laneW - 1
    val lo = i * laneW
    lanes(i) := dut.io.out(hi, lo)
  }

  val lanesBF16 = Wire(Vec(4, UInt(16.W)))
  if (laneW == 17) {
    for (i <- 0 until 4) lanesBF16(i) := hardfloat.fNFromRecFN(8, 8, lanes(i))
  } else {
    require(laneW == 16, s"Expected lane width 16 (BF16) or 17 (recFN), got $laneW")
    for (i <- 0 until 4) lanesBF16(i) := lanes(i)(15,0)
  }

  io.out_bf16  := Cat(lanesBF16.reverse) // lane0 at [15:0]
  io.out_recfn := dut.io.out             // raw 4×17b recFN, lane0 at [16:0]
}

class MxFpMul_AllATypes_BF16Out_SelfChecking_NewIO_Spec
  extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "MxFpMul — NEW IO; BF16 IEEE output; FP4/FP6=2x2->4 lanes; FP8=1x1->lane0; self-checking"

  it should "run randomized combos and self-check lanes (verbose pre/post prints)" in {

    // NOTE: You must instantiate with your desired precisions.
    // If you already have these in your project test params, plug them in here.
    val fpProductPrecision = (8, 8) // (expWidth, sigWidth) for product format via MxFormats(...)
    val fpAccPrecision     = gemmini.MxFloat(8, 8, 4, true, false) // adjust to your actual MxFloat ctor

    test(new MxFpMulHarnessBf16Out_NewIO(lut = false, fpProductPrecision, fpAccPrecision))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        // ---------- small-format helpers (positive-only) ----------
        case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
          val expMask = (1 << eBits) - 1
          val mantMask= (1 << mBits) - 1
          def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask) // sign=0
        }
        val FP4_E2M1 = MiniFmt(2,1, bias=1)
        val FP6_E2M3 = MiniFmt(2,3, bias=1)   // altfmt = 0
        val FP6_E3M2 = MiniFmt(3,2, bias=3)   // altfmt = 1
        val FP8_E4M3 = MiniFmt(4,3, bias=7)   // altfmt = 0
        val FP8_E5M2 = MiniFmt(5,2, bias=15)  // altfmt = 1

        def decodeSmall(fmt: MiniFmt, raw: Int): scala.Float = {
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

        // ---------- BF16 helpers ----------
        def bf16ToFloat(raw16: Int): scala.Float = java.lang.Float.intBitsToFloat(raw16 << 16)
        def floatToBf16Raw(f: scala.Float): Int = {
          val bits = java.lang.Float.floatToRawIntBits(f)
          val lsb  = (bits >>> 16) & 1
          val rnd  = bits + (0x7FFF + lsb)     // RNE to 16 MSBs
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

        val rng = new Random(0xBEEFBABE)
        def genSmall(fmt: MiniFmt): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) fmt.enc(0, 0)                                  // +0
          else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))  // subnormal
          else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),         // normal
                       rng.nextInt(fmt.mantMask + 1))
        }
        def genBF16(): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) 0x0000
          else if (r < 0.20f) (0x0001 + rng.nextInt(0x7F))
          else {
            val mag = math.pow(2.0, rng.nextInt(8) - 4).toFloat
            val base= rng.nextFloat() * mag
            floatToBf16Raw(base)
          }
        }

        val aW = h.io.in_activation.getWidth
        val wW = h.io.in_weights.getWidth
        val laneW = 16 // BF16 lanes on the harness output

        def pack2IntoHalves(raw0: Int, raw1: Int, elemBits: Int, totalW: Int): BigInt = {
            val half = totalW / 2
            require(totalW % 2 == 0, s"totalW=$totalW must be even")
            require(elemBits <= half, s"elemBits=$elemBits must fit in half=$half")
            val m = (1 << elemBits) - 1
            (BigInt(raw0 & m)) | (BigInt(raw1 & m) << half)
        }

        // ------------------------------------------------------------------
        // NEW PACK RULES:
        //   FP4: 2 acts (fp4) and 2 weights (fp4) => 4 outputs
        //   FP6: 2 acts (fp6) and 2 weights (fp6) => 4 outputs
        //   FP8: 1 act (fp8) and 1 weight (fp8) => only lane0 defined
        //
        // NOTE: We keep (aType,wType,alt) in the test like before.
        // ------------------------------------------------------------------

        def packActs(aType: Int, aAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
            val totalW = aW   // h.io.in_activation.getWidth
            aType match {
                case 0 => // fp4: still 2 values, each placed in its half-slot
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
                val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
                (packed, vals, f"fp4 acts: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 => // fp6: 2 values, each in its half-slot (6 bits each)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
                val fmt    = if (aAlt) FP6_E3M2 else FP6_E2M3
                val vals   = raws.map(r => decodeSmall(fmt, r & 0x3F))
                val nm     = if (aAlt) "fp6 E3M2" else "fp6 E2M3"
                (packed, vals, s"$nm acts: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 2 => // fp8: 1 value goes at bit 0, rest padding
                require(raws.length == 1)
                val a0     = raws.head & 0xFF
                val packed = BigInt(a0) // stays at [7:0], upper bits are padding
                val fmt    = if (aAlt) FP8_E5M2 else FP8_E4M3
                val vals   = Seq(decodeSmall(fmt, a0))
                val nm     = if (aAlt) "fp8 E5M2" else "fp8 E4M3"
                (packed, vals, f"$nm act: 0x$a0%02X -> ${vals.head}")
            }
        }

        def packWeis(wType: Int, wAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
            val totalW = wW // h.io.in_weights.getWidth
            wType match {
                case 0 => // fp4 weights: 2 values in half-slots
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
                val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
                (packed, vals, f"fp4 weis: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 1 => // fp6 weights: 2 values in half-slots (6b)
                require(raws.length == 2)
                val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
                val fmt    = if (wAlt) FP6_E3M2 else FP6_E2M3
                val vals   = raws.map(r => decodeSmall(fmt, r & 0x3F))
                val nm     = if (wAlt) "fp6 E3M2" else "fp6 E2M3"
                (packed, vals, s"$nm weis: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

                case 2 => // fp8 weights: 1 value at bit0
                require(raws.length == 1)
                val w0     = raws.head & 0xFF
                val packed = BigInt(w0)
                val fmt    = if (wAlt) FP8_E5M2 else FP8_E4M3
                val vals   = Seq(decodeSmall(fmt, w0))
                val nm     = if (wAlt) "fp8 E5M2" else "fp8 E4M3"
                (packed, vals, f"$nm wei: 0x$w0%02X -> ${vals.head}")
            }
        }

        // Expected lane mapping under NEW rules
        //  - For 2 acts × 2 weis: lane0=a0*w0, lane1=a0*w1, lane2=a1*w0, lane3=a1*w1
        //  - For 1×1: lane0 only
        def expectedBF16Lane(aVals: Seq[scala.Float], wVals: Seq[scala.Float], c: scala.Float, lane: Int): Option[Int] = {
          def toBF16(x: scala.Float) = floatToBf16Raw(x)
          (aVals.length, wVals.length) match {
            case (2,2) =>
              val pairs = Array((0,0), (0,1), (1,0), (1,1))
              val (ai, wi) = pairs(lane)
              Some(toBF16(aVals(ai) * wVals(wi) + c))

            case (1,1) =>
              if (lane == 0) Some(toBF16(aVals(0) * wVals(0) + c)) else None

            case _ =>
              None
          }
        }

        // Variants: keep same semantic knobs (type + altfmt), but NEW lane counts
        val aVariants = Seq(
          //("A: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
          // ("A: 2×fp6 (E2M3 alt0)", 1, false, () => Seq(genSmall(FP6_E2M3), genSmall(FP6_E2M3))),
          ("A: 2×fp6 (E3M2 alt1)", 1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2)))
          // ("A: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3))),
          // // ("A: 1×fp8 (E5M2 alt1)", 2, true,  () => Seq(genSmall(FP8_E5M2)))
        )
        val wVariants = Seq(
          // ("W: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
          // ("W: 2×fp6 (E2M3 alt0)", 1, false, () => Seq(genSmall(FP6_E2M3), genSmall(FP6_E2M3))),
          ("W: 2×fp6 (E3M2 alt1)", 1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2)))
          // ("W: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3))),
          // // ("W: 1×fp8 (E5M2 alt1)", 2, true,  () => Seq(genSmall(FP8_E5M2)))
        )

        val trialsPerCombo = 1
        h.io.enable.poke(true.B)

        // ------------------------------------------------------------------
        // TODO: map (aType,aAlt,wType,wAlt) into YOUR actual Bundle fields:
        //   - h.io.type_a : MxTypes()
        //   - h.io.type_w : MxTypes()
        //   - h.io.mode   : mxMode()
        //
        // Replace the bodies below with your real field pokes.
        // ------------------------------------------------------------------
        def pokeTypesAndMode(aType: Int, aAlt: Boolean, wType: Int, wAlt: Boolean): Unit = {
            // aType/wType are the mx_format encodings used in your RTL:
            //   0 -> FP4
            //   1 -> FP6
            //   2 -> FP8
            //
            // aAlt/wAlt are NOT used by the current RTL hookup (typeA/typeW only depend on mx_format),
            // but we keep them in the signature so the rest of the test stays the same.

            def expSigFromMxFormat(fmt: Int): (Int, Int) = fmt match {
                case 0 => (2, 2) // FP8? (as in your snippet)
                case 1 => (3, 3) // FP6
                case 2 => (4, 4) // FP4
                case other => throw new IllegalArgumentException(s"bad mx_format=$other")
            }

            val (aExp, aSig) = expSigFromMxFormat(aType)
            val (wExp, wSig) = expSigFromMxFormat(wType)

            // Poke type_a/type_w (MxTypes has fields exp/sig per your snippet)
            h.io.type_a.exp.poke(aExp.U)
            h.io.type_a.sig.poke(aSig.U)

            h.io.type_w.exp.poke(wExp.U)
            h.io.type_w.sig.poke(wSig.U)


        }

        for ((aName, aType, aAlt, aGen) <- aVariants) {
          for ((wName, wType, wAlt, wGen) <- wVariants) {
            println(s"\n==== Combo: $aName  vs  $wName  (a_type=$aType alt=${if(aAlt)1 else 0}; w_type=$wType alt=${if(wAlt)1 else 0}) ====")
            if (aType == wType) {
              for (t <- 0 until trialsPerCombo) {
                // --------------------- Generate stimuli ---------------------
                val aRaws = aGen()
                val (aPacked, aVals, aDesc) = packActs(aType, aAlt, aRaws)

                val wRaws = wGen()
                val (wPacked, wVals, wDesc) = packWeis(wType, wAlt, wRaws)

                // Under your new throughput rules, mixed fp8 with fp4/fp6 is probably invalid.
                // We still run combos, but expectations only apply when lane rules match.
                val cRaw  = genBF16()
                val cVal  = bf16ToFloat(cRaw)

                // --------------------- PRE-TEST PRINTS ----------------------
                println(f"-- trial #$t%02d  PRE")
                println(s"  in_activation  (${aW}b) = ${binStr(aPacked, aW)}   $aDesc")
                println(s"  in_weights     (${wW}b) = ${binStr(wPacked, wW)}   $wDesc")
                showBF16("  c_raw (BF16)           ", cRaw)

                val expOpt = Array.tabulate(4)(i => expectedBF16Lane(aVals, wVals, cVal, i))
                (0 until 4).foreach { i =>
                  expOpt(i) match {
                    case Some(e) => showBF16(f"  exp lane[$i]", e)
                    case None    => println(f"  exp lane[$i]: (n/a)")
                  }
                }

                // --------------------- Drive DUT ----------------------------
                pokeTypesAndMode(aType, aAlt, wType, wAlt)
                h.io.in_activation.poke(aPacked.U(aW.W))
                h.io.in_weights.poke(wPacked.U(wW.W))
                h.io.c_raw.poke(cRaw.U(16.W))

                // latency cushion
                h.clock.step(2)

                // --------------------- POST-STEP PRINTS (before asserts) ----
                val outBits = h.io.out_bf16.peek().litValue
                println(s"  out_bf16 (${4*laneW}b)      = ${binStr(outBits, 4*laneW)}")
                val got = Array.tabulate(4)(i => lane(outBits, i, laneW))
                (0 until 4).foreach(i => showBF16(f"  got lane[$i]", got(i)))

                // --------------------- ASSERTIONS ---------------------------
                val filledExp = Array.tabulate(4) { i => expOpt(i).getOrElse(got(i)) }
                val packedExp =
                  (BigInt(filledExp(3) & 0xFFFF) << 48) |
                  (BigInt(filledExp(2) & 0xFFFF) << 32) |
                  (BigInt(filledExp(1) & 0xFFFF) << 16) |
                  BigInt(filledExp(0) & 0xFFFF)

                expOpt.zipWithIndex.foreach {
                  case (Some(e), i) =>
                    assert(got(i) == e, f"lane[$i] mismatch: got 0x${got(i)}%04X exp 0x$e%04X")
                  case _ => // don't-care
                }

                h.io.out_bf16.expect(packedExp.U, s"trial $t packed expect mismatch")
                println(s"  RESULT: PASS (trial $t)")
              }
            }
          }
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)
      }
  }

  // -----------------------------------------------------------------------
  // User-defined 12-bit activation / weight probe
  // Edit `userCases` below to inject any raw 12-bit packed values and read
  // the 4-lane BF16 output.
  //
  // Packing convention (FP6 E3M2, same as the randomised test above):
  //   in_activation / in_weights are split into two halves by pack2IntoHalves:
  //     bits [half-1 : 0]      -> element 0
  //     bits [totalW-1 : half] -> element 1
  //   where half = portWidth / 2.
  //
  //   For a 12-bit port: half = 6, element 0 in [5:0], element 1 in [11:6].
  //   For a 16-bit port: half = 8, element 0 in [5:0] (zero-padded to 8b),
  //                               element 1 in [13:8] (zero-padded to 8b).
  //
  // c_raw is a raw BF16 (E8M7) integer, e.g. 0x3F80 = 1.0, 0x0000 = 0.0.
  // -----------------------------------------------------------------------
  it should "run user-defined 12-bit activation and weight inputs and print lane outputs" in {

    val fpProductPrecision = (8, 8)
    val fpAccPrecision     = gemmini.MxFloat(8, 8, 4, true, false)

    test(new MxFpMulHarnessBf16Out_NewIO(lut = false, fpProductPrecision, fpAccPrecision))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        // ---- helpers (same as above) ----
        def bf16ToFloat(raw16: Int): scala.Float = java.lang.Float.intBitsToFloat(raw16 << 16)
        def binStr(x: BigInt, w: Int): String = {
          val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
        }
        def laneVal(bits: BigInt, idx: Int, laneW: Int): Int =
          ((bits >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)).toInt
        def showBF16(tag: String, v: Int): Unit = {
          val s = (v >>> 15) & 1
          val e = (v >>> 7)  & 0xFF
          val f = v & 0x7F
          println(f"$tag: 0x$v%04X  s=$s e=0x$e%02X f=0x$f%02X  (~=${bf16ToFloat(v)}%g)")
        }

        val aW    = h.io.in_activation.getWidth
        val wW    = h.io.in_weights.getWidth
        val outLaneW = 16 // BF16 output lanes

        // ====================================================================
        // USER INPUTS — edit this table to test your own values.
        //
        // Each row: (label, activation_raw, weight_raw, c_raw_bf16)
        //   activation_raw : BigInt — raw bits for in_activation (12 b or aW b)
        //   weight_raw     : BigInt — raw bits for in_weights    (12 b or wW b)
        //   c_raw_bf16     : Int    — raw BF16 integer for the accumulate input
        // ====================================================================
        val userCases: Seq[(String, BigInt, BigInt, Int)] = Seq(
          // label            activation (12b)   weight (12b)    c_raw (BF16)
          ("user case 0",  BigInt("B6D", 16),  BigInt("02D", 16),  0x0000),  // c = 1.0
          //("user case 1",  BigInt("041", 16),  BigInt("041", 16),  0x0000),  // c = 0.0
          //("user case 2",  BigInt("FFF", 16),  BigInt("000", 16),  0x0000),  // max act, zero wei
        )

    
        h.io.type_a.exp.poke(3.U)
        h.io.type_a.sig.poke(3.U)
        h.io.type_w.exp.poke(3.U)
        h.io.type_w.sig.poke(3.U)
        h.io.enable.poke(true.B)

        for ((label, actRaw, weiRaw, cRaw) <- userCases) {
          println(s"\n==== $label ====")
          println(s"  in_activation (${aW}b) = ${binStr(actRaw, aW)}   hex=0x${actRaw.toString(16).toUpperCase}")
          println(s"  in_weights    (${wW}b) = ${binStr(weiRaw, wW)}   hex=0x${weiRaw.toString(16).toUpperCase}")
          showBF16("  c_raw (BF16)         ", cRaw)

          h.io.in_activation.poke(actRaw.U(aW.W))
          h.io.in_weights.poke(weiRaw.U(wW.W))
          h.io.c_raw.poke(cRaw.U(16.W))

          h.clock.step(2)

          // --- raw recFN(8,8) per lane: sign(1)|exp_recoded(9)|mant(7) = 17b ---
          val recFnBits  = h.io.out_recfn.peek().litValue
          val recFnLaneW = h.io.out_recfn.getWidth / 4   // 17
          println(s"  out_recfn (${h.io.out_recfn.getWidth}b) = ${binStr(recFnBits, h.io.out_recfn.getWidth)}")
          (0 until 4).foreach { i =>
            val rv   = laneVal(recFnBits, i, recFnLaneW)
            val sign = (rv >> (recFnLaneW - 1)) & 1
            val exp  = (rv >> 7) & 0x1FF   // bits [15:7] — 9-bit recoded exp
            val mant = rv & 0x7F           // bits [6:0]  — 7-bit mantissa
            println(f"  recFN lane[$i]: 0x${rv}%05X  s=$sign exp=0x$exp%03X(${exp}d) mant=0x$mant%02X")
          }

          // --- BF16 converted output ---
          val outBits = h.io.out_bf16.peek().litValue
          println(s"  out_bf16  (${4 * outLaneW}b) = ${binStr(outBits, 4 * outLaneW)}")
          val got = Array.tabulate(4)(i => laneVal(outBits, i, outLaneW))
          (0 until 4).foreach(i => showBF16(f"  BF16 lane[$i]", got(i)))
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)
      }
  }
}

// // ----------------------------- Test -----------------------------
// class MxFpMul_Fp4_WithRecC_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

//   behavior of "MxFpMul — fp4 activations; rec_c via E8M7; OUT converted to raw E8M7 per lane"

//   it should "run two fp4-mode cases and print inputs/outputs as raw E8M7" in {
//     val ts = TypeSupport(
//       actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
//       weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
//     )

//     test(new MxFpMulHarness(ts, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { h =>
//       // ---- helpers ----
//       def mask(w: Int) = (BigInt(1) << w) - 1
//       def binStr(x: BigInt, w: Int): String = {
//         val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
//       }
//       def lane(x: BigInt, idx: Int, laneW: Int): BigInt =
//         (x >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)

//       // Decode raw E8M7 lane (16b)
//       def showLane16(tag: String, bits16: BigInt): Unit = {
//         val s   = ((bits16 >> 15) & 0x1).toInt
//         val exp = ((bits16 >> 7)  & 0xFF).toInt
//         val frac= (bits16 & 0x7F).toInt
//         println(f"$tag: raw16=0x${bits16}%04x  s=$s exp=0x$exp%02x frac=0x$frac%02x")
//       }

//       // Pack raw E8M7: s(1) | exp(8) | frac(7)
//       def e8m7Raw(s: Int, e: Int, f: Int): BigInt = {
//         require(s == 0 || s == 1); require(e >= 0 && e < 256); require(f >= 0 && f < 128)
//         (BigInt(s) << 15) | (BigInt(e) << 7) | BigInt(f)
//       }

//       // Mini encoders for small formats (just for making fp4/fp6 stimuli)
//       case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
//         val sBits = 1
//         def expMask = (1 << eBits) - 1
//         def mantMask = (1 << mBits) - 1
//       }
//       val FP4_E2M1 = MiniFmt(2,1, bias=1)
//       val FP6_E2M3 = MiniFmt(2,3, bias=1)

//       def enc(fmt: MiniFmt, s: Int, e: Int, m: Int): Int = {
//         require((s == 0 || s == 1) && (e & ~fmt.expMask) == 0 && (m & ~fmt.mantMask) == 0)
//         (s << (fmt.eBits + fmt.mBits)) | (e << fmt.mBits) | m
//       }

//       // ---- widths (note: io.out is now 64b = 4×16) ----
//       val aW    = h.io.in_activation.getWidth
//       val wW    = h.io.in_weights.getWidth
//       val outW  = h.io.out.getWidth           // expect 64 (4×16)
//       val laneW = outW / 4                    // 16

//       // ---- fixed fp4 activations (2 lanes) ----
//       val a0 = enc(FP4_E2M1, 0, 0, 1) & 0xF   // 0_10_1 = 0x5
//       val a1 = enc(FP4_E2M1, 0, 0, 1) & 0xF   // 0_01_0 = 0x2
//       val inA: BigInt = (BigInt(a1) << 8) | BigInt(a0)

//       // Common settings
//       h.io.in_a_type.poke(0.U)     // fp4
//       h.io.a_altfmt.poke(false.B)
//       h.io.in_activation.poke(inA.U(aW.W))
//       h.io.enable.poke(true.B)

//       // ======================================================
//       // Case A: fp4 × fp4, c_raw = 0.0  → expect 4 lanes used
//       // ======================================================
//       {
//         val w0 = 0x2; val w1 = 0x3; val w2 = 0x4; val w3 = 0x1
//         val inW = (BigInt(w0 & 0xF) << 0)  |
//                   (BigInt(w1 & 0xF) << 8)  |
//                   (BigInt(w2 & 0xF) << 16) |
//                   (BigInt(w3 & 0xF) << 24)
//         h.io.in_w_type.poke(0.U)     // fp4
//         h.io.w_altfmt.poke(false.B)
//         h.io.in_weights.poke(inW.U(wW.W))

//         val cRaw = e8m7Raw(s = 0, e = 0x00, f = 0x00) // 0.0 in E8M7 raw
//         h.io.c_raw.poke(cRaw.U(16.W))

//         println("\n=== Case A: fp4(act) × fp4(wei), alt=0; c_raw(E8M7)=0.0 ===")
//         println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=0x$a0  a1=0x$a1")
//         println(s"in_weights    (fp4x4) = ${binStr(inW, wW)}   w=0x$w0 0x$w1 0x$w2 0x$w3")
//         println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}")

//         h.clock.step(1)

//         val recApplied = h.io.rec_c_applied.peek().litValue
//         val outBits    = h.io.out.peek().litValue
//         println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
//         println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
//         (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
//       }

//       // ==================================================================
//       // Case B: fp4 × fp6 (E2M3 single-weight), c_raw = 1.0 → 2 lanes used
//       // ==================================================================
//       {
//         val w_single = enc(FP6_E2M3, s=0, e=2, m=1) & 0x3F
//         val inW = BigInt(w_single) // low 6 bits of lowest slot
//         h.io.in_w_type.poke(1.U)    // fp6
//         h.io.w_altfmt.poke(false.B)  // E2M3 single-weight mode
//         h.io.in_weights.poke(inW.U(wW.W))

//         // val cRaw = e8m7Raw(s = 0, e = 0x7F, f = 0x00) // 1.0 in E8M7 raw
//         val cRaw = e8m7Raw(s = 0, e = 0x00, f = 0x00) // 0.0 in E8M7 raw
//         h.io.c_raw.poke(cRaw.U(16.W))

//         println("\n=== Case B: fp4(act) × fp6(E2M3 single), alt=1; c_raw(E8M7)=1.0 ===")
//         println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=0x$a0  a1=0x$a1")
//         println(s"in_weights    (fp6 1)  = ${binStr(inW, wW)}   w0(E2M3)=0x${w_single}")
//         println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}")

//         h.clock.step(1)

//         val recApplied = h.io.rec_c_applied.peek().litValue
//         val outBits    = h.io.out.peek().litValue
//         println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
//         println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
//         (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
//       }

//       h.io.enable.poke(false.B)
//       h.clock.step(1)
//     }
//   }
// }

// class MxFpMul_Fp4_WithRecC_MoreCoverage_Spec
//     extends AnyFlatSpec
//     with ChiselScalatestTester
//     with Matchers {

//   behavior of "MxFpMul — fp4 activations; print-heavy coverage across fp4/fp6/fp8 weights"

//   it should "exercise multiple stimuli per case and print lane-wise raw E8M7" in {
//     val ts = TypeSupport(
//       actSupportFp4 = true, actSupportFp6 = true, actSupportFp8 = true,
//       weiSupportFp4 = true, weiSupportFp6 = true, weiSupportFp8 = true
//     )

//     test(new MxFpMulHarness(ts, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { h =>
//       // ---------- helpers ----------
//       def mask(w: Int) = (BigInt(1) << w) - 1
//       def binStr(x: BigInt, w: Int): String = {
//         val s = x.toString(2); "b" + ("0" * (w - s.length)) + s
//       }
//       def lane(x: BigInt, idx: Int, laneW: Int): BigInt =
//         (x >> (idx * laneW)) & ((BigInt(1) << laneW) - 1)

//       def showLane16(tag: String, bits16: BigInt): Unit = {
//         val s   = ((bits16 >> 15) & 0x1).toInt
//         val exp = ((bits16 >> 7)  & 0xFF).toInt
//         val frac= (bits16 & 0x7F).toInt
//         println(f"$tag: raw16=0x${bits16}%04x  s=$s exp=0x$exp%02x frac=0x$frac%02x")
//       }

//       // Pack raw E8M7: s(1) | exp(8) | frac(7)
//       def e8m7Raw(s: Int, e: Int, f: Int): BigInt = {
//         require(s == 0 || s == 1); require(e >= 0 && e < 256); require(f >= 0 && f < 128)
//         (BigInt(s) << 15) | (BigInt(e) << 7) | BigInt(f)
//       }

//       // Tiny encoders for mini formats to craft stimuli (sign=0 unless noted)
//       case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
//         val sBits = 1
//         def expMask = (1 << eBits) - 1
//         def mantMask = (1 << mBits) - 1
//       }
//       val FP4_E2M1 = MiniFmt(2,1, bias=1)
//       val FP6_E2M3 = MiniFmt(2,3, bias=1)   // “single-weight” style vectors
//       val FP6_E3M2 = MiniFmt(3,2, bias=3)   // 4-lane vectors
//       val FP8_E5M2 = MiniFmt(5,2, bias=15)  // 4-lane vectors
//       val FP8_E4M3 = MiniFmt(4,3, bias=7)   // “single-weight” style vectors

//       def enc(fmt: MiniFmt, s: Int, e: Int, m: Int): Int = {
//         require((s == 0 || s == 1) && (e & ~fmt.expMask) == 0 && (m & ~fmt.mantMask) == 0)
//         (s << (fmt.eBits + fmt.mBits)) | (e << fmt.mBits) | m
//       }

//       // Widths
//       val aW    = h.io.in_activation.getWidth
//       val wW    = h.io.in_weights.getWidth
//       val outW  = h.io.out.getWidth           // expect 64 (4×16)
//       val laneW = outW / 4                    // 16

//       // -------- Activation packers (we keep fp4 activations) --------
//       // Two fp4 lanes in 16b port: lane0 nibble -> [3:0], lane1 nibble -> [11:8]
//       def packActsFp4(a0_nib: Int, a1_nib: Int): BigInt =
//         (BigInt(a1_nib & 0xF) << 8) | BigInt(a0_nib & 0xF)

//       // fp4 activation sets (normalized, positive)
//       val aSet = Seq(
//         ("Aset0", enc(FP4_E2M1, 0, 2, 1) & 0xF, enc(FP4_E2M1, 0, 1, 0) & 0xF), // a0=0x5, a1=0x2
//         ("Aset1", enc(FP4_E2M1, 0, 2, 0) & 0xF, enc(FP4_E2M1, 0, 1, 1) & 0xF)  // a0=0x4, a1=0x3
//       )

//       // -------- Weight packers --------
//       // 4 slots in 32b bus, each 8b wide; we use low bits per type.
//       def packW_fp4x4(w: Seq[Int]): BigInt =
//         (0 until 4).map(i => BigInt(w(i) & 0xF)  << (8*i)).reduce(_ | _)

//       def packW_fp6_e3m2_x4(w: Seq[Int]): BigInt =
//         (0 until 4).map(i => BigInt(w(i) & 0x3F) << (8*i)).reduce(_ | _)

//       def packW_fp6_e2m3_single(w: Int): BigInt = BigInt(w & 0x3F)

//       def packW_fp8_x4(w: Seq[Int]): BigInt =
//         (0 until 4).map(i => BigInt(w(i) & 0xFF) << (8*i)).reduce(_ | _)

//       def packW_fp8_e4m3_single(w: Int): BigInt = BigInt(w & 0xFF)

//       // -------- Common driver/printing --------
//       def driveAndPrintCase(
//         label: String,
//         a0_nib: Int, a1_nib: Int,
//         wType: Int, wAlt: Boolean, wPacked: BigInt, wDesc: String,
//         cRaw: BigInt, cDesc: String
//       ): Unit = {
//         val inA = packActsFp4(a0_nib, a1_nib) & mask(aW)

//         h.io.in_a_type.poke(0.U)        // fp4 activations
//         h.io.a_altfmt.poke(false.B)
//         h.io.in_activation.poke(inA.U(aW.W))
//         h.io.enable.poke(true.B)

//         h.io.in_w_type.poke(wType.U)
//         h.io.w_altfmt.poke(wAlt.B)
//         h.io.in_weights.poke(wPacked.U(wW.W))

//         h.io.c_raw.poke(cRaw.U(16.W))

//         val a0s = f"0x$a0_nib%X"; val a1s = f"0x$a1_nib%X"
//         println(s"\n=== $label ===")
//         println(s"in_activation (fp4x2) = ${binStr(inA, aW)}   a0=$a0s  a1=$a1s")
//         println(s"in_weights            = ${binStr(wPacked, wW)}   $wDesc")
//         println(s"c_raw (E8M7 16b)      = ${binStr(cRaw, 16)}   $cDesc")
//         println(s"(a_type=fp4, w_type=$wType, w_altfmt=${if (wAlt) 1 else 0})")

//         h.clock.step(1)

//         val recApplied = h.io.rec_c_applied.peek().litValue
//         val outBits    = h.io.out.peek().litValue
//         println(s"rec_c_applied(${h.io.rec_c_applied.getWidth}b recFN) = ${binStr(recApplied, h.io.rec_c_applied.getWidth)}")
//         println(s"out (raw E8M7 x4, ${outW}b)             = ${binStr(outBits, outW)}")
//         (0 until 4).foreach { i => showLane16(f"  lane[$i]", lane(outBits, i, laneW)) }
//       }

//       // -------- C values we'll reuse --------
//       val C_ZERO  = e8m7Raw(0, 0x00, 0x00) // +0.0
//       val C_ONE   = e8m7Raw(0, 0x7F, 0x00) // +1.0
//       val C_NEG1  = e8m7Raw(1, 0x7F, 0x00) // -1.0

//       // ===================================================================================
//       // CASE A: fp4 × fp4 (4 outputs). Two activation sets × two C values × different Ws.
//       // ===================================================================================
//       val wA_0 = Seq(0x2, 0x3, 0x4, 0x1) // simple ascending
//       val wA_1 = Seq(0x5, 0x4, 0x3, 0x1) // different mix

//       aSet.foreach { case (aname, a0, a1) =>
//         driveAndPrintCase(
//           label = s"Case A1 ($aname): fp4×fp4; C=0.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 0, wAlt = false, wPacked = packW_fp4x4(wA_0),
//           wDesc = f"fp4x4 w0..3=${wA_0.map(x => f"0x$x%X").mkString(" ")}",
//           cRaw = C_ZERO, cDesc = "C = +0.0"
//         )
//         driveAndPrintCase(
//           label = s"Case A2 ($aname): fp4×fp4; C=+1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 0, wAlt = false, wPacked = packW_fp4x4(wA_1),
//           wDesc = f"fp4x4 w0..3=${wA_1.map(x => f"0x$x%X").mkString(" ")}",
//           cRaw = C_ONE, cDesc = "C = +1.0"
//         )
//       }

//       // ===================================================================================
//       // CASE B: fp4 × fp6 (E2M3 single-weight). Two weights + two C values.
//       // (Here we set w_altfmt=true to indicate single-weight style.)
//       // ===================================================================================
//       val wB_single0 = enc(FP6_E2M3, 0, 2, 1) & 0x3F  // e=2,m=1
//       val wB_single1 = enc(FP6_E2M3, 0, 1, 3) & 0x3F  // e=1,m=3 (smaller)
//       aSet.foreach { case (aname, a0, a1) =>
//         driveAndPrintCase(
//           label = s"Case B1 ($aname): fp4×fp6(E2M3 single); C=0.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 1, wAlt = false, wPacked = packW_fp6_e2m3_single(wB_single0),
//           wDesc = f"fp6(E2M3) single w0=0x$wB_single0%02X",
//           cRaw = C_ZERO, cDesc = "C = +0.0"
//         )
//         driveAndPrintCase(
//           label = s"Case B2 ($aname): fp4×fp6(E2M3 single); C=+1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 1, wAlt = false, wPacked = packW_fp6_e2m3_single(wB_single1),
//           wDesc = f"fp6(E2M3) single w0=0x$wB_single1%02X",
//           cRaw = C_ONE, cDesc = "C = +1.0"
//         )
//       }

//       // ===================================================================================
//       // CASE C: fp4 × fp6 (E3M2 4-lane). One vector + two C values.
//       // (Here we set w_altfmt=false for E3M2 vector mode.)
//       // ===================================================================================
//       val wC_vec = Seq(
//         enc(FP6_E3M2, 0, 4, 1) & 0x3F, // unbiased +1
//         enc(FP6_E3M2, 0, 3, 2) & 0x3F, // unbiased  0
//         enc(FP6_E3M2, 0, 2, 0) & 0x3F, // unbiased -1
//         enc(FP6_E3M2, 0, 5, 3) & 0x3F  // unbiased +2
//       )
//       aSet.foreach { case (aname, a0, a1) =>
//         driveAndPrintCase(
//           label = s"Case C1 ($aname): fp4×fp6(E3M2 vec4); C=0.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 1, wAlt = true, wPacked = packW_fp6_e3m2_x4(wC_vec),
//           wDesc = f"fp6(E3M2) vec4 w=${wC_vec.map(x => f"0x$x%02X").mkString(" ")}",
//           cRaw = C_ZERO, cDesc = "C = +0.0"
//         )
//         driveAndPrintCase(
//           label = s"Case C2 ($aname): fp4×fp6(E3M2 vec4); C=+1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 1, wAlt = true, wPacked = packW_fp6_e3m2_x4(wC_vec.reverse),
//           wDesc = f"fp6(E3M2) vec4 w=${wC_vec.reverse.map(x => f"0x$x%02X").mkString(" ")}",
//           cRaw = C_ONE, cDesc = "C = +1.0"
//         )
//       }

//       // ===================================================================================
//       // CASE D: fp4 × fp8 (E5M2 4-lane).
//       // ===================================================================================
//       val wD_vec = Seq(
//         enc(FP8_E5M2, 0, 16, 0) & 0xFF, // +1
//         enc(FP8_E5M2, 0, 15, 1) & 0xFF, //  0
//         enc(FP8_E5M2, 0, 17, 2) & 0xFF, // +2
//         enc(FP8_E5M2, 0, 14, 3) & 0xFF  // -1
//       )
//       aSet.foreach { case (aname, a0, a1) =>
//         driveAndPrintCase(
//           label = s"Case D1 ($aname): fp4×fp8(E5M2 vec4); C=+1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 2, wAlt = true, wPacked = packW_fp8_x4(wD_vec),
//           wDesc = f"fp8(E5M2) vec4 w=${wD_vec.map(x => f"0x$x%02X").mkString(" ")}",
//           cRaw = C_ONE, cDesc = "C = +1.0"
//         )
//         driveAndPrintCase(
//           label = s"Case D2 ($aname): fp4×fp8(E5M2 vec4); C=-1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 2, wAlt = true, wPacked = packW_fp8_x4(wD_vec.reverse),
//           wDesc = f"fp8(E5M2) vec4 w=${wD_vec.reverse.map(x => f"0x$x%02X").mkString(" ")}",
//           cRaw = C_NEG1, cDesc = "C = -1.0"
//         )
//       }

//       // ===================================================================================
//       // CASE E: fp4 × fp8 (E4M3 single-weight).
//       // ===================================================================================
//       val wE_single0 = enc(FP8_E4M3, 0, 8, 1) & 0xFF
//       val wE_single1 = enc(FP8_E4M3, 0, 7, 3) & 0xFF
//       aSet.foreach { case (aname, a0, a1) =>
//         driveAndPrintCase(
//           label = s"Case E1 ($aname): fp4×fp8(E4M3 single); C=0.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 2, wAlt = false, wPacked = packW_fp8_e4m3_single(wE_single0),
//           wDesc = f"fp8(E4M3) single w0=0x$wE_single0%02X",
//           cRaw = C_ZERO, cDesc = "C = +0.0"
//         )
//         driveAndPrintCase(
//           label = s"Case E2 ($aname): fp4×fp8(E4M3 single); C=+1.0",
//           a0_nib = a0, a1_nib = a1,
//           wType = 2, wAlt = false, wPacked = packW_fp8_e4m3_single(wE_single1),
//           wDesc = f"fp8(E4M3) single w0=0x$wE_single1%02X",
//           cRaw = C_ONE, cDesc = "C = +1.0"
//         )
//       }

//       // Done
//       h.io.enable.poke(false.B)
//       h.clock.step(1)
//     }
//   }
// }
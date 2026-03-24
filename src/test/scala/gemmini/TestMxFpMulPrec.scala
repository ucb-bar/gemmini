package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random
import hardfloat._

import gemmini.MxFpMul

class MxFpMul_PrecisionProbe_Spec
  extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  behavior of "MxFpMul — precision probe: delta between full-precision expected and DUT output for FP4/FP6/FP8"

  it should "run a few cases per type combo and report expected vs actual deltas (no assertions)" in {

    val fpProductPrecision = (4, 4)
    val fpAccPrecision     = gemmini.MxFloat(8, 8, 4, true, false)

    test(new MxFpMulHarnessBf16Out_NewIO(lut = false, fpProductPrecision, fpAccPrecision))
      .withAnnotations(Seq(WriteVcdAnnotation)) { h =>

        // ---------- small-format helpers (positive-only) ----------
        case class MiniFmt(eBits: Int, mBits: Int, bias: Int) {
          val expMask  = (1 << eBits) - 1
          val mantMask = (1 << mBits) - 1
          def enc(e: Int, m: Int): Int = ((e & expMask) << mBits) | (m & mantMask)
        }
        val FP4_E2M1 = MiniFmt(2, 1, bias = 1)
        val FP6_E2M3 = MiniFmt(2, 3, bias = 1)
        val FP6_E3M2 = MiniFmt(3, 2, bias = 3)
        val FP8_E4M3 = MiniFmt(4, 3, bias = 7)
        val FP8_E5M2 = MiniFmt(5, 2, bias = 15)

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
          val rnd  = bits + (0x7FFF + lsb)
          (rnd >>> 16) & 0xFFFF
        }

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

        val rng = new Random(0xDEAD1234)
        def genSmall(fmt: MiniFmt): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) fmt.enc(0, 0)
          else if (r < 0.30f) fmt.enc(0, 1 + rng.nextInt(fmt.mantMask))
          else fmt.enc(1 + rng.nextInt((fmt.expMask - 1) max 1),
            rng.nextInt(fmt.mantMask + 1))
        }
        def genBF16(): Int = {
          val r = rng.nextFloat()
          if (r < 0.10f) 0x0000
          else if (r < 0.20f) (0x0001 + rng.nextInt(0x7F))
          else {
            val mag = math.pow(2.0, rng.nextInt(8) - 4).toFloat
            val base = rng.nextFloat() * mag
            floatToBf16Raw(base)
          }
        }

        val aW = h.io.in_activation.getWidth
        val wW = h.io.in_weights.getWidth
        val outLaneW = 16

        def pack2IntoHalves(raw0: Int, raw1: Int, elemBits: Int, totalW: Int): BigInt = {
          val half = totalW / 2
          require(totalW % 2 == 0, s"totalW=$totalW must be even")
          require(elemBits <= half, s"elemBits=$elemBits must fit in half=$half")
          val m = (1 << elemBits) - 1
          (BigInt(raw0 & m)) | (BigInt(raw1 & m) << half)
        }

        // ------------------------------------------------------------------
        // Pack activations: FP4/FP6 -> 2 elements; FP8 -> 1 element
        // ------------------------------------------------------------------
        def packActs(aType: Int, aAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
          val totalW = aW
          aType match {
            case 0 =>
              require(raws.length == 2)
              val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
              val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
              (packed, vals, f"fp4 acts: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

            case 1 =>
              require(raws.length == 2)
              val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
              val fmt    = if (aAlt) FP6_E3M2 else FP6_E2M3
              val vals   = raws.map(r => decodeSmall(fmt, r & 0x3F))
              val nm     = if (aAlt) "fp6 E3M2" else "fp6 E2M3"
              (packed, vals, s"$nm acts: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

            case 2 =>
              require(raws.length == 1)
              val a0     = raws.head & 0xFF
              val packed = BigInt(a0)
              val fmt    = if (aAlt) FP8_E5M2 else FP8_E4M3
              val vals   = Seq(decodeSmall(fmt, a0))
              val nm     = if (aAlt) "fp8 E5M2" else "fp8 E4M3"
              (packed, vals, f"$nm act: 0x$a0%02X -> ${vals.head}")
          }
        }

        def packWeis(wType: Int, wAlt: Boolean, raws: Seq[Int]): (BigInt, Seq[scala.Float], String) = {
          val totalW = wW
          wType match {
            case 0 =>
              require(raws.length == 2)
              val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 4, totalW)
              val vals   = raws.map(r => decodeSmall(FP4_E2M1, r & 0xF))
              (packed, vals, f"fp4 weis: ${raws.map(r => f"0x${r & 0xF}%X").mkString(", ")} -> ${vals.mkString(", ")}")

            case 1 =>
              require(raws.length == 2)
              val packed = pack2IntoHalves(raws(0), raws(1), elemBits = 6, totalW)
              val fmt    = if (wAlt) FP6_E3M2 else FP6_E2M3
              val vals   = raws.map(r => decodeSmall(fmt, r & 0x3F))
              val nm     = if (wAlt) "fp6 E3M2" else "fp6 E2M3"
              (packed, vals, s"$nm weis: ${raws.map(r => f"0x${r & 0x3F}%02X").mkString(", ")} -> ${vals.mkString(", ")}")

            case 2 =>
              require(raws.length == 1)
              val w0     = raws.head & 0xFF
              val packed = BigInt(w0)
              val fmt    = if (wAlt) FP8_E5M2 else FP8_E4M3
              val vals   = Seq(decodeSmall(fmt, w0))
              val nm     = if (wAlt) "fp8 E5M2" else "fp8 E4M3"
              (packed, vals, f"$nm wei: 0x$w0%02X -> ${vals.head}")
          }
        }

        // Expected lane mapping (full-precision reference)
        //  2×2: lane0=a0*w0+c, lane1=a0*w1+c, lane2=a1*w0+c, lane3=a1*w1+c
        //  1×1: lane0=a0*w0+c only
        def expectedFloatLane(aVals: Seq[scala.Float], wVals: Seq[scala.Float], c: scala.Float, lane: Int): Option[scala.Float] = {
          (aVals.length, wVals.length) match {
            case (2, 2) =>
              val pairs = Array((0, 0), (0, 1), (1, 0), (1, 1))
              val (ai, wi) = pairs(lane)
              Some(aVals(ai) * wVals(wi) + c)
            case (1, 1) =>
              if (lane == 0) Some(aVals(0) * wVals(0) + c) else None
            case _ => None
          }
        }

        def expectedBF16Lane(aVals: Seq[scala.Float], wVals: Seq[scala.Float], c: scala.Float, lane: Int): Option[Int] = {
          expectedFloatLane(aVals, wVals, c, lane).map(floatToBf16Raw)
        }

        // ------------------------------------------------------------------
        // pokeTypesAndMode — same as original test
        // ------------------------------------------------------------------
        def pokeTypesAndMode(aType: Int, aAlt: Boolean, wType: Int, wAlt: Boolean): Unit = {
          def expSigFromMxFormat(fmt: Int): (Int, Int) = fmt match {
            case 0 => (2, 2)
            case 1 => (3, 3)
            case 2 => (4, 4)
            case other => throw new IllegalArgumentException(s"bad mx_format=$other")
          }
          val (aExp, aSig) = expSigFromMxFormat(aType)
          val (wExp, wSig) = expSigFromMxFormat(wType)
          h.io.type_a.exp.poke(aExp.U)
          h.io.type_a.sig.poke(aSig.U)
          h.io.type_w.exp.poke(wExp.U)
          h.io.type_w.sig.poke(wSig.U)
        }

        // ------------------------------------------------------------------
        // ALL variants — FP4, FP6 (both alts), FP8 (both alts)
        // ------------------------------------------------------------------
        val aVariants = Seq(
          ("A: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
//          ("A: 2×fp6 (E2M3 alt0)", 1, false, () => Seq(genSmall(FP6_E2M3), genSmall(FP6_E2M3))),
          ("A: 2×fp6 (E3M2 alt1)", 1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2))),
          ("A: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3))),
//          ("A: 1×fp8 (E5M2 alt1)", 2, true,  () => Seq(genSmall(FP8_E5M2)))
        )
        val wVariants = Seq(
          ("W: 2×fp4",             0, false, () => Seq(genSmall(FP4_E2M1), genSmall(FP4_E2M1))),
//          ("W: 2×fp6 (E2M3 alt0)", 1, false, () => Seq(genSmall(FP6_E2M3), genSmall(FP6_E2M3))),
          ("W: 2×fp6 (E3M2 alt1)", 1, true,  () => Seq(genSmall(FP6_E3M2), genSmall(FP6_E3M2))),
          ("W: 1×fp8 (E4M3 alt0)", 2, false, () => Seq(genSmall(FP8_E4M3))),
//          ("W: 1×fp8 (E5M2 alt1)", 2, true,  () => Seq(genSmall(FP8_E5M2)))
        )

        val trialsPerCombo = 30
        h.io.enable.poke(true.B)

        val laneLabels2x2 = Array("a0×w0", "a0×w1", "a1×w0", "a1×w1")
        val laneLabels1x1 = Array("a0×w0", " n/a ", " n/a ", " n/a ")

        println("\n" + "=" * 90)
        println("  PRECISION PROBE: FP4 / FP6 / FP8 — all same-type combos")
        println("  Full-precision expected (BF16 RNE) vs DUT output — NO assertions")
        println("=" * 90)

        for ((aName, aType, aAlt, aGen) <- aVariants) {
          for ((wName, wType, wAlt, wGen) <- wVariants) {
            // Only run matching types (same constraint as original test)
            if (aType == wType) {
              println(s"\n${"=" * 90}")
              println(s"  Combo: $aName  vs  $wName")
              println(s"  (a_type=$aType alt=${if (aAlt) 1 else 0}; w_type=$wType alt=${if (wAlt) 1 else 0})")
              println(s"${"=" * 90}")

              for (t <- 0 until trialsPerCombo) {
                val aRaws = aGen()
                val (aPacked, aVals, aDesc) = packActs(aType, aAlt, aRaws)

                val wRaws = wGen()
                val (wPacked, wVals, wDesc) = packWeis(wType, wAlt, wRaws)

                val cRaw = genBF16()
                val cVal = bf16ToFloat(cRaw)

                val is2x2 = aVals.length == 2 && wVals.length == 2
                val labels = if (is2x2) laneLabels2x2 else laneLabels1x1
                val activeLanes = if (is2x2) 4 else 1

                // Drive DUT
                pokeTypesAndMode(aType, aAlt, wType, wAlt)
                h.io.in_activation.poke(aPacked.U(aW.W))
                h.io.in_weights.poke(wPacked.U(wW.W))
                h.io.c_raw.poke(cRaw.U(16.W))

                h.clock.step(2)

                // Read output
                val outBits = h.io.out_bf16.peek().litValue
                val got = Array.tabulate(4)(i => laneVal(outBits, i, outLaneW))

                // Print inputs
                println(f"\n  -- trial #$t%02d --")
                println(s"    $aDesc")
                println(s"    $wDesc")
                showBF16("    c_raw", cRaw)

                // Print delta table header
                println(f"    ${"Lane"}%-8s ${"Exp(float)"}%-14s ${"Exp(BF16)"}%-12s ${"Got(BF16)"}%-12s ${"Got(float)"}%-14s ${"Δ(float)"}%-14s ${"Δ(ULP)"}%-10s")
                println("    " + "-" * 84)

                for (i <- 0 until 4) {
                  val expFOpt    = expectedFloatLane(aVals, wVals, cVal, i)
                  val expBF16Opt = expectedBF16Lane(aVals, wVals, cVal, i)

                  (expFOpt, expBF16Opt) match {
                    case (Some(ef), Some(eb)) =>
                      val gotFloat   = bf16ToFloat(got(i))
                      val deltaFloat = gotFloat - ef
                      val deltaULP   = got(i) - eb
                      val ebHex      = f"0x${eb}%04X"
                      val gotHex     = f"0x${got(i)}%04X"
                      println(f"    ${labels(i)}%-8s ${ef}%-14g ${ebHex}%-12s ${gotHex}%-12s ${gotFloat}%-14g ${deltaFloat}%-+14g ${deltaULP}%-+10d")

                    case _ =>
                      val gotHex = f"0x${got(i)}%04X"
                      val gotF   = bf16ToFloat(got(i))
                      println(f"    ${labels(i)}%-8s ${"(n/a)"}%-14s ${""}%-12s ${gotHex}%-12s ${gotF}%-14g ${""}%-14s ${""}%-10s")
                  }
                }

                // Show recFN lanes for first trial of each combo
                if (t == 0) {
                  val recFnBits  = h.io.out_recfn.peek().litValue
                  val recFnLaneW = h.io.out_recfn.getWidth / 4
                  println(s"    [recFN lanes, ${recFnLaneW}b each]:")
                  (0 until activeLanes).foreach { i =>
                    val rv   = laneVal(recFnBits, i, recFnLaneW)
                    val sign = (rv >> (recFnLaneW - 1)) & 1
                    val exp  = (rv >> 7) & 0x1FF
                    val mant = rv & 0x7F
                    println(f"      lane[$i]: 0x${rv}%05X  s=$sign exp=0x$exp%03X(${exp}d) mant=0x$mant%02X")
                  }
                }
              }
            }
          }
        }

        h.io.enable.poke(false.B)
        h.clock.step(1)

        println("\n" + "=" * 90)
        println("  DONE — review Δ columns for precision loss at each type combo.")
        println("=" * 90 + "\n")
      }
  }
}
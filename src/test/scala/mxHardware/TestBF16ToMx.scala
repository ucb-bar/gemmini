package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class BF16ScaleRoundToTinyComprehensiveTest
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {

  // ----------------------------
  // Helpers: bit utilities
  // ----------------------------
  private def u8(x: Int): Int  = x & 0xFF
  private def u16(x: Int): Int = x & 0xFFFF

  private def signBit16(b: Int): Int = (b >>> 15) & 1
  private def exp16(b: Int): Int     = (b >>> 7) & 0xFF
  private def frac16(b: Int): Int    = b & 0x7F

  // BF16 -> Float32 exactly (BF16 is top 16 bits of FP32)
  private def bf16ToFloat(bf16: Int): scala.Float = {
    val bits32 = (u16(bf16) << 16)
    java.lang.Float.intBitsToFloat(bits32)
  }

  // ----------------------------
  // Decode your "tiny" packed formats to real numbers
  // ----------------------------

  // FP6: [5]=sign, [4:2]=exp3, [1:0]=frac2
  // bias = 3, exp=0 => subnorm/zero
  private def fp6ToDouble(fp6: Int): Double = {
    val b = fp6 & 0x3F
    val s = (b >>> 5) & 1
    val e = (b >>> 2) & 0x7
    val f = b & 0x3
    val sign = if (s == 1) -1.0 else 1.0
    val bias = 3

    if (e == 0) {
      if (f == 0) sign * 0.0
      else sign * (f.toDouble / 4.0) * math.pow(2.0, 1 - bias)
    } else {
      sign * (1.0 + f.toDouble / 4.0) * math.pow(2.0, e - bias)
    }
  }

  // FP8: [7]=sign, [6:3]=exp4, [2:0]=frac3
  // You interpret NaN as exp=15, frac=7 (0x7F with sign), and inf-ish as exp=15, frac=6 (0x7E with sign).
  // bias = 7 for normals; exp=0 => subnorm/zero
  private def fp8ToDouble(fp8: Int): Double = {
    val b = fp8 & 0xFF
    val s = (b >>> 7) & 1
    val e = (b >>> 3) & 0xF
    val f = b & 0x7
    val sign = if (s == 1) -1.0 else 1.0
    val bias = 7

    if (e == 0xF && f == 0x7) {
      Double.NaN
    } else if (e == 0) {
      if (f == 0) sign * 0.0
      else sign * (f.toDouble / 8.0) * math.pow(2.0, 1 - bias)
    } else {
      sign * (1.0 + f.toDouble / 8.0) * math.pow(2.0, e - bias)
    }
  }

  // FP4: [3]=sign, [2:1]=exp2, [0]=frac1
  // bias = 1, exp=0 => subnorm/zero
  private def fp4ToDouble(fp4: Int): Double = {
    val b = fp4 & 0xF
    val s = (b >>> 3) & 1
    val e = (b >>> 1) & 0x3
    val f = b & 0x1
    val sign = if (s == 1) -1.0 else 1.0
    val bias = 1

    if (e == 0) {
      if (f == 0) sign * 0.0
      else sign * (f.toDouble / 2.0) * math.pow(2.0, 1 - bias)
    } else {
      sign * (1.0 + f.toDouble / 2.0) * math.pow(2.0, e - bias)
    }
  }

  // ----------------------------
  // 1) Scale BF16 exponent exactly like RTL (your Scala model)
  // ----------------------------
  private def scaleBf16Exponent(bf16: Int, scale8: Int): Int = {
    val s     = signBit16(bf16)
    val e     = exp16(bf16)
    val frac  = frac16(bf16)

    val neg   = scale8 < 0
    val mag   = math.abs(scale8)

    val (newE, newFrac) =
      if (neg) {
        val ee = if (e < mag) 0 else (e - mag)
        (ee, frac)
      } else {
        val sum = e + mag
        if (sum >= 0xFF) (0xFF, 0x00) else (sum, frac)
      }

    u16((s << 15) | ((newE & 0xFF) << 7) | (newFrac & 0x7F))
  }

  // True “real” value the DUT is effectively quantizing (modulo NaN/Inf saturation behavior)
  private def trueScaledDouble(bf16: Int, scale8: Int): Double = {
    val scaled = scaleBf16Exponent(bf16, scale8)
    bf16ToFloat(scaled).toDouble
  }

  // ----------------------------
  // Enumerate representable outputs & choose acceptable codes
  // ----------------------------

  // Normalize “-0.0” and “+0.0” for comparisons
  private def isZeroD(x: Double): Boolean = x == 0.0

  private def nearestTwoCodes(
      target: Double,
      codes: IndexedSeq[(Int, Double)],
      ignoreZeroSign: Boolean
  ): Set[Int] = {

    if (ignoreZeroSign && target == 0.0) {
      val zeros = codes.collect { case (c, v) if v == 0.0 => c }.toSet
      if (zeros.nonEmpty) return zeros
    }

    val finite = codes.filter { case (_, v) =>
      !java.lang.Double.isNaN(v) && !java.lang.Double.isInfinite(v)
    }
    if (finite.isEmpty) return Set.empty

    // handle +/-Infinity (force saturate)
    if (java.lang.Double.isInfinite(target)) {
      val wantNeg = target < 0.0
      val maxAbs  = finite.map { case (_, v) => math.abs(v) }.max
      val sat = finite.collect {
        case (c, v) if math.abs(v) == maxAbs && ((v < 0.0) == wantNeg) => c
      }.toSet
      if (sat.nonEmpty) return sat
      return finite.collect { case (c, v) if math.abs(v) == maxAbs => c }.toSet
    }

    // saturation for huge finite targets (prevents tie bug)
    val maxAbs = finite.map { case (_, v) => math.abs(v) }.max
    if (java.lang.Double.isFinite(target) && math.abs(target) >= maxAbs && maxAbs != 0.0) {
      val wantNeg = target < 0.0
      val sat = finite.collect {
        case (c, v) if math.abs(v) == maxAbs && ((v < 0.0) == wantNeg) => c
      }.toSet
      if (sat.nonEmpty) return sat
      return finite.collect { case (c, v) if math.abs(v) == maxAbs => c }.toSet
    }

    // normal nearest-two
    val scored = finite.map { case (c, v) =>
      val vv = if (ignoreZeroSign && v == 0.0) 0.0 else v
      val tt = if (ignoreZeroSign && target == 0.0) 0.0 else target
      (c, math.abs(vv - tt))
    }.sortBy(_._2)

    if (scored.length == 1) Set(scored.head._1) else Set(scored(0)._1, scored(1)._1)
  }

  private def buildFp4Codes(): IndexedSeq[(Int, Double)] =
    (0 until 16).map(c => (c & 0xF, fp4ToDouble(c)))

  private def buildFp6Codes(): IndexedSeq[(Int, Double)] =
    (0 until 64).map(c => (c & 0x3F, fp6ToDouble(c)))

  private def buildFp8Codes(): IndexedSeq[(Int, Double)] =
    (0 until 256).map(c => (c & 0xFF, fp8ToDouble(c)))

  // ----------------------------
  // Vectors
  // ----------------------------
  private def directedBf16: Seq[Int] = Seq(
    0x0000, 0x8000,
    0x0001, 0x8001,
    0x007F, 0x807F,
    0x0080, 0x8080,
    0x3F80, 0xBF80,
    0x4000, 0xC000,
    0x7F7F, 0xFF7F,
    0x7F80, 0xFF80, // +/-Inf
    0x7FC1, 0xFFC1  // NaNs
  ).map(u16)

  private def floatToBf16BitsTrunc(f: scala.Float): Int = {
    val bits32 = java.lang.Float.floatToRawIntBits(f)
    u16(bits32 >>> 16)
  }

  private val baselineBf16InRange: Seq[Int] = Seq(
    0.0f, -0.0f,
    0.25f, -0.25f,
    0.5f, -0.5f,
    1.0f, -1.0f,
    1.5f, -1.5f,
    2.0f, -2.0f,
    3.0f, -3.0f,
    4.75f, -4.75f,
    6.5f, -6.5f,
    11.27150f, -11.27150f,
    7.0f, -7.0f,
    15.0f, -15.0f,
    31.0f, -31.0f,
    63.0f, -63.0f,
    64.0f, -64.0f,
    0.0625f, -0.0625f,
    0.125f, -0.125f,
    0.375f, -0.375f,
    1.125f, -1.125f,
    2.5f, -2.5f,
  ).map(floatToBf16BitsTrunc)

  private val scalesToTry: Seq[Int] =
    Seq(-32, -16, -8, -4, -2, -1, 0, 1, 2, 4, 8, 16, 32)

  private def randomBf16(rng: Random): Int = u16(rng.nextInt(1 << 16))

  // ----------------------------
  // Core runner (1-cycle latency)
  // ----------------------------
  private def runSelfCheckWithPrints(
      lanes: Int,
      pokesPerScale: Int,
      verbose: Boolean,
      // dut hooks
      dutPoke: (Int, Int) => Unit,
      pokeScale: Int => Unit,
      step: () => Unit,
      peekOut: Int => Int,
      // decode printing
      outToDouble: Int => Double,
      // for “between two representations” acceptance
      allCodes: IndexedSeq[(Int, Double)],
      // knobs you added (keep these!)
      isFp6: Boolean = false,
      ignoreZeroSign: Boolean = false,
      nanOkEitherSignMax: Boolean = false,
      maxMagMask: Int = 0,
      maxMagValue: Int = 0
  ): Unit = {
    val rng = new Random(0xC0FFEE)

    for (scale <- scalesToTry) {
      pokeScale(scale)

      // prime pipeline
      var prevInputs   = Array.fill(lanes)(0)

      for (_ <- 0 until 2) {
        for (i <- 0 until lanes) dutPoke(i, 0)
        step()
      }

      val vecs: Seq[Int] = directedBf16 ++ baselineBf16InRange ++ Seq.fill(pokesPerScale)(randomBf16(rng))

      var idx = 0
      while (idx < vecs.length) {
        val mismatches = scala.collection.mutable.ArrayBuffer[String]()

        // check outputs from previous cycle
        for (i <- 0 until lanes) {
          val got    = peekOut(i)
          val inBf16 = prevInputs(i)
          val inF    = bf16ToFloat(inBf16)
          val outD   = outToDouble(got)

          if (verbose) {
            val t = trueScaledDouble(inBf16, scale)
            println(f"[scale=$scale%4d] lane=$i%2d  in_bf16=0x$inBf16%04x  in=${inF}%-14s  out_bits=0x$got%02x  out=${outD}%-14s  true=${t}%-14s")
          }

          val isNaNIn = java.lang.Float.isNaN(inF)
          val isInfIn = java.lang.Float.isInfinite(inF)

          // Normalize zeros if desired (+0/-0 equivalence)
          val gotN =
            if (ignoreZeroSign && ((got & maxMagMask) == 0)) (got & maxMagMask)
            else got

          if (isNaNIn && nanOkEitherSignMax) {
            // For NaN input: accept either +max or -max, but enforce saturation to max magnitude.
            if ((gotN & maxMagMask) != maxMagValue) {
              mismatches += f"NaN->notMax @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x out_bits=0x$got%02x out=${outD}"
            }
          } else if (isInfIn) {
            // You said RTL saturates infs too; keep sign strict by default, but allow max magnitude check.
            if ((gotN & maxMagMask) != maxMagValue) {
              mismatches += f"Inf->notMax @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x out_bits=0x$got%02x out=${outD}"
            }
          } else {
            // Normal numbers: accept either of the two closest representable outputs
            val t = trueScaledDouble(inBf16, scale)

            // fp6 special-case: your fp6 decoder has no NaN, so ignore NaN comparisons
            val checkThis = (!isFp6 || !java.lang.Float.isNaN(inF))

            if (checkThis) {
              val allowed = nearestTwoCodes(t, allCodes, ignoreZeroSign)

              val gotCmp = got & (allCodes.head._1 match {
                case _ if allCodes.length == 16  => 0xF
                case _ if allCodes.length == 64  => 0x3F
                case _ if allCodes.length == 256 => 0xFF
                case _                           => 0xFF
              })

              if (!allowed.contains(gotCmp)) {
                mismatches += f"mismatch @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x in=${inF} true=${t} got_bits=0x$got%02x got=${outD} allowed=${allowed.toSeq.sorted.map(x => f"0x$x%02x").mkString("{",", ","}")}"
              }
            }
          }
          step()
        }

        if (mismatches.nonEmpty) fail(mismatches.mkString("\n"))

        // poke next cycle
        val nextInputs = Array.fill(lanes)(0)
        for (i <- 0 until lanes) {
          val bf16 = if (idx + i < vecs.length) vecs(idx + i) else 0
          dutPoke(i, bf16)
          nextInputs(i) = bf16
        }
        prevInputs = nextInputs

        step()
        idx += lanes
      }

      // final drain cycle check
      val mismatches = scala.collection.mutable.ArrayBuffer[String]()
      for (i <- 0 until lanes) {
        val got    = peekOut(i)
        val inBf16 = prevInputs(i)
        val inF    = bf16ToFloat(inBf16)
        val outD   = outToDouble(got)

        if (verbose) {
          val t = trueScaledDouble(inBf16, scale)
          println(f"[scale=$scale%4d] lane=$i%2d  in_bf16=0x$inBf16%04x  in=${inF}%-14s  out_bits=0x$got%02x  out=${outD}%-14s  true=${t}%-14s")
        }

        val isNaNIn = java.lang.Float.isNaN(inF)
        val isInfIn = java.lang.Float.isInfinite(inF)

        val gotN =
          if (ignoreZeroSign && ((got & maxMagMask) == 0)) (got & maxMagMask)
          else got

        if (isNaNIn && nanOkEitherSignMax) {
          if ((gotN & maxMagMask) != maxMagValue) {
            mismatches += f"NaN->notMax @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x out_bits=0x$got%02x out=${outD}"
          }
        } else if (isInfIn) {
          if ((gotN & maxMagMask) != maxMagValue) {
            mismatches += f"Inf->notMax @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x out_bits=0x$got%02x out=${outD}"
          }
        } else {
          val t = trueScaledDouble(inBf16, scale)
          val checkThis = (!isFp6 || !java.lang.Float.isNaN(inF))
          if (checkThis) {
            val allowed = nearestTwoCodes(t, allCodes, ignoreZeroSign)

            val gotCmp = got & (allCodes.length match {
              case 16  => 0xF
              case 64  => 0x3F
              case 256 => 0xFF
              case _   => 0xFF
            })

            if (!allowed.contains(gotCmp)) {
              mismatches += f"mismatch @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x in=${inF} true=${t} got_bits=0x$got%02x got=${outD} allowed=${allowed.toSeq.sorted.map(x => f"0x$x%02x").mkString("{",", ","}")}"
            }
          }
        }

        // step()
      }

      if (mismatches.nonEmpty) fail(mismatches.mkString("\n"))
    }
  }

  // ----------------------------
  // Tests
  // ----------------------------

  behavior of "BF16ScaleRoundToFP4"

  it should "self-check FP4 and print decimal BF16 + decimal FP4 output for every vector" in {
    test(new BF16ScaleRoundToFP4(outputnumLanes = 8)) { dut =>
      val verbose = true

      def pokeLane(i: Int, bf16: Int): Unit =
        dut.io.in_bf16(i).poke((bf16 & 0xFFFF).U)

      def pokeScale(scale: Int): Unit =
        dut.io.scale_e8m0.poke(scale.S(8.W).asUInt)

      def step(): Unit = dut.clock.step()

      def peekOutFp4(i: Int): Int =
        (dut.io.out_fp6(i).peek().litValue.toInt) & 0xF

      runSelfCheckWithPrints(
        lanes = 8,
        pokesPerScale = 20,
        verbose = verbose,
        dutPoke = pokeLane,
        pokeScale = pokeScale,
        step = step,
        peekOut = peekOutFp4,
        outToDouble = fp4ToDouble,
        allCodes = buildFp4Codes(),
        ignoreZeroSign = true,
        nanOkEitherSignMax = true,
        maxMagMask = 0x7,     // ignore sign bit, check magnitude bits only
        maxMagValue = 0x7     // fp4 max magnitude is ???_111
      )
    }
  }

  behavior of "BF16ScaleRoundToFP6"

  it should "self-check FP6 and print decimal BF16 + decimal FP6 output for every vector" in {
    test(new BF16ScaleRoundToFP6(outputnumLanes = 8)) { dut =>
      val verbose = true

      def pokeLane(i: Int, bf16: Int): Unit =
        dut.io.in_bf16(i).poke((bf16 & 0xFFFF).U)

      def pokeScale(scale: Int): Unit =
        dut.io.scale_e8m0.poke(scale.S(8.W).asUInt)

      def step(): Unit = dut.clock.step()

      def peekOutFp6(i: Int): Int =
        (dut.io.out_fp6(i).peek().litValue.toInt) & 0x3F

      runSelfCheckWithPrints(
        lanes = 8,
        pokesPerScale = 20,
        verbose = verbose,
        dutPoke = pokeLane,
        pokeScale = pokeScale,
        step = step,
        peekOut = peekOutFp6,
        outToDouble = fp6ToDouble,
        allCodes = buildFp6Codes(),
        isFp6 = true,              // keep your old “fp6 has no NaN” behavior
        ignoreZeroSign = true,
        nanOkEitherSignMax = true, // if NaNs reach fp6 path, still accept max saturation
        maxMagMask = 0x1F,         // fp6 magnitude bits (low 5)
        maxMagValue = 0x1F         // fp6 max magnitude pattern
      )
    }
  }

  behavior of "BF16ScaleRoundToFP8"

  it should "self-check FP8 and print decimal BF16 + decimal FP8 output for every vector" in {
    test(new BF16ScaleRoundToFP8(outputnumLanes = 8)) { dut =>
      val verbose = true

      def pokeLane(i: Int, bf16: Int): Unit =
        dut.io.in_bf16(i).poke((bf16 & 0xFFFF).U)

      def pokeScale(scale: Int): Unit =
        dut.io.scale_e8m0.poke(scale.S(8.W).asUInt)

      def step(): Unit = dut.clock.step()

      def peekOutFp8(i: Int): Int =
        (dut.io.out_fp6(i).peek().litValue.toInt) & 0xFF

      runSelfCheckWithPrints(
        lanes = 8,
        pokesPerScale = 20,
        verbose = verbose,
        dutPoke = pokeLane,
        pokeScale = pokeScale,
        step = step,
        peekOut = peekOutFp8,
        outToDouble = fp8ToDouble,
        allCodes = buildFp8Codes(),
        ignoreZeroSign = true,
        nanOkEitherSignMax = true,
        maxMagMask = 0x7F,     // ignore sign, check magnitude
        maxMagValue = 0x7E     // your fp8 “max magnitude” is typically 0x7E (Inf-ish) in your packer
      )
    }
  }
}
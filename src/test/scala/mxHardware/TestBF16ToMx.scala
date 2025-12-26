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
  // We interpret this as an IEEE-like mini-float with:
  //   bias = 3
  //   exp=0 => subnorm/zero (no implicit 1)
  // No Inf/NaN encoding in your fp6 packer.
  private def fp6ToDouble(fp6: Int): Double = {
    val b = fp6 & 0x3F
    val s = (b >>> 5) & 1
    val e = (b >>> 2) & 0x7
    val f = b & 0x3
    val sign = if (s == 1) -1.0 else 1.0
    val bias = 3

    if (e == 0) {
      // subnormal/zero: value = (f / 2^2) * 2^(1-bias)
      if (f == 0) sign * 0.0
      else sign * (f.toDouble / 4.0) * math.pow(2.0, 1 - bias)
    } else {
      // normal: (1 + f/4) * 2^(e-bias)
      sign * (1.0 + f.toDouble / 4.0) * math.pow(2.0, e - bias)
    }
  }

  // FP8: [7]=sign, [6:3]=exp4, [2:0]=frac3
  // Your packer uses:
  //   NaN  = sign ## 0x7F  (exp=15, frac=7)
  //   Inf  = sign ## 0x7E  (exp=15, frac=6)
  // We interpret normals as bias=7 (since expAdj is 0..15).
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
      // subnormal/zero: (f/2^3) * 2^(1-bias)
      if (f == 0) sign * 0.0
      else sign * (f.toDouble / 8.0) * math.pow(2.0, 1 - bias)
    } else {
      // normal: (1 + f/8) * 2^(e-bias)
      sign * (1.0 + f.toDouble / 8.0) * math.pow(2.0, e - bias)
    }
  }

  // ----------------------------
  // 1) Scale BF16 exponent exactly like RTL
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

  // ----------------------------
  // 2) Quantize Float -> IEEE-like E4M3 / E5M3 (RNE)
  // ----------------------------
  private def roundTiesToEven(x: Double): Long = {
    val floor = math.floor(x)
    val frac  = x - floor
    if (frac > 0.5) (floor + 1.0).toLong
    else if (frac < 0.5) floor.toLong
    else {
      val f = floor.toLong
      if ((f & 1L) == 0L) f else f + 1L
    }
  }

  private def floatToE4M3IEEE(f: scala.Float): Int = {
    val sign = if (java.lang.Float.floatToRawIntBits(f) < 0) 1 else 0
    if (java.lang.Float.isNaN(f)) return (sign << 7) | (0xF << 3) | 0x7
    if (java.lang.Float.isInfinite(f)) return (sign << 7) | (0xF << 3) | 0x0
    if (f == 0.0f) return (sign << 7)

    val a = math.abs(f.toDouble)
    val bias = 7
    val fracBits = 3
    val maxExpField = 0xF

    val e2   = math.floor(math.log(a) / math.log(2.0)).toInt
    val mant = a / math.pow(2.0, e2) // [1,2)
    var expField = e2 + bias

    if (expField >= maxExpField) return (sign << 7) | (0xF << 3)

    if (expField <= 0) {
      val scaled = a / math.pow(2.0, 1 - bias)
      val fracExact = scaled * (1 << fracBits)
      var fracField = roundTiesToEven(fracExact).toInt
      if (fracField <= 0) return (sign << 7)
      if (fracField >= (1 << fracBits)) return (sign << 7) | (1 << 3)
      return (sign << 7) | (0 << 3) | (fracField & 0x7)
    }

    val fracExact = (mant - 1.0) * (1 << fracBits)
    var fracField = roundTiesToEven(fracExact).toInt
    if (fracField == (1 << fracBits)) {
      fracField = 0
      expField += 1
      if (expField >= maxExpField) return (sign << 7) | (0xF << 3)
    }
    (sign << 7) | ((expField & 0xF) << 3) | (fracField & 0x7)
  }

  private def floatToE5M3IEEE(f: scala.Float): Int = {
    val sign = if (java.lang.Float.floatToRawIntBits(f) < 0) 1 else 0
    if (java.lang.Float.isNaN(f)) return (sign << 8) | (0x1F << 3) | 0x7
    if (java.lang.Float.isInfinite(f)) return (sign << 8) | (0x1F << 3) | 0x0
    if (f == 0.0f) return (sign << 8)

    val a = math.abs(f.toDouble)
    val bias = 15
    val fracBits = 3
    val maxExpField = 0x1F

    val e2   = math.floor(math.log(a) / math.log(2.0)).toInt
    val mant = a / math.pow(2.0, e2)
    var expField = e2 + bias

    if (expField >= maxExpField) return (sign << 8) | (0x1F << 3)

    if (expField <= 0) {
      val scaled = a / math.pow(2.0, 1 - bias)
      val fracExact = scaled * (1 << fracBits)
      var fracField = roundTiesToEven(fracExact).toInt
      if (fracField <= 0) return (sign << 8)
      if (fracField >= (1 << fracBits)) return (sign << 8) | (1 << 3)
      return (sign << 8) | (0 << 3) | (fracField & 0x7)
    }

    val fracExact = (mant - 1.0) * (1 << fracBits)
    var fracField = roundTiesToEven(fracExact).toInt
    if (fracField == (1 << fracBits)) {
      fracField = 0
      expField += 1
      if (expField >= maxExpField) return (sign << 8) | (0x1F << 3)
    }
    (sign << 8) | ((expField & 0x1F) << 3) | (fracField & 0x7)
  }

  // ----------------------------
  // 3) Mirror your packers (Scala golden)
  // ----------------------------
  private def packE4M3ToFp6(e4m3: Int): Int = {
    val in = u8(e4m3)
    val sign = (in >>> 7) & 1
    val exp  = (in >>> 3) & 0xF
    val sig  = in & 0x7

    val biasDiff = 4
    val fullSig = (1 << 3) | sig
    val underflow = exp <= biasDiff
    val adjExp = if (underflow) 0 else (exp - biasDiff)
    val overflow = ((adjExp >>> 3) & 1) == 1

    val isZeroInput = (exp == 0) && (sig == 0)
    val useSubnormal = underflow && !isZeroInput

    val shift = (biasDiff - exp + 1) & 0x3
    val shifted = (fullSig >>> shift) & 0xF
    val subSig2 = (shifted >>> 1) & 0x3

    val normSig2 = (sig >>> 1) & 0x3
    val normExp3 = adjExp & 0x7

    val outExp3 = if (useSubnormal) 0 else normExp3
    val outSig2 = if (useSubnormal) subSig2 else normSig2

    if (overflow) ((sign & 1) << 5) | 0x1F else ((sign & 1) << 5) | ((outExp3 & 7) << 2) | (outSig2 & 3)
  }

  private def packE5M3ToFp8(e5m3: Int): Int = {
    val in = e5m3 & 0x1FF
    val sign = (in >>> 8) & 1
    val exp  = (in >>> 3) & 0x1F
    val sig  = in & 0x7

    val isNaN = (exp == 0x1F) && (sig != 0)
    val isInf = (exp == 0x1F) && (sig == 0)

    val biasDiff = 8
    val raw = exp - biasDiff
    val expAdj =
      if (raw >= 15) 15
      else if (exp <= biasDiff) 0
      else raw

    if (isNaN) ((sign & 1) << 7) | 0x7F
    else if (isInf) ((sign & 1) << 7) | 0x7E
    else ((sign & 1) << 7) | ((expAdj & 0xF) << 3) | (sig & 0x7)
  }

  private def goldenFp6(bf16: Int, scale8: Int): Int = {
    val scaled = scaleBf16Exponent(bf16, scale8)
    val f = bf16ToFloat(scaled)
    val e4m3 = floatToE4M3IEEE(f)
    packE4M3ToFp6(e4m3) & 0x3F
  }

  private def goldenFp8(bf16: Int, scale8: Int): Int = {
    val scaled = scaleBf16Exponent(bf16, scale8)
    val f = bf16ToFloat(scaled)
    val e5m3 = floatToE5M3IEEE(f)
    packE5M3ToFp8(e5m3) & 0xFF
  }

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
    u16(bits32 >>> 16) // trunc to BF16 (matches your bf16ToFloat inverse)
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
  64.0f, -64.0f
).map(f => floatToBf16BitsTrunc(f))

  private val scalesToTry: Seq[Int] = Seq(-32, -16, -8, -4, -2, -1, 0, 1, 2, 4, 8, 16, 32)

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
      // gold + decode printing
      golden: (Int, Int) => Int,
      outToDouble: Int => Double, 
      isFp6: Boolean = false
  ): Unit = {
    val rng = new Random(0xC0FFEE)

    for (scale <- scalesToTry) {
      pokeScale(scale)

      // prime pipeline
      var prevInputs = Array.fill(lanes)(0)
      var prevExpected = Array.fill(lanes)(0)

      for (_ <- 0 until 2) {
        for (i <- 0 until lanes) dutPoke(i, 0)
        step()
      }

      val vecs: Seq[Int] = directedBf16 ++ baselineBf16InRange ++ Seq.fill(pokesPerScale)(randomBf16(rng))

      var idx = 0
      while (idx < vecs.length) {
        // check/print previous cycle outputs
        val mismatches = scala.collection.mutable.ArrayBuffer[String]()

        for (i <- 0 until lanes) {
          val got = peekOut(i)
          val exp = prevExpected(i)
          val inBf16 = prevInputs(i)
          val inF    = bf16ToFloat(inBf16)
          val outD   = outToDouble(got)

          if (verbose) {
            println(f"[scale=$scale%4d] lane=$i%2d  in_bf16=0x$inBf16%04x  in=${inF}%-14s  out_bits=0x$got%02x  out=${outD}%-14s  exp_bits=0x$exp%02x")
          }

          val isFP8_NaN = !isFp6 && java.lang.Float.isNaN(inF)
          val checkThis = (!isFp6 || !java.lang.Float.isNaN(inF))
          if (checkThis && got != exp) {
            if (!isFP8_NaN || ((got & 0x7F) != 0x7F)) {
              mismatches += f"mismatch @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x in=${inF} out_bits=0x$got%02x out=${outD} exp_bits=0x$exp%02x"
            }
          }
          // step()
        }

        if (mismatches.nonEmpty) {
          fail(mismatches.mkString("\n"))
        }

        // poke next cycle
        val nextInputs = Array.fill(lanes)(0)
        val nextExpected = Array.fill(lanes)(0)
        for (i <- 0 until lanes) {
          val bf16 = if (idx + i < vecs.length) vecs(idx + i) else 0
          dutPoke(i, bf16)
          nextInputs(i) = bf16
          nextExpected(i) = golden(bf16, scale)
        }
        prevInputs = nextInputs
        prevExpected = nextExpected

        step()
        idx += lanes
      }

      val mismatches = scala.collection.mutable.ArrayBuffer[String]()

      for (i <- 0 until lanes) {
        val got = peekOut(i)
        val exp = prevExpected(i)
        val inBf16 = prevInputs(i)
        val inF    = bf16ToFloat(inBf16)
        val outD   = outToDouble(got)

        if (verbose) {
          println(f"[scale=$scale%4d] lane=$i%2d  in_bf16=0x$inBf16%04x  in=${inF}%-14s  out_bits=0x$got%02x  out=${outD}%-14s  exp_bits=0x$exp%02x")
        }

        val isFP8_NaN = !isFp6 && java.lang.Float.isNaN(inF)
        val checkThis = (!isFp6 || !java.lang.Float.isNaN(inF))
        if (checkThis && got != exp) {
          if (!isFP8_NaN || ((got & 0x7F) != 0x7F)) {
            mismatches += f"mismatch @ scale=$scale lane=$i: in_bf16=0x$inBf16%04x in=${inF} out_bits=0x$got%02x out=${outD} exp_bits=0x$exp%02x"
          }
        }
      }

      if (mismatches.nonEmpty) {
        fail(mismatches.mkString("\n"))
      }
    }
  }

  // ----------------------------
  // Tests
  // ----------------------------

  behavior of "BF16ScaleRoundToFP6"

  it should "self-check FP6 and print decimal BF16 + decimal FP6 output for every vector" in {
    test(new BF16ScaleRoundToFP6(outputnumLanes = 8)) { dut =>
      val verbose = true   // <- flip to false when you crank pokesPerScale

      def pokeLane(i: Int, bf16: Int): Unit =
        dut.io.in_bf16(i).poke((bf16 & 0xFFFF).U)

      def pokeScale(scale: Int): Unit =
        dut.io.scale_e8m0.poke(scale.S(8.W).asUInt)

      def step(): Unit = dut.clock.step()

      def peekOutFp6(i: Int): Int =
        (dut.io.out_fp6(i).peek().litValue.toInt) & 0x3F // fp6 in low 6 bits

      runSelfCheckWithPrints(
        lanes = 8,
        pokesPerScale = 20,   // increase if desired; printing will explode
        verbose = verbose,
        dutPoke = pokeLane,
        pokeScale = pokeScale,
        step = step,
        peekOut = peekOutFp6,
        golden = goldenFp6,
        outToDouble = fp6ToDouble, 
        isFp6 = true
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
        (dut.io.out_fp6(i).peek().litValue.toInt) & 0xFF // fp8 uses full 8 bits

      runSelfCheckWithPrints(
        lanes = 8,
        pokesPerScale = 20,
        verbose = verbose,
        dutPoke = pokeLane,
        pokeScale = pokeScale,
        step = step,
        peekOut = peekOutFp8,
        golden = goldenFp8,
        outToDouble = fp8ToDouble
      )
    }
  }
}
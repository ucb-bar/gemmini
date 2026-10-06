package gemmini

import chisel3._
import chiseltest._
import chiseltest.simulator.VcsBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

// Directed + exhaustive simulation of FP8NearestFinder (the core of FP8 LUT projection).
// A Scala golden model decodes each FP8 byte to its real value and finds the nearest
// LUT entry; the hardware must return the same 4-bit index for every FP8 input.
class TestFP8NearestFinder extends AnyFlatSpec with ChiselScalatestTester {

  // ---- Scala golden decoders (OCP FP8) ----
  def decodeE4M3(b: Int): Double = {
    val sign = (b >> 7) & 1
    val exp  = (b >> 3) & 0xF
    val mant = b & 0x7
    val mag =
      if (exp == 0 && mant == 0) 0.0
      else if (exp == 0) (mant.toDouble / 8.0) * math.pow(2, -6)      // subnormal (2^(1-7))
      else (1.0 + mant.toDouble / 8.0) * math.pow(2, exp - 7)         // normal
    if (sign == 1) -mag else mag
  }
  def isNaNE4M3(b: Int): Boolean = (((b >> 3) & 0xF) == 0xF) && ((b & 0x7) == 0x7)

  def decodeE5M2(b: Int): Double = {
    val sign = (b >> 7) & 1
    val exp  = (b >> 2) & 0x1F
    val mant = b & 0x3
    val mag =
      if (exp == 0 && mant == 0) 0.0
      else if (exp == 0) (mant.toDouble / 4.0) * math.pow(2, -14)     // subnormal (2^(1-15))
      else (1.0 + mant.toDouble / 4.0) * math.pow(2, exp - 15)        // normal
    if (sign == 1) -mag else mag
  }
  def isSpecialE5M2(b: Int): Boolean = (((b >> 2) & 0x1F) == 0x1F)    // inf / NaN

  // Golden nearest: strict "<" keeps the earliest index on ties, matching the
  // hardware reduce (Mux(d1 <= d2, i1, i2) keeps the earlier candidate).
  def goldenNearest(inByte: Int, lut: Seq[Int], decode: Int => Double): Int = {
    val inV = decode(inByte)
    var bestIdx = 0
    var bestDist = Double.MaxValue
    for (i <- 0 until 16) {
      val d = math.abs(inV - decode(lut(i)))
      if (d < bestDist) { bestDist = d; bestIdx = i }
    }
    bestIdx
  }

  def runExhaustive(altfmt: Boolean, lut: Seq[Int], decode: Int => Double, skip: Int => Boolean): Unit = {
    require(lut.length == 16)
    test(new FP8NearestFinderWrapper(altfmt)).withAnnotations(Seq(VcsBackendAnnotation)) { dut =>
      for (i <- 0 until 16) dut.io.in_lut(i).poke(lut(i).U)
      var checked = 0
      for (b <- 0 until 256 if !skip(b)) {
        dut.io.in.poke(b.U)
        dut.clock.step(1)
        val hw = dut.io.nearestIdx.peek().litValue.toInt
        val gold = goldenNearest(b, lut, decode)
        assert(hw == gold,
          f"in=0x$b%02x (${decode(b)}): hw idx=$hw, golden idx=$gold " +
          f"[lut=${lut.map(l => f"0x$l%02x").mkString(",")}]")
        checked += 1
      }
      println(f"  ✓ ${if (altfmt) "E5M2" else "E4M3"}: $checked inputs matched golden")
    }
  }

  behavior of "FP8NearestFinder"

  it should "match golden nearest-index for E4M3 across all FP8 inputs" in {
    // A representative LUT of assorted E4M3 byte patterns (no NaN entries).
    val lutA = Seq(0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38,
                   0x80, 0x88, 0x90, 0x98, 0xA0, 0xA8, 0xB0, 0xB8)
    runExhaustive(altfmt = false, lutA, decodeE4M3, isNaNE4M3)

    // A second LUT skewed toward larger magnitudes.
    val lutB = Seq(0x00, 0x38, 0x40, 0x48, 0x50, 0x58, 0x60, 0x68,
                   0x70, 0x78, 0x01, 0x02, 0x04, 0x06, 0x0C, 0x14)
    runExhaustive(altfmt = false, lutB, decodeE4M3, isNaNE4M3)
  }

  it should "match golden nearest-index for E5M2 across all FP8 inputs" in {
    val lutA = Seq(0x00, 0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C, 0x34,
                   0x80, 0x84, 0x8C, 0x94, 0x9C, 0xA4, 0xAC, 0xB4)
    runExhaustive(altfmt = true, lutA, decodeE5M2, isSpecialE5M2)
  }
}

// Wrapper so ChiselTest has a clocked Module around the combinational RawModule.
class FP8NearestFinderWrapper(altfmt: Boolean) extends Module {
  val io = IO(new Bundle {
    val in         = Input(UInt(8.W))
    val in_lut     = Input(Vec(16, UInt(8.W)))
    val nearestIdx = Output(UInt(4.W))
  })
  val f = Module(new FP8NearestFinder(altfmt))
  f.io.in     := io.in
  f.io.in_lut := io.in_lut
  io.nearestIdx := f.io.nearestIdx
}

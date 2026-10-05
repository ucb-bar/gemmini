package gemmini

import chisel3._
import chisel3.util._

// Reference (pre-optimization) brute-force nearest finders: fixed-point |x - lut(i)| and a 16-way min-reduce per
// lane. Kept only for the equivalence tests of NearestFinder.scala; not instantiated by the design.

class RawFP6E3M2 extends Bundle {
  val sign = Bool()
  val isZero = Bool()
  val isSubnormal = Bool()
  val sExp = SInt(4.W)      // signed exponent, range [-2, 4]
  val sig = UInt(3.W)       // significand: 1.MM or 0.MM
}

object rawFP6E3M2FromBits {
  def apply(in: UInt): RawFP6E3M2 = {
    val sign = in(5)
    val exp  = in(4, 2)
    val mant = in(1, 0)

    val isZero = (exp === 0.U) && (mant === 0.U)
    val isSubnormal = (exp === 0.U) && (mant =/= 0.U)
    val sExp = Mux(exp === 0.U, (-2).S(4.W), (exp.zext - 3.S)(3, 0).asSInt)
    val implicitBit = Mux(exp === 0.U, 0.U(1.W), 1.U(1.W))
    val sig = Cat(implicitBit, mant)

    val raw = Wire(new RawFP6E3M2)
    raw.sign := sign
    raw.isZero := isZero
    raw.isSubnormal := isSubnormal
    raw.sExp := sExp
    raw.sig := sig
    raw
  }
}

class FP6E3M2NearestFinderRef extends RawModule {
  val io = IO(new Bundle {
    val in_fp6 = Input(UInt(6.W))
    val in_lut = Input(Vec(16, UInt(6.W)))
    val nearestIdx = Output(UInt(4.W))
  })

  def fp6ToFixedPoint(in: UInt): SInt = {
    val raw = rawFP6E3M2FromBits(in)
    val shiftAmt = (raw.sExp + 2.S).asUInt
    val shifted = Wire(UInt(9.W))
    shifted := (raw.sig << shiftAmt(2, 0))(8, 0)
    
    val magnitude = shifted(7, 0)  
    val signed = Mux(raw.sign, -(magnitude.zext.asSInt), magnitude.zext.asSInt)
    Mux(raw.isZero, 0.S, signed)
  }

  val fixed_in_fp6 = fp6ToFixedPoint(io.in_fp6)
  val fixedLuts = io.in_lut.map(fp6ToFixedPoint)

  val diffs = Wire(Vec(16, UInt(9.W)))
  for (i <- 0 until 16) {
    val diff = fixed_in_fp6 - fixedLuts(i)
    diffs(i) := Mux(diff < 0.S, (-diff).asUInt, diff.asUInt)(8, 0)
  }

  val diffPairs: Seq[(UInt, UInt)] = diffs.zipWithIndex.map { case (d, i) => (d, i.U(4.W)) }
  val minResult = diffPairs.reduce { (a, b) =>
    val (d1, i1) = a
    val (d2, i2) = b
    (Mux(d1 <= d2, d1, d2), Mux(d1 <= d2, i1, i2))
  }

  io.nearestIdx := minResult._2
}
class FP6E2M3NearestFinderRef extends RawModule {
  val io = IO(new Bundle {
    val in_fp6 = Input(UInt(6.W))
    val in_lut = Input(Vec(16, UInt(6.W)))
    val nearestIdx = Output(UInt(4.W))
  })

  def fixedPoint(in: UInt): SInt = {
    val sign = in(5)
    val exp  = in(4, 3)
    val mant = in(2, 0)
    val shift = Mux(exp === 0.U, 0.U(2.W), exp - 1.U)          // 0,0,1,2 for exp field 0,1,2,3
    val mag   = Mux(exp === 0.U, mant.pad(8), (((8.U(5.W) +& mant) << shift)(7, 0)))  // 8-bit, value*8 max 60
    Mux(sign, -(mag.zext), mag.zext)                           // 9-bit SInt; zero (mag=0) handled naturally
  }

  val fixed_in  = fixedPoint(io.in_fp6)
  val fixedLuts = io.in_lut.map(fixedPoint)

  val diffs = Wire(Vec(16, UInt(9.W)))
  for (i <- 0 until 16) {
    val diff = fixed_in - fixedLuts(i)
    diffs(i) := Mux(diff < 0.S, (-diff).asUInt, diff.asUInt)(8, 0)
  }

  val diffPairs: Seq[(UInt, UInt)] = diffs.zipWithIndex.map { case (d, i) => (d, i.U(4.W)) }
  val minResult = diffPairs.reduce { (a, b) =>
    val (d1, i1) = a
    val (d2, i2) = b
    (Mux(d1 <= d2, d1, d2), Mux(d1 <= d2, i1, i2))
  }
  io.nearestIdx := minResult._2
}

class FP8NearestFinderRef(altfmt: Boolean) extends RawModule {
  private val expW              = if (altfmt) 5 else 4
  private val mantW             = if (altfmt) 2 else 3
  private val bias              = if (altfmt) 15 else 7
  private val sigW              = mantW + 1                 // implicit bit + mantissa
  private val minSExp           = 1 - bias                 // subnormals share the min normal exponent
  private val maxNormalExpField = if (altfmt) 30 else 15   // largest finite exponent field
  private val maxShift          = maxNormalExpField - 1    // == maxSExp - minSExp
  private val fixedW            = sigW + maxShift
  private val shiftW            = log2Ceil(maxShift + 1)

  val io = IO(new Bundle {
    val in         = Input(UInt(8.W))
    val in_lut     = Input(Vec(16, UInt(8.W)))
    val nearestIdx = Output(UInt(4.W))
  })

  // Map an FP8 value to a monotonic fixed-point magnitude so "nearest" reduces
  // to a plain integer abs-difference. Scaled so the smallest subnormal is 1.
  def fp8ToFixedPoint(in: UInt): SInt = {
    val sign = in(7)
    val exp  = in(6, 7 - expW)
    val mant = in(mantW - 1, 0)

    val isZero      = (exp === 0.U) && (mant === 0.U)
    val implicitBit = Mux(exp === 0.U, 0.U(1.W), 1.U(1.W))
    val sig         = Cat(implicitBit, mant)                          // sigW bits
    val sExp        = Mux(exp === 0.U, minSExp.S(8.W), exp.zext - bias.S)

    val shiftAmt = (sExp + (bias - 1).S).asUInt                       // range 0..maxShift
    val shifted  = (sig << shiftAmt(shiftW - 1, 0))(fixedW - 1, 0)

    val signed = Mux(sign, -(shifted.zext.asSInt), shifted.zext.asSInt)
    Mux(isZero, 0.S, signed)
  }

  val fixed_in  = fp8ToFixedPoint(io.in)
  val fixedLuts = io.in_lut.map(fp8ToFixedPoint)

  val diffW = fixedW + 1
  val diffs = Wire(Vec(16, UInt(diffW.W)))
  for (i <- 0 until 16) {
    val diff = fixed_in - fixedLuts(i)
    diffs(i) := Mux(diff < 0.S, (-diff).asUInt, diff.asUInt)(diffW - 1, 0)
  }

  val diffPairs: Seq[(UInt, UInt)] = diffs.zipWithIndex.map { case (d, i) => (d, i.U(4.W)) }
  val minResult = diffPairs.reduce { (a, b) =>
    val (d1, i1) = a
    val (d2, i2) = b
    (Mux(d1 <= d2, d1, d2), Mux(d1 <= d2, i1, i2))
  }

  io.nearestIdx := minResult._2
}

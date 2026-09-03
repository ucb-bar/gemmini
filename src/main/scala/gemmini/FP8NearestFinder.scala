package gemmini

import chisel3._
import chisel3.util._

// Nearest-neighbour finder for FP8 non-uniform quantization.
// Given an FP8 value and a 16-entry LUT of FP8 values, it returns the 4-bit
// index of the LUT entry closest in magnitude. Projecting FP8 -> 4-bit index
// is what lets NUQ data be stored with 4-bit memory accesses, like the FP6 path.
//
// The FP8 sub-format is fixed at elaboration (compile-time capability):
//   altfmt = false => E4M3 (4-bit exp, bias 7,  3-bit mantissa)
//   altfmt = true  => E5M2 (5-bit exp, bias 15, 2-bit mantissa)
// Using a Scala Boolean (not a Chisel Bool) specializes the module: only the
// selected decoder is generated, and the fixed-point datapath is sized to the
// chosen format (E4M3: 18-bit, E5M2: 32-bit) rather than the max of both.
class FP8NearestFinder(altfmt: Boolean) extends RawModule {
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

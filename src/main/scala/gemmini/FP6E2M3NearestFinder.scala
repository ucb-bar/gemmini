package gemmini

import chisel3._
import chisel3.util._

// Nearest-LUT finder for FP6 E2M3 (sign[5] | exp2[4:3] bias1 | man3[2:0]), the 4-wide-via-LUT sibling
// of E3M2 on code1/altfmt1. Fixed-point = exact value * 8 (unit 2^-3): subnormal (exp field 0) -> mant;
// normal (field f in 1..3) -> (8 + mant) << (f - 1). Matches mx_fp_math.h::fp6_e2m3_to_fixed_point and
// lut_mapping_demo.fp6_e2m3_to_fixed_point (MxQuant-consistent E2M3, emax=2, max_norm 7.5).
class FP6E2M3NearestFinder extends RawModule {
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

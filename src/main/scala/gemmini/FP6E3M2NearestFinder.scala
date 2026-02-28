package gemmini

import chisel3._
import chisel3.util._

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

class FP6E3M2NearestFinder extends RawModule {
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
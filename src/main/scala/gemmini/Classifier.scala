package gemmini 

import chisel3._
import chisel3.util._

class MxClassifiedFp(format: MxFormats) extends Bundle {
  val isNaN = Bool()
  val isInf = Bool()
  val isZero = Bool()
  val isSub = Bool()
  val sign = UInt(1.W)
  val exp = UInt((format.exp).W)
  val sig = UInt((format.sig - 1).W)
}

object requiredPEMode {
  def apply(a: MxTypes, w: MxTypes): mxMode = {
    val key = Cat(a.sig, w.sig)
    val idx = MuxLookup(key, 0.U) (Seq(
      Cat(2.U(3.W), 2.U(3.W)) -> 0.U,
      Cat(2.U(3.W), 3.U(3.W)) -> 1.U,
      Cat(2.U(3.W), 4.U(3.W)) -> 2.U,
      Cat(3.U(3.W), 2.U(3.W)) -> 3.U,
      Cat(3.U(3.W), 3.U(3.W)) -> 4.U,
      Cat(3.U(3.W), 4.U(3.W)) -> 5.U,
      Cat(4.U(3.W), 2.U(3.W)) -> 6.U,
      Cat(4.U(3.W), 3.U(3.W)) -> 7.U,
      Cat(4.U(3.W), 4.U(3.W)) -> 8.U
    ))

    mxModeDecode(idx)
  }
}

object classify {
  def apply(dataType: MxFormats, in: Bits): MxClassifiedFp = {
    val width = dataType.sig + dataType.exp
    val sign = in(width - 1)
    val exp = in(width - 2, dataType.sig - 1)
    val sig = in(dataType.sig - 2, 0)

    val out = Wire(new MxClassifiedFp(dataType))

  if (dataType.sig + dataType.exp == 8) {
    val isNaN = exp.andR && sig.orR
    val isInf = if (dataType.exp == 5) exp.andR && !sig.orR else false.B
    out.isInf := isInf
    out.isNaN := isNaN
  } else {
    out.isNaN := DontCare
    out.isInf := DontCare
  }
    val isZero = (exp === 0.U) && (sig === 0.U)
    val isSub = (exp === 0.U) && (sig =/= 0.U)

    out.sig := sig(dataType.sig - 2, 0)
    out.exp := Mux(isSub, 
                   1.U(dataType.exp.W) -% dataType.bias.U(dataType.exp.W),
                   exp -% dataType.bias.U(dataType.exp.W)) -% 1.U(1.W)
    out.sign := sign
    out.isZero := isZero
    out.isSub := isSub

    out
  }
}
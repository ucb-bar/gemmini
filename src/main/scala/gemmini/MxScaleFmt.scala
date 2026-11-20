package gemmini

import chisel3._
import chisel3.util._

// Runtime block-scale formats (mx_flex_scales). fmt: 0 = E8M0, 1 = E8M0 with round-to-nearest encode,
// X in 2..7 = eXmY (X exponent bits relative to a signed per-operand base, Y = 8 - X mantissa bits, no sign,
// saturating). Scales stay one byte, so storage and DMA layout are unchanged.
class MxScaleFmtCfg extends Bundle {
  val a_fmt = UInt(3.W)
  val w_fmt = UInt(3.W)
  val out_fmt = UInt(3.W)
  val a_base = SInt(8.W)
  val w_base = SInt(8.W)
  val out_base = SInt(8.W)
}

object MxScaleFmt {
  val E8M0 = 0
  val E8RNE = 1
  val mantW = 6   // decoded mantissa: fraction bits of 1.m, Y <= 6

  def isPow2(fmt: UInt): Bool = fmt <= E8RNE.U

  // code -> (biased exponent as an E8M0 code, 0..254 for eXmY; 6-bit fraction of 1.m)
  def decode(code: UInt, fmt: UInt, base: SInt): (UInt, UInt) = {
    val exyz = (2 to 7).map { x =>
      val y = 8 - x
      val e = code(7, y)
      val m = if (y > 0) code(y - 1, 0) else 0.U
      val be = base +& e.zext +& 127.S
      val bexp = Mux(be < 0.S, 0.U, Mux(be > 254.S, 254.U, be.asUInt(7, 0)))
      val mant = if (y > 0) (m << (mantW - y)).asUInt(mantW - 1, 0) else 0.U(mantW.W)
      (x.U === fmt) -> (bexp, mant)
    }
    val bexp = MuxCase(code, exyz.map { case (c, (b, _)) => c -> b })
    val mant = MuxCase(0.U(mantW.W), exyz.map { case (c, (_, m)) => c -> m })
    (bexp, mant)
  }

  // bf16 x 2^(e9 - 254) x (1.mA)(1.mW): exact product, one RNE to bf16. Unit mantissas, zero/subnormal and Inf/NaN
  // inputs take the exponent-add path unchanged (== AccumulatorMem.applyE9M0Scale).
  def applyBf16(v: UInt, e9: UInt, mA: UInt, mW: UInt, pow2: UInt => UInt): UInt = {
    val sign = v(15)
    val exp = v(14, 7)
    val man = v(6, 0)
    val p = (Cat(1.U(1.W), mA) * Cat(1.U(1.W), mW))(13, 0)       // [1,4), 12 fraction bits
    val prod = Cat(1.U(1.W), man) * p                                // [1,8), 19 fraction bits, 22 bits
    val sh = Mux(prod(21), 2.U, Mux(prod(20), 1.U, 0.U))
    val norm = (prod >> sh)(19, 0)                                   // [1,2), 19 fraction bits
    val keep = norm(19, 12)
    val g = norm(11)
    val st = norm(10, 0).orR
    val up = g && (st || keep(0))
    val rnd = keep +& up                                             // 9 bits, carry when 1.111..1 rounds up
    val carry = rnd(8)
    val manOut = Mux(carry, 0.U(7.W), rnd(6, 0))
    val newExp = exp.zext +& e9.zext -& 254.S +& sh.zext +& carry.zext
    val ce = Mux(newExp < 0.S, 0.U(8.W), Mux(newExp > 255.S, 255.U(8.W), newExp.asUInt(7, 0)))
    val bypass = (mA === 0.U && mW === 0.U) || exp === 0.U || exp.andR
    Mux(bypass, pow2(v), Cat(sign, ce, manOut))
  }

  // Encode s = amax / pmax as fmt. amax: biased exp (EPS-clamped) + 7-bit fraction; pmax = pmaxSig/8 x 2^log2pmax.
  // Returns (scale code, unbiased exponent of the encoded s, 6-bit fraction of its mantissa).
  def encode(amaxExp: UInt, amaxMan: UInt, pmaxSig: UInt, log2pmax: UInt, fmt: UInt, base: SInt): (UInt, SInt, UInt) = {
    val n = Cat(1.U(1.W), amaxMan) << 12                             // amax 1.f x 2^19
    val q = n / pmaxSig                                              // (amax/pmax mantissa ratio) x 2^16, in (2^15, 2^18)
    val r = (n % pmaxSig) =/= 0.U
    val ge1 = q >= (1 << 16).U
    val qn = Mux(ge1, q, q << 1)(16, 0)                              // [1,2) with 16 fraction bits
    val e0 = amaxExp.zext -& 127.S -& log2pmax.zext -& Mux(ge1, 0.S, 1.S)
    val outs = (1 to 7).map { x =>
      val y = if (x == 1) 0 else 8 - x
      val keep = qn(16, 16 - y)
      val g = qn(15 - y)
      val st = qn(14 - y, 0).orR || r
      val up = g && (st || keep(0))
      val rnd = keep +& up
      val carry = rnd(y + 1)
      val mant = if (y > 0) Mux(carry, 0.U(y.W), rnd(y - 1, 0)) else 0.U(1.W)
      val es = e0 +& carry.zext
      val code = Wire(UInt(8.W))
      val esat = Wire(SInt(10.W))
      val msat = Wire(UInt(mantW.W))
      if (x == 1) {
        val b = es +& 127.S
        code := Mux(b < 0.S, 0.U, Mux(b > 254.S, 254.U, b.asUInt(7, 0)))
        esat := code.zext -& 127.S
        msat := 0.U
      } else {
        val emax = (1 << x) - 1
        val e = es -& base
        val lo = e < 0.S
        val hi = e > emax.S
        val ef = Mux(lo, 0.U, Mux(hi, emax.U, e.asUInt(x - 1, 0)))
        val mf = Mux(lo, 0.U, Mux(hi, ((1 << y) - 1).U, mant))
        code := Cat(ef, mf)(7, 0)
        esat := base +& ef.zext
        msat := (mf << (mantW - y)).asUInt(mantW - 1, 0)
      }
      (x.U === fmt || (x == 1).B && fmt === E8RNE.U) -> (code, esat, msat)
    }
    val code = MuxCase(0.U(8.W), outs.map { case (c, (cd, _, _)) => c -> cd })
    val es = MuxCase(0.S(10.W), outs.map { case (c, (_, e, _)) => c -> e })
    val ms = MuxCase(0.U(mantW.W), outs.map { case (c, (_, _, m)) => c -> m })
    (code, es, ms)
  }

  // bf16 x / (1.ms), rounded to odd at bf16 so the following RNE to the element format is a single rounding.
  def divBf16(v: UInt, ms: UInt): UInt = {
    val exp = v(14, 7)
    val n = Cat(1.U(1.W), v(6, 0)) << 11                             // x 1.f x 2^18
    val d = Cat(1.U(1.W), ms)                                        // 1.ms x 2^6
    val q = n / d                                                    // x/ms x 2^12, in (2^11, 2^13)
    val r = (n % d) =/= 0.U
    val ge1 = q(12)
    val sig = Mux(ge1, q(12, 5), q(11, 4))
    val lost = Mux(ge1, q(4, 0).orR, q(3, 0).orR) || r
    val manOut = sig(6, 0) | lost.asUInt
    val newExp = exp -& Mux(ge1, 0.U, 1.U)
    val bypass = ms === 0.U || exp === 0.U || exp.andR
    Mux(bypass, v, Cat(v(15), newExp(7, 0), manOut))
  }
}

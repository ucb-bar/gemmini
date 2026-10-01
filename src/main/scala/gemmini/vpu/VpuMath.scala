package gemmini.vpu

import chisel3._
import chisel3.util._
import hardfloat._

// Per-lane BF16 math for the VPU (call inside a Module). Add/sub/mul/round use chipyard hardfloat (RNE).
// exp, rcp and sqrt follow the atlas-npu lane boxes (Exp/ExLUT, Rcp/RcpLUT, Sqrt/SqrtLUT, same tables), with
// two fixes: exp(x <= -128) = 0 (atlas' fixed-point input wraps there) and rcp underflow -> 0 (atlas wraps to Inf).
object VpuMath {
  val EXP = 8
  val SIG = 8
  private def rne = consts.round_near_even

  def rec(x: UInt): UInt = recFNFromFN(EXP, SIG, x)
  def unrec(x: UInt): UInt = fNFromRecFN(EXP, SIG, x)

  // a*1 +/- b: hardfloat's AddRecFN does not elaborate at sigWidth 8; the product is exact, so one RNE rounding
  def add(a: UInt, b: UInt, sub: Bool): UInt = {
    val m = Module(new MulAddRecFN(EXP, SIG))
    m.io.op := Cat(0.U(1.W), sub); m.io.a := rec(a); m.io.b := rec("h3f80".U(16.W)); m.io.c := rec(b)
    m.io.roundingMode := rne; m.io.detectTininess := consts.tininess_afterRounding
    unrec(m.io.out)
  }

  def mul(a: UInt, b: UInt): UInt = {
    val m = Module(new MulRecFN(EXP, SIG))
    m.io.a := rec(a); m.io.b := rec(b)
    m.io.roundingMode := rne; m.io.detectTininess := consts.tininess_afterRounding
    unrec(m.io.out)
  }

  // ---- FP32 (recoded, 33 bits) for row sums ----
  def bf16ToF32Rec(x: UInt): UInt = recFNFromFN(8, 24, Cat(x, 0.U(16.W)))
  def addF32Rec(a: UInt, b: UInt): UInt = {
    val m = Module(new AddRecFN(8, 24))
    m.io.subOp := false.B; m.io.a := a; m.io.b := b
    m.io.roundingMode := rne; m.io.detectTininess := consts.tininess_afterRounding
    m.io.out
  }
  def f32RecToBf16(x: UInt): UInt = {
    val m = Module(new RecFNToRecFN(8, 24, EXP, SIG))
    m.io.in := x; m.io.roundingMode := rne; m.io.detectTininess := consts.tininess_afterRounding
    unrec(m.io.out)
  }

  // ---- compare (sign-magnitude -> ordered integer, as atlas compareReturnMax) ----
  def ordered(x: UInt): UInt = Mux(x(15), ~x, x ^ "h8000".U(16.W))
  def max(a: UInt, b: UInt): UInt = Mux(ordered(a) > ordered(b), a, b)
  def abs(x: UInt): UInt = Cat(0.U(1.W), x(14, 0))

  // ---- rcp: 128-entry 1/(1+f) table (Q1.16), exponent from the table MSB (atlas lutFixedToBf16Rcp) ----
  private def rcpTable = VecInit.tabulate(128) { i =>
    BigInt(math.round((1.0 / (1.0 + i / 128.0)) * 65536)).U(17.W)
  }
  def rcp(x: UInt): UInt = {
    val neg = x(15); val e = x(14, 7); val f = x(6, 0)
    val y = rcpTable(f)
    val hi = Log2(y)
    val bfExp = 254.S(11.W) + (hi.zext - 16.S) - e.zext
    val frac = (y >> Mux(hi > 7.U, hi - 7.U, 0.U))(6, 0)
    val inf = Cat(neg, "hff".U(8.W), 0.U(7.W))
    val zero = Cat(neg, 0.U(15.W))
    MuxCase(Cat(neg, bfExp(7, 0), frac), Seq(
      (e === 255.U) -> zero,                   // 1/inf, NaN -> 0 (as atlas)
      (e === 0.U || bfExp >= 255.S) -> inf,    // 1/0, 1/subnormal -> inf
      (bfExp <= 0.S) -> zero))                 // underflow -> 0 (fix)
  }

  // ---- sqrt: 128-entry sqrt(1+f) table, *sqrt(2) for odd unbiased exponents (atlas Sqrt/SqrtLUT) ----
  private def sqrtTable = VecInit.tabulate(128) { i =>
    BigInt(math.round(math.sqrt(1.0 + i / 128.0) * 65536)).U(17.W)
  }
  private val sqrt2 = BigInt(math.round(math.sqrt(2.0) * 65536))
  def sqrt(x: UInt): UInt = {
    val e = x(14, 7); val f = x(6, 0)
    val t = sqrtTable(f)
    val base = Mux(!e(0), ((t * sqrt2.U) >> 16)(16, 0), t)
    val hi = Log2(base)
    val bfExp = 127.S(11.W) + ((hi.zext - 16.S + e.zext - 127.S) >> 1)
    val frac = (base >> Mux(hi > 7.U, hi - 7.U, 0.U))(6, 0)
    val isNaN = e === 255.U && f =/= 0.U
    val isInf = e === 255.U && f === 0.U
    MuxCase(Cat(0.U(1.W), bfExp(7, 0), frac), Seq(
      (e === 0.U || isNaN) -> 0.U(16.W),                       // sqrt(0/subnormal/NaN) -> +0 (as atlas)
      (isInf || bfExp >= 255.S) -> "h7f80".U(16.W)))            // sign ignored (sqrt|x|), as atlas
  }

  def rsqrt(x: UInt): UInt = rcp(sqrt(x))

  // ---- exp, split in two pipeline stages (atlas Exp, isBase2 = 0) ----
  // Stage A: special cases + fixed-point range reduction x*log2(e) = k + r. Packs
  // {early(1), earlyRes(16), k(10, signed), r(12)} = 39 bits.
  val expMidW = 39
  def expStageA(x: UInt): UInt = {
    val raw = rawFloatFromFN(EXP, SIG, x)
    val sign = x(15); val e = x(14, 7); val f = x(6, 0)
    val of = !sign && (e > "h85".U || (e === "h85".U && f > "h31".U))
    val uf = sign && e >= "h86".U                               // x <= -128: exp underflows to 0 (fix)
    val early = raw.isNaN || raw.isZero || raw.isInf || of || uf
    val earlyRes = MuxCase(0.U(16.W), Seq(
      raw.isNaN -> Cat(raw.sign, "hff".U(8.W), isSigNaNRawFloat(raw), "h3f".U(6.W)),
      raw.isZero -> "h3f80".U(16.W),
      (raw.isInf && raw.sign) -> 0.U(16.W),
      ((raw.isInf && !raw.sign) || of) -> "h7f80".U(16.W),
      uf -> 0.U(16.W)))
    // Q9.12 value of x (atlas qmnFromRawFloat)
    val shift = 12.S + (raw.sExp - 256.S) - 7.S
    val sigWide = raw.sig.pad(21)
    val magWide = Mux(shift < 0.S, sigWide >> (-shift).asUInt, (sigWide << shift(5, 0).asUInt)(20, 0))
    val mag = magWide(20, 0)
    val q = Mux(raw.sign, (0.U(21.W) - mag)(20, 0), mag).asSInt
    // * 1/ln2 in Q2.12 (5909), >> 12 -> k (integer part) and r (12-bit fraction)
    val prod = ((q * 5909.S(15.W)) >> 12)(22, 0).asSInt
    val k = (prod >> 12)(9, 0)
    val r = prod.asUInt(11, 0)
    Cat(early, earlyRes, k, r)
  }

  private def expTable = VecInit.tabulate(32) { i => BigInt(math.round(math.pow(2.0, i / 32.0) * 65536)).U(17.W) }
  // Stage B: 2^r by 32-entry table + linear interpolation, scaled by 2^k, rounded to BF16.
  def expStageB(mid: UInt): UInt = {
    val early = mid(38); val earlyRes = mid(37, 22)
    val k = mid(21, 12).asSInt; val r = mid(11, 0)
    val addr = r(11, 7); val rLow = r(6, 0)
    val y0 = expTable(addr)
    val y1 = Mux(addr === 31.U, ((1 << 17) - 1).U(17.W), expTable(addr + 1.U))
    val interp = (y0 + (((y1 - y0) * rLow) >> 7))(16, 0)
    val rawOut = Wire(new RawFloat(EXP, SIG + 2))
    rawOut.isNaN := false.B; rawOut.isInf := false.B; rawOut.isZero := false.B; rawOut.sign := false.B
    rawOut.sExp := k + 256.S(10.W)
    val sigGR = interp >> 7
    val pre = Cat(0.U(1.W), sigGR(9, 0))
    rawOut.sig := Mux(interp(6, 0).orR, pre | 1.U, pre)
    val round = Module(new RoundRawFNToRecFN(EXP, SIG, 0))
    round.io.invalidExc := false.B; round.io.infiniteExc := false.B; round.io.in := rawOut
    round.io.roundingMode := rne; round.io.detectTininess := consts.tininess_beforeRounding
    Mux(early, earlyRes, unrec(round.io.out))
  }
}

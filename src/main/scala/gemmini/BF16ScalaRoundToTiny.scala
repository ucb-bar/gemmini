package gemmini

import chisel3._
import hardfloat._
import freechips.rocketchip.tile._
import chisel3.util._


object fp8ToE5M3 {
  def apply(in: UInt, altfmt: Bool): UInt = {
    Mux(altfmt, { // E5M2
      in ## 0.U(1.W)
    }, { // E4M3
      val sign = in(7)
      val exp  = in(6, 3)
      val sig  = in(2, 0)
      val isNaN        = exp.andR && sig.andR
      val isSubnormal  = !exp.orR
      val subnormShift = PriorityEncoder(Reverse(sig))
      val subnormSig   = ((sig << 1.U) << subnormShift)(2, 0)
      val subnormExp   = 8.U(5.W) - subnormShift

      Mux(isNaN,
        sign ## "b11111100".U(8.W), // NaN
        Mux(isSubnormal,
          Mux(sig.orR,
            sign ## subnormExp ## subnormSig, // Subnormal
            sign ## "b00000000".U(8.W)        // Zero
          ),
          sign ## ((0.U(1.W) ## exp) + 8.U) ## sig // Normal
        )
      )
    })
  }
}

object E3M1Tofp4 {
  def isE3M1NaN(in: UInt): Bool = { in(3, 1) === "b111".U(3.W) && in(0) }
  def isE3M1Inf(in: UInt): Bool = { in(3, 1) === "b111".U(3.W) && !in(0) }
  val FP4Min = "b001".U(3.W)
  val FP4Max = "b111".U(3.W)

  def apply(in: UInt): UInt = {
    val sign      = in(4)
    val exp       = in(3, 1)
    val sig       = in(0)

    val biasDiff = 2.U(3.W)
    val isZero = (exp === 0.U) && (sig === 0.U)
    val isSpecial = isE3M1NaN(in) || isE3M1Inf(in)

    val mapToZero   = (exp < 2.U) && !(exp === 1.U && sig === 1.U) // exp<2 except (exp=1, sig=1)
    val mapToSubnorm = ((exp === 2.U) && (sig === 0.U)) || ((exp === 1.U) && (sig === 1.U)) 
    val mapToMinNorm = (exp === 2.U) && (sig === 1.U)

    val mapToMax = (exp > (biasDiff +& 3.U)) || isSpecial

    val exp_adj = (exp - biasDiff)(1, 0)
    val outNorm = sign ## exp_adj ## sig
    val outMinNorm = sign ## "b010".U(3.W)

    Mux(mapToZero,
      0.U(4.W),   // reference canonicalizes zero to +0 (bf16_bits_to_fp4_e2m1_code returns 0, no sign)
      Mux(mapToMax,
        sign ## FP4Max,
        Mux(mapToSubnorm,
          sign ## FP4Min, 
          Mux(mapToMinNorm,
            outMinNorm,
            outNorm
          )
        )
      )
    )
  }
}


object E4M2ToFp6 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 7)

    val sign  = in(6)
    val exp   = in(5, 2) // 4-bit exponent (E4)
    val sig   = in(1, 0) // 2-bit fraction

    val biasDiff = 4.U(4.W) 
    val isZero = (exp === 0.U) && (sig === 0.U)
    val mapToZero = (exp <= 2.U) || isZero

    val mapToSubnorm = (exp <= biasDiff) && !mapToZero

    val k = WireInit(0.U(2.W))
    when (exp === 3.U) {
      k := MuxLookup(sig, 0.U) (Seq(
        "b00".U -> 1.U,
        "b01".U -> 1.U,
        "b10".U -> 2.U,
        "b11".U -> 2.U
      ))
    } .elsewhen (exp === 4.U) {
      k := MuxLookup(sig, 0.U) (Seq(
        "b00".U -> 2.U,
        "b01".U -> 2.U,
        "b10".U -> 3.U,
        "b11".U -> 3.U
      ))
    }
    val outSub = sign ## 0.U(3.W) ## k

    val normExp = (exp - biasDiff)(2, 0)
    val normSig = sig
    val outNorm = sign ## normExp ## normSig


    val mapToMax = (exp > (biasDiff +& 7.U))
    val outMax = sign ## "b11111".U(5.W)

    val out = Wire(UInt(6.W))
    when (mapToZero) {
      out := Cat(sign, 0.U(5.W))   // underflow keeps its sign, as the reference does
    } .elsewhen (mapToMax) {
      out := outMax
    } .elsewhen (mapToSubnorm) {
      out := outSub
    } .otherwise {
      out := outNorm
    }

    out
  }
}

object E5M3ToFp8 {
  def isE5M3NaN(in: UInt): Bool = { in(7, 3) === "b11111".U(5.W) && in(2, 0).orR }
  def isE5M3Inf(in: UInt): Bool = { in(7, 3) === "b11111".U(5.W) && !in(2, 0).orR }
  val FP8Min = "b0000001".U(7.W)
  val FP8Max = "b1111110".U(7.W)

  def apply (in: UInt): UInt = {
    require(in.getWidth == 9)
    val sign = in(8)
    val exp  = in(7, 3) // 5-bit exponent (E5)
    val sig  = in(2, 0) // 3-bit fraction

    val isZero = (exp === 0.U) && (sig === 0.U)
    val biasDiff = 8.U(5.W)
    val isSpecial = isE5M3NaN(in) || isE5M3Inf(in)

    val mapToZero = (exp <= 5.U) || isZero
    val mapToSubnorm = (exp >= 6.U) && (exp <= 8.U)
    val mapToMax = (exp > (23.U)) || isSpecial || (exp === 23.U && sig.andR)

    val exp_adj = (exp - biasDiff)(3, 0)
    val outNorm = sign ## exp_adj ## sig

    val outMinNorm = sign ## "b0001000".U(7.W)

    val outSub = WireInit(0.U(8.W))

    when (exp === 6.U) {
      val k = Mux(sig(2), 2.U(3.W), 1.U(3.W))
      outSub := sign ## 0.U(4.W) ## k
    } .elsewhen (exp === 7.U) {
      val k = MuxLookup(sig, 2.U) (Seq(
        "b000".U -> 2.U,
        "b001".U -> 2.U,
        "b010".U -> 2.U,
        "b011".U -> 3.U,
        "b100".U -> 3.U,
        "b101".U -> 3.U,
        "b110".U -> 4.U,
        "b111".U -> 4.U
      ))
      outSub := sign ## 0.U(4.W) ## k
    } .elsewhen (exp === 8.U) {
      val k = MuxLookup(sig, 4.U) (Seq(
        "b000".U -> 4.U,
        "b001".U -> 4.U,
        "b010".U -> 5.U,
        "b011".U -> 6.U,
        "b100".U -> 6.U,
        "b101".U -> 6.U,
        "b110".U -> 7.U
      ))
      outSub := Mux(sig === "b111".U, outMinNorm, sign ## 0.U(4.W) ## k)
    }

    // Unused for FP8 (BF16ToE4M3 replaced it); kept because the FP6/FP4 paths share this structure.
    Mux(mapToZero, Cat(sign, 0.U(7.W)),
      Mux(mapToMax, sign ## FP8Max,
        Mux(mapToSubnorm, outSub, outNorm)))
  }
}

// BF16 -> E4M3 code in one rounding step: RNE (OCP round='even'), subnormals allowed, saturating to
// +-448 (matches microxcaling's even/saturate/allow_denorm quantizer). Done directly rather than
// via hardfloat, which reserves E4M3's exp field 15 for Inf/NaN and would double-round through E5M3.
object BF16ToE4M3 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 16)
    val sign = in(15)
    val e8   = in(14, 7)
    val frac = in(6, 0)

    // Normal target (unbiased exp >= -6): keep 3 of 7 fraction bits, RNE (tie -> even).
    val qn      = frac(6, 4)                               // top 3 kept bits
    val rn      = frac(3)                                  // round bit
    val stickyn = frac(2, 0).orR
    val kn_raw  = qn +& (rn & (stickyn | qn(0)))           // 0..8
    val carry   = kn_raw === 8.U
    val kn      = Mux(carry, 0.U(3.W), kn_raw(2, 0))
    val expf    = (e8 +& carry.asUInt).zext.asSInt - 120.S // (e8 + carry - 127) + 7
    val satN    = (expf > 15.S) || ((expf === 15.S) && (kn === 7.U))
    val magN    = Mux(satN, 0x7E.U(7.W), Cat(expf.asUInt(3, 0), kn))

    // Subnormal target (unbiased exp < -6, i.e. e8 < 121): quantum 2^-9, so k = RNE(sig8 / 2^sh) with
    // sh = 125 - e8 (>= 5 here). sh >= 9 flushes to 0 (max sig8 < 2^8 < half of 2^9), so clamp the shift.
    val sig8   = Cat(1.U(1.W), frac)                       // value = sig8 * 2^(e8-127-7)
    val sh_raw = 125.U(8.W) - e8
    val flushS = sh_raw >= 9.U
    val sh     = Mux(sh_raw < 5.U, 5.U(5.W), Mux(sh_raw > 9.U, 9.U(5.W), sh_raw(4, 0)))
    val stepS  = (1.U(11.W) << sh)
    val halfS  = (stepS >> 1)
    val remS   = sig8 & (stepS - 1.U)(7, 0)
    val kBaseS = (sig8 >> sh)
    val ksUp   = kBaseS + Mux(remS > halfS(7, 0), 1.U, Mux(remS === halfS(7, 0), kBaseS(0), 0.U))
    val ks     = Mux(flushS, 0.U(5.W), ksUp(4, 0))         // 0..8, RNE
    val magS   = Mux(ks >= 8.U, 0x08.U(7.W), Cat(0.U(4.W), ks(2, 0)))

    val useSub = e8 < 121.U
    Mux(e8.andR, 0x7F.U(8.W),                              // NaN/Inf -> E4M3 NaN, sign dropped
      Mux(e8 === 0.U, Cat(sign, 0.U(7.W)),                 // zero / BF16 subnormal -> signed zero
        Cat(sign, Mux(useSub, magS, magN))))
  }
}

// BF16 -> 8-bit FP8 E5M2 code (1 sign | 5 exp | 2 mant, bias 15). RNE (OCP round='even'), overflow
// -> 0x7B, Inf/NaN preserved, subnormals down to 2^-16. Chisel port of
// mx_fp_math.h::bf16_bits_to_e5m2_code.
object BF16ToE5M2 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 16)
    val sign = in(15)
    val E    = in(14, 7)
    val M    = in(6, 0)
    val e    = E.zext - 127.S                                  // unbiased exponent (signed)

    // Normal range e in [-14, 15], RNE (tie -> even)
    val q      = M(6, 5)                                        // top 2 mantissa bits
    val r      = M(4)                                           // round bit
    val sticky = M(3, 0).orR
    val sig    = q +& (r & (sticky | q(0)))                     // 0..4
    val carry  = sig(2)
    val mant   = Mux(carry, 0.U(2.W), sig(1, 0))
    val expOut = e + carry.zext.asSInt
    val expField = (expOut + 15.S).asUInt
    val normCode = Cat(sign, expField(4, 0), mant)
    val normOvf  = expOut > 15.S

    // Subnormal e <= -15: k = RNE(sig8 / 2^shift), sig8 = 1.M
    val sig8    = Cat(1.U(1.W), M)                             // 8 bits
    val shiftR  = 0.S - (e + 9.S)
    val shiftRU = shiftR.asUInt
    val flush   = shiftR > 8.S
    val shiftU  = Mux(shiftR < 1.S, 1.U(4.W), Mux(shiftR > 8.S, 8.U(4.W), shiftRU(3, 0)))
    val stepU   = (1.U(9.W) << shiftU)
    val stepM1  = (stepU - 1.U)
    val stepHalf = (stepU >> 1)
    val rem     = sig8 & stepM1(7, 0)
    val half    = stepHalf(7, 0)
    val kShift  = (sig8 >> shiftU)
    val kUp     = kShift(3, 0) + Mux(rem > half, 1.U, Mux(rem === half, kShift(0), 0.U))  // RNE
    val k      = Mux(flush, 0.U(4.W), kUp)
    val subCode = Mux(k === 0.U, Cat(sign, 0.U(7.W)),
                  Mux(k >= 4.U, Cat(sign, "b0000100".U(7.W)),   // min normal 0x04
                                Cat(sign, 0.U(5.W), k(1, 0))))

    val out = Wire(UInt(8.W))
    when (E.andR) {
      out := Mux(M.orR, Cat(sign, "b1111101".U(7.W)), Cat(sign, "b1111100".U(7.W)))  // NaN / Inf
    }.elsewhen (E === 0.U) {
      out := Cat(sign, 0.U(7.W))                                // zero / bf16 subnormal
    }.elsewhen (e >= -14.S && e <= 15.S) {
      out := Mux(normOvf, Cat(sign, "b1111011".U(7.W)), normCode)
    }.elsewhen (e > 15.S) {
      out := Cat(sign, "b1111011".U(7.W))                       // overflow -> max finite
    }.otherwise {
      out := subCode
    }
    out
  }
}

// BF16 -> 6-bit FP6 E2M3 code (1 sign | 2 exp(bias 1) | 3 mant). Chisel port of
// mx_fp_math.h::bf16_bits_to_fp6_e2m3_code, consistent with MxQuant (npu-exploration/MXQuant
// microxcaling fp6_e2m3): ebits=2, mbits=5 (3 mantissa), emax=2, bias=1, max_norm 7.5, quantum
// 2^-3=0.125, subnormals encoded, RNE. exp field runs 1..3 (E_used 0..2). Output is a 6-bit code in
// the low 6 bits of an 8-bit lane (the FP6 LUT path reads the low 6).
object BF16ToE2M3 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 16)
    val sign = in(15)                                         // 1-bit sign -> code bit 5
    val E    = in(14, 7)
    val M    = in(6, 0)
    val e    = E.zext - 127.S                                  // unbiased bf16 exponent

    // Normal (e in [0,2]): 3 mantissa bits, RNE ties-to-even. E_used = min(e, 2).
    val q       = M(6, 4)                                      // top 3 mantissa bits
    val r       = M(3)                                         // round bit
    val sticky  = M(2, 0).orR
    val lsb     = q(0)
    val roundUp = r & (sticky | lsb)                           // ties-to-even
    val mantSum = q +& roundUp                                 // 0..8
    val carry   = mantSum(3)
    val mant    = Mux(carry, 0.U(3.W), mantSum(2, 0))
    val eClip   = Mux(e > 2.S, 2.S, e)                         // clip to emax before carry
    val expOut  = eClip + carry.zext.asSInt                    // 1..3 after +carry
    val normOvf = expOut > 2.S                                 // carried past emax -> saturate 7.5
    val expField = (Mux(normOvf, 2.S, expOut) + 1.S).asUInt    // unbiased 0->1,1->2,2->3
    val mantOut  = Mux(normOvf, 7.U(3.W), mant)
    val normCode = Cat(sign, expField(1, 0), mantOut)         // 6-bit code (sign at bit 5)

    // Subnormal (e < 0): quantum 2^-3. k = RNE(av / 0.125) = RNE(sig8 / 2^(4-e)), sig8 = 1.M (8 bits).
    // Any value with e <= -5 (shSub >= 9) rounds to 0 (av*8 < 0.5), so flush -- this also avoids the
    // 16-bit `stepU` shift overflowing for very small e (shSub >= 16), which would spuriously yield k=1.
    val sig8  = Cat(1.U(1.W), M)                               // 8 bits
    val shSub = (4.S - e).asUInt                               // >= 5
    val flushSub = shSub >= 9.U
    val stepU = (1.U(16.W) << shSub)
    val halfU = (stepU >> 1)
    val remU  = sig8 & (stepU - 1.U)(15, 0)
    val kBase = (sig8 >> shSub)
    val kUp   = kBase + Mux(remU > halfU(15,0), 1.U, Mux(remU === halfU(15,0), kBase(0), 0.U))  // RNE
    val kSub  = Mux(flushSub, 0.U(4.W), kUp(3, 0))
    val subCode = Mux(kSub === 0.U, Cat(sign, 0.U(5.W)),
                  Mux(kSub >= 8.U, Cat(sign, "b01000".U(5.W)),   // -> min normal (exp field 1, mant 0)
                                   Cat(sign, 0.U(2.W), kSub(2, 0))))  // subnormal exp field 0, mant k

    val out = Wire(UInt(8.W))
    when (E === 0.U || E.andR) {
      out := Cat(sign, 0.U(5.W))                               // zero / bf16-subnormal / non-finite -> code 0
    }.elsewhen (e < 0.S) {
      out := subCode
    }.otherwise {
      out := normCode                                          // normals + overflow (saturate 7.5)
    }
    out
  }
}

// BF16 -> 6-bit FP6 E3M2 code (1 sign | 3 exp(bias 3) | 2 mant). Rounds STRAIGHT to the E3M2 grid,
// RNE (OCP round='even'), subnormals kept (allow_denorm). Chisel port of the rewritten
// mx_fp_math.h::bf16_bits_to_fp6_e3m2_code (sibling of BF16ToE2M3): emin=-2, emax=4, max_norm 28,
// subnormal quantum 2^-4. Replaces the old BF16->E4M2->E4M2ToFp6 double-rounding path.
object BF16ToE3M2 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 16)
    val sign = in(15)
    val E    = in(14, 7)
    val M    = in(6, 0)
    val e    = E.zext - 127.S                                  // unbiased bf16 exponent

    // Normal (e in [-2,4]): 2 mantissa bits, RNE. E_used = min(e, 4).
    val q        = M(6, 5)                                     // top 2 mantissa bits
    val r        = M(4)                                        // round bit
    val sticky   = M(3, 0).orR
    val roundUp  = r & (sticky | q(0))                         // ties-to-even
    val mantSum  = q +& roundUp                                // 0..4
    val carry    = mantSum(2)
    val mant     = Mux(carry, 0.U(2.W), mantSum(1, 0))
    val eClip    = Mux(e > 4.S, 4.S, e)                        // clip to emax before carry
    val expOut   = eClip + carry.zext.asSInt                   // 1..5
    val normOvf  = expOut > 4.S                                // carried past emax -> saturate 28
    val expField = (Mux(normOvf, 4.S, expOut) + 3.S).asUInt    // unbiased -2->1 ... 4->7
    val mantOut  = Mux(normOvf, 3.U(2.W), mant)
    val normCode = Cat(sign, expField(2, 0), mantOut)          // 6-bit code (sign at bit 5)

    // Subnormal (e < -2): quantum 2^-4. k = RNE(sig8 / 2^(3-e)), sig8 = 1.M (8 bits). shSub >= 9
    // flushes to 0 (max sig8 < half of 2^9) and also guards the wide `stepU` shift for very small e.
    val sig8   = Cat(1.U(1.W), M)                             // 8 bits
    val shSub  = (3.S - e).asUInt                             // >= 6 in this branch
    val flushSub = shSub >= 9.U
    val stepU  = (1.U(16.W) << shSub)
    val halfU  = (stepU >> 1)
    val remU   = sig8 & (stepU - 1.U)(15, 0)
    val kBase  = (sig8 >> shSub)
    val kUp    = kBase + Mux(remU > halfU(15, 0), 1.U, Mux(remU === halfU(15, 0), kBase(0), 0.U))  // RNE
    val kSub   = Mux(flushSub, 0.U(4.W), kUp(3, 0))
    val subCode = Mux(kSub === 0.U, Cat(sign, 0.U(5.W)),
                  Mux(kSub >= 4.U, Cat(sign, "b00100".U(5.W)),      // -> min normal (exp field 1, mant 0)
                                   Cat(sign, 0.U(3.W), kSub(1, 0))))  // subnormal exp field 0, mant k

    val out = Wire(UInt(8.W))
    when (E === 0.U || E.andR) {
      out := Cat(sign, 0.U(5.W))                               // zero / bf16-subnormal / non-finite -> code 0
    }.elsewhen (e < -2.S) {
      out := subCode
    }.otherwise {
      out := normCode                                          // normals + overflow (saturate 28)
    }
    out
  }
}

object roundToMx {
  def apply(scaled_bf16: UInt, inputexpWidth: Int, inputsigWidth: Int, format: FType, pack_function: UInt => UInt): UInt = {
    val out = Wire(UInt(8.W))
    val raw_in = hardfloat.rawFloatFromFN(inputexpWidth, inputsigWidth, scaled_bf16)
    val roundAnyRawFNToRecFN = Module(new RoundAnyRawFNToRecFN(
      inputexpWidth,        // inExpWidth
      inputsigWidth,        // inSigWidth
      format.exp,           // outExpWidth
      format.sig,           // outSigWidth
      0                     // options
    ))

    roundAnyRawFNToRecFN.io.invalidExc    := false.B
    roundAnyRawFNToRecFN.io.infiniteExc   := false.B
    roundAnyRawFNToRecFN.io.in            := raw_in
    // The nibble-format golden (fp4/fp6 use out_requant="model") rounds ties to EVEN (RNE):
    // Spike bf16_bits_to_e3m1/e4m2_rne use round_up = guard & (sticky | lsb). FP8 keeps ties-away
    // but goes through BF16ToE4M3, not roundToMx, so it is unaffected by this.
    roundAnyRawFNToRecFN.io.roundingMode  := consts.round_near_even
    roundAnyRawFNToRecFN.io.detectTininess:= consts.tininess_afterRounding

    val rec_format = roundAnyRawFNToRecFN.io.out
    val ieee_format = format.ieee(rec_format)
    out := pack_function(ieee_format)
    out
  }
}

class BF16ScaleRoundToTiny(
  val outputnumLanes: Int = 4,
  val inputexpWidth: Int = 8,  // BF16 exp
  val inputsigWidth: Int = 8,  // we treat 7 frac bits + 1 pad
  val e5m2Lut: Boolean = false, // when true, the LUT-format slot (dataType 1) is FP8 E5M2, not FP6
) extends Module {
    
  val io = IO(new Bundle {
    val in_bf16      = Input(Vec(outputnumLanes, UInt(16.W)))
    val scale_e8m0   = Input(UInt(inputexpWidth.W))
    val dataType   = Input(UInt(2.W))
    val mx_fp8_altfmt = Input(Bool())   // dataType 1: 1 = E5M2 cast, 0 = FP6 (only meaningful when e5m2Lut)
    // The reference divides the block by its own max, so a non-finite max poisons the block:
    // /nan sends every element to NaN, /inf sends the finite ones to zero. This datapath
    // multiplies by a finite power of two, so it has to select that behaviour explicitly.
    val block_has_nan = Input(Bool())
    val block_has_inf = Input(Bool())
    val out      = Output(Vec(outputnumLanes, UInt(8.W)))
  })

  val data_buffer = WireInit(VecInit(Seq.fill(outputnumLanes)(0.U(16.W))))
  data_buffer := io.in_bf16
  dontTouch(data_buffer)
  val quantized_buffer =  WireInit(VecInit(Seq.fill(outputnumLanes)(0.U(8.W))))
  io.out := quantized_buffer
  dontTouch(quantized_buffer)
  //val scale_exp_unbiased = io.scale_e8m0
  val scale_exp_unbiased = io.scale_e8m0 - 127.U 
  val maxExp             = ((1 << (inputexpWidth)) - 2).U(inputexpWidth.W) // e.g. 0xFE for BF16

  for (i <- 0 until outputnumLanes) {
    val input_value = data_buffer(i)

    // BF16: [15]=sign, [14:7]=exp(8), [6:0]=frac(7)
    val sign      = input_value(15)
    val input_exp = input_value(14, 7)
    val input_sig = input_value(6, 0)

    val isNaN = input_exp.andR && input_sig.orR
    val isInf = input_exp.andR && !input_sig.orR

    val scale = Mux(scale_exp_unbiased(inputexpWidth-1), (~(scale_exp_unbiased - 1.U)), scale_exp_unbiased)
    val summed_u = Mux(scale_exp_unbiased(inputexpWidth-1), input_exp - scale, input_exp + scale)
    val underflow = scale_exp_unbiased(inputexpWidth-1) && (input_exp < scale)
    val overflow  = (!scale_exp_unbiased(inputexpWidth-1) && ((summed_u < input_exp) || (summed_u >= maxExp)))

    val scaled_exp = Wire(UInt(inputexpWidth.W))
    val sig = Wire(UInt((inputsigWidth - 1).W))
    sig := input_sig

    when (isNaN) {
      scaled_exp := ((1 << (inputexpWidth)) - 1).U(inputexpWidth.W)
    } .elsewhen (isInf) {
      scaled_exp := ((1 << (inputexpWidth)) - 1).U(inputexpWidth.W)
      sig := 0.U((inputsigWidth - 1).W)
    } .elsewhen (input_exp === 0.U) {
      // Zero or a BF16 subnormal. The exponent-only scaling below would treat it as 1.sig and
      // hand back a nonzero value -- an exact zero came out as 2^-9*scale once the block max fell
      // below ~2^-110. Flush, which is also what the reference does (its eps-floored scale sends
      // any BF16 subnormal to zero).
      scaled_exp := 0.U
      sig := 0.U((inputsigWidth - 1).W)
    } .elsewhen (underflow) {
      scaled_exp := 0.U
    } .elsewhen (overflow) {
      scaled_exp := maxExp
    } .otherwise {
      scaled_exp := summed_u
    }
    //dontTouch(scaled_exp)
    val format_fp4 = MxFType.E3M1
    val format_fp6 = MxFType.E4M2
    val format_fp8 = MxFType.E5M3

    val scaled_bf16 = Cat(sign, scaled_exp, sig)

    val dbg_input_value = WireDefault(input_value);      dontTouch(dbg_input_value)
    val dbg_input_exp   = WireDefault(input_exp);        dontTouch(dbg_input_exp)
    val dbg_input_sig   = WireDefault(input_sig);        dontTouch(dbg_input_sig)
    val dbg_scale       = WireDefault(scale);            dontTouch(dbg_scale)
    val dbg_summed_u    = WireDefault(summed_u);         dontTouch(dbg_summed_u)
    val dbg_underflow   = WireDefault(underflow);        dontTouch(dbg_underflow)
    val dbg_overflow    = WireDefault(overflow);         dontTouch(dbg_overflow)
    val dbg_scaled_exp  = WireDefault(scaled_exp);       dontTouch(dbg_scaled_exp)
    val dbg_scaled_bf16 = WireDefault(scaled_bf16);      dontTouch(dbg_scaled_bf16)

    // Runtime-symmetric requant element encode, selected by (dataType code, altfmt). All encoders are
    // cheap combinational objects, always instantiated, so ONE build can requant to any sub-format:
    //   code0/fp8: altfmt0 -> E4M3 (8-bit code), altfmt1 -> E5M2 (8-bit code)
    //   code1/fp6: altfmt0 -> E3M2 (6-bit code), altfmt1 -> E2M3 (6-bit code)
    //   code2/fp4: E2M1 (4-bit)
    // The 6-bit fp6 codes and 8-bit E5M2/E4M3 codes feed the packing/LUT-projection downstream.
    // code1/fp6: altfmt0 -> E3M2 (straight-grid RNE), altfmt1 -> E2M3. Both round directly to their
    // grid now (the old E3M2 BF16->E4M2->E4M2ToFp6 double-rounding path is gone).
    val fp6_out = Mux(io.mx_fp8_altfmt, BF16ToE2M3(scaled_bf16), BF16ToE3M2(scaled_bf16))
    val fp8_out = Mux(io.mx_fp8_altfmt, BF16ToE5M2(scaled_bf16), BF16ToE4M3(scaled_bf16))
    val rounded = Mux(io.dataType === 1.U,
                      fp6_out,
                      Mux(io.dataType === 0.U,
                        fp8_out,
                        roundToMx(scaled_bf16, inputexpWidth, inputsigWidth, format_fp4, (in: UInt) => E3M1Tofp4(in))
                      )
                    )

    // Block poisoned by a non-finite max (see the io comment). NaN wins over Inf, matching the
    // reference's `X = nan` taking precedence over `X = inf`.
    val poisoned = Mux(io.block_has_nan, 0x7F.U(8.W),
                   Mux(isNaN || isInf, 0x7F.U(8.W), Cat(sign, 0.U(7.W))))

    val dbg_rounded     = WireDefault(rounded);          dontTouch(dbg_rounded)
    quantized_buffer(i) := Mux(io.block_has_nan || io.block_has_inf, poisoned, rounded)
  }
}

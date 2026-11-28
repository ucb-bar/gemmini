package gemmini

import chisel3._
import hardfloat._
import freechips.rocketchip.tile._
import chisel3.util._


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


// BF16 -> E4M3 code in one RNE step, subnormals kept, saturating to +-448 (microxcaling even/saturate/allow_denorm)
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

    // subnormal target (e8 < 121): k = RNE(sig8 / 2^(125 - e8)); shifts >= 9 flush to 0
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

// BF16 -> E5M2 code (bias 15): RNE, overflow -> 0x7B, Inf/NaN kept, subnormals to 2^-16 (mx_fp_math.h port)
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

// BF16 -> FP6 E2M3 code (bias 1, max 7.5): RNE, subnormals kept; code in the low 6 bits of an 8-bit lane
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

    // subnormal (e < 0): k = RNE(sig8 / 2^(4-e)); e <= -5 flushes to 0 (also keeps the stepU shift in range)
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

// FP6 code (low 6 bits) -> E4M3 byte, exact; alt 1 = E2M3
object Fp6ToE4M3 {
  def apply(c: UInt, alt: Bool): UInt = {
    val s = c(5)
    val e32 = c(4, 2); val m32 = c(1, 0)
    val e3m2 = Mux(e32 =/= 0.U, Cat(e32 +& 4.U, m32, 0.U(1.W)),
      MuxLookup(m32, 0.U(7.W))(Seq(1.U -> "b0011000".U, 2.U -> "b0100000".U, 3.U -> "b0100100".U)))
    val e23 = c(4, 3); val m23 = c(2, 0)
    val e2m3 = Mux(e23 =/= 0.U, Cat(e23 +& 6.U, m23),
      MuxLookup(m23, 0.U(7.W))(Seq(1.U -> "b0100000".U, 2.U -> "b0101000".U, 3.U -> "b0101100".U,
        4.U -> "b0110000".U, 5.U -> "b0110010".U, 6.U -> "b0110100".U, 7.U -> "b0110110".U)))
    Cat(s, Mux(alt, e2m3, e3m2)(6, 0))
  }
}

// BF16 -> FP6 E3M2 code (bias 3, max 28): RNE straight to the E3M2 grid, subnormals kept (quantum 2^-4)
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

    // subnormal (e < -2): k = RNE(sig8 / 2^(3-e)); shSub >= 9 flushes to 0
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
    // FP4/FP6 round ties to even (RNE), matching Spike's bf16_bits_to_e3m1/e4m2_rne
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
    // a non-finite block max poisons the block like the reference's divide: NaN -> all NaN, Inf -> finite ones to 0
    val block_has_nan = Input(Bool())
    val block_has_inf = Input(Bool())
    val out      = Output(Vec(outputnumLanes, UInt(8.W)))
  })

  val data_buffer = WireInit(VecInit(Seq.fill(outputnumLanes)(0.U(16.W))))
  data_buffer := io.in_bf16
  val quantized_buffer =  WireInit(VecInit(Seq.fill(outputnumLanes)(0.U(8.W))))
  io.out := quantized_buffer
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
      // zero or BF16 subnormal: flush to 0 (the exponent-only scaling would treat it as 1.sig), as the reference does
      scaled_exp := 0.U
      sig := 0.U((inputsigWidth - 1).W)
    } .elsewhen (underflow) {
      scaled_exp := 0.U
    } .elsewhen (overflow) {
      scaled_exp := maxExp
    } .otherwise {
      scaled_exp := summed_u
    }
    val format_fp4 = MxFType.E3M1

    val scaled_bf16 = Cat(sign, scaled_exp, sig)


    // requant element encode by (code, altfmt): fp8 E4M3/E5M2, fp6 E3M2/E2M3, fp4 E2M1; all built, RNE to each grid
    val fp6_out = Mux(io.mx_fp8_altfmt, BF16ToE2M3(scaled_bf16), BF16ToE3M2(scaled_bf16))
    val fp8_out = Mux(io.mx_fp8_altfmt, BF16ToE5M2(scaled_bf16), BF16ToE4M3(scaled_bf16))
    val rounded = Mux(io.dataType === 1.U,
                      fp6_out,
                      Mux(io.dataType === 0.U,
                        fp8_out,
                        roundToMx(scaled_bf16, inputexpWidth, inputsigWidth, format_fp4, (in: UInt) => E3M1Tofp4(in))
                      )
                    )

    // block poisoned by a non-finite max: NaN wins over Inf, as in the reference
    val poisoned = Mux(io.block_has_nan, 0x7F.U(8.W),
                   Mux(isNaN || isInf, 0x7F.U(8.W), Cat(sign, 0.U(7.W))))

    quantized_buffer(i) := Mux(io.block_has_nan || io.block_has_inf, poisoned, rounded)
  }
}

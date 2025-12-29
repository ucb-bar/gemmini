package gemmini

import chisel3._
import hardfloat._
import freechips.rocketchip.util._
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

class BF16ScaleRoundToFP6(
  outputnumLanes: Int = 6
) extends BF16ScaleRoundToTiny(
  tinyWidth      = 6,
  outputnumLanes = outputnumLanes,
  format         = MxFType.E4M2,
  pack           = (in: UInt) => E4M2ToFp6(in)
)

class BF16ScaleRoundToFP4(
  outputnumLanes: Int = 6
) extends BF16ScaleRoundToTiny(
  tinyWidth      = 4,
  outputnumLanes = outputnumLanes,
  format         = MxFType.E3M1,
  pack           = (in: UInt) => E3M1Tofp4(in)
)

class BF16ScaleRoundToFP8(
  outputnumLanes: Int = 6
) extends BF16ScaleRoundToTiny(
  tinyWidth      = 8,
  outputnumLanes = outputnumLanes,
  format         = MxFType.E5M3,
  pack           = (in: UInt) => E5M3ToFp8(in)
)

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
    
    val mapToZero = (exp < 2.U) || isZero
    val mapToSubnorm = (exp === 2.U) && (sig === 0.U)
    val mapToMinNorm = (exp === 2.U) && (sig === 1.U)

    val mapToMax = (exp > (biasDiff +& 3.U)) || isSpecial

    val exp_adj = (exp - biasDiff)(1, 0)
    val outNorm = sign ## exp_adj ## sig
    val outMinNorm = sign ## "b010".U(3.W)

    Mux(mapToZero,
      0.U(4.W),
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
      out := 0.U(6.W)
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
    val mapToMax = (exp > (23.U)) || isSpecial

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

    Mux(mapToZero, 0.U(8.W), 
      Mux(mapToMax, sign ## FP8Max, 
        Mux(mapToSubnorm, outSub, outNorm)))
  }
}

class BF16ScaleRoundToTiny(
  val tinyWidth:     Int,
  val outputnumLanes: Int = 4,
  val inputexpWidth: Int = 8,  // BF16 exp
  val inputsigWidth: Int = 8,  // we treat 7 frac bits + 1 pad
  val format:        FType,
  val pack:          UInt => UInt // ieee(<== format.ieee) => tiny format (4/6/8 bits)
) extends Module {
    
  val io = IO(new Bundle {
    val in_bf16      = Input(Vec(outputnumLanes, UInt(16.W)))
    val scale_e8m0   = Input(UInt(inputexpWidth.W))
    val out_fp6      = Output(Vec(outputnumLanes, UInt(8.W)))
  })

  val data_buffer = RegInit(VecInit(Seq.fill(outputnumLanes)(0.U(16.W))))
  data_buffer := io.in_bf16

  val quantized_buffer =  WireInit(VecInit(Seq.fill(outputnumLanes)(0.U(8.W))))
  io.out_fp6 := quantized_buffer

  val scale_exp_unbiased = io.scale_e8m0
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
    } .elsewhen (underflow) {
      scaled_exp := 0.U
    } .elsewhen (overflow) {
      scaled_exp := maxExp
    } .otherwise {
      scaled_exp := summed_u
    }

    val scaled_bf16 = Cat(sign, scaled_exp, sig)
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
    roundAnyRawFNToRecFN.io.roundingMode  := consts.round_near_even
    roundAnyRawFNToRecFN.io.detectTininess:= consts.tininess_afterRounding

    val rec_format = roundAnyRawFNToRecFN.io.out          // recoded format
    val ieee_format = format.ieee(rec_format)

    quantized_buffer(i) := pack(ieee_format)
  }
}

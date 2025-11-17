package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import hardfloat._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import circt.stage.ChiselStage
import org.scalatest.matchers.should.Matchers
import scala.util.Random
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
  format         = FType.E4M3,
  pack           = (in: UInt) => E4M3ToFp6(in)
)

class BF16ScaleRoundToFP4(
  outputnumLanes: Int = 6
) extends BF16ScaleRoundToTiny(
  tinyWidth      = 4,
  outputnumLanes = outputnumLanes,
  format         = FType.E3M1,
  pack           = (in: UInt) => E3M1Tofp4(in)
)

class BF16ScaleRoundToFP8(
  outputnumLanes: Int = 6
) extends BF16ScaleRoundToTiny(
  tinyWidth      = 8,
  outputnumLanes = outputnumLanes,
  format         = FType.E5M3,
  pack           = (in: UInt) => E5M3ToFp8(in)
)

object E3M1Tofp4 {
  def apply(in: UInt): UInt = {
    val sign      = in(4)
    val exp       = in(3, 1)
    val sig       = in(0)
    val bias_diff = 2.U

    val adjExp = Mux(exp < bias_diff, 0.U, exp - bias_diff)

    Mux(adjExp(2), "b0111".U(4.W), sign ## adjExp(1, 0) ## sig)
  }
}

object E5M3ToFp8 {
  def apply(in: UInt): UInt = {
    val sign      = in(8)
    val exp       = in(7, 3)
    val sig       = in(2, 0)
    val bias_diff = 8.U

    val adjExp = Mux(exp < bias_diff, 0.U, exp - bias_diff)

    Mux(adjExp(4), "b01111111".U(8.W), sign ## adjExp(3, 0) ## sig)
  }
}


object E4M3ToFp6 {
  def apply(in: UInt): UInt = {
    require(in.getWidth == 8)

    val sign  = in(7)
    val exp   = in(6, 3) // 4-bit exponent (E4)
    val sig   = in(2, 0) // 3-bit fraction

    val biasDiff = 4.U(4.W) 

    val fullSig = Cat(1.U(1.W), sig)

    val underflow = exp <= biasDiff

    val expMinusBias = exp - biasDiff 
    val adjExp       = Mux(underflow, 0.U, expMinusBias)

    // Overflow in FP6: adjExp >= 8 -> adjExp(3) == 1
    val overflow = adjExp(3)

    // Subnormal handling for underflow
    val shift = (biasDiff - exp + 1.U)(1, 0) 

    val shifted   = (fullSig >> shift)
    val subSig2   = shifted(2, 1)
    val subExp3   = 0.U(3.W)

    // Normal mapping (no underflow)
    val normSig2 = sig(2, 1)
    val normExp3 = adjExp(2, 0)

    val isZeroInput   = (exp === 0.U) && (sig === 0.U)
    val useSubnormal  = underflow && !isZeroInput

    val outExp3 = Mux(useSubnormal, subExp3, normExp3)
    val outSig2 = Mux(useSubnormal, subSig2, normSig2)

    Mux(overflow,
      "b011111".U(6.W),
      sign ## outExp3 ## outSig2
    )
  }
}

class BF16ScaleRoundToTiny(
  val tinyWidth:     Int,
  val outputnumLanes: Int,
  val inputexpWidth: Int = 8,  // BF16 exp
  val inputsigWidth: Int = 8,  // we treat 7 frac bits + 1 pad
  val format:        FType,
  val pack:          UInt => UInt // ieee(<== format.ieee) => tiny format (4/6/8 bits)
) extends Module {
    
    val io = IO(new Bundle {
    val in_bf16      = Input(Vec(outputnumLanes, UInt(16.W)))
    val scale_e8m0   = Input(UInt(inputexpWidth.W))
    val out_fp6      = Output(Vec(outputnumLanes, UInt(6.W)))
  })

  val data_buffer = Reg(Vec(outputnumLanes, UInt(16.W)))
  data_buffer := io.in_bf16

  val quantized_buffer = Wire(Vec(outputnumLanes, UInt(6.W)))
  io.out_fp6 := quantized_buffer

  val scale_exp_unbiased = io.scale_e8m0
  val maxExp             = ((1 << (inputexpWidth)) - 1).U(inputexpWidth.W) // e.g. 0x7F for BF16

  for (i <- 0 until outputnumLanes) {
    val input_value = data_buffer(i)

    // BF16: [15]=sign, [14:7]=exp(8), [6:0]=frac(7)
    val sign      = input_value(15)
    val input_exp = input_value(14, 7)
    val input_sig = input_value(6, 0)

    val scale = Mux(scale_exp_unbiased(inputexpWidth-1), (~(scale_exp_unbiased - 1.U)), scale_exp_unbiased)
    val summed_u = Mux(scale_exp_unbiased(inputexpWidth-1), input_exp - scale, input_exp + scale)
    val underflow = scale_exp_unbiased(inputexpWidth-1) && (input_exp < scale)
    val overflow  = !scale_exp_unbiased(inputexpWidth-1) && (summed_u < input_exp)

    val scaled_exp = Wire(UInt(inputexpWidth.W))
    when (underflow) {
      scaled_exp := 0.U
    } .elsewhen (overflow) {
      scaled_exp := maxExp
    } .otherwise {
      scaled_exp := summed_u
    }

    val scaled_bf16 = Cat(sign, scaled_exp, input_sig)
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

    val rec_e4m3 = roundAnyRawFNToRecFN.io.out          // recoded E4M3
    val ieee_e4m3 = format.ieee(rec_e4m3)

    quantized_buffer(i) := E4M3ToFp6(ieee_e4m3)
  }
}

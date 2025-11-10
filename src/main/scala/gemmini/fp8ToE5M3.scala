package gemmini.hardfloatHelper

import chisel3._
import chisel3.util._
import hardfloat._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
class MxMulAddRecFNToRaw_preMul(expWidth: Int, sigWidth: Int) extends RawModule
{
    override def desiredName = s"MxMulAddRecFNToRaw_preMul_e${expWidth}_s${sigWidth}"
    val io = IO(new Bundle {
        val a = Input(new RawFloat(expWidth, sigWidth))
        val c = Input(Bits((expWidth + sigWidth + 1).W))
        val mulAddA = Output(UInt(sigWidth.W))
        val mulAddC = Output(UInt((sigWidth * 2).W))
        val toPostMul = Output(new MulAddRecFN_interIo(expWidth, sigWidth))
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
//*** POSSIBLE TO REDUCE THIS BY 1 OR 2 BITS?  (CURRENTLY 2 BITS BETWEEN
//***  UNSHIFTED C AND PRODUCT):
    val sigSumWidth = sigWidth * 3 + 3

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val rawA = io.a
    val rawC = rawFloatFromRecFN(expWidth, sigWidth, io.c)

    val signProd = rawA.sign
//*** REVIEW THE BIAS FOR 'sExpAlignedProd':
    val sExpAlignedProd =
        rawA.sExp +& (BigInt(1)<<expWidth).S + (-(BigInt(1)<<expWidth) + sigWidth + 3).S

    val doSubMags = signProd ^ rawC.sign

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val sNatCAlignDist = sExpAlignedProd - rawC.sExp
    val posNatCAlignDist = sNatCAlignDist(expWidth + 1, 0)
    val isMinCAlign = rawA.isZero || (sNatCAlignDist < 0.S)
    val CIsDominant =
        ! rawC.isZero && (isMinCAlign || (posNatCAlignDist <= sigWidth.U))
    val CAlignDist =
        Mux(isMinCAlign,
            0.U,
            Mux(posNatCAlignDist < (sigSumWidth - 1).U,
                posNatCAlignDist(log2Ceil(sigSumWidth) - 1, 0),
                (sigSumWidth - 1).U
            )
        )
    val mainAlignedSigC =
        (Mux(doSubMags, ~rawC.sig, rawC.sig) ## Fill(sigSumWidth - sigWidth + 2, doSubMags)).asSInt>>CAlignDist
    val reduced4CExtra =
        (orReduceBy4(rawC.sig<<((sigSumWidth - sigWidth - 1) & 3)) &
             lowMask(
                 CAlignDist>>2,
//*** NOT NEEDED?:
//                 (sigSumWidth + 2)>>2,
                 (sigSumWidth - 1)>>2,
                 (sigSumWidth - sigWidth - 1)>>2
             )
        ).orR
    val alignedSigC =
        Cat(mainAlignedSigC>>3,
            Mux(doSubMags,
                mainAlignedSigC(2, 0).andR && ! reduced4CExtra,
                mainAlignedSigC(2, 0).orR  ||   reduced4CExtra
            )
        )

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    io.mulAddA := rawA.sig
    io.mulAddC := alignedSigC(sigWidth * 2, 1)

    io.toPostMul.isSigNaNAny := isSigNaNRawFloat(rawA) || isSigNaNRawFloat(rawC)
    io.toPostMul.isNaNAOrB := rawA.isNaN
    io.toPostMul.isInfA    := rawA.isInf
    io.toPostMul.isZeroA   := rawA.isZero
    io.toPostMul.isInfB    := false.B
    io.toPostMul.isZeroB   := false.B
    io.toPostMul.signProd  := signProd
    io.toPostMul.isNaNC    := rawC.isNaN
    io.toPostMul.isInfC    := rawC.isInf
    io.toPostMul.isZeroC   := rawC.isZero
    io.toPostMul.sExpSum   :=
        Mux(CIsDominant, rawC.sExp, sExpAlignedProd - sigWidth.S)
    io.toPostMul.doSubMags := doSubMags
    io.toPostMul.CIsDominant := CIsDominant
    io.toPostMul.CDom_CAlignDist := CAlignDist(log2Ceil(sigWidth + 1) - 1, 0)
    io.toPostMul.highAlignedSigC :=
        alignedSigC(sigSumWidth - 1, sigWidth * 2 + 1)
    io.toPostMul.bit0AlignedSigC := alignedSigC(0)
}

class MxMulAddRecFN(expWidth: Int, sigWidth: Int) extends RawModule
{
    override def desiredName = s"MxMulAddRecFN_e${expWidth}_s${sigWidth}"
    val io = IO(new Bundle {
        val a = Input(new RawFloat(expWidth, sigWidth))
        val c = Input(Bits((expWidth + sigWidth + 1).W))
        val roundingMode   = Input(UInt(3.W))
        val detectTininess = Input(UInt(1.W))
        val out = Output(Bits((expWidth + sigWidth + 1).W))
        val exceptionFlags = Output(Bits(5.W))
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val mulAddRecFNToRaw_preMul =
        Module(new MxMulAddRecFNToRaw_preMul(expWidth, sigWidth))
    val mulAddRecFNToRaw_postMul =
        Module(new MulAddRecFNToRaw_postMul(expWidth, sigWidth))
	
    mulAddRecFNToRaw_preMul.io.a  := io.a
    mulAddRecFNToRaw_preMul.io.c  := io.c

	val mulAddResult =  (mulAddRecFNToRaw_preMul.io.mulAddA << (sigWidth -1)) +& mulAddRecFNToRaw_preMul.io.mulAddC

    mulAddRecFNToRaw_postMul.io.fromPreMul := mulAddRecFNToRaw_preMul.io.toPostMul
    mulAddRecFNToRaw_postMul.io.mulAddResult := mulAddResult
    mulAddRecFNToRaw_postMul.io.roundingMode := io.roundingMode

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------
    val roundRawFNToRecFN =
        Module(new RoundRawFNToRecFN(expWidth, sigWidth, 0))
    roundRawFNToRecFN.io.invalidExc   := mulAddRecFNToRaw_postMul.io.invalidExc
    roundRawFNToRecFN.io.infiniteExc  := false.B
    roundRawFNToRecFN.io.in           := mulAddRecFNToRaw_postMul.io.rawOut
    roundRawFNToRecFN.io.roundingMode := io.roundingMode
    roundRawFNToRecFN.io.detectTininess := io.detectTininess
    io.out            := roundRawFNToRecFN.io.out
    io.exceptionFlags := roundRawFNToRecFN.io.exceptionFlags
}



object fp8ToE5M3 {

	def apply(in: Bits, altfmt: Bool) = {
		Mux(altfmt, { // E5M2
			in ## 0.U(1.W)
		}, { // E4M3
			val sign = in(7)
			val exp = in(6, 3)
			val sig = in(2, 0)
			val isNaN = exp.andR && sig.andR
			val isSubnormal = !exp.orR
			val subnormShift = PriorityEncoder(Reverse(sig))
			val subnormSig = ((sig << 1.U) << subnormShift)(2, 0)
			val subnormExp = 8.U(5.W) - subnormShift
			Mux(isNaN,
				sign ## "b11111100".U(8.W), // NaN
				Mux(isSubnormal,
					Mux(sig.orR,
						sign ## subnormExp ## subnormSig, // Subnormal
						sign ## "b00000000".U(8.W) // Zero
					),
					sign ## ((0.U(1.W) ## exp) + 8.U) ## sig // Normal
				)
			)
		})
	}
}

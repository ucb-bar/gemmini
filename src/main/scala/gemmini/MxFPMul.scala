package gemmini 

import chisel3._
import chisel3.util._
import hardfloat._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import mxHardware._ 

object E8M7Helper {
  val exp = 8
  val sig = 8  // 7 fraction bits + 1 hidden bit
  
  def recode(value: UInt): UInt = {
    hardfloat.recFNFromFN(exp, sig, value)
  }
}

class MxFpMul (supportedTypes: TypeSupport, lut: Boolean) extends Module {
  val inAWidth = if (supportedTypes.actSupportFp8) 16 else if (supportedTypes.actSupportFp6) 12 else 8
  val inBWidth = if (supportedTypes.actSupportFp8) 32 else if (supportedTypes.weiSupportFp6) 24 else if (supportedTypes.weiSupportFp4) 16 else 8
  val outType = MxFormats(8, 8) // E8M8

  val io = IO(new Bundle {
    val in_activation = Input(UInt(inAWidth.W))
    val in_a_type = Input(UInt(2.W))
    val a_altfmt = Input(Bool())
    val in_weights = Input(UInt(inBWidth.W))
    val in_w_type = Input(UInt(2.W))
    val w_altfmt = Input(Bool())
    val enable = Input(Bool())
    val rec_c = Input(UInt((outType.exp + outType.sig).W))
    val out = Output(UInt((4*(outType.exp + outType.sig + 1)).W))
  })

  def normalize(prod: UInt, outBits: Int, inBits: Int): (UInt, UInt, Bool) = {
    if (inBits > outBits) {
      val isZero = prod === 0.U
      val isPositiveShift = prod(inBits-1)
      val leftShift = Mux(isPositiveShift, 0.U, PriorityEncoder(prod.asBools.reverse))
      val expAdj = Mux(isPositiveShift, 1.U, leftShift -& 1.U)
      val aligned = prod << leftShift
      (Mux(isZero, 0.U(outBits.W), aligned(outBits-1, 0)), Mux(isZero, 0.U, expAdj), isPositiveShift)
    } else {
      val isZero = prod === 0.U
      val extraPad = outBits - inBits
      val realProd = prod(inBits - 1, 0)
      val isPositiveShift = prod(inBits-1)
      val leftShift = Mux(isPositiveShift, 1.U, PriorityEncoder(realProd.asBools.reverse) +& 1.U)
      val expAdj = Mux(isPositiveShift, 1.U, leftShift - 2.U)
      val aligned = realProd << leftShift
      ((Mux(isZero, 0.U(outBits.W), aligned(inBits-1, 0) << (extraPad))(outBits-1, 0)), Mux(isZero, 0.U, expAdj), isPositiveShift)

      }
    }

  def pack(c : MxClassifiedFp, w_exp: Int, w_sig: Int): (SInt, UInt) = {
    val packed_sig = Mux(c.isZero, 0.U, (~c.isSub.asUInt ## c.sig))
    val packed_exp = Mux(c.isZero, 0.S, c.exp.asSInt)
    (packed_exp.pad(w_exp), packed_sig.pad(w_sig))
  }

 val type_a = WireDefault(MxTypes.apply(io.in_a_type, io.a_altfmt))
 val type_w = WireDefault(MxTypes.apply(io.in_w_type, io.w_altfmt))

  val lanes2_a = io.in_activation.asTypeOf(Vec(2, UInt((inAWidth/2).W)))
  val lanes1_a = io.in_activation.asTypeOf(Vec(1, UInt((inAWidth).W))) 
  val lanes4_w = io.in_weights.asTypeOf(Vec(4, UInt((inBWidth/4).W)))
  val lanes1_w = io.in_weights.asTypeOf(Vec(1, UInt((inBWidth).W)))

  // Classify + Pack inputs
  val in_a_w2 = Wire(Vec(2, UInt((supportedTypes.mxparameters.inPE_act_totalWidth/2).W)))
  val in_exp_a_w2 = Wire(Vec(2, SInt((inAWidth/2 - supportedTypes.mxparameters.inPE_act_totalWidth/2).W)))
  val in_a_w2_classified = Wire(Vec(2, new MxClassifiedFp(MxFormats.fp4)))

  if (supportedTypes.actSupportFp4) { 
    val in_a_w2_cl = lanes2_a.map { f => classify(MxFormats.fp4, f(3, 0)) }

    val pairs_a_w2 = in_a_w2_cl.map { f => 
      pack(f, inAWidth/2 - supportedTypes.mxparameters.inPE_act_totalWidth/2, supportedTypes.mxparameters.inPE_act_totalWidth / 2) 
    }
    val (exps_a_w2, sigs_a_w2) = pairs_a_w2.unzip
    in_a_w2_classified := VecInit(in_a_w2_cl)
    in_a_w2 := VecInit(sigs_a_w2)
    in_exp_a_w2 := VecInit(exps_a_w2)
  }

  val in_a_w3 = Wire(Vec(2, UInt((supportedTypes.mxparameters.inPE_act_totalWidth/2).W)))
  val in_exp_a_w3 = Wire(Vec(2, SInt((inAWidth/2 - supportedTypes.mxparameters.inPE_act_totalWidth/2).W)))
  val in_a_w3_classified_f6 = Wire(Vec(2, new MxClassifiedFp(MxFormats.fp6_1)))
  val in_a_w3_classified_f8 = Wire(Vec(2, new MxClassifiedFp(MxFormats.fp8_1)))


  if (supportedTypes.actSupportFp8 || supportedTypes.actSupportFp6) { 
    val in_a_w3_cl_fp6 = lanes2_a.map { f => classify(MxFormats.fp6_1, f(5, 0)) }
    val in_a_w3_cl_fp8 = lanes2_a.map { f => classify(MxFormats.fp8_1, f(7, 0)) }
    val pairs_a_w3_fp6_1 = in_a_w3_cl_fp6.map { f => 
      pack(f, inAWidth/2 - supportedTypes.mxparameters.inPE_act_totalWidth/2, supportedTypes.mxparameters.inPE_act_totalWidth/2)
    }
    val pairs_a_w3_fp8_1 = in_a_w3_cl_fp8.map { f => 
      pack(f, inAWidth/2 - supportedTypes.mxparameters.inPE_act_totalWidth/2, supportedTypes.mxparameters.inPE_act_totalWidth/2) 
    }

    in_exp_a_w3 := Mux(type_a.exp === 3.U, VecInit(pairs_a_w3_fp6_1.map(_._1)), VecInit(pairs_a_w3_fp8_1.map(_._1)))
    in_a_w3 := Mux(type_a.exp === 3.U, VecInit(pairs_a_w3_fp6_1.map(_._2)), VecInit(pairs_a_w3_fp8_1.map(_._2)))
    in_a_w3_classified_f6 := VecInit(in_a_w3_cl_fp6)
    in_a_w3_classified_f8 := VecInit(in_a_w3_cl_fp8)
  }

  val in_a_w4 = Wire(Vec(1, UInt((supportedTypes.mxparameters.inPE_act_totalWidth).W)))
  val in_exp_a_w4 = Wire(Vec(1, SInt((inAWidth - supportedTypes.mxparameters.inPE_act_totalWidth).W)))
  val in_a_w4_classified_f6 = Wire(Vec(1, new MxClassifiedFp(MxFormats.fp6_0)))
  val in_a_w4_classified_f8 = Wire(Vec(1, new MxClassifiedFp(MxFormats.fp8_0)))

  if (supportedTypes.actSupportFp6 || supportedTypes.actSupportFp8) { 
    val in_a_w4_cl_fp6 = lanes1_a.map { f => classify(MxFormats.fp6_0, f(5, 0)) }
    val in_a_w4_cl_fp8 = lanes1_a.map { f => classify(MxFormats.fp8_0, f(7, 0)) }

    val pairs_a_w4_fp6_0 = in_a_w4_cl_fp6.map { f => 
      pack(f, inAWidth - supportedTypes.mxparameters.inPE_act_totalWidth, supportedTypes.mxparameters.inPE_act_totalWidth) 
    }
    val pairs_a_w4_fp8_0 = in_a_w4_cl_fp8.map { f => 
      pack(f, inAWidth - supportedTypes.mxparameters.inPE_act_totalWidth, supportedTypes.mxparameters.inPE_act_totalWidth) 
    }
    in_exp_a_w4 := Mux(type_a.exp === 2.U, VecInit(pairs_a_w4_fp6_0.map(_._1)), VecInit(pairs_a_w4_fp8_0.map(_._1)))
    in_a_w4 := Mux(type_a.exp === 2.U, VecInit(pairs_a_w4_fp6_0.map(_._2)), VecInit(pairs_a_w4_fp8_0.map(_._2)))
    in_a_w4_classified_f6 := VecInit(in_a_w4_cl_fp6)
    in_a_w4_classified_f8 := VecInit(in_a_w4_cl_fp8)
  }

  val in_w_w2 = Wire(Vec(4, UInt((supportedTypes.mxparameters.inPE_wei_totalWidth/4).W)))
  val in_exp_w_w2 = Wire(Vec(4, SInt((inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4).W)))
  val in_w_w2_classified = Wire(Vec(4, new MxClassifiedFp(MxFormats.fp4)))

  if (supportedTypes.weiSupportFp4) {
    val in_w_w2_cl = lanes4_w.map { f => classify(MxFormats.fp4, f(3, 0)) }
    val pairs_w_w2= in_w_w2_cl.map { f => 
      pack(f, inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4, supportedTypes.mxparameters.inPE_wei_totalWidth / 4) 
    }
    val (exps_w_w2, sigs_w_w2) = pairs_w_w2.unzip
    
    in_w_w2 := VecInit(sigs_w_w2)
    in_exp_w_w2 := VecInit(exps_w_w2)
    in_w_w2_classified := VecInit(in_w_w2_cl)
  }
  
  val in_w_w3 = Wire(Vec(4, UInt((supportedTypes.mxparameters.inPE_wei_totalWidth/4).W)))
  val in_exp_w_w3 = Wire(Vec(4, SInt((inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4).W)))
  val in_w_w3_classified_f6 = Wire(Vec(4, new MxClassifiedFp(MxFormats.fp6_1)))
  val in_w_w3_classified_f8 = Wire(Vec(4, new MxClassifiedFp(MxFormats.fp8_1)))

  if (supportedTypes.weiSupportFp6 || supportedTypes.weiSupportFp8) {
    val in_w_w3_cl_fp6 = lanes4_w.map { f => classify(MxFormats.fp6_1, f(5, 0)) }
    val in_w_w3_cl_fp8 = lanes4_w.map { f => classify(MxFormats.fp8_1, f(7, 0)) }

    val pairs_w_w3_fp6_1 = in_w_w3_cl_fp6.map { f => 
      pack(f, inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4, supportedTypes.mxparameters.inPE_wei_totalWidth / 4) 
    }
    val pairs_w_w3_fp8_1 = in_w_w3_cl_fp8.map { f => 
      pack(f, inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4, supportedTypes.mxparameters.inPE_wei_totalWidth / 4) 
    }
    in_exp_w_w3 := Mux(type_w.exp === 3.U, VecInit(pairs_w_w3_fp6_1.map(_._1)), VecInit(pairs_w_w3_fp8_1.map(_._1)))
    in_w_w3 := Mux(type_w.exp === 3.U, VecInit(pairs_w_w3_fp6_1.map(_._2)), VecInit(pairs_w_w3_fp8_1.map(_._2)))
    in_w_w3_classified_f6 := VecInit(in_w_w3_cl_fp6)
    in_w_w3_classified_f8 := VecInit(in_w_w3_cl_fp8)
    // printf(p"in_w_w3_classified_f8(0).isZero: ${in_w_w3_classified_f8(0).isZero}, in_w_w3_classified_f8(1).isZero: ${in_w_w3_classified_f8(1).isZero}, in_w_w3_classified_f8(2).isZero: ${in_w_w3_classified_f8(2).isZero}, in_w_w3_classified_f8(3).isZero: ${in_w_w3_classified_f8(3).isZero}\n")
  }

  val in_w_w4 = Wire(Vec(1, UInt((supportedTypes.mxparameters.inPE_wei_totalWidth).W)))
  val in_exp_w_w4 = Wire(Vec(1, SInt((inBWidth - supportedTypes.mxparameters.inPE_wei_totalWidth).W)))
  val in_w_w4_classified_f6 = Wire(Vec(1, new MxClassifiedFp(MxFormats.fp6_0)))
  val in_w_w4_classified_f8 = Wire(Vec(1, new MxClassifiedFp(MxFormats.fp8_0)))

  if (supportedTypes.weiSupportFp8 || supportedTypes.weiSupportFp6) {
    val in_w_w4_cl_fp6 = lanes1_w.map { f => classify(MxFormats.fp6_0, f(5, 0)) }
    val in_w_w4_cl_fp8 = lanes1_w.map { f => classify(MxFormats.fp8_0, f(7, 0)) }

    val pairs_w_w4_fp6_0 = in_w_w4_cl_fp6.map { f => 
      pack(f, inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4, supportedTypes.mxparameters.inPE_wei_totalWidth / 4) 
    }
    val pairs_w_w4_fp8_0 = in_w_w4_cl_fp8.map { f => 
      pack(f, inBWidth/4 - supportedTypes.mxparameters.inPE_wei_totalWidth/4, supportedTypes.mxparameters.inPE_wei_totalWidth / 4) 
    }
    in_exp_w_w4 := Mux(type_w.exp === 2.U, VecInit(pairs_w_w4_fp6_0.map(_._1)), VecInit(pairs_w_w4_fp8_0.map(_._1)))
    in_w_w4 := Mux(type_w.exp === 2.U, VecInit(pairs_w_w4_fp6_0.map(_._2)), VecInit(pairs_w_w4_fp8_0.map(_._2)))
    in_w_w4_classified_f6 := VecInit(in_w_w4_cl_fp6)
    in_w_w4_classified_f8 := VecInit(in_w_w4_cl_fp8)
  }

  // TODO: FIX THIS, currently just very hacky should be done with one classification at the beginning
  val in_a_w2_zero = in_a_w2_classified.map(f => f.isZero).asUInt
  val in_a_w3_zero = Mux(io.a_altfmt, in_a_w3_classified_f8.map(f => f.isZero).asUInt, in_a_w3_classified_f6.map(f => f.isZero).asUInt)
  val in_a_w4_zero = Mux(io.a_altfmt, in_a_w4_classified_f6.map(f => f.isZero).asUInt, in_a_w4_classified_f8.map(f => f.isZero).asUInt  )

  val in_a_mask = VecInit.tabulate(4) { i =>
    Mux(type_a.sig === 2.U, in_a_w2_zero(i/2), Mux(type_a.sig === 3.U, in_a_w3_zero(i/2), in_a_w4_zero(0)))
  }

  val in_w_w2_zero = in_w_w2_classified.map(f => f.isZero).asUInt
  val in_w_w3_zero = Mux(io.w_altfmt, in_w_w3_classified_f8.map(f => f.isZero).asUInt, in_w_w3_classified_f6.map(f => f.isZero).asUInt)
  val in_w_w4_zero = Mux(io.w_altfmt, in_w_w4_classified_f6.map(f => f.isZero).asUInt, in_w_w4_classified_f8.map(f => f.isZero).asUInt)

  val in_w_mask = VecInit.tabulate(4) { i =>
    Mux(type_w.sig === 2.U, in_w_w2_zero(i), Mux(type_w.sig === 3.U, in_w_w3_zero(i), in_w_w4_zero(0)))
  }

  // printf(p"in_a_mask: ${Binary(in_a_mask.asUInt)}\n")
  // printf(p"in_w_mask: ${Binary(in_w_mask.asUInt)}\n")

  // decode PE mode
  val peNeeded = requiredPEMode(type_a, type_w)

  val out_pe = Wire(UInt(supportedTypes.mxparameters.outPE_width.W))

  // printf(p"input w : ${Binary(Mux(type_w.sig === 2.U, in_w_w2.asUInt, Mux(type_w.sig === 3.U, in_w_w3.asUInt, in_w_w4.asUInt)))} \n")

  // Connect to PE
  val PE = Module(new MxPE(supportedTypes.mxparameters, lut))
  PE.io.modeDecoded := peNeeded
  PE.io.enable := io.enable
  PE.io.mask_a := ~in_a_mask.asUInt
  PE.io.mask_w := ~in_w_mask.asUInt
  PE.io.in_a := Mux(type_a.sig === 2.U, in_a_w2.asUInt, Mux(type_a.sig === 3.U, in_a_w3.asUInt, in_a_w4.asUInt))
  PE.io.in_w := Mux(type_w.sig === 2.U, in_w_w2.asUInt, Mux(type_w.sig === 3.U, in_w_w3.asUInt, in_w_w4.asUInt))
  out_pe := PE.io.output

  // printf(p"out_pe: ${Binary(out_pe)}\n")

  val out_e = Wire(UInt((outType.exp * 4).W))

  // printf(p"input exp a: ${Binary(Mux(type_a.sig === 2.U, in_exp_a_w2.asUInt, Mux(type_a.sig === 3.U, in_exp_a_w3.asUInt, in_exp_a_w4.asUInt)))} \n")
  // printf(p"input exp w: ${Binary(Mux(type_w.sig === 2.U, in_exp_w_w2.asUInt, Mux(type_w.sig === 3.U, in_exp_w_w3.asUInt, in_exp_w_w4.asUInt)))} \n")

  val expAdder = Module(new MxExp(
    inA_exp_width = inAWidth - supportedTypes.mxparameters.inPE_act_totalWidth,
    inW_exp_width = inBWidth - supportedTypes.mxparameters.inPE_wei_totalWidth,
    elemW = 5,
    outType = outType
  ))
  expAdder.io.enable := io.enable
  expAdder.io.modeDecoded := peNeeded
  expAdder.io.mask_a := ~in_a_mask.asUInt
  expAdder.io.mask_w := ~in_w_mask.asUInt
  expAdder.io.in_a := Mux(type_a.sig === 2.U, in_exp_a_w2.asUInt, Mux(type_a.sig === 3.U, in_exp_a_w3.asUInt, in_exp_a_w4.asUInt))
  expAdder.io.in_w := Mux(type_w.sig === 2.U, in_exp_w_w2.asUInt, Mux(type_w.sig === 3.U, in_exp_w_w3.asUInt, in_exp_w_w4.asUInt))
  out_e := expAdder.io.out_exp 

  // printf(p"out_e: ${Binary(out_e)}\n")

  val out4_toRec = VecInit.tabulate(4) { i =>
    val out4_toRec_norm_1 = normalize(PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/4)-1, i*supportedTypes.mxparameters.outPE_width/4), outType.sig - 1, 6)
    val out4_toRec_norm_2 = normalize(PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/4)-1, i*supportedTypes.mxparameters.outPE_width/4), outType.sig - 1, 5)
    val out4_toRec_norm_3 = normalize(PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/4)-1, i*supportedTypes.mxparameters.outPE_width/4), outType.sig - 1, 4)
    // printf(p"input to normalize: ${PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/4)-1, i*supportedTypes.mxparameters.outPE_width/4)} \n")
    // printf(p"out4_toRec_norm_3: ${Binary(out4_toRec_norm_3._1)}, ${Binary(out4_toRec_norm_3._2)}, ${Binary(out4_toRec_norm_3._3)}\n")

    val out4_rec_exp = Mux(peNeeded.actWidth === 2.U && peNeeded.weiWidth === 2.U,  out4_toRec_norm_3._2,
                          Mux(peNeeded.actWidth === 3.U && peNeeded.weiWidth === 3.U,  out4_toRec_norm_1._2, out4_toRec_norm_2._2))

    val shift_dir = Mux(peNeeded.actWidth === 2.U && peNeeded.weiWidth === 2.U,  out4_toRec_norm_3._3,
                          Mux(peNeeded.actWidth === 3.U && peNeeded.weiWidth === 3.U,  out4_toRec_norm_1._3, out4_toRec_norm_2._3))

    val out4_rec_sig = Mux(peNeeded.actWidth === 2.U && peNeeded.weiWidth === 2.U,  out4_toRec_norm_3._1,
                          Mux(peNeeded.actWidth === 3.U && peNeeded.weiWidth === 3.U,  out4_toRec_norm_1._1, out4_toRec_norm_2._1))

    // printf(p"exp: ${Binary(Mux(shift_dir === 0.U, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) -% out4_rec_exp, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) +% out4_rec_exp))}, sig: ${Binary(out4_rec_sig)}\n")
    // printf(p"actWidth: ${peNeeded.actWidth}, weiWidth: ${peNeeded.weiWidth}\n")

    MxPEOutToRaw(
      expWidth = outType.exp,
      sigWidth = outType.sig,
      sign = 0.U(1.W), // TODO: fix sign handling
      exp = Mux(shift_dir === 0.U, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) -% out4_rec_exp, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) +% out4_rec_exp),
      sig = out4_rec_sig
    )
  }

  val out2_toRec = VecInit.tabulate(2) { i =>
    val out2_toRec_norm_1 = normalize(PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/2)-1, i*supportedTypes.mxparameters.outPE_width/2), outType.sig - 1, 7)
    val out2_toRec_norm_2 = normalize(PE.io.output((i+1)*(supportedTypes.mxparameters.outPE_width/2)-1, i*supportedTypes.mxparameters.outPE_width/2), outType.sig - 1 , 6)

    val out2_toRec_exp = Mux(peNeeded.actWidth === 2.U || peNeeded.weiWidth === 2.U, out2_toRec_norm_2._2, out2_toRec_norm_1._2)

    val shift_dir = Mux(peNeeded.actWidth === 2.U || peNeeded.weiWidth === 2.U, out2_toRec_norm_2._3, out2_toRec_norm_1._3)

    val out2_toRec_sig = Mux(peNeeded.actWidth === 2.U || peNeeded.weiWidth === 2.U, out2_toRec_norm_2._1, out2_toRec_norm_1._1)

    // printf(p"exp: ${Binary(Mux(shift_dir === 0.U, expAdder.io.out_exp((i*2+1)*outType.exp-1, (i*2)*outType.exp) -% out2_toRec_exp, expAdder.io.out_exp((i*2+1)*outType.exp-1, (i*2)*outType.exp) +% out2_toRec_exp))}, sig: ${Binary(out2_toRec_sig)}\n")

    MxPEOutToRaw(
      expWidth = outType.exp,
      sigWidth = outType.sig,
      sign = 0.U(1.W), // TODO: fix sign handling
      exp = Mux(shift_dir === 0.U, expAdder.io.out_exp((i*2+1)*outType.exp-1, (i*2)*outType.exp) -% out2_toRec_exp, expAdder.io.out_exp((i*2+1)*outType.exp-1, (i*2)*outType.exp) +% out2_toRec_exp),
      sig = out2_toRec_sig
    )
  }


  val out1_toRec = VecInit.tabulate(1) { i =>
    val out1_toRec_norm = normalize(PE.io.output(7,0), outType.sig - 1, 8)
    MxPEOutToRaw(
      expWidth = outType.exp,
      sigWidth = outType.sig,
      sign = 0.U(1.W), // TODO: fix sign handling
      exp = Mux(out1_toRec_norm._3 === 0.U, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) -% out1_toRec_norm._2, expAdder.io.out_exp((i+1)*outType.exp-1, i*outType.exp) +% out1_toRec_norm._2),
      sig = out1_toRec_norm._1
    )
  }

  val addUnits = Seq.fill(4)(Module(new MulAddRecFNPipe(0, outType.exp, outType.sig)))
  val laneMask  = VecInit((0 until 4).map(i => io.enable && (i.U < peNeeded.numOutputs)))
  val outputs = Wire(Vec(4, UInt(((outType.exp + outType.sig + 1)).W)))

  for (i <- 0 until 4) {
    val rawIn = Mux(peNeeded.numOutputs === 4.U, out4_toRec(i), Mux(peNeeded.numOutputs === 2.U, out2_toRec(i/2), out1_toRec(0)))

    val recIn_a = rawIn.sign ##
          (Mux(rawIn.isZero, 0.U(3.W), rawIn.sExp(outType.exp, outType.exp - 2)) |
                Mux(rawIn.isNaN, 1.U, 0.U)) ##
            rawIn.sExp(outType.exp - 3, 0) ##
            rawIn.sig((outType.sig) - 2, 0)

    val recIn_c = io.rec_c

    addUnits(i).io.roundingMode := hardfloat.consts.round_near_even
    addUnits(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    addUnits(i).io.op := 0.U
    addUnits(i).io.validin := laneMask(i)
    addUnits(i).io.a := recIn_a
    addUnits(i).io.b := E8M7Helper.recode(Cat(0.U(1.W), ((BigInt(1) << (outType.exp-1)) - 1).U(outType.exp.W), 0.U((outType.sig-1).W))) // 1.0
    addUnits(i).io.c := recIn_c

    outputs(i) := addUnits(i).io.out
  }

  io.out := outputs.asUInt


}

object MxPEOutToRaw {
  def apply(expWidth: Int, sigWidth: Int, sign: UInt, exp: UInt, sig: UInt): RawFloat = {
    val expIn = exp(expWidth-1, 0)
    val fractIn = sig(sigWidth-2, 0)

    val isZeroExpIn = (expIn === 0.U)
    val isZeroFractIn = (fractIn === 0.U)

    val normDist = countLeadingZeros(fractIn)
    val subnormFract = (fractIn << normDist) (sigWidth - 3, 0) << 1
    val adjustedExp =
      Mux(isZeroExpIn,
        normDist ^ ((BigInt(1) << (expWidth + 1)) - 1).U,
        expIn
      ) + ((BigInt(1) << (expWidth - 1)).U
        | Mux(isZeroExpIn, 2.U, 1.U))

    val isZero = isZeroExpIn && isZeroFractIn
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN := isSpecial && !isZeroFractIn
    out.isInf := isSpecial && isZeroFractIn
    out.isZero := isZero
    out.sign := sign
    out.sExp := adjustedExp(expWidth, 0).zext
    out.sig :=
      0.U(1.W) ## !isZero ## Mux(isZeroExpIn, subnormFract, fractIn)
    out
  }
}
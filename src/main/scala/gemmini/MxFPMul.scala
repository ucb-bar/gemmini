package gemmini 

import chisel3._
import chisel3.util._
import hardfloat._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._ 

class MxFpMul (lut: Boolean) (fpProductPrecision: (Int, Int), fpAccPrecision: MxFloat) extends Module with HasMxPEParameters {
  println("Creating MxFpMul with product precision: " + fpProductPrecision + " and acc precision: " + fpAccPrecision)
  // TODO: I am definining these parameters here, but this shouldn't be like this, I will change this module to extend parameter class
  val outType1 = MxFormats(fpProductPrecision._1, fpProductPrecision._2)
  val outType2 = MxFormats(fpProductPrecision._1, fpProductPrecision._2)
  val outType4 = MxFormats(fpProductPrecision._1, fpProductPrecision._2)
  val cType = MxFormats(fpAccPrecision.expWidth, fpAccPrecision.sigWidth)
  val totalAdderWidth = 4*(outType4.exp + 1)

  val ts = TypeSupport(
    actSupportFp4 = true, actSupportFp6_1 = true, actSupportFp8_0 = true,
    weiSupportFp4 = true, weiSupportFp6_1 = true, weiSupportFp8_0 = true
  )
  
  val io = IO(new Bundle {
    val in_activation = Input(UInt(inAWidth.W))
    val type_a = Input(new MxTypes())
    val in_weights = Input(UInt(inBWidth.W))
    val type_w = Input(new MxTypes())
    val mode = Input(new mxMode())
    val enable = Input(Bool())
    val rec_c = Input(UInt((4*(cType.exp + cType.sig + 1)).W))
    val out = Output(UInt((4*(cType.exp + cType.sig + 1)).W))
    // val input_mx_format = Input(UInt(2.W)) // this inputs are not needed because mode already encodes this information
    // val weight_mx_format = Input(UInt(2.W))
  })

  def normalize(prod: UInt, outBits: Int, inBits: Int): (UInt, UInt, Bool) = {
    if (inBits > outBits) {
      val isZero = prod === 0.U
      val isPositiveShift = prod(inBits-1)
      val leftShift = Mux(isPositiveShift, 0.U, PriorityEncoder(prod.asBools.reverse))
      val expAdj = Mux(isPositiveShift, 1.U, leftShift -& 1.U)
      val aligned = prod << leftShift

      // After alignment, the hidden-1 sits at bit inBits-1.
      // The outBits fraction bits we want occupy [inBits-2 : inBits-1-outBits].
      // Everything below that window is discarded; we must round instead of truncate.
      val truncated = aligned(inBits - 2, inBits - 1 - outBits)

      // bitsBelow: how many bits fall below the fraction window (elaboration-time constant)
      val bitsBelow = inBits - 1 - outBits
      // roundBit: MSB of the discarded portion (the deciding rounding bit)
      val roundBit: Bool  = if (bitsBelow >= 1) aligned(inBits - 2 - outBits)       else false.B
      // stickyBit: OR of all bits below the round bit; non-zero means we are strictly > midpoint
      val stickyBit: Bool = if (bitsBelow >= 2) aligned(inBits - 3 - outBits, 0).orR else false.B
      // Round-to-nearest-even: increment when roundBit=1 AND (past midpoint OR at midpoint with odd LSB)
      val doRound = roundBit && (stickyBit || truncated(0))

      // +& is width-growing addition: result is (outBits+1) bits, capturing any carry-out
      val rounded       = truncated +& doRound
      // roundOverflow: set when all outBits fraction bits were 1 and the increment wraps to 0
      // In that case the normalised significand becomes 1.000…0 and the exponent gains +1
      val roundOverflow = rounded(outBits)
      val finalSig      = Mux(roundOverflow, 0.U(outBits.W), rounded(outBits - 1, 0))
      val finalExpAdj   = expAdj +& roundOverflow

      (Mux(isZero, 0.U(outBits.W), finalSig), Mux(isZero, 0.U, finalExpAdj), isPositiveShift)
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

  def pack(c : MxClassifiedFp, w_exp: Int, w_sig: Int, padExp: Int): (UInt, UInt, UInt) = {
    val packed_sig = Mux(c.isZero, 0.U, (~c.isSub.asUInt ## c.sig))
    val packed_exp = Mux(c.isZero, 0.S, c.exp.asSInt)
    if (w_exp > padExp) {
      (packed_exp.pad(padExp).asUInt, packed_sig.pad(w_sig), c.sign)
    } else {
      (packed_exp.pad(w_exp).asUInt.pad(padExp), packed_sig.pad(w_sig), c.sign)
    }
  }

  // Separate input into lanes
  val lanes2_a = io.in_activation.asTypeOf(Vec(2, UInt((inAWidth/2).W)))
  val lanes1_a = io.in_activation.asTypeOf(Vec(1, UInt((inAWidth).W))) 
  val lanes2_w = io.in_weights.asTypeOf(Vec(2, UInt((inBWidth/2).W)))
  val lanes1_w = io.in_weights.asTypeOf(Vec(1, UInt((inBWidth).W)))

  // printf(p"Inputs a: ${Binary(lanes2_a.asUInt)}, Inputs w: ${Binary(lanes2_w.asUInt)}\n")

  // Wire up elements for PE + Exp Adder
  val inA_pe   = WireDefault(0.U(peInAWidth.W))
  val inW_pe   = WireDefault(0.U(peInBWidth.W))
  val inA_exp  = WireDefault(0.U((inAWidth - peInAWidth).W))
  val inW_exp  = WireDefault(0.U((inBWidth - peInBWidth).W))
  val inA_sign = WireDefault(0.U(2.W))
  val inW_sign = WireDefault(0.U(2.W))
  val in_a_mask= WireDefault("b11".U(2.W))
  val in_w_mask= WireDefault("b11".U(2.W))
  val nanA = WireDefault(false.B)
  val nanW = WireDefault(false.B)

  // Classify input lanes
  // Get exps, sigs, and signs from the classified inputs in the right format for the PE and Exp Adder
  // Define zero inputs for masking 
  if (actSupportFp4) {
    val in_a_w2_cl = lanes2_a.map { f => classify(MxFormats.fp4, f(3, 0)) }
    val (exps_a_w2, sigs_a_w2, signs_a_w2) = in_a_w2_cl.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i*2), peInAWidth / 2, (inAWidth - peInAWidth)/2) }.unzip3
    val in_a_w2_zero = in_a_w2_cl.map(f => f.isZero)

    // printf(p"in_a_w2_zero: ${Binary(in_a_w2_zero.asUInt)}\n")

    when (io.type_a.sig === 2.U) {
      inA_pe := VecInit(sigs_a_w2).asUInt
      inA_exp := VecInit(exps_a_w2).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w2(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w2_zero(i)}.asUInt
    }
  }
  if (actSupportFp6_1) {
    val in_a_w3_cl_fp6 = lanes2_a.map { f => classify(MxFormats.fp6_1, f(5, 0)) }
    val (exps_a_w3_fp6_1, sigs_a_w3_fp6_1, signs_a_w3_fp6_1) = in_a_w3_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i*2), peInAWidth / 2, (inAWidth - peInAWidth)/2) }.unzip3
    val in_a_w3_zero_fp6 = in_a_w3_cl_fp6.map(f => f.isZero)

    when (io.type_a.exp === 3.U && io.type_a.sig === 3.U) {
      inA_pe := VecInit(sigs_a_w3_fp6_1).asUInt
      inA_exp := VecInit(exps_a_w3_fp6_1).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w3_fp6_1(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w3_zero_fp6(i)}.asUInt
    }
  }
  if (actSupportFp8_1) {
    val in_a_w3_cl_fp8 = lanes2_a.map { f => classify(MxFormats.fp8_1, f(7, 0)) }
    val (exps_a_w3_fp8_1, sigs_a_w3_fp8_1, signs_a_w3_fp8_1) = in_a_w3_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i*2), peInAWidth / 2, (inAWidth - peInAWidth)/2) }.unzip3
    val in_a_w3_zero_fp8 = in_a_w3_cl_fp8.map(f => f.isZero)

    when (io.type_a.exp === 5.U && io.type_a.sig === 3.U) {
      inA_pe := VecInit(sigs_a_w3_fp8_1).asUInt
      inA_exp := VecInit(exps_a_w3_fp8_1).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w3_fp8_1(i) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w3_zero_fp8(i)}.asUInt
      nanA := in_a_w3_cl_fp8(0).isNaN || in_a_w3_cl_fp8(1).isNaN
    }
  }
  if (actSupportFp6_0) {
    val in_a_w4_cl_fp6 = lanes1_a.map { f => classify(MxFormats.fp6_0, f(5, 0)) }
    val (exps_a_w4_fp6_0, sigs_a_w4_fp6_0, signs_a_w4_fp6_0) = in_a_w4_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i*2), peInAWidth, inAWidth - peInAWidth) }.unzip3
    val in_a_w4_zero_fp6 = in_a_w4_cl_fp6.map(f => f.isZero)

    when (io.type_a.exp === 2.U && io.type_a.sig === 4.U) {
      inA_pe := VecInit(sigs_a_w4_fp6_0).asUInt
      inA_exp := VecInit(exps_a_w4_fp6_0).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w4_fp6_0(0) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w4_zero_fp6(0)}.asUInt
    }
  }
  if (actSupportFp8_0) {
    val in_a_w4_cl_fp8 = lanes1_a.map { f => classify(MxFormats.fp8_0, f(7, 0)) }
    val (exps_a_w4_fp8_0, sigs_a_w4_fp8_0, signs_a_w4_fp8_0) = in_a_w4_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i*2), peInAWidth, inAWidth - peInAWidth) }.unzip3
    val in_a_w4_zero_fp8 = in_a_w4_cl_fp8.map(f => f.isZero)

    when (io.type_a.exp === 4.U && io.type_a.sig === 4.U) {
      inA_pe := VecInit(sigs_a_w4_fp8_0).asUInt
      inA_exp := VecInit(exps_a_w4_fp8_0).asUInt
      inA_sign := VecInit.tabulate(2){ i => signs_a_w4_fp8_0(0) }.asUInt
      in_a_mask := VecInit.tabulate(2){i => in_a_w4_zero_fp8(0)}.asUInt
      nanA := in_a_w4_cl_fp8(0).isNaN
    }
  }

  if (weiSupportFp4) {
    val in_w_w2_cl = lanes2_w.map { f => classify(MxFormats.fp4, f(3, 0)) }
    val (exps_w_w2, sigs_w_w2, signs_w_w2) = in_w_w2_cl.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i), peInBWidth / 2, (inBWidth - peInBWidth)/2) }.unzip3
    val in_w_w2_zero = in_w_w2_cl.map(f => f.isZero)

    // printf(p"in_w_w2_zero: ${Binary(in_w_w2_zero.asUInt)}\n")

    when (io.type_w.sig === 2.U) {
      // printf(p"exps_w_w2: ${Binary(exps_w_w2.asUInt)} \n")
      inW_pe := VecInit(sigs_w_w2).asUInt
      inW_exp := VecInit(exps_w_w2).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w2(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w2_zero(i)}.asUInt
    }
  }
  if (weiSupportFp6_1) {
    val in_w_w3_cl_fp6 = lanes2_w.map { f => classify(MxFormats.fp6_1, f(5, 0)) }
    val (exps_w_w3_fp6_1, sigs_w_w3_fp6_1, signs_w_w3_fp6_1) = in_w_w3_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i), peInBWidth / 2, (inBWidth - peInBWidth)/2) }.unzip3
    val in_w_w3_zero_fp6 = in_w_w3_cl_fp6.map(f => f.isZero)

    when (io.type_w.exp === 3.U && io.type_w.sig === 3.U) {
      inW_pe := VecInit(sigs_w_w3_fp6_1).asUInt
      inW_exp := VecInit(exps_w_w3_fp6_1).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w3_fp6_1(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w3_zero_fp6(i)}.asUInt
    }
  }
  if (weiSupportFp8_1) {
    val in_w_w3_cl_fp8 = lanes2_w.map { f => classify(MxFormats.fp8_1, f(7, 0)) }
    val (exps_w_w3_fp8_1, sigs_w_w3_fp8_1, signs_w_w3_fp8_1) = in_w_w3_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i), peInBWidth / 2, (inBWidth - peInBWidth)/2) }.unzip3
    val in_w_w3_zero_fp8 = in_w_w3_cl_fp8.map(f => f.isZero)

    when (io.type_w.exp === 5.U && io.type_w.sig === 3.U) {
      inW_pe := VecInit(sigs_w_w3_fp8_1).asUInt
      inW_exp := VecInit(exps_w_w3_fp8_1).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w3_fp8_1(i) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w3_zero_fp8(i)}.asUInt
      nanW := in_w_w3_cl_fp8(0).isNaN || in_w_w3_cl_fp8(1).isNaN
    }
  }
  if (weiSupportFp6_0) {
    val in_w_w4_cl_fp6 = lanes1_w.map { f => classify(MxFormats.fp6_0, f(5, 0)) }
    val (exps_w_w4_fp6_0, sigs_w_w4_fp6_0, signs_w_w4_fp6_0) = in_w_w4_cl_fp6.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i), peInBWidth, inBWidth - peInBWidth) }.unzip3
    val in_w_w4_zero_fp6 = in_w_w4_cl_fp6.map(f => f.isZero)

    when (io.type_w.exp === 2.U && io.type_w.sig === 4.U) {
      inW_pe := VecInit(sigs_w_w4_fp6_0).asUInt
      inW_exp := VecInit(exps_w_w4_fp6_0).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w4_fp6_0(0) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w4_zero_fp6(0)}.asUInt
    }
  }
  if (weiSupportFp8_0) {
    val in_w_w4_cl_fp8 = lanes1_w.map { f => classify(MxFormats.fp8_0, f(7, 0)) }
    val (exps_w_w4_fp8_0, sigs_w_w4_fp8_0, signs_w_w4_fp8_0) = in_w_w4_cl_fp8.zipWithIndex.map { case (f, i) => pack(f, expAdderWidths(i), peInBWidth, inBWidth - peInBWidth) }.unzip3
    val in_w_w4_zero_fp8 = in_w_w4_cl_fp8.map(f => f.isZero)

    when (io.type_w.exp === 4.U && io.type_w.sig === 4.U) {
      inW_pe := VecInit(sigs_w_w4_fp8_0).asUInt
      inW_exp := VecInit(exps_w_w4_fp8_0).asUInt
      inW_sign := VecInit.tabulate(2){ i => signs_w_w4_fp8_0(0) }.asUInt
      in_w_mask := VecInit.tabulate(2){i => in_w_w4_zero_fp8(0)}.asUInt
      nanW := in_w_w4_cl_fp8(0).isNaN
    }
  }

  // Compute the sign of the outputs
  //val out_signs = Cat(inA_sign(0) ^ inW_sign(0), inA_sign(0) ^ inW_sign(1), inA_sign(1) ^ inW_sign(0), inA_sign(1) ^ inW_sign(1))
  val out_signs = Cat(inA_sign(1) ^ inW_sign(1),   
                      inA_sign(1) ^ inW_sign(0),   
                      inA_sign(0) ^ inW_sign(1),    
                      inA_sign(0) ^ inW_sign(0))    
  // PE Instantiation
  val out_pe = Wire(UInt(peOutWidth.W))
  val PE = Module(new MxPE(mxparameters, lut))
  PE.io.modeDecoded := io.mode
  PE.io.enable := io.enable
  PE.io.mask_a := ~in_a_mask.asUInt
  PE.io.mask_w := ~in_w_mask.asUInt
  PE.io.in_a := inA_pe
  PE.io.in_w := inW_pe
  // PE.io.activation_mx_format := io.input_mx_format
  // PE.io.weight_mx_format := io.weight_mx_format
  out_pe := PE.io.output

  // printf(p"PE Output: ${Binary(out_pe)}\n")

  // Exp Adder Instantiation
  val out_e = Wire(UInt(totalAdderWidth.W))
  val expAdder = Module(new MxExp(inA_exp_width = inAWidth - peInAWidth, inW_exp_width = inBWidth - peInBWidth, outWidth = totalAdderWidth, elemW = expAdderWidths, outTypes = Seq(outType1, outType4, outType1, outType4)))
  expAdder.io.enable := io.enable
  expAdder.io.modeDecoded := io.mode
  expAdder.io.mask_a := ~in_a_mask.asUInt
  expAdder.io.mask_w := ~in_w_mask.asUInt
  expAdder.io.in_a := inA_exp
  expAdder.io.in_w := inW_exp
  out_e := expAdder.io.out_exp

  // printf(p"InA Exp: ${Binary(inA_exp)}, InW Exp: ${Binary(inW_exp)}, Out Exp: ${Binary(out_e)}\n")


  val out4_toRec = VecInit.tabulate(4) { i =>
    val out4_toRec_norm_1 = normalize(out_pe((i)*(peOutWidth/4) + 5, i*peOutWidth/4), outType4.sig - 1, 6)
    val out4_toRec_norm_2 = normalize(out_pe((i)*(peOutWidth/4) + 4, i*peOutWidth/4), outType4.sig - 1, 5)
    val out4_toRec_norm_3 = normalize(out_pe((i)*(peOutWidth/4) + 3, i*peOutWidth/4), outType4.sig - 1, 4)

    val out4_rec_exp = Mux(io.type_a.sig === 2.U && io.type_w.sig === 2.U,  out4_toRec_norm_3._2,
      Mux(io.type_a.sig === 3.U && io.type_w.sig === 3.U,  out4_toRec_norm_1._2, out4_toRec_norm_2._2))
    val shift_dir = Mux(io.type_a.sig === 2.U && io.type_w.sig === 2.U,  out4_toRec_norm_3._3,
      Mux(io.type_a.sig === 3.U && io.type_w.sig === 3.U,  out4_toRec_norm_1._3, out4_toRec_norm_2._3))
    val out4_rec_sig = Mux(io.type_a.sig === 2.U && io.type_w.sig === 2.U,  out4_toRec_norm_3._1,
      Mux(io.type_a.sig === 3.U && io.type_w.sig === 3.U,  out4_toRec_norm_1._1, out4_toRec_norm_2._1))

    MxPEOutToRaw(
      expWidth = outType4.exp,
      sigWidth = outType4.sig,
      sign = out_signs(i),
      exp = Mux(!shift_dir, out_e((i+1)*(totalAdderWidth/4)-1, i*(totalAdderWidth/4)) -% out4_rec_exp, out_e((i+1)*(totalAdderWidth/4)-1, i*(totalAdderWidth/4)) +% out4_rec_exp),
      sig = out4_rec_sig,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(i / 2) || in_w_mask(i % 2)
    )
  }

  val out2_toRec = VecInit.tabulate(2) { i =>
    val out2_toRec_norm_1 = normalize(out_pe((i)*(peOutWidth/2) + 6, i*peOutWidth/2), outType2.sig - 1, 7)
    val out2_toRec_norm_2 = normalize(out_pe((i)*(peOutWidth/2) + 5, i*peOutWidth/2), outType2.sig - 1 , 6)

    val out2_toRec_exp = Mux(io.type_a.sig === 2.U || io.type_w.sig === 2.U, out2_toRec_norm_2._2, out2_toRec_norm_1._2)
    val shift_dir = Mux(io.type_a.sig === 2.U || io.type_w.sig === 2.U, out2_toRec_norm_2._3, out2_toRec_norm_1._3)
    val out2_toRec_sig = Mux(io.type_a.sig === 2.U || io.type_w.sig === 2.U, out2_toRec_norm_2._1, out2_toRec_norm_1._1)

    MxPEOutToRaw(
      expWidth = outType2.exp,
      sigWidth = outType2.sig,
      sign = out_signs(i*2),
      exp = Mux(!shift_dir, out_e((i)*(totalAdderWidth/2) + outType2.exp, (i)*(totalAdderWidth/2)) -% out2_toRec_exp, out_e((i)*(totalAdderWidth/2) + outType2.exp, (i)*(totalAdderWidth/2)) +% out2_toRec_exp),
      sig = out2_toRec_sig,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(i) || in_w_mask(i % 2)
    )
  }


  val out1_toRec = VecInit.tabulate(1) { i =>
    val out1_toRec_norm = normalize(out_pe(7,0), outType1.sig - 1, 8)

    MxPEOutToRaw(
      expWidth = outType1.exp,
      sigWidth = outType1.sig,
      sign = out_signs(0),
      exp = Mux(!out1_toRec_norm._3, out_e((i)*(totalAdderWidth) + outType1.exp, i*(totalAdderWidth)) -% out1_toRec_norm._2, out_e((i)*(totalAdderWidth) + outType1.exp, i*(totalAdderWidth)) +% out1_toRec_norm._2),
      sig = out1_toRec_norm._1,
      inputNaN  = nanA || nanW,
      inputZero = in_a_mask(0) || in_w_mask(0)
    )
  }

  val addUnits = Seq.fill(4)(Module(new hardfloatHelper.MxMulAddRecFN(cType.exp, cType.sig)))
  val laneMask  = VecInit((0 until 4).map(i => io.enable && (i.U < io.mode.numOutputs)))
  val outputs = Wire(Vec(4, UInt(((cType.exp + cType.sig + 1)).W)))

  def resize(in: RawFloat, inT: MxFormats, outT: MxFormats): RawFloat = {
    val resize_unit = Module(new RoundAnyRawFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig, 0))
    resize_unit.io.in := in
    resize_unit.io.roundingMode := hardfloat.consts.round_near_even
    resize_unit.io.detectTininess := hardfloat.consts.tininess_afterRounding
    resize_unit.io.invalidExc := false.B
    resize_unit.io.infiniteExc := false.B
    rawFloatFromRecFN(outT.exp, outT.sig, resize_unit.io.out)
  }

  for (i <- 0 until 4) {
    val rawIn = Mux(io.mode.numOutputs === 4.U,
      resize(out4_toRec(i), outType4, cType),
      Mux(io.mode.numOutputs === 2.U,
        resize(out2_toRec(i/2), outType2, cType),
        resize(out1_toRec(0), outType1, cType)))
    val recIn_c = io.rec_c.asTypeOf(Vec(4, UInt((cType.exp + cType.sig + 1).W)))(i)

    addUnits(i).io.roundingMode := hardfloat.consts.round_near_even
    addUnits(i).io.detectTininess := hardfloat.consts.tininess_afterRounding
    // addUnits(i).io.op := 0.U
    // addUnits(i).io.validin := laneMask(i)
    addUnits(i).io.a := rawIn
    addUnits(i).io.c := recIn_c

    outputs(i) := addUnits(i).io.out
  }

  io.out := outputs.asUInt


}

object MxPEOutToRaw {
  def apply(expWidth: Int, sigWidth: Int, sign: UInt, exp: UInt, sig: UInt, inputNaN: Bool, inputZero: Bool): RawFloat = {
    val expIn = exp(expWidth, 0)
    val fractIn = sig(sigWidth-2, 0)

    val isZeroExpIn = (expIn === 0.U)
    val isZeroFractIn = (fractIn === 0.U)

    val normDist = countLeadingZeros(fractIn)
    val subnormFract = if (sigWidth > 2) {
      (fractIn << normDist) (sigWidth - 3, 0) << 1
    } else {
      0.U
    }
    val adjustedExp =
      Mux(isZeroExpIn,
        normDist ^ ((BigInt(1) << (expWidth + 1)) - 1).U,
        expIn
      ) + ((BigInt(1) << (expWidth - 1)).U
        | Mux(isZeroExpIn, 2.U, 1.U))

    val isZero = isZeroExpIn && isZeroFractIn
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    // Saturate to max finite value (FP8 E4M3 alt0: 0x7E = 448) on exponent overflow
    // but propagate NaN when an input was actually NaN
    val satExp  = (BigInt(3) << (expWidth - 1)).U((expWidth + 1).W)
    val satFrac = ((BigInt(1) << (sigWidth - 1)) - 2).U((sigWidth - 1).W)
    val saturate     = isSpecial && !inputNaN
    val effectiveZero = (isZero || inputZero) && !inputNaN

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN  := inputNaN
    out.isInf  := false.B
    out.isZero := effectiveZero
    out.sign   := sign
    out.sExp   := Mux(inputNaN || saturate, satExp, adjustedExp(expWidth, 0)).zext
    out.sig    := 0.U(1.W) ## !effectiveZero ## Mux(inputNaN || saturate, satFrac, Mux(isZeroExpIn, subnormFract, fractIn))
    out
  }
}

package gemmini 

import chisel3._
import chisel3.util._


class MxExp(inA_exp_width: Int, inW_exp_width: Int, outWidth: Int, elemW: Seq[Int], outTypes: Seq[MxFormats]) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(UInt(inA_exp_width.W))
    val in_w = Input(UInt(inW_exp_width.W))
    val modeDecoded = Input(new mxMode())
    val mask_a = Input(UInt(2.W))
    val mask_w = Input(UInt(2.W))
    val enable = Input(Bool())
    val out_exp = Output(UInt(outWidth.W))
  })

  val in2_a = io.in_a.asTypeOf(Vec(2, UInt((inA_exp_width/2).W)))
  val in1_a = io.in_a.asTypeOf(Vec(1, UInt((inA_exp_width).W)))

  val in2_w = io.in_w.asTypeOf(Vec(2, UInt((inW_exp_width/2).W)))
  val in1_w = io.in_w.asTypeOf(Vec(1, UInt((inW_exp_width).W)))

  val laneMask  = VecInit((0 until 4).map(i => io.enable && (Mux(io.modeDecoded.numOutputs === 1.U, i.U < io.modeDecoded.numOutputs, Mux(io.modeDecoded.numOutputs === 2.U, i.U === 0.U || i.U === 2.U, true.B)))))

  val sums = elemW.zipWithIndex.map{ case (w, i) =>
    val adder = Module(new AddBit(w, outTypes(i)))
    if (w > inA_exp_width/2) {
      adder.io.a := Mux(io.modeDecoded.actInputs === 1.U, in1_a(0)(w-1, 0).asSInt, in2_a(i/2).asSInt.pad(w))
    } else {
      adder.io.a := Mux(io.modeDecoded.actInputs === 1.U, in1_a(0)(w-1, 0).asSInt, in2_a(i/2)(w-1, 0).asSInt)
    } 
    if (w > inW_exp_width/2) {
      adder.io.b := Mux(io.modeDecoded.weiInputs === 1.U, in1_w(0)(w-1, 0).asSInt, in2_w(i%2).asSInt.pad(w))
    } else {
      adder.io.b := Mux(io.modeDecoded.weiInputs === 1.U, in1_w(0)(w-1, 0).asSInt, in2_w(i%2)(w-1, 0).asSInt)
    }
    adder.io.enable := laneMask(i) && io.mask_a(i/2) && io.mask_w(i%2)
    adder.io.y
  }

  io.out_exp := Mux(io.modeDecoded.numOutputs === 1.U, sums(0).pad(outWidth), Mux(io.modeDecoded.numOutputs === 2.U, VecInit(sums(0).pad(outWidth/2), sums(2).pad(outWidth/2)).asUInt, VecInit(sums.map(_.pad(outWidth/4))).asUInt))

}

class AddBit(elemW: Int, outType: MxFormats) extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(elemW.W))
    val b = Input(SInt(elemW.W))
    val enable = Input(Bool())
    val y = Output(UInt((outType.exp + 1).W))
  })
  val partial1 = io.a +& io.b
  val bias = 2.S +& outType.bias.S.pad(outType.exp + 1)
  val result = partial1 +& bias
  io.y := Mux(io.enable, result.pad(outType.exp+1).asUInt(outType.exp, 0), 0.U((outType.exp + 1).W))
}
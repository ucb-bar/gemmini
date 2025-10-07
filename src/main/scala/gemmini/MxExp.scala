package mxHardware 

import chisel3._
import chisel3.util._


class MxExp(inA_exp_width: Int, inW_exp_width: Int, elemW: Int, outType: MxFormats) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(UInt(inA_exp_width.W))
    val in_w = Input(UInt(inW_exp_width.W))
    val modeDecoded = Input(new mxMode())
    val mask_a = Input(UInt(4.W))
    val mask_w = Input(UInt(4.W))
    val enable = Input(Bool())
    val out_exp = Output(UInt((outType.exp * 4).W))
  })

  val adders = Seq.fill(4){ Module(new Add6Bit(elemW, outType)) }

  val laneMask  = VecInit((0 until 4).map(i => io.enable && (Mux(io.modeDecoded.numOutputs === 1.U, i.U < io.modeDecoded.numOutputs, Mux(io.modeDecoded.numOutputs === 2.U, i.U === 0.U || i.U === 2.U, true.B)))))
  val input_a = io.in_a.asTypeOf(Vec(2, SInt((inA_exp_width/2).W)))
  val input_w = io.in_w.asTypeOf(Vec(4, SInt((inW_exp_width/4).W)))

  val sums = Wire(Vec(4, UInt(outType.exp.W)))
  for (i <- 0 until 4) {
    val a_2 = input_a(i/2)
    val a_1 = input_a(0)

    val w_4 = input_w(i)
    val w_2 = input_w(i)
    val w_1 = input_w(0)

    adders(i).io.a := Mux(io.modeDecoded.actInputs === 1.U, a_1, a_2)
    adders(i).io.b := Mux(io.modeDecoded.weiInputs === 1.U, w_1, Mux(io.modeDecoded.weiInputs === 2.U, w_2, w_4))
    adders(i).io.enable := laneMask(i) && io.mask_a(i) && io.mask_w(i)
    sums(i)       := adders(i).io.y
  }

  io.out_exp := sums.asUInt
}

class Add6Bit(elemW: Int, outType: MxFormats) extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(elemW.W))
    val b = Input(SInt(elemW.W))
    val enable = Input(Bool())
    val y = Output(UInt(outType.exp.W))
  })
  val partial1 = io.a.pad(outType.exp) +& io.b.pad(outType.exp)
  val bias = 2.S +& outType.bias.S.pad(outType.exp + 1)
  val result = partial1 + bias
  // printf(p"a: ${Binary(io.a)}, b: ${Binary(io.b)}, result: ${Binary(result)}\n")
  io.y := Mux(io.enable, result.asUInt(outType.exp - 1, 0), 0.U(outType.exp.W))
}
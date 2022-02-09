package gemmini

import chisel3._
import chisel3.util._

import Util._

class AccumulatorReadRespWithFullData[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U, shift_width: Int) extends Bundle {
  val resp = new AccumulatorReadResp(fullDataType, scale_t, shift_width)
  val full_data = fullDataType.cloneType
}


class AccumulatorScaleResp[T <: Data: Arithmetic](fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]]) extends Bundle {
  val full_data = fullDataType.cloneType
  val data = rDataType.cloneType
  val acc_bank_id = UInt(2.W)
  val fromDMA = Bool()
}

class AccumulatorScaleIO[T <: Data: Arithmetic, U <: Data](
  fullDataType: Vec[Vec[T]], scale_t: U, shift_width: Int,
  rDataType: Vec[Vec[T]]
) extends Bundle {
  val in = Flipped(Decoupled(new AccumulatorReadResp[T,U](fullDataType, scale_t, shift_width)))
  val out = Decoupled(new AccumulatorScaleResp[T](fullDataType, rDataType))
}

class AccScaleDataWithIndex[T <: Data: Arithmetic, U <: Data](t: T, u: U) extends Bundle {
  val shift_width = log2Ceil(t.getWidth)

  val scale = u.cloneType
  val act = UInt(2.W) // TODO magic number
  val relu6_shift = UInt(shift_width.W)
  val data = t.cloneType
  val full_data = t.cloneType
  val id = UInt(2.W) // TODO hardcoded
  val index = UInt()
}

class AccScalePipe[T <: Data : Arithmetic, U <: Data](t: T, rDataType: Vec[Vec[T]], scale_func: (T, U) => T, scale_t: U, latency: Int, has_nonlinear_activations: Boolean)(implicit ev: Arithmetic[T]) extends Module {
  val u = scale_t
  val io = IO(new Bundle {
    val in = Input(Valid(new AccScaleDataWithIndex(t, u)(ev)))
    val out = Output(Valid(new AccScaleDataWithIndex(t, u)(ev)))
  })
  import ev._
  val out = WireInit(io.in)

  val e_scaled = scale_func(io.in.bits.data, io.in.bits.scale)
  val e_clipped = e_scaled.clippedToWidthOf(rDataType.head.head)
  val e_act = MuxCase(e_clipped, Seq(
    (has_nonlinear_activations.B && io.in.bits.act === Activation.RELU) -> e_clipped.relu,
    (has_nonlinear_activations.B && io.in.bits.act === Activation.RELU6) -> e_clipped.relu6(io.in.bits.relu6_shift)))

  out.bits.data := e_act
  io.out := Pipe(out, latency)
}


class AccumulatorScale[T <: Data: Arithmetic, U <: Data](
  fullDataType: Vec[Vec[T]], rDataType: Vec[Vec[T]],
  scale_t: U, shift_width: Int,
  read_small_data: Boolean, read_full_data: Boolean,
  scale_func: (T, U) => T,
  num_scale_units: Int,
  latency: Int,
  has_nonlinear_activations: Boolean)(implicit ev: Arithmetic[T]) extends Module {

  import ev._

  val io = IO(new AccumulatorScaleIO[T,U](
    fullDataType, scale_t, shift_width, rDataType
  )(ev))
  val t = io.in.bits.data(0)(0).cloneType
  val out = Wire(Decoupled(new AccumulatorScaleResp[T](
    fullDataType, rDataType)(ev)))

  if (num_scale_units == -1) {
    val in = Wire(Decoupled(new AccumulatorReadRespWithFullData(fullDataType, scale_t, shift_width)(ev)))
    in.valid := io.in.valid
    io.in.ready := in.ready
    in.bits.resp := io.in.bits
    in.bits.full_data := io.in.bits.data

    val pipe_out = Pipeline(in, latency, Seq.fill(latency)((x: AccumulatorReadRespWithFullData[T,U]) => x) :+ {
      x: AccumulatorReadRespWithFullData[T,U] =>
      val activated_rdata = VecInit(x.resp.data.map(v => VecInit(v.map { e =>
        val e_scaled = scale_func(e, x.resp.scale)
        val e_clipped = e_scaled.clippedToWidthOf(rDataType.head.head)
        val e_act = MuxCase(e_clipped, Seq(
          (x.resp.act === Activation.RELU) -> e_clipped.relu,
          (x.resp.act === Activation.RELU6) -> e_clipped.relu6(x.resp.relu6_shift)))

        e_act
      })))
      val result = WireInit(x)
      result.resp.data := activated_rdata
      result
    })
    out.valid      := pipe_out.valid
    pipe_out.ready := out.ready
    out.bits.full_data := pipe_out.bits.full_data
    out.bits.data      := pipe_out.bits.resp.data
    out.bits.fromDMA   := pipe_out.bits.resp.fromDMA
    out.bits.acc_bank_id := pipe_out.bits.resp.acc_bank_id
  } else {
    val width = io.in.bits.data.size * io.in.bits.data(0).size
    val nEntries = 3
    val regs = Reg(Vec(nEntries, Valid(new AccumulatorReadResp[T,U](
      fullDataType, scale_t, shift_width)(ev))))
    val out_regs = Reg(Vec(nEntries, new AccumulatorScaleResp[T](
      fullDataType, rDataType)(ev)))

    val fired_masks = Reg(Vec(nEntries, Vec(width, Bool())))
    val completed_masks = Reg(Vec(nEntries, Vec(width, Bool())))
    val head_oh = RegInit(1.U(nEntries.W))
    val tail_oh = RegInit(1.U(nEntries.W))
    out.valid := Mux1H(head_oh.asBools, (regs zip completed_masks).map({case (r, c) => r.valid && c.reduce(_&&_)}))
    out.bits  := Mux1H(head_oh.asBools, out_regs)
    when (out.fire) {
      for (i <- 0 until nEntries) {
        when (head_oh(i)) {
          regs(i).valid := false.B
        }
      }
      head_oh := (head_oh << 1) | head_oh(nEntries-1)
    }

    io.in.ready := !Mux1H(tail_oh.asBools, regs.map(_.valid)) || (tail_oh === head_oh && out.fire)
    when (io.in.fire) {
      for (i <- 0 until nEntries) {
        when (tail_oh(i)) {
          regs(i).valid := true.B
          regs(i).bits  := io.in.bits
          out_regs(i).fromDMA := io.in.bits.fromDMA
          out_regs(i).acc_bank_id := io.in.bits.acc_bank_id
          fired_masks(i).foreach(_ := false.B)
          completed_masks(i).foreach(_ := false.B)
        }
      }
      tail_oh := (tail_oh << 1) | tail_oh(nEntries-1)
    }

    val inputs = Seq.fill(width*nEntries) { Wire(Decoupled(new AccScaleDataWithIndex(t, scale_t)(ev))) }

    for (i <- 0 until nEntries) {
      for (w <- 0 until width) {
        val input = inputs(i*width+w)
        input.valid       := regs(i).valid && !fired_masks(i)(w)
        input.bits.data   := regs(i).bits.data(w / io.in.bits.data(0).size)(w % io.in.bits.data(0).size)
        input.bits.full_data := regs(i).bits.data(w / io.in.bits.data(0).size)(w % io.in.bits.data(0).size)
        input.bits.scale  := regs(i).bits.scale
        input.bits.act    := regs(i).bits.act
        input.bits.relu6_shift := regs(i).bits.relu6_shift
        input.bits.id := i.U
        input.bits.index := w.U
        when (input.fire) {
          fired_masks(i)(w) := true.B
        }
      }
    }
    for (i <- 0 until num_scale_units) {
      val arbIn = inputs.zipWithIndex.filter({ case (_, w) => w % num_scale_units == i }).map(_._1)
      val arb = Module(new RRArbiter(new AccScaleDataWithIndex(t, scale_t)(ev), arbIn.length))
      arb.io.in <> arbIn
      arb.io.out.ready := true.B
      val arbOut = Reg(Valid(new AccScaleDataWithIndex(t, scale_t)(ev)))
      arbOut.valid := arb.io.out.valid
      arbOut.bits  := arb.io.out.bits
      when (reset.asBool) {
        arbOut.valid := false.B
      }
      val pipe = Module(new AccScalePipe(t, rDataType, scale_func, scale_t, latency, has_nonlinear_activations)(ev, ev))
      pipe.io.in := arbOut
      val pipe_out = pipe.io.out

      for (j <- 0 until nEntries) {
        for (w <- 0 until width) {
          if ((j*width+w) % num_scale_units == i) {
            val id0 = w % io.in.bits.data(0).size
            val id1 = w / io.in.bits.data(0).size
            when (pipe_out.fire && pipe_out.bits.id === j.U && pipe_out.bits.index === w.U) {
              out_regs(j).data     (id1)(id0) := pipe_out.bits.data
              out_regs(j).full_data(id1)(id0) := pipe_out.bits.full_data
              completed_masks(j)(w) := true.B
            }
          }
        }
      }
    }
    when (reset.asBool) {
      regs.foreach(_.valid := false.B)
    }
  }

  io.out <> out

  if (read_small_data)
    io.out.bits.data := out.bits.data
  else
    io.out.bits.data := DontCare

  if (read_full_data)
    io.out.bits.full_data := out.bits.full_data
  else
    io.out.bits.full_data := DontCare

}


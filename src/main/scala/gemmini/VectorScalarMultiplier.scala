package gemmini

import chisel3._
import chisel3.util._

import Util._

class VectorScalarMultiplierReq[T <: Data, U <: Data, Tag <: Data](block_cols: Int, t: T, u: U, tag_t: Tag) extends Bundle {
  val in: Vec[T] = Vec(block_cols, t.cloneType)
  val scale: U = u.cloneType
  val tag: Tag = tag_t.cloneType

  override def cloneType: VectorScalarMultiplierReq.this.type = new VectorScalarMultiplierReq(block_cols, t, u, tag_t).asInstanceOf[this.type]
}

class VectorScalarMultiplierResp[T <: Data, Tag <: Data](block_cols: Int, t: T, tag_t: Tag) extends Bundle {
  val out: Vec[T] = Vec(block_cols, t.cloneType)
  val tag: Tag = tag_t.cloneType

  override def cloneType: VectorScalarMultiplierResp.this.type = new VectorScalarMultiplierResp(block_cols, t, tag_t).asInstanceOf[this.type]
}

// Currently, this class only supports multiplications of scratchpad inputs, rather than accumulator inputs
// class VectorScalarMultiplier[T <: Data, U <: Data, Tag <: Data](config: GemminiArrayConfig[T, U], tag_t: Tag) extends Module {
  // import config._
  // val block_cols = meshColumns * tileColumns
class VectorScalarMultiplier[T <: Data, U <: Data, Tag <: Data](mvin_scale_args: Option[MvinScaleArguments[T, U]], block_cols: Int, t: T, tag_t: Tag) extends Module {

  val u = mvin_scale_args match {
    case Some(MvinScaleArguments(_, _, multiplicand_t)) => multiplicand_t
    case None => Bool() // TODO make this a 0 width UInt
  }

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new VectorScalarMultiplierReq(block_cols, t, u, tag_t)))
    val resp = Decoupled(new VectorScalarMultiplierResp(block_cols, t, tag_t))
  })

  val req = Reg(UDValid(chiselTypeOf(io.req.bits)))

  mvin_scale_args match {
    case Some(MvinScaleArguments(mvin_scale_func, _, t)) => {
      io.req.ready := !req.valid || io.resp.fire()
      io.resp.valid := req.valid
      io.resp.bits.out := req.bits.in.map(x => mvin_scale_func(x, req.bits.scale.asTypeOf(t)))
      io.resp.bits.tag := req.bits.tag
    }

    case None => {
      io.req.ready := io.resp.ready
      io.resp.valid := io.req.valid
      io.resp.bits.out := io.req.bits.in
      io.resp.bits.tag := io.req.bits.tag
    }
  }

  when (io.req.fire()) {
    req.push(io.req.bits)
  }.elsewhen(io.resp.fire()) {
    req.pop()
  }

  when (reset.toBool()) {
    req.pop()
  }

  dontTouch(io.req)
  dontTouch(io.resp)
  dontTouch(req)
}

object VectorScalarMultiplier {
  // Returns the input and output IO of the module (together with the pipeline)
  def apply[T <: Data, U <: Data, Tag <: Data](config: GemminiArrayConfig[T, U], tag_t: Tag, is_acc: Boolean) = {
    val mvin_scale_args = if (is_acc) config.mvin_scale_acc_args else config.mvin_scale_args
    val t = if (is_acc) config.accType else config.inputType
    val block_cols = config.meshColumns * config.tileColumns

    val vsm = Module(new VectorScalarMultiplier(mvin_scale_args, block_cols, t, tag_t))

    val in = vsm.io.req
    val out = mvin_scale_args match {
      case Some(MvinScaleArguments(_, latency, _)) => Pipeline(vsm.io.resp, latency)
      case None => vsm.io.resp
    }

    (in, out)
  }
}

package gemmini

import chisel3._
import chisel3.util._

import Util._

class PixelRepeaterReq[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Bundle {
  val in: Vec[T] = Vec(block_cols, t.cloneType)
  val mask: Vec[Bool] = Vec(block_cols, Bool())
  val laddr: LocalAddr = laddr_t.cloneType
  val len: UInt = UInt(8.W) // TODO magic number
  val pixel_repeats: UInt = UInt(8.W) // TODO magic number
  val last: Bool = Bool()
  val tag: Tag = tag_t.cloneType

  assert(block_cols <= 255, "len must be longer")

  override def cloneType: PixelRepeaterReq.this.type = new PixelRepeaterReq(t, laddr_t, block_cols, tag_t).asInstanceOf[this.type]
}

class PixelRepeaterResp[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Bundle {
  val out: Vec[T] = Vec(block_cols, t.cloneType)
  val mask: Vec[Bool] = Vec(block_cols, Bool())
  val laddr: LocalAddr = laddr_t.cloneType
  val last: Bool = Bool()
  val tag: Tag = tag_t.cloneType

  override def cloneType: PixelRepeaterResp.this.type = new PixelRepeaterResp(t, laddr_t, block_cols, tag_t).asInstanceOf[this.type]
}

class PixelRepeater[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new PixelRepeaterReq(t, laddr_t, block_cols, tag_t)))
    val resp = Decoupled(new PixelRepeaterResp(t, laddr_t, block_cols, tag_t))
  })

  val req = Reg(UDValid(io.req.bits.cloneType))

  io.req.ready := !req.valid || (io.resp.ready && req.bits.pixel_repeats === 0.U)

  io.resp.valid := req.valid

  io.resp.bits.out := (req.bits.in.asUInt() << (req.bits.pixel_repeats * req.bits.len)).asTypeOf(io.resp.bits.out)
  io.resp.bits.mask := (req.bits.mask.asUInt() << (req.bits.pixel_repeats * req.bits.len)).asTypeOf(io.resp.bits.mask)

  io.resp.bits.last := req.bits.last && (req.bits.pixel_repeats === 0.U)
  io.resp.bits.tag := req.bits.tag

  io.resp.bits.laddr := Mux(req.bits.laddr.full_sp_addr() < (laddr_t.maxRows / 2).U,
    req.bits.laddr.floorSub(req.bits.pixel_repeats, 0.U),
    req.bits.laddr.floorSub(req.bits.pixel_repeats, (laddr_t.maxRows / 2).U),
  )

  assert(!(req.valid && req.bits.laddr.is_acc_addr))

  when (io.req.fire()) {
    req.push(io.req.bits)
    req.bits.pixel_repeats := io.req.bits.pixel_repeats - 1.U
  }

  when (io.resp.fire()) {
    req.bits.pixel_repeats := req.bits.pixel_repeats - 1.U

    when (req.bits.pixel_repeats === 0.U) {
      req.pop()
    }
  }

  when (reset.toBool()) {
    req.pop()
  }
}

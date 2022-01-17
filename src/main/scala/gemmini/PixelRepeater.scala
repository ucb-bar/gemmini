package gemmini

import chisel3._
import chisel3.util._

import Util._

class PixelRepeaterReq[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, aligned_to: Int, tag_t: Tag) extends Bundle {
  val in: Vec[T] = Vec(block_cols, t.cloneType)
  val mask: Vec[Bool] = Vec(block_cols * (t.getWidth/8) / aligned_to, Bool())
  val laddr: LocalAddr = laddr_t.cloneType
  val len: UInt = UInt(log2Up(block_cols+1).W) // TODO magic number
  val pixel_repeats: UInt = UInt(8.W) // TODO magic number
  val last: Bool = Bool()
  val tag: Tag = tag_t.cloneType

  assert(block_cols <= 255, "len must be longer")
}

class PixelRepeaterResp[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, aligned_to: Int, tag_t: Tag) extends Bundle {
  val out: Vec[T] = Vec(block_cols, t.cloneType)
  val mask: Vec[Bool] = Vec(block_cols * (t.getWidth/8) / aligned_to, Bool())
  val laddr: LocalAddr = laddr_t.cloneType
  val last: Bool = Bool()
  val tag: Tag = tag_t.cloneType
}

class PixelRepeater[T <: Data, Tag <: Data](t: T, laddr_t: LocalAddr, block_cols: Int, aligned_to: Int, tag_t: Tag, passthrough: Boolean) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new PixelRepeaterReq(t, laddr_t, block_cols, aligned_to, tag_t)))
    val resp = Decoupled(new PixelRepeaterResp(t, laddr_t, block_cols, aligned_to, tag_t))
  })

  if (passthrough) {
    io.resp.valid := io.req.valid
    io.resp.bits.out := io.req.bits.in
    io.resp.bits.mask := io.req.bits.mask
    io.resp.bits.laddr := io.req.bits.laddr
    io.resp.bits.last := io.req.bits.last
    io.resp.bits.tag := io.req.bits.tag

    io.req.ready := io.resp.ready
  } else {
    val req = Reg(UDValid(io.req.bits.cloneType))

    io.req.ready := !req.valid || (io.resp.ready && req.bits.pixel_repeats === 0.U)

    val out_shift = Wire(UInt(log2Up(block_cols / 2 + 1).W))
    out_shift := req.bits.pixel_repeats * req.bits.len

    io.resp.bits.out := (req.bits.in.asUInt() << (out_shift * t.getWidth.U)).asTypeOf(io.resp.bits.out)
    io.resp.bits.mask := (req.bits.mask.asUInt() << (out_shift * ((t.getWidth / 8) / aligned_to).U)).asTypeOf(io.resp.bits.mask)

    io.resp.bits.last := req.bits.last && (req.bits.pixel_repeats === 0.U)
    io.resp.bits.tag := req.bits.tag

    val is_acc_addr = req.bits.laddr.is_acc_addr
    assert(!(req.valid && is_acc_addr && req.bits.pixel_repeats > 0.U))

    val sp_addr = Mux(req.bits.laddr.full_sp_addr() < (laddr_t.spRows / 2).U,
      req.bits.laddr.floorSub(req.bits.pixel_repeats, 0.U)._1,
      req.bits.laddr.floorSub(req.bits.pixel_repeats, (laddr_t.spRows / 2).U)._1,
    )

    val underflow = !is_acc_addr && Mux(req.bits.laddr.full_sp_addr() < (laddr_t.spRows / 2).U,
      req.bits.laddr.floorSub(req.bits.pixel_repeats, 0.U)._2,
      req.bits.laddr.floorSub(req.bits.pixel_repeats, (laddr_t.spRows / 2).U)._2,
    )

    io.resp.bits.laddr := Mux(is_acc_addr, req.bits.laddr, sp_addr)

    io.resp.valid := req.valid && !underflow

    when(io.resp.fire || underflow) {
      req.bits.pixel_repeats := req.bits.pixel_repeats - 1.U

      when(req.bits.pixel_repeats === 0.U) {
        req.pop()
      }
    }

    when(io.req.fire) {
      req.push(io.req.bits)
      req.bits.pixel_repeats := io.req.bits.pixel_repeats - 1.U
    }

    when(reset.asBool()) {
      req.pop()
    }
  }
}

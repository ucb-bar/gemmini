package gemmini

import chisel3._
import chisel3.util._

import Util._

class ZeroWriterReq[Tag <: Data](laddr_t: LocalAddr, max_cols: Int, tag_t: Tag) extends Bundle {
  val laddr = laddr_t
  val cols = UInt(log2Up(max_cols+1).W)
  val block_stride = UInt(16.W) // TODO magic number
  val tag = tag_t

}

class ZeroWriterResp[Tag <: Data](laddr_t: LocalAddr, block_cols: Int, tag_t: Tag) extends Bundle {
  val laddr = laddr_t.cloneType
  val mask = Vec(block_cols, Bool())
  val last = Bool()
  val tag = tag_t

}

class ZeroWriter[T <: Data, U <: Data, V <: Data, Tag <: Data](config: GemminiArrayConfig[T, U, V], tag_t: Tag)
  extends Module {
  import config._

  val block_cols = meshColumns * tileColumns
  val max_cols = (dma_maxbytes / (inputType.getWidth / 8)) max block_cols

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))
    val resp = Decoupled(new ZeroWriterResp(local_addr_t, block_cols, tag_t))
  })

  val req = Reg(UDValid(new ZeroWriterReq(local_addr_t, max_cols, tag_t)))

  val col_counter = Reg(UInt(log2Up(max_cols).W))

  io.req.ready := !req.valid

  io.resp.valid := req.valid
  io.resp.bits.laddr := req.bits.laddr + req.bits.block_stride * (col_counter / block_cols.U)
  io.resp.bits.mask.zipWithIndex.foreach { case (m, i) => m := col_counter + i.U < req.bits.cols }
  io.resp.bits.last := col_counter +& block_cols.U >= req.bits.cols
  io.resp.bits.tag := req.bits.tag

  when (io.resp.fire) {
    val next_col_counter = floorAdd(col_counter, block_cols.U, req.bits.cols)

    col_counter := next_col_counter

    when (next_col_counter === 0.U) {
      req.pop()
      io.req.ready := true.B
    }
  }

  when (io.req.fire) {
    req.push(io.req.bits)

    col_counter := 0.U
  }

  when (reset.asBool()) {
    req.pop()
  }
}

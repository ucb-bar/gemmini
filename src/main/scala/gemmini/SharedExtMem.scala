package gemmini

import chisel3._
import chisel3.util._

import Util._


class ExtMemIO extends Bundle {
  val read_req = DecoupledIO(UInt())
  val read_resp = Flipped(DecoupledIO(UInt()))
  val write_req = DecoupledIO(new Bundle {
    val addr = Output(UInt())
    val data = Output(UInt())
    val mask = Output(UInt())
  })
}

class ExtSpadMemIO(sp_banks: Int, acc_banks: Int, acc_sub_banks: Int) extends Bundle {
  val spad = Vec(sp_banks, new ExtMemIO)
  val acc = Vec(acc_banks, Vec(acc_sub_banks, new ExtMemIO))
}


class SharedSyncReadMem(nSharers: Int, depth: Int, mask_len: Int, data_len: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(nSharers, Flipped(new ExtMemIO()))
  })
  val mem = SyncReadMem(depth, Vec(mask_len, UInt(data_len.W)))
  val wens = io.in.map(_.write_req.valid)
  val wen = wens.reduce(_||_)
  val waddr = Mux1H(wens, io.in.map(_.write_req.bits.addr))
  val wmask = Mux1H(wens, io.in.map(_.write_req.bits.mask))
  val wdata = Mux1H(wens, io.in.map(_.write_req.bits.data))
  assert(PopCount(wens) <= 1.U)
  val rens = io.in.map(_.read_req.valid)
  assert(PopCount(rens) <= 1.U)
  val ren = rens.reduce(_||_)
  val raddr = Mux1H(rens, io.in.map(_.read_req.bits))
  val rdata = mem.read(raddr, ren && !wen)
  io.in.foreach(_.read_resp.bits := rdata.asUInt)
  when (wen) {
    mem.write(waddr, wdata.asTypeOf(Vec(mask_len, UInt(data_len.W))), wmask.asTypeOf(Vec(mask_len, Bool())))
  }

}

class SharedExtMem(
  sp_banks: Int, acc_banks: Int, acc_sub_banks: Int,
  sp_depth: Int, sp_mask_len: Int, sp_data_len: Int,
  acc_depth: Int, acc_mask_len: Int, acc_data_len: Int
) extends Module {
  val nSharers = 2
  val io = IO(new Bundle {
    val in = Vec(nSharers, Flipped(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks)))
  })
  for (i <- 0 until sp_banks) {
    val spad_mem = Module(new SharedSyncReadMem(nSharers, sp_depth, sp_mask_len, sp_data_len))
    for (w <- 0 until nSharers) {
      spad_mem.io.in(w) <> io.in(w).spad(i)
    }
  }
  for (i <- 0 until acc_banks) {
    for (s <- 0 until acc_sub_banks) {
      val acc_mem = Module(new SharedSyncReadMem(nSharers, acc_depth, acc_mask_len, acc_data_len))

      acc_mem.io.in(0) <> io.in(0).acc(i)(s)
      // The FP gemmini expects a taller, skinnier accumulator mem
      acc_mem.io.in(1) <> io.in(1).acc(i)(s)
      acc_mem.io.in(1).read_req.bits := io.in(1).acc(i)(s).read_req.bits >> 1
      io.in(1).acc(i)(s).read_resp.bits := acc_mem.io.in(1).read_resp.bits.asTypeOf(Vec(2, UInt((acc_data_len * acc_mask_len / 2).W)))(RegNext(io.in(1).acc(i)(s).read_req.bits(0)))

      acc_mem.io.in(1).write_req.bits.addr := io.in(1).acc(i)(s).write_req.bits.addr >> 1
      acc_mem.io.in(1).write_req.bits.data := Cat(io.in(1).acc(i)(s).write_req.bits.data,
        io.in(1).acc(i)(s).write_req.bits.data)
      acc_mem.io.in(1).write_req.bits.mask := Mux(io.in(1).acc(i)(s).write_req.bits.addr(0),
        io.in(1).acc(i)(s).write_req.bits.mask << (acc_mask_len / 2), io.in(1).acc(i)(s).write_req.bits.mask)
    }
  }
}


package gemmini
import chisel3._
import chisel3.util._

class SinglePortedSyncMemIO[T <: Data](n: Int, t: T) extends Bundle {
  val addr = Input(UInt((log2Ceil(n) max 1).W))
  val wdata = Input(t)
  val rdata = Output(t)
  val wen = Input(Bool())
  val ren = Input(Bool())

}

class SinglePortSyncMem[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new SinglePortedSyncMemIO(n, t))

  assert(!(io.ren && io.wen), "undefined behavior in single-ported SRAM")

  val mem = SyncReadMem(n, t)

  when (io.wen) {
    mem.write(io.addr, io.wdata)
    io.rdata := DontCare
  }.otherwise {
    io.rdata := mem.read(io.addr, io.ren)
  }
}

class TwoPortSyncMem[T <: Data](n: Int, t: T, mask_len: Int) extends Module {
  val io = IO(new Bundle {
    val waddr = Input(UInt((log2Ceil(n) max 1).W))
    val raddr = Input(UInt((log2Ceil(n) max 1).W))
    val wdata = Input(t)
    val rdata = Output(t)
    val wen = Input(Bool())
    val ren = Input(Bool())
    val mask = Input(Vec(mask_len, Bool()))
  })

  assert(!(io.wen && io.ren && io.raddr === io.waddr), "undefined behavior in dual-ported SRAM")

  // val mem = SyncReadMem(n, t)
  val mask_elem = UInt((t.getWidth / mask_len).W)
  val mem = SyncReadMem(n, Vec(mask_len, mask_elem))

  io.rdata := mem.read(io.raddr, io.ren).asTypeOf(t)

  when (io.wen) {
    mem.write(io.waddr, io.wdata.asTypeOf(Vec(mask_len, mask_elem)), io.mask)
  }
}

class AsymmetricTwoPortSyncMem[T <: Data](n: Int, t: T, mask_len: Int) extends Module {
  val fullWidth = t.getWidth
  val halfWidth = fullWidth / 2
  assert(!(mask_len % 2 == 1), "masklen must be even")
  val half_mask_len = mask_len / 2
  val mask_elem = UInt((fullWidth / mask_len).W)

  val io = IO(new Bundle {
    // full width write
    val waddr = Input(UInt((log2Ceil(n) max 1).W))
    val wdata = Input(t)
    val mask = Input(Vec(mask_len, Bool()))
    val wen = Input(Bool())

    // full width write (for accumulation RMW)
    val raddr_full = Input(UInt((log2Ceil(n) max 1).W))
    val rdata_full = Output(t)
    val ren_full = Input(Bool())

    val raddr_half = Input(UInt((log2Ceil(n) + 1).W))
    val ren_half = Input(Bool())
    val rdata_half = Output(UInt(halfWidth.W))
  })
  assert(!(io.wen && io.ren_full && io.raddr_full === io.waddr), "undefined behavior in dual-ported SRAM")
  assert(!(io.wen && io.ren_half && io.raddr_half(log2Ceil(n), 1) === io.waddr), "undefined behaviour in dual-ported SRAM")
  assert(!(io.ren_half && io.ren_full), "cannot read full and half simultaneously")

  val memLo = SyncReadMem(n, Vec(half_mask_len, mask_elem))
  val memHi = SyncReadMem(n, Vec(half_mask_len, mask_elem))

  // always write full width
  val write_data = io.wdata.asTypeOf(Vec(mask_len, mask_elem))
  val wLo_data = VecInit(write_data.slice(0, half_mask_len))
  val wHi_data = VecInit(write_data.slice(half_mask_len, mask_len))
  val wLo_mask = VecInit(io.mask.slice(0, half_mask_len))
  val wHi_mask = VecInit(io.mask.slice(half_mask_len, mask_len))

  when(io.wen) {
    memLo.write(io.waddr, wLo_data, wLo_mask)
    memHi.write(io.waddr, wHi_data, wHi_mask)
  }

  val readAddr = Mux(io.ren_half, io.raddr_half >> 1, io.raddr_full)(log2Ceil(n) - 1, 0)
  val bankSel = io.raddr_half(0).asBool
  val bankSelResp = RegNext(bankSel)

  val renLo = io.ren_full | (io.ren_half && !bankSel)
  val renHi = io.ren_full | (io.ren_half && bankSel)

  val rLo = memLo.read(readAddr, renLo)
  val rHi = memHi.read(readAddr, renHi)

  io.rdata_full := Cat(rHi.asUInt, rLo.asUInt).asTypeOf(t)
  io.rdata_half := Mux(bankSelResp, rHi.asUInt, rLo.asUInt)
}

class SplitSinglePortSyncMem[T <: Data](n: Int, t: T, splits: Int) extends Module {
  val io = IO(new Bundle {
    val waddr = Input(UInt((log2Ceil(n) max 1).W))
    val raddr = Input(UInt((log2Ceil(n) max 1).W))
    val wdata = Input(t)
    val rdata = Output(t)
    val wen = Input(Bool())
    val ren = Input(Bool())
  })

  val lens = n / splits
  val last_len = n - (splits-1)*lens

  def is_in_range(addr: UInt, i: Int) = {
    if (i == splits-1)
      addr >= (i*lens).U
    else
      addr >= (i*lens).U && addr < ((i+1)*lens).U
  }

  def split_addr(addr: UInt, i: Int) = {
    addr - (i*lens).U
  }

  val srams = Seq.fill(splits-1)(SinglePortSyncMem(lens, t).io) :+ SinglePortSyncMem(last_len, t).io

  val output_split = Reg(UInt((log2Ceil(splits) max 1).W))
  io.rdata := DontCare

  srams.zipWithIndex.foreach { case (sr, i) =>
    sr.addr := Mux(sr.ren, split_addr(io.raddr, i), split_addr(io.waddr, i))
    sr.wdata := io.wdata
    sr.ren := io.ren && is_in_range(io.raddr, i)
    sr.wen := io.wen && is_in_range(io.waddr, i)

    when (sr.ren) {
      output_split := i.U
    }

    // This is an awkward Chisel Vec error workaround
    when (output_split === i.U) {
      io.rdata := sr.rdata
    }
  }
}

object SinglePortSyncMem {
  def apply[T <: Data](n: Int, t: T): SinglePortSyncMem[T] = Module(new SinglePortSyncMem(n, t))
}

object TwoPortSyncMem {
  def apply[T <: Data](n: Int, t: T, mask_len: Int): TwoPortSyncMem[T] = Module(new TwoPortSyncMem(n, t, mask_len))
}

object AsymmetricTwoPortSyncMem {
  def apply[T <: Data](n: Int, t: T, mask_len: Int): AsymmetricTwoPortSyncMem[T] = Module(new AsymmetricTwoPortSyncMem(n, t, mask_len))
}

object SplitSinglePortSyncMem {
  def apply[T <: Data](n: Int, t: T, splits: Int): SplitSinglePortSyncMem[T] = Module(new SplitSinglePortSyncMem(n, t, splits))
}

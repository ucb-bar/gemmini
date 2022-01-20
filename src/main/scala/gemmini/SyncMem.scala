
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

object SplitSinglePortSyncMem {
  def apply[T <: Data](n: Int, t: T, splits: Int): SplitSinglePortSyncMem[T] = Module(new SplitSinglePortSyncMem(n, t, splits))
}

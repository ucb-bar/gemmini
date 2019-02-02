
package systolic
import chisel3._
import chisel3.util._

class SinglePortedSyncMem[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt((log2Ceil(n) max 1).W))
    val wdata = Input(t)
    val rdata = Output(t)
    val wen = Input(Bool())
    val ren = Input(Bool())
  })

  val mem = SyncReadMem(n, t)

  when (io.wen) {
    mem.write(io.addr, io.wdata)
    io.rdata := DontCare
  }.otherwise {
    io.rdata := mem.read(io.addr, io.ren)
  }
}

object SinglePortedSyncMem {
  def apply[T <: Data](n: Int, t: T): SinglePortedSyncMem[T] = Module(new SinglePortedSyncMem(n, t))
}

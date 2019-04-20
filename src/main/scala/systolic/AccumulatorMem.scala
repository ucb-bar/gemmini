package systolic

import chisel3._
import chisel3.util._

class AccumulatorReadIO[T <: Data: Arithmetic](n: Int, shift_width: Int, rdataType: Vec[Vec[T]]) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val data = Input(rdataType)
  val shift = Output(UInt(shift_width.W))
  val relu = Output(Bool())

  override def cloneType: this.type = new AccumulatorReadIO(n, shift_width, rdataType).asInstanceOf[this.type]
}

class AccumulatorWriteIO[T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]]) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val data = Output(t)
  val acc = Output(Bool())

  override def cloneType: this.type = new AccumulatorWriteIO(n, t).asInstanceOf[this.type]
}

class AccumulatorMemIO [T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]], rdata: Vec[Vec[T]]) extends Bundle {
  val read = Flipped(new AccumulatorReadIO(n, t.head.head.getWidth - rdata.head.head.getWidth, rdata))
  val write = Flipped(new AccumulatorWriteIO(n, t))

  override def cloneType: this.type = new AccumulatorMemIO(n, t, rdata).asInstanceOf[this.type]
}

class AccumulatorMem[T <: Data](n: Int, t: Vec[Vec[T]], rdataType: Vec[Vec[T]])(implicit ev: Arithmetic[T]) extends Module {
  import ev._

  // TODO unify this with TwoPortSyncMemIO
  val io = IO(new AccumulatorMemIO(n, t, rdataType))

  val mem = TwoPortSyncMem(n, t)

  val wdata_buf = RegNext(io.write.data)
  val waddr_buf = RegNext(io.write.addr)
  val acc_buf = RegNext(io.write.acc)
  val w_buf_valid = RegNext(io.write.en)

  val w_sum = VecInit((mem.io.rdata zip wdata_buf).map { case (rv, wv) =>
    VecInit((rv zip wv).map(t => t._1 + t._2))
  })

  mem.io.waddr := waddr_buf
  mem.io.wen := w_buf_valid
  mem.io.wdata := Mux(acc_buf, w_sum, wdata_buf)

  mem.io.raddr := Mux(io.write.en && io.write.acc, io.write.addr, io.read.addr)
  mem.io.ren := io.read.en || (io.write.en && io.write.acc)
  val activated_rdata = VecInit(mem.io.rdata.map(v => VecInit(v.map(e => (Mux(io.read.relu, e.relu, e) >> io.read.shift).clippedToWidthOf(rdataType.head.head)))))
  io.read.data := activated_rdata

  assert(!(io.read.en && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.en && io.write.en && io.read.addr === io.write.addr), "reading from and writing to same address is not supported") // TODO
  assert(!(io.read.en && w_buf_valid && waddr_buf === io.read.addr), "reading from an address immediately after writing to it is not supported") // TODO
}

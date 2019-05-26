package systolic

import chisel3._
import chisel3.util._

class AccumulatorReadIO[T <: Data: Arithmetic](n: Int, shift_width: Int, rdataType: Vec[Vec[T]]) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val data = Input(rdataType)
  val shift = Output(UInt(shift_width.W))
  val relu6_shift = Output(UInt(shift_width.W))
  val act = Output(UInt(2.W))

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
  val read = Flipped(new AccumulatorReadIO(n, log2Ceil(t.head.head.getWidth), rdata))
  val write = Flipped(new AccumulatorWriteIO(n, t))

  override def cloneType: this.type = new AccumulatorMemIO(n, t, rdata).asInstanceOf[this.type]
}

class AccumulatorMem[T <: Data](n: Int, t: Vec[Vec[T]], rdataType: Vec[Vec[T]], mem_pipeline: Int)
                               (implicit ev: Arithmetic[T]) extends Module {
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

  val rdata_buf = ShiftRegister(mem.io.rdata, mem_pipeline) // TODO if mem_pipeline > 1, this is inefficient

  val shift = RegNext(io.read.shift) // TODO should this take mem_pipeline into account?
  val relu6_shift = RegNext(io.read.relu6_shift) // TODO should this take mem_pipeline into account?

  val activated_rdata = VecInit(rdata_buf.map(v => VecInit(v.map { e =>
    val e_clipped = (e >> shift).clippedToWidthOf(rdataType.head.head)
    val e_act = MuxCase(e_clipped, Seq(
      (io.read.act === Activation.RELU) -> e_clipped.relu,
      (io.read.act === Activation.RELU6) -> e_clipped.relu6(relu6_shift)))

    e_act
  })))
  io.read.data := activated_rdata

  // require(mem_pipeline >= 1, "accumulator memory needs at least one pipeline cycle") // TODO
  assert(!(io.read.en && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.en && io.write.en && io.read.addr === io.write.addr), "reading from and writing to same address is not supported") // TODO
  assert(!(io.read.en && w_buf_valid && waddr_buf === io.read.addr), "reading from an address immediately after writing to it is not supported") // TODO
}

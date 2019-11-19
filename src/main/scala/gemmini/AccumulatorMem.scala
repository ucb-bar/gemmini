package gemmini

import chisel3._
import chisel3.util._


class AccumulatorReadReq(val n: Int, val shift_width: Int) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val shift = UInt(shift_width.W)
  val relu6_shift = UInt(shift_width.W)
  val act = UInt(2.W)

  val fromDMA = Bool()
}

class AccumulatorReadResp[T <: Data: Arithmetic](rdataType: Vec[Vec[T]]) extends Bundle {
  val data = rdataType.cloneType
  val fromDMA = Bool()

  override def cloneType: this.type = new AccumulatorReadResp(rdataType.cloneType).asInstanceOf[this.type]
}

class AccumulatorReadIO[T <: Data: Arithmetic](n: Int, shift_width: Int, rdataType: Vec[Vec[T]]) extends Bundle {
  val req = Decoupled(new AccumulatorReadReq(n, shift_width))
  val resp = Flipped(Decoupled(new AccumulatorReadResp(rdataType.cloneType)))

  override def cloneType: this.type = new AccumulatorReadIO(n, shift_width, rdataType.cloneType).asInstanceOf[this.type]
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
  // TODO Do writes in this module work with matrices of size 2? If we try to read from an address right after writing
  // to it, then we might not get the written data. We might need some kind of cooldown counter after addresses in the
  // accumulator have been written to for configurations with such small matrices

  // TODO Refuse a read from an address which has only just been written to

  import ev._

  // TODO unify this with TwoPortSyncMemIO
  val io = IO(new AccumulatorMemIO(n, t, rdataType))

  val mem = TwoPortSyncMem(n, t)

  // For any write operation, we spend 2 cycles reading the existing address out, buffering it in a register, and then
  // accumulating on top of it (if necessary)
  val wdata_buf = ShiftRegister(io.write.data, 2)
  val waddr_buf = ShiftRegister(io.write.addr, 2)
  val acc_buf = ShiftRegister(io.write.acc, 2)
  val w_buf_valid = ShiftRegister(io.write.en, 2)

  val w_sum = VecInit((RegNext(mem.io.rdata) zip wdata_buf).map { case (rv, wv) =>
    VecInit((rv zip wv).map(t => t._1 + t._2))
  })

  mem.io.waddr := waddr_buf
  mem.io.wen := w_buf_valid
  mem.io.wdata := Mux(acc_buf, w_sum, wdata_buf)

  mem.io.raddr := Mux(io.write.en && io.write.acc, io.write.addr, io.read.req.bits.addr)
  mem.io.ren := io.read.req.fire() || (io.write.en && io.write.acc)

  class PipelinedRdataAndActT extends Bundle {
    val data = mem.io.rdata.cloneType
    val shift = io.read.req.bits.shift.cloneType
    val relu6_shift = io.read.req.bits.relu6_shift.cloneType
    val act = io.read.req.bits.act.cloneType
    val fromDMA = io.read.req.bits.fromDMA.cloneType
  }
  
  val q = Module(new Queue(new PipelinedRdataAndActT, 1, true, true))
  q.io.enq.bits.data := mem.io.rdata
  q.io.enq.bits.shift := RegNext(io.read.req.bits.shift)
  q.io.enq.bits.relu6_shift := RegNext(io.read.req.bits.relu6_shift)
  q.io.enq.bits.act := RegNext(io.read.req.bits.act)
  q.io.enq.bits.fromDMA := RegNext(io.read.req.bits.fromDMA)
  q.io.enq.valid := RegNext(io.read.req.fire())

  val p = Pipeline(q.io.deq, mem_pipeline, Seq.fill(mem_pipeline)((x: PipelinedRdataAndActT) => x) :+ {
    x: PipelinedRdataAndActT =>
      val activated_rdata = VecInit(x.data.map(v => VecInit(v.map { e =>
        val e_clipped = (e >> x.shift).clippedToWidthOf(rdataType.head.head)
        val e_act = MuxCase(e_clipped, Seq(
          (x.act === Activation.RELU) -> e_clipped.relu,
          (x.act === Activation.RELU6) -> e_clipped.relu6(x.relu6_shift)))

        e_act
      })))

      val result = WireInit(x)
      result.data := activated_rdata

      result
  })

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  io.read.req.ready := q_will_be_empty && (
      // Make sure we aren't accumulating, which would take over both ports
      !(io.write.en && io.write.acc) &&
      // Make sure we aren't reading something that is still being written
      !(RegNext(io.write.en) && RegNext(io.write.addr) === io.read.req.bits.addr) &&
      !(w_buf_valid && waddr_buf === io.read.req.bits.addr)
    )
  io.read.resp.bits.data := p.bits.data
  io.read.resp.bits.fromDMA := p.bits.fromDMA
  io.read.resp.valid := p.valid
  p.ready := io.read.resp.ready

  val read_write_same_address_1 = !(io.read.req.fire() && io.write.en && io.read.req.bits.addr === io.write.addr)
  val read_write_same_address_2 = !(io.read.req.fire() && w_buf_valid && waddr_buf === io.read.req.bits.addr)

  // assert(!(io.read.req.valid && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.req.fire() && io.write.en && io.read.req.bits.addr === io.write.addr), "reading from and writing to same address is not supported")
  assert(!(io.read.req.fire() && w_buf_valid && waddr_buf === io.read.req.bits.addr), "reading from an address immediately after writing to it is not supported")
}

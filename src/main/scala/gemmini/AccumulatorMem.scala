package gemmini

import chisel3._
import chisel3.util._

import Util._

class AccumulatorReadReq[T <: Data](n: Int, shift_width: Int, scale_t: T) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val scale = scale_t
  val relu6_shift = UInt(shift_width.W)
  val act = UInt(2.W)
  val full = Bool() // Whether or not we return the full bitwidth output

  val fromDMA = Bool()

  override def cloneType: this.type = new AccumulatorReadReq(n, shift_width, scale_t.cloneType).asInstanceOf[this.type]
}

class AccumulatorReadResp[T <: Data: Arithmetic](rdataType: Vec[Vec[T]], fullDataType: Vec[Vec[T]]) extends Bundle {
  val data = rdataType.cloneType
  val full_data = fullDataType.cloneType
  val fromDMA = Bool()

  override def cloneType: this.type = new AccumulatorReadResp(rdataType.cloneType, fullDataType.cloneType).asInstanceOf[this.type]
}

class AccumulatorReadIO[T <: Data: Arithmetic, U <: Data](n: Int, shift_width: Int, rdataType: Vec[Vec[T]], fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val req = Decoupled(new AccumulatorReadReq(n, shift_width, scale_t))
  val resp = Flipped(Decoupled(new AccumulatorReadResp(rdataType.cloneType, fullDataType.cloneType)))

  override def cloneType: this.type = new AccumulatorReadIO(n, shift_width, rdataType.cloneType, fullDataType.cloneType, scale_t.cloneType).asInstanceOf[this.type]
}

class AccumulatorWriteReq[T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]]) extends Bundle {
  val addr = UInt(log2Up(n).W)
  val data = t.cloneType
  val acc = Bool()
  val mask = Vec(t.getWidth / 8, Bool()) // TODO Use aligned_to here
  // val current_waddr = Flipped(Valid(UInt(log2Ceil(n).W))) // This is the raddr that is being fed into the SRAM right now

  override def cloneType: this.type = new AccumulatorWriteReq(n, t).asInstanceOf[this.type]
}

class AccumulatorMemIO [T <: Data: Arithmetic, U <: Data](n: Int, t: Vec[Vec[T]], rdata: Vec[Vec[T]], scale_t: U) extends Bundle {
  val read = Flipped(new AccumulatorReadIO(n, log2Ceil(t.head.head.getWidth), rdata, t, scale_t))
  // val write = Flipped(new AccumulatorWriteIO(n, t))
  val write = Flipped(Decoupled(new AccumulatorWriteReq(n, t)))

  override def cloneType: this.type = new AccumulatorMemIO(n, t, rdata, scale_t).asInstanceOf[this.type]
}

class AccumulatorMem[T <: Data, U <: Data](n: Int, t: Vec[Vec[T]], rdataType: Vec[Vec[T]], mem_pipeline: Int, scale_args: ScaleArguments[T, U], read_small_data: Boolean, read_full_data: Boolean, num_scale_units: Int)
                               (implicit ev: Arithmetic[T]) extends Module {
  // TODO Do writes in this module work with matrices of size 2? If we try to read from an address right after writing
  // to it, then we might not get the written data. We might need some kind of cooldown counter after addresses in the
  // accumulator have been written to for configurations with such small matrices

  // TODO Refuse a read from an address which has only just been written to

  // TODO make a new aligned_to variable specifically for AccumulatorMem. We should assume that inputs are at least
  // accType.getWidth/8 aligned, because it won't make sense to do matrix additions directly in the DMA otherwise.

  import ev._

  // TODO unify this with TwoPortSyncMemIO
  val io = IO(new AccumulatorMemIO(n, t, rdataType, scale_args.multiplicand_t))

  val mem = TwoPortSyncMem(n, t, t.getWidth / 8) // TODO We assume byte-alignment here. Use aligned_to instead

  // For any write operation, we spend 2 cycles reading the existing address out, buffering it in a register, and then
  // accumulating on top of it (if necessary)
  val wdata_buf = ShiftRegister(io.write.bits.data, 2)
  val waddr_buf = ShiftRegister(io.write.bits.addr, 2)
  val acc_buf = ShiftRegister(io.write.bits.acc, 2)
  val mask_buf = ShiftRegister(io.write.bits.mask, 2)
  val w_buf_valid = ShiftRegister(io.write.fire(), 2)

  val w_sum = VecInit((RegNext(mem.io.rdata) zip wdata_buf).map { case (rv, wv) =>
    VecInit((rv zip wv).map(t => t._1 + t._2))
  })

  mem.io.waddr := waddr_buf
  mem.io.wen := w_buf_valid
  mem.io.wdata := Mux(acc_buf, w_sum, wdata_buf)
  mem.io.mask := mask_buf

  mem.io.raddr := Mux(io.write.fire() && io.write.bits.acc, io.write.bits.addr, io.read.req.bits.addr)
  mem.io.ren := io.read.req.fire() || (io.write.fire() && io.write.bits.acc)

  class PipelinedRdataAndActT extends Bundle {
    val data = mem.io.rdata.cloneType
    val full_data = mem.io.rdata.cloneType
    val scale = io.read.req.bits.scale.cloneType
    val relu6_shift = io.read.req.bits.relu6_shift.cloneType
    val act = io.read.req.bits.act.cloneType
    val fromDMA = io.read.req.bits.fromDMA.cloneType
  }

  val q = Module(new Queue(new PipelinedRdataAndActT, 1, true, true))
  q.io.enq.bits.data := mem.io.rdata
  q.io.enq.bits.full_data := mem.io.rdata
  q.io.enq.bits.scale := RegNext(io.read.req.bits.scale)
  q.io.enq.bits.relu6_shift := RegNext(io.read.req.bits.relu6_shift)
  q.io.enq.bits.act := RegNext(io.read.req.bits.act)
  q.io.enq.bits.fromDMA := RegNext(io.read.req.bits.fromDMA)
  q.io.enq.valid := RegNext(io.read.req.fire())

  class ScaleModule extends Module {
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(new PipelinedRdataAndActT))
      val out = Decoupled(new PipelinedRdataAndActT)
    })

    if (num_scale_units == -1) {
      io.out <> Pipeline(io.in, mem_pipeline, Seq.fill(mem_pipeline)((x: PipelinedRdataAndActT) => x) :+ {
        x: PipelinedRdataAndActT =>
        val activated_rdata = VecInit(x.data.map(v => VecInit(v.map { e =>
          // val e_scaled = e >> x.shift
          val e_scaled = scale_args.scale_func(e, x.scale)
          val e_clipped = e_scaled.clippedToWidthOf(rdataType.head.head)
          val e_act = MuxCase(e_clipped, Seq(
            (x.act === Activation.RELU) -> e_clipped.relu,
            (x.act === Activation.RELU6) -> e_clipped.relu6(x.relu6_shift)))

          e_act
        })))
        val result = WireInit(x)
        result.data := activated_rdata

        result
      })
    } else {
      val width = io.in.bits.data.size * io.in.bits.data(0).size
      val nEntries = 3

      val regs = Reg(Vec(nEntries, Valid(new PipelinedRdataAndActT)))
      val fired_masks = Reg(Vec(nEntries, Vec(width, Bool())))
      val completed_masks = Reg(Vec(nEntries, Vec(width, Bool())))

      val outArb = Module(new RRArbiter(new PipelinedRdataAndActT, nEntries))
      for (i <- 0 until nEntries) {
        outArb.io.in(i).valid := regs(i).valid && completed_masks(i).reduce(_&&_)
        outArb.io.in(i).bits  := regs(i).bits
        when (outArb.io.in(i).fire()) { regs(i).valid := false.B }
      }
      io.out <> outArb.io.out

      io.in.ready := !(regs.map(_.valid).reduce(_&&_)) || io.out.fire()
      when (io.in.fire()) {
        var allocated = false.B
        for (i <- 0 until nEntries) {
          when (!allocated && (!regs(i).valid || outArb.io.in(i).fire())) {
            regs(i).valid := true.B
            regs(i).bits  := io.in.bits
            fired_masks(i).foreach(_ := false.B)
            completed_masks(i).foreach(_ := false.B)
          }
          allocated = allocated || !regs(i).valid || outArb.io.in(i).fire()
        }
      }



      class DataWithIndex extends Bundle {
        val scale = io.in.bits.scale.cloneType
        val act = io.in.bits.act.cloneType
        val relu6_shift = io.in.bits.relu6_shift.cloneType
        val data = io.in.bits.data(0)(0).cloneType
        val id = UInt(2.W) // TODO hardcoded
        val index = UInt()
      }
      val inputs = Seq.fill(width*nEntries) { Wire(Decoupled(new DataWithIndex)) }

      for (i <- 0 until nEntries) {
        for (w <- 0 until width) {
          val input = inputs(i*width+w)
          input.valid       := regs(i).valid && !fired_masks(i)(w)
          input.bits.data   := regs(i).bits.data(w / io.in.bits.data(0).size)(w % io.in.bits.data(0).size)
          input.bits.scale  := regs(i).bits.scale
          input.bits.act    := regs(i).bits.act
          input.bits.relu6_shift := regs(i).bits.relu6_shift
          input.bits.id := i.U
          input.bits.index := w.U
          when (input.fire()) {
            fired_masks(i)(w) := true.B
          }
        }
      }
      for (i <- 0 until num_scale_units) {
        val arbIn = inputs.zipWithIndex.filter({ case (_, w) => w % num_scale_units == i }).map(_._1)
        val arb = Module(new RRArbiter(new DataWithIndex, arbIn.length))
        arb.io.in <> arbIn
        arb.io.out.ready := true.B
        val arbOut = arb.io.out
        val e_scaled = scale_args.scale_func(arbOut.bits.data, arbOut.bits.scale)
        val e_clipped = e_scaled.clippedToWidthOf(rdataType.head.head)
        val e_act = MuxCase(e_clipped, Seq(
          (arbOut.bits.act === Activation.RELU) -> e_clipped.relu,
          (arbOut.bits.act === Activation.RELU6) -> e_clipped.relu6(arbOut.bits.relu6_shift)
        ))
        val pipe_in = Wire(Valid(new DataWithIndex))
        pipe_in.valid := arbOut.valid
        pipe_in.bits  := arbOut.bits
        pipe_in.bits.data := e_act
        val pipe_out = Pipe(pipe_in, mem_pipeline)
        for (j <- 0 until nEntries) {
          for (w <- 0 until width) {
            if ((j*width+w) % num_scale_units == i) {
              when (pipe_out.fire() && pipe_out.bits.id === j.U && pipe_out.bits.index === w.U) {
                regs(j).bits.data(w / io.in.bits.data(0).size)(w % io.in.bits.data(0).size) := pipe_out.bits.data
                completed_masks(j)(w) := true.B
              }
            }
          }
        }
      }
      when (reset.asBool) {
        regs.foreach(_.valid := false.B)
      }
    }
  }
  val scale_module = Module(new ScaleModule)
  scale_module.io.in <> q.io.deq
  val p = scale_module.io.out

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  io.read.req.ready := q_will_be_empty && (
      // Make sure we aren't accumulating, which would take over both ports
      !(io.write.fire() && io.write.bits.acc) &&
      // Make sure we aren't reading something that is still being written
      !(RegNext(io.write.fire()) && RegNext(io.write.bits.addr) === io.read.req.bits.addr) &&
      !(w_buf_valid && waddr_buf === io.read.req.bits.addr)
    )
  io.read.resp.bits.data := p.bits.data
  io.read.resp.bits.full_data := p.bits.full_data
  io.read.resp.bits.fromDMA := p.bits.fromDMA
  io.read.resp.valid := p.valid
  p.ready := io.read.resp.ready

  if (read_small_data)
    io.read.resp.bits.data := p.bits.data
  else
    io.read.resp.bits.data := 0.U.asTypeOf(p.bits.data) // TODO make this DontCare instead

  if (read_full_data)
    io.read.resp.bits.full_data := p.bits.full_data
  else
    io.read.resp.bits.full_data := 0.U.asTypeOf(q.io.enq.bits.full_data) // TODO make this DontCare instead

  // io.write.current_waddr.valid := mem.io.wen
  // io.write.current_waddr.bits := mem.io.waddr
  io.write.ready := !io.write.bits.acc || (!(io.write.bits.addr === mem.io.waddr && mem.io.wen) &&
    !(io.write.bits.addr === RegNext(io.write.bits.addr) && RegNext(io.write.fire())))

  // assert(!(io.read.req.valid && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.req.fire() && io.write.fire() && io.read.req.bits.addr === io.write.bits.addr), "reading from and writing to same address is not supported")
  assert(!(io.read.req.fire() && w_buf_valid && waddr_buf === io.read.req.bits.addr), "reading from an address immediately after writing to it is not supported")
}

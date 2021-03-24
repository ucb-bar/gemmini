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

class AccumulatorReadResp[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U, shift_width: Int) extends Bundle {
  val data = fullDataType.cloneType
  val fromDMA = Bool()
  val scale = scale_t.cloneType
  val relu6_shift = UInt(shift_width.W)
  val act = UInt(2.W)
  val acc_bank_id = UInt(2.W) // TODO don't hardcode
  override def cloneType: this.type = new AccumulatorReadResp(fullDataType.cloneType, scale_t, shift_width).asInstanceOf[this.type]
}

class AccumulatorReadIO[T <: Data: Arithmetic, U <: Data](n: Int, shift_width: Int, fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val req = Decoupled(new AccumulatorReadReq[U](n, shift_width, scale_t))
  val resp = Flipped(Decoupled(new AccumulatorReadResp[T, U](fullDataType, scale_t, shift_width)))

  override def cloneType: this.type = new AccumulatorReadIO(n, shift_width, fullDataType.cloneType, scale_t.cloneType).asInstanceOf[this.type]
}

class AccumulatorWriteReq[T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]]) extends Bundle {
  val addr = UInt(log2Up(n).W)
  val data = t.cloneType
  val acc = Bool()
  val mask = Vec(t.getWidth / 8, Bool()) // TODO Use aligned_to here
  // val current_waddr = Flipped(Valid(UInt(log2Ceil(n).W))) // This is the raddr that is being fed into the SRAM right now

  override def cloneType: this.type = new AccumulatorWriteReq(n, t).asInstanceOf[this.type]
}

class AccumulatorMemIO [T <: Data: Arithmetic, U <: Data](n: Int, t: Vec[Vec[T]], scale_t: U) extends Bundle {
  val read = Flipped(new AccumulatorReadIO(n, log2Ceil(t.head.head.getWidth), t, scale_t))
  // val write = Flipped(new AccumulatorWriteIO(n, t))
  val write = Flipped(Decoupled(new AccumulatorWriteReq(n, t)))

  override def cloneType: this.type = new AccumulatorMemIO(n, t, scale_t).asInstanceOf[this.type]
}

class AccumulatorMem[T <: Data, U <: Data](
  n: Int, t: Vec[Vec[T]], scale_args: ScaleArguments[T, U],
  acc_singleported: Boolean, num_acc_sub_banks: Int
)
  (implicit ev: Arithmetic[T]) extends Module {
  // TODO Do writes in this module work with matrices of size 2? If we try to read from an address right after writing
  // to it, then we might not get the written data. We might need some kind of cooldown counter after addresses in the
  // accumulator have been written to for configurations with such small matrices

  // TODO Refuse a read from an address which has only just been written to

  // TODO make a new aligned_to variable specifically for AccumulatorMem. We should assume that inputs are at least
  // accType.getWidth/8 aligned, because it won't make sense to do matrix additions directly in the DMA otherwise.

  import ev._

  // TODO unify this with TwoPortSyncMemIO
  val io = IO(new AccumulatorMemIO(n, t, scale_args.multiplicand_t))


  // For any write operation, we spend 2 cycles reading the existing address out, buffering it in a register, and then
  // accumulating on top of it (if necessary)
  val wdata_buf = ShiftRegister(io.write.bits.data, 2)
  val waddr_buf = ShiftRegister(io.write.bits.addr, 2)
  val acc_buf = ShiftRegister(io.write.bits.acc, 2)
  val mask_buf = ShiftRegister(io.write.bits.mask, 2)
  val w_buf_valid = ShiftRegister(io.write.fire(), 2)
  val acc_rdata = Wire(t)
  acc_rdata := DontCare
  val read_rdata = Wire(t)
  read_rdata := DontCare
  val block_read_req = WireInit(false.B)
  val w_sum = VecInit((RegNext(acc_rdata) zip wdata_buf).map { case (rv, wv) =>
    VecInit((rv zip wv).map(t => t._1 + t._2))
  })

  if (!acc_singleported) {
    val mem = TwoPortSyncMem(n, t, t.getWidth / 8) // TODO We assume byte-alignment here. Use aligned_to instead
    mem.io.waddr := waddr_buf
    mem.io.wen := w_buf_valid
    mem.io.wdata := Mux(acc_buf, w_sum, wdata_buf)
    mem.io.mask := mask_buf
    acc_rdata := mem.io.rdata
    read_rdata := mem.io.rdata
    mem.io.raddr := Mux(io.write.fire() && io.write.bits.acc, io.write.bits.addr, io.read.req.bits.addr)
    mem.io.ren := io.read.req.fire() || (io.write.fire() && io.write.bits.acc)
  } else {
    val mask_len = t.getWidth / 8
    val mask_elem = UInt((t.getWidth / mask_len).W)
    val reads = Wire(Vec(2, Decoupled(UInt())))
    reads(0).valid := io.write.valid && io.write.bits.acc
    reads(0).bits  := io.write.bits.addr
    reads(0).ready := true.B
    reads(1).valid := io.read.req.valid
    reads(1).bits  := io.read.req.bits.addr
    reads(1).ready := true.B
    block_read_req := !reads(1).ready
    for (i <- 0 until num_acc_sub_banks) {
      def isThisBank(addr: UInt) = addr(log2Ceil(num_acc_sub_banks)-1,0) === i.U
      def getBankIdx(addr: UInt) = addr >> log2Ceil(num_acc_sub_banks)
      val mem = SyncReadMem(n / num_acc_sub_banks, Vec(mask_len, mask_elem))

      val ren = WireInit(false.B)
      val raddr = WireInit(getBankIdx(reads(0).bits))
      val nEntries = 3
      // Writes coming 2 cycles after read leads to bad bank behavior
      // Add another buffer here
      class W_Q_Entry[T <: Data](mask_len: Int, mask_elem: T) extends Bundle {
        val valid = Bool()
        val data = Vec(mask_len, mask_elem)
        val mask = Vec(mask_len, Bool())
        val addr = UInt(log2Ceil(n/num_acc_sub_banks).W)
        override def cloneType: this.type = new W_Q_Entry(mask_len, mask_elem).asInstanceOf[this.type]
      }
      val w_q = Reg(Vec(nEntries, new W_Q_Entry(mask_len, mask_elem)))
      for (e <- w_q) {
        when (e.valid) {
          assert(!(
            io.write.valid && io.write.bits.acc &&
            isThisBank(io.write.bits.addr) && getBankIdx(io.write.bits.addr) === e.addr &&
            ((io.write.bits.mask.asUInt & e.mask.asUInt) =/= 0.U)
          ))
          when (io.read.req.valid && isThisBank(io.read.req.bits.addr) && getBankIdx(io.read.req.bits.addr) === e.addr) {
            reads(1).ready := false.B
          }
        }
      }
      val w_q_head = RegInit(1.U(nEntries.W))
      val w_q_tail = RegInit(1.U(nEntries.W))
      when (reset.asBool) {
        w_q.foreach(_.valid := false.B)
      }
      val wen = WireInit(false.B)
      val wdata = Mux1H(w_q_head.asBools, w_q.map(_.data))
      val wmask = Mux1H(w_q_head.asBools, w_q.map(_.mask))
      val waddr = Mux1H(w_q_head.asBools, w_q.map(_.addr))
      when (wen) {
        w_q_head := w_q_head << 1 | w_q_head(nEntries-1)
        for (i <- 0 until nEntries) {
          when (w_q_head(i)) {
            w_q(i).valid := false.B
          }
        }
      }

      when (w_buf_valid && isThisBank(waddr_buf)) {
        assert(!((w_q_tail.asBools zip w_q.map(_.valid)).map({ case (h,v) => h && v }).reduce(_||_)))
        w_q_tail := w_q_tail << 1 | w_q_tail(nEntries-1)
        for (i <- 0 until nEntries) {
          when (w_q_tail(i)) {
            w_q(i).valid := true.B
            w_q(i).data  := Mux(acc_buf, w_sum, wdata_buf).asTypeOf(Vec(mask_len, mask_elem))
            w_q(i).mask  := mask_buf
            w_q(i).addr  := getBankIdx(waddr_buf)
          }
        }

      }
      val bank_rdata = mem.read(raddr, ren && !wen).asTypeOf(t)
      when (RegNext(ren && reads(0).valid && isThisBank(reads(0).bits))) {
        acc_rdata := bank_rdata
      } .elsewhen (RegNext(ren)) {
        read_rdata := bank_rdata
      }
      when (wen) {
        mem.write(waddr, wdata, wmask)
      }
      // Three requestors, 1 slot
      // Priority is incoming reads for RMW > writes from RMW > incoming reads
      when (reads(0).valid && isThisBank(reads(0).bits)) {
        ren := true.B
        when (isThisBank(reads(1).bits)) {
          reads(1).ready := false.B
        }
      } .elsewhen ((w_q_head.asBools zip w_q.map(_.valid)).map({ case (h,v) => h && v }).reduce(_||_)) {
        wen := true.B
        when (isThisBank(reads(1).bits)) {
          reads(1).ready := false.B
        }
      } .otherwise {
        ren := isThisBank(reads(1).bits)
        raddr := getBankIdx(reads(1).bits)
      }
    }
  }

  val q = Module(new Queue(new AccumulatorReadResp(t, scale_args.multiplicand_t, log2Ceil(t.head.head.getWidth)),  1, true, true))
  q.io.enq.bits.data := read_rdata
  q.io.enq.bits.scale := RegNext(io.read.req.bits.scale)
  q.io.enq.bits.relu6_shift := RegNext(io.read.req.bits.relu6_shift)
  q.io.enq.bits.act := RegNext(io.read.req.bits.act)
  q.io.enq.bits.fromDMA := RegNext(io.read.req.bits.fromDMA)
  q.io.enq.bits.acc_bank_id := DontCare
  q.io.enq.valid := RegNext(io.read.req.fire())

  val p = q.io.deq

  io.read.resp.bits.data := p.bits.data
  io.read.resp.bits.fromDMA := p.bits.fromDMA
  io.read.resp.bits.relu6_shift := p.bits.relu6_shift
  io.read.resp.bits.act := p.bits.act
  io.read.resp.bits.scale := p.bits.scale
  io.read.resp.bits.acc_bank_id := DontCare // This is set in Scratchpad
  io.read.resp.valid := p.valid
  p.ready := io.read.resp.ready

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  io.read.req.ready := q_will_be_empty && (
      // Make sure we aren't accumulating, which would take over both ports
      !(io.write.fire() && io.write.bits.acc) &&
      // Make sure we aren't reading something that is still being written
      !(RegNext(io.write.fire()) && RegNext(io.write.bits.addr) === io.read.req.bits.addr) &&
      !(w_buf_valid && waddr_buf === io.read.req.bits.addr) &&
      !block_read_req
  )

  // io.write.current_waddr.valid := mem.io.wen
  // io.write.current_waddr.bits := mem.io.waddr
  io.write.ready := !io.write.bits.acc || (!(io.write.bits.addr === waddr_buf && w_buf_valid) &&
    !(io.write.bits.addr === RegNext(io.write.bits.addr) && RegNext(io.write.fire())))

  // assert(!(io.read.req.valid && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.req.fire() && io.write.fire() && io.read.req.bits.addr === io.write.bits.addr), "reading from and writing to same address is not supported")
  assert(!(io.read.req.fire() && w_buf_valid && waddr_buf === io.read.req.bits.addr), "reading from an address immediately after writing to it is not supported")
}

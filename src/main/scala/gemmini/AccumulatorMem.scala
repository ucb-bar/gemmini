package gemmini

import chisel3._
import chisel3.util._

import Util._

class AccumulatorReadReq[T <: Data](n: Int, shift_width: Int, scale_t: T) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val scale = scale_t
  val relu6_shift = UInt(shift_width.W)
  val act = UInt(2.W) // TODO magic number
  val full = Bool() // Whether or not we return the full bitwidth output

  val fromDMA = Bool()

}

class AccumulatorReadResp[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U, shift_width: Int) extends Bundle {
  val data = fullDataType.cloneType
  val fromDMA = Bool()
  val scale = scale_t.cloneType
  val relu6_shift = UInt(shift_width.W)
  val act = UInt(2.W) // TODO magic number
  val acc_bank_id = UInt(2.W) // TODO don't hardcode
}

class AccumulatorReadIO[T <: Data: Arithmetic, U <: Data](n: Int, shift_width: Int, fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val req = Decoupled(new AccumulatorReadReq[U](n, shift_width, scale_t))
  val resp = Flipped(Decoupled(new AccumulatorReadResp[T, U](fullDataType, scale_t, shift_width)))

}

class AccumulatorWriteReq[T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]]) extends Bundle {
  val addr = UInt(log2Up(n).W)
  val data = t.cloneType
  val acc = Bool()
  val mask = Vec(t.getWidth / 8, Bool()) // TODO Use aligned_to here
  // val current_waddr = Flipped(Valid(UInt(log2Ceil(n).W))) // This is the raddr that is being fed into the SRAM right now

}


class AccumulatorMemIO [T <: Data: Arithmetic, U <: Data](n: Int, t: Vec[Vec[T]], scale_t: U,
  acc_sub_banks: Int, use_shared_ext_mem: Boolean
) extends Bundle {
  val read = Flipped(new AccumulatorReadIO(n, log2Ceil(t.head.head.getWidth), t, scale_t))
  val write = Flipped(Decoupled(new AccumulatorWriteReq(n, t)))

  val ext_mem = if (use_shared_ext_mem) Some(Vec(acc_sub_banks, new ExtMemIO)) else None

  val adder = new Bundle {
    val valid = Output(Bool())
    val op1 = Output(t.cloneType)
    val op2 = Output(t.cloneType)
    val sum = Input(t.cloneType)
  }

}

class AccPipe[T <: Data : Arithmetic](latency: Int, t: T)(implicit ev: Arithmetic[T]) extends Module {
  val io = IO(new Bundle {
    val op1 = Input(t.cloneType)
    val op2 = Input(t.cloneType)
    val sum = Output(t.cloneType)
  })
  import ev._
  io.sum := ShiftRegister(io.op1 + io.op2, latency)
}

class AccPipeShared[T <: Data : Arithmetic](latency: Int, t: Vec[Vec[T]], banks: Int) extends Module {
  val io = IO(new Bundle {
    val in_sel = Input(Vec(banks, Bool()))
    val ina = Input(Vec(banks, t.cloneType))
    val inb = Input(Vec(banks, t.cloneType))
    val out = Output(t.cloneType)
  })
  val ina = Mux1H(io.in_sel, io.ina)
  val inb = Mux1H(io.in_sel, io.inb)
  io.out := VecInit((ina zip inb).map { case (rv, wv) =>
    VecInit((rv zip wv).map { case (re, we) =>
      val m = Module(new AccPipe(latency, t.head.head.cloneType))
      m.io.op1 := re
      m.io.op2 := we
      m.io.sum
    })
  })
}

class AccumulatorMem[T <: Data, U <: Data](
  n: Int, t: Vec[Vec[T]], scale_func: (T, U) => T, scale_t: U,
  acc_singleported: Boolean, acc_sub_banks: Int,
  use_shared_ext_mem: Boolean,
  acc_latency: Int, acc_type: T, is_dummy: Boolean
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
  val io = IO(new AccumulatorMemIO(n, t, scale_t, acc_sub_banks, use_shared_ext_mem))

  require (acc_latency >= 2)

  val pipelined_writes = Reg(Vec(acc_latency, Valid(new AccumulatorWriteReq(n, t))))
  val oldest_pipelined_write = pipelined_writes(acc_latency-1)
  pipelined_writes(0).valid := io.write.fire
  pipelined_writes(0).bits  := io.write.bits
  for (i <- 1 until acc_latency) {
    pipelined_writes(i) := pipelined_writes(i-1)
  }

  val rdata_for_adder = Wire(t)
  rdata_for_adder := DontCare
  val rdata_for_read_resp = Wire(t)
  rdata_for_read_resp := DontCare

  val adder_sum = io.adder.sum
  io.adder.valid := pipelined_writes(0).valid && pipelined_writes(0).bits.acc
  io.adder.op1 := rdata_for_adder
  io.adder.op2 := pipelined_writes(0).bits.data

  val block_read_req = WireInit(false.B)
  val block_write_req = WireInit(false.B)

  val mask_len = t.getWidth / 8
  val mask_elem = UInt((t.getWidth / mask_len).W)

  if (!acc_singleported && !is_dummy) {
    require(!use_shared_ext_mem)
    val mem = TwoPortSyncMem(n, t, mask_len) // TODO We assume byte-alignment here. Use aligned_to instead
    mem.io.waddr := oldest_pipelined_write.bits.addr
    mem.io.wen := oldest_pipelined_write.valid
    mem.io.wdata := Mux(oldest_pipelined_write.bits.acc, adder_sum, oldest_pipelined_write.bits.data)
    mem.io.mask := oldest_pipelined_write.bits.mask
    rdata_for_adder := mem.io.rdata
    rdata_for_read_resp := mem.io.rdata
    mem.io.raddr := Mux(io.write.fire && io.write.bits.acc, io.write.bits.addr, io.read.req.bits.addr)
    mem.io.ren := io.read.req.fire || (io.write.fire && io.write.bits.acc)
  } else if (!is_dummy) {
    val rmw_req = Wire(Decoupled(UInt()))
    rmw_req.valid := io.write.valid && io.write.bits.acc
    rmw_req.bits := io.write.bits.addr
    rmw_req.ready := true.B

    block_write_req := !rmw_req.ready

    val only_read_req = Wire(Decoupled(UInt()))
    only_read_req.valid := io.read.req.valid
    only_read_req.bits := io.read.req.bits.addr
    only_read_req.ready := true.B

    block_read_req := !only_read_req.ready

    for (i <- 0 until acc_sub_banks) {
      def isThisBank(addr: UInt) = addr(log2Ceil(acc_sub_banks)-1,0) === i.U
      def getBankIdx(addr: UInt) = addr >> log2Ceil(acc_sub_banks)
      val (read, write) = if (use_shared_ext_mem) {
        def read(addr: UInt, ren: Bool): Data = {
          io.ext_mem.get(i).read_en := ren
          io.ext_mem.get(i).read_addr := addr
          io.ext_mem.get(i).read_data
        }
        io.ext_mem.get(i).write_en := false.B
        io.ext_mem.get(i).write_addr := DontCare
        io.ext_mem.get(i).write_data := DontCare
        io.ext_mem.get(i).write_mask := DontCare
        def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = {
          io.ext_mem.get(i).write_en := true.B
          io.ext_mem.get(i).write_addr := addr
          io.ext_mem.get(i).write_data := wdata.asUInt
          io.ext_mem.get(i).write_mask := wmask.asUInt
        }
        (read _, write _)
      } else {
        val mem = SyncReadMem(n / acc_sub_banks, Vec(mask_len, mask_elem))
        def read(addr: UInt, ren: Bool): Data = mem.read(addr, ren)
        def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = mem.write(addr, wdata, wmask)
        (read _, write _)
      }

      val ren = WireInit(false.B)
      val raddr = WireInit(getBankIdx(rmw_req.bits))
      val nEntries = 3

      // Writes coming 2 cycles after read leads to bad bank behavior
      // Add another buffer here
      class W_Q_Entry[T <: Data](mask_len: Int, mask_elem: T) extends Bundle {
        val valid = Bool()
        val data = Vec(mask_len, mask_elem)
        val mask = Vec(mask_len, Bool())
        val addr = UInt(log2Ceil(n/acc_sub_banks).W)
      }

      val w_q = Reg(Vec(nEntries, new W_Q_Entry(mask_len, mask_elem)))
      for (e <- w_q) {
        when (e.valid) {
          assert(!(
            io.write.fire && io.write.bits.acc &&
            isThisBank(io.write.bits.addr) && getBankIdx(io.write.bits.addr) === e.addr &&
            ((io.write.bits.mask.asUInt & e.mask.asUInt) =/= 0.U)
          ), "you cannot accumulate to an AccumulatorMem address until previous writes to that address have completed")

          when (io.write.bits.acc && isThisBank(io.write.bits.addr) && getBankIdx(io.write.bits.addr) === e.addr) {
            rmw_req.ready := false.B
          }

          when (isThisBank(io.read.req.bits.addr) && getBankIdx(io.read.req.bits.addr) === e.addr) {
            only_read_req.ready := false.B
          }
        }
      }

      val w_q_head = RegInit(1.U(nEntries.W))
      val w_q_tail = RegInit(1.U(nEntries.W))

      val w_q_full = (w_q_tail.asBools zip w_q.map(_.valid)).map({ case (h,v) => h && v }).reduce(_||_)
      val w_q_empty = !(w_q_head.asBools zip w_q.map(_.valid)).map({ case (h,v) => h && v }).reduce(_||_)

      val wen = WireInit(false.B)
      val wdata = Mux1H(w_q_head.asBools, w_q.map(_.data))
      val wmask = Mux1H(w_q_head.asBools, w_q.map(_.mask))
      val waddr = Mux1H(w_q_head.asBools, w_q.map(_.addr))
      when (wen) {
        w_q_head := (w_q_head << 1).asUInt() | w_q_head(nEntries-1)
        for (i <- 0 until nEntries) {
          when (w_q_head(i)) {
            w_q(i).valid := false.B
          }
        }
      }

      val w_q_push = oldest_pipelined_write.valid && isThisBank(oldest_pipelined_write.bits.addr)

      when (w_q_push) {
        assert(!w_q_full || wen, "we ran out of acc-sub-bank write q entries")

        w_q_tail := (w_q_tail << 1).asUInt() | w_q_tail(nEntries-1)
        for (i <- 0 until nEntries) {
          when (w_q_tail(i)) {
            w_q(i).valid := true.B
            w_q(i).data  := Mux(oldest_pipelined_write.bits.acc, adder_sum, oldest_pipelined_write.bits.data).asTypeOf(Vec(mask_len, mask_elem))
            w_q(i).mask  := oldest_pipelined_write.bits.mask
            w_q(i).addr  := getBankIdx(oldest_pipelined_write.bits.addr)
          }
        }
      }

      val bank_rdata = read(raddr, ren && !wen).asTypeOf(t)
      when (RegNext(ren && rmw_req.valid && isThisBank(rmw_req.bits))) {
        rdata_for_adder := bank_rdata
      } .elsewhen (RegNext(ren)) {
        rdata_for_read_resp := bank_rdata
      }

      when (wen) {
        write(waddr, wdata, wmask)
      }

      // Three requestors, 1 slot
      // Priority is (in descending order):
      //   1. incoming reads for RMW
      //   2. writes from RMW
      //   3. incoming reads
      when (rmw_req.fire && isThisBank(rmw_req.bits)) {
        ren := true.B
        when (isThisBank(only_read_req.bits)) {
          only_read_req.ready := false.B
        }
      } .elsewhen (!w_q_empty) {
        wen := true.B
        when (isThisBank(only_read_req.bits)) {
          only_read_req.ready := false.B
        }
      } .otherwise {
        ren := isThisBank(only_read_req.bits) && only_read_req.fire
        raddr := getBankIdx(only_read_req.bits)
      }

      when (reset.asBool) {
        w_q.foreach(_.valid := false.B)
      }
    }
  }

  val q = Module(new Queue(new AccumulatorReadResp(t, scale_t, log2Ceil(t.head.head.getWidth)),  1, true, true))
  q.io.enq.bits.data := rdata_for_read_resp

  if (is_dummy) {
    rdata_for_read_resp := DontCare
    rdata_for_adder := DontCare
  }

  q.io.enq.bits.scale := RegNext(io.read.req.bits.scale)
  q.io.enq.bits.relu6_shift := RegNext(io.read.req.bits.relu6_shift)
  q.io.enq.bits.act := RegNext(io.read.req.bits.act)
  q.io.enq.bits.fromDMA := RegNext(io.read.req.bits.fromDMA)
  q.io.enq.bits.acc_bank_id := DontCare
  q.io.enq.valid := RegNext(io.read.req.fire)

  val p = q.io.deq

  io.read.resp.bits.data := p.bits.data
  io.read.resp.bits.fromDMA := p.bits.fromDMA
  io.read.resp.bits.relu6_shift := p.bits.relu6_shift
  io.read.resp.bits.act := p.bits.act
  io.read.resp.bits.scale := p.bits.scale
  io.read.resp.bits.acc_bank_id := DontCare // This is set in Scratchpad
  io.read.resp.valid := p.valid
  p.ready := io.read.resp.ready

  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U
  io.read.req.ready := q_will_be_empty && (
      // Make sure we aren't accumulating, which would take over both ports
      !(io.write.valid && io.write.bits.acc) &&
      !pipelined_writes.map(r => r.valid && r.bits.addr === io.read.req.bits.addr).reduce(_||_)  &&
      !block_read_req
  )

  io.write.ready := !block_write_req &&
    !pipelined_writes.map(r => r.valid && r.bits.addr === io.write.bits.addr && io.write.bits.acc).reduce(_||_)

  when (reset.asBool()) {
    pipelined_writes.foreach(_.valid := false.B)
  }

  // assert(!(io.read.req.valid && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.req.fire && io.write.fire && io.read.req.bits.addr === io.write.bits.addr), "reading from and writing to same address is not supported")
}

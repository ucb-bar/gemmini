package gemmini

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import Util._

class AccumulatorReadReq[T <: Data: Arithmetic, U <: Data](n: Int, acc_t: T, scale_t: U) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val scale = scale_t
  val igelu_qb = acc_t.cloneType
  val igelu_qc = acc_t.cloneType
  val iexp_qln2 = acc_t.cloneType
  val iexp_qln2_inv = acc_t.cloneType
  val activation_mx_format = UInt(2.W)
  val weight_mx_format = UInt(2.W)
  val act = UInt(Activation.bitwidth.W) // TODO magic number
  val full = Bool() // Whether or not we return the full bitwidth output
  val chunk_id = UInt(GemminiISA.MX_CHUNK_ID_BITS.W)

  val fromDMA = Bool()

}

class AccumulatorReadResp[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val data = fullDataType.cloneType
  val fromDMA = Bool()
  val scale = scale_t.cloneType
  val igelu_qb = fullDataType.head.head.cloneType
  val igelu_qc = fullDataType.head.head.cloneType
  val iexp_qln2 = fullDataType.head.head.cloneType
  val iexp_qln2_inv = fullDataType.head.head.cloneType
  val act = UInt(Activation.bitwidth.W) // TODO magic number
  val acc_bank_id = UInt(2.W) // TODO magic number
  val chunk_id = UInt(GemminiISA.MX_CHUNK_ID_BITS.W)
}

class AccumulatorReadIO[T <: Data: Arithmetic, U <: Data](n: Int, fullDataType: Vec[Vec[T]], scale_t: U, chunk_t: Vec[Vec[T]]) extends Bundle {
  val req = Decoupled(new AccumulatorReadReq[T, U](n, fullDataType.head.head.cloneType, scale_t))
  val resp = Flipped(Decoupled(new AccumulatorReadResp[T, U](chunk_t, scale_t)))
}

class AccumulatorWriteReq[T <: Data: Arithmetic](n: Int, t: Vec[Vec[T]]) extends Bundle {
  val addr = UInt(log2Up(n).W)
  val data = t.cloneType
  val acc = Bool()
  val mask = Vec(t.getWidth / 8, Bool()) // TODO Use aligned_to here
  val offset = UInt(log2Up(t.length*t.head.length).W) // TODO(nicolas): not hardcoded
}


class AccumulatorMemIO [T <: Data: Arithmetic, U <: Data](n: Int, t: Vec[Vec[T]], scale_t: U, chunk_t: Vec[Vec[T]],
  acc_sub_banks: Int, use_shared_ext_mem: Boolean, use_mx_scaling: Boolean, meshRows: Int, tileRows: Int
) extends Bundle {
  val read = Flipped(new AccumulatorReadIO(n, t, scale_t, chunk_t))
  val write = Flipped(Decoupled(new AccumulatorWriteReq(n, t)))

  val ext_mem = if (use_shared_ext_mem) Some(Vec(acc_sub_banks, new ExtMemIO)) else None
  
  val adder = new Bundle {
    val valid = Output(Bool())
    val op1 = Output(t.cloneType)
    val op2 = Output(t.cloneType)
    val sum = Input(t.cloneType)
  }
  // Scaling-factor memory control
  val counter_i = Input(UInt(16.W))
  val counter_j = Input(UInt(16.W))
  val counter_k = Input(UInt(16.W))
  val i = Input(UInt(16.W))
  val j = Input(UInt(16.W))
  val k = Input(UInt(16.W))
  val dataType_out = Input(UInt(2.W)) // output mx format datatype
  val mx_multi_elem = Input(Bool()) // throughput: 2 elements/lane, datatype-independent
  val mx_fp8_altfmt = Input(Bool()) // code0 sub-format: 1 = E5M2 (4-bit LUT output), 0 = E4M3
  val scale_mem_write_act = if (use_mx_scaling) {
    Some(Flipped(Decoupled(new ScalingFactorWriteReq(13, 64))))
  } else None
  val scale_mem_write_w = if (use_mx_scaling) {
    Some(Flipped(Decoupled(new ScalingFactorWriteReq(13, 64))))
  } else None
  val scaleMemCntl = if (use_mx_scaling) {
    Some(Input(new ScalingFactorCntl(meshRows * tileRows)))
  } else None
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
  use_shared_ext_mem: Boolean, use_tl_ext_ram: Boolean,
  acc_latency: Int, acc_type: T, is_dummy: Boolean, use_mx_scaling: Boolean,
  testConfig: Boolean,
  scale_mem: Option[GemminiScalingFactorMemConfig],
  meshRows: Int,
  tileRows: Int,
)
  (implicit ev: Arithmetic[T]) extends Module {
  // TODO Do writes in this module work with matrices of size 2? If we try to read from an address right after writing
  // to it, then we might not get the written data. We might need some kind of cooldown counter after addresses in the
  // accumulator have been written to for configurations with such small matrices

  // TODO make a new aligned_to variable specifically for AccumulatorMem. We should assume that inputs are at least
  // accType.getWidth/8 aligned, because it won't make sense to do matrix additions directly in the DMA otherwise.
  
  import ev._

  val numChunks = (t.length * t.head.length) / 8  // DIM/8; each chunk = 8 acc elems = 512b
  val chunk_t = if (use_mx_scaling) Vec(t.length / numChunks, t.head.cloneType)
                else Vec(t.length, t.head.cloneType)

  // TODO unify this with TwoPortSyncMemIO
  val io = IO(new AccumulatorMemIO(n, t, scale_t, chunk_t, acc_sub_banks, use_shared_ext_mem, use_mx_scaling, meshRows, tileRows))

  val scaleFactorMem = scale_mem.map { conf =>
    Module(new ScalingFactorMem(
      depth = conf.depth,
      sramWidth = conf.subbankLineSizeInBytes*8,
      actOutputScalingWidth = 8,
      numBanks = conf.numBanks,
      testConfig = testConfig,
      meshRows = meshRows,
      tileRows = tileRows
    ))
  }
 
  def calculateScaleAddr(write_addr: UInt): UInt = {
    (write_addr & (~("h_f".U)).asUInt).asUInt  // derive the scale-mem read addr from the accumulator write addr
  }

  def applyE9M0Scale[T <: Data](
    value: T,
    scale_e9m0: UInt,
    expBits: Int,
    mantBits: Int
  )(implicit ev: Arithmetic[T]): T = {

    val valueUInt = value.asUInt(15, 0)
    val totalBits = valueUInt.getWidth
   
    require(totalBits == 1 + expBits + mantBits,
      s"value width = $totalBits, but 1(sign)+$expBits(exp)+$mantBits(mant) != total")

    val sign     = valueUInt(totalBits - 1)
    val expHigh  = totalBits - 2
    val expLow   = expHigh - expBits + 1
    val exp      = valueUInt(expHigh, expLow)
    val mantHigh = expLow - 1
    val mantLow  = 0
    val mant     = valueUInt(mantHigh, mantLow)

    val scaleS:  SInt = Cat(0.U(1.W), scale_e9m0).asSInt
    val expS:    SInt = Cat(0.U(1.W), exp).asSInt
    // scale = 2^(a+b-254) where a,b are fpe8m0 codes; scaleOffset = (a+b) - 254
    val scaleOffset: SInt = scaleS - 254.S
    val newExp: SInt      = expS + scaleOffset

    val maxExp = ((1 << expBits) - 1).S

    val clampedExpS = Wire(SInt(newExp.getWidth.W))
    when (newExp < 0.S) {
      clampedExpS := 0.S
    } .elsewhen (newExp > maxExp) {
      clampedExpS := maxExp
    } .otherwise {
      clampedExpS := newExp
    }
    val clampedExp = clampedExpS.asUInt(expBits - 1, 0)

    val isZero  = (exp === 0.U) && (mant === 0.U)
    val finalExp = Mux(isZero, 0.U(expBits.W), clampedExp)
    val outUInt = Cat(sign, finalExp, mant)
    outUInt.asTypeOf(value)
  }   
  
  def applyMxScaling(
    data: Vec[Vec[T]], 
    scales: Vec[Vec[UInt]]
  ): Vec[Vec[T]] = {
    VecInit(data.zip(scales).map { case (dataRow, scaleRow) =>
    VecInit(dataRow.zip(scaleRow).map { case (elem, scale) =>
      applyE9M0Scale(elem, scale, 8, 7)
      })
    })
  }
 
    require(acc_latency >= 2)
  require(!acc_singleported || !use_mx_scaling, "MX scaling requires non-singleported accumulator")
    val dataType = io.dataType_out
    
    val scaled_data = WireInit(0.U.asTypeOf(t))
    val scalecounter = RegInit(0.U(1.W))
    val pipelined_writes = Reg(Vec(acc_latency, Valid(new AccumulatorWriteReq(n, t))))
    val oldest_pipelined_write = Wire(Valid(new AccumulatorWriteReq(n, t)))
    oldest_pipelined_write := pipelined_writes(acc_latency-1)

    pipelined_writes(0).valid := io.write.fire
    pipelined_writes(0).bits  := io.write.bits
    
    

  if (use_mx_scaling) {
    val scale_mem = scaleFactorMem.get
    scale_mem.io.dataType := io.dataType_out
    scale_mem.io.mx_multi_elem := io.mx_multi_elem
    scale_mem.io.mx_fp8_altfmt := io.mx_fp8_altfmt
    scale_mem.io.scale_mem_write_w <> io.scale_mem_write_w.get
    scale_mem.io.scale_mem_write_act <> io.scale_mem_write_act.get
    scale_mem.io.counter_i := io.counter_i
    scale_mem.io.counter_j := io.counter_j
    scale_mem.io.counter_k := io.counter_k
    scale_mem.io.i := io.i
    scale_mem.io.j := io.j
    scale_mem.io.k := io.k
   
    scale_mem.io.scaleMemCntl <> io.scaleMemCntl.get
    scale_mem.io.read_req.valid := false.B
    scale_mem.io.read_req.bits.addr := DontCare
    scale_mem.io.read_req.bits.scaling_enable := false.B
    scale_mem.io.read_resp.ready := true.B

    when(io.write.fire) {
      scale_mem.io.read_req.valid := true.B
      scale_mem.io.read_req.bits.scaling_enable := true.B
      scale_mem.io.read_req.bits.addr := calculateScaleAddr(io.write.bits.addr)
    }
    val dim = meshRows * tileRows
    when(scale_mem.io.read_resp.valid) {
      // Narrow dim/4 window only for E4M3-single; E4M3-quad and E5M2 use the full-width path below.
      when(dataType === 0.U && !io.mx_multi_elem && !io.mx_fp8_altfmt) {
        for (i <- 0 until dim) {
          val dataElement = Wire(UInt(64.W))
          val offset = pipelined_writes(0).bits.offset
          dataElement := pipelined_writes(0).bits.data(i).asUInt

          val scaled_result = WireInit(0.U(64.W))
          when(i.U >= offset && i.U < (offset +& (dim / 4).U)) {
            scaled_result:= VecInit(dataElement.asTypeOf(Vec(4, UInt(16.W))).zipWithIndex.map {
              case (e, j) =>
              val scale = scale_mem.io.read_resp.bits.combined_scales((i.U - offset)*4.U +& j.U)(8, 0)
              applyE9M0Scale(e, scale, 8, 7) }).asUInt
          }
          scaled_data(i) := scaled_result.asTypeOf(pipelined_writes(0).bits.data(i))
        }
      }.otherwise {
        for (i <- 0 until dim) {
          val scaled_chunks = Wire(Vec(4, UInt(16.W)))
          val dataElement = Wire(UInt(64.W))
          dataElement := pipelined_writes(0).bits.data(i).asUInt
          val scale = scale_mem.io.read_resp.bits.combined_scales(i)
          for (j <- 0 until 4) {
            scaled_chunks(j) := applyE9M0Scale(
              dataElement(j*16 + 15, j*16),
              scale(j*9 + 8, j*9),
              8, 7)
          }
          scaled_data(i) := Cat(scaled_chunks.reverse).asTypeOf(pipelined_writes(0).bits.data(i))
        }
      }
    }
  }
  for (i <- 1 until acc_latency) {
    // always shift
    pipelined_writes(i) := pipelined_writes(i-1)

    // optional override
    if (use_mx_scaling) {
      when (i.U === 1.U) {
        pipelined_writes(i).bits.data := scaled_data
      }
    }
  }


  val rdata_for_adder = Wire(t)
  rdata_for_adder := DontCare
  val rdata_for_read_resp = Wire(chunk_t)
  rdata_for_read_resp := DontCare
  
  val adder_sum = io.adder.sum

  
  io.adder.op1 := rdata_for_adder
  if(use_mx_scaling){
    io.adder.op2 := scaled_data
    io.adder.valid := pipelined_writes(0).valid && pipelined_writes(0).bits.acc
  }
  else {
     io.adder.op2 := pipelined_writes(0).bits.data
     io.adder.valid := pipelined_writes(0).valid && pipelined_writes(0).bits.acc
  }
 
  

  val block_read_req = WireInit(false.B)
  val block_write_req = WireInit(false.B)

  val mask_len = t.getWidth / (8 * 32)
  val mask_elem = UInt((t.getWidth / mask_len).W)

  // val ext_mem_write_q_enq = if (use_shared_ext_mem && use_tl_ext_ram) {
  //   require(acc_sub_banks == 1)
  //   Some(io.ext_mem.get.map { ext_mem =>
  //     val write_q = Module(new Queue(new Bundle {
  //       val write_addr = UInt()
  //       val write_data = UInt()
  //       val write_mask = UInt()
  //     }, 8, pipe = true, flow = true))

  //     write_q.io.enq.valid := false.B
  //     write_q.io.enq.bits := DontCare

  //     ext_mem.write_valid := write_q.io.deq.valid
  //     ext_mem.write_addr := write_q.io.deq.bits.write_addr
  //     ext_mem.write_data := write_q.io.deq.bits.write_data
  //     ext_mem.write_mask := write_q.io.deq.bits.write_mask
  //     write_q.io.deq.ready := ext_mem.write_ready
  //     write_q.io.enq
  //   })
  // } else None

  io.ext_mem.foreach { ext_mem =>
    ext_mem.foreach(_.write_req.valid := false.B)
    ext_mem.foreach(_.write_req.bits.addr := 0.U(io.write.bits.addr.getWidth.W))
    ext_mem.foreach(_.write_req.bits.mask := 0.U(io.write.bits.mask.getWidth.W))
    ext_mem.foreach(_.write_req.bits.data := 0.U(io.write.bits.data.getWidth.W))
    ext_mem.foreach(_.read_req.bits := 0.U((mask_len * mask_elem.getWidth).W))
    ext_mem.foreach(_.read_req.valid := false.B)
    ext_mem.foreach(_.read_resp.ready := false.B) // no reading from external accmem
  }

  if (!acc_singleported && !is_dummy) {
    // if (use_shared_ext_mem && use_tl_ext_ram) {
    //   // duplicate write to external memory
    //   val enq = ext_mem_write_q_enq.get(0)
    //   enq.valid := oldest_pipelined_write.valid
    //   enq.bits.write_addr := oldest_pipelined_write.bits.addr
    //   enq.bits.write_data := Mux(oldest_pipelined_write.bits.acc, adder_sum.asUInt, oldest_pipelined_write.bits.data.asUInt)
    //   enq.bits.write_mask := oldest_pipelined_write.bits.mask.asUInt
    //   // TODO (richard): add buffer here and potentially propagate backpressure to systolic array
    //   assert(enq.ready || !enq.valid, "accumulator external memory write dropped")
    // } else if (use_shared_ext_mem) {
    //   require(false, "cannot use two-port external acc mem bank")
    // }

    println("Creating Accumulator memory with sizes: acc_num_entries " + n + " len " + mask_len + "\n")

    val mem = AsymmetricTwoPortSyncMem(n, t, mask_len, if (use_mx_scaling) numChunks else 2) // TODO We assume byte-alignment here. Use aligned_to instead

    // write
    mem.io.waddr := oldest_pipelined_write.bits.addr
    mem.io.wen := oldest_pipelined_write.valid
    mem.io.wdata := Mux(oldest_pipelined_write.bits.acc, adder_sum, oldest_pipelined_write.bits.data)
    mem.io.mask := VecInit(oldest_pipelined_write.bits.mask.grouped(32).map(_.reduce(_ || _)).toSeq)

    if (use_mx_scaling) {
      // full-width read
      mem.io.raddr_full := io.write.bits.addr
      mem.io.ren_full := io.write.fire && io.write.bits.acc
      rdata_for_adder := mem.io.rdata_full

      // half-width read: address = {addr, bank_sel}, taking bankSelBits of chunk_id
      val bankSelBits = if (numChunks > 1) log2Up(numChunks) else 1
      mem.io.raddr_half := Cat(io.read.req.bits.addr, io.read.req.bits.chunk_id(bankSelBits - 1, 0))
      mem.io.ren_half := io.read.req.fire

      rdata_for_read_resp := mem.io.rdata_half.asTypeOf(chunk_t)
    } else {
      // Non-MX build: full read port is shared between accumulation RMW and read response
      // (write-accumulate wins; io.read.req.ready already blocks reads while accumulating).
      mem.io.raddr_full := Mux(io.write.fire && io.write.bits.acc, io.write.bits.addr, io.read.req.bits.addr)
      mem.io.ren_full := (io.write.fire && io.write.bits.acc) || io.read.req.fire
      rdata_for_adder := mem.io.rdata_full
      rdata_for_read_resp := mem.io.rdata_full.asTypeOf(chunk_t)

      mem.io.raddr_half := 0.U
      mem.io.ren_half := false.B
    }

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
      val (read, write) = if (use_shared_ext_mem && !use_tl_ext_ram) {
        def read(addr: UInt, ren: Bool): Data = {
          io.ext_mem.get(i).read_req.valid := ren
          io.ext_mem.get(i).read_req.bits := addr
          io.ext_mem.get(i).read_resp.bits
        }
        io.ext_mem.get(i).write_req.bits := DontCare
        def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = {
          io.ext_mem.get(i).write_req.valid := true.B
          io.ext_mem.get(i).write_req.bits.addr := addr
          io.ext_mem.get(i).write_req.bits.data := wdata.asUInt
          io.ext_mem.get(i).write_req.bits.mask := wmask.asUInt
        }
        (read _, write _)
      } else {
        val mem = SyncReadMem(n / acc_sub_banks, Vec(mask_len, mask_elem))
        io.ext_mem.get(i).read_req.bits := 0.U((mask_len * mask_elem.getWidth).W)
        io.ext_mem.get(i).read_req.valid := false.B

        def read(addr: UInt, ren: Bool): Data = mem.read(addr, ren)
        def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = if (use_tl_ext_ram) {
          mem.write(addr, wdata, wmask)
          // duplicate write signal to external memory
          // val enq = ext_mem_write_q_enq.get(i)
          // enq.valid := true.B
          // enq.bits.write_mask := wmask.asUInt
          // enq.bits.write_addr := addr
          // enq.bits.write_data := wdata.asUInt
          // // TODO (richard): propagate backpressure to systolic array, add fence ability
          // assert(enq.ready, "accumulator external memory write dropped")
        } else {
          mem.write(addr, wdata, wmask)
        }
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
        w_q_head := (w_q_head << 1).asUInt | w_q_head(nEntries-1)
        for (i <- 0 until nEntries) {
          when (w_q_head(i)) {
            w_q(i).valid := false.B
          }
        }
      }

      val w_q_push = oldest_pipelined_write.valid && isThisBank(oldest_pipelined_write.bits.addr)

      when (w_q_push) {
        assert(!w_q_full || wen, "we ran out of acc-sub-bank write q entries")

        w_q_tail := (w_q_tail << 1).asUInt | w_q_tail(nEntries-1)
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

  val q = Module(new Queue(new AccumulatorReadResp(chunk_t, scale_t), 1, true, true))
  q.io.enq.bits.data := rdata_for_read_resp.asTypeOf(chunk_t)

  if (is_dummy) {
    rdata_for_read_resp := DontCare
    rdata_for_adder := DontCare
  }

  q.io.enq.bits.scale := RegNext(io.read.req.bits.scale)
  q.io.enq.bits.igelu_qb := RegNext(io.read.req.bits.igelu_qb)
  q.io.enq.bits.igelu_qc := RegNext(io.read.req.bits.igelu_qc)
  q.io.enq.bits.iexp_qln2 := RegNext(io.read.req.bits.iexp_qln2)
  q.io.enq.bits.iexp_qln2_inv := RegNext(io.read.req.bits.iexp_qln2_inv)
  q.io.enq.bits.act := RegNext(io.read.req.bits.act)
  q.io.enq.bits.fromDMA := RegNext(io.read.req.bits.fromDMA)
  q.io.enq.bits.acc_bank_id := DontCare
  q.io.enq.bits.chunk_id := RegNext(io.read.req.bits.chunk_id)
  q.io.enq.valid := RegNext(io.read.req.fire)

  val p = q.io.deq

  io.read.resp.bits.data := p.bits.data
  io.read.resp.bits.fromDMA := p.bits.fromDMA
  io.read.resp.bits.igelu_qb := p.bits.igelu_qb
  io.read.resp.bits.igelu_qc := p.bits.igelu_qc
  io.read.resp.bits.iexp_qln2 := p.bits.iexp_qln2
  io.read.resp.bits.iexp_qln2_inv := p.bits.iexp_qln2_inv
  io.read.resp.bits.act := p.bits.act
  io.read.resp.bits.scale := p.bits.scale
  io.read.resp.bits.acc_bank_id := DontCare // This is set in Scratchpad
  io.read.resp.valid := p.valid
  io.read.resp.bits.chunk_id := p.bits.chunk_id
  p.ready := io.read.resp.ready

  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U
  dontTouch(q_will_be_empty)
  io.read.req.ready := q_will_be_empty && (
      // Make sure we aren't accumulating, which would take over both ports
      !(io.write.valid && io.write.bits.acc) &&
      !pipelined_writes.map(r => r.valid && r.bits.addr === io.read.req.bits.addr).reduce(_||_)  &&
      !block_read_req
  )

  io.write.ready := !block_write_req &&
    !pipelined_writes.map(r => r.valid && r.bits.addr === io.write.bits.addr && io.write.bits.acc).reduce(_||_)

  when (reset.asBool) {
    pipelined_writes.foreach(_.valid := false.B)
  }

  // assert(!(io.read.req.valid && io.write.en && io.write.acc), "reading and accumulating simultaneously is not supported")
  assert(!(io.read.req.fire && io.write.fire && io.read.req.bits.addr === io.write.bits.addr), "reading from and writing to same address is not supported")
}

package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}

import Util._

class ScratchpadMemReadRequest[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val len = UInt(8.W) // TODO don't use a magic number for the width here

  val scale = UInt(scale_t_bits.W)

  val has_acc_bitwidth = Bool()

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val status = new MStatus

  override def cloneType: this.type = new ScratchpadMemReadRequest(local_addr_t, scale_t_bits).asInstanceOf[this.type]
}

class ScratchpadMemWriteRequest(local_addr_t: LocalAddr)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val len = UInt(8.W) // TODO don't use a magic number for the width here

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val status = new MStatus

  // Pooling variables
  val pool_en = Bool()
  val store_en = Bool()

  override def cloneType: this.type = new ScratchpadMemWriteRequest(local_addr_t).asInstanceOf[this.type]
}

class ScratchpadMemWriteResponse extends Bundle {
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadMemReadResponse extends Bundle {
  val bytesRead = UInt(16.W) // TODO magic number here
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadReadMemIO[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemReadRequest(local_addr_t, scale_t_bits))
  val resp = Flipped(Valid(new ScratchpadMemReadResponse))

  override def cloneType: this.type = new ScratchpadReadMemIO(local_addr_t, scale_t_bits).asInstanceOf[this.type]
}

// class ScratchpadWriteMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadWriteMemIO(local_addr_t: LocalAddr)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemWriteRequest(local_addr_t))
  val resp = Flipped(Valid(new ScratchpadMemWriteResponse))

  override def cloneType: this.type = new ScratchpadWriteMemIO(local_addr_t).asInstanceOf[this.type]
}

class ScratchpadReadReq(val n: Int) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val fromDMA = Bool()
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val fromDMA = Bool()
}

class ScratchpadReadIO(val n: Int, val w: Int) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(n))
  val resp = Flipped(Decoupled(new ScratchpadReadResp(w)))
}

class ScratchpadWriteIO(val n: Int, val w: Int, val mask_len: Int) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val mask = Output(Vec(mask_len, Bool()))
  val data = Output(UInt(w.W))
}

class ScratchpadBank(n: Int, w: Int, mem_pipeline: Int, aligned_to: Int) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages

  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // How many mask bits are there?
  val mask_elem = UInt((w min (aligned_to * 8)).W) // What datatype does each mask bit correspond to?

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))
  })

  // val mem = SyncReadMem(n, UInt(w.W))
  val mem = SyncReadMem(n, Vec(mask_len, mask_elem))

  when (io.write.en) {
    if (aligned_to >= w)
      mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)))
    else
      mem.write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), io.write.mask)
  }

  val raddr = io.read.req.bits.addr
  val ren = io.read.req.fire()
  val rdata = mem.read(raddr, ren).asUInt()
  val fromDMA = io.read.req.bits.fromDMA

  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new ScratchpadReadResp(w), 1, true, true))
  q.io.enq.valid := RegNext(ren)
  q.io.enq.bits.data := rdata
  q.io.enq.bits.fromDMA := RegNext(fromDMA)

  val q_will_be_empty = (q.io.count +& q.io.enq.fire()) - q.io.deq.fire() === 0.U
  io.read.req.ready := q_will_be_empty

  // Build the rest of the resp pipeline
  val rdata_p = Pipeline(q.io.deq, mem_pipeline)
  io.read.resp <> rdata_p
}

class Scratchpad[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
    (implicit p: Parameters, ev: Arithmetic[T]) extends LazyModule {

  import config._
  import ev._

  val maxBytes = dma_maxbytes
  val dataBits = dma_buswidth

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val spad_w = inputType.getWidth *  block_cols
  val acc_w = accType.getWidth * block_cols

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new StreamReader(config, max_in_flight_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_banks * acc_bank_entries, block_rows, use_tlb_register_filter))
  val writer = LazyModule(new StreamWriter(max_in_flight_reqs, dataBits, maxBytes,
    if (acc_read_full_width) acc_w else spad_w, aligned_to, inputType, block_cols, use_tlb_register_filter))

  // TODO make a cross-bar vs two separate ports a config option
  // id_node :=* reader.node
  // id_node :=* writer.node

  xbar_node := reader.node // TODO
  xbar_node := writer.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {

    val io = IO(new Bundle {
      // DMA ports
      val dma = new Bundle {
        val read = Flipped(new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits))
        val write = Flipped(new ScratchpadWriteMemIO(local_addr_t))
      }

      // SRAM ports
      val srams = new Bundle {
        val read = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w)))
        val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1)))
      }

      // Accumulator ports
      val acc = new Bundle {
        val read = Flipped(Vec(acc_banks, new AccumulatorReadIO(acc_bank_entries, log2Up(accType.getWidth), Vec(meshColumns, Vec(tileColumns, inputType)), Vec(meshColumns, Vec(tileColumns, accType)), acc_scale_args.multiplicand_t)))
        val write = Flipped(Vec(acc_banks, new AccumulatorWriteIO(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType)))))
      }

      // TLB ports
      val tlb = Vec(2, new FrontendTLBIO)

      // Misc. ports
      val busy = Output(Bool())
      val flush = Input(Bool())
    })

    val write_dispatch_q = Queue(io.dma.write.req)

    write_dispatch_q.ready := false.B

    val write_issue_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t), mem_pipeline+1, pipe=true))
    val read_issue_q = Module(new Queue(new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), mem_pipeline+1, pipe=true)) // TODO can't this just be a normal queue?

    write_issue_q.io.enq.valid := false.B
    write_issue_q.io.enq.bits := write_dispatch_q.bits

    val writeData = Wire(Valid(UInt((spad_w max acc_w).W)))
    writeData.valid := false.B
    writeData.bits := DontCare
    val fullAccWriteData = Wire(UInt(acc_w.W))
    fullAccWriteData := DontCare
    val writeData_is_full_width = write_issue_q.io.deq.bits.laddr.is_acc_addr &&
      write_issue_q.io.deq.bits.laddr.read_full_acc_row
    val writeData_is_all_zeros = write_issue_q.io.deq.bits.laddr.is_garbage()

    writer.module.io.req.valid := write_issue_q.io.deq.valid && (writeData.valid || writeData_is_all_zeros)
    write_issue_q.io.deq.ready := writer.module.io.req.ready && (writeData.valid || writeData_is_all_zeros)
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.len := Mux(writeData_is_full_width,
      write_issue_q.io.deq.bits.len * (accType.getWidth / 8).U,
      write_issue_q.io.deq.bits.len * (inputType.getWidth / 8).U)
    writer.module.io.req.bits.data := MuxCase(writeData.bits, Seq(
       writeData_is_all_zeros -> 0.U,
       writeData_is_full_width -> fullAccWriteData
    ))
    writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status
    writer.module.io.req.bits.pool_en := write_issue_q.io.deq.bits.pool_en
    writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en

    // FpgaDebug(write_issue_q.io.deq.bits.laddr.data)
    // FpgaDebug(write_issue_q.io.deq.bits.laddr.accumulate)
    // FpgaDebug(write_issue_q.io.deq.bits.laddr.is_acc_addr)

    io.dma.write.resp.valid := false.B
    io.dma.write.resp.bits.cmd_id := write_dispatch_q.bits.cmd_id

    read_issue_q.io.enq <> io.dma.read.req

    reader.module.io.req.valid := read_issue_q.io.deq.valid
    read_issue_q.io.deq.ready := reader.module.io.req.ready
    reader.module.io.req.bits.vaddr := read_issue_q.io.deq.bits.vaddr
    reader.module.io.req.bits.spaddr := Mux(read_issue_q.io.deq.bits.laddr.is_acc_addr,
      read_issue_q.io.deq.bits.laddr.full_acc_addr(), read_issue_q.io.deq.bits.laddr.full_sp_addr())
    reader.module.io.req.bits.len := read_issue_q.io.deq.bits.len
    reader.module.io.req.bits.scale := read_issue_q.io.deq.bits.scale
    reader.module.io.req.bits.is_acc := read_issue_q.io.deq.bits.laddr.is_acc_addr
    reader.module.io.req.bits.accumulate := read_issue_q.io.deq.bits.laddr.accumulate
    reader.module.io.req.bits.has_acc_bitwidth := read_issue_q.io.deq.bits.has_acc_bitwidth
    reader.module.io.req.bits.status := read_issue_q.io.deq.bits.status
    reader.module.io.req.bits.cmd_id := read_issue_q.io.deq.bits.cmd_id

    val (mvin_scale_in, mvin_scale_out) = VectorScalarMultiplier(config.mvin_scale_args, config.inputType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits), is_acc = false)
    val (mvin_scale_acc_in, mvin_scale_acc_out) = if (mvin_scale_shared) (mvin_scale_in, mvin_scale_out) else
      VectorScalarMultiplier(config.mvin_scale_acc_args, config.accType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits), is_acc = true)

    mvin_scale_in.valid := reader.module.io.resp.valid && (mvin_scale_shared.B || !reader.module.io.resp.bits.is_acc ||
      (reader.module.io.resp.bits.is_acc && !reader.module.io.resp.bits.has_acc_bitwidth))

    mvin_scale_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_in.bits.in))
    mvin_scale_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_t)
    mvin_scale_in.bits.tag := reader.module.io.resp.bits

    mvin_scale_out.ready := false.B

    if (!mvin_scale_shared) {
      mvin_scale_acc_in.valid := reader.module.io.resp.valid &&
        (reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth)
      mvin_scale_acc_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_acc_in.bits.in))
      mvin_scale_acc_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_acc_t)
      mvin_scale_acc_in.bits.tag := reader.module.io.resp.bits

      mvin_scale_acc_out.ready := false.B
    }

    reader.module.io.resp.ready := Mux(reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth,
      mvin_scale_acc_in.ready, mvin_scale_in.ready)

    val mvin_scale_finished = mvin_scale_out.fire() && mvin_scale_out.bits.tag.last
    val mvin_scale_acc_finished = mvin_scale_acc_out.fire() && mvin_scale_acc_out.bits.tag.last
    io.dma.read.resp.valid := mvin_scale_finished || mvin_scale_acc_finished
    io.dma.read.resp.bits.cmd_id := Mux(mvin_scale_finished, mvin_scale_out.bits.tag.cmd_id, mvin_scale_acc_out.bits.tag.cmd_id)
    io.dma.read.resp.bits.bytesRead := Mux(mvin_scale_finished, mvin_scale_out.bits.tag.bytes_read, mvin_scale_acc_out.bits.tag.bytes_read)

    /*
    reader.module.io.resp.ready := false.B
    io.dma.read.resp.valid := reader.module.io.resp.fire() && reader.module.io.resp.bits.last
    io.dma.read.resp.bits.cmd_id := reader.module.io.resp.bits.cmd_id
    io.dma.read.resp.bits.bytesRead := reader.module.io.resp.bits.bytes_read
    */

    io.tlb(0) <> writer.module.io.tlb
    io.tlb(1) <> reader.module.io.tlb

    writer.module.io.flush := io.flush
    reader.module.io.flush := io.flush

    io.busy := writer.module.io.busy || reader.module.io.busy || write_issue_q.io.deq.valid

    {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(sp_bank_entries, spad_w, mem_pipeline, aligned_to)) }
      val bank_ios = VecInit(banks.map(_.io))

      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.sp_bank())

      when (!write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := bank_issued_io.read.resp.valid && bank_issued_io.read.resp.bits.fromDMA
        writeData.bits := bank_issued_io.read.resp.bits.data
      }

      // Reading from the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val ex_read_req = io.srams.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U

        bio.read.req.valid := exread || (dmawrite && !write_dispatch_q.bits.laddr.is_garbage())
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire() || write_dispatch_q.bits.laddr.is_garbage()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val ex_read_resp = io.srams.read(i).resp
        val dma_resp_ready = writer.module.io.req.ready &&
          !write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U && // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid
          !write_issue_q.io.deq.bits.laddr.is_garbage()

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_resp_ready, ex_read_resp.ready)
        ex_read_resp.valid := bio.read.resp.valid // TODO should we AND this with fromDMA?
        ex_read_resp.bits := bio.read.resp.bits
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.srams.write(i).en
        val dmaread = mvin_scale_out.valid && !mvin_scale_out.bits.tag.is_acc &&
          mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t).sp_bank() === i.U

        bio.write.en := exwrite || dmaread

        when (exwrite) {
          bio.write.addr := io.srams.write(i).addr
          bio.write.data := io.srams.write(i).data
          bio.write.mask := io.srams.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := mvin_scale_out.bits.tag.addr
          bio.write.data := mvin_scale_out.bits.out.asUInt()
          bio.write.mask := mvin_scale_out.bits.tag.mask take ((spad_w / (aligned_to * 8)) max 1)

          mvin_scale_out.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
        }
      }
    }

    {
      val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
      val spad_row_t = Vec(meshColumns, Vec(tileColumns, inputType))

      val banks = Seq.fill(acc_banks) { Module(new AccumulatorMem(acc_bank_entries, acc_row_t, spad_row_t, mem_pipeline, acc_scale_args, acc_read_small_width, acc_read_full_width)) }
      val bank_ios = VecInit(banks.map(_.io))

      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.acc_bank())

      when (write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := bank_issued_io.read.resp.valid && bank_issued_io.read.resp.bits.fromDMA
        writeData.bits := bank_issued_io.read.resp.bits.data.asUInt()
        fullAccWriteData := bank_issued_io.read.resp.bits.full_data.asUInt()
      }

      // Reading from the Accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val ex_read_req = io.acc.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready &&
          write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.acc_bank() === i.U

        bio.read.req.valid := exread || (dmawrite && !write_dispatch_q.bits.laddr.is_garbage())
        bio.read.req.bits.scale := ex_read_req.bits.scale
        bio.read.req.bits.relu6_shift := ex_read_req.bits.relu6_shift
        bio.read.req.bits.act := ex_read_req.bits.act
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from accumulator banks
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.full := false.B
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          bio.read.req.bits.full := write_dispatch_q.bits.laddr.read_full_acc_row
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire() || write_dispatch_q.bits.laddr.is_garbage()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val ex_read_resp = io.acc.read(i).resp
        val dma_resp_ready = writer.module.io.req.ready &&
          write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.acc_bank() === i.U && // I believe we don't need to check that write_issue_q is valid here, because if the accumulator bank's resp is valid, then that means that the write_issue_q's deq should also be valid
          !write_issue_q.io.deq.bits.laddr.is_garbage()

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_resp_ready, ex_read_resp.ready)
        ex_read_resp.valid := bio.read.resp.valid // TODO should we AND this with fromDMA?
        ex_read_resp.bits := bio.read.resp.bits
      }

      // Writing to the accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.acc.write(i).en

        val from_mvin_scale = mvin_scale_out.valid && mvin_scale_out.bits.tag.is_acc
        val from_mvin_scale_acc = mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.tag.is_acc

        val dmaread_bank = Mux(from_mvin_scale_acc, mvin_scale_acc_out.bits.tag.addr.asTypeOf(local_addr_t).acc_bank(),
          mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t).acc_bank())
        val dmaread_row = Mux(from_mvin_scale_acc, mvin_scale_acc_out.bits.tag.addr, mvin_scale_out.bits.tag.addr)

        val mvin_scale_out_last = mvin_scale_out.valid && mvin_scale_out.bits.tag.last

        val dmaread = (from_mvin_scale || from_mvin_scale_acc) &&
          dmaread_bank === i.U &&
          !(bio.write.current_waddr.valid && bio.write.current_waddr.bits === dmaread_row) &&
          (mvin_scale_same.B || from_mvin_scale || !mvin_scale_out_last)

        bio.write.en := exwrite || dmaread

        when (exwrite) {
          bio.write.addr := io.acc.write(i).addr
          bio.write.data := io.acc.write(i).data
          bio.write.acc := io.acc.write(i).acc
          bio.write.mask := io.acc.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := dmaread_row
          bio.write.data := Mux(from_mvin_scale_acc,
            mvin_scale_acc_out.bits.out.asTypeOf(acc_row_t),
            VecInit(mvin_scale_out.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t))
          bio.write.acc := Mux(from_mvin_scale_acc, mvin_scale_acc_out.bits.tag.accumulate,
            mvin_scale_out.bits.tag.accumulate)
          bio.write.mask :=
            Mux(from_mvin_scale_acc,
              mvin_scale_acc_out.bits.tag.mask,
              {
                val n = accType.getWidth / inputType.getWidth
                val mask = mvin_scale_out.bits.tag.mask take ((spad_w / (aligned_to * 8)) max 1)
                val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
                expanded
              })

          when (from_mvin_scale_acc) {
            mvin_scale_acc_out.ready := true.B
          }.otherwise {
            mvin_scale_out.ready := true.B
          }
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.acc := DontCare
          bio.write.mask := DontCare
        }
      }
    }
  }
}

package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import Util._


// class ScratchpadMemReadRequest(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadMemReadRequest(local_addr_t: LocalAddr)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val len = UInt(8.W) // TODO don't use a magic number for the width here

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val status = new MStatus

  override def cloneType: this.type = new ScratchpadMemReadRequest(local_addr_t).asInstanceOf[this.type]
}

// class ScratchpadMemWriteRequest(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadMemWriteRequest(local_addr_t: LocalAddr)
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val cmd_id = UInt(8.W) // TODO don't use a magic number here

  val status = new MStatus

  override def cloneType: this.type = new ScratchpadMemWriteRequest(local_addr_t).asInstanceOf[this.type]
}

class ScratchpadMemWriteResponse extends Bundle {
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadMemReadResponse extends Bundle {
  val bytesRead = UInt(16.W) // TODO magic number here
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

// class ScratchpadReadMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadReadMemIO(local_addr_t: LocalAddr)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemReadRequest(local_addr_t.cloneType))
  val resp = Flipped(Valid(new ScratchpadMemReadResponse))

  override def cloneType: this.type = new ScratchpadReadMemIO(local_addr_t.cloneType).asInstanceOf[this.type]
}

// class ScratchpadWriteMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
class ScratchpadWriteMemIO(local_addr_t: LocalAddr)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemWriteRequest(local_addr_t.cloneType))
  val resp = Flipped(Valid(new ScratchpadMemWriteResponse))

  override def cloneType: this.type = new ScratchpadWriteMemIO(local_addr_t.cloneType).asInstanceOf[this.type]
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

// TODO find a more elegant way to move data into accumulator
// TODO replace the SRAM types with Vec[Vec[inputType]], rather than just simple UInts
// TODO support unaligned accesses, for both multiple and single matrix loads
// TODO scratchpad is currently broken when one row is larger than dataBits. The requests arrive out-of-order, meaning that half of one row might arrive after the first have of another row. Some kind of re-ordering buffer may be needed
class Scratchpad[T <: Data: Arithmetic](
    /*nBanks: Int, nRows: Int, local_addr_t: LocalAddr, sp_addr_t: SPAddr,*/ config: GemminiArrayConfig[T])
    (implicit p: Parameters) extends LazyModule {

  import config._

  val maxBytes = dma_maxbytes
  val dataBits = dma_buswidth

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val spad_w = inputType.getWidth *  block_cols
  val acc_w = accType.getWidth * block_cols

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new StreamReader(max_in_flight_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_rows, block_rows))
  val writer = LazyModule(new StreamWriter(max_in_flight_reqs, dataBits, maxBytes, spad_w, aligned_to))

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
        val read = Flipped(new ScratchpadReadMemIO(local_addr_t))
        val write = Flipped(new ScratchpadWriteMemIO(local_addr_t))
      }

      // SRAM ports
      val read  = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w)))
      val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1)))

      // Accumulator ports
      val acc = new AccumulatorMemIO(acc_rows, Vec(meshColumns, Vec(tileColumns, accType)), Vec(meshColumns, Vec(tileColumns, inputType)))

      // TLB ports
      val tlb = Vec(2, new FrontendTLBIO)
      val mstatus = Output(new MStatus)

      // Misc. ports
      val busy = Output(Bool())
      val flush = Input(Bool())
    })

    val write_dispatch_q = Queue(io.dma.write.req)

    write_dispatch_q.ready := false.B

    val write_issue_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t), mem_pipeline+1, pipe=true))
    val read_issue_q = Module(new Queue(new ScratchpadMemReadRequest(local_addr_t), mem_pipeline+1, pipe=true)) // TODO can't this just be a normal queue?

    write_issue_q.io.enq.valid := false.B
    write_issue_q.io.enq.bits := write_dispatch_q.bits

    val writeData = Wire(Valid(UInt((spad_w max acc_w).W)))
    writeData.valid := false.B
    writeData.bits := DontCare

    writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid
    write_issue_q.io.deq.ready := writer.module.io.req.ready && writeData.valid
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.data := writeData.bits
    writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status

    io.dma.write.resp.valid := false.B
    io.dma.write.resp.bits.cmd_id := write_dispatch_q.bits.cmd_id

    read_issue_q.io.enq <> io.dma.read.req

    reader.module.io.req.valid := read_issue_q.io.deq.valid
    read_issue_q.io.deq.ready := reader.module.io.req.ready
    reader.module.io.req.bits.vaddr := read_issue_q.io.deq.bits.vaddr
    reader.module.io.req.bits.spaddr := Mux(read_issue_q.io.deq.bits.laddr.is_acc_addr,
      read_issue_q.io.deq.bits.laddr.acc_row(), read_issue_q.io.deq.bits.laddr.full_sp_addr())
    reader.module.io.req.bits.len := read_issue_q.io.deq.bits.len
    reader.module.io.req.bits.is_acc := read_issue_q.io.deq.bits.laddr.is_acc_addr
    reader.module.io.req.bits.status := read_issue_q.io.deq.bits.status
    reader.module.io.req.bits.cmd_id := read_issue_q.io.deq.bits.cmd_id

    reader.module.io.resp.ready := false.B
    io.dma.read.resp.valid := reader.module.io.resp.fire() && reader.module.io.resp.bits.last
    io.dma.read.resp.bits.cmd_id := reader.module.io.resp.bits.cmd_id
    io.dma.read.resp.bits.bytesRead := reader.module.io.resp.bits.bytes_read

    io.tlb(0) <> writer.module.io.tlb
    io.tlb(1) <> reader.module.io.tlb

    writer.module.io.flush := io.flush
    reader.module.io.flush := io.flush

    io.busy := writer.module.io.busy || reader.module.io.busy

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
        val ex_read_req = io.read(i).req
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val ex_read_resp = io.read(i).resp
        val dma_resp_ready = writer.module.io.req.ready &&
          !write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_resp_ready, ex_read_resp.ready)
        ex_read_resp.valid := bio.read.resp.valid // TODO should we AND this with fromDMA?
        ex_read_resp.bits := bio.read.resp.bits
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.write(i).en
        val dmaread = reader.module.io.resp.valid &&
          !reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.addr.asTypeOf(local_addr_t).sp_bank() === i.U

        bio.write.en := exwrite || dmaread

        when (exwrite) {
          bio.write.addr := io.write(i).addr
          bio.write.data := io.write(i).data
          bio.write.mask := io.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := reader.module.io.resp.bits.addr
          bio.write.data := reader.module.io.resp.bits.data
          bio.write.mask := reader.module.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)

          reader.module.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
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

      val accumulator = Module(new AccumulatorMem(acc_rows, acc_row_t, spad_row_t, mem_pipeline))

      when (write_issue_q.io.deq.bits.laddr.is_acc_addr) {
        writeData.valid := accumulator.io.read.resp.valid && accumulator.io.read.resp.bits.fromDMA
        writeData.bits := accumulator.io.read.resp.bits.data.asUInt()
      }

      // Accumulator reads
      {
        val exread = io.acc.read.req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_issue_q.io.enq.ready && write_dispatch_q.bits.laddr.is_acc_addr

        accumulator.io.read.req.valid := exread || dmawrite
        accumulator.io.read.req.bits.shift := io.acc.read.req.bits.shift
        accumulator.io.read.req.bits.relu6_shift := io.acc.read.req.bits.relu6_shift
        accumulator.io.read.req.bits.act := io.acc.read.req.bits.act
        io.acc.read.req.ready := accumulator.io.read.req.ready

        // The ExecuteController gets priority when reading from the accumulator
        when (exread) {
          accumulator.io.read.req.bits.addr := io.acc.read.req.bits.addr
          accumulator.io.read.req.bits.fromDMA := false.B
        }.elsewhen(dmawrite) {
          accumulator.io.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          accumulator.io.read.req.bits.fromDMA := true.B

          when (accumulator.io.read.req.fire()) {
            write_dispatch_q.ready := true.B
            write_issue_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          accumulator.io.read.req.bits.addr := DontCare
          accumulator.io.read.req.bits.fromDMA := DontCare
        }

        val ex_resp_ready = io.acc.read.resp.ready
        val dma_resp_ready = writer.module.io.req.ready && write_issue_q.io.deq.bits.laddr.is_acc_addr // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid

        accumulator.io.read.resp.ready := Mux(accumulator.io.read.resp.bits.fromDMA, dma_resp_ready, ex_resp_ready)
        io.acc.read.resp.valid := accumulator.io.read.resp.valid // TODO should we AND this with fromDMA?
        io.acc.read.resp.bits := accumulator.io.read.resp.bits
      }

      // Accumulator writes
      {
        val exwrite = io.acc.write.en
        val dmaread = reader.module.io.resp.valid && reader.module.io.resp.bits.is_acc

        accumulator.io.write.en := exwrite || dmaread

        when (exwrite) {
          accumulator.io.write.addr := io.acc.write.addr
          accumulator.io.write.data := io.acc.write.data
          accumulator.io.write.acc := io.acc.write.acc
        }.elsewhen (dmaread) {
          accumulator.io.write.addr := reader.module.io.resp.bits.addr
          accumulator.io.write.data := reader.module.io.resp.bits.data.asTypeOf(acc_row_t)
          accumulator.io.write.acc := false.B

          reader.module.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          accumulator.io.write.addr := DontCare
          accumulator.io.write.data := DontCare
          accumulator.io.write.acc := DontCare
        }
      }
    }
  }
}

package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLEdgeOut, TLIdentityNode}
import freechips.rocketchip.util.InOrderArbiter
import icenet.{StreamReader, StreamWriter, StreamReadRequest, StreamWriteRequest}

class DecoupledTLB(entries: Int)(implicit edge: TLEdgeOut, p: Parameters)
    extends CoreModule {
  val lgMaxSize = log2Ceil(coreDataBytes)
  val io = new Bundle {
    val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
    val resp = Decoupled(new TLBResp)
    val ptw = new TLBPTWIO
  }

  val req = Reg(new TLBReq(lgMaxSize))
  val resp = Reg(new TLBResp)
  val tlb = Module(new TLB(false, lgMaxSize, TLBConfig(entries)))

  val s_idle :: s_tlb_req :: s_tlb_resp :: s_done :: Nil = Enum(4)
  val state = RegInit(s_idle)

  when (io.req.fire()) {
    req := io.req.bits
    state := s_tlb_req
  }

  when (tlb.io.req.fire()) {
    state := s_tlb_resp
  }

  when (state === s_tlb_resp) {
    when (tlb.io.resp.miss) {
      state := s_tlb_req
    } .otherwise {
      resp := tlb.io.resp
      state := s_done
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.req.ready := state === s_idle

  tlb.io.sfence.valid := false.B
  tlb.io.sfence.bits := DontCare
  tlb.io.kill := false.B
  tlb.io.req.valid := state === s_tlb_req
  tlb.io.req.bits := req

  io.resp.valid := state === s_done
  io.resp.bits := resp

  io.ptw <> tlb.io.ptw
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  val req = Decoupled(new TLBReq(lgMaxSize))
  val resp = Flipped(Decoupled(new TLBResp))
}

class FrontendTLB(nClients: Int, entries: Int)
                 (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = new TLBPTWIO
  })

  val lgMaxSize = log2Ceil(coreDataBytes)
  val tlbArb = Module(new InOrderArbiter(
    new TLBReq(lgMaxSize), new TLBResp, nClients))
  val tlb = Module(new DecoupledTLB(entries))
  tlb.io.req <> tlbArb.io.out_req
  tlbArb.io.out_resp <> tlb.io.resp
  io.ptw <> tlb.io.ptw

  tlbArb.io.in_req <> io.clients.map(_.req)
  io.clients.zip(tlbArb.io.in_resp).foreach {
    case (client, arb_resp) => client.resp <> arb_resp
  }
}

class ScratchpadMemRequest(val nBanks: Int, val nRows: Int, val acc_rows: Int)
    (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)

  val spbank = UInt(log2Ceil(nBanks).W)
  val spaddr = UInt(log2Ceil(nRows).W)

  val accaddr = UInt(log2Ceil(acc_rows).W)
  val is_acc = Bool()

  val write = Bool()
}

class ScratchpadMemResponse extends Bundle {
  val error = Bool()
}

class ScratchpadMemIO(val nBanks: Int, val nRows: Int, val acc_rows: Int)
    (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemRequest(nBanks, nRows, acc_rows))
  val resp = Flipped(Decoupled(new ScratchpadMemResponse))
}

class ScratchpadReadIO(val n: Int, val w: Int) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val data = Input(UInt(w.W))
}

class ScratchpadWriteIO(val n: Int, val w: Int) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val data = Output(UInt(w.W))
}

class ScratchpadBank(n: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w))
  })

  val mem = SyncReadMem(n, UInt(w.W))

  when (io.write.en) { mem.write(io.write.addr, io.write.data) }

  io.read.data := mem.read(io.read.addr, io.read.en)
}

// TODO replace the SRAM types with Vec[Vec[inputType]], rather than just simple UInts
class Scratchpad[T <: Data](
    nBanks: Int, nRows: Int, w: Int, inputType: T, accType: T, config: SystolicArrayConfig,
    val maxBytes: Int = 64, val dataBits: Int = 64)
    (implicit p: Parameters, ev: Arithmetic[T]) extends LazyModule {

  import config._
  import ev._

  val dataBytes = dataBits / 8
  val outFlits = w / dataBits
  val nXacts = ((w/8) - 1) / maxBytes + 1

  require(w % dataBits == 0)

  val node = TLIdentityNode()
  val reader = LazyModule(new StreamReader(nXacts, outFlits, maxBytes))
  val writer = LazyModule(new StreamWriter(nXacts, maxBytes))
  node := reader.node
  node := writer.node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val dma = Flipped(new ScratchpadMemIO(nBanks, nRows, acc_rows))
      val read  = Flipped(Vec(nBanks, new ScratchpadReadIO(nRows, w)))
      val write = Flipped(Vec(nBanks, new ScratchpadWriteIO(nRows, w)))
      val tlb = new FrontendTLBIO

      // Accumulator ports // TODO add a store DMA for accumulator
      val acc_read = Flipped(new AccumulatorReadIO(acc_rows, accType.getWidth - inputType.getWidth, Vec(meshColumns, Vec(tileColumns, inputType))))
      val acc_write = Flipped(new AccumulatorWriteIO(acc_rows, Vec(meshColumns, Vec(tileColumns, accType))))
    })

    require(reader.module.dataBits == dataBits)
    require(writer.module.dataBits == dataBits)

    val (s_idle :: s_translate_req :: s_translate_resp ::
         s_readreq :: s_readresp :: s_readwait ::
         s_writereq :: s_writedata :: s_writeresp ::
         s_respond :: Nil) = Enum(10)
    val state = RegInit(s_idle)
    val error = Reg(Bool())

    io.dma.req.ready := state === s_idle
    io.dma.resp.valid := state === s_respond
    io.dma.resp.bits.error := error

    val rowBytes = w / 8
    val nBeats = (w - 1) / dataBits + 1
    val rowAddrBits = log2Ceil(rowBytes)
    val byteAddrBits = log2Ceil(dataBytes)

    val req = Reg(new ScratchpadMemRequest(nBanks, nRows, acc_rows))
    val reqVpn = req.vaddr(coreMaxAddrBits-1, pgIdxBits)
    val reqOffset = req.vaddr(pgIdxBits-1, 0)
    val reqPpn = Reg(UInt(ppnBits.W))
    val reqPaddr = Cat(reqPpn, reqOffset)
    val bytesLeft = Reg(UInt(log2Ceil(rowBytes+1).W))

    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.vaddr := Cat(reqVpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.passthrough := false.B
    io.tlb.req.bits.size := log2Ceil(dataBytes).U
    io.tlb.req.bits.cmd := Mux(req.write, M_XWR, M_XRD)
    io.tlb.resp.ready := state === s_translate_resp

    val tlberr = Mux(req.write,
      io.tlb.resp.bits.pf.st || io.tlb.resp.bits.ae.st,
      io.tlb.resp.bits.pf.ld || io.tlb.resp.bits.ae.ld)

    val nextVaddr = Cat(reqVpn + 1.U, 0.U(pgIdxBits.W))
    val pageBytes = nextVaddr - req.vaddr
    val lastPage = bytesLeft <= pageBytes
    val bytesAvail = Mux(lastPage, bytesLeft, pageBytes)
    val lastFlit = bytesAvail <= dataBytes.U
    val bytesToSend = Mux(lastFlit, bytesAvail, dataBytes.U)

    val readReq = Wire(new StreamReadRequest)
    readReq.partial := !lastPage
    readReq.address := reqPaddr
    readReq.length := bytesAvail

    val writeReq = Wire(new StreamWriteRequest)
    writeReq.address := reqPaddr
    writeReq.length := bytesAvail

    reader.module.io.req.valid := state === s_readreq
    reader.module.io.req.bits := readReq
    reader.module.io.resp.ready := state === s_readresp

    writer.module.io.req.valid := state === s_writereq
    writer.module.io.req.bits := writeReq
    writer.module.io.resp.ready := state === s_writeresp

    val rowBuffer = Reg(Vec(nBeats, UInt(dataBits.W)))
    val bufAddr = Reg(UInt(rowAddrBits.W))
    val bufIdx = bufAddr >> byteAddrBits.U//bufAddr(rowAddrBits-1, byteAddrBits)
    val bufDone = Reg(Bool())

    val (rowData, rowKeep) = {
      val offset = bufAddr(byteAddrBits-1, 0)
      val rshift = Cat(offset, 0.U(3.W))
      val lshift = Cat(dataBytes.U - offset, 0.U(3.W))

      val first = rowBuffer(bufIdx) >> rshift
      val second = rowBuffer(bufIdx + 1.U) << lshift

      val data = first | second
      val nbytes = bytesToSend(byteAddrBits, 0)
      val bytemask = (1.U << nbytes) - 1.U

      (data(dataBits-1, 0), bytemask(dataBytes-1, 0))
    }

    reader.module.io.out.ready := !bufDone
    writer.module.io.in.valid := state === s_writedata
    writer.module.io.in.bits.data := rowData
    writer.module.io.in.bits.keep := rowKeep
    writer.module.io.in.bits.last := lastFlit

    val dmardata = WireInit(0.U(w.W))
    val dmaren = WireInit(false.B)
    val dmawen = WireInit(false.B)

    val banks = Seq.fill(nBanks) { Module(new ScratchpadBank(nRows, w)) }

    for (i <- 0 until nBanks) {
      val bank = banks(i)
      val read = io.read(i)
      val write = io.write(i)
      val bankren = dmaren && io.dma.req.bits.spbank === i.U && !io.dma.req.bits.is_acc
      val bankwen = dmawen && req.spbank === i.U && !req.is_acc

      bank.io.read.en := bankren || read.en
      bank.io.read.addr := Mux(bankren, io.dma.req.bits.spaddr, read.addr)
      read.data := bank.io.read.data
      when (req.spbank === i.U && !req.is_acc) { dmardata := bank.io.read.data }

      bank.io.write.en := bankwen || write.en
      bank.io.write.addr := Mux(bankwen, req.spaddr, write.addr)
      bank.io.write.data := Mux(bankwen, rowBuffer.asUInt, write.data)
    }

    val accumulator = Module(new AccumulatorMem(acc_rows, Vec(meshColumns, Vec(tileColumns, accType)), Vec(meshColumns, Vec(tileColumns, inputType))))
    val accbankren = dmaren && io.dma.req.bits.is_acc
    accumulator.io.read.en := accbankren || io.acc_read.en
    accumulator.io.read.addr := Mux(accbankren, io.dma.req.bits.accaddr, io.acc_read.addr)
    accumulator.io.read.shift := io.acc_read.shift
    io.acc_read.data := accumulator.io.read.data
    when (req.is_acc) { dmardata := accumulator.io.read.data.asUInt() }
    accumulator.io.write <> io.acc_write

    when (io.dma.req.fire()) {
      req := io.dma.req.bits
      bufAddr := 0.U
      bufDone := false.B
      bytesLeft := rowBytes.U
      state := s_translate_req

      when (io.dma.req.bits.write) { dmaren := true.B }
    }

    when (io.tlb.req.fire()) {
      when (req.write && !bufDone) {
        rowBuffer := dmardata.asTypeOf(Vec(nBeats, UInt(dataBits.W)))
        bufDone := true.B
      }
      state := s_translate_resp
    }

    when (io.tlb.resp.fire()) {
      when (tlberr) {
        error := true.B
        state := s_respond
      } .otherwise {
        reqPpn := io.tlb.resp.bits.paddr >> pgIdxBits.U
        state := Mux(req.write, s_writereq, s_readreq)
      }
    }

    when (reader.module.io.req.fire()) {
      req.vaddr := nextVaddr
      bytesLeft := bytesLeft - bytesAvail
      state := s_readresp
    }

    when (reader.module.io.resp.fire()) {
      state := Mux(bytesLeft === 0.U, s_readwait, s_translate_req)
    }

    when (reader.module.io.out.fire()) {
      rowBuffer(bufIdx) := reader.module.io.out.bits.data
      bufAddr := bufAddr + dataBytes.U
      when (reader.module.io.out.bits.last) { bufDone := true.B }
    }

    when (writer.module.io.req.fire()) {
      state := s_writedata
    }

    when (writer.module.io.in.fire()) {
      bufAddr := bufAddr + bytesToSend
      req.vaddr := req.vaddr + bytesToSend
      bytesLeft := bytesLeft - bytesToSend
      when (writer.module.io.in.bits.last) { state := s_writeresp }
    }

    when (writer.module.io.resp.fire()) {
      error := false.B
      state := Mux(bytesLeft === 0.U, s_respond , s_translate_req)
    }

    when (state === s_readwait && bufDone) {
      dmawen := true.B
      error := false.B
      state := s_respond
    }

    when (io.dma.resp.fire()) { state := s_idle }
  }
}

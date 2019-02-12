package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{M_XRD, M_XWR}
import freechips.rocketchip.tile._
import icenet.IceNicSendPath
import icenet.IceNetConsts._

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
  val tlb = Module(new TLB(lgMaxSize, entries))

  val s_idle :: s_tlb_req :: s_tlb_resp :: s_done :: Nil = Enum(4)
  val state = Reg(init = s_idle)

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

class ScratchpadMemRequest(n: Int)
    (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val spaddr = UInt(log2Ceil(n).W)
  val write = Bool()
}

class ScratchpadMemResponse extends Bundle {
  val error = Bool()
}

class InputScratchpad(n: Int, w: Int) (implicit p: Parameters)
    extends LazyModule with HasCoreParameters {

  val node = TLIdentityNode()
  val sendpath = LazyModule(new IceNicSendPath)
  val recvpath = LazyModule(new IceNicRecvPath)
  node := TLWidthWidget(NET_IF_BYTES) := sendpath.node
  node := TLWidthWidget(NET_IF_BYTES) := recvpath.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new ScratchpadMemRequest(n)))
      val resp = Decoupled(new ScratchpadMemResponse)
      val read = new Bundle {
        val en = Input(Bool())
        val addr = Input(UInt(log2Ceil(n).W))
        val data = Output(UInt(w.W))
      }
      val write = new Bundle {
        val en = Input(Bool())
        val addr = Input(UInt(log2Ceil(n).W))
        val data = Input(UInt(w.W))
      }
      val tlb = new FrontendTLBIO
    })

    val rowBytes = (n * w) / 8

    val (s_idle :: s_translate_req :: s_translate_resp :: s_wait ::
         s_sendreq :: s_sendcomp :: s_recvreq :: s_recvcomp) = Enum(8)
    val state = RegInit(s_idle)

    val nBeats = (w - 1) / NET_IF_WIDTH + 1
    val dataBuf = Reg(Vec(nBeats, UInt(NET_IF_WIDTH.W)))
    val bufIdx = Reg(UInt(log2Ceil(nBeats).W))
    val bufDone = Reg(Bool())
    val readDone = Reg(Bool())

    val req = Reg(new ScratchpadMemRequest(n))
    val reqVpn = req.vaddr(coreMaxAddrBits-1, pgIdxBits)
    val reqOffset = req.vaddr(pgIdxBits-1, 0)
    val reqPpn = Reg(UInt(ppnBits.W))
    val bytesLeft = Reg(UInt(log2Ceil(rowBytes+1).W))

    val mem = SyncReadMem(n, UInt(w.W))
    val waddr = Wire(UInt(log2Ceil(n).W))
    val wen = Wire(Bool())
    val wdata = Wire(UInt(w.W))
    val raddr = Wire(UInt(log2Ceil(n).W))
    val ren = Wire(Bool())
    val rdata = mem.read(raddr, ren)

    when (wen) { mem.write(waddr, wdata) }

    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.vaddr := Cat(reqVpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.passthrough = false.B
    io.tlb.req.bits.size := log2Ceil(NET_IF_BYTES).U
    io.tlb.req.bits.cmd := Mux(req.write, M_XWR, M_XRD)
    io.tlb.resp.ready := state === s_translate_resp

    val nextVaddr = Cat(reqVpn + 1.U, 0.U(pgIdxBits.W))
    val pageBytes = nextVaddr - req.vaddr
    val partial = pageBytes < bytesLeft
    val partlen = Wire(UInt(15.W))
    val partaddr = Wire(UInt(48.W))

    partlen := Mux(partial, pageBytes, bytesLeft)
    partaddr := Cat(reqPpn, reqOffset)

    sendpath.module.io.req.valid := state === s_sendreq
    sendpath.module.io.req.bits := Cat(partial, partlen, partaddr)
    sendpath.module.io.comp.ready := state === s_sendcomp

    when (io.req.fire()) {
      req := io.req.bits
      bufIdx := 0.U
      bufDone := false.B
      readDone := false.B
      bytesLeft := rowBytes.U
      state := s_translate_req

      when (io.req.bits.write) { ren := true.B }
    }

    when (io.tlb.req.fire()) {
      when (!readDone) {
        dataBuf := rdata.asTypeOf(Vec(nBeats, UInt(NET_IF_WIDTH.W)))
        readDone := true.B
      }
      state := s_translate_resp
    }

    when (io.tlb.resp.fire()) {
      reqPpn := io.tlb.resp.bits.paddr >> pgIdxBits.U
      state := Mux(req.write, s_recvreq, s_sendreq)
    }

    when (sendpath.module.io.req.fire()) {
      req.vaddr := nextVaddr
      bytesLeft := bytesLeft - partlen
      state := s_sendcomp
    }

    when (sendpath.module.io.comp.fire()) {
      state := Mux(bytesLeft === 0.U, s_wait, s_translate_req)
    }

    when (sendpath.module.io.out.fire()) {
      dataBuf(bufIdx) := sendpath.module.io.out.bits.data
      bufIdx := bufIdx + 1.U
      when (sendpath.module.io.out.bits.last) { bufDone := true.B }
    }

    when (recvpath.module.io.req.fire()) {
      req.vaddr := nextVaddr
      bytesLeft := bytesLeft - partlen
      state := s_recvcomp
    }

    when (recvpath.module.io.comp.fire()) {
      state := Mux(bytesLeft === 0.U, s_wait, s_translate_req)
    }

    when (recvpath.module.io.in.fire()) {
    }

    when (state === s_wait) {
      when (!req.write && bufDone) {
        wen := true.B
        waddr := req.spaddr
        wdata := dataBuf.asUInt
        state := s_idle
      }
    }

    when (io.write.en) {
      wen := true.B
      waddr := io.write.addr
      wdata := io.write.data
    }

    when (io.read.en) {
      ren := true.B
      raddr := io.read.addr
    }

    io.read.data := rdata
  }
}

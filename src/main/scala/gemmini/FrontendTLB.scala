package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreBundle, CoreModule}
import freechips.rocketchip.tilelink.TLEdgeOut
import freechips.rocketchip.util.InOrderArbiter

import Util._


class DecoupledTLBReq(val lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle {
  val tlb_req = new TLBReq(lgMaxSize)
  val status = new MStatus
}

class TLBExceptionIO extends Bundle {
  val interrupt = Output(Bool())
  val flush_retry = Input(Bool())
  val flush_skip = Input(Bool())

  def flush(dummy: Int = 0): Bool = flush_retry || flush_skip
}

// TODO can we make TLB hits only take one cycle?
class DecoupledTLB(entries: Int, maxSize: Int)(implicit edge: TLEdgeOut, p: Parameters)
  extends CoreModule {

  val lgMaxSize = log2Ceil(maxSize)
  val io = new Bundle {
    // val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
    val req = Flipped(Decoupled(new DecoupledTLBReq(lgMaxSize)))
    val resp = Valid(new TLBResp)
    val ptw = new TLBPTWIO

    val exp = new TLBExceptionIO
  }

  val tlb = Module(new TLB(false, lgMaxSize, TLBConfig(entries)))
  val req = RegEnableThru(io.req.bits, io.req.fire())

  val s_idle :: s_waiting_for_resp :: s_interrupt :: Nil = Enum(3)
  val state = RegInit(s_idle)

  io.req.ready := state === s_idle
  tlb.io.req.valid := io.req.fire() || state === s_waiting_for_resp
  tlb.io.req.bits := req.tlb_req

  io.resp.valid := false.B
  io.resp.bits := tlb.io.resp

  tlb.io.kill := false.B

  io.exp.interrupt := state === s_interrupt
  tlb.io.sfence.valid := io.exp.flush()
  tlb.io.sfence.bits.rs1 := false.B
  tlb.io.sfence.bits.rs2 := false.B
  tlb.io.sfence.bits.addr := DontCare
  tlb.io.sfence.bits.asid := DontCare

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := req.status


  when (io.req.fire() || state === s_waiting_for_resp) {
    // We could actually check the response from the TLB instantaneously to get a response in the same cycle. However,
    // our current arbiters don't play well with that scenario. To get the instantaneous response, simple erase the
    // "state === s_idle" condition from the "elsewhen" below

    val miss = tlb.io.resp.miss
    val exception = Mux(req.tlb_req.cmd === M_XRD, tlb.io.resp.pf.ld || tlb.io.resp.ae.ld, tlb.io.resp.pf.st || tlb.io.resp.ae.st)

    when (exception) {
      state := s_interrupt
    }.elsewhen (miss || state === s_idle) {
      state := s_waiting_for_resp
    }.otherwise {
      io.resp.valid := true.B
      state := s_idle
    }
  }

  when (state === s_interrupt) {
    when (tlb.io.sfence.fire()) {
      io.resp.valid := io.exp.flush_skip
      state := Mux(io.exp.flush_retry, s_waiting_for_resp, s_idle)
    }
  }

  assert(!io.exp.flush_retry || !io.exp.flush_skip, "TLB: flushing with both retry and skip at same time")
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  // val req = Decoupled(new TLBReq(lgMaxSize))
  val req = Decoupled(new DecoupledTLBReq(lgMaxSize))
  val resp = Flipped(Valid(new TLBResp))
}

class FrontendTLB(nClients: Int, entries: Int, maxSize: Int)
                 (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = new TLBPTWIO
    val exp = new TLBExceptionIO
  })

  val lgMaxSize = log2Ceil(coreDataBytes)
  // val tlbArb = Module(new InOrderArbiter(new TLBReq(lgMaxSize), new TLBResp, nClients))
  val tlbArb = Module(new InOrderArbiter(new DecoupledTLBReq(lgMaxSize), new TLBResp, nClients))
  val tlb = Module(new DecoupledTLB(entries, maxSize))
  tlb.io.req <> tlbArb.io.out_req

  // tlbArb.io.out_resp <> tlb.io.resp
  tlbArb.io.out_resp.valid := tlb.io.resp.valid
  tlbArb.io.out_resp.bits := tlb.io.resp.bits

  io.ptw <> tlb.io.ptw
  io.exp <> tlb.io.exp

  tlbArb.io.in_req <> io.clients.map(_.req)
  io.clients.zip(tlbArb.io.in_resp).foreach {
    case (client, arb_resp) =>
      // client.resp <> arb_resp
      client.resp.valid := arb_resp.valid
      client.resp.bits := arb_resp.bits
      arb_resp.ready := true.B
  }
}

/*class TLBArb (nClients: Int, lgMaxSize: Int)(implicit p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val in_req = Vec(nClients, Flipped(Decoupled(new TLBReq(lgMaxSize))))
    val in_resp = Vec(nClients, Flipped(Valid(new TLBResp)))
    val out_req = Decoupled(new TLBReq(lgMaxSize))
    val out_resp = Valid(new TLBResp)
  })

  val priority = Reg(UInt(log2Up(nClients).W))
}*/

package gemmini

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreBundle, CoreModule}
import freechips.rocketchip.tilelink.TLEdgeOut

import Util._

import midas.targetutils.PerfCounter

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
class DecoupledTLB(entries: Int, maxSize: Int, use_firesim_simulation_counters: Boolean)(implicit edge: TLEdgeOut, p: Parameters)
  extends CoreModule {

  val lgMaxSize = log2Ceil(maxSize)
  val io = new Bundle {
    val req = Flipped(Valid(new DecoupledTLBReq(lgMaxSize)))
    val resp = new TLBResp
    val ptw = new TLBPTWIO

    val exp = new TLBExceptionIO

    val counter = new CounterEventIO()
  }

  val interrupt = RegInit(false.B)
  io.exp.interrupt := interrupt

  val tlb = Module(new TLB(false, lgMaxSize, TLBConfig(nSets=1, nWays=entries)))
  tlb.io.req.valid := io.req.valid
  tlb.io.req.bits := io.req.bits.tlb_req
  io.resp := tlb.io.resp
  tlb.io.kill := false.B

  tlb.io.sfence.valid := io.exp.flush()
  tlb.io.sfence.bits.rs1 := false.B
  tlb.io.sfence.bits.rs2 := false.B
  tlb.io.sfence.bits.addr := DontCare
  tlb.io.sfence.bits.asid := DontCare
  tlb.io.sfence.bits.hv := false.B
  tlb.io.sfence.bits.hg := false.B

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := io.req.bits.status
  val exception = io.req.valid && Mux(io.req.bits.tlb_req.cmd === M_XRD, tlb.io.resp.pf.ld || tlb.io.resp.ae.ld, tlb.io.resp.pf.st || tlb.io.resp.ae.st)
  when (exception) { interrupt := true.B }
  when (interrupt && tlb.io.sfence.fire) {
    interrupt := false.B
  }

  assert(!io.exp.flush_retry || !io.exp.flush_skip, "TLB: flushing with both retry and skip at same time")

  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_HIT_REQ, io.req.fire && !tlb.io.resp.miss)
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_TOTAL_REQ, io.req.fire)
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_MISS_CYCLE, tlb.io.resp.miss)

  if (use_firesim_simulation_counters) {
    PerfCounter(io.req.fire && !tlb.io.resp.miss, "tlb_hits", "total number of tlb hits")
    PerfCounter(io.req.fire, "tlb_reqs", "total number of tlb reqs")
    PerfCounter(tlb.io.resp.miss, "tlb_miss_cycles", "total number of cycles where the tlb is resolving a miss")
  }
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  // val req = Decoupled(new TLBReq(lgMaxSize))
  val req = Valid(new DecoupledTLBReq(lgMaxSize))
  val resp = Flipped(new TLBResp)
}

class FrontendTLB(nClients: Int, entries: Int, maxSize: Int, use_tlb_register_filter: Boolean, use_firesim_simulation_counters: Boolean, use_shared_tlb: Boolean)
                 (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {

  val num_tlbs = if (use_shared_tlb) 1 else nClients
  val lgMaxSize = log2Ceil(coreDataBytes)

  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = Vec(num_tlbs, new TLBPTWIO)
    val exp = Vec(num_tlbs, new TLBExceptionIO)
    val counter = new CounterEventIO()
  })

  val tlbs = Seq.fill(num_tlbs)(Module(new DecoupledTLB(entries, maxSize, use_firesim_simulation_counters)))

  io.ptw <> VecInit(tlbs.map(_.io.ptw))
  io.exp <> VecInit(tlbs.map(_.io.exp))

  val tlbArbOpt = if (use_shared_tlb) Some(Module(new RRArbiter(new DecoupledTLBReq(lgMaxSize), nClients))) else None

  if (use_shared_tlb) {
    val tlbArb = tlbArbOpt.get
    val tlb = tlbs.head
    tlb.io.req.valid := tlbArb.io.out.valid
    tlb.io.req.bits := tlbArb.io.out.bits
    tlbArb.io.out.ready := true.B
  }

  io.clients.zipWithIndex.foreach { case (client, i) =>
    val last_translated_valid = RegInit(false.B)
    val last_translated_vpn = RegInit(0.U(vaddrBits.W))
    val last_translated_ppn = RegInit(0.U(paddrBits.W))

    val l0_tlb_hit = last_translated_valid && ((client.req.bits.tlb_req.vaddr >> pgIdxBits).asUInt() === (last_translated_vpn >> pgIdxBits).asUInt())
    val l0_tlb_paddr = Cat(last_translated_ppn >> pgIdxBits, client.req.bits.tlb_req.vaddr(pgIdxBits-1,0))

    val tlb = if (use_shared_tlb) tlbs.head else tlbs(i)
    val tlbReq = if (use_shared_tlb) tlbArbOpt.get.io.in(i).bits else tlb.io.req.bits
    val tlbReqValid = if (use_shared_tlb) tlbArbOpt.get.io.in(i).valid else tlb.io.req.valid
    val tlbReqFire = if (use_shared_tlb) tlbArbOpt.get.io.in(i).fire else tlb.io.req.fire

    tlbReqValid := RegNext(client.req.valid && !l0_tlb_hit)
    tlbReq := RegNext(client.req.bits)

    when (tlbReqFire && !tlb.io.resp.miss) {
      last_translated_valid := true.B
      last_translated_vpn := tlbReq.tlb_req.vaddr
      last_translated_ppn := tlb.io.resp.paddr
    }

    when (tlb.io.exp.flush()) {
      last_translated_valid := false.B
    }

    when (tlbReqFire) {
      client.resp := tlb.io.resp
    }.otherwise {
      client.resp := DontCare
      client.resp.paddr := RegNext(l0_tlb_paddr)
      client.resp.miss := !RegNext(l0_tlb_hit)
    }

    // If we're not using the TLB filter register, then we set this value to always be false
    if (!use_tlb_register_filter) {
      last_translated_valid := false.B
    }
  }

  // TODO Return the sum of the TLB counters, rather than just the counters of the first TLB. This only matters if we're
  // not using the shared TLB
  tlbs.foreach(_.io.counter.external_reset := false.B)
  io.counter.collect(tlbs.head.io.counter)
}

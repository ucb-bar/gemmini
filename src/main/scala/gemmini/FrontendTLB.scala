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
    val req = Flipped(Valid(new DecoupledTLBReq(lgMaxSize)))
    val resp = new TLBResp
    val ptw = new TLBPTWIO

    val exp = new TLBExceptionIO
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

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := io.req.bits.status
  val exception = io.req.valid && Mux(io.req.bits.tlb_req.cmd === M_XRD, tlb.io.resp.pf.ld || tlb.io.resp.ae.ld, tlb.io.resp.pf.st || tlb.io.resp.ae.st)
  when (exception) { interrupt := true.B }
  when (interrupt && tlb.io.sfence.fire()) {
    interrupt := false.B
  }

  assert(!io.exp.flush_retry || !io.exp.flush_skip, "TLB: flushing with both retry and skip at same time")
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  // val req = Decoupled(new TLBReq(lgMaxSize))
  val req = Valid(new DecoupledTLBReq(lgMaxSize))
  val resp = Flipped(new TLBResp)
}

class FrontendTLB(nClients: Int, entries: Int, maxSize: Int)
                 (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = new TLBPTWIO
    val exp = new TLBExceptionIO
  })

  val lgMaxSize = log2Ceil(coreDataBytes)
  val tlbArb = Module(new RRArbiter(new DecoupledTLBReq(lgMaxSize), nClients))
  val tlb = Module(new DecoupledTLB(entries, maxSize))
  tlb.io.req.valid := tlbArb.io.out.valid
  tlb.io.req.bits := tlbArb.io.out.bits
  tlbArb.io.out.ready := true.B

  io.ptw <> tlb.io.ptw
  io.exp <> tlb.io.exp

  io.clients.zip(tlbArb.io.in).foreach { case (client, req) =>
    val last_translated_valid = RegInit(false.B)
    val last_translated_vpn = RegInit(0.U(vaddrBits.W))
    val last_translated_ppn = RegInit(0.U(paddrBits.W))

    val l0_tlb_hit = last_translated_valid && ((client.req.bits.tlb_req.vaddr >> pgIdxBits) === (last_translated_vpn >> pgIdxBits))
    val l0_tlb_paddr = Cat(last_translated_ppn >> pgIdxBits, client.req.bits.tlb_req.vaddr(pgIdxBits-1,0))

    when (req.fire() && !tlb.io.resp.miss) {
      last_translated_valid := true.B
      last_translated_vpn := req.bits.tlb_req.vaddr
      last_translated_ppn := tlb.io.resp.paddr
    }
    when (io.exp.flush()) {
      last_translated_valid := false.B
    }

    req.valid := RegNext(client.req.valid && !l0_tlb_hit)
    req.bits := RegNext(client.req.bits)

    when (!req.fire()) {
      client.resp := DontCare
      client.resp.paddr := RegNext(l0_tlb_paddr)
      client.resp.miss := !RegNext(l0_tlb_hit)
    } .otherwise {
      client.resp := tlb.io.resp
    }
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

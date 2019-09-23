package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.tilelink.TLBundleParameters
import freechips.rocketchip.util.DecoupledHelper
import testchipip.{StreamChannel, TLHelper}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import Util._

class StreamReadRequest(val spad_rows: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(xLen.W)
  val spaddr = UInt(log2Up(spad_rows).W)
  val cmd_id = UInt(8.W) // TODO magic number
}

class StreamReadResponse(val dataWidth: Int, val spad_rows: Int)(implicit p: Parameters) extends CoreBundle {
  val data = UInt(dataWidth.W)
  val spaddr = UInt(log2Up(spad_rows).W)
  val last = Bool()
  val cmd_id = UInt(8.W) // TODO magic number
}

class ReservationBufferTag(val spad_rows: Int, val maxLen: Int, val nCmds: Int = 2) extends Bundle {
  // TODO get nCmds from config
  val spaddr = UInt(log2Up(spad_rows).W)
  val lgLen = UInt(log2Up(log2Ceil(maxLen+1)).W)
  val cmd_id = UInt(log2Up(nCmds).W)
}

class StreamReader(nXacts: Int, beatBits: Int, maxBytes: Int, dataWidth: Int, aligned_to: Int, spad_rows: Int, meshRows: Int)
                  (implicit p: Parameters) extends LazyModule {

  val core = LazyModule(new StreamReaderCore(nXacts, beatBits, maxBytes, dataWidth, aligned_to, spad_rows))
  val node = core.node

  lazy val module = new LazyModuleImp(this) {
    val beatsInSpadRow = dataWidth / beatBits
    val beatsInMaxReq = (maxBytes * 8) / beatBits
    val reserveBufferRowLen = beatsInSpadRow max beatsInMaxReq

    val matsInMaxReq = (maxBytes * 8) / dataWidth // How many matrices' rows can be requested in one request?

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows)))
      val resp = Decoupled(new StreamReadResponse(dataWidth, spad_rows))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
    })

    val resBuffer = Module(new ReservationBuffer(UInt(beatBits.W), new ReservationBufferTag(spad_rows, matsInMaxReq),
      nXacts, reserveBufferRowLen))

    val lineBreaker = Module(new LineBreaker(dataWidth, spad_rows, maxBytes*8, meshRows))

    core.module.io.req <> io.req
    io.tlb <> core.module.io.tlb
    io.busy := core.module.io.busy

    resBuffer.io.alloc <> core.module.io.reserve.alloc
    resBuffer.io.alloc_stream <> core.module.io.reserve.alloc_stream
    resBuffer.io.in <> core.module.io.reserve.in

    lineBreaker.io.in.valid := resBuffer.io.out.valid
    lineBreaker.io.in.bits.data := resBuffer.io.out.bits.data.asUInt()
    lineBreaker.io.in.bits.spaddr := resBuffer.io.out.bits.tag.spaddr
    lineBreaker.io.in.bits.lgLen := resBuffer.io.out.bits.tag.lgLen
    lineBreaker.io.in.bits.cmd_id := resBuffer.io.out.bits.tag.cmd_id
    resBuffer.io.out.ready := lineBreaker.io.in.ready

    io.resp.valid := lineBreaker.io.out.valid
    io.resp.bits.data := lineBreaker.io.out.bits.data
    io.resp.bits.spaddr := lineBreaker.io.out.bits.spaddr
    io.resp.bits.cmd_id := lineBreaker.io.out.bits.cmd_id
    io.resp.bits.last := lineBreaker.io.out.bits.last
    lineBreaker.io.out.ready := io.resp.ready
  }
}

// TODO StreamReaderCore and StreamWriter are actually very alike. Is there some parent class they could both inherit from?
class StreamReaderCore(nXacts: Int, beatBits: Int, maxBytes: Int, dataWidth: Int, aligned_to: Int, spad_rows: Int)
                      (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-reader", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)

    val dataBytes = dataWidth / 8
    val beatBytes = beatBits / 8

    val beatsInSpadRow = dataWidth / beatBits
    val beatsInMaxReq = (maxBytes * 8) / beatBits
    val reserveBufferRowLen = beatsInSpadRow max beatsInMaxReq

    val matsInMaxReq = (maxBytes * 8) / dataWidth // How many matrices' rows can be requested in one request?

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows)))
      val reserve = new ReservationBufferAllocatorIO(UInt(beatBits.W), new ReservationBufferTag(spad_rows, matsInMaxReq),
        nXacts, reserveBufferRowLen)
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
    })

    val (s_idle :: s_translate_req :: s_translate_resp :: s_req_new_block ::
      s_req_blocks :: Nil) = Enum(5)
    val  state = RegInit(s_idle)

    val req = Reg(new StreamReadRequest(spad_rows))

    val vpn = req.vaddr(xLen-1, pgIdxBits)

    val bytesRequested = Reg(UInt(log2Ceil(dataBytes).W)) // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = dataBytes.U - bytesRequested // TODO This needs to take the request length into account

    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy)
    val xactId = OHToUInt(xactOnehot)

    val xactBusy_add = Mux(tl.a.fire(), (1.U << xactId).asUInt(), 0.U)
    val xactBusy_remove = ~Mux(tl.d.fire(), (1.U << tl.d.bits.source).asUInt(), 0.U)
    xactBusy := (xactBusy | xactBusy_add) & xactBusy_remove.asUInt()

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := !xactBusy.andR() && state_machine_ready_for_req
    io.busy := xactBusy.orR

    val send_sizes = (aligned_to to (dataBytes max maxBytes) by aligned_to)
      .reverse // The possible sizes of requests we can send over TileLink
    val send_sizes_lg = send_sizes.map(s => log2Ceil(s).U)
    val send_sizes_valid = send_sizes.map { s =>
      val lgsz = log2Ceil(s)
      val is_aligned = req.vaddr(lgsz - 1, 0) === 0.U

      val across_page_boundary = (req.vaddr + s.U)(xLen - 1, pgIdxBits) =/= vpn

      val is_too_large = s.U > bytesLeft

      is_aligned && !across_page_boundary && !is_too_large
    }
    val send_size = MuxCase(send_sizes.last.U, (send_sizes_valid zip send_sizes).map { case (v, sz) => v -> sz.U })
    val lg_send_size = MuxCase(send_sizes_lg.last, (send_sizes_valid zip send_sizes_lg).map { case (v, lgsz) => v -> lgsz })

    // Address translation
    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.vaddr := Cat(vpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.passthrough := false.B
    io.tlb.req.bits.size := send_size
    io.tlb.req.bits.cmd := M_XRD

    val ppn = RegEnable(io.tlb.resp.bits.paddr(paddrBits-1, pgIdxBits),
      io.tlb.resp.fire())
    val paddr = Cat(ppn, req.vaddr(pgIdxBits-1, 0))

    val last_vpn_translated = RegEnable(vpn, io.tlb.resp.fire())
    val last_vpn_translated_valid = RegInit(false.B) // TODO check for flush

    when (io.tlb.resp.fire()) {
      last_vpn_translated_valid := true.B
      state := s_req_new_block
    }.elsewhen (io.tlb.req.fire()) {
      state := s_translate_resp
    }

    // Firing off TileLink read requests and allocating space inside the reservation buffer for them
    val get = edge.Get(
      fromSource = xactId,
      toAddress = paddr,
      lgSize = lg_send_size
    )._2

    tl.a.valid := (state === s_req_new_block && io.reserve.alloc.ready) || (state === s_req_blocks)
    tl.a.bits := get

    io.reserve.alloc.valid := state === s_req_new_block && tl.a.ready
    io.reserve.alloc.bits.tag.spaddr := req.spaddr
    io.reserve.alloc.bits.tag.lgLen := 0.U // TODO
    io.reserve.alloc.bits.tag.cmd_id := req.cmd_id

    io.reserve.alloc_stream.valid := (state === s_req_new_block || state === s_req_blocks) && tl.a.ready
    io.reserve.alloc_stream.bits.xact_id := xactId
    io.reserve.alloc_stream.bits.len := send_size / beatBytes.U
    io.reserve.alloc_stream.bits.last := false.B

    when (tl.a.fire()) {
      val next_vaddr = req.vaddr + send_size
      val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
      req.vaddr := next_vaddr

      bytesRequested := bytesRequested + send_size

      when (send_size >= bytesLeft) {
        // We're done with this request at this point
        state_machine_ready_for_req := true.B
        io.reserve.alloc_stream.bits.last := true.B
        state := s_idle
      }.elsewhen (new_page) {
        state := s_translate_req
      }.otherwise {
        state := s_req_blocks
      }
    }

    // Forward TileLink read responses to the reservation buffer
    tl.d.ready := xactBusy.orR
    io.reserve.in.valid := tl.d.fire()
    io.reserve.in.bits.xact_id := tl.d.bits.source
    io.reserve.in.bits.data := tl.d.bits.data

    // Accepting requests to kick-start the state machine
    when (io.req.fire()) {
      req := io.req.bits
      bytesRequested := 0.U

      val vpn_already_translated = last_vpn_translated_valid &&
        last_vpn_translated === io.req.bits.vaddr(xLen-1, pgIdxBits)
      state := Mux(vpn_already_translated, s_req_new_block, s_translate_req)
    }
  }
}

class StreamWriteRequest(val dataWidth: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(xLen.W)
  val data = UInt(dataWidth.W)
}

class StreamWriter(nXacts: Int, beatBits: Int, maxBytes: Int, dataWidth: Int, aligned_to: Int)
                  (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-writer", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)
    val dataBytes = dataWidth / 8
    val beatBytes = beatBits / 8

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamWriteRequest(dataWidth)))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
    })

    val (s_idle :: s_translate_req :: s_translate_resp ::
      s_writing_new_block :: s_writing_beats :: Nil) = Enum(5)
    val state = RegInit(s_idle)

    val req = Reg(new StreamWriteRequest(dataWidth))

    val vpn = req.vaddr(xLen-1, pgIdxBits)

    val bytesSent = Reg(UInt(log2Ceil(dataBytes).W))  // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = dataBytes.U - bytesSent

    val beatsLeft = Reg(UInt(log2Ceil(maxBytes/aligned_to).W))

    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy)
    val xactId = OHToUInt(xactOnehot)

    val xactBusy_add = Mux(tl.a.fire(), (1.U << xactId).asUInt(), 0.U)
    val xactBusy_remove = ~Mux(tl.d.fire(), (1.U << tl.d.bits.source).asUInt(), 0.U)
    xactBusy := (xactBusy | xactBusy_add) & xactBusy_remove.asUInt()

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := !xactBusy.andR() && state_machine_ready_for_req
    io.busy := xactBusy.orR

    val send_sizes = (aligned_to to (dataBytes max maxBytes) by aligned_to).
      filter(s => s < beatBytes || s % beatBytes == 0)
      .reverse // The possible sizes of requests we can send over TileLink
    val send_sizes_lg = send_sizes.map(s => log2Ceil(s).U)
    val send_sizes_valid = send_sizes.map { s =>
      val lgsz = log2Ceil(s)
      val is_aligned = req.vaddr(lgsz - 1, 0) === 0.U

      val across_page_boundary = (req.vaddr + s.U)(xLen - 1, pgIdxBits) =/= vpn

      val is_too_large = s.U > bytesLeft

      is_aligned && !across_page_boundary && !is_too_large
    }
    val send_size = MuxCase(send_sizes.last.U, (send_sizes_valid zip send_sizes).map { case (v, sz) => v -> sz.U })
    val lg_send_size = MuxCase(send_sizes_lg.last, (send_sizes_valid zip send_sizes_lg).map { case (v, lgsz) => v -> lgsz })

    // Address translation
    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.vaddr := Cat(vpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.passthrough := false.B
    io.tlb.req.bits.size := send_size
    io.tlb.req.bits.cmd := M_XWR

    val ppn = RegEnable(io.tlb.resp.bits.paddr(paddrBits-1, pgIdxBits),
      io.tlb.resp.fire())
    val paddr = Cat(ppn, req.vaddr(pgIdxBits-1, 0))

    val last_vpn_translated = RegEnable(vpn, io.tlb.resp.fire())
    val last_vpn_translated_valid = RegInit(false.B) // TODO check for flush

    when (io.tlb.resp.fire()) {
      last_vpn_translated_valid := true.B
      state := s_writing_new_block
    }.elsewhen (io.tlb.req.fire()) {
      state := s_translate_resp
    }

    // Firing off TileLink write requests
    val putFull = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = RegEnableThru(paddr, state === s_writing_new_block),
      lgSize = RegEnableThru(lg_send_size, state === s_writing_new_block),
      data = (req.data >> bytesSent).asUInt())._2

    tl.a.valid := state === s_writing_new_block || state === s_writing_beats
    tl.a.bits := putFull
    tl.d.ready := xactBusy.orR

    when (tl.a.fire()) {
      when (state === s_writing_new_block) {
        val totalBeats = (send_size / beatBytes.U) + (send_size % beatBytes.U)
        beatsLeft := totalBeats - 1.U

        val next_vaddr = req.vaddr + send_size
        val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
        req.vaddr := next_vaddr

        when (totalBeats === 1.U) {
          bytesSent := bytesSent + send_size

          when (send_size >= bytesLeft) {
            // We're done with this request at this point
            state_machine_ready_for_req := true.B
            state := s_idle
          }.elsewhen (new_page) {
            state := s_translate_req
          }
        }.otherwise {
          bytesSent := bytesSent + beatBytes.U
          state := s_writing_beats
        }
      }.elsewhen(state === s_writing_beats) {
        beatsLeft := beatsLeft - 1.U
        bytesSent := bytesSent + beatBytes.U

        when (beatsLeft === 0.U) {
          val new_page = req.vaddr(pgIdxBits-1, 0) === 0.U

          when (beatBytes.U >= bytesLeft) {
            // We're done with this request at this point
            state_machine_ready_for_req := true.B
            state := s_idle
          }.elsewhen(new_page) {
            state := s_translate_req
          }.otherwise {
            state := s_writing_new_block
          }
        }
      }
    }

    // Accepting requests to kick-start the state machine
    when (io.req.fire()) {
      req := io.req.bits
      bytesSent := 0.U

      val vpn_already_translated = last_vpn_translated_valid &&
        last_vpn_translated === io.req.bits.vaddr(xLen-1, pgIdxBits)
      state := Mux(vpn_already_translated, s_writing_new_block, s_translate_req)
    }
  }
}

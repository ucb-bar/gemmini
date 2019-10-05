package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import testchipip.TLHelper
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import Util._

class StreamReadRequest(val spad_rows: Int, val acc_rows: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(xLen.W)
  val spaddr = UInt(log2Up(spad_rows max acc_rows).W)
  val is_acc = Bool()
  val len = UInt(16.W) // TODO magic number
  val cmd_id = UInt(8.W) // TODO magic number
}

class StreamReadResponse(val spadWidth: Int, val accWidth: Int, val spad_rows: Int, val acc_rows: Int,
                         val aligned_to: Int) (implicit p: Parameters) extends CoreBundle {
  val data = UInt((spadWidth max accWidth).W)
  val addr = UInt(log2Up(spad_rows max acc_rows).W)
  val mask = Vec((spadWidth max accWidth) / (aligned_to * 8) max 1, Bool())
  val is_acc = Bool()
  val last = Bool()
  val lgLen = UInt(8.W) // TODO magic number
  val cmd_id = UInt(8.W) // TODO magic number
}

class StreamReader(nXacts: Int, beatBits: Int, maxBytes: Int, spadWidth: Int, accWidth: Int, aligned_to: Int,
                   spad_rows: Int, acc_rows: Int, meshRows: Int)
                  (implicit p: Parameters) extends LazyModule {
  val core = LazyModule(new StreamReaderCore(nXacts, beatBits, maxBytes, spadWidth, accWidth, aligned_to, spad_rows, acc_rows, meshRows))
  val node = core.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows, acc_rows)))
      val resp = Decoupled(new StreamReadResponse(spadWidth, accWidth, spad_rows, acc_rows, aligned_to))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
    })

    val xactTracker = Module(new XactTracker(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes))

    val beatPacker = Module(new BeatPacker(beatBits, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes, aligned_to, meshRows))

    core.module.io.req <> io.req
    io.tlb <> core.module.io.tlb
    io.busy := xactTracker.io.busy

    xactTracker.io.alloc <> core.module.io.reserve
    xactTracker.io.peek.xactid := RegEnableThru(core.module.io.beatData.bits.xactid, beatPacker.io.req.fire())
    xactTracker.io.peek.pop := beatPacker.io.in.fire() && core.module.io.beatData.bits.last

    core.module.io.beatData.ready := beatPacker.io.in.ready
    beatPacker.io.req.valid := core.module.io.beatData.valid
    beatPacker.io.req.bits := xactTracker.io.peek.entry
    beatPacker.io.in.valid := core.module.io.beatData.valid
    beatPacker.io.in.bits := core.module.io.beatData.bits.data

    beatPacker.io.out.ready := io.resp.ready
    io.resp.valid := beatPacker.io.out.valid
    io.resp.bits.data := beatPacker.io.out.bits.data
    io.resp.bits.addr := beatPacker.io.out.bits.addr
    io.resp.bits.mask := beatPacker.io.out.bits.mask
    io.resp.bits.is_acc := beatPacker.io.out.bits.is_acc
    io.resp.bits.cmd_id := xactTracker.io.peek.entry.cmd_id
    io.resp.bits.lgLen := xactTracker.io.peek.entry.lgLen
    io.resp.bits.last := beatPacker.io.out.bits.last
  }
}

class StreamReadBeat (val nXacts: Int, val beatBits: Int) extends Bundle {
  val xactid = UInt(log2Up(nXacts).W)
  val data = UInt(beatBits.W)
  // val first = Bool()
  val last = Bool()
}

// TODO StreamReaderCore and StreamWriter are actually very alike. Is there some parent class they could both inherit from?
class StreamReaderCore(nXacts: Int, beatBits: Int, maxBytes: Int, spadWidth: Int, accWidth: Int,
                       aligned_to: Int, spad_rows: Int, acc_rows: Int, meshRows: Int)
                      (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-reader", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)

    val spadWidthBytes = spadWidth / 8
    val accWidthBytes = accWidth / 8
    val beatBytes = beatBits / 8

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows, acc_rows)))
      val reserve = new XactTrackerAllocIO(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes)
      val beatData = Decoupled(new StreamReadBeat(nXacts, beatBits))
      val tlb = new FrontendTLBIO
    })

    val (s_idle :: s_translate_req :: s_translate_resp :: s_req_new_block ::
      /*s_req_blocks ::*/ Nil) = Enum(4)
    val  state = RegInit(s_idle)

    val req = Reg(new StreamReadRequest(spad_rows, acc_rows))

    val vpn = req.vaddr(xLen-1, pgIdxBits)

    val bytesRequested = Reg(UInt(log2Ceil(spadWidthBytes max accWidthBytes max maxBytes).W)) // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = Mux(req.is_acc, req.len * accWidthBytes.U, req.len * spadWidthBytes.U) - bytesRequested // TODO make "len" "lgLen" to get rid of the multiplier

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := state_machine_ready_for_req

    val send_sizes = (aligned_to to (spadWidthBytes max accWidthBytes max maxBytes) by aligned_to)
      .filter(s => isPow2(s))
      .reverse // The possible sizes of requests we can send over TileLink
    val send_sizes_lg = send_sizes.map(s => log2Ceil(s).U)
    val send_sizes_valid = send_sizes.map { s =>
      val lgsz = log2Ceil(s)
      val is_aligned = if (s == 1) { true.B } else { req.vaddr(lgsz - 1, 0) === 0.U }

      val across_page_boundary = (req.vaddr + s.U - 1.U)(xLen - 1, pgIdxBits) =/= vpn

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
      fromSource = io.reserve.xactid,
      toAddress = paddr,
      lgSize = lg_send_size
    )._2

    tl.a.valid := state === s_req_new_block && io.reserve.ready
    tl.a.bits := get

    io.reserve.valid := state === s_req_new_block && tl.a.ready // TODO decouple "reserve.valid" from "tl.a.ready"
    io.reserve.entry.shift := 0.U // TODO
    io.reserve.entry.is_acc := req.is_acc
    io.reserve.entry.lgLen := lg_send_size
    io.reserve.entry.cmd_id := req.cmd_id

    io.reserve.entry.addr := req.spaddr + meshRows.U *
      Mux(req.is_acc, bytesRequested / accWidthBytes.U, bytesRequested / spadWidthBytes.U)
    io.reserve.entry.spad_row_offset := Mux(req.is_acc, bytesRequested % accWidthBytes.U, bytesRequested % spadWidthBytes.U)

    when (tl.a.fire()) {
      val next_vaddr = req.vaddr + send_size
      val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
      req.vaddr := next_vaddr

      bytesRequested := bytesRequested + send_size

      when (send_size >= bytesLeft) {
        // We're done with this request at this point
        state_machine_ready_for_req := true.B
        state := s_idle
      }.elsewhen (new_page) {
        state := s_translate_req
      }/*.otherwise {
        state := s_req_blocks
      }*/
    }

    // Forward TileLink read responses to the reservation buffer
    tl.d.ready := io.beatData.ready
    io.beatData.valid := tl.d.valid
    io.beatData.bits.xactid := tl.d.bits.source
    io.beatData.bits.data := tl.d.bits.data
    // io.beatData.bits.last := edge.first(tl.d)
    io.beatData.bits.last := edge.last(tl.d)
    // TODO the size data is already returned from TileLink, so there's no need for us to store it in the XactTracker ourselves

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
    val lgBeatBytes = log2Ceil(beatBytes)

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
    io.req.ready := !xactBusy.andR() && state_machine_ready_for_req // TODO this doesn't really need to check if xactBusy has any open spots. We can delay that check till we're actually writing over TileLink
    io.busy := xactBusy.orR

    val send_sizes = (aligned_to to (dataBytes max maxBytes) by aligned_to).
      filter(s => isPow2(s)).
      filter(s => s < beatBytes || s % beatBytes == 0)
      .reverse // The possible sizes of requests we can send over TileLink
    val send_sizes_lg = send_sizes.map(s => log2Ceil(s).U)
    val send_sizes_valid = send_sizes.map { s =>
      val lgsz = log2Ceil(s)
      val is_aligned = if (s == 1) { true.B } else { req.vaddr(lgsz - 1, 0) === 0.U }

      val across_page_boundary = (req.vaddr + s.U - 1.U)(xLen - 1, pgIdxBits) =/= vpn

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
    val paddr_aligned = Cat(paddr(paddrBits-1, lgBeatBytes), 0.U(lgBeatBytes.W)) // Aligned to beatBytes
    val paddr_offset = paddr(lgBeatBytes-1, 0)

    val last_vpn_translated = RegEnable(vpn, io.tlb.resp.fire())
    val last_vpn_translated_valid = RegInit(false.B) // TODO check for flush

    when (io.tlb.resp.fire()) {
      last_vpn_translated_valid := true.B
      state := s_writing_new_block
    }.elsewhen (io.tlb.req.fire()) {
      state := s_translate_resp
    }

    /*val first_fire = RegInit(false.B)
    when (tl.a.fire()) {
      first_fire := true.B
    }*/

    // Firing off TileLink write requests
    val putFull = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = RegEnableThru(paddr_aligned, state === s_writing_new_block),
      lgSize = RegEnableThru(lg_send_size, state === s_writing_new_block),
      // data = Mux(!first_fire, (req.data >> (bytesSent * 8.U)).asUInt(), 0.U))._2
      data = (req.data >> (bytesSent * 8.U)).asUInt())._2

    val mask = Cat((0 until beatBytes).map(i => i.U >= paddr_offset && i.U < paddr_offset + send_size).reverse)

    val putPartial = edge.Put(
      fromSource = xactId,
      toAddress = paddr_aligned,
      lgSize = lgBeatBytes.U, // lg_send_size,
      // data = Mux(!first_fire, ((req.data >> (bytesSent * 8.U)) << (paddr_offset * 8.U)).asUInt(), 0.U),
      data = ((req.data >> (bytesSent * 8.U)) << (paddr_offset * 8.U)).asUInt(),
      mask =  mask)._2

    tl.a.valid := (state === s_writing_new_block || state === s_writing_beats) && !xactBusy.andR()
    tl.a.bits := Mux(RegEnableThru(send_size < beatBytes.U, state === s_writing_new_block), putPartial, putFull)
    tl.d.ready := xactBusy.orR

    when (tl.a.fire()) {
      when (state === s_writing_new_block) {
        val totalBeats = (send_size / beatBytes.U) + Mux(send_size % beatBytes.U =/= 0.U, 1.U, 0.U)
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

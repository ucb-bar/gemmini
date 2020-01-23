package gemmini

import chisel3._
import chisel3.util._
import chisel3.core.withReset

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import testchipip.TLHelper
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.rocket.constants.MemoryOpConstants

import Util._


class StreamReadRequest(val spad_rows: Int, val acc_rows: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val spaddr = UInt(log2Up(spad_rows max acc_rows).W)
  val is_acc = Bool()
  val status = new MStatus
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
  val bytes_read = UInt(8.W) // TODO magic number
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
      val flush = Input(Bool())
    })


    val xactTracker = Module(new XactTracker(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes))

    val beatPacker = Module(new BeatMerger(beatBits, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes, aligned_to, meshRows))

    core.module.io.req <> io.req
    io.tlb <> core.module.io.tlb
    io.busy := xactTracker.io.busy
    core.module.io.flush := io.flush

    xactTracker.io.alloc <> core.module.io.reserve
    xactTracker.io.peek.xactid := RegEnableThru(core.module.io.beatData.bits.xactid, beatPacker.io.req.fire())
    xactTracker.io.peek.pop := beatPacker.io.in.fire() && core.module.io.beatData.bits.last


    core.module.io.beatData.ready := beatPacker.io.in.ready
    beatPacker.io.req.valid := core.module.io.beatData.valid
    beatPacker.io.req.bits := xactTracker.io.peek.entry
    beatPacker.io.req.bits.lg_len_req := core.module.io.beatData.bits.lg_len_req
    beatPacker.io.in.valid := core.module.io.beatData.valid
    beatPacker.io.in.bits := core.module.io.beatData.bits.data

    beatPacker.io.out.ready := io.resp.ready
    io.resp.valid := beatPacker.io.out.valid
    io.resp.bits.data := beatPacker.io.out.bits.data
    io.resp.bits.addr := beatPacker.io.out.bits.addr
    io.resp.bits.mask := beatPacker.io.out.bits.mask
    io.resp.bits.is_acc := beatPacker.io.out.bits.is_acc
    io.resp.bits.cmd_id := RegEnable(xactTracker.io.peek.entry.cmd_id, beatPacker.io.req.fire())
    io.resp.bits.bytes_read := RegEnable(xactTracker.io.peek.entry.bytes_to_read, beatPacker.io.req.fire())
    io.resp.bits.last := beatPacker.io.out.bits.last
  }
}

class StreamReadBeat (val nXacts: Int, val beatBits: Int, val maxReqBytes: Int) extends Bundle {
  val xactid = UInt(log2Up(nXacts).W)
  val data = UInt(beatBits.W)
  val lg_len_req = UInt(log2Up(log2Up(maxReqBytes+1)+1).W)
  val last = Bool()
}

// TODO StreamReaderCore and StreamWriter are actually very alike. Is there some parent class they could both inherit from?
class StreamReaderCore(nXacts: Int, beatBits: Int, maxBytes: Int, spadWidth: Int, accWidth: Int,
                       aligned_to: Int, spad_rows: Int, acc_rows: Int, meshRows: Int)
                      (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-reader", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  // TODO when we request data from multiple rows which are actually contiguous in main memory, we should merge them into fewer requests

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)

    val spadWidthBytes = spadWidth / 8
    val accWidthBytes = accWidth / 8
    val beatBytes = beatBits / 8

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows, acc_rows)))
      val reserve = new XactTrackerAllocIO(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes)
      val beatData = Decoupled(new StreamReadBeat(nXacts, beatBits, maxBytes))
      val tlb = new FrontendTLBIO
      val flush = Input(Bool())
    })

    val s_idle :: s_translate_req :: s_translate_resp :: s_req_new_block :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val req = Reg(new StreamReadRequest(spad_rows, acc_rows))

    val vpn = req.vaddr(coreMaxAddrBits-1, pgIdxBits)

    val bytesRequested = Reg(UInt(log2Ceil(spadWidthBytes max accWidthBytes max maxBytes).W)) // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = Mux(req.is_acc, req.len * accWidthBytes.U, req.len * spadWidthBytes.U) - bytesRequested

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := state_machine_ready_for_req

    // Address translation
    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.tlb_req.vaddr := Cat(vpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.tlb_req.passthrough := false.B
    io.tlb.req.bits.tlb_req.size := 0.U
    io.tlb.req.bits.tlb_req.cmd := M_XRD
    io.tlb.req.bits.status := req.status

    val ppn = RegEnable(io.tlb.resp.bits.paddr(paddrBits-1, pgIdxBits),
      io.tlb.resp.fire())
    val paddr = Cat(ppn, req.vaddr(pgIdxBits-1, 0))

    val last_vpn_translated = RegEnable(vpn, io.tlb.resp.fire())
    val last_vpn_translated_valid = withReset(reset.toBool() || io.flush) { RegInit(false.B) }

    when (io.tlb.resp.fire()) {
      last_vpn_translated_valid := true.B
      state := s_req_new_block
    }.elsewhen (io.tlb.req.fire()) {
      state := s_translate_resp
    }

    // Select the size and mask of the TileLink request
    class Packet extends Bundle {
      val size = UInt(log2Up(maxBytes+1).W)
      val lg_size = UInt(log2Ceil(log2Ceil(maxBytes+1)).W)
      val bytes_read = UInt(log2Up(maxBytes+1).W)
      val shift = UInt(log2Up(maxBytes).W)
      val paddr = UInt(paddrBits.W)
    }

    val read_sizes = ((aligned_to max beatBytes) to maxBytes by aligned_to).
      filter(s => isPow2(s)).
      filter(s => s % beatBytes == 0)
    val read_packets = read_sizes.map { s =>
      val lg_s = log2Ceil(s)
      val paddr_aligned_to_size = if (s == 1) paddr else Cat(paddr(paddrBits-1, lg_s), 0.U(lg_s.W))
      val paddr_offset = if (s > 1) paddr(lg_s-1, 0) else 0.U

      val packet = Wire(new Packet())
      packet.size := s.U
      packet.lg_size := lg_s.U
      packet.bytes_read := minOf(s.U - paddr_offset, bytesLeft)
      packet.shift := paddr_offset
      packet.paddr := paddr_aligned_to_size

      packet
    }
    val read_packet = read_packets.reduce { (acc, p) =>
      Mux(p.bytes_read > acc.bytes_read, p, acc)
    }
    val read_paddr = read_packet.paddr
    val read_lg_size = read_packet.lg_size
    val read_bytes_read = read_packet.bytes_read
    val read_shift = read_packet.shift


    // Firing off TileLink read requests and allocating space inside the reservation buffer for them
    val get = edge.Get(
      fromSource = io.reserve.xactid,
      toAddress = read_paddr,
      lgSize = read_lg_size
    )._2

    tl.a.valid := state === s_req_new_block && io.reserve.ready
    tl.a.bits := get

    io.reserve.valid := state === s_req_new_block && tl.a.ready // TODO decouple "reserve.valid" from "tl.a.ready"
    io.reserve.entry.shift := read_shift
    io.reserve.entry.is_acc := req.is_acc
    // io.reserve.entry.lg_len_req := read_lg_size
    io.reserve.entry.lg_len_req := DontCare // TODO just remove this from the IO completely
    io.reserve.entry.bytes_to_read := read_bytes_read
    io.reserve.entry.cmd_id := req.cmd_id

    io.reserve.entry.addr := req.spaddr + meshRows.U *
      Mux(req.is_acc,
        // We only add "if" statements here to satisfy the Verilator linter. The code would be cleaner without the
        // "if" condition and the "else" clause
        if (bytesRequested.getWidth >= log2Up(accWidthBytes+1)) bytesRequested / accWidthBytes.U else 0.U,
        if (bytesRequested.getWidth >= log2Up(spadWidthBytes+1)) bytesRequested / spadWidthBytes.U else 0.U)
    io.reserve.entry.spad_row_offset := Mux(req.is_acc, bytesRequested % accWidthBytes.U, bytesRequested % spadWidthBytes.U)

    when (tl.a.fire()) {
      val next_vaddr = req.vaddr + read_bytes_read // send_size
      val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
      req.vaddr := next_vaddr

      bytesRequested := bytesRequested + read_bytes_read // send_size

      // when (send_size >= bytesLeft) {
      when (read_bytes_read >= bytesLeft) {
        // We're done with this request at this point
        state_machine_ready_for_req := true.B
        state := s_idle
      }.elsewhen (new_page) {
        state := s_translate_req
      }
    }

    // Forward TileLink read responses to the reservation buffer
    tl.d.ready := io.beatData.ready
    io.beatData.valid := tl.d.valid
    io.beatData.bits.xactid := tl.d.bits.source
    io.beatData.bits.data := tl.d.bits.data
    io.beatData.bits.lg_len_req := tl.d.bits.size
    io.beatData.bits.last := edge.last(tl.d)
    // TODO the size data is already returned from TileLink, so there's no need for us to store it in the XactTracker ourselves

    // Accepting requests to kick-start the state machine
    when (io.req.fire()) {
      req := io.req.bits
      bytesRequested := 0.U

      val vpn_already_translated = last_vpn_translated_valid &&
        last_vpn_translated === io.req.bits.vaddr(coreMaxAddrBits-1, pgIdxBits)
      state := Mux(vpn_already_translated, s_req_new_block, s_translate_req)
    }
  }
}

class StreamWriteRequest(val dataWidth: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val data = UInt(dataWidth.W)
  val status = new MStatus
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
    val maxBeatsPerReq = maxBytes / beatBytes

    require(beatBytes > 0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamWriteRequest(dataWidth)))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
      val flush = Input(Bool())
    })

    val (s_idle :: s_translate_req :: s_translate_resp ::
      s_writing_new_block :: s_writing_beats :: Nil) = Enum(5)
    val state = RegInit(s_idle)

    val req = Reg(new StreamWriteRequest(dataWidth))

    val vpn = req.vaddr(coreMaxAddrBits-1, pgIdxBits)

    val bytesSent = Reg(UInt(log2Ceil(dataBytes).W))  // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = dataBytes.U - bytesSent

    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy)
    val xactId = OHToUInt(xactOnehot)

    val xactBusy_add = Mux(tl.a.fire(), (1.U << xactId).asUInt(), 0.U)
    val xactBusy_remove = ~Mux(tl.d.fire(), (1.U << tl.d.bits.source).asUInt(), 0.U)
    xactBusy := (xactBusy | xactBusy_add) & xactBusy_remove.asUInt()

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := state_machine_ready_for_req
    io.busy := xactBusy.orR

    // Address translation
    io.tlb.req.valid := state === s_translate_req
    io.tlb.req.bits.tlb_req.vaddr := Cat(vpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.tlb_req.passthrough := false.B
    io.tlb.req.bits.tlb_req.size := 0.U // send_size
    io.tlb.req.bits.tlb_req.cmd := M_XWR
    io.tlb.req.bits.status := req.status

    val ppn = RegEnable(io.tlb.resp.bits.paddr(paddrBits-1, pgIdxBits),
      io.tlb.resp.fire())
    val paddr = Cat(ppn, req.vaddr(pgIdxBits-1, 0))

    val last_vpn_translated = RegEnable(vpn, io.tlb.resp.fire())
    val last_vpn_translated_valid = withReset(reset.toBool() || io.flush) { RegInit(false.B) }

    when (io.tlb.resp.fire()) {
      last_vpn_translated_valid := true.B
      state := s_writing_new_block
    }.elsewhen (io.tlb.req.fire()) {
      state := s_translate_resp
    }

    // Select the size and mask of the TileLink request
    class Packet extends Bundle {
      val size = UInt(log2Ceil(maxBytes).W)
      val lg_size = UInt(log2Ceil(log2Ceil(maxBytes)).W)
      val mask = Vec(maxBeatsPerReq, Vec(beatBytes, Bool()))
      val paddr = UInt(paddrBits.W)
      val is_full = Bool()

      def bytes_written(dummy: Int = 0) = PopCount(mask.flatten)
      def total_beats(dummy: Int = 0) = size / beatBytes.U
    }

    val write_sizes = ((aligned_to max beatBytes) to maxBytes by aligned_to).
      filter(s => isPow2(s)).
      filter(s => s % beatBytes == 0).
      filter(s => s <= dataBytes*2)
    val write_packets = write_sizes.map { s =>
      val lg_s = log2Ceil(s)
      val paddr_aligned_to_size = if (s == 1) paddr else Cat(paddr(paddrBits-1, lg_s), 0.U(lg_s.W))

      val mask = (0 until maxBytes).map { i =>
        if (s > 1) {
          val paddr_offset = paddr(lg_s-1, 0)
          i.U >= paddr_offset &&
            i.U < paddr_offset + bytesLeft
        } else {
          true.B
        } && (i < s).B
      }

      val packet = Wire(new Packet())
      packet.size := s.U
      packet.lg_size := lg_s.U
      packet.mask := VecInit(mask.grouped(beatBytes).map(v => VecInit(v)).toSeq)
      packet.paddr := paddr_aligned_to_size
      packet.is_full := mask.take(s).reduce(_ && _)

      packet
    }
    val best_write_packet = write_packets.reduce { (acc, p) =>
      Mux(p.bytes_written() > acc.bytes_written(), p, acc)
    }
    val write_packet = RegEnableThru(best_write_packet, state === s_writing_new_block)

    val write_size = write_packet.size
    val lg_write_size = write_packet.lg_size
    val write_beats = write_packet.total_beats()
    val write_paddr = write_packet.paddr
    val write_full = write_packet.is_full

    val beatsLeft = Reg(UInt(log2Up(maxBytes/aligned_to).W))
    val beatsSent = Mux(state === s_writing_new_block, 0.U, write_beats - beatsLeft)

    val write_mask = write_packet.mask(beatsSent)
    val write_shift = PriorityEncoder(write_mask)

    val bytes_written_this_beat = PopCount(write_mask)

    // Firing off TileLink write requests
    val putFull = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = write_paddr,
      lgSize = lg_write_size,
      data = (req.data >> (bytesSent * 8.U)).asUInt()
    )._2

    val putPartial = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = write_paddr,
      lgSize = lg_write_size,
      data = ((req.data >> (bytesSent * 8.U)) << (write_shift * 8.U)).asUInt(),
      mask = write_mask.asUInt()
    )._2

    tl.a.valid := (state === s_writing_new_block || state === s_writing_beats) && !xactBusy.andR()
    tl.a.bits := Mux(write_full, putFull, putPartial)
    tl.d.ready := xactBusy.orR()

    when (tl.a.fire()) {
      when (state === s_writing_new_block) {
        beatsLeft := write_beats - 1.U

        val next_vaddr = req.vaddr + bytes_written_this_beat
        val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
        req.vaddr := next_vaddr

        bytesSent := bytesSent + bytes_written_this_beat

        when (write_beats === 1.U) {
          when (bytes_written_this_beat >= bytesLeft) {
            // We're done with this request at this point
            state_machine_ready_for_req := true.B
            state := s_idle
          }.elsewhen (new_page) {
            state := s_translate_req
          }
        }.otherwise {
          state := s_writing_beats
        }
      }.elsewhen(state === s_writing_beats) {
        beatsLeft := beatsLeft - 1.U
        bytesSent := bytesSent + bytes_written_this_beat

        when (beatsLeft === 0.U) {
          val new_page = req.vaddr(pgIdxBits-1, 0) === 0.U

          when (bytes_written_this_beat >= bytesLeft) {
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
        last_vpn_translated === io.req.bits.vaddr(coreMaxAddrBits-1, pgIdxBits)
      state := Mux(vpn_already_translated, s_writing_new_block, s_translate_req)
    }
  }
}

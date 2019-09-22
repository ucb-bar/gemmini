package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.tilelink.TLBundleParameters
import freechips.rocketchip.util.DecoupledHelper
// import icenet.{ReservationBufferAlloc, ReservationBufferData}
import testchipip.{StreamChannel, TLHelper}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import Util._

class StreamReadRequest(val nXacts: Int) extends Bundle {
  val address = UInt(48.W)
  val length = UInt(15.W)
  val partial = Bool()
}

class StreamChannelWithID(val nXacts: Int, val dataBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val keep = UInt((dataBits/8).W)
  val last = Bool()
  val id = UInt((log2Ceil(nXacts) max 1).W)
}

/*class StreamReader(nXacts: Int, outFlits: Int, maxBytes: Int)
                  (implicit p: Parameters) extends LazyModule {

  val core = LazyModule(new StreamReaderCore(nXacts, outFlits, maxBytes))
  val node = core.node

  lazy val module = new LazyModuleImp(this) {
    val dataBits = core.module.dataBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(nXacts)))
      val resp = Decoupled(Bool())
      val out = Decoupled(new StreamChannelWithID(nXacts, dataBits))
      val reset_Xacts = Input(Bool()) // TODO inelegant
    })

    core.module.io.req <> io.req
    io.resp <> core.module.io.resp

    core.module.io.alloc.ready := io.out.ready

    io.out.valid := core.module.io.out.valid
    core.module.io.out.ready := io.out.ready
    io.out.bits.data := core.module.io.out.bits.data.data
    io.out.bits.last := core.module.io.out.bits.data.last
    io.out.bits.keep := core.module.io.out.bits.data.keep
    io.out.bits.id := core.module.io.out.bits.id

    core.module.io.reset_Xacts := io.reset_Xacts
  }
}

class StreamReaderCore(nXacts: Int, outFlits: Int, maxBytes: Int)
                      (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-reader", sourceId = IdRange(0, nXacts))

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val dataBits = tl.params.dataBits
    val beatBytes = dataBits / 8
    val byteAddrBits = log2Ceil(beatBytes)
    val addrBits = tl.params.addressBits
    val lenBits = 15

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(nXacts)))
      val resp = Decoupled(Bool())
      val alloc = Decoupled(new ReservationBufferAlloc(nXacts, outFlits))
      val out = Decoupled(new ReservationBufferData(nXacts, dataBits))
      val reset_Xacts = Input(Bool()) // TODO inelegant
    })

    val s_idle :: s_read :: s_resp :: Nil = Enum(3)
    val state = RegInit(s_idle)

    // Physical (word) address in memory
    val sendaddr = Reg(UInt(addrBits.W))
    // Number of words to send
    val sendlen  = Reg(UInt(lenBits.W))
    // 0 if last packet in sequence, 1 otherwise
    val sendpart = Reg(Bool())

    // TODO is xact needed for anything anymore?
    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy).asUInt()
    val xactId = OHToUInt(xactOnehot)
    val xactLast = Reg(UInt(nXacts.W))
    val xactLeftKeep = Reg(Vec(nXacts, UInt(beatBytes.W)))
    val xactRightKeep = Reg(Vec(nXacts, UInt(beatBytes.W)))

    val reqSize = MuxCase(byteAddrBits.U,
      (log2Ceil(maxBytes) until byteAddrBits by -1).map(lgSize =>
        // Use the largest size (beatBytes <= size <= maxBytes)
        // s.t. sendaddr % size == 0 and sendlen > size
        (sendaddr(lgSize-1,0) === 0.U &&
          (sendlen >> lgSize.U).asUInt =/= 0.U) -> lgSize.U))
    val isLast = (xactLast >> tl.d.bits.source)(0) && edge.last(tl.d)
    val canSend = state === s_read && !xactBusy.andR

    val fullKeep = ~0.U(beatBytes.W)
    val loffset = Reg(UInt(byteAddrBits.W))
    val roffset = Reg(UInt(byteAddrBits.W))
    val lkeep = fullKeep << loffset
    val rkeep = fullKeep >> roffset
    val first = Reg(Bool())

    /*xactBusy := (xactBusy | Mux(tl.a.fire(), xactOnehot, 0.U)) &
      (~Mux(tl.d.fire() && edge.last(tl.d),
        UIntToOH(tl.d.bits.source), 0.U)).asUInt()*/

    // TODO this doesn't work if we want to support unaligned accesses
    xactBusy := xactBusy | Mux(tl.a.fire(), xactOnehot, 0.U)
    // when (tl.d.fire() && edge.last(tl.d) && xactBusy.andR()) {
    when (io.reset_Xacts) {
      xactBusy := 0.U
    }

    val helper = DecoupledHelper(tl.a.ready, io.alloc.ready)

    io.req.ready := !xactBusy.andR() // && (state === s_idle || state === s_resp) // state === s_idle // TODO this needs to be more sophisticated for general usage
    io.alloc.valid := helper.fire(io.alloc.ready, canSend)
    io.alloc.bits.id := xactId
    io.alloc.bits.count := (1.U << (reqSize - byteAddrBits.U))
    tl.a.valid := helper.fire(tl.a.ready, canSend)
    tl.a.bits := edge.Get(
      fromSource = xactId,
      toAddress = sendaddr,
      lgSize = reqSize)._2

    val outLeftKeep = xactLeftKeep(tl.d.bits.source)
    val outRightKeep = xactRightKeep(tl.d.bits.source)

    io.out.valid := tl.d.valid
    io.out.bits.id := tl.d.bits.source
    io.out.bits.data.data := tl.d.bits.data
    io.out.bits.data.keep := MuxCase(fullKeep, Seq(
      (edge.first(tl.d) && edge.last(tl.d)) -> (outLeftKeep & outRightKeep),
      edge.first(tl.d) -> outLeftKeep,
      edge.last(tl.d)  -> outRightKeep))
    io.out.bits.data.last := isLast
    tl.d.ready := io.out.ready
    io.resp.valid := state === s_resp
    io.resp.bits := true.B

    when (io.req.fire()) {
      val req = io.req.bits
      val lastaddr = req.address + req.length
      val startword = req.address(addrBits-1, byteAddrBits)
      val endword = lastaddr(addrBits-1, byteAddrBits) +
        Mux(lastaddr(byteAddrBits-1, 0) === 0.U, 0.U, 1.U)

      loffset := req.address(byteAddrBits-1, 0)
      roffset := Cat(endword, 0.U(byteAddrBits.W)) - lastaddr
      first := true.B

      sendaddr := Cat(startword, 0.U(byteAddrBits.W))
      sendlen  := Cat(endword - startword, 0.U(byteAddrBits.W))
      sendpart := req.partial
      state := s_read

      assert(req.length > 0.U, s"request length must be >0")
    }

    when (tl.a.fire()) {
      val reqBytes = (1.U << reqSize).asUInt()
      sendaddr := sendaddr + reqBytes
      sendlen  := sendlen - reqBytes
      when (sendlen === reqBytes) {
        xactLast := (xactLast & (~xactOnehot).asUInt) | Mux(sendpart, 0.U, xactOnehot)
        xactRightKeep(xactId) := rkeep
        state := s_resp
      } .otherwise {
        xactLast := xactLast & (~xactOnehot).asUInt
        xactRightKeep(xactId) := fullKeep
      }
      when (first) {
        first := false.B
        xactLeftKeep(xactId) := lkeep
      } .otherwise {
        xactLeftKeep(xactId) := fullKeep
      }
    }

    when (io.resp.fire()) {
      state := s_idle
    }
  }
}*/

// w is the maximum width of the data that it can be requested to send at once
class StreamWriteRequest(val dataWidth: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(xLen.W)
  val data = UInt(dataWidth.W)
}

class StreamWriter(nXacts: Int, beatBits: Int, maxBytes: Int, dataWidth: Int, aligned_to: Int)
                  (implicit p: Parameters) extends LazyModule with MemoryOpConstants {
  val node = TLHelper.makeClientNode(
    name = "stream-writer", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
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

    val bytesSent = RegInit(0.U(log2Ceil(dataBytes).W))
    val bytesLeft = (dataBytes/aligned_to).U - bytesSent

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

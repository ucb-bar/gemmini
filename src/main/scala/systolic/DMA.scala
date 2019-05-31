package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.util.DecoupledHelper
import icenet.{ReservationBufferAlloc, ReservationBufferData}
import testchipip.{StreamChannel, TLHelper}

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

class StreamReader(nXacts: Int, outFlits: Int, maxBytes: Int)
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

    // val buffer = Module(new ReservationBuffer(nXacts, outFlits, dataBits))
    // buffer.io.alloc <> core.module.io.alloc
    // buffer.io.in <> core.module.io.out

    // val aligner = Module(new Aligner(dataBits))
    // aligner.io.in <> buffer.io.out
    // io.out <> aligner.io.out

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
}

class StreamWriteRequest extends Bundle {
  val address = UInt(48.W)
  val length = UInt(16.W)
}

class StreamWriter(nXacts: Int, maxBytes: Int)
                  (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-writer", sourceId = IdRange(0, nXacts))

  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val dataBits = tl.params.dataBits
    val beatBytes = dataBits / 8
    val byteAddrBits = log2Ceil(beatBytes)
    val addrBits = tl.params.addressBits
    val lenBits = 16

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamWriteRequest))
      val resp = Decoupled(UInt(lenBits.W))
      val in = Flipped(Decoupled(new StreamChannel(dataBits)))
    })

    val s_idle :: s_data :: s_resp :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val length = Reg(UInt(lenBits.W))
    val baseAddr = Reg(UInt(addrBits.W))
    val offset = Reg(UInt(addrBits.W))
    val addrMerged = baseAddr + offset
    val bytesToSend = length - offset
    val baseByteOff = baseAddr(byteAddrBits-1, 0)
    val byteOff = addrMerged(byteAddrBits-1, 0)
    val extraBytes = Mux(baseByteOff === 0.U, 0.U, beatBytes.U - baseByteOff)

    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy)
    val xactId = OHToUInt(xactOnehot)

    val maxBeats = maxBytes / beatBytes
    val beatIdBits = log2Ceil(maxBeats)

    val beatsLeft = Reg(UInt(beatIdBits.W))
    val headAddr = Reg(UInt(addrBits.W))
    val headXact = Reg(UInt(log2Ceil(nXacts).W))
    val headSize = Reg(UInt(log2Ceil(maxBytes + 1).W))

    val newBlock = beatsLeft === 0.U
    val canSend = !xactBusy.andR || !newBlock

    val reqSize = MuxCase(0.U,
      (log2Ceil(maxBytes) until 0 by -1).map(lgSize =>
        (addrMerged(lgSize-1,0) === 0.U &&
          (bytesToSend >> lgSize.U).asUInt =/= 0.U) -> lgSize.U))

    xactBusy := (xactBusy | Mux(tl.a.fire() && newBlock, xactOnehot, 0.U)) &
      (~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)).asUInt

    val overhang = RegInit(0.U(dataBits.W))
    val sendTrail = bytesToSend <= extraBytes
    val fulldata = overhang | (io.in.bits.data << Cat(baseByteOff, 0.U(3.W))).asUInt

    val fromSource = Mux(newBlock, xactId, headXact)
    val toAddress = Mux(newBlock, addrMerged, headAddr)
    val lgSize = Mux(newBlock, reqSize, headSize)
    val wdata = fulldata(dataBits-1, 0)
    val wmask = Cat((0 until beatBytes).map(
      i => (i.U >= byteOff) && (i.U < bytesToSend)).reverse)
    val wpartial = !wmask.andR

    val putPartial = edge.Put(
      fromSource = xactId,
      toAddress = addrMerged & (~(beatBytes-1).U(addrBits.W)).asUInt,
      lgSize = log2Ceil(beatBytes).U,
      data = Mux(sendTrail, overhang, wdata),
      mask = wmask)._2

    val putFull = edge.Put(
      fromSource = fromSource,
      toAddress = toAddress,
      lgSize = lgSize,
      data = wdata)._2

    io.req.ready := !xactBusy.andR() // && (state === s_idle || state === s_resp) // TODO this needs to be more sophisticated for general usage
    tl.a.valid := (state === s_data) && (io.in.valid || sendTrail) && canSend
    tl.a.bits := Mux(wpartial, putPartial, putFull)
    tl.d.ready := xactBusy.orR
    io.in.ready := state === s_data && canSend && !sendTrail && tl.a.ready
    io.resp.valid := state === s_resp && !xactBusy.orR
    io.resp.bits := length

    // TODO allow in and req to fire at the same time
    when (tl.a.fire()) {
      when (!newBlock) {
        beatsLeft := beatsLeft - 1.U
      } .elsewhen (reqSize > byteAddrBits.U) {
        val nBeats = (1.U << (reqSize - byteAddrBits.U)).asUInt()
        beatsLeft := nBeats - 1.U
        headAddr := addrMerged
        headXact := xactId
        headSize := reqSize
      }

      val bytesSent = PopCount(wmask)
      offset := offset + bytesSent
      overhang := fulldata >> dataBits.U

      when (bytesSent === bytesToSend && io.in.bits.last) { state := s_resp }
    }

    when (io.req.fire()) {
      offset := 0.U
      baseAddr := io.req.bits.address
      length := io.req.bits.length
      beatsLeft := 0.U
      state := s_data
    }

    when (io.resp.fire()) { state := s_idle }
  }
}

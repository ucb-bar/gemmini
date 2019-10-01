package systolic

import chisel3._
import chisel3.util._

import systolic.Util._

/*
class BeatPackerInStream(val beatBits: Int) extends Bundle {
  val data = UInt(beatBits.W)
}
*/

class BeatPackerOut(val spadWidth: Int, val spadRows: Int, val alignedTo: Int) extends Bundle {
  val data = UInt(spadWidth.W)
  val addr = UInt(log2Up(spadRows).W)
  val mask = Vec(spadWidth/(alignedTo*8), Bool())
  val last = Bool()
}

/*
  beatBits: in bits
  maxShift: in bytes
  spadWidth: width of spad row in bits
  spadRows: unit-less
  maxReqBytes: in bytes
  aligned_to: in bytes
 */
class BeatPacker(beatBits: Int, maxShift: Int, spadWidth: Int, spadRows: Int, maxReqBytes: Int, alignedTo: Int, meshRows: Int)
  extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new XactTrackerEntry(maxShift, spadWidth, spadRows, maxReqBytes)))
    // val in = Flipped(Decoupled(new BeatPackerInStream(beatBits)))
    val in = Flipped(Decoupled(UInt(beatBits.W)))
    val out = Decoupled(new BeatPackerOut(spadWidth, spadRows, alignedTo))
  })

  val spadWidthBytes = spadWidth/8

  val req = Reg(UDValid(new XactTrackerEntry(maxShift, spadWidth, spadRows, maxReqBytes)))

  val buflen = maxReqBytes max spadWidthBytes // in bytes
  val buffer = Reg(UInt((buflen*8).W))

  val bytesSent = Reg(UInt(log2Up(buflen).W))
  val bytesRead = Reg(UInt(log2Up(buflen+1).W))

  val len = (1.U << Mux(io.req.fire(), io.req.bits.lgLen, req.bits.lgLen)).asUInt()

  io.req.ready := !req.valid

  io.in.ready := io.req.fire() || (req.valid && bytesRead =/= len)

  io.out.valid := req.valid && bytesRead > bytesSent && (bytesRead - bytesSent >= spadWidthBytes.U ||
    bytesRead === (1.U << req.bits.lgLen).asUInt())
  io.out.bits.data := (buffer >> (bytesSent * 8.U)) << (req.bits.spad_row_offset * 8.U)
  io.out.bits.mask := VecInit((0 until spadWidthBytes by alignedTo).map(i =>
    i.U >= req.bits.spad_row_offset &&
      i.U <= req.bits.spad_row_offset +& (1.U << req.bits.lgLen).asUInt()))
  io.out.bits.addr := req.bits.addr + (bytesSent / spadWidthBytes.U) * meshRows.U

  val last = spadWidthBytes.U >= (1.U << req.bits.lgLen).asUInt() - bytesSent
  io.out.bits.last := last

  when (io.out.fire()) {
    bytesSent := bytesSent + spadWidthBytes.U

    when (last) {
      req.valid := false.B
      io.req.ready := true.B
    }
  }

  when (io.req.fire()) {
    req.valid := true.B
    req.bits := io.req.bits
    bytesRead := 0.U
    bytesSent := 0.U
  }

  when (io.in.fire()) {
    val current_bytesRead = Mux(io.req.fire(), 0.U, bytesRead)

    val rshift = Mux(current_bytesRead =/= 0.U, 0.U, Mux(io.req.fire(), io.req.bits.shift, req.bits.shift)) * 8.U // in bits
    val lshift = current_bytesRead * 8.U // bits
    val mask = (~(((~0.U(beatBits.W)) >> rshift) << lshift)).asUInt()
    buffer := (buffer & mask) | ((io.in.bits >> rshift) << lshift).asUInt()

    bytesRead := satAdd(current_bytesRead, (beatBits.U - rshift) / 8.U, len)
  }

  when (reset.toBool()) {
    req.valid := false.B
  }
}

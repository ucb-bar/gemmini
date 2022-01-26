package gemmini

import chisel3._
import chisel3.util._

import Util._

class BeatMergerOut(val spadWidth: Int, val accWidth: Int, val spadRows: Int, val accRows: Int,
                    val alignedTo: Int) extends Bundle {
  val data = UInt((spadWidth max accWidth).W)
  val addr = UInt(log2Up(spadRows max accRows).W)
  val is_acc = Bool()
  val accumulate = Bool()
  val has_acc_bitwidth = Bool()
  val mask = Vec((spadWidth max accWidth)/(alignedTo*8) max 1, Bool())
  val last = Bool()
}

/*
  beatBits: in bits
  maxShift: in bytes
  spadWidth: width of spad row in bits
  accWidth: width of accumulator row in bits
  spadRows: unit-less
  accRows: unit-less
  maxReqBytes: in bytes
  aligned_to: in bytes
 */
class BeatMerger[U <: Data](beatBits: Int, maxShift: Int, spadWidth: Int, accWidth: Int, spadRows: Int, accRows: Int, maxReqBytes: Int, alignedTo: Int, meshRows: Int, mvin_scale_t_bits: Int, nCmds: Int)
  extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds)))
    val in = Flipped(Decoupled(UInt(beatBits.W)))
    val out = Decoupled(new BeatMergerOut(spadWidth, accWidth, spadRows, accRows, alignedTo))
  })

  val beatBytes = beatBits/8
  val spadWidthBytes = spadWidth/8
  val accWidthBytes = accWidth/8

  val req = Reg(UDValid(new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds)))

  val buflen = maxReqBytes max spadWidthBytes max accWidthBytes // in bytes
  val buffer = Reg(UInt((buflen*8).W))

  val rowBytes = Mux(req.bits.has_acc_bitwidth, accWidthBytes.U, spadWidthBytes.U)

  val bytesSent = Reg(UInt(log2Up(buflen+1).W))
  val bytesRead = Reg(UInt(log2Up(buflen+1).W))
  val bytesReadAfterShift = Mux(bytesRead > req.bits.shift, bytesRead - req.bits.shift, 0.U)
  val bytesDiscarded = bytesRead - bytesReadAfterShift
  val usefulBytesRead = minOf(bytesReadAfterShift, req.bits.bytes_to_read)

  val bytesSent_next = {
    val spad_row_offset = Mux(bytesSent === 0.U, req.bits.spad_row_offset, 0.U)
    satAdd(bytesSent, rowBytes - spad_row_offset, req.bits.bytes_to_read)
  }

  val last_sending = bytesSent_next === req.bits.bytes_to_read
  val last_reading = beatBytes.U >= (1.U << req.bits.lg_len_req).asUInt() - bytesRead

  io.req.ready := !req.valid

  io.in.ready := io.req.fire || (req.valid && bytesRead =/= (1.U << req.bits.lg_len_req).asUInt())

  io.out.valid := req.valid && usefulBytesRead > bytesSent && (usefulBytesRead - bytesSent >= rowBytes ||
    usefulBytesRead === req.bits.bytes_to_read)
  io.out.bits.data := (buffer >> (bytesSent * 8.U)) << Mux(bytesSent === 0.U, req.bits.spad_row_offset * 8.U, 0.U)
  io.out.bits.mask := VecInit((0 until (spadWidthBytes max accWidthBytes) by alignedTo).map { i =>
    val spad_row_offset = Mux(bytesSent === 0.U, req.bits.spad_row_offset, 0.U)
    i.U >= spad_row_offset &&
      i.U < spad_row_offset +& (req.bits.bytes_to_read - bytesSent)
  })
  io.out.bits.addr := req.bits.addr + req.bits.block_stride * {
    val total_bytes_sent = req.bits.spad_row_offset + bytesSent
    Mux(req.bits.has_acc_bitwidth,
      // We only add "if" statements here to satisfy the Verilator linter. The code would be cleaner without the
      // "if" condition and the "else" clause
      if (total_bytes_sent.getWidth >= log2Up(accWidthBytes+1)) total_bytes_sent / accWidthBytes.U else 0.U,
      if (total_bytes_sent.getWidth >= log2Up(spadWidthBytes+1)) total_bytes_sent / spadWidthBytes.U else 0.U)
  }

  io.out.bits.is_acc := req.bits.is_acc
  io.out.bits.accumulate := req.bits.accumulate
  io.out.bits.has_acc_bitwidth := req.bits.has_acc_bitwidth
  io.out.bits.last := last_sending
  io.out.bits.accumulate := req.bits.accumulate
  io.out.bits.has_acc_bitwidth := req.bits.has_acc_bitwidth

  when (bytesRead === (1.U << req.bits.lg_len_req).asUInt() &&
    bytesSent === req.bits.bytes_to_read) {
    req.pop()
  }

  when (io.out.fire) {
    bytesSent := bytesSent_next

    when (last_sending && bytesRead === (1.U << req.bits.lg_len_req).asUInt()) {
      req.pop()
      io.req.ready := true.B
    }
  }

  when (io.req.fire) {
    req.push(io.req.bits)
    bytesRead := 0.U
    bytesSent := 0.U
  }

  when (io.in.fire) {
    val current_bytesRead = Mux(io.req.fire, 0.U, bytesRead)
    val current_bytesDiscarded = Mux(io.req.fire, 0.U, bytesDiscarded)
    val current_usefulBytesRead = Mux(io.req.fire, 0.U, usefulBytesRead)
    val current_shift = Mux(io.req.fire, io.req.bits.shift, req.bits.shift)
    val current_lg_len_req = Mux(io.req.fire, io.req.bits.lg_len_req, req.bits.lg_len_req)
    val current_len_req = (1.U << current_lg_len_req).asUInt()

    when (current_shift - current_bytesDiscarded <= beatBytes.U /* &&
      current_bytesRead < current_len_req */
    ) {
      val rshift = (current_shift - current_bytesDiscarded) * 8.U // in bits
      val lshift = current_usefulBytesRead * 8.U // in bits
      val mask = (~(((~0.U(beatBits.W)) >> rshift) << lshift)).asUInt()

      buffer := (buffer & mask) | ((io.in.bits >> rshift) << lshift).asUInt()
    }

    bytesRead := satAdd(current_bytesRead, beatBytes.U, current_len_req)

    when (!io.req.fire && bytesSent === req.bits.bytes_to_read && last_reading) {
      req.pop()
    }
  }

  when (reset.asBool()) {
    req.valid := false.B
  }
}

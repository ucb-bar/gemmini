package gemmini

import chisel3._
import chisel3.util._

// SPAD_REQUANT: stream a row-major BF16 tile (M x N, N a multiple of 32) from the scratchpad through the existing
// MxRequantizer. Block (m, b) = 4 consecutive spad rows src + 4*(m*GN + b) -> one requant beat -> 32 E4M3 codes
// (2 spad rows) at dst, flat (dst + 2*(m*GN + b) + h) or operand-A tiled (dst + ((m/16)*N/16 + 2b + h)*16 + m%16).
// Scales are filed by the requantizer's linear mode (scale k = block k) and flushed by its existing coalescer.
class SpadRequantCmd(val addrW: Int) extends Bundle {
  val src   = UInt(addrW.W)
  val dst   = UInt(addrW.W)
  val m     = UInt(16.W)
  val n     = UInt(16.W)
  val tiled = Bool()
}

object SpadRequantCmd {
  // rs1 = src[13:0] | dst[27:14] | tiled[28]; rs2 = M[15:0] | N[31:16]
  def decode(rs1: UInt, rs2: UInt, addrW: Int): SpadRequantCmd = {
    val c = Wire(new SpadRequantCmd(addrW))
    c.src := rs1(13, 0); c.dst := rs1(27, 14); c.tiled := rs1(28)
    c.m := rs2(15, 0); c.n := rs2(31, 16)
    c
  }
}

class SpadRowWrite(val addrW: Int, val rowW: Int) extends Bundle {
  val addr = UInt(addrW.W)
  val data = UInt(rowW.W)
}

class SpadRequant(addrW: Int, bankRowBits: Int, nBanks: Int, rowW: Int, maxScales: Int) extends Module {
  val io = IO(new Bundle {
    val cmd     = Flipped(Decoupled(new SpadRequantCmd(addrW)))
    val rd_req  = Decoupled(UInt(addrW.W))
    val rd_resp = Vec(nBanks, Flipped(Decoupled(UInt(rowW.W))))
    val rq_in   = Decoupled(UInt((4 * rowW).W))   // 32 BF16, element k = bits [16k+15:16k]
    val rq_out  = Flipped(Decoupled(UInt((2 * rowW).W)))   // 32 codes, code k = bits [8k+7:8k]
    val wr      = Decoupled(new SpadRowWrite(addrW, rowW))
    val linear_gn = Output(UInt(16.W))
    val linear_m  = Output(UInt(16.W))
    val flush_busy = Input(Bool())   // requantizer scale flush in progress
    val active  = Output(Bool())
    val busy    = Output(Bool())
  })
  val bankOf = (a: UInt) => if (nBanks == 1) 0.U else a(addrW - 1, bankRowBits)

  val active = RegInit(false.B)
  val c      = Reg(new SpadRequantCmd(addrW))
  val gn     = c.n >> 5
  val total  = c.m * gn                   // blocks
  val rdCnt  = Reg(UInt(20.W))            // spad rows read (4 per block)
  val outCnt = Reg(UInt(16.W))            // blocks written
  val om     = Reg(UInt(16.W)); val ob = Reg(UInt(16.W))
  val drain  = RegInit(0.U(2.W))          // cycles after the last write before checking flush_busy

  io.cmd.ready := !active
  when (io.cmd.fire) {
    active := io.cmd.bits.m =/= 0.U && io.cmd.bits.n >= 32.U
    c := io.cmd.bits
    rdCnt := 0.U; outCnt := 0.U; om := 0.U; ob := 0.U; drain := 0.U
  }
  // the scale flush writes whole 32-scale rows and the resident flush 8-row words
  val cmdScales = io.cmd.bits.m * (io.cmd.bits.n >> 5)
  assert(!io.cmd.fire || (io.cmd.bits.n(4, 0) === 0.U && cmdScales <= maxScales.U && cmdScales(4, 0) === 0.U &&
    io.cmd.bits.m(2, 0) === 0.U), "SPAD_REQUANT: N must be a multiple of 32, M of 8, and M*N/32 a multiple of 32 and <= 2048")

  // ---- reads: one spad row per cycle, responses consumed in issue order (bank FIFO) ----
  val inflight = Module(new Queue(UInt(log2Ceil(nBanks max 2).W), 8))
  val rdAddr = c.src + rdCnt
  io.rd_req.valid := active && rdCnt < (total << 2) && inflight.io.enq.ready
  io.rd_req.bits := rdAddr
  inflight.io.enq.valid := io.rd_req.fire
  inflight.io.enq.bits := bankOf(rdAddr)
  when (io.rd_req.fire) { rdCnt := rdCnt + 1.U }

  val rowBuf = Reg(Vec(4, UInt(rowW.W)))
  val rowCnt = RegInit(0.U(2.W))
  val full   = RegInit(false.B)
  val accept = !full || io.rq_in.ready
  io.rd_resp.zipWithIndex.foreach { case (r, b) =>
    r.ready := inflight.io.deq.valid && inflight.io.deq.bits === b.U && accept
  }
  val respFire = VecInit(io.rd_resp.map(_.fire)).asUInt.orR
  val respData = Mux1H(io.rd_resp.map(_.fire), io.rd_resp.map(_.bits))
  inflight.io.deq.ready := respFire
  io.rq_in.valid := full
  io.rq_in.bits := Cat(rowBuf.reverse)
  when (io.rq_in.fire) { full := false.B }
  when (respFire) {
    rowBuf(rowCnt) := respData
    rowCnt := rowCnt + 1.U
    when (rowCnt === 3.U) { full := true.B }
  }

  // ---- writes: 2 code rows per block ----
  val outBuf = Reg(UInt((2 * rowW).W))
  val outValid = RegInit(false.B)
  val half = RegInit(false.B)
  io.rq_out.ready := !outValid
  when (io.rq_out.fire) { outBuf := io.rq_out.bits; outValid := true.B; half := false.B }
  val tilesK = c.n >> 4
  val flatAddr  = c.dst + (outCnt << 1) + half.asUInt
  val tiledAddr = c.dst + ((((om >> 4) * tilesK) + (ob << 1) + half.asUInt) << 4) + om(3, 0)
  io.wr.valid := outValid
  io.wr.bits.addr := Mux(c.tiled, tiledAddr, flatAddr)
  io.wr.bits.data := Mux(half, outBuf(2 * rowW - 1, rowW), outBuf(rowW - 1, 0))
  when (io.wr.fire) {
    half := !half
    when (half) {
      outValid := false.B
      outCnt := outCnt + 1.U
      val lastB = ob === gn - 1.U
      ob := Mux(lastB, 0.U, ob + 1.U)
      om := Mux(lastB, om + 1.U, om)
    }
  }

  // ---- completion: every block written, then the scale flush has finished ----
  when (active && outCnt === total && !outValid) {
    when (drain =/= 3.U) { drain := drain + 1.U } .elsewhen (!io.flush_busy) { active := false.B }
  }

  io.linear_gn := gn
  io.linear_m := c.m
  io.active := active
  io.busy := active
}

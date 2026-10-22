package gemmini

import chisel3._
import chisel3.util._

// SPAD_REQUANT: stream a row-major BF16 tile (M x N, N a multiple of 32) from the scratchpad through the existing
// MxRequantizer. Row width W bits (128 at DIM16, 64 at DIM8, 256 at DIM32): a block = 512/W rows, its codes 256/W rows,
// an operand-A tile T = W/8 rows. A requant beat carries B blocks (b..b+B-1) of one row: B = 1 at DIM8/16, 2 at DIM32
// (64-lane requantizer, N a multiple of 64), so a beat reads I = B*512/W rows and writes O = B*256/W code rows
// (DIM16: I 4, O 2, T 16; DIM8: I 8, O 4, T 8; DIM4: I 16, O 8, T 4; DIM32: I 4, O 2, T 32). Block (m, b) = 512/W consecutive spad rows from
// src + (512/W)*(m*GN + b) -> 32 E4M3 codes at dst, flat (dst + O*(m*GN/B + b/B) + h) or operand-A tiled
// (dst + ((m/T)*N/T + O*(b/B) + h)*T + m%T); the formulas below are written for W 128.
// Scales are filed by the requantizer's linear mode (scale k = block k) and flushed by its existing coalescer to
// the command's own scale DRAM address (and, if resident, the act-scale memory), so no CONFIG_SCALE_MEM is involved.
// FP4 (rs2[32]): blocks are fed in row pairs (m, b), (m+1, b); the requantizer packs each pair into 32 bytes (byte k =
// row m col k low nibble | row m+1 high) = the FP4 operand-A layout, written as 2 rows at row pair p = m/2: flat
// dst + 2*(p*GN + b) + h, tiled dst + ((p/16)*N/16 + 2b + h)*16 + p%16. Its first beat of each pair is garbage.
// LUT (rs2[33]): same pair order / packing as FP4, but each code is the nearest index in row m's output codebook
// (act-out LUT entry m >> G) of the LUT format given by rs2[34] (FP6 class, else FP8 class) and rs2[35] (alt format:
// E2M3 / E5M2, else E3M2 / E4M3-quad); G = rs2[40:36]. Output codebooks: loaded flat (MX_LOAD_LUT sel 2) beforehand.
class SpadRequantCmd(val addrW: Int) extends Bundle {
  val src   = UInt(addrW.W)
  val dst   = UInt(addrW.W)
  val m     = UInt(16.W)
  val n     = UInt(16.W)
  val tiled = Bool()
  val resident = Bool()
  val scale_addr = UInt(33.W)
  val fp4   = Bool()
  val lut   = Bool()
  val lut_fp6 = Bool()
  val lut_alt = Bool()
  val lut_g = UInt(5.W)
  def nib: Bool = fp4 || lut   // 4-bit output: row pairs, 2 codes per byte
}

object SpadRequantCmd {
  // rs1 = src[F-1:0] | dst[2F-1:F] | tiled[2F] | resident[2F+1] | scale DRAM addr[min(63, 2F+34):2F+2]
  //   (F = VpuCmd.fieldW: 14 -> src[13:0] | dst[27:14] | tiled[28] | resident[29] | scale[62:30])
  // rs2 = M[15:0] | N[31:16] | fp4[32] | lut[33] | lut fp6[34] | lut alt[35] | lut G[40:36]
  def decode(rs1: UInt, rs2: UInt, addrW: Int): SpadRequantCmd = {
    val c = Wire(new SpadRequantCmd(addrW))
    val f = gemmini.vpu.VpuCmd.fieldW(addrW)
    c.src := rs1(f - 1, 0); c.dst := rs1(2 * f - 1, f); c.tiled := rs1(2 * f)
    c.resident := rs1(2 * f + 1); c.scale_addr := rs1(math.min(63, 2 * f + 34), 2 * f + 2)
    c.m := rs2(15, 0); c.n := rs2(31, 16); c.fp4 := rs2(32)
    c.lut := rs2(33); c.lut_fp6 := rs2(34); c.lut_alt := rs2(35); c.lut_g := rs2(40, 36)
    c
  }
}

object SpadRequantRows {
  // exact spad footprints for ordering (RS, LoopMatmul) at rowBytes-byte rows (16 at DIM16, 8 at DIM8; tile height T =
  // rowBytes): BF16 source M*N*2/rowBytes rows; E4M3 destination M*N/rowBytes (M rounded up to T if tiled); 4-bit
  // destination M*N/(2*rowBytes) (M rounded up to 2T if tiled). rowBytes 16: M*N/8, M*N/16 (M16), M*N/32 (M32).
  def src(c: SpadRequantCmd, rowBytes: Int): UInt = (c.m * c.n) >> log2Ceil(rowBytes / 2)
  def dst(c: SpadRequantCmd, rowBytes: Int): UInt = {
    val lg = log2Ceil(rowBytes)
    val mT  = (c.m +& (rowBytes - 1).U) & ~(rowBytes - 1).U(17.W)
    val m2T = (c.m +& (2 * rowBytes - 1).U) & ~(2 * rowBytes - 1).U(17.W)
    Mux(c.nib, Mux(c.tiled, (m2T * c.n) >> (lg + 1), (c.m * c.n) >> (lg + 1)),
               Mux(c.tiled, (mT * c.n) >> lg, (c.m * c.n) >> lg))
  }
}

class SpadRowWrite(val addrW: Int, val rowW: Int) extends Bundle {
  val addr = UInt(addrW.W)
  val data = UInt(rowW.W)
}

class SpadRequant(addrW: Int, bankRowBits: Int, nBanks: Int, rowW: Int, maxScales: Int, blocksPerBeat: Int = 1) extends Module {
  require((blocksPerBeat == 1 && Seq(32, 64, 128).contains(rowW)) || (blocksPerBeat == 2 && rowW == 256),
    "SPAD_REQUANT: 16-, 8- or 4-byte spad rows (1 block per beat) or 32-byte rows (2)")
  val inRows = 512 * blocksPerBeat / rowW; val outRows = 256 * blocksPerBeat / rowW; val tileRows = rowW / 8   // I, O, T
  val blkRows = 512 / rowW   // spad rows per block
  val lgIn = log2Ceil(inRows); val lgOut = log2Ceil(outRows); val lgT = log2Ceil(tileRows)
  val lgBlk = log2Ceil(blkRows); val lgBpb = log2Ceil(blocksPerBeat)
  val io = IO(new Bundle {
    val cmd     = Flipped(Decoupled(new SpadRequantCmd(addrW)))
    val rd_req  = Decoupled(UInt(addrW.W))
    val rd_resp = Vec(nBanks, Flipped(Decoupled(UInt(rowW.W))))
    val rq_in   = Decoupled(UInt((512 * blocksPerBeat).W))   // 32B BF16, element k = bits [16k+15:16k]
    val rq_out  = Flipped(Decoupled(UInt((256 * blocksPerBeat).W)))   // 32B codes, code k = bits [8k+7:8k]
    val rq_out_garbage = Input(Bool())                     // FP4: the first beat of a pair carries no output
    val wr      = Decoupled(new SpadRowWrite(addrW, rowW))
    val linear_gn = Output(UInt(16.W))
    val linear_m  = Output(UInt(16.W))
    val linear_base = Output(UInt(33.W))   // scale DRAM address for this command's flush
    val linear_resident = Output(Bool())
    val fp4 = Output(Bool())   // 4-bit output (FP4 or LUT: two-beat pair packing), valid while active
    val lut = Output(Bool())   // LUT output: format / alt / G below, valid while active
    val lut_fp6 = Output(Bool())
    val lut_alt = Output(Bool())
    val lut_g = Output(UInt(5.W))
    val flush_busy = Input(Bool())   // requantizer scale flush in progress
    val active  = Output(Bool())
    val busy    = Output(Bool())
  })
  val bankOf = (a: UInt) => if (nBanks == 1) 0.U else a(addrW - 1, bankRowBits)

  val active = RegInit(false.B)
  val c      = Reg(new SpadRequantCmd(addrW))
  val gn     = c.n >> 5
  val gnB    = gn >> lgBpb                // beats per row
  val total  = c.m * gn                   // blocks
  val beats  = total >> lgBpb
  val outTotal = Mux(c.nib, beats >> 1, beats)   // outputs (O code rows each)
  val rdCnt  = Reg(UInt(20.W))            // spad rows read (I per beat)
  val outCnt = Reg(UInt(16.W))            // outputs written
  // read beat walk: E4M3 (m, b) b inner; FP4 (pair, b, r) r inner; rdB = beat in row. rdBase = first block of row m (or the pair's even row)
  val rdBase = Reg(UInt(16.W)); val rdB = Reg(UInt(16.W)); val rdR = Reg(Bool()); val rdRow = Reg(UInt(lgIn.W))
  val om     = Reg(UInt(16.W)); val ob = Reg(UInt(16.W))
  val drain  = RegInit(0.U(2.W))          // cycles after the last write before checking flush_busy

  io.cmd.ready := !active
  when (io.cmd.fire) {
    active := io.cmd.bits.m =/= 0.U && io.cmd.bits.n >= 32.U
    c := io.cmd.bits
    rdCnt := 0.U; outCnt := 0.U; om := 0.U; ob := 0.U; drain := 0.U
    rdBase := 0.U; rdB := 0.U; rdR := false.B; rdRow := 0.U
  }
  // the scale flush writes whole 32-scale rows and the resident flush 8-row words
  val cmdScales = io.cmd.bits.m * (io.cmd.bits.n >> 5)
  assert(!io.cmd.fire || (io.cmd.bits.n(4 + lgBpb, 0) === 0.U && cmdScales <= maxScales.U && cmdScales(4, 0) === 0.U &&
    io.cmd.bits.m(2, 0) === 0.U), "SPAD_REQUANT: N must be a multiple of 32 (64 at 2 blocks/beat), M of 8, and M*N/32 a multiple of 32 and <= 2048")

  // ---- reads: one spad row per cycle, responses consumed in issue order (bank FIFO) ----
  val inflight = Module(new Queue(UInt(log2Ceil(nBanks max 2).W), 8))
  val rdAddr = c.src + ((rdBase + Mux(rdR, gn, 0.U) + (rdB << lgBpb)) << lgBlk) + rdRow
  io.rd_req.valid := active && rdCnt < (total << lgBlk) && inflight.io.enq.ready
  io.rd_req.bits := rdAddr
  inflight.io.enq.valid := io.rd_req.fire
  inflight.io.enq.bits := bankOf(rdAddr)
  when (io.rd_req.fire) {
    rdCnt := rdCnt + 1.U
    rdRow := rdRow + 1.U
    when (rdRow === (inRows - 1).U) {
      val lastB = rdB === gnB - 1.U
      when (c.nib && !rdR) { rdR := true.B } .otherwise {
        rdR := false.B
        rdB := Mux(lastB, 0.U, rdB + 1.U)
        when (lastB) { rdBase := rdBase + Mux(c.nib, gn << 1, gn) }
      }
    }
  }

  val rowBuf = Reg(Vec(inRows, UInt(rowW.W)))
  val rowCnt = RegInit(0.U(lgIn.W))
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
    when (rowCnt === (inRows - 1).U) { full := true.B }
  }

  // ---- writes: O code rows per block ----
  val outBuf = Reg(UInt((256 * blocksPerBeat).W))
  val outValid = RegInit(false.B)
  val half = RegInit(0.U(lgOut.W))   // code row of the block being written
  io.rq_out.ready := !outValid
  val rqKeep = !(c.nib && io.rq_out_garbage)
  assert(!(io.rq_out.fire && rqKeep) || outCnt < outTotal, "SPAD_REQUANT: more requantizer outputs than blocks it sent")
  when (io.rq_out.fire && rqKeep) { outBuf := io.rq_out.bits; outValid := true.B; half := 0.U }
  val tilesK = c.n >> lgT
  val flatAddr  = c.dst + (outCnt << lgOut) + half
  val tiledAddr = c.dst + ((((om >> lgT) * tilesK) + (ob << lgOut) + half) << lgT) + om(lgT - 1, 0)
  io.wr.valid := outValid
  io.wr.bits.addr := Mux(c.tiled, tiledAddr, flatAddr)
  io.wr.bits.data := outBuf.asTypeOf(Vec(outRows, UInt(rowW.W)))(half)
  when (io.wr.fire) {
    half := half + 1.U
    when (half === (outRows - 1).U) {
      outValid := false.B
      outCnt := outCnt + 1.U
      val lastB = ob === gnB - 1.U
      ob := Mux(lastB, 0.U, ob + 1.U)
      om := Mux(lastB, om + 1.U, om)
    }
  }

  // ---- completion: every block written, then the scale flush has finished ----
  when (active && outCnt === outTotal && !outValid) {
    when (drain =/= 3.U) { drain := drain + 1.U } .elsewhen (!io.flush_busy) { active := false.B }
  }

  io.linear_gn := gn
  io.linear_m := c.m
  io.linear_base := c.scale_addr
  io.linear_resident := c.resident
  io.fp4 := c.nib
  io.lut := c.lut
  io.lut_fp6 := c.lut_fp6
  io.lut_alt := c.lut_alt
  io.lut_g := c.lut_g
  io.active := active
  io.busy := active
}

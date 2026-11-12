package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config.Parameters

import scala.math.{pow}


// per scale-mem slot (half) QuantLut state from CONFIG_SCALE_MEM; a read response picks its slot by its halves tag,
// so a config for the next loop never changes the bounds/flags of older reads still in flight
class LutSlotA extends Bundle { val i = UInt(9.W); val halved = Bool() }
class LutSlotW extends Bundle { val j = UInt(9.W); val k = UInt(9.W); val halved = Bool(); val kblock = Bool(); val kmix = Bool() }
class LutSlots extends Bundle { val a = Vec(2, new LutSlotA); val w = Vec(2, new LutSlotW) }

class QuantLutIO(
  lutConfig: GemminiLUTConfig,
  outputnumLanes: Int = 32 ,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  iterator_bitwidth: Int,
) extends Bundle {
  val lut_write_weight =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(0)))) //input
  val lut_write_act_in =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(1)))) //input
  val lut_write_act_out =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig(2)))) //input
  val quant_fp6 = Flipped(Valid(Vec(outputnumLanes, UInt(lutConfig.rdataWidth.W)))) //input
  val projected_data = Valid(Vec(outputnumLanes, UInt(lutConfig.raddrWidth.W)))
  val spad_projected_data   = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected))
  val spad_deprojected_data = Vec(sp_banks, Flipped(new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val loop_bound_i = Input(UInt(iterator_bitwidth.W))
  val loop_bound_j = Input(UInt(iterator_bitwidth.W))
  val loop_bound_k = Input(UInt(iterator_bitwidth.W))
  val read_a = Input(Bool())
  val read_d = Input(Bool())
  val quant_lut_update_granularity = Input(UInt(lutConfig.lutUpdateRegularityWidth.W))
  val act_lut_halved = Input(Bool())   // act-in lookups use LUT half act_lut_half: {half, local}
  val act_lut_half = Input(UInt(1.W))
  val act_out_row = Input(Valid(UInt(16.W)))   // LUT SPAD_REQUANT: act-out codebook = row >> G (counter_act_out left alone)
  val wgt_lut_halved = Input(Bool())   // weight lookups use LUT half wgt_lut_half: {half, local}
  val wgt_lut_half = Input(UInt(1.W))
  val wgt_lut_kblock = Input(Bool())   // the loop's weight codebooks hold one set per K-block (offset by the k-tile's K-block)
  val lut_slots = Input(new LutSlots)
  val mx_fp8_altfmt = Input(Bool())   // act-out projection sub-format: 1 = E5M2/E2M3 finder, 0 = E4M3/E3M2
  val output_mx_format = Input(UInt(2.W)) // requant output code: 0=fp8, 1=fp6, 2=fp4 (picks the finder family)
}

class QuantLut(
  lutConfig: GemminiLUTConfig,
  outputnumLanes: Int = 32 ,
  sp_bank_entries: Int,
  sp_banks: Int,
  sp_width: Int,
  sp_width_projected: Int,
  lut_update_regularity_w: Int,
  lut_update_regularity_act_in: Int,
  lut_update_regularity_act_out: Int,
  iterator_bitwidth: Int,
) extends Module {
  val io = IO(new QuantLutIO(lutConfig, outputnumLanes, sp_bank_entries, sp_banks, sp_width, sp_width_projected, iterator_bitwidth))
  val rdataWidth = lutConfig.rdataWidth
  val raddrWidth = lutConfig.raddrWidth
  // Per-operand deprojected code widths (default rdataWidth). Storage is rdataWidth-wide; the deproject
  // packs each operand's codes at its own width so asymmetric builds (act/wei different widths) align.
  val actCodeW = lutConfig.actCodeW
  val weiCodeW = lutConfig.weiCodeW

  // Deproject width = operand nibbles (sp_width_projected/4 = 2*meshColumns), decoupled from the requant
  // output width outputnumLanes (they differ at DIM=8 numChunks=1: 16 vs 32).
  val depLanes  = sp_width_projected / 4
  val luDim     = depLanes / 2
  val log2LuDim = log2Ceil(luDim)
  val log2Lanes = log2Ceil(depLanes)

  // Single-buffer LUT caches (no double buffering)
  // All caches as Vec of Regs to support dynamic hardware indexing
  val lutCache_act_in  = RegInit(VecInit(Seq.fill(2*lutConfig(1)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  // weight codebooks = lutConfig(0) (the lut0 / weight write port); read as rows of wRowLen consecutive codebooks
  val nW      = lutConfig(0)._1
  val wRowLen = math.min(depLanes, nW)
  val nWRows  = nW / wRowLen
  val lutCache_weight  = RegInit(VecInit(Seq.fill(nW)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  // act_out: the projection codebooks are not stored as raw codes but as the derived nearest-finder tables
  // (sorted-order permutation + per-format decision thresholds), computed once per table at load time by the
  // NearestFinderTableEngine and shared by every output lane. Only the finder formats the operands need are
  // elaborated (lutConfig.resolvedFinderFormats).
  val finderFormats = lutConfig.resolvedFinderFormats
  val finderEngine  = Module(new NearestFinderTableEngine(finderFormats, rdataWidth, log2Ceil(lutConfig(2)._1)))
  val finderTable_t = new NearestFinderTable(finderEngine.nSlots, finderEngine.thrW)
  val lutCache_act_out = RegInit(VecInit(Seq.fill(2*lutConfig(2)._1)(0.U.asTypeOf(finderTable_t))))
  // 16 b: counter_j counts a loop's weight rows up to (J << log2LuDim) - 1 = 127 at NC 256; 6 b wrapped at 64
  val counter_i = RegInit(0.U(16.W))
  val counter_j = RegInit(0.U(16.W))
  val counter_k = RegInit(0.U(16.W))   // k-tile of the weight rows within the loop (kblock codebooks)
  val kTilesPerKb = math.max(32 / luDim, 1)   // k-tiles per 32-channel MX K-block
  val out_counter_i = RegInit(0.U(16.W))
  // act_in write: all codebooks, or (half mode, loop-managed) only one half, from the bundle's first half
  val halfEntries = lutConfig(1)._1 / 2   // act-in codebooks = lutConfig(1) (the lut1 / act-in write port)
  when(io.lut_write_act_in.fire) {
    for (lane <- 0 until lutConfig(1)._1) {
      val (h, src) = (lane / halfEntries, lane % halfEntries)
      when (!io.lut_write_act_in.bits.half_mode || io.lut_write_act_in.bits.half === h.U) {
        for (entry <- 0 until 16) {
          lutCache_act_in(lane)(entry) := Mux(io.lut_write_act_in.bits.half_mode,
            io.lut_write_act_in.bits.data(src)((entry+1)*rdataWidth-1, entry*rdataWidth),
            io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth))
        }
      }
    }
  }
  io.lut_write_act_in.ready := true.B

  // weight write: all codebooks, or (half mode, loop-managed) only one half, from the bundle's first half
  val halfEntriesW = nW / 2
  when(io.lut_write_weight.fire) {
    for (lane <- 0 until nW) {
      val (h, src) = (lane / halfEntriesW, lane % halfEntriesW)
      when (!io.lut_write_weight.bits.half_mode || io.lut_write_weight.bits.half === h.U) {
        for (entry <- 0 until 16) {
          lutCache_weight(lane)(entry) := Mux(io.lut_write_weight.bits.half_mode,
            io.lut_write_weight.bits.data(src)((entry+1)*rdataWidth-1, entry*rdataWidth),
            io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth))
        }
      }
    }
  }
  io.lut_write_weight.ready := true.B

  // act_out write: the write bundle carries all tables at once; feed them one per cycle through the finder
  // engine (bits stay stable until the handshake, which happens on the last table) and store the derived
  // tables as they come out `latency` cycles later.
  val act_out_tables = lutConfig(2)._1
  val act_out_wr_idx = RegInit(0.U(log2Ceil(act_out_tables).W))
  val act_out_last   = act_out_wr_idx === (act_out_tables - 1).U
  io.lut_write_act_out.ready := act_out_last
  finderEngine.io.in.valid      := io.lut_write_act_out.valid
  finderEngine.io.in.bits.codes := io.lut_write_act_out.bits.data(act_out_wr_idx).asTypeOf(Vec(16, UInt(rdataWidth.W)))
  finderEngine.io.in.bits.is6   := io.lut_write_act_out.bits.entry_bits === 6.U
  finderEngine.io.in.bits.tag   := act_out_wr_idx
  when(io.lut_write_act_out.valid) {
    act_out_wr_idx := Mux(act_out_last, 0.U, act_out_wr_idx + 1.U)
  }
  when(finderEngine.io.out.valid) {
    lutCache_act_out(finderEngine.io.out.bits.tag) := finderEngine.io.out.bits.table
  }

  val projectedIndices = WireDefault(VecInit(Seq.fill(outputnumLanes)(0.U(raddrWidth.W))))
  val projectedDataValid = WireDefault(false.B)
  val counter_act_out = RegInit(0.U(16.W))
  // Projection nearest-finders (act_out): one shared derived table (sorted permutation + thresholds) per
  // codebook, a key compare + popcount + permutation lookup per lane. The threshold set within a code-width
  // class is selected by mx_fp8_altfmt (0 = E4M3 / E3M2, 1 = E5M2 / E2M3); the class (8- or 6-bit codes) is
  // the one the codebook was loaded with.
  val proj_in      = WireDefault(VecInit(Seq.fill(outputnumLanes)(0.U(rdataWidth.W))))
  val proj_table   = WireDefault(0.U.asTypeOf(finderTable_t))
  val proj_nearest = Wire(Vec(outputnumLanes, UInt(raddrWidth.W)))
  val proj_slot    = if (finderEngine.nSlots == 1) 0.U else io.mx_fp8_altfmt.asUInt
  val proj_thr     = proj_table.thr(proj_slot)
  val finderWidths = finderFormats.map(NfFormat.width).distinct
  for (i <- 0 until outputnumLanes) {
    val keys = finderWidths.map(w => NearestFinder.key(proj_in(i)(w - 1, 0), w).pad(finderEngine.thrW))
    val k = if (finderWidths.length == 1) keys.head
            else Mux(proj_table.is6, keys(finderWidths.indexOf(6)), keys(finderWidths.indexOf(8)))
    proj_nearest(i) := NearestFinder.lane(k, proj_thr, proj_table.perm)
  }

  when(io.quant_fp6.valid) {
    proj_table := lutCache_act_out(Mux(io.act_out_row.valid, io.act_out_row.bits, counter_act_out) >> io.quant_lut_update_granularity)
    for (i <- 0 until outputnumLanes) {
      proj_in(i) := io.quant_fp6.bits(i)
      projectedIndices(i) := proj_nearest(i)
    }
    projectedDataValid := true.B
    when (io.act_out_row.valid) {
      // SPAD_REQUANT rows: the matmul requant's counter is not advanced
    }.elsewhen (counter_act_out === ((io.loop_bound_i << log2Lanes.U) - 1.U)){
      counter_act_out := 0.U
    }.otherwise{
      counter_act_out := counter_act_out + 1.U
    }
  }.otherwise {
    projectedDataValid := false.B
  }

  io.projected_data.valid := projectedDataValid
  io.projected_data.bits := projectedIndices
  
  val used_lut_act_0 = WireDefault(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))
  val used_lut_act_1 = WireDefault(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))
  val counter_act = RegInit(0.U(log2Ceil(16).W))

  dontTouch(counter_act)
  dontTouch(used_lut_act_0)
  dontTouch(used_lut_act_1)

  for (i <- 0 until sp_banks) {
    // Each bank has its own deprojected_bits
    val deprojected_bits = WireDefault(VecInit(Seq.fill(depLanes)(0.U(rdataWidth.W))))

    // Initialize unused spad_projected_data outputs (QuantLut doesn't send requests)
    io.spad_projected_data(i).req.valid := false.B
    io.spad_projected_data(i).req.bits := DontCare
    io.spad_projected_data(i).resp.ready := true.B
    io.spad_deprojected_data(i).req.ready := false.B
    io.spad_deprojected_data(i).resp.valid := false.B
    io.spad_deprojected_data(i).resp.bits.data := 0.U
    io.spad_deprojected_data(i).resp.bits.fromDMA := false.B
    io.spad_deprojected_data(i).resp.bits.weight_mx_format := 1.U
    io.spad_deprojected_data(i).resp.bits.input_mx_format := 1.U
    // read_a/read_d are input-side tags only; not meaningful on the deprojected output (mesh ignores them)
    io.spad_deprojected_data(i).resp.bits.read_a := false.B
    io.spad_deprojected_data(i).resp.bits.read_d := false.B
    io.spad_deprojected_data(i).resp.bits.lut_halves := 0.U
    val lut_idx = (counter_i << 1.U) >> io.quant_lut_update_granularity
    val lut_idx_wire = WireDefault(lut_idx)
    val lut_idx_1 = ((counter_i << 1.U) + 1.U) >> io.quant_lut_update_granularity
    val lut_idx_wire_1 = WireDefault(lut_idx)
    dontTouch(lut_idx_wire)
    dontTouch(lut_idx_wire_1)
    
    
    when(io.spad_projected_data(i).resp.valid) {
      // Gate the deproj on the per-response operand tag (aligned with the read data), not a fixed latency,
      // so it fires exactly when the projected data arrives.
      val sa = io.lut_slots.a(io.spad_projected_data(i).resp.bits.lut_halves(0))
      val sw = io.lut_slots.w(io.spad_projected_data(i).resp.bits.lut_halves(1))
      when(io.spad_projected_data(i).resp.bits.read_a) {
        when(counter_i === ((sa.i << log2LuDim.U) - 1.U)){
          counter_i := 0.U
        }.otherwise{
          counter_i := counter_i + 1.U
        } 
        // Route by the runtime read_a tag, so the activation operand may live in any scratchpad bank.
        val act_local_0 = (counter_i << 1.U) >> io.quant_lut_update_granularity
        val act_local_1 = ((counter_i << 1.U) + 1.U) >> io.quant_lut_update_granularity
        def act_idx(l: UInt): UInt = Mux(sa.halved, Cat(io.spad_projected_data(i).resp.bits.lut_halves(0), l(log2Ceil(halfEntries) - 1, 0)), l)
        assert(!sa.halved || act_local_1 < halfEntries.U, "QuantLut: loop-managed act-in block needs > half the codebooks")
        used_lut_act_0 := lutCache_act_in(act_idx(act_local_0))
        used_lut_act_1 := lutCache_act_in(act_idx(act_local_1))
        for (k <- 0 until luDim) { //act data layout is 2 interleaved 4-bit codes per k (kNa1,kNa0), total 2*luDim lanes
          val chunk_4bit_0 = io.spad_projected_data(i).resp.bits.data(2*k*4 + 3, 2*k*4)
          val chunk_4bit_1 = io.spad_projected_data(i).resp.bits.data(2*k*4 + 7, 2*k*4 + 4)
          val deprojected_bit_0 = used_lut_act_0(chunk_4bit_0)
          val deprojected_bit_1 = used_lut_act_1(chunk_4bit_1)
          deprojected_bits(2*k) := deprojected_bit_0
          deprojected_bits(2*k + 1) := deprojected_bit_1
        }
      }
      when(io.spad_projected_data(i).resp.bits.read_d) {
       // count only deprojected weight rows (a mixed-K loop's MXFP8 K-blocks bypass the LUT)
       val wtag = io.spad_projected_data(i).resp.bits.lut_halves
       when (wtag(2) || !wtag(3)) {
        when(counter_j === ((sw.j << log2LuDim.U) - 1.U)){
          counter_j := 0.U
          counter_k := Mux(!sw.kblock || counter_k === sw.k - 1.U, 0.U, counter_k + 1.U)
        }.otherwise{
          counter_j := counter_j + 1.U
        }
       }
        // Route by the runtime read_d tag, so weights may live in any scratchpad bank.
        // kblock codebooks: K-block kb of the loop uses the set at kb * (codebooks per K-block = (J << lanes) >> G)
        // mixed-K loop: the K-block's codebook set = its slot (read tag [7:6])
        val cb_per_kb = (sw.j << log2Lanes.U) >> io.quant_lut_update_granularity
        val kb_off = Mux(sw.kmix, wtag(7, 6) * cb_per_kb,
          Mux(sw.kblock, (counter_k >> log2Ceil(kTilesPerKb)) * cb_per_kb, 0.U))
        def wgt_local(k: Int): UInt = kb_off + ((((counter_j >> log2LuDim.U) << log2Lanes.U) + k.U) >> io.quant_lut_update_granularity)
        assert(!sw.halved || wgt_local(depLanes - 1) < halfEntriesW.U, "QuantLut: loop-managed weight block needs > half the codebooks")
        // A row's lanes read one aligned window of codebooks (wgt_local(k) = wbase + (k >> G)): select its row once
        // per bank, then each lane picks within the row -- a wRowLen:1 lane mux whatever the capacity
        val wbase = kb_off + (((counter_j >> log2LuDim.U) << log2Lanes.U) >> io.quant_lut_update_granularity)
        val ibase = Mux(sw.halved, Cat(io.spad_projected_data(i).resp.bits.lut_halves(1), wbase(log2Ceil(halfEntriesW) - 1, 0)), wbase)
        val wrow  = ibase >> log2Ceil(wRowLen)
        val wrows = VecInit((0 until nWRows).map(r => VecInit((0 until wRowLen).map(c => lutCache_weight(r * wRowLen + c)))))
        val rowW  = Mux(wrow < nWRows.U, wrows(wrow(math.max(log2Ceil(nWRows), 1) - 1, 0)),
                        0.U.asTypeOf(wrows(0)))   // past the written codebooks: zero, as an unwritten entry
        for (k <- 0 until depLanes) {
          val col = (ibase + (k.U >> io.quant_lut_update_granularity))(math.max(log2Ceil(wRowLen), 1) - 1, 0)
          val used_lut_w = if (wRowLen == 1) rowW(0) else rowW(col)
          val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
          deprojected_bits(k) := used_lut_w(chunk_4bit)

        }
      }
      // Pack per operand: activation codes at actCodeW, weight codes at weiCodeW (each sliced from the
      // rdataWidth-wide codebook entry). Equal widths -> identical to the old uniform Cat.
      val act_packed = Cat((0 until depLanes).reverse.map(k => deprojected_bits(k)(actCodeW - 1, 0)))
      val wei_packed = Cat((0 until depLanes).reverse.map(k => deprojected_bits(k)(weiCodeW - 1, 0)))
      io.spad_deprojected_data(i).resp.bits.data := Mux(io.spad_projected_data(i).resp.bits.read_a,
        act_packed, wei_packed)
      io.spad_deprojected_data(i).resp.valid := true.B
      io.spad_deprojected_data(i).resp.bits.fromDMA := io.spad_projected_data(i).resp.bits.fromDMA
      io.spad_deprojected_data(i).resp.bits.weight_mx_format := io.spad_projected_data(i).resp.bits.weight_mx_format
      io.spad_deprojected_data(i).resp.bits.input_mx_format := io.spad_projected_data(i).resp.bits.input_mx_format
    }
  }
}

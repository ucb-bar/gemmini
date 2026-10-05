package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config.Parameters

import scala.math.{pow}

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
  val lutCache_act_in  = RegInit(VecInit(Seq.fill(2*lutConfig(0)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  val lutCache_weight  = RegInit(VecInit(Seq.fill(2*lutConfig(1)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  // act_out: the projection codebooks are not stored as raw codes but as the derived nearest-finder tables
  // (sorted-order permutation + per-format decision thresholds), computed once per table at load time by the
  // NearestFinderTableEngine and shared by every output lane. Only the finder formats the operands need are
  // elaborated (lutConfig.resolvedFinderFormats).
  val finderFormats = lutConfig.resolvedFinderFormats
  val finderEngine  = Module(new NearestFinderTableEngine(finderFormats, rdataWidth, log2Ceil(lutConfig(2)._1)))
  val finderTable_t = new NearestFinderTable(finderEngine.nSlots, finderEngine.thrW)
  val lutCache_act_out = RegInit(VecInit(Seq.fill(2*lutConfig(2)._1)(0.U.asTypeOf(finderTable_t))))
  val counter_i = RegInit(0.U(6.W))
  val counter_j = RegInit(0.U(6.W))
  val out_counter_i = RegInit(0.U(6.W))
  // act_in write
  when(io.lut_write_act_in.fire) {
    for (lane <- 0 until lutConfig(0)._1) {
      for (entry <- 0 until 16) {
        lutCache_act_in(lane)(entry) := io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
      }
    }
  }
  io.lut_write_act_in.ready := true.B

  // weight write
  when(io.lut_write_weight.fire) {
    for (lane <- 0 until lutConfig(1)._1) {
      for (entry <- 0 until 16) {
        lutCache_weight(lane)(entry) := io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
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
  val counter_act_out = RegInit(0.U(log2Ceil(256).W))
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
    proj_table := lutCache_act_out(counter_act_out >> io.quant_lut_update_granularity)
    for (i <- 0 until outputnumLanes) {
      proj_in(i) := io.quant_fp6.bits(i)
      projectedIndices(i) := proj_nearest(i)
    }
    projectedDataValid := true.B
    when (counter_act_out === ((io.loop_bound_i << log2Lanes.U) - 1.U)){
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
    val lut_idx = (counter_i << 1.U) >> io.quant_lut_update_granularity
    val lut_idx_wire = WireDefault(lut_idx)
    val lut_idx_1 = ((counter_i << 1.U) + 1.U) >> io.quant_lut_update_granularity
    val lut_idx_wire_1 = WireDefault(lut_idx)
    dontTouch(lut_idx_wire)
    dontTouch(lut_idx_wire_1)
    
    
    when(io.spad_projected_data(i).resp.valid) {
      // Gate the deproj on the per-response operand tag (aligned with the read data), not a fixed latency,
      // so it fires exactly when the projected data arrives.
      when(io.spad_projected_data(i).resp.bits.read_a) {
        when(counter_i === ((io.loop_bound_i << log2LuDim.U) - 1.U)){
          counter_i := 0.U
        }.otherwise{
          counter_i := counter_i + 1.U
        } 
        // Route by the runtime read_a tag, so the activation operand may live in any scratchpad bank.
        used_lut_act_0 := lutCache_act_in((counter_i << 1.U) >> io.quant_lut_update_granularity)
        used_lut_act_1 := lutCache_act_in(((counter_i << 1.U) + 1.U) >> io.quant_lut_update_granularity)
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
       when(counter_j === ((io.loop_bound_j << log2LuDim.U) - 1.U)){
          counter_j := 0.U
        }.otherwise{
          counter_j := counter_j + 1.U
        }
        // Route by the runtime read_d tag, so weights may live in any scratchpad bank.
        for (k <- 0 until depLanes) {
          val used_lut_w = lutCache_weight((((counter_j >> log2LuDim.U) << log2Lanes.U) + k.U) >> io.quant_lut_update_granularity)
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

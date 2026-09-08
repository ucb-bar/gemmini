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

  // Single-buffer LUT caches (no double buffering)
  // All caches as Vec of Regs to support dynamic hardware indexing
  val lutCache_act_in  = RegInit(VecInit(Seq.fill(2*lutConfig(0)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  val lutCache_weight  = RegInit(VecInit(Seq.fill(2*lutConfig(1)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
  val lutCache_act_out = RegInit(VecInit(Seq.fill(2*lutConfig(2)._1)(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))))
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

  // act_out write
  when(io.lut_write_act_out.fire) {
    for (lane <- 0 until lutConfig(2)._1) {
      for (entry <- 0 until 16) {
        lutCache_act_out(lane)(entry) := io.lut_write_act_out.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
      }
    }
  }
  io.lut_write_act_out.ready := true.B

  val projectedIndices = WireDefault(VecInit(Seq.fill(32)(0.U(raddrWidth.W))))
  val projectedDataValid = WireDefault(false.B)
  val counter_act_out = RegInit(0.U(log2Ceil(256).W))
  val used_lut_act_out = WireDefault(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))
  dontTouch(used_lut_act_out)
  val minIdx = WireDefault(0.U(raddrWidth.W))
  // Projection nearest-finders (act_out): pick the decoder by LUT format.
  // Uniform driver wires keep the surrounding control logic format-agnostic;
  // the FP6 default path drives identical values to before.
  val proj_in      = WireDefault(VecInit(Seq.fill(32)(0.U(rdataWidth.W))))
  val proj_lut     = WireDefault(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))
  val proj_nearest = Wire(Vec(32, UInt(raddrWidth.W)))

  lutConfig.projFormat match {
    case LutFP8E5M2 =>
      // E5M2-capable build: instantiate BOTH finders and select at runtime by altfmt, so an FP6 output
      // (altfmt=0) projects with the FP6 finder and an E5M2 output (altfmt=1) with the FP8 finder. LUT
      // entries are rdataWidth (8) wide; the FP6 finder reads the low 6 bits (FP6 codes are 6-bit).
      val fp8Finders = Seq.fill(32)(Module(new FP8NearestFinder(altfmt = true)))
      val fp6Finders = Seq.fill(32)(Module(new FP6E3M2NearestFinder()))
      val proj_lut6  = VecInit(proj_lut.map(_(5, 0)))
      for (i <- 0 until 32) {
        fp8Finders(i).io.in     := proj_in(i)
        fp8Finders(i).io.in_lut := proj_lut
        fp6Finders(i).io.in_fp6 := proj_in(i)(5, 0)
        fp6Finders(i).io.in_lut := proj_lut6
        proj_nearest(i) := Mux(io.mx_fp8_altfmt, fp8Finders(i).io.nearestIdx, fp6Finders(i).io.nearestIdx)
      }
    case LutFP8E4M3 =>
      // All-formats build (mxGemminiAll): requant output is a code0 fp8 sub-format (E4M3 = altfmt0, E5M2 =
      // altfmt1, both 8-bit codes) OR a code1 fp6 sub-format (E3M2 = altfmt0, E2M3 = altfmt1, 6-bit codes in
      // the low 6). Pick the finder family by output code, then the sub-format by mx_fp8_altfmt (symmetric
      // with the input decode). fp8 finders take the full 8-bit codebook; fp6 finders the low 6 bits.
      val fp8e4Finders = Seq.fill(32)(Module(new FP8NearestFinder(false)))  // E4M3 (exp4)
      val fp8e5Finders = Seq.fill(32)(Module(new FP8NearestFinder(true)))   // E5M2 (exp5)
      val fp6e3Finders = Seq.fill(32)(Module(new FP6E3M2NearestFinder()))
      val fp6e2Finders = Seq.fill(32)(Module(new FP6E2M3NearestFinder()))
      val proj_lut6    = VecInit(proj_lut.map(_(5, 0)))
      for (i <- 0 until 32) {
        fp8e4Finders(i).io.in     := proj_in(i)
        fp8e4Finders(i).io.in_lut := proj_lut
        fp8e5Finders(i).io.in     := proj_in(i)
        fp8e5Finders(i).io.in_lut := proj_lut
        fp6e3Finders(i).io.in_fp6 := proj_in(i)(5, 0)
        fp6e3Finders(i).io.in_lut := proj_lut6
        fp6e2Finders(i).io.in_fp6 := proj_in(i)(5, 0)
        fp6e2Finders(i).io.in_lut := proj_lut6
        val fp6Idx = Mux(io.mx_fp8_altfmt, fp6e2Finders(i).io.nearestIdx, fp6e3Finders(i).io.nearestIdx)
        val fp8Idx = Mux(io.mx_fp8_altfmt, fp8e5Finders(i).io.nearestIdx, fp8e4Finders(i).io.nearestIdx)
        proj_nearest(i) := Mux(io.output_mx_format === 1.U, fp6Idx, fp8Idx)
      }
    case LutFP6E2M3 => // single-format E2M3 build: 6-bit codes, only the E2M3 finder
      val finders = Seq.fill(32)(Module(new FP6E2M3NearestFinder()))
      for (i <- 0 until 32) {
        finders(i).io.in_fp6 := proj_in(i)
        finders(i).io.in_lut := proj_lut
        proj_nearest(i)      := finders(i).io.nearestIdx
      }
    case _ => // LutFP6E3M2 (default)
      val finders = Seq.fill(32)(Module(new FP6E3M2NearestFinder()))
      for (i <- 0 until 32) {
        finders(i).io.in_fp6 := proj_in(i)
        finders(i).io.in_lut := proj_lut
        proj_nearest(i)      := finders(i).io.nearestIdx
      }
  }

  when(io.quant_fp6.valid) {
    used_lut_act_out := lutCache_act_out(counter_act_out >> io.quant_lut_update_granularity)
    proj_lut := used_lut_act_out
    for (i <- 0 until 32) {
      proj_in(i) := io.quant_fp6.bits(i)
      projectedIndices(i) := proj_nearest(i)
    }
    projectedDataValid := true.B
    when (counter_act_out === ((io.loop_bound_i << 5.U) - 1.U)){
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
  //val used_lut_w = WireDefault(VecInit(Seq.fill(16)(0.U(rdataWidth.W))))
  val counter_act = RegInit(0.U(log2Ceil(16).W))
  //val counter_w = RegInit(0.U(log2Ceil(lutConfig(1)._1).W))

  // dontTouch(used_lut_w)
  dontTouch(counter_act)
  dontTouch(used_lut_act_0)
  dontTouch(used_lut_act_1)

  for (i <- 0 until sp_banks) {
    // Each bank has its own deprojected_bits
    val deprojected_bits = WireDefault(VecInit(Seq.fill(outputnumLanes)(0.U(rdataWidth.W))))

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
      // Finding 12: gate on the per-response operand tag (carried aligned with the read data) instead
      // of the hardcoded-latency io.read_a. This fires the deproj + advances counter_i exactly when the
      // projected data arrives -> no dropped/zeroed leading beats and no codebook-row phase offset.
      when(io.spad_projected_data(i).resp.bits.read_a) {
        when(counter_i === ((io.loop_bound_i << 4.U) - 1.U)){
          counter_i := 0.U
        }.otherwise{
          counter_i := counter_i + 1.U
        } 
        // W3 (placement-agnostic): route by the runtime read_a tag, not a hardwired bank half.
        // The old `if (i < sp_banks/2)` pinned activation deproj to the lower bank half, forcing the
        // SW to place the weight operand in the upper half. Any bank whose read carries read_a now does
        // the activation deprojection, so the operand may live in ANY scratchpad bank.
        used_lut_act_0 := lutCache_act_in((counter_i << 1.U) >> io.quant_lut_update_granularity)
        used_lut_act_1 := lutCache_act_in(((counter_i << 1.U) + 1.U) >> io.quant_lut_update_granularity)
        for (k <- 0 until 16) { //act data layout is k15a1, k15a0, k14a1, k14a0,...,k0a1,k0a0, each 4 bit, total 32*4
          val chunk_4bit_0 = io.spad_projected_data(i).resp.bits.data(2*k*4 + 3, 2*k*4)
          val chunk_4bit_1 = io.spad_projected_data(i).resp.bits.data(2*k*4 + 7, 2*k*4 + 4)
          val deprojected_bit_0 = used_lut_act_0(chunk_4bit_0)
          val deprojected_bit_1 = used_lut_act_1(chunk_4bit_1)
          deprojected_bits(2*k) := deprojected_bit_0
          deprojected_bits(2*k + 1) := deprojected_bit_1
        }
      }
      when(io.spad_projected_data(i).resp.bits.read_d) {
       when(counter_j === ((io.loop_bound_j << 4.U) - 1.U)){
          counter_j := 0.U
        }.otherwise{
          counter_j := counter_j + 1.U
        }
        // W3 (placement-agnostic): route by the runtime read_d tag, not a hardwired bank half.
        // The old `if (i >= sp_banks/2)` pinned weight deproj to the upper bank half. Any bank whose
        // read carries read_d now does the weight deprojection, so weights may live in ANY bank.
        for (k <- 0 until 32) {
          val used_lut_w = lutCache_weight((((counter_j >> 4.U) << 5.U) + k.U) >> io.quant_lut_update_granularity)
          val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
          deprojected_bits(k) := used_lut_w(chunk_4bit)

        }
        // when (counter_w === (lutConfig(1)._1 - 1).U){
        //   counter_w := 0.U
        // }.otherwise{
        //   counter_w := counter_w + 1.U
        // }
      }
      io.spad_deprojected_data(i).resp.bits.data := Cat(deprojected_bits.reverse)
      io.spad_deprojected_data(i).resp.valid := true.B
      io.spad_deprojected_data(i).resp.bits.fromDMA := io.spad_projected_data(i).resp.bits.fromDMA
      io.spad_deprojected_data(i).resp.bits.weight_mx_format := io.spad_projected_data(i).resp.bits.weight_mx_format
      io.spad_deprojected_data(i).resp.bits.input_mx_format := io.spad_projected_data(i).resp.bits.input_mx_format
    }
  }
}

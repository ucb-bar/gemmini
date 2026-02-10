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
  val counter_j = Input(UInt(iterator_bitwidth.W))
  val counter_i = Input(UInt(iterator_bitwidth.W))
  val a_fire_counter = Input(UInt(log2Up(16).W))
  val b_fire_counter = Input(UInt(log2Up(16).W))
  val quant_lut_update_granularity = Input(UInt(lutConfig.lutUpdateRegularityWidth.W))
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
  val lutCache_weight_0 = Seq.fill(lutConfig(0)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_weight_1 = Seq.fill(lutConfig(0)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_in_0 = Seq.fill(lutConfig(1)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_in_1 = Seq.fill(lutConfig(1)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_out_0 = Seq.fill(lutConfig(2)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_out_1 = Seq.fill(lutConfig(2)._1)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  //io.lut_write.ready := !io.lutReadEnable
  
  val lutCache_act_in = WireInit(VecInit.fill(lutConfig(0)._1)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_act_in_flag = RegInit(false.B)
  val lutCache_act_in_buffer_0_read_enable = RegInit(false.B)
  val lutCache_act_in_buffer_1_read_enable = RegInit(false.B)
  val lutCache_act_in_buffer_select = RegInit(false.B)
  val counter_i_reg = RegNext(io.counter_i)
  // val counter_w = RegInit(0.U(5.W))
  // val counter_act = RegInit(0.U(5.W))
  // dontTouch(counter_act)
  // dontTouch(counter_w)
  when(io.lut_write_act_in.fire){
    when(lutCache_act_in_flag === false.B){
      for (lane <- 0 until lutConfig(0)._1) {
        for (entry <- 0 until 16) {
          lutCache_act_in_0(lane)(entry) := io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_act_in_0(lane)(entry) := io.lut_write_act_in.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }  
      lutCache_act_in_flag := ~lutCache_act_in_flag
      lutCache_act_in_buffer_0_read_enable := true.B
    }.otherwise {
      for (lane <- 0 until lutConfig(0)._1) {
        for (entry <- 0 until 16) {
          lutCache_act_in_1(lane)(entry) := io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_act_in_1(lane)(entry) := io.lut_write_act_in.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
      }
    }  
      lutCache_act_in_flag := ~lutCache_act_in_flag
      lutCache_act_in_buffer_1_read_enable := true.B
    }
  }
  
  val quant_lut_update_granularity = io.quant_lut_update_granularity
  val gran_div_32 = quant_lut_update_granularity >> 5.U
  val lutCache_update_enable_act_in = WireInit(0.U(1.W))
  val counter_mask = (1.U << Log2(gran_div_32)) - 1.U
  // FP6 only: tile = 32 elements, period = regularity/32 tiles
  lutCache_update_enable_act_in := ((io.counter_i& counter_mask) === 0.U) && ((counter_i_reg& counter_mask) === (gran_div_32 -1.U))

  when(lutCache_update_enable_act_in === 1.U){ //32 is the maxblock under fp6
    when(lutCache_act_in_buffer_0_read_enable && (lutCache_act_in_buffer_select === false.B)){
      lutCache_act_in_buffer_0_read_enable := false.B
    }
    when(lutCache_act_in_buffer_1_read_enable && (lutCache_act_in_buffer_select === true.B)){
      lutCache_act_in_buffer_1_read_enable := false.B
    }
    when(lutCache_act_in_buffer_0_read_enable || lutCache_act_in_buffer_1_read_enable){
      lutCache_act_in_buffer_select := ~lutCache_act_in_buffer_select
    }
  }

  when((lutCache_act_in_buffer_select === false.B) && lutCache_act_in_buffer_0_read_enable){
    lutCache_act_in := lutCache_act_in_0
  }.elsewhen((lutCache_act_in_buffer_select === true.B) && lutCache_act_in_buffer_1_read_enable){
    lutCache_act_in := lutCache_act_in_1
  }

  io.lut_write_act_in.ready := !lutCache_act_in_buffer_0_read_enable || !lutCache_act_in_buffer_1_read_enable

  val lutCache_weight = WireInit(VecInit.fill(lutConfig(1)._1)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_weight_flag = RegInit(false.B)
  val lutCache_weight_buffer_0_read_enable = RegInit(false.B)
  val lutCache_weight_buffer_1_read_enable = RegInit(false.B)
  val lutCache_weight_buffer_select = RegInit(false.B)
  val counter_j_reg = RegNext(io.counter_j)

  when(io.lut_write_weight.fire){
    when(lutCache_weight_flag === false.B){
      for (lane <- 0 until lutConfig(1)._1) {
        for (entry <- 0 until 16) {
          lutCache_weight_0(lane)(entry) := io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_weight_0(lane)(entry) := io.lut_write_weight.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_weight_flag := ~lutCache_weight_flag
      lutCache_weight_buffer_0_read_enable := true.B
    }.otherwise {
      for (lane <- 0 until lutConfig(1)._1) {
        for (entry <- 0 until 16) {
          lutCache_weight_1(lane)(entry) := io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_weight_1(lane)(entry) := io.lut_write_weight.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_weight_flag := ~lutCache_weight_flag
      lutCache_weight_buffer_1_read_enable := true.B
    }
  }
  
  val lutCache_update_enable_w_in = WireInit(0.U(1.W))
  // FP6 only: tile = 32 elements, period = regularity_w/32 tiles
  lutCache_update_enable_w_in := ((io.counter_j& counter_mask) === 0.U) && ((counter_j_reg& counter_mask) === (gran_div_32 -1.U))

  when(lutCache_update_enable_w_in === 1.U){
    when(lutCache_weight_buffer_0_read_enable && (lutCache_weight_buffer_select === false.B)){
      lutCache_weight_buffer_0_read_enable := false.B
    }
    when(lutCache_weight_buffer_1_read_enable && (lutCache_weight_buffer_select === true.B)){
      lutCache_weight_buffer_1_read_enable := false.B
    }
    when(lutCache_weight_buffer_0_read_enable || lutCache_weight_buffer_1_read_enable){
      lutCache_weight_buffer_select := ~lutCache_weight_buffer_select
    }
  }

  when((lutCache_weight_buffer_select === false.B) && lutCache_weight_buffer_0_read_enable){
    lutCache_weight := lutCache_weight_0
  }.elsewhen((lutCache_weight_buffer_select === true.B) && lutCache_weight_buffer_1_read_enable){
    lutCache_weight := lutCache_weight_1
  }

  io.lut_write_weight.ready := !lutCache_weight_buffer_0_read_enable || !lutCache_weight_buffer_1_read_enable

  val lutCache_act_out = WireInit(VecInit.fill(outputnumLanes)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_act_out_flag = RegInit(false.B)
  val lutCache_act_out_buffer_0_read_enable = RegInit(false.B)
  val lutCache_act_out_buffer_1_read_enable = RegInit(false.B)
  val lutCache_act_out_buffer_select = RegInit(false.B)

  when(io.lut_write_act_out.fire){
    when(lutCache_act_out_flag === false.B){
      for (lane <- 0 until lutConfig(2)._1) {
        for (entry <- 0 until 16) {
          lutCache_act_out_0(lane)(entry) := io.lut_write_act_out.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_act_out_0(lane)(entry) := io.lut_write_act_out.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_act_out_flag := ~lutCache_act_out_flag
      lutCache_act_out_buffer_0_read_enable := true.B
    }.otherwise {
      for (lane <- 0 until lutConfig(2)._1) {
        for (entry <- 0 until 16) {
          lutCache_act_out_1(lane)(entry) := io.lut_write_act_out.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
          //lutCache_act_out_1(lane)(entry) := io.lut_write_act_out.bits.data(0)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_act_out_flag := ~lutCache_act_out_flag
      lutCache_act_out_buffer_1_read_enable := true.B
    }
  }
  
  val lutCache_update_enable_act_out = WireInit(0.U(1.W))
  // FP6 only: tile = 32 elements, period = regularity_act_out/32 tiles
  lutCache_update_enable_act_out := ((io.counter_i& counter_mask) === 0.U) && ((counter_i_reg& counter_mask) === (gran_div_32 -1.U))

  when(lutCache_update_enable_act_out === 1.U){
    when(lutCache_act_out_buffer_0_read_enable && (lutCache_act_out_buffer_select === false.B)){
      lutCache_act_out_buffer_0_read_enable := false.B
    }
    when(lutCache_act_out_buffer_1_read_enable && (lutCache_act_out_buffer_select === true.B)){
      lutCache_act_out_buffer_1_read_enable := false.B
    }
    when(lutCache_act_out_buffer_0_read_enable || lutCache_act_out_buffer_1_read_enable){
      lutCache_act_out_buffer_select := ~lutCache_act_out_buffer_select
    }
  }

  when((lutCache_act_out_buffer_select === false.B) && lutCache_act_out_buffer_0_read_enable){
    lutCache_act_out := lutCache_act_out_0
  }.elsewhen((lutCache_act_out_buffer_select === true.B) && lutCache_act_out_buffer_1_read_enable){
    lutCache_act_out := lutCache_act_out_1
  }

  io.lut_write_act_out.ready := !lutCache_act_out_buffer_0_read_enable || !lutCache_act_out_buffer_1_read_enable

  val projectedIndices = RegInit(VecInit(Seq.fill(lutConfig(2)._1)(0.U(raddrWidth.W))))
  val projectedDataValid = RegInit(false.B)
  val counter_act_out = RegInit(0.U(log2Ceil(lutConfig(2)._1).W))
  //TODO: double check if this is the correct way to do nearest neighbor search, aligning with the algorithm implementation
  when((lutCache_act_out_buffer_0_read_enable || lutCache_act_out_buffer_1_read_enable) && io.quant_fp6.valid) {
    
    for (i <- 0 until lutConfig(2)._1) {
      val inputFp6 = io.quant_fp6.bits(i)
      val distances = VecInit((0 until 16).map { j =>
        val diff = Mux(inputFp6 > lutCache_act_out(counter_act_out)(j), 
                      inputFp6 - lutCache_act_out(counter_act_out)(j), 
                      lutCache_act_out(counter_act_out)(j) - inputFp6)
        diff
      })
      
      val minIdx = distances.zipWithIndex.map { case (dist, idx) =>
        (dist, idx.U(raddrWidth.W))
      }.reduce { (a, b) =>
        val selectA = a._1 <= b._1
        (Mux(selectA, a._1, b._1), Mux(selectA, a._2, b._2))
      }._2

      projectedIndices(i) := minIdx
    }
    projectedDataValid := true.B
    when (counter_act_out === (outputnumLanes - 1).U){
      counter_act_out := 0.U
    }.otherwise{
      counter_act_out := counter_act_out + 1.U
    }
  }.otherwise {
    projectedDataValid := false.B
  }

  io.projected_data.valid := projectedDataValid
  io.projected_data.bits := projectedIndices
  
  
  for (i <- 0 until sp_banks) {
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
  
    when(io.spad_projected_data(i).resp.valid) {
      val deprojected_bits = Wire(Vec(outputnumLanes, UInt(6.W)))
      for (k <- 0 until 32) {
      deprojected_bits(k) := 0.U
    }
    when((lutCache_act_in_buffer_0_read_enable || lutCache_act_in_buffer_1_read_enable)) {
      for (k <- 0 until 32) {
        val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
        deprojected_bits(k) := lutCache_act_in(io.a_fire_counter(3, 0))(chunk_4bit)  
      }
    }
    when ((lutCache_weight_buffer_0_read_enable || lutCache_weight_buffer_1_read_enable)) {
      for (k <- 0 until 32) {
        val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
        deprojected_bits(k) := lutCache_weight(io.b_fire_counter(3, 0))(chunk_4bit)  
      }
    }
      
    io.spad_deprojected_data(i).resp.bits.data := deprojected_bits.asUInt
    io.spad_deprojected_data(i).resp.valid := true.B
    io.spad_deprojected_data(i).resp.bits.fromDMA := io.spad_projected_data(i).resp.bits.fromDMA
    io.spad_deprojected_data(i).resp.bits.weight_mx_format := io.spad_projected_data(i).resp.bits.weight_mx_format
    io.spad_deprojected_data(i).resp.bits.input_mx_format := io.spad_projected_data(i).resp.bits.input_mx_format
    }
  }
}
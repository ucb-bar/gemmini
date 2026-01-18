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
<<<<<<< HEAD
  val lutReadEnable = Output(Bool()) 
  val lut_write =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig))) //input
  val quant_fp6 = Flipped(Valid(Vec(outputnumLanes, UInt(lutConfig.rdataWidth.W)))) //input
  val projected_data = Valid(Vec(outputnumLanes, UInt(lutConfig.raddrWidth.W))) //output
  // val spad_projected_data = Flipped(Decoupled(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected)))) 
  // val spad_deprojected_data = Decoupled(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)))
=======
  val lut_write_weight =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig))) //input
  val lut_write_act_in =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig))) //input
  val lut_write_act_out =  Flipped(Decoupled(new QuantLutWriteBundle(lutConfig))) //input
  val quant_fp6 = Flipped(Valid(Vec(outputnumLanes, UInt(lutConfig.rdataWidth.W)))) //input
  val projected_data = Valid(Vec(outputnumLanes, UInt(lutConfig.raddrWidth.W))) //output
>>>>>>> e1e04af (change the QuantLut as double buffer)
  val spad_projected_data   = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width_projected))
  val spad_deprojected_data = Vec(sp_banks, Flipped(new ScratchpadReadIO(sp_bank_entries, sp_width)))
  val counter_j = Input(UInt(iterator_bitwidth.W))
  val counter_i = Input(UInt(iterator_bitwidth.W))
  val a_fire = Input(Bool())
  val b_fire = Input(Bool())
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
  val lutCache_weight_0 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_weight_1 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_in_0 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_in_1 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_out_0 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  val lutCache_act_out_1 = Seq.fill(32)(RegInit(VecInit(Seq.fill(16)(0.U(rdataWidth.W)))))
  //io.lut_write.ready := !io.lutReadEnable
  
  val lutCache_act_in = WireInit(VecInit.fill(32)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_act_in_flag = RegInit(false.B)
  val lutCache_act_in_buffer_0_read_enable = RegInit(false.B)
  val lutCache_act_in_buffer_1_read_enable = RegInit(false.B)
  val lutCache_act_in_buffer_select = RegInit(false.B)
  val counter_i_reg = RegNext(io.counter_i)
  when(io.lut_write_act_in.fire){
    when(lutCache_act_in_flag === false.B){
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_act_in_0(lane)(entry) := io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
      }
      lutCache_act_in_flag := ~lutCache_act_in_flag
      lutCache_act_in_buffer_0_read_enable := true.B
    }
    }.otherwise {
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_act_in_1(lane)(entry) := io.lut_write_act_in.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
      }
      lutCache_act_in_flag := ~lutCache_act_in_flag
      lutCache_act_in_buffer_1_read_enable := true.B
      }
    }
  }
  
  when((io.counter_i(log2Ceil(lut_update_regularity_act_in)-1, 0) === 0.U) && (counter_i_reg(log2Ceil(lut_update_regularity_act_in)-2, 0) === 32.U)){ //32 is the maxblock under fp6
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

  val lutCache_weight = WireInit(VecInit.fill(32)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_weight_flag = RegInit(false.B)
  val lutCache_weight_buffer_0_read_enable = RegInit(false.B)
  val lutCache_weight_buffer_1_read_enable = RegInit(false.B)
  val lutCache_weight_buffer_select = RegInit(false.B)
  val counter_j_reg = RegNext(io.counter_j)

  when(io.lut_write_weight.fire){
    when(lutCache_weight_flag === false.B){
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_weight_0(lane)(entry) := io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_weight_flag := ~lutCache_weight_flag
      lutCache_weight_buffer_0_read_enable := true.B
    }.otherwise {
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_weight_1(lane)(entry) := io.lut_write_weight.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_weight_flag := ~lutCache_weight_flag
      lutCache_weight_buffer_1_read_enable := true.B
    }
  }

  when((io.counter_j(log2Ceil(lut_update_regularity_w)-1, 0) === 0.U) && (counter_j_reg(log2Ceil(lut_update_regularity_w)-2, 0) === 32.U)){
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

  val lutCache_act_out = WireInit(VecInit.fill(32)(VecInit.fill(16)(0.U(rdataWidth.W))))
  val lutCache_act_out_flag = RegInit(false.B)
  val lutCache_act_out_buffer_0_read_enable = RegInit(false.B)
  val lutCache_act_out_buffer_1_read_enable = RegInit(false.B)
  val lutCache_act_out_buffer_select = RegInit(false.B)
  val counter_k_reg = RegNext(io.counter_k)

  when(io.lut_write_act_out.fire){
    when(lutCache_act_out_flag === false.B){
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_act_out_0(lane)(entry) := io.lut_write_act_out.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_act_out_flag := ~lutCache_act_out_flag
      lutCache_act_out_buffer_0_read_enable := true.B
    }.otherwise {
      for (lane <- 0 until 32) {
        for (entry <- 0 until 16) {
          lutCache_act_out_1(lane)(entry) := io.lut_write_act_out.bits.data(lane)((entry+1)*rdataWidth-1, entry*rdataWidth)
        }
      }
      lutCache_act_out_flag := ~lutCache_act_out_flag
      lutCache_act_out_buffer_1_read_enable := true.B
    }
  }

  when((io.counter_i(log2Ceil(lut_update_regularity_act_out)-1, 0) === 0.U) && (counter_i_reg(log2Ceil(lut_update_regularity_act_out)-2, 0) === 32.U)){
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

  val projectedIndices = RegInit(VecInit(Seq.fill(outputnumLanes)(0.U(raddrWidth.W))))
  val projectedDataValid = RegInit(false.B)

  //TODO: double check if this is the correct way to do nearest neighbor search, aligning with the algorithm implementation
  when((lutCache_act_out_buffer_0_read_enable || lutCache_act_out_buffer_1_read_enable) && io.quant_fp6.valid) {
    for (i <- 0 until outputnumLanes) {
      val inputFp6 = io.quant_fp6.bits(i)
      val distances = VecInit((0 until 16).map { j =>
        val diff = Mux(inputFp6 > lutCache_act_out(i)(j), 
                      inputFp6 - lutCache_act_out(i)(j), 
                      lutCache_act_out(i)(j) - inputFp6)
        diff
      })
      
      val minIdx = distances.zipWithIndex.map { case (dist, idx) =>
        (dist, idx.U(5.W))
      }.reduce { (a, b) =>
        val selectA = a._1 <= b._1
        (Mux(selectA, a._1, b._1), Mux(selectA, a._2, b._2))
      }._2

      projectedIndices(i) := minIdx
    }
    projectedDataValid := true.B
  }.otherwise {
    projectedDataValid := false.B
  }

  io.projected_data.valid := projectedDataValid
  io.projected_data.bits := projectedIndices

  for (i <- 0 until sp_banks) {
    io.spad_deprojected_data(i).req <> io.spad_projected_data(i).req

    io.spad_deprojected_data(i).resp.valid := false.B
    io.spad_deprojected_data(i).resp.bits  := 0.U.asTypeOf(new ScratchpadReadResp(sp_width))
    io.spad_projected_data(i).resp.ready   := io.spad_deprojected_data(i).resp.ready

    when(io.spad_projected_data(i).resp.valid) {
      val deprojected_bits = Wire(Vec(32, UInt(6.W)))
      for (k <- 0 until 32) {
      deprojected_bits(k) := 0.U
    }
    when(io.a_fire && (lutCache_act_in_buffer_0_read_enable || lutCache_act_in_buffer_1_read_enable)) {
      for (k <- 0 until 32) {
        val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
        deprojected_bits(k) := lutCache_act_in(k)(chunk_4bit)  
      }
    }.elsewhen(io.b_fire && (lutCache_weight_buffer_0_read_enable || lutCache_weight_buffer_1_read_enable)) {
      for (k <- 0 until 32) {
        val chunk_4bit = io.spad_projected_data(i).resp.bits.data((k+1)*4-1, k*4)
        deprojected_bits(k) := lutCache_weight(k)(chunk_4bit)  
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
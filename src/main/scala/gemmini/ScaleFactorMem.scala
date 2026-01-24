package gemmini

import chisel3._
import chisel3.util._


class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W) 
  val scaling_enable = Bool() 
}

class ScalingFactorReadResp(numRows: Int, numCols: Int) extends Bundle {  
  val combined_scales = Vec(numRows, UInt(9.W))
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, numRows: Int, numCols: Int, meshRows:Int, tileRows: Int) extends Bundle {
  // writes happen to all interleaved banks for a line
  val scale_mem_write_w = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  val scale_mem_write_act = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  val read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth)))
  val read_resp = Decoupled(new ScalingFactorReadResp(numRows, numCols))
  val dataType = Input(UInt(2.W))
  val scaleMemCnlt = Input(new ScalingFactorCnlt(meshRows*tileRows)) // dummy output to match interface
  val counter_i = Input(UInt(16.W))
  val counter_j = Input(UInt(16.W))  
  val counter_k = Input(UInt(16.W))
}

class ScalingFactorMem(
  depth: Int = 128,                     
  bankWidth: Int = 128,                 
  actOutputScalingWidth: Int = 8,       
  numBanks: Int = 8,
  testConfig: Boolean = false ,
  meshRows: Int,
  tileRows: Int,
) extends Module {

  val rowAddrWidth = log2Ceil(depth)  
  val bytesPerBank = bankWidth / 8        
  val AddrWidth = rowAddrWidth  + log2Ceil(numBanks)     
  val bankaddressWidth = log2Ceil(numBanks) 
  val totalScales = 32
  val counterWidth = log2Ceil(totalScales)  
  val writeDataWidth = bankWidth * 2
  val io = IO(new ScalingFactorMemIO(
    AddrWidth, 
    writeDataWidth, 
    2*meshRows*tileRows,  // 32 rows (activation scales)
    2*meshRows*tileRows,   // 32 columns (weight scales)
    meshRows,
    tileRows,
  ))

  val counter_a_fire = io.scaleMemCnlt.counter_a
  val counter_b_fire = io.scaleMemCnlt.counter_b
  val fire_a = io.scaleMemCnlt.fire_a
  val fire_b = io.scaleMemCnlt.fire_b
  val write_baseAddr_act = io.scaleMemCnlt.baseAddress_act
  val write_baseAddr_w = io.scaleMemCnlt.baseAddress_w
  val initByte = 0x7e.U(8.W)
  val defaultRow = VecInit(Seq.fill(bytesPerBank)(initByte)) 
  val counter_i = io.counter_i
  val counter_j = io.counter_j
  val counter_k = io.counter_k

  val combined_scales_buffer = WireInit(VecInit(Seq.fill(2*meshRows*tileRows)(
  VecInit(Seq.fill(2*meshRows*tileRows)(0.U(9.W))))))
  //val combined_scales_buffer_r = RegInit(VecInit(Seq.fill(2*meshRows*tileRows)(
  //VecInit(Seq.fill(2*meshRows*tileRows)(0.U(9.W))))))
  val combined_scales_valid = WireDefault(false.B)
  val bankDataT = Vec(bytesPerBank, UInt(8.W))
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, bankDataT))
  val fp8Mode = io.dataType === 2.U
  
  val weight_write_buffer_sel = RegInit(false.B) 
  val weight_buffer_0_read_enable = RegInit(false.B)
  val weight_buffer_1_read_enable = RegInit(false.B)
  val weight_write_counter = RegInit(0.U(8.W))
  val write_row_addr_w = io.scale_mem_write_w.bits.addr + (write_baseAddr_w >> (log2Ceil(2*meshRows*tileRows)))
 
  val weight_buffer_write_full = RegInit(false.B)
  when(io.scale_mem_write_w.fire) {
    val write_bytes_low = io.scale_mem_write_w.bits.data(bytesPerBank * 8 - 1, 0).asTypeOf(bankDataT)
    val write_bytes_high = io.scale_mem_write_w.bits.data(bytesPerBank * 2 * 8 - 1, bytesPerBank * 8).asTypeOf(bankDataT)
    when(weight_write_buffer_sel === false.B) { 
      weight_write_counter := weight_write_counter + 1.U
      banks(4).write(write_row_addr_w, write_bytes_low)
      banks(5).write(write_row_addr_w, write_bytes_high)
      weight_buffer_0_read_enable := true.B
    }.otherwise{
      weight_write_counter := weight_write_counter + 1.U
      banks(6).write(write_row_addr_w, write_bytes_low)
      banks(7).write(write_row_addr_w, write_bytes_high)
      weight_buffer_1_read_enable := true.B
    }
    weight_write_buffer_sel := ~weight_write_buffer_sel
    when((weight_write_counter === (depth - 1).U)){
      weight_write_counter := 0.U
      
      // when(weight_write_buffer_sel === false.B){
      //   weight_buffer_0_read_enable := true.B
      // }.otherwise{
      //   weight_buffer_1_read_enable := true.B
      // }
    }
  }
  
  val act_write_buffer_sel = RegInit(false.B) 
  val act_buffer_0_read_enable = RegInit(false.B)
  val act_buffer_1_read_enable = RegInit(false.B)
  val act_write_counter = RegInit(0.U(8.W))
  val write_row_addr_act = io.scale_mem_write_act.bits.addr  + (write_baseAddr_act >> (log2Ceil(2*meshRows*tileRows)))
  when(io.scale_mem_write_act.fire) {
    val write_bytes_low = io.scale_mem_write_act.bits.data(bytesPerBank * 8 - 1, 0).asTypeOf(bankDataT)
    val write_bytes_high = io.scale_mem_write_act.bits.data(bytesPerBank * 2 * 8 - 1, bytesPerBank * 8).asTypeOf(bankDataT)
    when(act_write_buffer_sel === false.B) {  // ✓
      banks(0).write(write_row_addr_act, write_bytes_low)
      banks(1).write(write_row_addr_act, write_bytes_high)
      act_buffer_0_read_enable := true.B
    }.otherwise{
      act_write_counter := act_write_counter + 1.U
      banks(2).write(write_row_addr_act, write_bytes_low)
      banks(3).write(write_row_addr_act, write_bytes_high)
      act_buffer_1_read_enable := true.B
    }
    act_write_buffer_sel := ~act_write_buffer_sel
    when((act_write_counter === (depth - 1).U)){
      act_write_counter := 0.U
    }
  }
  val max_block_fp8 = meshRows * tileRows
  val max_block_non_fp8 = 2*meshRows * tileRows
  val read_row_addr = WireDefault(counter_k >> log2Ceil(max_block_non_fp8))
  io.scale_mem_write_w.ready := ((!weight_buffer_0_read_enable) || (!weight_buffer_1_read_enable)) || (weight_write_counter ===0.U || (weight_write_counter > read_row_addr)) 
  io.scale_mem_write_act.ready := (!act_buffer_0_read_enable) || (!act_buffer_1_read_enable) || (act_write_counter ===0.U || ((act_write_counter === read_row_addr)))
  val act_read_buffer_select = RegInit(false.B)
  val weight_read_buffer_select = RegInit(false.B)
  val act_read_counter = RegInit(0.U(8.W))
  val weight_read_counter = RegInit(0.U(8.W))

  when(io.read_req.fire && io.read_req.bits.scaling_enable){
    act_read_buffer_select := ~act_read_buffer_select
    weight_read_buffer_select := ~weight_read_buffer_select
    when(act_buffer_0_read_enable && ((act_write_counter === read_row_addr))){
        act_buffer_0_read_enable := false.B
    }
    when(act_buffer_1_read_enable && ((act_write_counter === read_row_addr))){
        act_buffer_1_read_enable := false.B
    }
 
    when(weight_buffer_0_read_enable && ((weight_write_counter === read_row_addr))){
      weight_buffer_0_read_enable := false.B
    }
    when(weight_buffer_1_read_enable && ((weight_write_counter === read_row_addr))){
      weight_buffer_1_read_enable := false.B
    }
  }
  
  val act_scales = WireDefault(VecInit(Seq.fill(meshRows*tileRows * 2)(0.U(8.W))))
  val weight_scales = WireDefault(VecInit(Seq.fill(meshRows*tileRows * 2)(0.U(8.W))))
  val scale_counter = RegInit(0.U(6.W))

  def multiplyScalesE8M0(act: UInt, weight: UInt): UInt = {
    val sum = act +& weight
    sum(8, 0)
  }

  val read_fire = io.read_req.fire && io.read_req.bits.scaling_enable && ((act_buffer_0_read_enable && weight_buffer_0_read_enable) || (act_buffer_1_read_enable && weight_buffer_1_read_enable)  )
  val read_fire_real = read_fire && (scale_counter === 0.U) 
  




  val act_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  val weight_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  val act_bank_sel =  Wire(UInt(2.W))
  val weight_bank_sel =  Wire(UInt(2.W))
  
  act_bank_sel := 0.U
  weight_bank_sel := 0.U

  val read_fire_banks = VecInit(Seq(
    read_fire_real && act_buffer_0_read_enable && weight_buffer_0_read_enable && (act_bank_sel === 0.U) ,     // bank 0
    read_fire_real && act_buffer_0_read_enable && weight_buffer_0_read_enable && (act_bank_sel === 1.U),     // bank 1
    read_fire_real && act_buffer_1_read_enable && weight_buffer_1_read_enable && (act_bank_sel === 2.U),     // bank 2
    read_fire_real && act_buffer_1_read_enable && weight_buffer_1_read_enable && (act_bank_sel === 3.U),     // bank 3
    read_fire_real && weight_buffer_0_read_enable && act_buffer_0_read_enable && (weight_bank_sel === 0.U),  // bank 4
    read_fire_real && weight_buffer_0_read_enable && act_buffer_0_read_enable && (weight_bank_sel === 1.U),  // bank 5
    read_fire_real && weight_buffer_1_read_enable && act_buffer_0_read_enable && (weight_bank_sel === 2.U),  // bank 6
    read_fire_real && weight_buffer_1_read_enable && act_buffer_0_read_enable && (weight_bank_sel === 3.U)  // bank 7
  ))
  
    
  val bank_data = VecInit((0 until 8).map { i => if (testConfig) defaultRow  else banks(i).read(read_row_addr, read_fire_banks(i))})

  when(fp8Mode){
    act_bank_sel := counter_i(1+log2Ceil(max_block_fp8), log2Ceil(max_block_fp8))
    weight_bank_sel := counter_j(1+log2Ceil(max_block_fp8), log2Ceil(max_block_fp8))
    when(act_bank_sel === 0.U && (act_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(0)(i)
      }
    }.elsewhen( act_bank_sel === 1.U && (act_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(1)(i)
      }
    }.elsewhen( act_bank_sel === 2.U && (act_buffer_1_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(2)(i)
      }
    }.elsewhen( act_bank_sel === 3.U && (act_buffer_1_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(3)(i)
      }
    }   
    when(weight_bank_sel === 0.U && (weight_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(4)(i)
      }
    }.elsewhen( weight_bank_sel === 1.U && (weight_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(5)(i)
      }
    }.elsewhen( weight_bank_sel === 2.U && (weight_buffer_1_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(6)(i)
      }
    }.elsewhen( weight_bank_sel === 3.U && (weight_buffer_1_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(7)(i)
      }
    }
  }.otherwise{
    act_bank_sel := Cat(0.U(1.W), counter_i(log2Ceil(max_block_non_fp8))) 
    weight_bank_sel := Cat(0.U(1.W),counter_j(log2Ceil(max_block_non_fp8)))
    when(act_bank_sel(0) === 0.U && (act_buffer_0_read_enable) && (weight_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(0)(i)
        act_bank_data_vec(meshRows*tileRows+i) := bank_data(1)(i)
      }
    }.elsewhen(act_bank_sel(0) === 1.U && (act_buffer_0_read_enable) && (weight_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data(2)(i)
        act_bank_data_vec(meshRows*tileRows+i) := bank_data(3)(i)
      }
    }
    when(counter_j(1+log2Ceil(max_block_non_fp8), log2Ceil(max_block_non_fp8)) === 1.U && (weight_buffer_0_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(4)(i)
        weight_bank_data_vec(meshRows*tileRows+i) := bank_data(5)(i)
      }
    }.elsewhen( counter_j(1+log2Ceil(max_block_non_fp8), log2Ceil(max_block_non_fp8)) === 1.U && (weight_buffer_1_read_enable)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data(6)(i)
        weight_bank_data_vec(meshRows*tileRows+i) := bank_data(7)(i)
      }
    }
  }


  val read_addr_reg = RegNext(io.read_req.bits.addr)
  val read_fire_d1 = RegNext(read_fire, false.B)
  val read_fire_real_d1 = RegNext(read_fire_real, false.B)
  io.read_resp.bits.combined_scales.foreach(_ := 0.U)
  io.read_resp.valid := false.B
  io.read_req.ready := io.read_req.bits.scaling_enable && ((act_buffer_0_read_enable && weight_buffer_0_read_enable) || (act_buffer_1_read_enable && weight_buffer_1_read_enable))

   for(i <- 0 until 2*meshRows*tileRows) {     
    for(j <- 0 until 2*meshRows*tileRows) {
      when(read_fire_d1) {
        combined_scales_buffer(i)(j) := multiplyScalesE8M0(act_scales(i), weight_scales(j))
        combined_scales_valid := true.B  
      }.otherwise {
        combined_scales_buffer(i)(j) := 0.U
        combined_scales_valid := false.B  
      }
    }
  }
  

  //when(read_fire_real_d1 && (scale_counter === 0.U) ) {
  val  combined_scales_buffer_r = combined_scales_buffer
  

  when(read_fire_d1) {
    for(i <- 0 until 2*meshRows*tileRows) {
      act_scales(i) := act_bank_data_vec(i)
      weight_scales(i) := weight_bank_data_vec(i)
      // printf(p"[ScalingFactorMem] Read act_scales=${act_scales(i) }\n")
      // printf(p"[ScalingFactorMem] Read weight_scales=${weight_scales(i) }\n")
    }
    //printf(p"[ScalingFactorMem] Read scales from row=${read_addr_reg}\n")
    io.read_resp.valid := combined_scales_valid
    when (((scale_counter === ((meshRows*tileRows-1).U) && fp8Mode) || (scale_counter === ((2*meshRows*tileRows-1).U) && !fp8Mode))) {
      scale_counter := 0.U
    }.otherwise{
      scale_counter := scale_counter +& 1.U
      when(fp8Mode){
        for(j <- 0 until meshRows*tileRows){
          when(scale_counter === 0.U){
          io.read_resp.bits.combined_scales(j) := combined_scales_buffer(0)(j)
          // printf(p"[ScalingFactorMem] Read scale from row=${scale_counter}, and get the scale=${io.read_resp.bits.combined_scales(j)}\n")
          }.otherwise{
            io.read_resp.bits.combined_scales(j) := combined_scales_buffer_r(scale_counter)(j)
            // printf(p"[ScalingFactorMem] Read scale from row=${scale_counter}, and get the scale=${io.read_resp.bits.combined_scales(j)}\n")
          }
        }
      }.otherwise{
        for(j <- 0 until 2*meshRows*tileRows){
          when(scale_counter === 0.U){
          io.read_resp.bits.combined_scales(j) := combined_scales_buffer(0)(j)
          //printf(p"[ScalingFactorMem] Read scale from row=${scale_counter}, weight_row=${weight_row_counter}, and get the scale=${io.read_resp.bits.combined_scales(j)}\n")
          }.otherwise{
            io.read_resp.bits.combined_scales(j) := combined_scales_buffer_r(scale_counter)(j)
          }
        }
      }
    }
  }
  
 
  

  
  // when(reset.asBool || !io.read_req.bits.scaling_enable) {
  //   //current_row := 0.U
  //   combined_scales_valid := false.B
  // }
}

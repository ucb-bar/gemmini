package gemmini

import chisel3._
import chisel3.util._


class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W) 
  val scaling_enable = Bool() 
}

class ScalingFactorReadResp(numRows: Int, numCols: Int) extends Bundle {  
  val combined_scales = Vec(16, UInt(36.W))
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, numRows: Int, numCols: Int, meshRows:Int, tileRows: Int) extends Bundle {
  // writes happen to all interleaved banks for a line
  val scale_mem_write_w = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  val scale_mem_write_act = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  val read_req = Flipped(Decoupled(new ScalingFactorReadReq(7)))
  val read_resp = Decoupled(new ScalingFactorReadResp(numRows, numCols))
  val dataType = Input(UInt(2.W))
  val scaleMemCntl = Input(new ScalingFactorCntl(meshRows*tileRows)) // dummy output to match interface
  val counter_i = Input(UInt(16.W))
  val counter_j = Input(UInt(16.W))  
  val counter_k = Input(UInt(16.W))
  val i = Input(UInt(16.W))
  val j = Input(UInt(16.W))  
  val k = Input(UInt(16.W))
}

class ScalingFactorMem(
  depth: Int = 8,                     
  sramWidth: Int = 128,                 
  actOutputScalingWidth: Int = 8,       
  numBanks: Int = 8,
  testConfig: Boolean = false ,
  meshRows: Int,
  tileRows: Int,
) extends Module {
  val totalSizeBytes = 2*8*sramWidth*numBanks/8
  val bytesPerBank = sramWidth / 8        
  val AddrWidth = log2Ceil(totalSizeBytes)      
  val bankaddressWidth = log2Ceil(numBanks) 
  val totalScales = 32
  val counterWidth = log2Ceil(totalScales)  
  val io = IO(new ScalingFactorMemIO(
    AddrWidth, 
    64, 
    2*meshRows*tileRows,  // 32 rows (activation scales)
    2*meshRows*tileRows,   // 32 columns (weight scales)
    meshRows,
    tileRows,
  ))

  val initByte = 0x7e.U(8.W)
  val defaultRow = VecInit(Seq.fill(bytesPerBank)(initByte)) 
  val counter_i = io.counter_i
  val counter_j = io.counter_j
  val counter_k = io.counter_k
  

  val read_fire = io.read_req.fire && io.read_req.bits.scaling_enable
  val read_fire_d1 = RegNext(read_fire, false.B)
  val counter_i_runtime = RegInit(0.U(9.W))
  val counter_j_runtime = RegInit(0.U(9.W)) 
  val counter_k_runtime = RegInit(0.U(9.W))
  dontTouch(counter_i_runtime)
  dontTouch(counter_j_runtime)
  dontTouch(counter_k_runtime)
  val scale_counter = RegInit(0.U(4.W))

  when(read_fire_d1) {
    when((counter_i_runtime === (io.scaleMemCntl.loop_bound_i - 1.U)) && 
        (scale_counter === 15.U)) {
      counter_i_runtime := 0.U
    }.elsewhen(scale_counter === 15.U) {
      counter_i_runtime := counter_i_runtime + 1.U
    }

    when((counter_j_runtime === (io.scaleMemCntl.loop_bound_j - 1.U)) && 
        (counter_i_runtime === (io.scaleMemCntl.loop_bound_i - 1.U)) && 
        (scale_counter === 15.U)) {
      counter_j_runtime := 0.U
    }.elsewhen((counter_i_runtime === (io.scaleMemCntl.loop_bound_i - 1.U)) && 
              (scale_counter === 15.U)) {
      counter_j_runtime := counter_j_runtime + 1.U
    }

    when((counter_j_runtime === (io.scaleMemCntl.loop_bound_j - 1.U)) && 
        (counter_i_runtime === (io.scaleMemCntl.loop_bound_i - 1.U)) && 
        (counter_k_runtime === (io.scaleMemCntl.loop_bound_k - 1.U)) && 
        (scale_counter === 15.U)) {
      counter_k_runtime := 0.U
    }.elsewhen((counter_j_runtime === (io.scaleMemCntl.loop_bound_j - 1.U)) && 
              (counter_i_runtime === (io.scaleMemCntl.loop_bound_i - 1.U)) && 
              (scale_counter === 15.U)) {
      counter_k_runtime := counter_k_runtime + 1.U
    }
  }
  


  val combined_scales_buffer = WireInit(VecInit(Seq.fill(2*meshRows*tileRows)(
  VecInit(Seq.fill(2*meshRows*tileRows)(0.U(9.W))))))
  val combined_scales_valid = WireDefault(false.B)
  val bankDataT = Vec(bytesPerBank, UInt(8.W))
  val banks =RegInit(VecInit(Seq.fill(2*8*numBanks)(VecInit(Seq.fill(bytesPerBank)(0.U(8.W))))))

  val fp8Mode = io.dataType === 0.U
  
  val write_addr_w = io.scale_mem_write_w.bits.addr
  val write_weight_counter  = RegInit(0.U(2.W))
  val write_weight_full_row = RegInit(0.U(sramWidth.W))
  val write_row_addr_w = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(8*numBanks) - 1 ,log2Ceil(bytesPerBank)))
  dontTouch(write_row_addr_w)
  when(write_weight_counter === 1.U && io.scale_mem_write_w.fire){
      write_weight_counter := 0.U
      val write_bytes = Cat(io.scale_mem_write_w.bits.data, write_weight_full_row(63, 0)).asTypeOf(bankDataT)
      banks(write_row_addr_w) := write_bytes;
  }.elsewhen(io.scale_mem_write_w.fire) {
      write_weight_counter := write_weight_counter + 1.U
      write_weight_full_row := Cat(write_weight_full_row(127, 64), io.scale_mem_write_w.bits.data) 
  }
  


  val write_addr_a = io.scale_mem_write_act.bits.addr
  val write_act_counter  = RegInit(0.U(2.W))
  val write_act_full_row = RegInit(0.U(sramWidth.W))

  val write_row_addr_act = WireInit(write_addr_a(log2Ceil(bytesPerBank) + log2Ceil(8*numBanks) -1, log2Ceil(bytesPerBank)))
  dontTouch(write_row_addr_act)
  when(write_act_counter === 1.U && io.scale_mem_write_act.fire) {
      write_act_counter := 0.U
      val write_bytes = Cat(io.scale_mem_write_act.bits.data, write_act_full_row(63, 0))
      banks((8 * numBanks).U + write_row_addr_act) := write_bytes.asTypeOf(bankDataT)
  }.elsewhen(io.scale_mem_write_act.fire) {
      write_act_counter := write_act_counter + 1.U
      write_act_full_row := Cat(write_act_full_row(127, 64), io.scale_mem_write_act.bits.data)
  }


  val max_block_fp8 = meshRows * tileRows
  val max_block_non_fp8 = 2 * meshRows * tileRows

  // k: MX block = 32 elements, k-tile = 16 -> 2 k-tiles per scale group -> shift 1
  val ij_shift = Mux(fp8Mode, 0.U, 1.U)
  val read_row_addr_act = WireDefault((io.scaleMemCntl.loop_bound_i << ij_shift) * (counter_k_runtime >> 1.U) + (counter_i_runtime << ij_shift) + (numBanks * 8).U + (io.scaleMemCntl.scale_mem_read_act_sel << log2Ceil(numBanks * 8 / 2).U))
  val read_row_addr_w = WireDefault((io.scaleMemCntl.loop_bound_j << ij_shift) * (counter_k_runtime >> 1.U) + (counter_j_runtime << ij_shift) + (io.scaleMemCntl.scale_mem_read_w_sel << log2Ceil(numBanks * 8 / 2).U))
  
  dontTouch(read_row_addr_act)
  dontTouch(read_row_addr_w)
  io.scale_mem_write_w.ready := true.B
  io.scale_mem_write_act.ready := true.B

  val act_scales = WireDefault(VecInit(Seq.fill(meshRows*tileRows * 2)(0.U(8.W))))
  val weight_scales = WireDefault(VecInit(Seq.fill(meshRows*tileRows * 2)(0.U(8.W))))
  dontTouch(act_scales)
  dontTouch(weight_scales)
  
 

  def multiplyScalesE8M0(act: UInt, weight: UInt): UInt = {
    val sum = act +& weight
    sum(8, 0)
  }

  

  //val read_fire_real = read_fire && (scale_counter === 0.U) && !read_fire_d1
  


  val act_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  val weight_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  dontTouch(act_bank_data_vec)
  dontTouch(weight_bank_data_vec)
  
  val bank_data_0_act = if (testConfig) defaultRow  else banks(read_row_addr_act).asTypeOf(bankDataT)
  val bank_data_0_w = if (testConfig) defaultRow  else banks(read_row_addr_w).asTypeOf(bankDataT)

  when(fp8Mode){
    when(read_fire_d1){
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data_0_act(i)
      }
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data_0_w(i)
      }
    }
  }.otherwise{
    when(read_fire_d1) {
      for (i <- 0 until meshRows*tileRows) {
        act_bank_data_vec(i) := bank_data_0_act(i)
        act_bank_data_vec(meshRows*tileRows+i) := banks(read_row_addr_act + 1.U)(i)
      }
      for (i <- 0 until meshRows*tileRows) {
        weight_bank_data_vec(i) := bank_data_0_w(i)
        weight_bank_data_vec(meshRows*tileRows+i) := banks(read_row_addr_w + 1.U)(i)
      }
    }
  }


  io.read_resp.bits.combined_scales.foreach(_ := 0.U)
  io.read_resp.valid := false.B
  io.read_req.ready := io.read_req.bits.scaling_enable 

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
  
  when(read_fire_d1) {
    io.read_resp.valid := combined_scales_valid
    when(scale_counter === 15.U) {
      scale_counter := 0.U
    }.otherwise{
      scale_counter := scale_counter + 1.U
    }
    when(fp8Mode){
      for(i <- 0 until meshRows*tileRows) {
        val single_scale = combined_scales_buffer(scale_counter)(i)
//          val single_scale = 127.U // TODO: get rid of this, just for debugging
        io.read_resp.bits.combined_scales(i) := Cat(0.U(27.W), single_scale(8, 0))
      }
    }.otherwise{
      for(i <- 0 until meshRows*tileRows) {
        val single_scale_0 = combined_scales_buffer((scale_counter << 1.U))(2*i)
        val single_scale_1 = combined_scales_buffer((scale_counter << 1.U))(2*i+1)
        val single_scale_2 = combined_scales_buffer((scale_counter << 1.U)+1.U)(2*i)
        val single_scale_3 = combined_scales_buffer((scale_counter << 1.U)+1.U)(2*i+1)
        val single_scale = Cat(single_scale_3, single_scale_2, single_scale_1, single_scale_0)
        io.read_resp.bits.combined_scales(i) := single_scale
      }
    }
    for(i <- 0 until 2*meshRows*tileRows) {
      act_scales(i) := act_bank_data_vec(i)
      weight_scales(i) := weight_bank_data_vec(i)
    }
  }
}


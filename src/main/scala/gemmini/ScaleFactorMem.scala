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
  val mx_multi_elem = Input(Bool())   // throughput: 2 elements/lane (E4M3-quad included), datatype-independent
  val mx_fp8_altfmt = Input(Bool())   // code0 sub-format: 1 = E5M2 (4-bit LUT output, nibble scale layout)
  val scaleMemCntl = Input(new ScalingFactorCntl(meshRows*tileRows)) // dummy output to match interface
  val counter_i = Input(UInt(16.W))
  val counter_j = Input(UInt(16.W))  
  val counter_k = Input(UInt(16.W))
  val i = Input(UInt(16.W))
  val j = Input(UInt(16.W))  
  val k = Input(UInt(16.W))
}

class ScalingFactorMem(
  depth: Int = 64,                     
  sramWidth: Int = 128,                 
  actOutputScalingWidth: Int = 8,       
  numBanks: Int = 8,
  testConfig: Boolean = false ,
  meshRows: Int,
  tileRows: Int,
) extends Module {
  val scaleMemSizeFactor = 4
  val doubleBufferFactor = 2
  val totalSizeBytes = 64*sramWidth*numBanks/8
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
  val depth_sram = 64
  val half_sfMem_rows = depth_sram * numBanks / 2
  val bankDataT = Vec(bytesPerBank, UInt(8.W))
  val banks = Seq.fill(numBanks)(SyncReadMem(depth_sram, bankDataT))

  val read_fire = io.read_req.fire && io.read_req.bits.scaling_enable
  val read_fire_d1 = RegNext(read_fire, false.B)
  val counter_i_runtime = RegInit(0.U(9.W))
  val counter_j_runtime = RegInit(0.U(9.W)) 
  val counter_k_runtime = RegInit(0.U(9.W))
  dontTouch(counter_i_runtime)
  dontTouch(counter_j_runtime)
  dontTouch(counter_k_runtime)
  val scale_counter = RegInit(0.U(4.W))
  val scale_counter_d1 = RegNext(scale_counter)
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
  val combined_scales_buffer_reg = RegInit(VecInit(Seq.fill(2*meshRows*tileRows)(
  VecInit(Seq.fill(2*meshRows*tileRows)(0.U(9.W))))))
  val combined_scales_valid = WireDefault(false.B)
  //val bankDataT = Vec(bytesPerBank, UInt(8.W))
  //val banks =RegInit(VecInit(Seq.fill(scaleMemSizeFactor*doubleBufferFactor*8*numBanks)(VecInit(Seq.fill(bytesPerBank)(0.U(8.W))))))

  // fp8Mode selects the single-throughput E4M3 scale structure (write banking, read banking, combine layout).
  // E4M3-quad is code0 but MULTI throughput -> it uses the non-fp8 (2-element) scale layout like E5M2, matching
  // how its scales are loaded. Gate on !mx_multi_elem so only single E4M3 takes the fp8 path.
  // fp8Mode = E4M3-single only (code0, 8-bit direct, 16-wide scale layout). E5M2 (code0/altfmt1) is a
  // 4-bit LUT output whose scales use the nibble (32-wide) layout, matching the requant coalescer's
  // isNibble grouping -- so it must NOT take the fp8 scale path.
  val fp8Mode = io.dataType === 0.U && !io.mx_multi_elem && !io.mx_fp8_altfmt
  
  val write_addr_w = io.scale_mem_write_w.bits.addr
  val write_weight_counter  = RegInit(0.U(2.W))
  val write_weight_full_row = RegInit(0.U(sramWidth.W))
  //val write_row_addr_w = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(half_sfMem_rows) ,log2Ceil(bytesPerBank)))
  // FP8 mode
  val write_row_addr_w_fp8 = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) - 1, log2Ceil(bytesPerBank)))
  val bank_idx_w_fp8 = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 1, log2Ceil(bytesPerBank) + log2Ceil(depth_sram)))
  // Non-FP8 mode  
  val bank_idx_w_internal = WireInit(write_addr_w(log2Ceil(bytesPerBank)))
  val write_row_addr_w_nonfp8 = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(depth_sram), log2Ceil(bytesPerBank) + 1))
  val bank_idx_w_nonfp8 = WireInit(write_addr_w(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 2, log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 1))
  dontTouch(write_row_addr_w_fp8)
  dontTouch(bank_idx_w_fp8)
  dontTouch(bank_idx_w_internal)
  dontTouch(write_row_addr_w_nonfp8)
  dontTouch(bank_idx_w_nonfp8)

  when(write_weight_counter === 1.U && io.scale_mem_write_w.fire){
    write_weight_counter := 0.U
    val write_bytes = Cat(io.scale_mem_write_w.bits.data, write_weight_full_row(63, 0)).asTypeOf(bankDataT)
    val bank_sel_w = Mux(fp8Mode, bank_idx_w_fp8, Cat(bank_idx_w_nonfp8, bank_idx_w_internal))
    val write_row_addr_w = Mux(fp8Mode, write_row_addr_w_fp8, write_row_addr_w_nonfp8)
    for (b <- 0 until 4) {
      when(bank_sel_w === b.U) {
        banks(b).write(write_row_addr_w, write_bytes)
      }
    }
  }.elsewhen(io.scale_mem_write_w.fire) {
      write_weight_counter := write_weight_counter + 1.U
      write_weight_full_row := Cat(write_weight_full_row(127, 64), io.scale_mem_write_w.bits.data) 
  }
  
  val write_addr_a = io.scale_mem_write_act.bits.addr
  val write_act_counter  = RegInit(0.U(2.W))
  val write_act_full_row = RegInit(0.U(sramWidth.W))
  // FP8 mode
  val write_row_addr_act_fp8 = WireInit(write_addr_a(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) - 1, log2Ceil(bytesPerBank)))
  val bank_idx_act_fp8 = WireInit(write_addr_a(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 1, log2Ceil(bytesPerBank) + log2Ceil(depth_sram)))
  // Non-FP8 mode
  val bank_idx_act_internal = WireInit(write_addr_a(log2Ceil(bytesPerBank)))
  val write_row_addr_act_nonfp8 = WireInit(write_addr_a(log2Ceil(bytesPerBank) + log2Ceil(depth_sram), log2Ceil(bytesPerBank) + 1))
  val bank_idx_act_nonfp8 = WireInit(write_addr_a(log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 2, log2Ceil(bytesPerBank) + log2Ceil(depth_sram) + 1))
  dontTouch(write_row_addr_act_fp8)
  dontTouch(bank_idx_act_fp8)
  dontTouch(bank_idx_act_internal)
  dontTouch(write_row_addr_act_nonfp8)
  dontTouch(bank_idx_act_nonfp8)
  when(write_act_counter === 1.U && io.scale_mem_write_act.fire) {
      write_act_counter := 0.U
      val write_bytes = Cat(io.scale_mem_write_act.bits.data, write_act_full_row(63, 0)).asTypeOf(bankDataT)
      val bank_sel_act = Mux(fp8Mode, bank_idx_act_fp8, Cat(bank_idx_act_nonfp8, bank_idx_act_internal))
      val write_row_addr_act = Mux(fp8Mode, write_row_addr_act_fp8, write_row_addr_act_nonfp8)
      for (b <- 0 until 4) {
        when(bank_sel_act === b.U) {
          banks(b + 4).write(write_row_addr_act, write_bytes)
        }
      }
  }.elsewhen(io.scale_mem_write_act.fire) {
      write_act_counter := write_act_counter + 1.U
      write_act_full_row := Cat(write_act_full_row(127, 64), io.scale_mem_write_act.bits.data)
  }


  val max_block_fp8 = meshRows * tileRows
  val max_block_non_fp8 = 2 * meshRows * tileRows

  // k: MX block = 32 elements, k-tile = 16 -> 2 k-tiles per scale group -> shift 1
  val row_addr_width = log2Ceil(2*depth_sram)
  val read_row_addr_act = Wire(UInt(row_addr_width.W))
  val read_row_addr_w = Wire(UInt(row_addr_width.W))
  read_row_addr_act := (io.scaleMemCntl.loop_bound_i) * (counter_k_runtime >> 1.U) + (counter_i_runtime)
  read_row_addr_w := (io.scaleMemCntl.loop_bound_j) * (counter_k_runtime >> 1.U) + (counter_j_runtime)
  val read_bank_idx_act = WireDefault(read_row_addr_act(row_addr_width - 1))
  val read_bank_idx_w = WireDefault(read_row_addr_w(row_addr_width - 1))
  val read_row_addr_act_real = WireDefault(read_row_addr_act(row_addr_width - 2, 0))
  val read_row_addr_w_real = WireDefault(read_row_addr_w(row_addr_width - 2, 0))
  val double_buffer_act_sel = io.scaleMemCntl.scale_mem_read_act_sel.asBool
  val double_buffer_w_sel = io.scaleMemCntl.scale_mem_read_w_sel.asBool

  dontTouch(read_row_addr_act)
  dontTouch(read_row_addr_w)
  dontTouch(read_bank_idx_act)
  dontTouch(read_bank_idx_w)
  dontTouch(read_row_addr_act_real)
  dontTouch(read_row_addr_w_real)

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
  val read_fire_real = WireDefault(false.B) 
  val read_fire_real_d = RegNext(read_fire_real)
  
  read_fire_real := read_fire && (scale_counter === 0.U) 
  
  // val act_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  // val weight_bank_data_vec = WireInit(VecInit(Seq.fill(meshRows*tileRows*2)(0.U(8.W))))
  // dontTouch(act_bank_data_vec)
  // dontTouch(weight_bank_data_vec)

  val read_fire_banks = VecInit(Seq(
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_w === 0.U) && !double_buffer_w_sel   , !double_buffer_w_sel  )),  
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_w === 1.U) && !double_buffer_w_sel   , !double_buffer_w_sel  )),  
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_w === 0.U) && double_buffer_w_sel    , double_buffer_w_sel  )),  
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_w === 1.U) && double_buffer_w_sel    , double_buffer_w_sel  )),  
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_act === 0.U) && !double_buffer_act_sel , !double_buffer_act_sel)),
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_act === 1.U) && !double_buffer_act_sel , !double_buffer_act_sel)),
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_act === 0.U) && double_buffer_act_sel  , double_buffer_act_sel)),
    read_fire_real && (Mux(fp8Mode, (read_bank_idx_act === 1.U) && double_buffer_act_sel  , double_buffer_act_sel)),
  ))
  val read_fire_banks_d1 = RegNext(read_fire_banks)
  val bank_data_0 = VecInit((0 until 4).map { i =>  banks(i).read(read_row_addr_w_real, read_fire_banks(i))})
  val bank_data_1 = VecInit((0 until 4).map { i =>  banks(i+4).read(read_row_addr_act_real, read_fire_banks(i+4))})

  when(fp8Mode){
    when(read_fire_banks_d1(4)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(0)(i)
      }
    }.elsewhen(read_fire_banks_d1(5)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(1)(i)
      }
    }.elsewhen(read_fire_banks_d1(6)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(2)(i)
      }
    }.elsewhen(read_fire_banks_d1(7)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(3)(i)
      }
    }
    when(read_fire_banks_d1(0)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(0)(i)
      }
    }.elsewhen(read_fire_banks_d1(1)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(1)(i)
      }
    }.elsewhen(read_fire_banks_d1(2)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(2)(i)
      }
    }.elsewhen(read_fire_banks_d1(3)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(3)(i)
      }
    }
  }.otherwise{
    when(read_fire_banks_d1(4)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(0)(i)
        act_scales(meshRows*tileRows+i) := bank_data_1(1)(i)
      }
    }.elsewhen(read_fire_banks_d1(6)) {
      for (i <- 0 until meshRows*tileRows) {
        act_scales(i) := bank_data_1(2)(i)
        act_scales(meshRows*tileRows+i) := bank_data_1(3)(i)
      }
    }
    when(read_fire_banks_d1(0)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(0)(i)
        weight_scales(meshRows*tileRows+i) := bank_data_0(1)(i)
      }
    }.elsewhen(read_fire_banks_d1(2)) {
      for (i <- 0 until meshRows*tileRows) {
        weight_scales(i) := bank_data_0(2)(i)
        weight_scales(meshRows*tileRows+i) := bank_data_0(3)(i)
      }
    }
  }


  // val bank_data_0_act = VecInit((0 until 4).map { i => if (testConfig) defaultRow  else banks(i).read(read_row_addr_act, read_fire_banks(i))})
  // val bank_data_0_w = VecInit((0 until 4).map { i => if (testConfig) defaultRow  else banks(i+4).read(read_row_addr_w, read_fire_banks(i+4))})

  // when(fp8Mode){
  //   when(read_fire_d1){
  //     for (i <- 0 until meshRows*tileRows) {
  //       act_bank_data_vec(i) := bank_data_0_act(i)
  //     }
  //     for (i <- 0 until meshRows*tileRows) {
  //       weight_bank_data_vec(i) := bank_data_0_w(i)
  //     }
  //   }
  // }.otherwise{
  //   when(read_fire_d1) {
  //     for (i <- 0 until meshRows*tileRows) {
  //       act_bank_data_vec(i) := bank_data_0_act(i)
  //       act_bank_data_vec(meshRows*tileRows+i) := banks(read_row_addr_act + 1.U)(i)
  //     }
  //     for (i <- 0 until meshRows*tileRows) {
  //       weight_bank_data_vec(i) := bank_data_0_w(i)
  //       weight_bank_data_vec(meshRows*tileRows+i) := banks(read_row_addr_w + 1.U)(i)
  //     }
  //   }
  // }


  io.read_resp.bits.combined_scales.foreach(_ := 0.U)
  io.read_resp.valid := false.B
  io.read_req.ready := io.read_req.bits.scaling_enable 

  // for(i <- 0 until 2*meshRows*tileRows) {
  //   act_scales(i) := act_bank_data_vec(i)
  //   weight_scales(i) := weight_bank_data_vec(i)
  // }

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

  when(read_fire) {
    when(scale_counter === 15.U) {
      scale_counter := 0.U
    }.otherwise{
      scale_counter := scale_counter + 1.U
    }
  }

  when(read_fire_real_d){
    combined_scales_buffer_reg := combined_scales_buffer
  }

  when(read_fire_d1) {
    io.read_resp.valid := combined_scales_valid
    when(fp8Mode){
      when(scale_counter_d1 === 0.U){
        for(i <- 0 until meshRows*tileRows) {
          val single_scale = combined_scales_buffer(scale_counter_d1)(i)
          io.read_resp.bits.combined_scales(i) := Cat(0.U(27.W), single_scale(8, 0))
        }
      }.otherwise{
        for(i <- 0 until meshRows*tileRows) {
          val single_scale = combined_scales_buffer_reg(scale_counter_d1)(i)
          io.read_resp.bits.combined_scales(i) := Cat(0.U(27.W), single_scale(8, 0))
        }
      }
    }.otherwise{
      when(scale_counter_d1 === 0.U){
        for (i <- 0 until meshRows*tileRows) {
          val scale_row_0 = combined_scales_buffer((scale_counter_d1 << 1.U))
          val scale_row_1 = combined_scales_buffer((scale_counter_d1 << 1.U) + 1.U)
          val scale_all = Cat(scale_row_1.asUInt, scale_row_0.asUInt)
          io.read_resp.bits.combined_scales(i) := scale_all(36*i+35, 36*i)
        }
      }.otherwise{
        for (i <- 0 until meshRows*tileRows) {
          val scale_row_0 = combined_scales_buffer_reg((scale_counter_d1 << 1.U))      
          val scale_row_1 = combined_scales_buffer_reg((scale_counter_d1 << 1.U) + 1.U) 
          val scale_all = Cat(scale_row_1.asUInt, scale_row_0.asUInt)
          io.read_resp.bits.combined_scales(i) := scale_all(36*i+35, 36*i)
        }
      }
    }
  }
}

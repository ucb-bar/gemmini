package gemmini

import chisel3._
import chisel3.util._

// Interface Summary Table:
// ═══════════════════════════════════════════════════════════════════════════
// 2 Banks Design:
// - Bank 0: Activation scaling factors (256 bits = 32 × 8-bit scales)
// - Bank 1: Weight scaling factors (256 bits = 32 × 8-bit scales)
// - Each read accesses both banks at the same row address
// - Output: combined_scales = act_scale * weight_scales (E8M0 multiplication)

// ┌──────────────────────────┬───────────┬────────────┬──────────────────────┐
// │ Signal Name              │ Direction │ Width      │ Description          │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ WRITE INTERFACE          │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ write.valid              │ Input     │ 1          │ Write request valid  │
// │ write.ready              │ Output    │ 1          │ Write ready          │
// │ write.bits.addr          │ Input     │ 9          │ [8:1]=row addr, [0]=bank sel │
// │ write.bits.data          │ Input     │ 256        │ Write data (32 bytes)│
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ CONTROL                  │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ scaling_enable           │ Input     │ 1          │ Enable read phase    │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ READ INTERFACE           │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ read_req.valid           │ Input     │ 1          │ Read valid           │
// │ read_req.ready           │ Output    │ 1          │ Read ready           │
// │ read_req.bits.addr       │ Input     │ 8          │ Row address          │
// │ read_resp.valid          │ Output    │ 1          │ Response valid       │
// │ read_resp.ready          │ Input     │ 1          │ Response ready       │
// │ read_resp.bits.combined_scales│ Output│ 32×8      │ Combined scales      │
// └──────────────────────────┴───────────┴────────────┴──────────────────────┘

class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)  
  val data = UInt(dataWidth.W) 
  val scaling_enable = Bool()
}

class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)  
}

class ScalingFactorReadResp(scaleSize: Int) extends Bundle {  
  val combined_scales = Vec(scaleSize, UInt(8.W)) 
  val new_scales = Bool()
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, scaleSize: Int) extends Bundle {
  val write = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))  
  val read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth - 1)))      
  val read_resp = Decoupled(new ScalingFactorReadResp(scaleSize))               
}


class ScalingFactorMem(
  depth: Int = 256,                     
  bankWidth: Int = 256,                   
  actOutputScalingWidth: Int = 8,         
  numBanks: Int = 2                     
) extends Module {


  val rowAddrWidth = log2Ceil(depth)      
  val bytesPerBank = bankWidth / 8        
  val writeAddrWidth = rowAddrWidth + 1   
  val numScalesPerBank = bankWidth / actOutputScalingWidth  
  val counterWidth = log2Ceil(numScalesPerBank)  
  
  val io = IO(new ScalingFactorMemIO(writeAddrWidth, bankWidth, numScalesPerBank))
  
  // Create 2 banks: Bank 0 = Activation, Bank 1 = Weight
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, Vec(bytesPerBank, UInt(8.W))))
  
  io.write.ready := !io.write.bits.scaling_enable
  
  when(io.write.fire) {
    val bank_sel = io.write.bits.addr(0)      // LSB = bank select
    val row_addr = io.write.bits.addr >> 1.U  // High bits = row address

    val write_bytes = Wire(Vec(bytesPerBank, UInt(8.W)))
    for(i <- 0 until bytesPerBank) {
      write_bytes(i) := io.write.bits.data((i+1)*8-1, i*8)
    }
  
    for(bank_idx <- 0 until numBanks) {
      when(bank_sel === bank_idx.U) {
        banks(bank_idx).write(row_addr, write_bytes)
        //printf(p"[ScalingFactorMem] Write: bank=${bank_idx}, row=${row_addr}, data=0x${Hexadecimal(io.write.bits.data)}\n")
      }
    }
  }
  
  val current_row = RegInit(0.U(rowAddrWidth.W))
  val scale_counter = RegInit(0.U(counterWidth.W))
  val scale_counter_d = RegNext(scale_counter)
  val act_scales = Reg(Vec(numScalesPerBank, UInt(8.W)))
  val weight_scales = Reg(Vec(numScalesPerBank, UInt(8.W)))
  
  val act_scale_data_valid = RegInit(false.B)
  val weight_scale_data_valid = RegInit(false.B)
  //val combined_scales_buffer = Reg(Vec(numScalesPerBank, UInt(8.W)))
  val combined_scales_buffer_w = VecInit(Seq.fill(numScalesPerBank)(0.U(9.W)))
  val combined_scales_valid = RegInit(false.B)
  val new_scales = RegInit(false.B)

  // Read ready signal for read requests when scaling is enabled, during psum accumulation phase
  io.read_req.ready := io.write.bits.scaling_enable
  io.read_resp.bits.new_scales := new_scales
 
  def multiplyScalesE8M0(act: UInt, weight: UInt): UInt = {
    val bias_e9m0 = 255.U
    val sum = act +& weight
    Mux(sum >= bias_e9m0, sum - bias_e9m0, 0.U)
  }
  
  val read_fire = io.read_req.fire && io.write.bits.scaling_enable
  val act_bank_data_vec = banks(0).read(io.read_req.bits.addr, io.read_req.fire )
  val weight_bank_data_vec = banks(1).read(io.read_req.bits.addr, read_fire)
  
 
  val read_addr_reg = RegNext(io.read_req.bits.addr)
  val scale_counter_enable = RegInit(false.B)
  
  when(read_fire) {
    current_row := io.read_req.bits.addr
    scale_counter := 0.U
    combined_scales_valid := false.B
    scale_counter_enable := true.B
  }

  //val read_fire_d1 = RegNext(read_fire, false.B)
  
  when(read_fire) {
    for(i <- 0 until numScalesPerBank) {
      act_scales(i) := act_bank_data_vec(i)
      weight_scales(i) := weight_bank_data_vec(i)
    }
    printf(p"[ScalingFactorMem] Data captured from row=${read_addr_reg}\n")
  }
  
  
  when(scale_counter_enable && io.write.bits.scaling_enable) {
    val current_act_scale = act_scales(scale_counter_d)
    for(i <- 0 until numScalesPerBank) {
      combined_scales_buffer_w(i) := multiplyScalesE8M0(current_act_scale, weight_scales(i))
      when(scale_counter_d === 0.U) {
        printf(p"[ScalingFactorMem] combined_scales_buffer_w=${combined_scales_buffer_w(i)}, current_act_scale=${current_act_scale}, current_w_scale=${weight_scales(i)}\n")
      }
    }
    
    when(scale_counter_d === (numScalesPerBank - 1).U) {
      scale_counter := 0.U
      combined_scales_valid := false.B
      current_row := current_row + 1.U
      scale_counter_enable := false.B
      new_scales := true.B
    }.otherwise {
      scale_counter := scale_counter + 1.U
      combined_scales_valid := true.B
      new_scales := false.B
    }
  }
  
  io.read_resp.valid := combined_scales_valid
  io.read_resp.bits.combined_scales := VecInit(Seq.fill(numScalesPerBank)(0.U(8.W)))
  when(combined_scales_valid) {
      io.read_resp.bits.combined_scales := combined_scales_buffer_w
  }
  
  
  when(reset.asBool || !io.write.bits.scaling_enable) {
    scale_counter := 0.U
    current_row := 0.U
    act_scale_data_valid := false.B
    weight_scale_data_valid := false.B
    combined_scales_valid := false.B
  }
}

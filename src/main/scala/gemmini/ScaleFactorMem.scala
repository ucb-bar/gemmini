package gemmini

import chisel3._
import chisel3.util._

// Interface Summary Table:
// ═══════════════════════════════════════════════════════════════════════════
// 4 Banks Design:
// - Banks 0,1: Activation scaling factors (2 × 128 bits = 32 × 8-bit scales)
// - Banks 2,3: Weight scaling factors (2 × 128 bits = 32 × 8-bit scales)
// - Output: combined_scales = outer_product(act_scales, weight_scales)
//   Result is a 16×16 matrix (each scale is E8M0 format)

class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)  
  val data = UInt((2*dataWidth).W)  // 256 bits
}

class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W) 
  val scaling_enable = Bool() 
}

class ScalingFactorReadResp(numRows: Int, numCols: Int) extends Bundle {  
  val combined_scales = Vec(numRows, Vec(numCols, UInt(9.W)))
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, numRows: Int, numCols: Int) extends Bundle {
  val write = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))  
  val read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth)))      
  val read_resp = Decoupled(new ScalingFactorReadResp(numRows, numCols))               
}

class ScalingFactorMem(
  depth: Int = 256,                     
  bankWidth: Int = 128,                 
  actOutputScalingWidth: Int = 8,       
  numBanks: Int = 4                     
) extends Module {

  val rowAddrWidth = log2Ceil(depth)      
  val bytesPerBank = bankWidth / 8        
  val writeAddrWidth = rowAddrWidth + 1   
  val numScalesPerBank = bankWidth / actOutputScalingWidth  // 16 scales per bank
  val totalScales = 2 * numScalesPerBank  // 32 scales total
  val counterWidth = log2Ceil(totalScales)  

  val io = IO(new ScalingFactorMemIO(
    writeAddrWidth, 
    bankWidth, 
    numScalesPerBank,  // 16 rows (activation scales)
    numScalesPerBank   // 16 columns (weight scales)
  ))
  
  // Create 4 banks: Banks 0,1 = Activation, Banks 2,3 = Weight
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, Vec(bytesPerBank, UInt(8.W))))
  
  io.write.ready := !io.read_req.bits.scaling_enable
  
  when(io.write.fire) {
    val bank_sel = io.write.bits.addr(0)      
    val row_addr = io.write.bits.addr >> 1.U  

    val write_bytes_low = Wire(Vec(bytesPerBank, UInt(8.W)))
    val write_bytes_high = Wire(Vec(bytesPerBank, UInt(8.W)))
    
    for(i <- 0 until bytesPerBank) {
      write_bytes_low(i) := io.write.bits.data((i+1)*8-1, i*8)
      write_bytes_high(i) := io.write.bits.data((i+1)*8-1 + bankWidth, i*8 + bankWidth)
    }
  
    when(bank_sel === 0.U) {
      banks(0).write(row_addr, write_bytes_low)
      banks(1).write(row_addr, write_bytes_high)
    }.elsewhen(bank_sel === 1.U) {
      banks(2).write(row_addr, write_bytes_low)
      banks(3).write(row_addr, write_bytes_high)
    }
  }
  
  // Registers for storing scales
  //val current_row = RegInit(0.U(rowAddrWidth.W))
  val act_scales = WireDefault(VecInit(Seq.fill(numScalesPerBank)(0.U(8.W))))
  val weight_scales = WireDefault(VecInit(Seq.fill(numScalesPerBank)(0.U(8.W))))

  val combined_scales_buffer = Wire(Vec(numScalesPerBank, Vec(numScalesPerBank, UInt(9.W))))
  val combined_scales_valid = WireDefault(false.B)

  io.read_req.ready := io.read_req.bits.scaling_enable
 
  // E8M0 multiplication function
  def multiplyScalesE8M0(act: UInt, weight: UInt): UInt = {
    val sum = act +& weight
    sum(8, 0)
  }
  
  val read_fire = io.read_req.fire && io.read_req.bits.scaling_enable
  val read_bank_sel = io.read_req.bits.addr(0)      
  val read_row_addr = io.read_req.bits.addr >> 1.U  
  
  val act_bank_data_vec = Wire(Vec(numScalesPerBank, UInt(8.W)))
  val weight_bank_data_vec = Wire(Vec(numScalesPerBank, UInt(8.W)))
  
  when(read_bank_sel === 0.U) {
    act_bank_data_vec := banks(0).read(read_row_addr, read_fire)
    weight_bank_data_vec := banks(2).read(read_row_addr, read_fire)
  }.otherwise {
    act_bank_data_vec := banks(1).read(read_row_addr, read_fire)
    weight_bank_data_vec := banks(3).read(read_row_addr, read_fire)
  }
 
  val read_addr_reg = RegNext(io.read_req.bits.addr)
  val read_fire_d1 = RegNext(read_fire, false.B)
  
  when(read_fire_d1) {
    for(i <- 0 until numScalesPerBank) {
      act_scales(i) := act_bank_data_vec(i)
      weight_scales(i) := weight_bank_data_vec(i)
    }
    printf(p"[ScalingFactorMem] Read scales from row=${read_addr_reg}\n")
  }
  
  for(i <- 0 until numScalesPerBank) {     
    for(j <- 0 until numScalesPerBank) {
      when(read_fire_d1) {
        combined_scales_buffer(i)(j) := multiplyScalesE8M0(act_scales(i), weight_scales(j))
        when (((i == 0).B && (j < 4).B)){
          printf(p"ScalingFactorMem] combined_scales_buffer=${combined_scales_buffer(0)(0)}, act_scales=${act_scales(0)} , weight_scales=${weight_scales(0)}\n")
        }
        combined_scales_valid := true.B  
      }.otherwise {
        combined_scales_buffer(i)(j) := 0.U
        combined_scales_valid := false.B  
      }
    }
  }
  
  io.read_resp.valid := combined_scales_valid
  io.read_resp.bits.combined_scales := combined_scales_buffer
  
  when(reset.asBool || !io.read_req.bits.scaling_enable) {
    //current_row := 0.U
    combined_scales_valid := false.B
  }
}
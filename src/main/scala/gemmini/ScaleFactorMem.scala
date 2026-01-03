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

class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W) 
  val scaling_enable = Bool() 
}

class ScalingFactorReadResp(numRows: Int, numCols: Int) extends Bundle {  
  val combined_scales = Vec(numRows, UInt(9.W))
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, numRows: Int, numCols: Int) extends Bundle {
  // writes happen to all interleaved banks for a line
  val write = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  val read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth)))
  val read_resp = Decoupled(new ScalingFactorReadResp(numRows, numCols))
  val dataType = Input(UInt(2.W))
}

class ScalingFactorMem(
  depth: Int = 256,                     
  bankWidth: Int = 128,                 
  actOutputScalingWidth: Int = 8,       
  numBanks: Int = 8,
  testConfig: Boolean = false
) extends Module {

  val rowAddrWidth = log2Ceil(depth) 
  val bytesPerBank = bankWidth / 8        
  val AddrWidth = rowAddrWidth  + log2Ceil(numBanks)     
  val bankaddressWidth = log2Ceil(numBanks) 
  val numScalesPerBank = bankWidth / actOutputScalingWidth  // 16 scales per bank
  val totalScales = 2 * numScalesPerBank  // 32 scales total
  val counterWidth = log2Ceil(totalScales)  
  val writeDataWidth = bankWidth * 2
  val io = IO(new ScalingFactorMemIO(
    AddrWidth, 
    writeDataWidth, 
    4*numScalesPerBank,  // 16 rows (activation scales)
    numScalesPerBank   // 16 columns (weight scales)
  ))

  val combined_scales_buffer = WireInit(VecInit(Seq.fill(4*numScalesPerBank)(
  VecInit(Seq.fill(4*numScalesPerBank)(0.U(9.W))))))
  val combined_scales_buffer_r = RegInit(VecInit(Seq.fill(4*numScalesPerBank)(
  VecInit(Seq.fill(4*numScalesPerBank)(0.U(9.W))))))
  val combined_scales_valid = WireDefault(false.B)
  // Create 8 banks: Banks 0,1,2,3 = Activation, Banks 4, 5, 6, 7= Weight
  val bankDataT = Vec(bytesPerBank, UInt(8.W))
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, bankDataT))

  val initByte = 0x7e.U(8.W)
  val defaultRow = VecInit(Seq.fill(bytesPerBank)(initByte)) 
  val fp8Mode = io.dataType === 2.U
  io.write.ready := !io.read_req.bits.scaling_enable
  
  when(io.write.fire) {
    val bank_sel = io.write.bits.addr >> rowAddrWidth.U      
    val row_addr = (io.write.bits.addr(rowAddrWidth + 1, rowAddrWidth)).asUInt
   
    val write_bytes_low = io.write.bits.data(bytesPerBank * 8 - 1, 0).asTypeOf(bankDataT)
    val write_bytes_high = io.write.bits.data(bytesPerBank * 2 * 8 - 1, bytesPerBank * 8).asTypeOf(bankDataT)
    //printf(p"[ScalingFactorMem] write_bytes_low=${write_bytes_low}\n")
    when(bank_sel === 0.U) {
      banks(0).write(row_addr, write_bytes_low)
      banks(1).write(row_addr, write_bytes_low)
    }.elsewhen(bank_sel === 1.U) {
      banks(2).write(row_addr, write_bytes_low)
      banks(3).write(row_addr, write_bytes_low)
    }.elsewhen(bank_sel === 2.U) {
      banks(4).write(row_addr, write_bytes_low)
      banks(5).write(row_addr, write_bytes_low)
    }.elsewhen(bank_sel === 3.U) {
      banks(6).write(row_addr, write_bytes_low)
      banks(7).write(row_addr, write_bytes_low)
    }
  }
  
  // Registers for storing scales
  //val current_row = RegInit(0.U(rowAddrWidth.W))
  val act_scales = WireDefault(VecInit(Seq.fill(numScalesPerBank * 4)(0.U(8.W))))
  val weight_scales = WireDefault(VecInit(Seq.fill(numScalesPerBank * 4)(0.U(8.W))))
  val scale_counter = RegInit(0.U(6.W))
  val weight_row_counter = RegInit(0.U(2.W))


  
  // E8M0 multiplication function
  def multiplyScalesE8M0(act: UInt, weight: UInt): UInt = {
    val sum = act +& weight
    sum(8, 0)
  }
  val read_fire = io.read_req.fire && io.read_req.bits.scaling_enable
  val read_fire_real = io.read_req.fire && io.read_req.bits.scaling_enable && (scale_counter === 0.U)

  val read_bank_sel = io.read_req.bits.addr >> rowAddrWidth.U      
  val read_row_addr = (io.read_req.bits.addr(rowAddrWidth + 1, rowAddrWidth)).asUInt

  val act_bank_data_vec = WireDefault(VecInit(Seq.fill(numScalesPerBank*4)(0.U(8.W))))
  val weight_bank_data_vec = WireDefault(VecInit(Seq.fill(numScalesPerBank*4)(0.U(8.W))))


  when(fp8Mode){
    when(read_bank_sel === 0.U) {
        val bank0_data = if (testConfig) { defaultRow } else { banks(0).read(read_row_addr, read_fire_real)}
        val bank4_data = if (testConfig) { defaultRow } else { banks(4).read(read_row_addr, read_fire_real)}
      for (i <- 0 until numScalesPerBank) {
        act_bank_data_vec(i) := bank0_data(i)
        weight_bank_data_vec(i) := bank4_data(i)
        //printf(p"[ScalingFactorMem] Read bank0_data=${bank0_data(i)}\n")
        //printf(p"[ScalingFactorMem] Read bank2_data=${bank2_data(i)}\n")
      }
    }.elsewhen(read_bank_sel === 1.U) {
      val bank1_data = if (testConfig) { defaultRow } else { banks(1).read(read_row_addr, read_fire_real)}
      val bank5_data = if (testConfig) { defaultRow } else { banks(5).read(read_row_addr, read_fire_real)}

      for (i <- 0 until numScalesPerBank) {
        act_bank_data_vec(i) := bank1_data(i)
        weight_bank_data_vec(i) := bank5_data(i)
      }
    }.elsewhen(read_bank_sel === 2.U) {
      val bank2_data = if (testConfig) { defaultRow } else { banks(2).read(read_row_addr, read_fire_real)}
      val bank6_data = if (testConfig) { defaultRow } else { banks(6).read(read_row_addr, read_fire_real)}

      for (i <- 0 until numScalesPerBank) {
        act_bank_data_vec(i) := bank2_data(i)
        weight_bank_data_vec(i) := bank6_data(i)
      }
    }.elsewhen(read_bank_sel === 3.U) {
      val bank3_data = if (testConfig) { defaultRow } else { banks(3).read(read_row_addr, read_fire_real)}
      val bank7_data = if (testConfig) { defaultRow } else { banks(7).read(read_row_addr, read_fire_real)}

      for (i <- 0 until numScalesPerBank) {
        act_bank_data_vec(i) := bank3_data(i)
        weight_bank_data_vec(i) := bank7_data(i)
      }
    }
  }.otherwise{
    val bank0_data = if (testConfig) { defaultRow } else { banks(0).read(read_row_addr, read_fire_real)}
    val bank1_data = if (testConfig) { defaultRow } else { banks(1).read(read_row_addr, read_fire_real)}
    val bank2_data = if (testConfig) { defaultRow } else { banks(2).read(read_row_addr, read_fire_real)}
    val bank3_data = if (testConfig) { defaultRow } else { banks(3).read(read_row_addr, read_fire_real)}
    val bank4_data = if (testConfig) { defaultRow } else { banks(4).read(read_row_addr, read_fire_real)}
    val bank5_data = if (testConfig) { defaultRow } else { banks(5).read(read_row_addr, read_fire_real)}
    val bank6_data = if (testConfig) { defaultRow } else { banks(6).read(read_row_addr, read_fire_real)}
    val bank7_data = if (testConfig) { defaultRow } else { banks(7).read(read_row_addr, read_fire_real)}

    for (j <- 0 until numBanks/2) {
      val bank_data_act = j match {
        case 0 => bank0_data
        case 1 => bank1_data
        case 2 => bank2_data
        case 3 => bank3_data
      }
      val bank_data_weight = j match {
        case 0 => bank4_data
        case 1 => bank5_data
        case 2 => bank6_data
        case 3 => bank7_data
      }
      for (i <- 0 until numScalesPerBank) {
      act_bank_data_vec(numScalesPerBank*j + i) := bank_data_act(i)
      weight_bank_data_vec(numScalesPerBank*j + i) := bank_data_weight(i)
      }
    }
  }
  // }.otherwise{
  //   val a_bank0_data = if (testConfig) {defaultRow} else { banks(0).read(read_row_addr, read_fire_real)}
  //   val a_bank1_data = if (testConfig) {defaultRow} else { banks(1).read(read_row_addr, read_fire_real)}
  //   val w_bank0_data = if (testConfig) {defaultRow} else { banks(2).read(read_row_addr, read_fire_real)}
  //   val w_bank1_data = if (testConfig) {defaultRow} else { banks(3).read(read_row_addr, read_fire_real)}

  //   a_bank0_data.zipWithIndex.foreach { case (data, i) => act_bank_data_vec(i) := data}
  //   a_bank1_data.zipWithIndex.foreach { case (data, i) => act_bank_data_vec(numScalesPerBank + i) := data}
  //   w_bank0_data.zipWithIndex.foreach { case (data, i) => weight_bank_data_vec(i) := data}
  //   w_bank1_data.zipWithIndex.foreach { case (data, i) => weight_bank_data_vec(numScalesPerBank + i) := data}
  // }

  val read_addr_reg = RegNext(io.read_req.bits.addr)
  val read_fire_d1 = RegNext(read_fire, false.B)
  val read_fire_real_d1 = RegNext(read_fire_real, false.B)
  io.read_resp.bits.combined_scales.foreach(_ := 0.U)
  io.read_resp.valid := false.B
  io.read_req.ready := io.read_req.bits.scaling_enable && (scale_counter === 0.U)

   for(i <- 0 until 4*numScalesPerBank) {     
    for(j <- 0 until 4*numScalesPerBank) {
      when(read_fire_d1) {
        combined_scales_buffer(i)(j) := multiplyScalesE8M0(act_scales(i), weight_scales(j))
        combined_scales_valid := true.B  
      }.otherwise {
        combined_scales_buffer(i)(j) := 0.U
        combined_scales_valid := false.B  
      }
    }
  }
  

  when(read_fire_real_d1) {
    combined_scales_buffer_r := combined_scales_buffer
  }

  when(read_fire_d1) {
    for(i <- 0 until 4*numScalesPerBank) {
      act_scales(i) := act_bank_data_vec(i)
      weight_scales(i) := weight_bank_data_vec(i)
      //printf(p"[ScalingFactorMem] Read act_scales=${act_scales(i) }\n")
      //printf(p"[ScalingFactorMem] Read weight_scales=${weight_scales(i) }\n")
    }
    //printf(p"[ScalingFactorMem] Read scales from row=${read_addr_reg}\n")
    when (((scale_counter === ((numScalesPerBank-1).U) && fp8Mode) || (scale_counter === ((numScalesPerBank-1).U) && !fp8Mode && (weight_row_counter === 3.U)))) {
      scale_counter := 0.U
      weight_row_counter := 0.U
    }.otherwise{
      io.read_resp.valid := combined_scales_valid
      scale_counter := scale_counter +& 1.U
      when((scale_counter(3,0) === ((numScalesPerBank-1).U)) && !fp8Mode){
        weight_row_counter := weight_row_counter +& 1.U
      }
      when(fp8Mode){
        for(j <- 0 until numScalesPerBank){
          when(scale_counter === 0.U){
          io.read_resp.bits.combined_scales(j) := combined_scales_buffer(0)(j)
          //printf(p"[ScalingFactorMem] Read scale from row=${scale_counter}, and get the scale=${io.read_resp.bits.combined_scales(j)}\n")
          }.otherwise{
            io.read_resp.bits.combined_scales(j) := combined_scales_buffer_r(scale_counter)(j)
          }
        }
      }.otherwise{
        for(j <- 0 until 4*numScalesPerBank){
          when(scale_counter === 0.U && weight_row_counter === 0.U){
          io.read_resp.bits.combined_scales(j) := combined_scales_buffer(0)(j)
          //printf(p"[ScalingFactorMem] Read scale from row=${scale_counter}, weight_row=${weight_row_counter}, and get the scale=${io.read_resp.bits.combined_scales(j)}\n")
          }.otherwise{
            io.read_resp.bits.combined_scales(j) := combined_scales_buffer_r(scale_counter + weight_row_counter * numScalesPerBank.U)(j)
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

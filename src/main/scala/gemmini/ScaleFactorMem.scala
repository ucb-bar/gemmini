package gemmini

import chisel3._
import chisel3.util._


// Interface Summary Table:
// ═══════════════════════════════════════════════════════════════════════════

// ┌──────────────────────────┬───────────┬────────────┬──────────────────────┐
// │ Signal Name              │ Direction │ Width      │ Description          │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ WRITE INTERFACE          │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ write.valid              │ Input     │ 1          │ Write request valid  │
// │ write.ready              │ Output    │ 1          │ Write ready          │
// │ write.bits.addr          │ Input     │ 16         │ [15:8]=row addr, [7:0]=byte mask │
// │ write.bits.data          │ Input     │ 64         │ Write data           │
// │ write.bits.bank_sel      │ Input     │ 2          │ Bank select (0-3)    │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ CONTROL                  │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ compute_enable           │ Input     │ 1          │ Enable read phase    │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ WEIGHT READ INTERFACE    │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ weight_read_req.valid    │ Input     │ 1          │ Weight read valid    │
// │ weight_read_req.ready    │ Output    │ 1          │ Weight read ready    │
// │ weight_read_req.bits.addr│ Input     │ 8          │ Weight read address  │
// │ weight_read_resp.valid   │ Output    │ 1          │ Weight data valid    │
// │ weight_read_resp.ready   │ Input     │ 1          │ Weight data ready    │
// │ weight_read_resp.bits... │ Output    │ 128        │ Weight data (2 banks)│
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ ACT READ INTERFACE       │           │            │                      │
// ├──────────────────────────┼───────────┼────────────┼──────────────────────┤
// │ act_read_req             │ Input     │ 1          │ Act read request     │
// │ act_addr                 │ Input     │ 8          │ Act read address     │
// │ act_read_resp.valid      │ Output    │ 1          │ Act data valid       │
// │ act_read_resp.bits       │ Output    │ 8          │ Act data (1 byte)    │
// └──────────────────────────┴───────────┴────────────┴──────────────────────┘

// write request - Modified to support masked write
class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)  // [high bits]=row address, [low 8 bits]=byte mask
  val data = UInt(dataWidth.W)  // 64 bits (8 bytes)
  val bank_sel = UInt(2.W) 
}

class ScalingFactorReadReq(addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val is_weight = Bool()
}

class ScalingFactorReadResp(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val valid = Bool()
}

class ScalingFactorMemIO(addrWidth: Int, dataWidth: Int, scalingWidth: Int) extends Bundle {
  val write = Flipped(Decoupled(new ScalingFactorWriteReq(addrWidth, dataWidth)))
  
  val scaling_enable = Input(Bool())
  
  val weight_read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth - 8)))
  val weight_read_resp = Decoupled(new ScalingFactorReadResp(dataWidth * 2)) // 2 banks combined
  
  val act_read_req = Input(Bool())
  val act_addr = Input(UInt((addrWidth - 8).W))
  val act_read_resp = Valid(UInt(scalingWidth.W))
}

// Parameterized Scaling Factor Memory with Masked Write Support
class ScalingFactorMem(
  depth: Int = 256,           // Depth of each bank
  bankWidth: Int = 64,        // Bit width of each bank (8 bytes)
  actOutputScalingWidth: Int = 8,    // Activation scaling factor output bit width
  numBanks: Int = 4          // Number of banks
) extends Module {

  val rowAddrWidth = log2Ceil(depth)  // 8 bits for row address
  val bytesPerBank = bankWidth / 8    // Number of bytes in one bank (8 bytes for 64-bit bank)
  val byteMaskWidth = bytesPerBank    // 8 bits for byte mask (8 bytes)
  val writeAddrWidth = rowAddrWidth + byteMaskWidth  // 16 bits total
  val combinedWidth = bankWidth * 2  
  val numActBytes = combinedWidth / actOutputScalingWidth  
  val counterWidth = log2Ceil(numActBytes)
  
  val io = IO(new ScalingFactorMemIO(writeAddrWidth, bankWidth, actOutputScalingWidth))
  
  // Create banks - use Vec of UInt(8.W) for byte-level access
  // Banks 0, 1: activation scaling factors
  // Banks 2, 3: weight scaling factors
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, Vec(bytesPerBank, UInt(8.W))))
  
  // ========== Write Phase with Byte Masking ==========
  io.write.ready := !io.scaling_enable
  
  when(io.write.fire) {
    // Extract row address (high 8 bits) and byte mask (low 8 bits)
    val row_addr = io.write.bits.addr >> byteMaskWidth.U
    val byte_mask = io.write.bits.addr(byteMaskWidth - 1, 0)
    
    // Convert 64-bit data to 8 bytes
    val write_bytes = Wire(Vec(bytesPerBank, UInt(8.W)))
    for(i <- 0 until bytesPerBank) {
      write_bytes(i) := io.write.bits.data((i+1)*8-1, i*8)
    }
    
    // Determine write mode: if all 8 bits of mask are set, it's a full row write
    val is_full_row_write = byte_mask === 0xFF.U
    
    // Write to selected bank with byte masking
    for(bank_idx <- 0 until numBanks) {
      when(io.write.bits.bank_sel === bank_idx.U) {
        when(is_full_row_write) {
          // Full row write: directly write all 8 bytes
          banks(bank_idx).write(row_addr, write_bytes)
        }.otherwise {
          // Masked write: Read-modify-write
          val current_data = banks(bank_idx).read(row_addr)
          val modified_data = Wire(Vec(bytesPerBank, UInt(8.W)))
          
          for(byte_idx <- 0 until bytesPerBank) {
            when(byte_mask(byte_idx)) {
              modified_data(byte_idx) := write_bytes(byte_idx)
            }.otherwise {
              modified_data(byte_idx) := current_data(byte_idx)
            }
          }
          
          banks(bank_idx).write(row_addr, modified_data)
        }
      }
    }
  }
  
  // ========== Weight Read (returns full row from 2 banks) ==========
  val weight_read_fire = io.weight_read_req.fire && io.scaling_enable
  
  io.weight_read_req.ready := io.scaling_enable
  
  val weight_rdata2_vec = banks(2).read(io.weight_read_req.bits.addr, weight_read_fire)
  val weight_rdata3_vec = banks(3).read(io.weight_read_req.bits.addr, weight_read_fire)
  
  // Convert Vec to UInt
  val weight_rdata2 = Cat(weight_rdata2_vec.reverse)
  val weight_rdata3 = Cat(weight_rdata3_vec.reverse)
  
  val weight_resp_valid_d1 = RegNext(weight_read_fire, false.B)
  //val weight_resp_valid_d2 = RegNext(weight_resp_valid_d1, false.B)
  val weight_resp_data_r = Cat(weight_rdata3, weight_rdata2)
  
  io.weight_read_resp.valid := weight_resp_valid_d1
  io.weight_read_resp.bits.data := weight_resp_data_r
  io.weight_read_resp.bits.valid := weight_resp_valid_d1
  
  // ========== Activation Scaling Factor Read ==========
  val act_counter = RegInit(0.U(counterWidth.W))
  val should_read_act_banks = io.act_read_req && io.scaling_enable && (act_counter === 0.U)
  
  val act_rdata0_vec = banks(0).read(io.act_addr, should_read_act_banks)
  val act_rdata1_vec = banks(1).read(io.act_addr, should_read_act_banks)
  
  // Convert Vec to UInt
  val act_rdata0 = Cat(act_rdata0_vec.reverse)
  val act_rdata1 = Cat(act_rdata1_vec.reverse)
  val act_combined_data_w =Cat(act_rdata1, act_rdata0)
  val act_combined_data_r = RegNext(Cat(act_rdata1, act_rdata0))
  val act_counter_d1 = RegNext(act_counter)
  val act_counter_d2 = RegNext(act_counter_d1)
  
  when(io.act_read_req && io.scaling_enable) {
    when(act_counter === (numActBytes - 1).U) {
      act_counter := 0.U
    }.otherwise {
      act_counter := act_counter + 1.U
    }
  }
  
  val act_output = Wire(UInt(actOutputScalingWidth.W))
  act_output := 0.U
  
  for(i <- 0 until numActBytes) {
    when(act_counter_d1 === i.U) {
      if (act_counter_d1 == 0.U) {
        act_output := act_combined_data_w((i+1)*actOutputScalingWidth-1, i*actOutputScalingWidth)
      } else {
        act_output := act_combined_data_r((i+1)*actOutputScalingWidth-1, i*actOutputScalingWidth)
      }
    }
  }
  
  val act_resp_valid_d1 = RegNext(io.act_read_req && io.scaling_enable, false.B)
  val act_resp_valid_d2 = RegNext(act_resp_valid_d1, false.B)
  io.act_read_resp.valid := act_resp_valid_d1
  io.act_read_resp.bits := act_output
  
  when(reset.asBool || !io.scaling_enable) {
    act_counter := 0.U
  }
}
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
// │ write.bits.addr          │ Input     │ 8          │ Write address        │
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

// write request
class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
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
  
  val weight_read_req = Flipped(Decoupled(new ScalingFactorReadReq(addrWidth)))
  val weight_read_resp = Decoupled(new ScalingFactorReadResp(dataWidth * 2)) // 2 banks combined
  
  val act_read_req = Input(Bool())
  val act_addr = Input(UInt(addrWidth.W))
  val act_read_resp = Valid(UInt(scalingWidth.W))
}

// Parameterized Scaling Factor Memory
class ScalingFactorMem(
  depth: Int = 256,           // Depth of each bank
  bankWidth: Int = 64,        // Bit width of each bank
  actOutputScalingWidth: Int = 8,    // Activation scaling factor output bit width
  numBanks: Int = 4          // Number of banks
) extends Module {

  val addrWidth = log2Ceil(depth)
  val combinedWidth = bankWidth * 2  
  val numActBytes = combinedWidth / actOutputScalingWidth  
  val counterWidth = log2Ceil(numActBytes)
  
  val io = IO(new ScalingFactorMemIO(addrWidth, bankWidth, actOutputScalingWidth))
  
  // Create banks
  // Banks 0, 1: activation scaling factors
  // Banks 2, 3: weight scaling factors
  val banks = Seq.fill(numBanks)(SyncReadMem(depth, UInt(bankWidth.W)))
  
  // ========== Write Phase ==========
  io.write.ready := !io.scaling_enable
  
  when(io.write.fire) {
    for(i <- 0 until numBanks) {
      when(io.write.bits.bank_sel === i.U) {
        banks(i).write(io.write.bits.addr, io.write.bits.data)
      }
    }
  }
  
  val weight_read_fire = io.weight_read_req.fire && io.scaling_enable
  
  io.weight_read_req.ready := io.scaling_enable
  
  val weight_rdata2 = banks(2).read(io.weight_read_req.bits.addr, weight_read_fire)
  val weight_rdata3 = banks(3).read(io.weight_read_req.bits.addr, weight_read_fire)
  val weight_resp_valid_d1 = RegNext(weight_read_fire, false.B)
  val weight_resp_valid_d2 = RegNext(weight_resp_valid_d1, false.B)
  val weight_resp_data_r = RegNext(Cat(weight_rdata3, weight_rdata2))
  
  io.weight_read_resp.valid := weight_resp_valid_d2
  io.weight_read_resp.bits.data := weight_resp_data_r
  io.weight_read_resp.bits.valid := weight_resp_valid_d2
  
  // ========== Activation Scaling Factor Read ==========
  // FIX #2: Properly manage counter to align with data pipeline
  
  val act_counter = RegInit(0.U(counterWidth.W))
  val should_read_act_banks = io.act_read_req && io.scaling_enable && (act_counter === 0.U)
  val act_rdata0 = banks(0).read(io.act_addr, should_read_act_banks)
  val act_rdata1 = banks(1).read(io.act_addr, should_read_act_banks)
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
    when(act_counter_d2 === i.U) {
      act_output := act_combined_data_r((i+1)*actOutputScalingWidth-1, i*actOutputScalingWidth)
    }
  }
  
  val act_resp_valid_d1 = RegNext(io.act_read_req && io.scaling_enable, false.B)
  val act_resp_valid_d2 = RegNext(act_resp_valid_d1, false.B)
  io.act_read_resp.valid := act_resp_valid_d2
  io.act_read_resp.bits := act_output
  
  when(reset.asBool || !io.scaling_enable) {
    act_counter := 0.U
  }
}
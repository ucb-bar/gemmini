package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

import Util._

class Im2ColReadReq[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {
  import config._

  val addr = new LocalAddr(sp_banks, sp_bank_entries, acc_banks, acc_bank_entries)
  val fire_counter = UInt(log2Up(meshColumns * tileColumns).W)
  val input_width = UInt(4.W)
  val weight_width = UInt(3.W)
  val channel = UInt(3.W)
}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {
  import config._

  val a_im2col = Output(Vec(meshColumns * tileColumns, inputType))
}

class Im2Col[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Module {
  import config._

  val io = new Bundle {
    val req = Flipped(Decoupled(new Im2ColReadReq(config))) // from ExecuteController
    val resp = Decoupled(new Im2ColReadResp(config)) // to ExecuteController

    /*
    val req = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())
      val bits = Input(new Im2ColReadReq(config))
    }

    val resp = new Bundle {
      val valid = Output(Bool())
      val ready = Input(Bool())
      val bits = Output(new Im2ColReadResp(config))
    }
    */

    val sram_reads = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)) // from Scratchpad
  }

  object State extends ChiselEnum {
    val idle, busy = Value
  }
  import State._

  val state = RegInit(idle)

  val req = Reg(new Im2ColReadReq(config))

  io.req.ready := state === idle

  when (io.req.fire()) {
    req := io.req.bits
    state := busy
  }

  when (state === busy) {
    io.sram_reads(req.addr.sp_bank()).req.valid := true.B
    io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()
  }

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}

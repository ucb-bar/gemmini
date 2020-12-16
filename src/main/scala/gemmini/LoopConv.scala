package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._

class LoopConvLdBiasReq extends Bundle {

  val no_bias = Bool()
}

class LoopConvLdIBias(concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvLdBiasReq))
    val cmd = Decoupled(Output(new RoCCCommand))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, st = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvLdBiasReq())


}

class LoopConvLdInputReq {

}

class LoopConvLdInput {

}

class LoopConvLdWeightReq {

}

class LoopConvLdWeight {

}

class LoopConvLdExecuteReq {

}

class LoopConvLdExecute {

}

class LoopConvLdStReq {

}

class LoopConvLdSt {

}

class LoopConv {

}

package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._

class LoopLoader(block_size: Int, coreMaxAddrBits:Int, max_addr: Int, input_w: Int, dma_max_bytes: Int)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16
  val max_block_len = (dma_max_bytes / (block_size * input_w / 8)) max 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val busy = Output(Bool())
  })
  //queue for cmd
  val cmd = Queue(io.in)
  val is_ldloop = cmd.bits.inst.funct === LOOP_LD
  val is_ldconfig = cmd.bits.inst.funct === LOOP_WS_CONFIG_ADDRS_AB || cmd.bits.inst.funct === LOOP_WS_CONFIG_STRIDES_AB
  //ToDo: either load A or B (for now just do with B)
  val load_cmd = Wire(new RoCCCommand())
  load_cmd := DontCare
  load_cmd.inst.funct := LOAD2_CMD
  io.out.valid := cmd.valid
  io.out.bits := Mux(is_ldloop, load_cmd, cmd.bits)

}

object LoopLoader{
  def apply(in: DecoupledIO[RoCCCommand], block_size: Int, coreMaxAddrBits: Int, max_addr: Int, input_w: Int, dma_max_bytes: Int)
           (implicit p: Parameters): Tuple2[DecoupledIO[RoCCCommand], Bool] = {
    val lld = Module(new LoopLoader(block_size, coreMaxAddrBits, max_addr, input_w, dma_max_bytes))
    lld.io.in <> in
    (lld.io.out, lld.io.busy)
  }
}
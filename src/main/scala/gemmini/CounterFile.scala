package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode}
import GemminiISA._
import Util._

// A simple counter file. Every counter is incremented when the corresponding event signal is high on rising edge.
class CounterFile(nPerfCounter: Int, counterWidth: Int) extends Module
{
  val addrWidth = log2Ceil(nPerfCounter)

  val io = IO(new Bundle()
  {
    val reset = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val data = Output(UInt(counterWidth.W))
    val eventSignal = Input(Vec(nPerfCounter, Bool()))
  })

  val counters = Vec(nPerfCounter, RegInit(0.U(counterWidth.W), io.reset))

  // Connect read port
  io.data := counters[addr]

  // Update signal
  (io.eventSignal zip counters) map (_ => (signal, counter) {
    when (signal) {
      counter := counter + 1
    }
  })
}
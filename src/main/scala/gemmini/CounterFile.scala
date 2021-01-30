package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode}
import GemminiISA._
import Util._

// Counter Address list
object CounterEvent {
  val DISABLE = 0.U

  val MAIN_LD_CYCLES = 1.U
  val MAIN_ST_CYCLES = 2.U
  val MAIN_EX_CYCLES = 3.U
  val MAIN_LD_ST_CYCLES = 4.U
  val MAIN_LD_EX_CYCLES = 5.U
  val MAIN_ST_EX_CYCLES = 6.U
  val MAIN_LD_ST_EX_CYCLES = 7.U

  val LOAD_DMA_WAIT_CYCLE = 8.U
  val LOAD_ACTIVE_CYCLE = 9.U
  val LOAD_SCRATCHPAD_WAIT_CYCLE = 10.U

  val STORE_DMA_WAIT_CYCLE = 11.U
  val STORE_ACTIVE_CYCLE = 12.U
  val STORE_SCRATCHPAD_WAIT_CYCLE = 13.U

  val DMA_TLB_MISS_CYCLE = 14.U
  val DMA_TLB_HIT_REQ = 15.U
  val DMA_TLB_TOTAL_REQ = 106.U

  val RDMA_ACTIVE_CYCLE = 17.U
  val RDMA_TLB_WAIT_CYCLES = 18.U
  val RDMA_TL_WAIT_CYCLES = 13.U

  val WDMA_ACTIVE_CYCLE = 13.U
  val WDMA_TLB_WAIT_CYCLES = 14.U
  val WDMA_TL_WAIT_CYCLES = 19.U

  val EXE_ACTIVE_CYCLE = 20.U
  val EXE_FLUSH_CYCLE = 21.U
  val EXE_CONTROL_Q_BLOCK_CYCLE = 22.U
  val EXE_PRELOAD_HAZ_CYCLE = 23.U
  val EXE_OVERLAP_HAZ_CYCLE = 24.U

  val SCRATCHPAD_A_WAIT_CYCLE = 25.U
  val SCRATCHPAD_B_WAIT_CYCLE = 26.U
  val SCRATCHPAD_D_WAIT_CYCLE = 27.U

  val ACC_A_WAIT_CYCLE = 28.U
  val ACC_B_WAIT_CYCLE = 29.U
  val ACC_D_WAIT_CYCLE = 30.U

  val A_GARBAGE_CYCLE = 31.U
  val B_GARBAGE_CYCLE = 32.U
  val C_GARBAGE_CYCLE = 33.U

  val IM2COL_MEM_CYCLES = 34.U
  val IM2COL_ACTIVE_CYCLES = 35.U
  val IM2COL_TRANSPOSER_WAIT_CYCLE = 36.U

  val ROB_FULL_CYCLES = 37.U
  val ROB_ACTIVE_CYCLES = 38.U

  val LOOP_MATMUL_ACTIVE_CYCLES = 39.U
  val TRANSPOSE_PRELOAD_UNROLLER_ACTIVE_CYCLES = 40.U

  val n = 41
}

object CounterExternal {
  val DISABLE = 0.U

  val ROB_LD_COUNT = 1.U
  val ROB_ST_COUNT = 2.U
  val ROB_EX_COUNT = 3.U

  val RDMA_BYTE_REC = 4.U
  val WDMA_BYTE_SENT = 5.U

  val n = 6
}

class CounterEventIO extends Bundle {
  val event_signal = Input(Vec(CounterEvent.n, Bool()))
  val external_values = Input(Vec(CounterExternal.n, p(XLen)))
  val external_reset = Output(Bool())

  // Connect Event Signal
  private var connected = Array.fill(CounterEvent.n)(false)
  def connectEventSignal(addr: UInt, sig: UInt) = {
    event_signal(addr) := sig
    connected(addr) = true
  }

  // Connect Event Signal
  private var connected_external = Array.fill(CounterEvent.n)(false)
  def connectExternalCounter(addr: UInt, ext_value: UInt) = {
    external_values(addr) := ext_value
    connected_external(addr) = true
  }

  // Collect IO from submodule
  def collect(io: CounterIO) = {
    io.external_reset := external_reset
    for (i <- 0 to CounterEvent.n)
      if (io.connected(i)) {
        if (connected(i))
          throw new IllegalStateException("Port " + i + " is already connected in another IO")
        else {
          connected(i) = true
          event_signal(i) := io.event_signal(i)
        }
      }
    for (i <- 0 to CounterExternal.n)
      if (io.connected_external(i)) {
        if (connected_external(i))
          throw new IllegalStateException("External counter " + i + " is already connected in another IO")
        else {
          connected_external(i) = true
          external_values(i) := io.external_values(i)
        }
      }
  }
}

class CounterIO(nPerfCounter: Int, counterWidth: Int) extends Bundle {
  val addrWidth = log2Ceil(nPerfCounter)

  val reset = Input(Bool())
  val snapshot = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(counterWidth.W))
  val event_io = new CounterEventIO
  val config_address = Valid(UInt(log2Ceil(CounterEvent.n)))
}

// A simple counter file. Every counter is incremented when the corresponding event signal is high on rising edge.
class CounterFile(nPerfCounter: Int, counterWidth: Int) extends Module
{
  val io = IO(new CounterIO(nPerfCounter, counterWidth))

  val counters = Vec(nPerfCounter, RegInit(0.U(counterWidth.W), io.reset))
  val counter_config = Vec(nPerfCounter, RegInit(0.U(CounterEvent.n)))
  val snapshot_enable = RegInit(false.B, io.reset)

  // Snapshot: In case the access instruction get blocked, it is possible to take a snapshot when reading counter
  // value by setting a bit in the instruction. All subsequent readings return the values from the snapshot until 
  // it is cleared by a instruction with "clear" bit marked. 
  // When the snapshot bit is set, the normal counter are still being incremented. 
  // TODO implement this

  // Connect read port
  io.data := counters(addr)

  // Write configuration reg
  when (io.config_address.valid) {
    counter_config(io.addr) := io.config_address.bits
  }

  // Update signal
  (counters zip counter_config) map (_ => (counter, config) {
    when (io.event_signal(config)) {
      counter := counter + 1
    }
  })
}
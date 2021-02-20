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
  val DISABLE = 0

  val MAIN_LD_CYCLES = 1
  val MAIN_ST_CYCLES = 2
  val MAIN_EX_CYCLES = 3
  val MAIN_LD_ST_CYCLES = 4
  val MAIN_LD_EX_CYCLES = 5
  val MAIN_ST_EX_CYCLES = 6
  val MAIN_LD_ST_EX_CYCLES = 7

  val LOAD_DMA_WAIT_CYCLE = 8
  val LOAD_ACTIVE_CYCLE = 9
  val LOAD_SCRATCHPAD_WAIT_CYCLE = 10

  val STORE_DMA_WAIT_CYCLE = 11
  val STORE_ACTIVE_CYCLE = 12
  val STORE_SCRATCHPAD_WAIT_CYCLE = 13

  val DMA_TLB_MISS_CYCLE = 14
  val DMA_TLB_HIT_REQ = 15
  val DMA_TLB_TOTAL_REQ = 106

  val RDMA_ACTIVE_CYCLE = 17
  val RDMA_TLB_WAIT_CYCLES = 18
  val RDMA_TL_WAIT_CYCLES = 13

  val WDMA_ACTIVE_CYCLE = 13
  val WDMA_TLB_WAIT_CYCLES = 14
  val WDMA_TL_WAIT_CYCLES = 19

  val EXE_ACTIVE_CYCLE = 20
  val EXE_FLUSH_CYCLE = 21
  val EXE_CONTROL_Q_BLOCK_CYCLE = 22
  val EXE_PRELOAD_HAZ_CYCLE = 23
  val EXE_OVERLAP_HAZ_CYCLE = 24

  val SCRATCHPAD_A_WAIT_CYCLE = 25
  val SCRATCHPAD_B_WAIT_CYCLE = 26
  val SCRATCHPAD_D_WAIT_CYCLE = 27

  val ACC_A_WAIT_CYCLE = 28
  val ACC_B_WAIT_CYCLE = 29
  val ACC_D_WAIT_CYCLE = 30

  val A_GARBAGE_CYCLES = 31
  val B_GARBAGE_CYCLES = 32
  val D_GARBAGE_CYCLES = 33

  val IM2COL_MEM_CYCLES = 34
  val IM2COL_ACTIVE_CYCLES = 35
  val IM2COL_TRANSPOSER_WAIT_CYCLE = 36

  val ROB_FULL_CYCLES = 37
  val ROB_ACTIVE_CYCLES = 38

  val LOOP_MATMUL_ACTIVE_CYCLES = 39
  val TRANSPOSE_PRELOAD_UNROLLER_ACTIVE_CYCLES = 40

  val n = 41
}

object CounterExternal {
  val DISABLE = 0

  val ROB_LD_COUNT = 1
  val ROB_ST_COUNT = 2
  val ROB_EX_COUNT = 3

  val RDMA_BYTES_REC = 4
  val WDMA_BYTES_SENT = 5

  val n = 6
}

class CounterEventIO extends Bundle {
  val event_signal = Input(Vec(CounterEvent.n, Bool()))
  val external_values = Input(Vec(CounterExternal.n, UInt()))
  val external_reset = Output(Bool())

  // Connect Event Signal
  private var connected = Array.fill(CounterEvent.n)(false)
  def connectEventSignal(addr: Int, sig: UInt) = {
    event_signal(addr) := sig
    connected(addr) = true
  }

  // Connect Event Signal
  private var connected_external = Array.fill(CounterEvent.n)(false)
  def connectExternalCounter(addr: Int, ext_value: UInt) = {
    external_values(addr) := ext_value
    connected_external(addr) = true
  }

  // Collect IO from submodule
  def collect(io: CounterEventIO) = {
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
  val counter_reset = Input(Bool())
  val snapshot = Input(Bool())
  val snapshot_reset = Input(Bool())
  val addr = Input(UInt(log2Ceil(nPerfCounter).W))
  val data = Output(UInt(counterWidth.W))
  val config_address = Flipped(Valid(UInt(log2Ceil(CounterEvent.n).W)))

  val event_io = new CounterEventIO
}

// A simple counter file. Every counter is incremented when the corresponding event signal is high on rising edge.
class CounterFile(nPerfCounter: Int, counterWidth: Int) extends Module
{
  val io = IO(new CounterIO(nPerfCounter, counterWidth))

  val counters = Vec(nPerfCounter, RegInit(0.U(counterWidth.W), io.counter_reset))
  val counter_snapshot = Vec(nPerfCounter, RegInit(0.U(counterWidth.W), io.counter_reset))
  val counter_config = Vec(nPerfCounter, RegInit(0.U(CounterEvent.n)))
  val snapshot_enable = RegInit(false.B, io.counter_reset)

  // Snapshot: In case a sequence of access instructions get interrupted (i.e. preempted by OS), it is possible
  // to take a snapshot when reading counter value by setting a bit in the instruction. All subsequent readings
  // return the values from the snapshot until it is cleared by a instruction with "clear" bit marked. 
  // When the snapshot bit is set, the normal counters are still being incremented. 
  when (io.snapshot_reset) {
    snapshot_enable := false.B
  } .elsewhen (io.snapshot) {
    snapshot_enable := true.B
    // Move counter values to snapshot register
    (counter_snapshot zip counters) map { case (s, c) => {
      s := c
    }}
  }

  // Connect read port
  io.data := counters(io.addr)

  // Write configuration reg
  when (io.config_address.valid) {
    counter_config(io.addr) := io.config_address.bits
  }

  // Update signal
  (counters zip counter_config) map { case (counter, config) => {
    when (io.event_io.event_signal(config)) {
      counter := counter + 1.U
    }}
  }
}

class CounterController(nPerfCounter: Int, counterWidth: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCResponse)
    val event_io = new CounterEventIO
  })

  val module = new CounterFile(nPerfCounter: Int, counterWidth: Int)
  module.io.event_io <> io.event_io
  
  val out_reg = Reg(io.out.bits.cloneType)
  val out_valid_reg = RegInit(false.B)

  // Decode access option
  // rs1[2:0] = Counter index
  // rs1[3] = Global counter value reset
  // rs1[4] = Snapshot reset
  // rs1[5] = Take snapshot
  // rs1[6] = Change config
  // rs1[12:7] = new counter address for counter with index specified in rs1[2:0]

  io.in.ready := !out_valid_reg
  module.io.addr := io.in.bits.rs1(2, 0)
  module.io.counter_reset := io.in.bits.rs1(3) & io.in.fire()
  module.io.snapshot_reset := io.in.bits.rs1(4) & io.in.fire()
  module.io.snapshot := io.in.bits.rs1(5) & io.in.fire()
  module.io.config_address.valid := io.in.bits.rs1(6) & io.in.fire()
  module.io.config_address.bits := io.in.bits.rs1(12, 7)

  when (io.out.fire()) {
    out_valid_reg := false.B
  } .elsewhen (io.in.fire()) {
    out_valid_reg := true.B
    out_reg.rd := io.in.bits.inst.rd
    out_reg.data := 0.U
    out_reg.data := module.io.data
  }

  io.out.valid := out_valid_reg
  io.out.bits := out_reg
}
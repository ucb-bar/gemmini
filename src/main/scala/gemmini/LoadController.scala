
package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters
import midas.targetutils.PerfCounter

// TODO we need to check for WAW errors here
// TODO deal with errors when reading scratchpad responses
class LoadController[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], coreMaxAddrBits: Int,
                                                      local_addr_t: LocalAddr)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(rob_entries)))

    val dma = new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits)

    val completed = Decoupled(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val strides = Reg(Vec(load_states, UInt(coreMaxAddrBits.W)))
  val scales = Reg(Vec(load_states, UInt(mvin_scale_t_bits.W)))
  val shrinks = Reg(Vec(load_states, Bool())) // Shrink inputs to accumulator
  val block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W))) // Spad stride during block move-ins
  val pixel_repeats = Reg(Vec(load_states, UInt(pixel_repeats_bits.W)))
  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))

  val cmd = Queue(io.cmd, ld_queue_length)

  val vaddr = cmd.bits.cmd.rs1
  val mvin_rs2 = cmd.bits.cmd.rs2.asTypeOf(new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t))
  val localaddr = mvin_rs2.local_addr
  val cols = mvin_rs2.num_cols
  val rows = mvin_rs2.num_rows

  val config_stride = cmd.bits.cmd.rs2

  val config_mvin_rs1 = cmd.bits.cmd.rs1.asTypeOf(new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits))

  val config_scale = config_mvin_rs1.scale
  val config_shrink = config_mvin_rs1.shrink
  val config_block_stride = config_mvin_rs1.stride
  val config_pixel_repeats = config_mvin_rs1.pixel_repeats

  val mstatus = cmd.bits.cmd.status

  val load_state_id = MuxCase(0.U, Seq((cmd.bits.cmd.inst.funct === LOAD2_CMD) -> 1.U,
    (cmd.bits.cmd.inst.funct === LOAD3_CMD) -> 2.U))
  val config_state_id = config_mvin_rs1.state_id
  val state_id = Mux(cmd.bits.cmd.inst.funct === CONFIG_CMD, config_state_id, load_state_id)

  val stride = strides(state_id)
  val scale = scales(state_id)
  val shrink = shrinks(state_id)
  val block_stride = block_strides(state_id)
  val pixel_repeat = pixel_repeats(state_id)

  val all_zeros = vaddr === 0.U

  val localaddr_plus_row_counter = localaddr + row_counter

  val actual_rows_read = Mux(stride === 0.U && !all_zeros, 1.U, rows)

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoLoad = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  // Command tracker instantiation
  val nCmds = (max_in_flight_mem_reqs / block_rows) + 1

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(rob_entries).W)
  }

  val maxBytesInRowRequest = config.dma_maxbytes max (block_cols * config.inputType.getWidth / 8) max
    (block_cols * config.accType.getWidth / 8)
  val maxBytesInMatRequest = block_rows * maxBytesInRowRequest

  val cmd_tracker = Module(new DMACommandTracker(nCmds, maxBytesInMatRequest, deps_t))

  io.busy := cmd.valid || cmd_tracker.io.busy

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoLoad && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && row_counter =/= 0.U)
  io.dma.req.bits.vaddr := vaddr + row_counter * stride
  io.dma.req.bits.laddr := localaddr_plus_row_counter
  io.dma.req.bits.cols := cols
  io.dma.req.bits.repeats := Mux(stride === 0.U && !all_zeros, rows - 1.U, 0.U)
  io.dma.req.bits.block_stride := block_stride
  io.dma.req.bits.scale := scale
  io.dma.req.bits.has_acc_bitwidth := localaddr_plus_row_counter.is_acc_addr && !shrink
  io.dma.req.bits.all_zeros := all_zeros
  io.dma.req.bits.status := mstatus
  io.dma.req.bits.pixel_repeats := pixel_repeat

  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoLoad
  cmd_tracker.io.alloc.bits.bytes_to_read :=
    Mux(io.dma.req.bits.has_acc_bitwidth, cols * actual_rows_read * config.accType.getWidth.U,
      cols * actual_rows_read * config.inputType.getWidth.U) / 8.U
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id.bits
  cmd_tracker.io.request_returned.valid := io.dma.resp.fire // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := io.dma.resp.bits.bytesRead
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  io.busy := cmd.valid || cmd_tracker.io.busy

  // Row counter
  when (io.dma.req.fire) {
    row_counter := wrappingAdd(row_counter, 1.U, actual_rows_read)

    assert(block_stride >= rows)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        when(DoConfig) {
          stride := config_stride
          scale := config_scale
          shrink := config_shrink
          block_stride := config_block_stride
          pixel_repeat := Mux(config_pixel_repeats === 0.U, 1.U, config_pixel_repeats) // TODO this default value was just added to maintain backwards compatibility. we should deprecate and remove it later
          cmd.ready := true.B
        }

        .elsewhen(DoLoad && cmd_tracker.io.alloc.fire()) {
          control_state := Mux(io.dma.req.fire, sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === actual_rows_read-1.U && io.dma.req.fire)

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }

  // Optimizations based on config parameters
  if (!has_first_layer_optimizations)
    pixel_repeats.foreach(_ := 1.U)

  // Performance counter
  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.LOAD_ACTIVE_CYCLE, control_state === sending_rows)
  io.counter.connectEventSignal(CounterEvent.LOAD_DMA_WAIT_CYCLE, control_state === waiting_for_dma_req_ready)
  io.counter.connectEventSignal(CounterEvent.LOAD_SCRATCHPAD_WAIT_CYCLE, io.dma.req.valid && !io.dma.req.ready)

  if (use_firesim_simulation_counters) {
    PerfCounter(io.dma.req.valid && !io.dma.req.ready, "load_dma_wait_cycle", "cycles during which load controller is waiting for DMA to be available")
  }

  // Assertions
  assert(!(cmd_tracker.io.alloc.fire() && cmd_tracker.io.alloc.bits.bytes_to_read === 0.U), "A single mvin instruction must load more than 0 bytes")
  assert(has_first_layer_optimizations.B || !(cmd.valid && DoConfig && config_pixel_repeats > 1.U), "If first-layer optimizations are not enabled, then pixel-repeats cannot be greater than 1")
}


package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters
import midas.targetutils.PerfCounter

// TODO this is almost a complete copy of LoadController. We should combine them into one class
// TODO deal with errors when reading scratchpad responses
class StoreController[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V],
                                                                    coreMaxAddrBits: Int, local_addr_t: LocalAddr)(implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(reservation_station_entries)))

    val dma = new ScratchpadWriteMemIO(local_addr_t, accType.getWidth, acc_scale_t_bits)

    val completed = Decoupled(UInt(log2Up(reservation_station_entries).W))

    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  // val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)

  object State extends ChiselEnum {
    val waiting_for_command, waiting_for_dma_req_ready, sending_rows, pooling = Value
  }
  import State._

  val control_state = RegInit(waiting_for_command)

  val stride = Reg(UInt(coreMaxAddrBits.W))
  val block_rows = meshRows * tileRows
  val block_stride = block_rows.U
  val block_cols = meshColumns * tileColumns
  val max_blocks = (dma_maxbytes / (block_cols * inputType.getWidth / 8)) max 1

  val activation = Reg(UInt(Activation.bitwidth.W)) // TODO magic number
  val igelu_qb = Reg(accType)
  val igelu_qc = Reg(accType)
  val iexp_qln2 = Reg(accType)
  val iexp_qln2_inv = Reg(accType)
  val norm_stats_id = Reg(UInt(8.W)) // TODO magic number
  val acc_scale = Reg(acc_scale_t)

  //val row_counter = RegInit(0.U(log2Ceil(block_rows).W))
  val row_counter = RegInit(0.U(12.W)) // TODO magic number
  val block_counter = RegInit(0.U(8.W)) // TODO magic number

  // Pooling variables
  val pool_stride = Reg(UInt(CONFIG_MVOUT_RS1_MAX_POOLING_STRIDE_WIDTH.W)) // When this is 0, pooling is disabled
  val pool_size = Reg(UInt(CONFIG_MVOUT_RS1_MAX_POOLING_WINDOW_SIZE_WIDTH.W))
  val pool_out_dim = Reg(UInt(CONFIG_MVOUT_RS1_POOL_OUT_DIM_WIDTH.W))
  val pool_porows = Reg(UInt(CONFIG_MVOUT_RS1_POOL_OUT_ROWS_WIDTH.W))
  val pool_pocols = Reg(UInt(CONFIG_MVOUT_RS1_POOL_OUT_COLS_WIDTH.W))
  val pool_orows = Reg(UInt(CONFIG_MVOUT_RS1_OUT_ROWS_WIDTH.W))
  val pool_ocols = Reg(UInt(CONFIG_MVOUT_RS1_OUT_COLS_WIDTH.W))
  val pool_upad = Reg(UInt(CONFIG_MVOUT_RS1_UPPER_ZERO_PADDING_WIDTH.W))
  val pool_lpad = Reg(UInt(CONFIG_MVOUT_RS1_LEFT_ZERO_PADDING_WIDTH.W))

  val porow_counter = RegInit(0.U(pool_porows.getWidth.W))
  val pocol_counter = RegInit(0.U(pool_pocols.getWidth.W))
  val wrow_counter = RegInit(0.U(pool_size.getWidth.W))
  val wcol_counter = RegInit(0.U(pool_size.getWidth.W))

  val pooling_is_enabled = has_max_pool.B && pool_stride =/= 0.U
  val mvout_1d_enabled = pool_size =/= 0.U && !pooling_is_enabled //1-D move out enabled (no pooling)

  val orow = porow_counter * pool_stride +& wrow_counter - pool_upad // TODO get rid of this multiplication
  val orow_is_negative = porow_counter * pool_stride +& wrow_counter < pool_upad // TODO get rid of this multiplication

  val ocol = pocol_counter * pool_stride +& wcol_counter - pool_lpad // TODO get rid of this multiplication
  val ocol_is_negative = pocol_counter * pool_stride +& wcol_counter < pool_lpad // TODO get rid of this multiplication

  val pool_total_rows = pool_porows * pool_pocols * pool_size * pool_size // TODO get this value from software

  // Commands
  val cmd = Queue(io.cmd, st_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val mvout_rs2 = cmd.bits.cmd.rs2.asTypeOf(new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t))
  val localaddr = mvout_rs2.local_addr
  val cols = mvout_rs2.num_cols
  val rows = mvout_rs2.num_rows
  val blocks = (cols / block_cols.U(cols.getWidth.W)) + (cols % block_cols.U =/= 0.U)

  val config_mvout_rs1 = cmd.bits.cmd.rs1.asTypeOf(new ConfigMvoutRs1)
  val config_mvout_rs2 = cmd.bits.cmd.rs2.asTypeOf(new ConfigMvoutRs2(acc_scale_t_bits, 32))
  val config_cmd_type = config_mvout_rs1.cmd_type
  val config_stride = config_mvout_rs2.stride
  val config_activation = config_mvout_rs1.activation
  val config_acc_scale = config_mvout_rs2.acc_scale
  val config_pool_stride = config_mvout_rs1.pool_stride
  val config_pool_size = config_mvout_rs1.pool_size
  val config_pool_out_dim = config_mvout_rs1.pool_out_dim
  val config_porows = config_mvout_rs1.porows
  val config_pocols = config_mvout_rs1.pocols
  val config_orows = config_mvout_rs1.orows
  val config_ocols = config_mvout_rs1.ocols
  val config_upad = config_mvout_rs1.upad
  val config_lpad = config_mvout_rs1.lpad

  val config_norm_rs1 = cmd.bits.cmd.rs1.asTypeOf(new ConfigNormRs1(accType.getWidth))
  val config_norm_rs2 = cmd.bits.cmd.rs2.asTypeOf(new ConfigNormRs2(accType.getWidth))
  val config_stats_id = config_norm_rs1.norm_stats_id
  val config_activation_msb = config_norm_rs1.act_msb
  val config_set_stats_id_only = config_norm_rs1.set_stats_id_only
  val config_iexp_q_const_type = config_norm_rs1.q_const_type
  val config_iexp_q_const = config_norm_rs1.q_const
  val config_igelu_qb = config_norm_rs2.qb
  val config_igelu_qc = config_norm_rs2.qc

  assert(config_norm_rs1.cmd_type === config_mvout_rs1.cmd_type)

  val mstatus = cmd.bits.cmd.status

  val current_vaddr = vaddr + row_counter * stride
  val current_localaddr = WireInit(localaddr + (block_counter * block_stride + row_counter))

  val pool_row_addr = localaddr + (orow * pool_ocols +& ocol)
  when (orow_is_negative || ocol_is_negative || orow >= pool_orows || ocol >= pool_ocols) {
    pool_row_addr.make_this_garbage()
  }

  val pool_vaddr = vaddr + (porow_counter * pool_out_dim + pocol_counter) * stride // TODO get rid of these multiplications

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE
  val DoConfigNorm = config.has_normalizations.B && cmd.bits.cmd.inst.funct === CONFIG_CMD && config_cmd_type === CONFIG_NORM
  val DoStore = !DoConfig && !DoConfigNorm

  cmd.ready := false.B

  val mvout_1d_rows = pool_orows * pool_ocols //for 1D mvout
  // Command tracker instantiation
  val nCmds = (max_in_flight_mem_reqs / block_rows) + 1

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(reservation_station_entries).W)
  }

  val cmd_tracker_max_rows = ((block_rows * max_blocks) max
    (((1 << pool_orows.getWidth)-1) * ((1 << pool_ocols.getWidth)-1) + 2*((1 << pool_lpad.getWidth)-1) + 2*((1 << pool_upad.getWidth)-1))) min
    ((config.sp_banks * config.sp_bank_entries) max
    (config.acc_banks * config.acc_bank_entries))

  val cmd_tracker = Module(new DMACommandTracker(nCmds, cmd_tracker_max_rows, deps_t))

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoStore && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && (block_counter =/= 0.U || row_counter =/= 0.U)) ||
    (control_state === pooling && (wcol_counter =/= 0.U || wrow_counter =/= 0.U || pocol_counter =/= 0.U || porow_counter =/= 0.U))

  io.dma.req.bits.vaddr := Mux(pooling_is_enabled || mvout_1d_enabled, pool_vaddr, current_vaddr)
  io.dma.req.bits.laddr := Mux(pooling_is_enabled, pool_row_addr, current_localaddr) //Todo: laddr for 1D?
  io.dma.req.bits.laddr.norm_cmd := Mux(block_counter === blocks - 1.U, current_localaddr.norm_cmd,
        NormCmd.non_reset_version(current_localaddr.norm_cmd))

  io.dma.req.bits.acc_act := activation
  io.dma.req.bits.acc_igelu_qb := igelu_qb.asTypeOf(io.dma.req.bits.acc_igelu_qb)
  io.dma.req.bits.acc_igelu_qc := igelu_qc.asTypeOf(io.dma.req.bits.acc_igelu_qc)
  io.dma.req.bits.acc_iexp_qln2 := iexp_qln2.asTypeOf(io.dma.req.bits.acc_iexp_qln2)
  io.dma.req.bits.acc_iexp_qln2_inv := iexp_qln2_inv.asTypeOf(io.dma.req.bits.acc_iexp_qln2_inv)
  io.dma.req.bits.acc_norm_stats_id := norm_stats_id
  io.dma.req.bits.acc_scale := acc_scale.asTypeOf(io.dma.req.bits.acc_scale)

  io.dma.req.bits.len := Mux(block_counter === blocks - 1.U, ((cols - 1.U) % block_cols.U) + 1.U, block_cols.U)
  io.dma.req.bits.block := block_counter
  io.dma.req.bits.status := mstatus
  io.dma.req.bits.pool_en := pooling_is_enabled && (wrow_counter =/= 0.U || wcol_counter =/= 0.U)
  io.dma.req.bits.store_en := Mux(pooling_is_enabled, wrow_counter === pool_size - 1.U && wcol_counter === pool_size - 1.U,
    block_counter === blocks - 1.U)

  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoStore
  cmd_tracker.io.alloc.bits.bytes_to_read := Mux(!pooling_is_enabled, Mux(mvout_1d_enabled, mvout_1d_rows, rows*blocks), pool_total_rows) // TODO do we have to add upad and lpad to this?
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id.bits

  cmd_tracker.io.request_returned.valid := io.dma.resp.fire // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := 1.U
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  io.busy := cmd.valid || cmd_tracker.io.busy

  // Row counter
  when (io.dma.req.fire) {
    when (!pooling_is_enabled) {
      //where does rows come from?
      //row_counter := wrappingAdd(row_counter, 1.U, rows)
      when(mvout_1d_enabled){
        pocol_counter := wrappingAdd(pocol_counter, 1.U, pool_ocols)
        porow_counter := wrappingAdd(porow_counter, 1.U, pool_orows, pocol_counter === pool_ocols - 1.U)
      }

      block_counter := wrappingAdd(block_counter, 1.U, blocks)
      row_counter := Mux(mvout_1d_enabled, wrappingAdd(row_counter, 1.U, mvout_1d_rows), wrappingAdd(row_counter, 1.U, rows, block_counter === blocks - 1.U))
    }.otherwise {
      wcol_counter := wrappingAdd(wcol_counter, 1.U, pool_size)
      wrow_counter := wrappingAdd(wrow_counter, 1.U, pool_size, wcol_counter === pool_size - 1.U)
      pocol_counter := wrappingAdd(pocol_counter, 1.U, pool_pocols, wrow_counter === pool_size - 1.U && wcol_counter === pool_size - 1.U)
      porow_counter := wrappingAdd(porow_counter, 1.U, pool_porows, pocol_counter === pool_pocols - 1.U && wrow_counter === pool_size - 1.U && wcol_counter === pool_size - 1.U)
    }

    assert(!(io.dma.req.bits.laddr.read_full_acc_row && blocks > 1.U), "Block-mvouts are not permitted when moving out full accumulator data")
    assert(!((pooling_is_enabled || mvout_1d_enabled) && blocks > 1.U), "Block-mvouts are not permitted when pooling")
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        when(DoConfig) {
          stride := config_stride

          activation := config_activation
          when (!config_acc_scale.asUInt().andR()) {
            acc_scale := config_acc_scale.asTypeOf(acc_scale_t)
          }

          pool_size := config_pool_size
          pool_stride := config_pool_stride
          when (config_pool_stride =/= 0.U) {
            pool_out_dim := config_pool_out_dim
            pool_porows := config_porows
            pool_pocols := config_pocols
            pool_orows := config_orows
            pool_ocols := config_ocols
            pool_upad := config_upad
            pool_lpad := config_lpad
          }.elsewhen(config_pool_size =/= 0.U){
            pool_orows := config_orows
            pool_ocols := config_ocols
            pool_out_dim := config_pool_out_dim
          }
          cmd.ready := true.B
        }
        .elsewhen(config.has_normalizations.B && DoConfigNorm) {
          when (!config_set_stats_id_only.asBool()) {
            igelu_qb := config_igelu_qb.asTypeOf(igelu_qb)
            igelu_qc := config_igelu_qc.asTypeOf(igelu_qc)
            when(config_iexp_q_const_type === 0.U) {
              iexp_qln2 := config_iexp_q_const.asTypeOf(iexp_qln2)
            }.elsewhen(config_iexp_q_const_type === 1.U) {
              iexp_qln2_inv := config_iexp_q_const.asTypeOf(iexp_qln2_inv)
            }
            activation := Cat(config_activation_msb, activation(1, 0)) // TODO: magic number
          }
          norm_stats_id := config_stats_id
          cmd.ready := true.B
        }
        .elsewhen(DoStore && cmd_tracker.io.alloc.fire()) {
          val next_state = Mux(pooling_is_enabled, pooling, sending_rows)
          control_state := Mux(io.dma.req.fire, next_state, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire) {
        control_state := Mux(pooling_is_enabled, pooling, sending_rows)
      }
    }

    is (sending_rows) {
      val last_block = block_counter === blocks - 1.U && io.dma.req.fire
      val last_row = Mux(mvout_1d_enabled, row_counter === mvout_1d_rows - 1.U, row_counter === rows - 1.U) && io.dma.req.fire
      //normal mvout: row, 1D mvout: orows*ocols

      val only_one_dma_req = block_counter === 0.U && row_counter === 0.U // This is a special case when only one DMA request is made

      when ((last_block && last_row) || only_one_dma_req) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }

    is (pooling) {
      // TODO Is it really possible for all the counters to be 0 here?
      val last_row = (porow_counter === 0.U && pocol_counter === 0.U && wrow_counter === 0.U && wcol_counter === 0.U) ||
        (porow_counter === pool_porows - 1.U && pocol_counter === pool_pocols - 1.U &&
          wrow_counter === pool_size - 1.U && wcol_counter === pool_size - 1.U && io.dma.req.fire)

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }

  // Optimizations when features are disabled
  if (!config.has_normalizations) {
    current_localaddr.norm_cmd := NormCmd.RESET

    igelu_qb := DontCare
    igelu_qc := DontCare
    iexp_qln2 := DontCare
    iexp_qln2_inv := DontCare
    norm_stats_id := 0.U
  }

  // Performance counter
  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.STORE_ACTIVE_CYCLE, control_state === sending_rows || control_state === pooling)
  io.counter.connectEventSignal(CounterEvent.STORE_POOLING_CYCLE, pooling_is_enabled)
  io.counter.connectEventSignal(CounterEvent.STORE_DMA_WAIT_CYCLE, control_state === waiting_for_dma_req_ready)
  io.counter.connectEventSignal(CounterEvent.STORE_SCRATCHPAD_WAIT_CYCLE, io.dma.req.valid && !io.dma.req.ready)

  if (use_firesim_simulation_counters) {
    PerfCounter(pooling_is_enabled, "pooling_cycles", "cycles during which store controller is max-pooling")
    PerfCounter(io.dma.req.valid && !io.dma.req.ready, "st_dma_wait_cycle", "cycles during which store controller is stalling for the DMA to be ready")
  }
}

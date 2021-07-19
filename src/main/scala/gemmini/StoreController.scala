
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO this is almost a complete copy of LoadController. We should combine them into one class
// TODO deal with errors when reading scratchpad responses
class StoreController[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], coreMaxAddrBits: Int, local_addr_t: LocalAddr)
                     (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(rob_entries)))

    val dma = new ScratchpadWriteMemIO(local_addr_t)

    val completed = Decoupled(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
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

  //val row_counter = RegInit(0.U(log2Ceil(block_rows).W))
  val row_counter = RegInit(0.U(12.W)) // TODO magic number
  val block_counter = RegInit(0.U(8.W)) // TODO magic number

  // Pooling variables
  val pool_stride = Reg(UInt(2.W)) // When this is 0, pooling is disabled // TODO magic number
  val pool_size = Reg(UInt(2.W)) // TODO magic number
  val pool_out_dim = Reg(UInt(8.W)) // TODO magic number
  val pool_porows = Reg(UInt(8.W)) // TODO magic number
  val pool_pocols = Reg(UInt(8.W)) // TODO magic number
  val pool_orows = Reg(UInt(8.W)) // TODO magic number
  val pool_ocols = Reg(UInt(8.W)) // TODO magic number
  val pool_upad = Reg(UInt(2.W)) // TODO magic number
  val pool_lpad = Reg(UInt(2.W)) // TODO magic number

  val porow_counter = RegInit(0.U(pool_porows.getWidth.W))
  val pocol_counter = RegInit(0.U(pool_pocols.getWidth.W))
  val wrow_counter = RegInit(0.U(pool_size.getWidth.W))
  val wcol_counter = RegInit(0.U(pool_size.getWidth.W))

  val pooling_is_enabled = pool_stride =/= 0.U
  val mvout_1d_enabled = pool_size =/= 0.U && !pooling_is_enabled //1-D move out enabled (no pooling)

  val orow = porow_counter * pool_stride +& wrow_counter - pool_upad // TODO get rid of this multiplication
  val orow_is_negative = porow_counter * pool_stride +& wrow_counter < pool_upad // TODO get rid of this multiplication

  val ocol = pocol_counter * pool_stride +& wcol_counter - pool_lpad // TODO get rid of this multiplication
  val ocol_is_negative = pocol_counter * pool_stride +& wcol_counter < pool_lpad // TODO get rid of this multiplication

  val pool_total_rows = pool_porows * pool_pocols * pool_size * pool_size // TODO get this value from software

  // Commands
  val cmd = Queue(io.cmd, st_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val localaddr = cmd.bits.cmd.rs2.asTypeOf(local_addr_t)
  val cols = cmd.bits.cmd.rs2(32 + mvout_cols_bits - 1, 32) // TODO magic numbers
  val rows = cmd.bits.cmd.rs2(48 + mvout_rows_bits - 1, 48) // TODO magic numbers
  val blocks = (cols / block_cols.U) + (cols % block_cols.U =/= 0.U)
  val config_stride = cmd.bits.cmd.rs2
  val config_pool_stride = cmd.bits.cmd.rs1(5, 4) // TODO magic numbers
  val config_pool_size = cmd.bits.cmd.rs1(7, 6) // TODO magic numbers
  val config_pool_out_dim = cmd.bits.cmd.rs1(31, 24) // TODO magic numbers
  val config_porows = cmd.bits.cmd.rs1(39, 32) // TODO magic numbers
  val config_pocols = cmd.bits.cmd.rs1(47, 40) // TODO magic numbers
  val config_orows = cmd.bits.cmd.rs1(55, 48) // TODO magic numbers
  val config_ocols = cmd.bits.cmd.rs1(63, 56) // TODO magic numbers
  val config_upad = cmd.bits.cmd.rs1(9, 8) // TODO magic numbers
  val config_lpad = cmd.bits.cmd.rs1(11, 10) // TODO magic numbers

  val mstatus = cmd.bits.cmd.status

  val current_vaddr = vaddr + row_counter * stride
  val current_localaddr = localaddr + (block_counter * block_stride + row_counter)

  val pool_row_addr = localaddr + (orow * pool_ocols +& ocol)
  when (orow_is_negative || ocol_is_negative || orow >= pool_orows || ocol >= pool_ocols) {
    pool_row_addr.make_this_garbage()
  }

  val pool_vaddr = vaddr + (porow_counter * pool_out_dim + pocol_counter) * stride // TODO get rid of these multiplications

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoStore = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  val mvout_1d_rows = pool_orows * pool_ocols //for 1D mvout
  // Command tracker instantiation
  val nCmds = (max_in_flight_reqs / block_rows) + 1

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(rob_entries).W)
  }

  val cmd_tracker_max_rows = ((block_rows * max_blocks) max
    (((1 << pool_orows.getWidth)-1) * ((1 << pool_ocols.getWidth)-1) + 2*((1 << pool_lpad.getWidth)-1) + 2*((1 << pool_upad.getWidth)-1))) min
    ((config.sp_banks * config.sp_bank_entries) max
    (config.acc_banks * config.acc_bank_entries))

  val cmd_tracker = Module(new DMACommandTracker(nCmds, cmd_tracker_max_rows, deps_t, prng_seed = prng_seed,
    proportion_of_slow_accesses_out_of_128 = if (delay_sts) proportion_of_slow_accesses_out_of_128 else 0,
    stall_delay = stall_delay))

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoStore && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && (block_counter =/= 0.U || row_counter =/= 0.U)) ||
    (control_state === pooling && (wcol_counter =/= 0.U || wrow_counter =/= 0.U || pocol_counter =/= 0.U || porow_counter =/= 0.U))

  io.dma.req.bits.vaddr := Mux(pooling_is_enabled || mvout_1d_enabled, pool_vaddr, current_vaddr)
  io.dma.req.bits.laddr := Mux(pooling_is_enabled, pool_row_addr, current_localaddr) //Todo: laddr for 1D?

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

  cmd_tracker.io.request_returned.valid := io.dma.resp.fire() // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := 1.U
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  io.busy := cmd.valid || cmd_tracker.io.busy

  // Row counter
  when (io.dma.req.fire()) {
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
          .elsewhen(DoStore && cmd_tracker.io.alloc.fire()) {
            val next_state = Mux(pooling_is_enabled, pooling, sending_rows)
            control_state := Mux(io.dma.req.fire(), next_state, waiting_for_dma_req_ready)
          }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire()) {
        control_state := Mux(pooling_is_enabled, pooling, sending_rows)
      }
    }

    is (sending_rows) {
      val last_block = block_counter === blocks - 1.U && io.dma.req.fire()
      val last_row = Mux(mvout_1d_enabled, row_counter === mvout_1d_rows - 1.U, row_counter === rows - 1.U) && io.dma.req.fire()
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
          wrow_counter === pool_size - 1.U && wcol_counter === pool_size - 1.U && io.dma.req.fire())

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }

  val pool_cycles_counter = RegInit(0.U(32.W))
  when (pooling_is_enabled) {
    pool_cycles_counter := pool_cycles_counter + 1.U
  }.otherwise {
    pool_cycles_counter := 0.U
  }
  // assert(pool_cycles_counter <= 1000.U)
  dontTouch(pool_cycles_counter)
}

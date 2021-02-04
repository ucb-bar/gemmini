package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO deal with errors when reading scratchpad responses
class LoadController[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], coreMaxAddrBits: Int, local_addr_t: LocalAddr, num_dma: Int)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(rob_entries)))

    //val dma = new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits)
    val dma = Vec(num_dma, Output(new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits)))
    //for bundle, use Vec instead of Seq.fill

    val completed = Decoupled(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
    //to differentiate between load controller
    val ld_cont_id = UInt(2.W)
  })

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val strides = Reg(Vec(3, UInt(coreMaxAddrBits.W)))
  val scales = Reg(Vec(3, UInt(mvin_scale_t_bits.W)))
  val shrinks = Reg(Vec(3, Bool())) // Shrink inputs to accumulator
  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))

  val cmd = Queue(io.cmd, ld_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val localaddr = cmd.bits.cmd.rs2.asTypeOf(local_addr_t)
  val cols = cmd.bits.cmd.rs2(32 + mvin_len_bits - 1, 32) // TODO magic numbers
  val rows = cmd.bits.cmd.rs2(48 + mvin_rows_bits - 1, 48) // TODO magic numbers
  val config_stride = cmd.bits.cmd.rs2
  val config_scale = cmd.bits.cmd.rs1(32 + mvin_scale_t_bits - 1, 32) // TODO magic numbers
  val config_shrink = cmd.bits.cmd.rs1(2)

  val mstatus = cmd.bits.cmd.status

  val load_state_id = MuxCase(0.U, Seq((cmd.bits.cmd.inst.funct === LOAD2_CMD) -> 1.U,
    (cmd.bits.cmd.inst.funct === LOAD3_CMD) -> 2.U))
  val config_state_id = cmd.bits.cmd.rs1(4,3)
  val state_id = Mux(cmd.bits.cmd.inst.funct === CONFIG_CMD, config_state_id, load_state_id)

  val stride = strides(state_id)
  val scale = scales(state_id)
  val shrink = shrinks(state_id)

  val localaddr_plus_row_counter = localaddr + row_counter

  val actual_rows_read = Mux(stride === 0.U, 1.U, rows)

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoLoad = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  // Command tracker instantiation
  val nCmds = (max_in_flight_reqs / block_rows) + 1

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(rob_entries).W)
  }

  val maxBytesInRowRequest = config.dma_maxbytes max (block_cols * config.inputType.getWidth / 8) max
    (block_cols * config.accType.getWidth / 8)
  val maxBytesInMatRequest = block_rows * maxBytesInRowRequest

  val cmd_tracker = Module(new DMAReadCommandTracker(nCmds, maxBytesInMatRequest, deps_t))

  io.busy := cmd.valid || cmd_tracker.io.busy

  // TODO: would this work fine? (want to make a dma request to a single DMA that is ready)
  val req_dma = MuxCase(0.U, Seq.tabulate(num_dma){
    i => io.dma(i).req.ready -> i.asUInt()
  })
  io.dma.zipWithIndex.foreach{case(d, i) =>
    when(req_dma === i.asUInt()){
      d.req.valid:= (control_state === waiting_for_command && cmd.valid && DoLoad && cmd_tracker.io.alloc.ready) ||
        control_state === waiting_for_dma_req_ready ||
        (control_state === sending_rows && row_counter =/= 0.U)
      d.req.bits.vaddr := vaddr + row_counter * stride
      d.req.bits.laddr := localaddr_plus_row_counter
      d.req.bits.len := cols
      d.req.bits.repeats := Mux(stride === 0.U, rows - 1.U, 0.U)
      d.req.bits.scale := scale
      d.req.bits.has_acc_bitwidth := localaddr_plus_row_counter.is_acc_addr && !shrink
      d.req.bits.status := mstatus
    }
  }

  val dma_resp_fires = io.dma.map(_.resp.fire())
  val dma_req_fires = io.dma.map(_.req.fire())
  val dma_acc_bitwidth = io.dma.map(_.req.bits.has_acc_bitwidth)
  // TODO: what if both dma's resp.fire() are 1?
  val dma_resp = MuxCase(io.dma(0).resp.bits, Seq.tabulate(num_dma){
    i => io.dma(i).resp.fire() -> io.dma(i).resp.bits
  })

  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoLoad //TODO: no need to check load_controller ID here?
  cmd_tracker.io.alloc.bits.bytes_to_read :=
    Mux(dma_acc_bitwidth.reduce(_||_), cols * actual_rows_read * config.accType.getWidth.U,
      cols * actual_rows_read * config.inputType.getWidth.U) / 8.U
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id.bits
  cmd_tracker.io.request_returned.valid := dma_resp_fires.reduce(_||_) && (dma_resp.ld_cont_id === io.ld_cont_id) // to check if the dma response is for this load controller
  cmd_tracker.io.request_returned.bits.cmd_id := dma_resp.cmd_id//Mux(io.dma_A.resp.fire(), io.dma_A.resp.bits.cmd_id, io.dma_B.resp.bits.cmd_id) // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := dma_resp.bytesRead//Mux(io.dma_A.resp.fire(), io.dma_A.resp.bits.bytesRead, io.dma_B.resp.bits.bytesRead)
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  for(d <- 0 until num_dma){
    io.dma(d).req.bits.cmd_id := cmd_id
    io.dma(d).req.bits.ld_cont_id := io.ld_cont_id
  }

  //io.dma.req.bits.cmd_id := cmd_id
  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  io.busy := cmd.valid || cmd_tracker.io.busy

  // Row counter
  val dma_req_fire = dma_req_fires.reduce(_||_)
  when (dma_req_fire) {
    row_counter := wrappingAdd(row_counter, 1.U, actual_rows_read)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        // when(DoConfig && !cmd_tracker.io.cmd_completed.valid) {
        when(DoConfig) {
          stride := config_stride
          scale := config_scale
          shrink := config_shrink
          cmd.ready := true.B
        }

        .elsewhen(DoLoad && cmd_tracker.io.alloc.fire()) {
          control_state := Mux(dma_req_fire, sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (dma_req_fire) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === actual_rows_read-1.U && dma_req_fire)

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }
}

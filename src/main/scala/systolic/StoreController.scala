package systolic

import chisel3._
import chisel3.util._
import SystolicISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO this is almost a complete copy of LoadController. We should combine them into one class
// TODO deal with errors when reading scratchpad responses
// class StoreController[T <: Data : Arithmetic](config: SystolicArrayConfig[T], coreMaxAddrBits: Int, sp_addr_t: SPAddr, acc_addr_t: AccAddr)
class StoreController[T <: Data : Arithmetic](config: SystolicArrayConfig[T], coreMaxAddrBits: Int, local_addr_t: LocalAddr)
                     (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDeps))

    // val dma = new ScratchpadWriteMemIO(sp_banks, sp_bank_entries, acc_rows)
    val dma = new ScratchpadWriteMemIO(local_addr_t)

    // TODO what's a better way to express no bits?
    val pushLoad = Decoupled(UInt(1.W))
    val pullLoad = Flipped(Decoupled(UInt(1.W)))
    val pushEx = Decoupled(UInt(1.W))
    val pullEx = Flipped(Decoupled(UInt(1.W)))

    val busy = Output(Bool())
  })

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val stride = RegInit((sp_width / 8).U(coreMaxAddrBits.W))
  val block_rows = meshRows * tileRows
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))

  val cmd = Queue(io.cmd, ld_str_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val localaddr = cmd.bits.cmd.rs2.asTypeOf(local_addr_t)
  val config_stride = cmd.bits.cmd.rs2
  val mstatus = cmd.bits.cmd.status

  val localaddr_plus_row_counter = localaddr + row_counter

  io.busy := cmd.valid

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoStore = !DoConfig // TODO change this if more commands are added

  cmd.ready := false.B

  val pullEx = cmd.bits.deps.pullEx
  val pushEx = cmd.bits.deps.pushEx
  val pullLoad = cmd.bits.deps.pullLoad
  val pushLoad = cmd.bits.deps.pushLoad
  val pullDep = pullEx || pullLoad
  val pushDep = pushEx || pushLoad

  val pull_deps_ready = !pullDep || (pullEx && io.pullEx.valid && !pullLoad) ||
    (pullLoad && io.pullLoad.valid && !pullEx) || (pullEx && pullLoad && io.pullEx.valid && io.pullLoad.valid)
  val push_deps_ready = !pushDep || (pushEx && io.pushEx.ready && !pushLoad) ||
    (pushLoad && io.pushLoad.ready && !pushEx) || (pushEx && pushLoad && io.pushEx.ready && io.pushLoad.ready)

  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoStore && pull_deps_ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && row_counter =/= 0.U)
  io.dma.req.bits.vaddr := vaddr + row_counter * stride
  io.dma.req.bits.laddr := localaddr_plus_row_counter
  io.dma.req.bits.status := mstatus

  io.pushLoad.valid := false.B
  io.pullLoad.ready := false.B
  io.pushEx.valid := false.B
  io.pullEx.ready := false.B

  // TODO are these really needed?
  io.pushLoad.bits := DontCare
  io.pushEx.bits := DontCare

  // Row counter
  when (io.dma.req.fire()) {
    row_counter := wrappingAdd(row_counter, 1.U, block_rows)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid && pull_deps_ready) {
        when(DoConfig && push_deps_ready) {
          stride := config_stride

          io.pushLoad.valid := pushLoad
          io.pullLoad.ready := pullLoad
          io.pullEx.ready := pullEx
          io.pushEx.valid := pushEx

          cmd.ready := true.B
        }

        .elsewhen(DoStore) {
          io.pullEx.ready := pullEx
          io.pullLoad.ready := pullLoad
          control_state := Mux(io.dma.req.fire(), sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire()) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === (block_rows-1).U && io.dma.req.fire())

      when (last_row && push_deps_ready) {
        control_state := waiting_for_command

        io.pushLoad.valid := pushLoad
        io.pushEx.valid := pushEx

        cmd.ready := true.B
      }
    }
  }
}

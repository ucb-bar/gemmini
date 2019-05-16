package systolic

import chisel3._
import chisel3.util._
import SystolicISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO this is almost a complete copy of LoadController. We should combine them into one class
class StoreController(config: SystolicArrayConfig, xLen: Int, sp_addr: SPAddr, acc_addr: AccAddr)(implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDeps))

    val dma = new ScratchpadMemIO(sp_banks, sp_bank_entries, acc_rows)

    // TODO what's a better way to express no bits?
    val pushLoad = Decoupled(UInt(1.W))
    val pullLoad = Flipped(Decoupled(UInt(1.W)))
    val pushEx = Decoupled(UInt(1.W))
    val pullEx = Flipped(Decoupled(UInt(1.W)))

    val busy = Output(Bool())
  })

  val waiting_for_command :: waiting_for_dma_resp :: waiting_for_dma_ready :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val wait_for_dma_req = WireInit(false.B) // Not really a state on its own, just a trigger for behavior that is shared across states

  val stride = RegInit((sp_width / 8).U(xLen.W))
  val block_rows = meshRows * tileRows

  val cmd = Queue(io.cmd, ld_str_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val accaddr = cmd.bits.cmd.rs2.asTypeOf(acc_addr)
  val spaddr = cmd.bits.cmd.rs2.asTypeOf(sp_addr)
  val config_stride = cmd.bits.cmd.rs2

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

  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoStore && pull_deps_ready) || control_state === waiting_for_dma_ready
  io.dma.req.bits.vaddr := vaddr
  io.dma.req.bits.spbank := spaddr.bank
  io.dma.req.bits.spaddr := spaddr.row
  io.dma.req.bits.accaddr := accaddr.row
  io.dma.req.bits.is_acc := accaddr.is_acc_addr
  io.dma.req.bits.stride := stride
  io.dma.req.bits.write := true.B
  io.dma.resp.ready := true.B

  io.pushLoad.valid := false.B
  io.pullLoad.ready := false.B
  io.pushEx.valid := false.B
  io.pullEx.ready := false.B

  // TODO are these really needed?
  io.pushLoad.bits := DontCare
  io.pushEx.bits := DontCare

  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid && pull_deps_ready && push_deps_ready) {
        when(DoConfig) {
          stride := config_stride

          io.pushLoad.valid := pushLoad
          io.pullLoad.ready := pullLoad
          io.pullEx.ready := pullEx
          io.pushEx.valid := pushEx

          cmd.ready := true.B
        }

        .elsewhen(io.dma.req.fire()) {
          io.pullEx.ready := pullEx
          io.pullLoad.ready := pullLoad

          control_state := waiting_for_dma_resp
        }
      }
    }

    is (waiting_for_dma_resp) {
      when (io.dma.resp.valid) {
        cmd.ready := true.B

        io.pushEx.valid := pushEx
        io.pushLoad.valid := pushLoad

        control_state := waiting_for_command
      }
    }

    is (waiting_for_dma_ready) {
      wait_for_dma_req := true.B
    }
  }

  when (wait_for_dma_req) {
    when (io.dma.req.fire()) {
      control_state := waiting_for_dma_resp
    }
  }
}

package systolic

import chisel3._
import chisel3.util._
import Util._
import freechips.rocketchip.config.Parameters

class LoadController(config: SystolicArrayConfig, spaddr: SPAddr)(implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDeps))

    val dma = new ScratchpadMemIO(sp_banks, sp_bank_entries)

    // TODO what's a better way to express no bits?
    val pushStore = Decoupled(UInt(1.W))
    val pullStore = Flipped(Decoupled(UInt(1.W)))
    val pushEx = Decoupled(UInt(1.W))
    val pullEx = Flipped(Decoupled(UInt(1.W)))
  })

  val waiting_for_command :: waiting_for_dma_resp :: Nil = Enum(2)
  val control_state = RegInit(waiting_for_command)

  // TODO make separate queue lengths for each controller
  val cmd = Queue(io.cmd, ld_str_queue_length)
  val rs1 = cmd.bits.cmd.rs1
  val rs2 = cmd.bits.cmd.rs2

  cmd.ready := false.B

  val pullEx = cmd.bits.deps.pullEx
  val pushEx = cmd.bits.deps.pushEx
  val pullStore = cmd.bits.deps.pullStore
  val pushStore = cmd.bits.deps.pushStore
  val pullDep = pullEx || pullStore
  val pushDep = pushEx || pushStore

  val pull_deps_ready = !pullDep || (pullEx && io.pullEx.valid && !pullStore) ||
    (pullStore && io.pullStore.valid && !pullEx) || (pullEx && pullStore && io.pullEx.valid && io.pullStore.valid)
  val push_deps_ready = !pushDep || (pushEx && io.pushEx.ready && !pushStore) ||
    (pushStore && io.pushStore.ready && !pushEx) || (pushEx && pushStore && io.pushEx.ready && io.pushStore.ready)

  io.dma.req.valid := control_state === waiting_for_command && cmd.valid && pull_deps_ready
  io.dma.req.bits.vaddr := rs1
  io.dma.req.bits.spbank := rs2.asTypeOf(spaddr).spbank
  io.dma.req.bits.spaddr := rs2.asTypeOf(spaddr).sprow
  io.dma.req.bits.write := false.B
  io.dma.resp.ready := true.B

  io.pushStore.valid := false.B
  io.pullStore.ready := false.B
  io.pushEx.valid := false.B
  io.pullEx.ready := false.B

  // TODO are these really needed?
  io.pushStore.bits := DontCare
  io.pushEx.bits := DontCare

  switch (control_state) {
    is (waiting_for_command) {
      when (io.dma.req.fire()) {
        io.pullEx.ready := pullEx
        io.pullStore.ready := pullStore
        control_state := waiting_for_dma_resp
      }
    }

    is (waiting_for_dma_resp) {
      when (io.dma.resp.valid && push_deps_ready) {
        cmd.ready := true.B
        io.pushEx.valid := pushEx
        io.pushStore.valid := pushStore
        control_state := waiting_for_command
      }
    }
  }
}

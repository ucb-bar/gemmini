package systolic

import chisel3._
import chisel3.util._
import SystolicISA._
import Util._
import freechips.rocketchip.config.Parameters

class LoadController[T <: Data](config: SystolicArrayConfig, xLen: Int, sp_addr: SPAddr, acc_addr: AccAddr,
                                inputType: T, accType: T)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDeps))

    val dma = new ScratchpadMemIO(sp_banks, sp_bank_entries, acc_rows)

    // TODO what's a better way to express no bits?
    val pushStore = Decoupled(UInt(1.W))
    val pullStore = Flipped(Decoupled(UInt(1.W)))
    val pushEx = Decoupled(UInt(1.W))
    val pullEx = Flipped(Decoupled(UInt(1.W)))

    val busy = Output(Bool())
  })

  val waiting_for_command :: waiting_for_dma_resp :: waiting_for_dma_ready :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  val stride = RegInit((sp_width / 8).U(xLen.W))
  val block_rows = meshRows * tileRows
  val sp_row_offset = RegInit(0.U(log2Ceil(block_rows).W))
  val vaddr_offset = RegInit(0.U(xLen.W))

  val done_loading = sp_row_offset === 0.U

  val cmd = Queue(io.cmd, ld_str_queue_length)
  val vaddr = cmd.bits.cmd.rs1
  val accaddr = cmd.bits.cmd.rs2.asTypeOf(acc_addr)
  val spaddr = cmd.bits.cmd.rs2.asTypeOf(sp_addr)
  val config_stride = cmd.bits.cmd.rs2

  // TODO find more elegant way to load into accumulator
  val acc_load_beats = accType.getWidth / inputType.getWidth
  val acc_load_beat_stride = sp_width / 8
  val acc_load_cntr = RegInit(0.U((log2Ceil(acc_load_beats) max 1).W))
  val done_loading_acc = WireInit(false.B)

  io.busy := cmd.valid

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD
  val DoLoad = !DoConfig // TODO change this if more commands are added

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

  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoLoad && pull_deps_ready) || control_state === waiting_for_dma_ready
  io.dma.req.bits.vaddr := vaddr + vaddr_offset
  io.dma.req.bits.spbank := spaddr.bank
  io.dma.req.bits.spaddr := spaddr.row + sp_row_offset
  io.dma.req.bits.accaddr := accaddr.row + sp_row_offset
  io.dma.req.bits.is_acc := accaddr.is_acc_addr
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
      when (cmd.valid && pull_deps_ready && push_deps_ready) {
        when(DoConfig) {
          stride := config_stride

          io.pushStore.valid := pushStore
          io.pullStore.ready := pullStore
          io.pullEx.ready := pullEx
          io.pushEx.valid := pushEx

          cmd.ready := true.B
        }

        .elsewhen(io.dma.req.fire()) {
          io.pullStore.ready := pullStore
          io.pullEx.ready := pullEx

          when (accaddr.is_acc_addr && (acc_load_beats != 1).B) {
            acc_load_cntr := wrappingAdd(acc_load_cntr, 1.U, acc_load_beats)
            vaddr_offset := vaddr_offset + acc_load_beat_stride.U
          }.otherwise {
            sp_row_offset := wrappingAdd(sp_row_offset, 1.U, block_rows)
            vaddr_offset := vaddr_offset + stride
          }

          control_state := waiting_for_dma_resp
        }
      }
    }

    is (waiting_for_dma_resp) {
      when (io.dma.resp.valid) {
        when (done_loading && (!accaddr.is_acc_addr || acc_load_cntr === 0.U)) {
          cmd.ready := true.B

          io.pushStore.valid := pushStore
          io.pushEx.valid := pushEx

          vaddr_offset := 0.U
          acc_load_cntr := 0.U

          control_state := waiting_for_command
        }.otherwise {
          // TODO we may be wasting a cycle here. Do we have to take a detour into waiting_for_dma_ready?
          control_state := waiting_for_dma_ready
        }
      }
    }

    is (waiting_for_dma_ready) {
      when (io.dma.req.fire()) {
        when (accaddr.is_acc_addr) {
          acc_load_cntr := wrappingAdd(acc_load_cntr, 1.U, acc_load_beats)
          vaddr_offset := vaddr_offset + acc_load_beat_stride.U
          done_loading_acc := acc_load_cntr === (acc_load_beats-1).U
        }

        when (!accaddr.is_acc_addr || done_loading_acc) {
          sp_row_offset := wrappingAdd(sp_row_offset, 1.U, block_rows)
          vaddr_offset := Mux(!accaddr.is_acc_addr, vaddr_offset + stride,
            vaddr_offset + (stride - (acc_load_beat_stride * (acc_load_beats-1)).U))
        }

        control_state := waiting_for_dma_resp
      }
    }
  }
}

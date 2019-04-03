package systolic

import chisel3._
import chisel3.util._
import Util._
import freechips.rocketchip.config.Parameters

class DMAArbiter(val nBanks: Int, val nRows: Int)
                (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val load = Flipped(new ScratchpadMemIO(nBanks, nRows))
    val store = Flipped(new ScratchpadMemIO(nBanks, nRows))
    val dma = new ScratchpadMemIO(nBanks, nRows)
  })

  assert(!(io.load.req.valid && io.load.req.bits.write), "writing on load port")
  assert(!(io.store.req.valid && !io.store.req.bits.write), "reading on store port")

  val waiting_for_cmd :: waiting_for_load_resp :: waiting_for_store_resp :: waiting_for_store_req_ready :: Nil = Enum(4)
  val control_state = RegInit(waiting_for_cmd)

  val backed_up_store_init = Wire(Valid(new ScratchpadMemRequest(nBanks, nRows)))
  backed_up_store_init.bits := DontCare
  backed_up_store_init.valid := false.B

  val backed_up_store = RegInit(backed_up_store_init) // We back up store commands if a load and store are requested at the same time

  io.store.req.ready := control_state === waiting_for_cmd && io.dma.req.ready
  io.load.req.ready := control_state === waiting_for_cmd && io.dma.req.ready

  io.dma.req.bits := DontCare
  io.dma.req.valid := false.B
  io.dma.resp.ready := false.B

  io.load.resp.bits := io.dma.resp.bits
  io.store.resp.bits := io.dma.resp.bits
  io.load.resp.valid := false.B
  io.store.resp.valid := false.B

  switch (control_state) {
    is (waiting_for_cmd) {
      when (io.dma.req.ready) {
        when(io.load.req.valid && !io.store.req.valid) {
          io.dma.req <> io.load.req
          control_state := waiting_for_load_resp
        }.elsewhen(io.store.req.valid && !io.load.req.valid) {
          io.dma.req <> io.store.req
          control_state := waiting_for_store_resp
        }.elsewhen(io.load.req.valid && io.store.req.valid) {
          io.dma.req <> io.load.req

          backed_up_store.bits := io.store.req.bits
          backed_up_store.valid := true.B

          control_state := waiting_for_load_resp
        }
      }
    }

    is (waiting_for_load_resp) {
      io.load.resp <> io.dma.resp

      when (io.dma.resp.fire() && !backed_up_store.valid) {
        // TODO We waste a cycle here. We could immediately issue a new command here
        control_state := waiting_for_cmd
      }.elsewhen(io.dma.resp.fire() && io.dma.req.ready) {
        io.dma.req.bits := backed_up_store.bits
        backed_up_store.valid := false.B
        control_state := waiting_for_store_resp
      }.elsewhen(io.dma.resp.fire()) {
        control_state := waiting_for_store_req_ready
      }
    }

    is (waiting_for_store_resp) {
      io.store.resp <> io.dma.resp

      // TODO We waste a cycle here. We could immediately issue a new command here
      when (io.dma.resp.fire()) {
        control_state := waiting_for_cmd
      }
    }

    is (waiting_for_store_req_ready) {
      io.dma.req.bits := backed_up_store.bits
      io.dma.req.valid := true.B

      when (io.dma.req.fire()) {
        backed_up_store.valid := false.B
        control_state := waiting_for_store_resp
      }
    }
  }
}

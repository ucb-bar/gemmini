// See README.md for license details.

package systolic

import chisel3._
import chisel3.util._



class RoCCUnit(implicit p: Parameters) extends Module()(p) {

  val io = new Bundle {
       val rocc = new freechips.rocketchip.tile.RoCCCoreIO
  }

  // Decode
  val rocc_inst = io.rocc.cmd.inst.asUInt
  val funct = rocc_inst.funct
  val rs1 = io.rocc.cmd.bits.rs1
  val rs2 = io.rocc.cmd.bits.rs2 


  io.cmd.ready := (state === s_idle)

  io.rocc..busy := (state =/= s_idle)
  io.rocc.interrupt := Bool(false)
  io.rocc.mem.req.valid := Bool(false)

  when (io.cmd.fire()) {
    addr1 := io.cmd.bits.rs1
    addr2 := io.cmd.bits.rs2
    resp_rd := io.cmd.bits.inst.rd
  }


}


}





/**
  * A wrapper between RoCC interface and the systolic array controller.
  */
/*


class SystolicArray(implicit p: Parameters) extends LazyRoCC {
      override lazy val module = new SystolicArrayModule(this)
      override val atlNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("SystolicArrayRoCC")))))
}

class SystolicArrayModule(outer: SystolicArray) extends LazyRoCCModule(outer) {
  val cmd = Queue(io.cmd)
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2

  //RoCC instruction handling
  //=======================================
  val rocc <> Module(new RoCCUnit) 
  val sys_arr_ctrl = Module(new Controller)
  rocc.io.rocc.cmd  <> io.cmd
  io.resp <> rocc.io.rocc.resp
  io.busy <> rocc.io.rocc.busy
  io.interrupt <> rocc.io.rocc.interrupt
  rocc.io.rocc.exception <> io.exception


  //dma port to system bus
  //====================================
  private val blockOffset = blockOffBits
  private val beatOffset = log2Up(cacheDataBits/8)

  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
  val offset = addr(blockOffset - 1, 0)
  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)

  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val (tl_out, edgesOut) = outer.atlNode.out(0)
  val gnt = tl_out.d.bits
  val recv_data = Reg(UInt(width = cacheDataBits))
  val recv_beat = Reg(UInt(width = log2Up(cacheDataBeats+1)), init = UInt(0))

  tl_out.a.valid := (state === s_acq)
  tl_out.a.bits := edgesOut.Get(
                       fromSource = UInt(0),
                       toAddress = addr_block << blockOffset,
                       lgSize = UInt(lgCacheBlockBytes))._2
  tl_out.d.ready := (state === s_gnt)

  when (tl_out.a.fire()) { state := s_gnt }

  when (tl_out.d.fire()) {
    recv_beat := recv_beat + UInt(1)
    recv_data := gnt.data
    state := s_check
  }

  when (state === s_check) {
    //Accelerator functionality (finished = when done with this piece of memory)
    when (recv_beat === UInt(cacheDataBeats)) {
      addr := next_addr
      state := Mux(finished, s_resp, s_acq)
    } .otherwise {
      state := s_gnt
    }
  }


  io.interrupt := Bool(false)
  io.mem.req.valid := Bool(false)
  // Tie off unused channels
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)

  //===============================================

  //Page Table Translation Request
  //==============================================
  val req_addr = Reg(UInt(width = coreMaxAddrBits))
  val req_rd = Reg(io.resp.bits.rd)
  val req_offset = req_addr(pgIdxBits - 1, 0)
  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
  val pte = Reg(new PTE)

  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  private val ptw = io.ptw(0)

  when (ptw.req.fire()) { state := s_ptw_resp }

  when (state === s_ptw_resp && ptw.resp.valid) {
    pte := ptw.resp.bits.pte
    state := s_resp
  }

  ptw.req.valid := (state === s_ptw_req)
  ptw.req.bits.valid := true.B
  ptw.req.bits.bits.addr := req_vpn

  io.resp.valid := (state === s_resp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), SInt(-1, xLen).asUInt)


  //=========================================

}

class WithSystolicArray extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site).map { r =>
    r.copy(rocc = Seq(
      RoCCParams(
        opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
        generator = (p: Parameters) => LazyModule(new SystolicArray()(p)))))
  }
})

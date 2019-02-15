package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

class SystolicArray(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new SystolicArrayModule(this)
  override val atlNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("SystolicArrayRoCC")))))
}

class SystolicArrayModule(outer: SystolicArray,val width: Int, val tileRows: Int, val tileColumns: Int, val meshRows: Int, val meshColumns: Int,
                          val sramEntries: Int, val queue_length: Int, val block_size: Int) extends LazyRoCCModule(outer) {
  val cmd = Queue(io.cmd, queue_length)
  io.busy := !(cmd.entries === 0.U)

  val fire_counter = new Counter(block_size)

  //aliases of cmd
  val rs1 = Reg(UInt())
  rs1 := cmd.bits.rs1
  val rs2 = Reg(UInt())
  rs2 := cmd.bits.rs2
  //val rs3 = RegInit(cmd.bits.rs3)
  val d_address_rs1 = Reg(UInt()) //verify if it is only updated once
  val c_address_rs2 = Reg(UInt())
  val preload_flag = RegInit(false.B)
  //val rd = RegInit(cmd.bits.rd)
  val funct = cmd.bits.inst.funct
  val opcode = cmd.bits.inst.opcode
  val DoLoad = funct === UInt(2)
  val DoStore = funct === UInt(3)
  val DoComputeAndWrite = funct === UInt(4)
  val DoComputeOnly = funct === UInt(5)
  val DoPreLoad = funct === UInt(6)

  // STATE defines
  val idle :: start_load_to_SRAM :: Nil = Enum(2)
  val DRAM_to_SRAM_state = RegInit(idle)
  val idle :: start_store_to_DRAM :: Nil = Enum(2)
  val SRAM_to_DRAM_state = RegInit(idle)
  val idle :: feed_data :: Nil = Enum(2)

  val feed_state = RegInit(idle)

  //SRAM scratchpad
  val sp_a = Module(new InputScratchpad(n,w))
  val sp_b = Module(new InputScratchpad(n,w))
  val sp_c = Module(new InputScratchpad(n,w))
  val sp_d = Module(new InputScratchpad(n,w))


  mesh.io.a.valid := false.B
  mesh.io.b.valid := false.B
  mesh.io.d.valid := false.B
  mesh.io.out.ready := false.B


  sp_a.io.read.addr := rs1
  sp_b.io.read.addr := rs2
  sp_d.io.read.addr := d_address_rs1
  sp_c.io.write.addr := c_address_rs2
  sp_a.io.read.en := false.B
  sp_b.io.read.en := false.B
  sp_c.io.write.en := false.B
  sp_d.io.read.en := false.B

  val wrap = Wire(Bool()) // is this false by default?
  wrap := false.B

  when(cmd.valid && DoPreLoad) {
    d_address_rs1 := cmd.bits.rs1 //SRAM_D_Address
    c_address_rs2 := cmd.bits.rs2 //SRAM_C_Address
    cmd.deq.ready := true.B
    preload_flag := true.B
  }



  switch(feed_state) {
    is(idle) {
      when((DoComputeAndWrite || DoComputeOnly) && cmd.valid) {
        sp_a.io.read.en := true.B
        sp_b.io.read.en := true.B
        sp_d.io.read.en := preload_flag

        compute_state := feed_data
      }

    }
    is(feed_data) {
      sp_a.io.read.en := true.B
      sp_b.io.read.en := true.B
      sp_d.io.read.en := preload_flag

      when(mesh.io.a.ready && mesh.io.b.ready && mesh.io.d.ready) {
        wrap = fire_counter.inc()
        rs1 := rs1 + 1.U // check if should move to previous state too
        rs2 := rs2 + 1.U
        d_address_rs1 := d_address_rs1 + 1.U

        mesh.io.a.valid := true.B
        mesh.io.a.bits := sp_a.io.read.data
        mesh.io.b.valid := true.B
        mesh.io.b.bits := sp_b.io.read.data
        mesh.io.d.valid := true.B
        mesh.io.d.bits := Mux(preload_flag, sp_d.io.read.data, 0.U)
        mesh.io.out_address.valid := true.B
        mesh.io.out_address.bits := c_address_rs2

      }

      when(wrap) {
        preload_flag := false.B
        compute_state := idle
        cmd.deq.ready := true.B
      }
    }
  }

  mesh.io.out.ready := true.B

  when(mesh.io.out.fire()) {
    sp_c.io.write.en := true.B
    sp_c.io.write.wdata := mesh_io.out.bits
  }



//        when(DoComputeOnly){
//
//        }
//        }.elsewhen(DoComputeAndWrite){
//          compute_state := write_output
//          mesh.io.out.ready := true.B
//
//        }
//      }
//    }
//    is(write_output) {
//      sp_c.io.write.en := true.B
//      mesh.io.out.ready := true.B
//      when(mesh.io.out.fire()) {
//        wrap = fire_counter.inc()
//        rd := rd + 1.U
//        sp_c.io.write.wdata := mesh.io.out.bits
//      }
//      when(wrap) {
//        compute_state := idle
//        cmd.deq.ready := true.B
//
//        }
//      }
//    }


  cmd.deq.ready := false.B
  sp_a.req.valid := false.B

  switch(DRAM_to_SRAM_state){
    is(idle) {
      when (DoLoad && cmd.valid && sp_a.io.req.ready){
        sp_a.io.req.valid := true.B
        sp_a.io.req.bits := ScratchpadMemRequest(
          vaddr = rs1,
          spaddr = rs2,
          write = true.B
        )
        DRAM_to_SRAM_state := start_load_to_SRAM
      }
    }
    is(start_load_to_SRAM){
      when(sp_a.io.resp.valid){
        cmd.deq.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }

  switch(SRAM_to_DRAM_state){
    is(idle) {
      when (DoStore && cmd.valid && sp_a.io.req.ready){
        sp_a.io.req.valid := true.B
        sp_a.io.req.bits := ScratchpadMemRequest(
          vaddr = rs2,
          spaddr = rs1,
          write = false.B
        )
        SRAM_to_DRAM_state := start_store_to_DRAM

      }
    is(start_store_to_DRAM){
      when(sp_a.io.resp.valid){
        cmd.deq.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }

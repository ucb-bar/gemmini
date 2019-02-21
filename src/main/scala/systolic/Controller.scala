package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.tile._

case class SystolicArrayConfig(
                                val tileRows: Int,
                                val tileColumns: Int,
                                val meshRows: Int,
                                val meshColumns: Int,
                                val queue_length: Int,
                                val block_size: Int,
                                val sp_banks: Int,
                                val sp_bank_entries: Int,
                                val sp_width: Int,
                                val InternalSramEntries: Int,
                                val InternalSramBanks: Int
                              )
case object SystolicArrayKey extends Field[SystolicArrayConfig]

class SystolicArray[T <: Data: Arithmetic](dtype: T, opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC (
    opcodes = OpcodeSet.custom3,
    nPTWPorts = 1) {
  val config = p(SystolicArrayKey)
  val spad = LazyModule(new Scratchpad(
    config.sp_banks, config.sp_bank_entries, config.sp_width))
  override lazy val module = new SystolicArrayModule(this, dtype)
  override val tlNode = spad.node
}

class SystolicArrayModule[T <: Data: Arithmetic]
    (outer: SystolicArray[T], val inner_type: T)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {
  import outer.config._
  import outer.spad

  val cmd = Queue(io.cmd, queue_length)

  io.busy := cmd.valid

  val fire_counter = new Counter(block_size)
  val output_counter = new Counter(block_size)

  //aliases of cmd
  val rs1 = Reg(UInt(xLen.W))
  rs1 := cmd.bits.rs1
  val rs2 = Reg(UInt(xLen.W))
  rs2 := cmd.bits.rs2
  //val rs3 = RegInit(cmd.bits.rs3)
  val d_address_rs1 = Reg(UInt(xLen.W)) //verify if it is only updated once
  val c_address_rs2 = Reg(UInt(xLen.W))
  val preload_zeros = RegInit(false.B)
  //val rd = RegInit(cmd.bits.rd)
  val funct = cmd.bits.inst.funct
  val opcode = cmd.bits.inst.opcode
  val DoLoad = funct === 2.U
  val DoStore = funct === 3.U
  val DoComputeAndFlip = funct === 4.U
  val DoComputeAndStay = funct === 5.U
  val DoPreLoad = funct === 6.U

  val meshIO = Module(new MeshWithMemory(inner_type,Dataflow.BOTH, tileRows,
    tileColumns,meshRows,meshColumns,
    InternalSramEntries,InternalSramBanks)) //what you mean by T/df/banks in MeshWithMemory

  // STATE defines
  val idle :: feed_data :: Nil = Enum(2)
  val idle_store :: start_load_to_SRAM :: Nil = Enum(2)
  val idle_load :: start_store_to_DRAM :: Nil = Enum(2)
  val DRAM_to_SRAM_state = RegInit(idle)
  val SRAM_to_DRAM_state = RegInit(idle)
  val feed_state = RegInit(idle)

  ////////
  implicit val edge = outer.tlNode.edges.out.head
  val tlb = Module(new FrontendTLB(1, 4))
  tlb.io.clients(0) <> outer.spad.module.io.tlb
  io.ptw.head <> tlb.io.ptw

  ///////
  //asserts
  assert(meshRows*tileRows == meshColumns*tileColumns) // this also assumes symmetric systolic array
  assert(meshRows*tileRows == InternalSramEntries)
  assert(InternalSramEntries == sp_bank_entries)
  assert(sp_width == meshRows*tileRows*inner_type.getWidth)
  assert(block_size == meshRows*tileRows)

  // compute/output_compute counters
  val blocks_fired = new Counter(10)
  val blocks_outputed = new Counter(10)

  //SRAM scratchpad
  val a_read_bank_number = rs1(rs1.getWidth - 1, rs1.getWidth-log2Ceil(sp_banks))
  val b_read_bank_number = rs2(rs2.getWidth - 1, rs2.getWidth-log2Ceil(sp_banks))
  val d_read_bank_number = d_address_rs1(d_address_rs1.getWidth - 1, d_address_rs1.getWidth-log2Ceil(sp_banks))

  val sp_a_read = spad.module.io.read(a_read_bank_number)
  val sp_b_read = spad.module.io.read(b_read_bank_number)
  val sp_d_read = spad.module.io.read(d_read_bank_number)

  meshIO.io.a.valid := false.B
  meshIO.io.b.valid := false.B
  meshIO.io.d.valid := false.B
  meshIO.io.out.ready := false.B

  sp_a_read.addr := rs1
  sp_b_read.addr := rs2
  sp_d_read.addr := d_address_rs1
  //sp_c.io.write.addr := c_address_rs2
  sp_a_read.en := false.B
  sp_b_read.en := false.B
  //sp_c.io.write.en := false.B
  sp_d_read.en := false.B

  val fired_all_rows = Wire(Bool())
  fired_all_rows := false.B

  val outputed_all_rows = Wire(Bool())
  outputed_all_rows := false.B

  when(cmd.valid && DoPreLoad) {
    d_address_rs1 := cmd.bits.rs1 //SRAM_D_Address
    c_address_rs2 := cmd.bits.rs2 //SRAM_C_Address
    cmd.ready := true.B
    preload_zeros := cmd.bits.inst.rd === 1.U // when rd number is 1 it means to preload zeros
  }



  switch(feed_state) {
    is(idle) {
      when((DoComputeAndFlip || DoComputeAndStay) && cmd.valid) {
        sp_a_read.en := true.B
        sp_b_read.en := true.B
        sp_d_read.en := preload_zeros

        feed_state := feed_data
      }

    }
    is(feed_data) {
      sp_a_read.en := true.B
      sp_b_read.en := true.B
      sp_d_read.en := preload_zeros

      when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
        fired_all_rows := fire_counter.inc()
        rs1 := rs1 + 1.U // check if should move to previous state too
        rs2 := rs2 + 1.U
        d_address_rs1 := d_address_rs1 + 1.U

        meshIO.io.a.valid := true.B
        meshIO.io.a.bits := sp_a_read.data.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
        meshIO.io.b.valid := true.B
        meshIO.io.b.bits := sp_b_read.data.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
        meshIO.io.d.valid := true.B
        meshIO.io.d.bits := Mux(
          preload_zeros,
          sp_d_read.data.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type))),
          (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
        )
        meshIO.io.tag_in.valid := true.B
        meshIO.io.tag_in.bits := c_address_rs2 //if this is 0xFFFFFF then don't output
        meshIO.io.s.bits := DoComputeAndFlip
        meshIO.io.m.bits := Dataflow.OS.id.U
      }

      when(fired_all_rows) {
        preload_zeros := false.B
        feed_state := idle
        cmd.ready := true.B
        blocks_fired.inc()
      }
    }
  }

  meshIO.io.out.ready := true.B

  when(meshIO.io.out.fire() && !meshIO.io.tag_out===0xFFFFFFFFL.U) {
    val w_address = meshIO.io.tag_out
    val w_bank_number = w_address(w_address.getWidth,w_address.getWidth-log2Ceil(sp_banks))
    val w_bank_address = w_address(w_address.getWidth-log2Ceil(sp_banks)-1,0)

    spad.module.io.write(w_bank_number).en := true.B
    spad.module.io.write(w_bank_number).addr := w_bank_address
    spad.module.io.write(w_bank_number).data := meshIO.io.out.bits.asUInt()
    outputed_all_rows := output_counter.inc()
  }
when(outputed_all_rows) {blocks_outputed.inc()}


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


  cmd.ready := false.B
  spad.module.io.dma.req.valid := false.B

  switch(DRAM_to_SRAM_state){
    is(idle_load) {
      when (DoLoad && cmd.valid && spad.module.io.dma.req.ready && blocks_fired.value === blocks_outputed.value){
        spad.module.io.dma.req.valid := true.B
        spad.module.io.dma.req.bits.vaddr := rs1
        spad.module.io.dma.req.bits.spbank := rs2(rs2.getWidth,rs2.getWidth-log2Ceil(sp_banks))
        spad.module.io.dma.req.bits.spaddr := rs2(rs2.getWidth-log2Ceil(sp_banks)-1,0)
        spad.module.io.dma.req.bits.write := true.B
        DRAM_to_SRAM_state := start_load_to_SRAM
      }
    }
    is(start_load_to_SRAM){
      when(spad.module.io.dma.resp.valid){
        cmd.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }
  spad.module.io.dma.resp.ready := true.B

  switch(SRAM_to_DRAM_state){
    is(idle_store) {
      when (DoStore && cmd.valid && spad.module.io.dma.req.ready && blocks_fired.value === blocks_outputed.value){
        spad.module.io.dma.req.valid := true.B
        spad.module.io.dma.req.bits.vaddr := rs2
        spad.module.io.dma.req.bits.spbank := rs1(rs1.getWidth,rs1.getWidth-log2Ceil(sp_banks))
        spad.module.io.dma.req.bits.spaddr := rs1(rs1.getWidth-log2Ceil(sp_banks)-1,0)
        spad.module.io.dma.req.bits.write := false.B
        SRAM_to_DRAM_state := start_store_to_DRAM

      }
    }
    is(start_store_to_DRAM){
      when(spad.module.io.dma.resp.valid){ // assumes no page faults occur
        cmd.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }
}

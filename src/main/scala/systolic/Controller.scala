package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import Util._

case class SystolicArrayConfig(
  tileRows: Int,
  tileColumns: Int,
  meshRows: Int,
  meshColumns: Int,
  queue_length: Int,
  block_size: Int,
  sp_banks: Int,
  sp_bank_entries: Int,
  sp_width: Int,
  InternalSramEntries: Int,
  InternalSramBanks: Int
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

  val rs_translate = new Bundle {
    val junk = UInt((xLen-log2Ceil(sp_bank_entries)-log2Ceil(sp_banks)).W)
    val spbank = UInt(log2Ceil(sp_banks).W)
    val spaddr = UInt(log2Ceil(sp_bank_entries).W)
  }
  val cmd = Queue(io.cmd, queue_length)
  cmd.ready := false.B
  io.busy := cmd.valid
  // io.cmd.valid implies io.cmd.bits.inst.xd = 0
  assert(!io.cmd.valid || io.cmd.bits.inst.xd === false.B,
    "This controller doesn't support rd instructions due to unconnected RoCC resp")

  //aliases of cmd
  val mydataflow = RegInit(Dataflow.OS.id.U)
  val rs1 = cmd.bits.rs1
  val rs2 = cmd.bits.rs2
  val a_address_rs1 = Reg(rs_translate)
  val b_address_rs2 = Reg(rs_translate)

  val d_address_rs1 = cmd.bits.rs1.asTypeOf(rs_translate) //SRAM_D_Address
  val c_address_rs2 = cmd.bits.rs2.asTypeOf(rs_translate) //SRAM_C_Address

  val funct = cmd.bits.inst.funct
  val DoLoad = funct === 2.U
  val DoStore = funct === 3.U
  val DoComputeAndFlip = funct === 4.U
  val DoComputeAndStay = funct === 5.U

  val DoPreLoad = funct === 8.U
  val DoSetMode = funct === 9.U

  val preload_zeros = WireInit(cmd.bits.inst.rd === 1.U) // when rd number is 1 it means to preload zeros

  val meshIO = Module(new MeshWithMemory(inner_type, xLen, Dataflow.BOTH, tileRows,
    tileColumns, meshRows, meshColumns,
    InternalSramEntries, InternalSramBanks))

  // STATE defines
  // TODO these need to be cleaned up
  val decode :: compute :: flush :: flushing :: Nil = Enum(4)
  val control_state = RegInit(decode)

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

  //SRAM scratchpad
  val a_read_bank_number = a_address_rs1.spbank
  val b_read_bank_number = b_address_rs2.spbank
  val d_read_bank_number = d_address_rs1.spbank

  meshIO.io.a.valid := false.B
  meshIO.io.b.valid := false.B
  meshIO.io.d.valid := false.B
  meshIO.io.s.valid := false.B
  meshIO.io.tag_in.valid := false.B
  meshIO.io.flush.valid := false.B

  meshIO.io.a.bits := DontCare
  meshIO.io.b.bits := DontCare
  meshIO.io.d.bits := DontCare
  meshIO.io.tag_in.bits := DontCare
  meshIO.io.s.bits := DontCare
  meshIO.io.m := mydataflow

  val start_sram_feeding = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)
  val perform_store = WireInit(false.B)
  val perform_load = WireInit(false.B)
  val perform_single_preload = WireInit(false.B)
  val perform_single_mul = WireInit(false.B)
  val perform_mul_pre = WireInit(false.B)
  // TODO these are confusingly named
  val performing_single_preload = RegNext(perform_single_preload)
  val performing_single_mul = RegNext(perform_single_mul)
  val performing_mul_pre = RegNext(perform_mul_pre)

  val fired_all_rows = WireInit(false.B)
  val fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))
  val fire_count_started = Reg(Bool())
  val output_counter = new Counter(block_size)

  fire_counter := 0.U
  fire_count_started := perform_mul_pre || perform_single_preload || performing_single_mul

  // TODO should this be a wire to change immediately?
  val in_s = Reg(Bool())
  when (cmd.valid && DoComputeAndFlip) {
    in_s := true.B
  }.elsewhen (cmd.valid && DoComputeAndStay) {
    in_s := false.B
  }

  val tag_garbage = Cat(Seq.fill(xLen)(1.U(1.W)))

  for(i <- 0 until sp_banks){
    spad.module.io.read(i).en := start_sram_feeding &&
      ((a_read_bank_number === i.U && !perform_single_preload) ||
       (b_read_bank_number === i.U && !perform_single_preload) ||
       (d_read_bank_number === i.U && !preload_zeros))
    spad.module.io.read(i).addr := MuxCase(0.U,
      Seq(
        (a_read_bank_number === i.U && !perform_single_preload) -> (a_address_rs1.spaddr + fire_counter),
        (b_read_bank_number === i.U && !perform_single_preload) -> (b_address_rs2.spaddr + fire_counter),
        (d_read_bank_number === i.U && !preload_zeros) -> (d_address_rs1.spaddr + block_size.U - 1.U - fire_counter)
      )
    )
  }

  val readData = VecInit(spad.module.io.read.map(_.data))
  val dataA = readData(RegNext(a_read_bank_number))
  val dataB = readData(RegNext(b_read_bank_number))
  val dataD = readData(RegNext(d_read_bank_number))

  switch(control_state){
    is(decode){
      when(cmd.valid && DoStore){
        perform_store := true.B
      }

      when(cmd.valid && DoLoad){
        perform_load := true.B
      }

      when(cmd.valid && DoSetMode){
        val data_mode = rs1(0) //0 is output stationary, 1 is weight
        mydataflow := data_mode
        cmd.ready := true.B
      }

      when(cmd.valid && DoPreLoad) {
        perform_single_preload := true.B
        start_sram_feeding := true.B
      }

      when(cmd.valid && (DoComputeAndFlip || DoComputeAndStay)) {
        a_address_rs1 := rs1.asTypeOf(rs_translate)
        b_address_rs2 := rs2.asTypeOf(rs_translate)
        control_state := compute
        cmd.ready := true.B
      }
    }
    is(compute){
      // TODO Flush when no valid command is waiting
      when (cmd.valid && !DoPreLoad) {
        perform_single_mul := true.B
        start_sram_feeding := true.B
        preload_zeros := true.B
        when(fired_all_rows) {
          control_state := flush
        }
      }.elsewhen (cmd.valid && DoPreLoad) {
        start_sram_feeding := true.B
        perform_mul_pre := true.B
        when(fired_all_rows){
          cmd.ready := true.B
        }
      }
    }
    is (flush) {
      when(meshIO.io.flush.ready) {
        meshIO.io.flush.valid := true.B
        control_state := flushing
      }
    }
    is (flushing) {
      when(meshIO.io.flush.ready) {
        control_state := decode
      }
    }
  }

  when(perform_mul_pre){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
      fired_all_rows := fire_counter === 0.U && fire_count_started
    }
  }

  when (performing_mul_pre) {
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := Mux(
      preload_zeros,
      (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type))),
      dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    )
    meshIO.io.tag_in.valid := true.B
    meshIO.io.s.valid := true.B
    meshIO.io.tag_in.bits := c_address_rs2.asUInt() //if this is 0xFFFFFF then don't output
    meshIO.io.s.bits := in_s

    when (fired_all_rows) {
      performing_mul_pre := false.B
    }
  }

  when(perform_single_mul){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
      fired_all_rows := fire_counter === 0.U && fire_count_started
    }
  }

  when(performing_single_mul) {
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))

    meshIO.io.tag_in.valid := true.B
    meshIO.io.s.valid := true.B
    meshIO.io.tag_in.bits := tag_garbage //if this is 0xFFFFFF then don't output
    meshIO.io.s.bits := in_s

    // TODO ugly
    when (fired_all_rows) {
      performing_single_mul := false.B
    }
  }

  when(perform_single_preload){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
      fired_all_rows := fire_counter === 0.U && fire_count_started
    }
  }

  when (performing_single_preload) {
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := Mux(
      preload_zeros,
      (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type))),
      dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    )
    meshIO.io.tag_in.valid := true.B
    meshIO.io.s.valid := true.B
    meshIO.io.tag_in.bits := c_address_rs2.asUInt() //if this is 0xFFFFFF then don't output
    meshIO.io.s.bits := in_s

    // TODO ugly
    when (fired_all_rows) {
      cmd.ready := true.B
      performing_single_preload := false.B
    }
  }

  meshIO.io.out.ready := true.B

  val w_address = meshIO.io.tag_out.asTypeOf(rs_translate)
  val w_bank_number = w_address.spbank
  val w_bank_address = w_address.spaddr

  for(i <- 0 until sp_banks) {
    spad.module.io.write(i).en := start_array_outputting && w_bank_number === i.U
    spad.module.io.write(i).addr := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> (w_bank_address + block_size.U - 1.U - output_counter.value)))
    spad.module.io.write(i).data := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> meshIO.io.out.bits.asUInt()))
  }

  when(meshIO.io.out.fire() && meshIO.io.tag_out =/= tag_garbage) {
    output_counter.inc()
    start_array_outputting := true.B
  }

  spad.module.io.dma.resp.ready := true.B // The controller discards DMA responses
  // For both mvin and mvout, rs1 = DRAM address, rs2 = scratchpad address
  spad.module.io.dma.req.bits.vaddr := rs1
  spad.module.io.dma.req.bits.spbank := rs2.asTypeOf(rs_translate).spbank
  spad.module.io.dma.req.bits.spaddr := rs2.asTypeOf(rs_translate).spaddr

  spad.module.io.dma.req.valid := false.B
  spad.module.io.dma.req.bits.write := false.B
  // TODO: spad.module.io.dma.req.valid should be asserted before waiting for ready (potential deadlock)
  when (perform_load && spad.module.io.dma.req.ready){
    spad.module.io.dma.req.valid := true.B
    spad.module.io.dma.req.bits.write := false.B
  }
  when (perform_load && spad.module.io.dma.resp.valid) {
    cmd.ready := true.B
  }

  when (perform_store && spad.module.io.dma.req.ready) {
    spad.module.io.dma.req.valid := true.B
    spad.module.io.dma.req.bits.write := true.B
  }
  when (perform_store && spad.module.io.dma.resp.valid) { // assumes no page faults occur
    cmd.ready := true.B
  }
}

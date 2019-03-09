package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._

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
  val a_address_rs1 = Reg(UInt(xLen.W))
  val b_address_rs2 = Reg(UInt(xLen.W))

  val d_address_rs1 = cmd.bits.rs1 //SRAM_D_Address
  val c_address_rs2 = cmd.bits.rs2 //SRAM_C_Address

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
  val decode :: compute ::flush :: Nil = Enum(3)
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
  val a_read_bank_number = a_address_rs1(rs1.getWidth - 1, rs1.getWidth-log2Ceil(sp_banks))
  val b_read_bank_number = b_address_rs2(rs2.getWidth - 1, rs2.getWidth-log2Ceil(sp_banks))
  val d_read_bank_number = d_address_rs1(d_address_rs1.getWidth - 1, d_address_rs1.getWidth-log2Ceil(sp_banks))

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

  val fired_all_rows = WireInit(false.B)
  val fire_counter = new Counter(block_size)
  val output_counter = new Counter(block_size)
  val in_s = RegInit(0.U)

  for(i <- 0 until sp_banks){
    spad.module.io.read(i).en := start_sram_feeding &&
      ((a_read_bank_number === i.U && !perform_single_preload) ||
       (b_read_bank_number === i.U && !perform_single_preload) ||
       (d_read_bank_number === i.U && !preload_zeros))
    spad.module.io.read(i).addr := MuxCase(0.U,
      Seq(
        (a_read_bank_number === i.U && !perform_single_preload) -> (a_address_rs1 + fire_counter.value),
        (b_read_bank_number === i.U && !perform_single_preload) -> (b_address_rs2 + fire_counter.value),
        (d_read_bank_number === i.U && !preload_zeros) -> (d_address_rs1+fire_counter.value)
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
      when(cmd.valid && DoPreLoad){
        perform_single_preload := true.B
        start_sram_feeding := true.B
      }
      when(cmd.valid && (DoComputeAndFlip || DoComputeAndStay)){
        a_address_rs1 := rs1
        b_address_rs2 := rs2
        in_s := Mux(DoComputeAndFlip,~in_s,in_s)
        control_state := compute
        cmd.ready := true.B
      }
    }
    is(compute){
      when(cmd.valid && !DoPreLoad) {
        perform_single_mul := true.B
        start_sram_feeding := true.B
        preload_zeros := true.B
        when(fired_all_rows){
          control_state := flush
          meshIO.io.flush.valid := true.B
        }
      }
      when(cmd.valid && DoPreLoad) {
        start_sram_feeding := true.B
        perform_mul_pre := true.B
        when(fired_all_rows){
          cmd.ready := true.B
        }
      }
    }
    is(flush){
      when(meshIO.io.flush.ready){
        control_state := decode
      }
    }
  }

  when(perform_mul_pre){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fired_all_rows := fire_counter.inc()
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
      meshIO.io.tag_in.bits := c_address_rs2 //if this is 0xFFFFFF then don't output
      meshIO.io.s.bits := in_s
    }
  }

  when(perform_single_mul){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fired_all_rows := fire_counter.inc()

      meshIO.io.a.valid := true.B
      meshIO.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
      meshIO.io.b.valid := true.B
      meshIO.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
      meshIO.io.d.valid := true.B
      meshIO.io.d.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))

      meshIO.io.tag_in.valid := true.B
      meshIO.io.s.valid := true.B
      meshIO.io.tag_in.bits := 0xFFFFFFFFL.U //if this is 0xFFFFFF then don't output
      meshIO.io.s.bits := in_s
    }
  }

  when(perform_single_preload){
    when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready) {
      fired_all_rows := fire_counter.inc()
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
      meshIO.io.tag_in.bits := c_address_rs2 //if this is 0xFFFFFF then don't output
      meshIO.io.s.bits := in_s
    }

    when(fired_all_rows) {
      cmd.ready := true.B
    }
  }

  meshIO.io.out.ready := true.B

  val w_address = meshIO.io.tag_out
  val w_bank_number = w_address(w_address.getWidth-1,w_address.getWidth-log2Ceil(sp_banks))
  val w_bank_address = w_address(w_address.getWidth-log2Ceil(sp_banks)-1,0)

  for(i <- 0 until sp_banks){
    spad.module.io.write(i).en := start_array_outputting && w_bank_number === i.U
    spad.module.io.write(i).addr := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> w_bank_address))
    spad.module.io.write(i).data := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> meshIO.io.out.bits.asUInt()))
  }

  when(meshIO.io.out.fire() && !meshIO.io.tag_out===0xFFFFFFFFL.U) {
    start_array_outputting := true.B
  }

  spad.module.io.dma.resp.ready := true.B // The controller discards DMA responses
  // For both mvin and mvout, rs1 = DRAM address, rs2 = scratchpad address
  spad.module.io.dma.req.bits.vaddr := rs1
  spad.module.io.dma.req.bits.spbank := rs2(rs2.getWidth-1,rs2.getWidth-log2Ceil(sp_banks))
  spad.module.io.dma.req.bits.spaddr := (rs2 >> log2Ceil(sp_width/8))(log2Ceil(sp_bank_entries)-1,0)

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

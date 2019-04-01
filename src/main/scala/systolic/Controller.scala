package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import SystolicISA._
import Util._

case class SystolicArrayConfig(
  tileRows: Int,
  tileColumns: Int,
  meshRows: Int,
  meshColumns: Int,
  queue_length: Int,
  sp_banks: Int,
  sp_bank_entries: Int,
  sp_width: Int,
  shifter_banks: Int
)
case object SystolicArrayKey extends Field[SystolicArrayConfig]

class SystolicCmdWithDependencies(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val pushStore = Bool()
  val pushLoad = Bool()
  val pushEx = Bool()
  val pullStore = Bool()
  val pullLoad = Bool()
  val pullEx = Bool()
}

class SPAddr(xLen: Int, sp_banks: Int, sp_bank_entries: Int) extends Bundle {
  val junk = UInt((xLen-log2Ceil(sp_bank_entries)-log2Ceil(sp_banks)).W)
  val spbank = UInt(log2Ceil(sp_banks).W)
  val sprow = UInt(log2Ceil(sp_bank_entries).W)

  override def cloneType: SPAddr.this.type = new SPAddr(xLen, sp_banks, sp_bank_entries).asInstanceOf[this.type]
}

class SystolicArray[T <: Data: Arithmetic](dtype: T, opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC (
    opcodes = OpcodeSet.custom3,
    nPTWPorts = 1) {
  val config = p(SystolicArrayKey)
  val spad = LazyModule(new Scratchpad(
    config.sp_banks, config.sp_bank_entries, config.sp_width))
  override lazy val module = new SystolicArrayModule(this, dtype)
  override val tlNode = spad.node
}

// TODO add WS support
class SystolicArrayModule[T <: Data: Arithmetic]
    (outer: SystolicArray[T], val inner_type: T)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  // Parameters
  val block_size = meshRows*tileRows
  assert(meshRows*tileRows == meshColumns*tileColumns) // TODO remove when square requirement is lifted

  val tag_garbage = Cat(Seq.fill(xLen)(1.U(1.W)))

  val spaddr = new SPAddr(xLen, sp_banks, sp_bank_entries)

  val cmd = MultiHeadedQueue(io.cmd, queue_length, 4)
  cmd.pop := 0.U
  io.busy := false.B // cmd.valid(0) // TODO
  // io.cmd.valid implies io.cmd.bits.inst.xd = 0
  /*assert(!io.cmd.valid(0) || io.cmd.bits.inst.xd === false.B,
    "This controller doesn't support rd instructions due to unconnected RoCC resp")*/ // TODO

  // Aliases of cmds
  val current_dataflow = RegInit(Dataflow.OS.id.U)

  val rs1 = cmd.bits(0).rs1
  val rs2 = cmd.bits(0).rs2
  val funct = cmd.bits(0).inst.funct

  val DoLoad = funct === LOAD_CMD
  val DoStore = funct === STORE_CMD
  val DoSetMode = funct === MODE_CMD
  // TODO clean these names up. Maybe use a function instead
  val DoComputeAndFlip = funct === COMPUTE_AND_FLIP_CMD
  val DoComputeAndStay = funct === COMPUTE_AND_STAY_CMD
  val DoComputeSecond = cmd.bits(1).inst.funct === COMPUTE_AND_FLIP_CMD || cmd.bits(1).inst.funct === COMPUTE_AND_STAY_CMD
  val DoComputeThird = cmd.bits(2).inst.funct === COMPUTE_AND_FLIP_CMD || cmd.bits(2).inst.funct === COMPUTE_AND_STAY_CMD
  val DoPreload = funct === PRELOAD_CMD
  val DoPreloadSecond = cmd.bits(1).inst.funct === PRELOAD_CMD
  val DoPreloadThird = cmd.bits(2).inst.funct === PRELOAD_CMD
  val DoPreloadFourth = cmd.bits(3).inst.funct === PRELOAD_CMD

  val preload_cmd_place = Mux(DoPreload, 0.U, 1.U)

  // SRAM addresses of matmul operands
  val a_address_rs1 = WireInit(cmd.bits(0).rs1.asTypeOf(spaddr))
  val b_address_rs2 = WireInit(cmd.bits(0).rs2.asTypeOf(spaddr))
  val d_address_rs1 = WireInit(cmd.bits(preload_cmd_place).rs1.asTypeOf(spaddr))
  val c_address_rs2 = WireInit(cmd.bits(preload_cmd_place).rs2.asTypeOf(spaddr))

  val preload_zeros = WireInit(d_address_rs1.asUInt() === tag_garbage)

  // Instantiate the actual mesh
  val meshIO = Module(new MeshWithMemory(inner_type, xLen, Dataflow.BOTH, tileRows,
    tileColumns, meshRows, meshColumns, shifter_banks, shifter_banks))

  meshIO.io.a.valid := false.B
  meshIO.io.b.valid := false.B
  meshIO.io.d.valid := false.B
  meshIO.io.tag_in.valid := false.B
  meshIO.io.flush.valid := false.B

  meshIO.io.a.bits := DontCare
  meshIO.io.b.bits := DontCare
  meshIO.io.d.bits := DontCare
  meshIO.io.tag_in.bits := DontCare
  meshIO.io.s := DontCare
  meshIO.io.m := current_dataflow

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
  assert(sp_width == meshRows*tileRows*inner_type.getWidth)
  assert(sp_bank_entries % block_size == 0)
  assert(block_size == meshRows*tileRows)

  //SRAM scratchpad
  val a_read_bank_number = a_address_rs1.spbank
  val b_read_bank_number = b_address_rs2.spbank
  val d_read_bank_number = d_address_rs1.spbank

  val start_inputting_ab = WireInit(false.B)
  val start_inputting_d = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)

  val perform_store = WireInit(false.B)
  val perform_load = WireInit(false.B)
  val perform_single_preload = RegInit(false.B)
  val perform_single_mul = RegInit(false.B)
  val perform_mul_pre = RegInit(false.B)

  val fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))
  val fire_count_started = RegInit(false.B)
  val fired_all_rows = fire_counter === 0.U && fire_count_started
  val keep_firing = WireInit(false.B)
  val output_counter = new Counter(block_size)

  fire_counter := 0.U
  when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready &&
    (start_inputting_ab || start_inputting_d)) {
    fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
  }

  val in_s = DoComputeAndFlip

  // Scratchpad reads
  for(i <- 0 until sp_banks){
    spad.module.io.read(i).en :=
      ((a_read_bank_number === i.U && start_inputting_ab) ||
       (b_read_bank_number === i.U && start_inputting_ab) ||
       (d_read_bank_number === i.U && start_inputting_d && !preload_zeros))
    spad.module.io.read(i).addr := MuxCase(0.U,
      Seq(
        (a_read_bank_number === i.U && start_inputting_ab) -> (a_address_rs1.sprow + fire_counter),
        (b_read_bank_number === i.U && start_inputting_ab) -> (b_address_rs2.sprow + fire_counter),
        (d_read_bank_number === i.U && start_inputting_d) -> (d_address_rs1.sprow + block_size.U - 1.U - fire_counter)
      )
    )
  }

  val readData = VecInit(spad.module.io.read.map(_.data))

  val dataAbank = WireInit(a_read_bank_number)
  val dataBbank = WireInit(b_read_bank_number)
  val dataDbank = WireInit(d_read_bank_number)

  when (perform_single_preload) {
    dataDbank := cmd.bits(0).rs1.asTypeOf(spaddr).spbank
  }.elsewhen (perform_mul_pre) {
    dataAbank := cmd.bits(0).rs1.asTypeOf(spaddr).spbank
    dataBbank := cmd.bits(0).rs2.asTypeOf(spaddr).spbank
    dataDbank := cmd.bits(1).rs1.asTypeOf(spaddr).spbank
  }.elsewhen (perform_single_mul) {
    dataAbank := cmd.bits(0).rs1.asTypeOf(spaddr).spbank
    dataBbank := cmd.bits(0).rs2.asTypeOf(spaddr).spbank
  }

  val dataA = readData(dataAbank)
  val dataB = readData(dataBbank)
  val dataD = Mux(RegNext(preload_zeros), 0.U, readData(dataDbank))

  // FSM logic
  switch(control_state){
    is (decode) {
      when(cmd.valid(0)) {
        when(DoStore) {
          perform_store := true.B
        }

        .elsewhen(DoLoad) {
          perform_load := true.B
        }

        .elsewhen(DoSetMode) {
          val data_mode = rs1(0) // 0 is output stationary, 1 is weight
          current_dataflow := data_mode
          cmd.pop := 1.U
        }

        .elsewhen(DoPreload) {
          perform_single_preload := true.B
          start_inputting_d := true.B
          fire_count_started := true.B
          control_state := compute
        }

        assert(!DoComputeAndStay && !DoComputeAndFlip, "compute came before preload")
      }
    }
    is (compute) {
      when (cmd.valid(0)) {
        // Only preloading
        when(DoPreload) {
          start_inputting_d := true.B

          when(fired_all_rows) {
            perform_single_preload := false.B

            start_inputting_d := false.B

            fire_count_started := false.B
            cmd.pop := 1.U

            // Can we immediately launch into a mulpre?
            when(cmd.valid(1) && DoComputeSecond && cmd.valid(2) && DoPreloadThird) {
              start_inputting_ab := true.B
              start_inputting_d := true.B

              perform_mul_pre := true.B

              a_address_rs1 := cmd.bits(1).rs1.asTypeOf(spaddr)
              b_address_rs2 := cmd.bits(1).rs2.asTypeOf(spaddr)
              d_address_rs1 := cmd.bits(2).rs1.asTypeOf(spaddr)
              c_address_rs2 := cmd.bits(2).rs2.asTypeOf(spaddr)
            }
            // Can we immediately launch into a mul?
            .elsewhen(cmd.valid(1) && DoComputeSecond && cmd.valid(2) && !DoPreloadThird) {
              start_inputting_ab := true.B
              preload_zeros := true.B

              perform_single_mul := true.B

              a_address_rs1 := cmd.bits(1).rs1.asTypeOf(spaddr)
              b_address_rs2 := cmd.bits(1).rs2.asTypeOf(spaddr)
            }
          }
        }

        // Overlapping
        .elsewhen(cmd.valid(1) && DoPreloadSecond && !perform_single_mul) {
          perform_mul_pre := true.B
          start_inputting_ab := true.B
          start_inputting_d := true.B

          fire_count_started := true.B

          when(fired_all_rows) {
            perform_mul_pre := false.B

            start_inputting_ab := false.B
            start_inputting_d := false.B

            fire_count_started := false.B
            cmd.pop := 2.U

            // Can we immediately launch into a mulpre?
            when(cmd.valid(2) && DoComputeThird && cmd.valid(3) && DoPreloadFourth) {
              start_inputting_ab := true.B
              start_inputting_d := true.B

              perform_mul_pre := true.B

              a_address_rs1 := cmd.bits(2).rs1.asTypeOf(spaddr)
              b_address_rs2 := cmd.bits(2).rs2.asTypeOf(spaddr)
              d_address_rs1 := cmd.bits(3).rs1.asTypeOf(spaddr)
              c_address_rs2 := cmd.bits(3).rs2.asTypeOf(spaddr)
            }
            // Can we immediately launch into a mul?
            .elsewhen(cmd.valid(2) && DoComputeThird && cmd.valid(3) && !DoPreloadFourth) {
              start_inputting_ab := true.B
              preload_zeros := true.B

              perform_single_mul := true.B

              a_address_rs1 := cmd.bits(2).rs1.asTypeOf(spaddr)
              b_address_rs2 := cmd.bits(2).rs2.asTypeOf(spaddr)
            }
          }
        }

        // Only compute
        .elsewhen(cmd.valid(1) && !DoPreloadSecond) {
        // .otherwise {
          perform_single_mul := true.B
          start_inputting_ab := true.B
          start_inputting_d := false.B
          preload_zeros := true.B

          fire_count_started := true.B

          when(fired_all_rows) {
            perform_single_mul := false.B

            start_inputting_ab := false.B

            fire_count_started := false.B
            cmd.pop := 1.U
            control_state := flush
          }
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

  // Computing logic
  when(perform_mul_pre){
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.tag_in.valid := true.B
    meshIO.io.tag_in.bits := c_address_rs2.asUInt() //if this is 0xFFFFFF then don't output
    meshIO.io.s := in_s
  }

  when(perform_single_mul){
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.tag_in.valid := true.B
    meshIO.io.tag_in.bits := tag_garbage //if this is 0xFFFFFF then don't output
    meshIO.io.s := in_s
  }

  when(perform_single_preload){
    meshIO.io.a.valid := true.B
    meshIO.io.a.bits := (0.U).asTypeOf(Vec(meshRows, Vec(tileRows, inner_type)))
    meshIO.io.b.valid := true.B
    meshIO.io.b.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.d.valid := true.B
    meshIO.io.d.bits := dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inner_type)))
    meshIO.io.tag_in.valid := true.B
    meshIO.io.tag_in.bits := c_address_rs2.asUInt() //if this is 0xFFFFFF then don't output
    meshIO.io.s := in_s
  }

  // Scratchpad writes
  val w_address = meshIO.io.tag_out.asTypeOf(spaddr)
  val w_bank_number = w_address.spbank
  val w_bank_address = w_address.sprow

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
  spad.module.io.dma.req.bits.spbank := rs2.asTypeOf(spaddr).spbank
  spad.module.io.dma.req.bits.spaddr := rs2.asTypeOf(spaddr).sprow

  spad.module.io.dma.req.valid := false.B
  spad.module.io.dma.req.bits.write := false.B
  // TODO: spad.module.io.dma.req.valid should be asserted before waiting for ready (potential deadlock)
  when (perform_load && spad.module.io.dma.req.ready){
    spad.module.io.dma.req.valid := true.B
    spad.module.io.dma.req.bits.write := false.B
  }
  when (perform_load && spad.module.io.dma.resp.valid) {
    cmd.pop := 1.U
  }

  when (perform_store && spad.module.io.dma.req.ready) {
    spad.module.io.dma.req.valid := true.B
    spad.module.io.dma.req.bits.write := true.B
  }
  when (perform_store && spad.module.io.dma.resp.valid) { // assumes no page faults occur
    cmd.pop := 1.U
  }
}

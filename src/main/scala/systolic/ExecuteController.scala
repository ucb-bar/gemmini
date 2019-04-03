package systolic

import chisel3._
import chisel3.util._
import SystolicISA._
import Util._
import freechips.rocketchip.config.Parameters

class ExecuteController[T <: Data: Arithmetic](xLen: Int, config: SystolicArrayConfig, spaddr: SPAddr, inner_type: T)
                                              (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDependencies))

    val read  = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)))
    val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, sp_width)))

    // TODO what's a better way to express no bits?
    val pushLoad = Decoupled(UInt(1.W))
    val pullLoad = Flipped(Decoupled(UInt(1.W)))
    val pushStore = Decoupled(UInt(1.W))
    val pullStore = Flipped(Decoupled(UInt(1.W)))
  })

  val block_size = meshRows*tileRows
  val tag_garbage = Cat(Seq.fill(xLen)(1.U(1.W)))

  val cmd = MultiHeadedQueue(io.cmd, queue_length, 4)
  cmd.pop := 0.U

  val current_dataflow = RegInit(Dataflow.OS.id.U)

  val functs = cmd.bits.map(_.cmd.inst.funct)
  val rs1s = VecInit(cmd.bits.map(_.cmd.rs1))
  val rs2s = VecInit(cmd.bits.map(_.cmd.rs2))

  val DoSetMode = functs(0) === MODE_CMD
  val DoComputeAndFlip = functs(0) === COMPUTE_AND_FLIP_CMD
  val DoComputeAndStay = functs(0) === COMPUTE_AND_STAY_CMD
  val DoComputes = functs.map(f => f === COMPUTE_AND_FLIP_CMD || f === COMPUTE_AND_STAY_CMD)
  val DoPreloads = functs.map(_ === PRELOAD_CMD)

  val preload_cmd_place = Mux(DoPreloads(0), 0.U, 1.U)

  val in_s = DoComputeAndFlip

  // SRAM addresses of matmul operands
  val a_address_rs1 = WireInit(rs1s(0).asTypeOf(spaddr))
  val b_address_rs2 = WireInit(rs2s(0).asTypeOf(spaddr))
  val d_address_rs1 = WireInit(rs1s(preload_cmd_place).asTypeOf(spaddr))
  val c_address_rs2 = WireInit(rs2s(preload_cmd_place).asTypeOf(spaddr))

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
  val waiting_for_preload :: compute :: flush :: flushing :: Nil = Enum(4)
  val control_state = RegInit(waiting_for_preload)

  //SRAM scratchpad
  val a_read_bank_number = a_address_rs1.spbank
  val b_read_bank_number = b_address_rs2.spbank
  val d_read_bank_number = d_address_rs1.spbank

  val start_inputting_ab = WireInit(false.B)
  val start_inputting_d = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)

  val perform_single_preload = RegInit(false.B)
  val perform_single_mul = RegInit(false.B)
  val perform_mul_pre = RegInit(false.B)

  val fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))
  val fire_count_started = RegInit(false.B)
  val fired_all_rows = fire_counter === 0.U && fire_count_started
  val output_counter = new Counter(block_size)

  fire_counter := 0.U
  when(meshIO.io.a.ready && meshIO.io.b.ready && meshIO.io.d.ready &&
    (start_inputting_ab || start_inputting_d)) {
    fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
  }

  // Scratchpad reads
  for(i <- 0 until sp_banks){
    io.read(i).en :=
      ((a_read_bank_number === i.U && start_inputting_ab) ||
        (b_read_bank_number === i.U && start_inputting_ab) ||
        (d_read_bank_number === i.U && start_inputting_d && !preload_zeros))
    io.read(i).addr := MuxCase(0.U,
      Seq(
        (a_read_bank_number === i.U && start_inputting_ab) -> (a_address_rs1.sprow + fire_counter),
        (b_read_bank_number === i.U && start_inputting_ab) -> (b_address_rs2.sprow + fire_counter),
        (d_read_bank_number === i.U && start_inputting_d) -> (d_address_rs1.sprow + block_size.U - 1.U - fire_counter)
      )
    )
  }

  val readData = VecInit(io.read.map(_.data))

  val dataAbank = WireInit(a_read_bank_number)
  val dataBbank = WireInit(b_read_bank_number)
  val dataDbank = WireInit(d_read_bank_number)

  when (perform_single_preload) {
    dataDbank := rs1s(0).asTypeOf(spaddr).spbank
  }.elsewhen (perform_mul_pre) {
    dataAbank := rs1s(0).asTypeOf(spaddr).spbank
    dataBbank := rs2s(0).asTypeOf(spaddr).spbank
    dataDbank := rs1s(0).asTypeOf(spaddr).spbank
  }.elsewhen (perform_single_mul) {
    dataAbank := rs1s(0).asTypeOf(spaddr).spbank
    dataBbank := rs2s(0).asTypeOf(spaddr).spbank
  }

  val dataA = readData(dataAbank)
  val dataB = readData(dataBbank)
  val dataD = Mux(RegNext(preload_zeros), 0.U, readData(dataDbank))

  // FSM logic
  switch(control_state){
    is (waiting_for_preload) {
      when(cmd.valid(0)) {

        when(DoSetMode) {
          val data_mode = rs1s(0)(0)
          current_dataflow := data_mode
          cmd.pop := 1.U
        }

        .elsewhen(DoPreloads(0)) {
          perform_single_preload := true.B
          start_inputting_d := true.B
          fire_count_started := true.B
          control_state := compute
        }

        assert(!DoComputes(0), "compute came before preload")
      }
    }
    is (compute) {
      when (cmd.valid(0)) {
        // Only preloading
        when(DoPreloads(0)) {
          start_inputting_d := true.B

          when(fired_all_rows) {
            perform_single_preload := false.B

            start_inputting_d := false.B

            fire_count_started := false.B
            cmd.pop := 1.U

            // Can we immediately launch into a mulpre?
            when(cmd.valid(1) && DoComputes(1) && cmd.valid(2) && DoPreloads(2)) {
              start_inputting_ab := true.B
              start_inputting_d := true.B

              perform_mul_pre := true.B

              a_address_rs1 := rs1s(1).asTypeOf(spaddr)
              b_address_rs2 := rs2s(1).asTypeOf(spaddr)
              d_address_rs1 := rs1s(2).asTypeOf(spaddr)
              c_address_rs2 := rs2s(2).asTypeOf(spaddr)
            }
            // Can we immediately launch into a mul?
            .elsewhen(cmd.valid(1) && DoComputes(1) && cmd.valid(2) && !DoPreloads(2)) {
              start_inputting_ab := true.B
              preload_zeros := true.B

              perform_single_mul := true.B

              a_address_rs1 := rs1s(1).asTypeOf(spaddr)
              b_address_rs2 := rs2s(1).asTypeOf(spaddr)
            }
          }
        }

          // Overlapping
        .elsewhen(cmd.valid(1) && DoPreloads(1) && !perform_single_mul) {
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
            when(cmd.valid(2) && DoComputes(2) && cmd.valid(3) && DoPreloads(3)) {
              start_inputting_ab := true.B
              start_inputting_d := true.B

              perform_mul_pre := true.B

              a_address_rs1 := rs1s(2).asTypeOf(spaddr)
              b_address_rs2 := rs2s(2).asTypeOf(spaddr)
              d_address_rs1 := rs1s(3).asTypeOf(spaddr)
              c_address_rs2 := rs2s(3).asTypeOf(spaddr)
            }
            // Can we immediately launch into a mul?
            .elsewhen(cmd.valid(2) && DoComputes(2) && cmd.valid(3) && !DoPreloads(3)) {
              start_inputting_ab := true.B
              preload_zeros := true.B

              perform_single_mul := true.B

              a_address_rs1 := rs1s(2).asTypeOf(spaddr)
              b_address_rs2 := rs2s(2).asTypeOf(spaddr)
            }
          }
        }

        // Only compute
        .elsewhen(cmd.valid(1) && !DoPreloads(1)) {
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
        control_state := waiting_for_preload
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
    io.write(i).en := start_array_outputting && w_bank_number === i.U
    io.write(i).addr := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> (w_bank_address + block_size.U - 1.U - output_counter.value)))
    io.write(i).data := MuxCase(0.U,
      Seq((w_bank_number === i.U && start_array_outputting) -> meshIO.io.out.bits.asUInt()))
  }

  when(meshIO.io.out.fire() && meshIO.io.tag_out =/= tag_garbage) {
    output_counter.inc()
    start_array_outputting := true.B
  }
}

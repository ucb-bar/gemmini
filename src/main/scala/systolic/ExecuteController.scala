package systolic

import chisel3._
import chisel3.util._
import SystolicISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO handle reads from the same bank
// TODO don't flush all 4 time steps when shorter flushes will work
class ExecuteController[T <: Data](xLen: Int, tagWidth: Int, config: SystolicArrayConfig, sp_addr: SPAddr, acc_addr: AccAddr,
                                   inputType: T, outputType: T, accType: T)
                                              (implicit p: Parameters, ev: Arithmetic[T]) extends Module {
  import config._
  import ev._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new SystolicCmdWithDeps))

    val read  = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width))
    val write = Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, sp_width))
    val acc = Flipped(new AccumulatorMemIO(acc_rows, Vec(meshColumns, Vec(tileColumns, accType)), Vec(meshColumns, Vec(tileColumns, inputType))))

    // TODO what's a better way to express no bits?
    val pushLoad = Decoupled(UInt(1.W))
    val pullLoad = Flipped(Decoupled(UInt(1.W)))
    val pushStore = Decoupled(UInt(1.W))
    val pullStore = Flipped(Decoupled(UInt(1.W)))

    val pushLoadLeft = Input(UInt(log2Ceil(depq_len+1).W))
    val pushStoreLeft = Input(UInt(log2Ceil(depq_len+1).W))

    val busy = Output(Bool())
  })

  val block_size = meshRows*tileRows

  val tag_garbage = Cat(Seq.fill(tagWidth)(1.U(1.W)))
  val tag_with_deps = new Bundle {
    val pushLoad = Bool()
    val pushStore = Bool()
    val tag = UInt(tagWidth.W)
  }

  val cmd_q_heads = 3
  assert(ex_queue_length >= cmd_q_heads)
  val (cmd, _) = MultiHeadedQueue(io.cmd, ex_queue_length, cmd_q_heads)
  cmd.pop := 0.U

  io.busy := cmd.valid(0)

  val current_dataflow = if (dataflow == Dataflow.BOTH) Reg(UInt(1.W)) else dataflow.id.U

  val functs = cmd.bits.map(_.cmd.inst.funct)
  val rs1s = VecInit(cmd.bits.map(_.cmd.rs1(tagWidth-1, 0)))
  val rs2s = VecInit(cmd.bits.map(_.cmd.rs2(tagWidth-1, 0)))

  val DoConfig = functs(0) === CONFIG_CMD
  val DoComputes = functs.map(f => f === COMPUTE_AND_FLIP_CMD || f === COMPUTE_AND_STAY_CMD)
  val DoPreloads = functs.map(_ === PRELOAD_CMD)

  val preload_cmd_place = Mux(DoPreloads(0), 0.U, 1.U)
  val a_address_place = Mux(current_dataflow === Dataflow.WS.id.U, 0.U, Mux(preload_cmd_place === 0.U, 1.U, 2.U))

  val in_s = functs(0) === COMPUTE_AND_FLIP_CMD
  val in_s_flush = Reg(Bool())
  when (current_dataflow === Dataflow.WS.id.U) {
    in_s_flush := 0.U
  }

  val in_shift = Reg(UInt(log2Ceil(accType.getWidth - inputType.getWidth + 1).W))
  val activation = Reg(UInt(2.W))

  // SRAM addresses of matmul operands
  val a_address_rs1 = WireInit(rs1s(a_address_place).asTypeOf(sp_addr))
  val b_address_rs2 = WireInit(rs2s(0).asTypeOf(sp_addr))
  val d_address_rs1 = WireInit(rs1s(preload_cmd_place).asTypeOf(sp_addr))
  val c_address_rs2 = WireInit(rs2s(preload_cmd_place).asTypeOf(sp_addr))

  val preload_zeros = WireInit(d_address_rs1.asUInt()(tagWidth-1, 0) === tag_garbage)
  val accumulate_zeros = WireInit(b_address_rs2.asUInt()(tagWidth-1, 0) === tag_garbage)

  // Dependency stuff
  val pushLoads = cmd.bits.map(_.deps.pushLoad)
  val pullLoads = cmd.bits.map(_.deps.pullLoad)
  val pushStores = cmd.bits.map(_.deps.pushStore)
  val pullStores = cmd.bits.map(_.deps.pullStore)
  val pushDeps = (pushLoads zip pushStores).map { case (pl, ps) => pl || ps }
  val pullDeps = (pullLoads zip pullStores).map { case (pl, ps) => pl || ps }

  val pull_deps_ready = (pullDeps, pullLoads, pullStores).zipped.map { case (pullDep, pullLoad, pullStore) =>
    !pullDep || (pullLoad && !pullStore && io.pullLoad.valid) ||
      (pullStore && !pullLoad && io.pullStore.valid) ||
      (pullLoad && pullStore && io.pullLoad.valid && io.pullStore.valid)
  }

  val push_deps_ready = (pushDeps, pushLoads, pushStores).zipped.map { case (pushDep, pushLoad, pushStore) =>
    !pushDep || (pushLoad && !pushStore && io.pushLoadLeft >= 3.U) ||
      (pushStore && !pushLoad && io.pushStoreLeft >= 3.U) ||
      (pushStore && pushLoad && io.pushLoadLeft >= 3.U && io.pushStoreLeft >= 3.U)
  }

  io.pushLoad.valid := false.B
  io.pushLoad.bits := DontCare
  io.pushStore.valid := false.B
  io.pushStore.bits := DontCare

  io.pullLoad.ready := false.B
  io.pullStore.ready := false.B

  // Instantiate the actual mesh
  val mesh = Module(new MeshWithDelays(inputType, outputType, accType, tag_with_deps, dataflow, tileRows,
    tileColumns, meshRows, meshColumns, shifter_banks, shifter_banks))

  mesh.io.a.valid := false.B
  mesh.io.b.valid := false.B
  mesh.io.d.valid := false.B
  mesh.io.tag_in.valid := false.B
  mesh.io.tag_garbage.pushLoad := false.B
  mesh.io.tag_garbage.pushStore := false.B
  mesh.io.tag_garbage.tag := tag_garbage
  mesh.io.flush.valid := false.B

  mesh.io.a.bits := DontCare
  mesh.io.b.bits := DontCare
  mesh.io.d.bits := DontCare
  mesh.io.tag_in.bits := DontCare
  mesh.io.s := in_s
  mesh.io.m := current_dataflow
  mesh.io.shift := in_shift
  mesh.io.flush.bits := 0.U

  // Hazards
  val raw_hazard_pre = mesh.io.tags_in_progress.map { t =>
    val is_garbage = t.tag === tag_garbage
    val pre_raw_haz = t.tag === rs1s(0)(tagWidth-1, 0)
    val mul_raw_haz = t.tag === rs1s(1)(tagWidth-1, 0) || t.tag === rs2s(1)(tagWidth-1, 0)

    !is_garbage && (pre_raw_haz || mul_raw_haz)
  }.reduce(_ || _)

  val raw_hazard_mulpre = mesh.io.tags_in_progress.map { t =>
    val is_garbage = t.tag === tag_garbage
    val pre_raw_haz = t.tag === rs1s(1)(tagWidth-1, 0)
    val mul_raw_haz = t.tag === rs1s(2)(tagWidth-1, 0) || t.tag === rs2s(2)(tagWidth-1, 0)

    !is_garbage && (mul_raw_haz || pre_raw_haz)
  }.reduce(_ || _)

  val matmul_in_progress = mesh.io.tags_in_progress.map(_.tag =/= tag_garbage).reduce(_ || _)

  // STATE defines
  val waiting_for_cmd :: compute :: flush :: flushing :: Nil = Enum(4)
  val control_state = RegInit(waiting_for_cmd)

  // SRAM scratchpad
  val dataAbank = a_address_rs1.bank
  val dataBbank = b_address_rs2.bank
  val dataDbank = d_address_rs1.bank

  val a_read_from_acc = a_address_rs1.asTypeOf(acc_addr).is_acc_addr
  val b_read_from_acc = b_address_rs2.asTypeOf(acc_addr).is_acc_addr
  val d_read_from_acc = d_address_rs1.asTypeOf(acc_addr).is_acc_addr

  val start_inputting_a = WireInit(false.B)
  val start_inputting_b = WireInit(false.B)
  val start_inputting_d = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)

  val perform_single_preload = RegInit(false.B)
  val perform_single_mul = RegInit(false.B)
  val perform_mul_pre = RegInit(false.B)

  val output_counter = new Counter(block_size)

  // Fire counters which resolve same-bank accesses
  val a_fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))
  val b_fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))
  val d_fire_counter = Reg(UInt((log2Ceil(block_size) max 1).W))

  def same_bank(addr1: SPAddr, addr2: SPAddr, start_inputting1: Bool, start_inputting2: Bool): Bool = {
    val addr1_read_from_acc = addr1.asTypeOf(acc_addr).is_acc_addr
    val addr2_read_from_acc = addr2.asTypeOf(acc_addr).is_acc_addr

    val is_garbage = addr1.asUInt()(tagWidth-1, 0) === tag_garbage || addr2.asUInt()(tagWidth-1, 0) === tag_garbage ||
      !start_inputting1 || !start_inputting2

    !is_garbage && ((addr1_read_from_acc && addr2_read_from_acc) || (!addr1_read_from_acc && !addr2_read_from_acc && addr1.bank === addr2.bank))
  }

  // The priority scheme we follow is that A fires first, then B, then D
  val a_can_fire = a_fire_counter === b_fire_counter && a_fire_counter === d_fire_counter
  val b_can_fire = (!same_bank(a_address_rs1, b_address_rs2, start_inputting_a, start_inputting_b) || a_fire_counter =/= b_fire_counter) && b_fire_counter === d_fire_counter
  val d_can_fire =
    ((!same_bank(a_address_rs1, d_address_rs1, start_inputting_a, start_inputting_d) && !same_bank(b_address_rs2, d_address_rs1, start_inputting_b, start_inputting_d)) && (b_can_fire || b_fire_counter =/= d_fire_counter)) ||
      ((same_bank(a_address_rs1, d_address_rs1, start_inputting_a, start_inputting_d) && !same_bank(b_address_rs2, d_address_rs1, start_inputting_b, start_inputting_d)) && (a_fire_counter =/= d_fire_counter && (b_can_fire || b_fire_counter =/= d_fire_counter))) ||
      ((!same_bank(a_address_rs1, d_address_rs1, start_inputting_a, start_inputting_d) && same_bank(b_address_rs2, d_address_rs1, start_inputting_b, start_inputting_d)) && b_fire_counter =/= d_fire_counter) ||
      ((same_bank(a_address_rs1, d_address_rs1, start_inputting_a, start_inputting_d) && same_bank(b_address_rs2, d_address_rs1, start_inputting_b, start_inputting_d)) && b_fire_counter =/= d_fire_counter)

  val firing = start_inputting_a || start_inputting_b || start_inputting_d

  when (!firing) {
    a_fire_counter := 0.U
  }.elsewhen (a_can_fire && firing) {
    a_fire_counter := wrappingAdd(a_fire_counter, 1.U, block_size)
  }

  when (!firing) {
    b_fire_counter := 0.U
  }.elsewhen (b_can_fire && firing) {
    b_fire_counter := wrappingAdd(b_fire_counter, 1.U, block_size)
  }

  when (!firing) {
    d_fire_counter := 0.U
  }.elsewhen (d_can_fire && firing) {
    d_fire_counter := wrappingAdd(d_fire_counter, 1.U, block_size)
  }

  // The last line in this (long) Boolean is just to make sure that we don't think we're done as soon as we begin firing
  // TODO change when square requirement lifted
  val about_to_fire_all_rows = ((a_fire_counter === (block_size-1).U && a_can_fire) || a_fire_counter === 0.U) &&
                               ((b_fire_counter === (block_size-1).U && b_can_fire) || b_fire_counter === 0.U) &&
                               ((d_fire_counter === (block_size-1).U && d_can_fire) || d_fire_counter === 0.U) &&
                               (a_fire_counter =/= 0.U || b_fire_counter =/= 0.U || d_fire_counter =/= 0.U)

  // Scratchpad reads
  for (i <- 0 until sp_banks) {
    val read_a = a_can_fire && !a_read_from_acc && dataAbank === i.U && start_inputting_a
    val read_b = b_can_fire && !b_read_from_acc && dataBbank === i.U && start_inputting_b && !accumulate_zeros
    val read_d = d_can_fire && !d_read_from_acc && dataDbank === i.U && start_inputting_d && !preload_zeros

    io.read(i).en := read_a || read_b || read_d
    io.read(i).addr := MuxCase(a_address_rs1.row + a_fire_counter,
      Seq(read_b -> (b_address_rs2.row + b_fire_counter),
        read_d -> (d_address_rs1.row + block_size.U - 1.U - d_fire_counter)))
  }

  // Accumulator read // TODO can only handle one acc read for now
  {
    val read_a_from_acc = a_can_fire && a_read_from_acc && start_inputting_a
    val read_b_from_acc = b_can_fire && b_read_from_acc && start_inputting_b && !accumulate_zeros
    val read_d_from_acc = d_can_fire && d_read_from_acc && start_inputting_d && !preload_zeros

    io.acc.read.en := read_a_from_acc || read_b_from_acc || read_d_from_acc
    io.acc.read.shift := in_shift
    io.acc.read.act := activation

    io.acc.read.addr := MuxCase(a_address_rs1.asTypeOf(acc_addr).row + a_fire_counter,
      Seq(read_b_from_acc -> (b_address_rs2.asTypeOf(acc_addr).row + b_fire_counter),
        read_d_from_acc -> (d_address_rs1.asTypeOf(acc_addr).row + block_size.U - 1.U - d_fire_counter)))
  }

  val readData = VecInit(io.read.map(_.data))
  val accReadData = io.acc.read.data.asUInt()

  val dataA = Mux(RegNext(a_read_from_acc), accReadData, readData(RegNext(dataAbank)))
  val dataB = MuxCase(readData(RegNext(dataBbank)),
    Seq(RegNext(accumulate_zeros) -> 0.U, RegNext(b_read_from_acc) -> accReadData))
  val dataD = MuxCase(readData(RegNext(dataDbank)),
    Seq(RegNext(preload_zeros) -> 0.U, RegNext(d_read_from_acc) -> accReadData))

  // FSM logic
  switch (control_state) {
    is (waiting_for_cmd) {
      // Default state
      perform_single_preload := false.B
      perform_mul_pre := false.B
      perform_single_mul := false.B

      when(cmd.valid(0) && pull_deps_ready(0) && push_deps_ready(0))
      {
        when(DoConfig && !matmul_in_progress) {
          activation := rs1s(0)(4, 3)
          in_shift := rs2s(0)

          if (dataflow == Dataflow.BOTH)
            current_dataflow := rs1s(0)(2)

          io.pullLoad.ready := cmd.bits(0).deps.pullLoad
          io.pullStore.ready := cmd.bits(0).deps.pullStore
          io.pushLoad.valid := cmd.bits(0).deps.pushLoad
          io.pushStore.valid := cmd.bits(0).deps.pushStore

          cmd.pop := 1.U
        }

        // Preload
        .elsewhen(DoPreloads(0) && cmd.valid(1) && !raw_hazard_pre) {
          perform_single_preload := true.B
          start_inputting_a := current_dataflow === Dataflow.OS.id.U
          start_inputting_d := true.B

          io.pullLoad.ready := cmd.bits(0).deps.pullLoad
          io.pullStore.ready := cmd.bits(0).deps.pullStore

          when (current_dataflow === Dataflow.OS.id.U) {
            in_s_flush := rs2s(0)(tagWidth-1, 0) =/= tag_garbage
          }

          control_state := compute
        }

        // Overlap compute and preload
        .elsewhen(DoComputes(0) && cmd.valid(1) && DoPreloads(1) && pull_deps_ready(1) && push_deps_ready(1) && cmd.valid(2) && !raw_hazard_mulpre) {
          perform_mul_pre := true.B
          start_inputting_a := true.B
          start_inputting_b := true.B
          start_inputting_d := true.B

          io.pullLoad.ready := cmd.bits(1).deps.pullLoad
          io.pullStore.ready := cmd.bits(1).deps.pullStore

          when (current_dataflow === Dataflow.OS.id.U) {
            in_s_flush := rs2s(1)(tagWidth - 1, 0) =/= tag_garbage
          }

          control_state := compute
        }

        // Single mul
        .elsewhen(DoComputes(0)) {
          perform_single_mul := true.B
          start_inputting_a := current_dataflow === Dataflow.WS.id.U
          start_inputting_b := true.B

          control_state := compute
        }

        // Flush
        .elsewhen(matmul_in_progress) {
          control_state := flush
        }
      }.elsewhen(matmul_in_progress) {
        // TODO code duplication
        control_state := flush
      }
    }
    is (compute) {
      // Only preloading
      when(perform_single_preload) {
        start_inputting_a := current_dataflow === Dataflow.OS.id.U
        start_inputting_d := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 1.U
          control_state := waiting_for_cmd
        }
      }

      // Overlapping
      .elsewhen(perform_mul_pre)
      {
        start_inputting_a := true.B
        start_inputting_b := true.B
        start_inputting_d := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 2.U
          control_state := waiting_for_cmd
        }
      }

      // Only compute
      .elsewhen(perform_single_mul) {
        start_inputting_a := current_dataflow === Dataflow.WS.id.U
        start_inputting_b := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 1.U
          control_state := waiting_for_cmd
        }
      }
    }
    is (flush) {
      when(mesh.io.flush.ready) {
        mesh.io.flush.valid := true.B
        mesh.io.s := in_s_flush
        control_state := flushing
      }
    }
    is (flushing) {
      when(mesh.io.flush.ready) {
        // TODO we waste a cycle here if it was better to continue with the flush
        control_state := waiting_for_cmd
      }
    }
  }

  // Computing logic
  when (perform_mul_pre || perform_single_mul || perform_single_preload) {
    // Default inputs
    mesh.io.a.valid := RegNext(a_can_fire)
    mesh.io.b.valid := RegNext(b_can_fire)
    mesh.io.d.valid := RegNext(d_can_fire)
    mesh.io.tag_in.valid := true.B

    mesh.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
    mesh.io.d.bits := dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))

    mesh.io.tag_in.bits.pushLoad := VecInit(pushLoads take 2)(preload_cmd_place)
    mesh.io.tag_in.bits.pushStore := VecInit(pushStores take 2)(preload_cmd_place)
    mesh.io.tag_in.bits.tag := c_address_rs2.asUInt()
  }

  when (perform_single_preload) {
    mesh.io.a.bits := Mux(current_dataflow === Dataflow.WS.id.U, 0.U, dataA).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.b.bits := (0.U).asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
    mesh.io.s := RegNext(in_s_flush) // TODO create a new in_s_preload
  }

  when (perform_single_mul) {
    mesh.io.a.bits := Mux(current_dataflow === Dataflow.OS.id.U, 0.U, dataA).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.tag_in.bits.pushLoad := false.B
    mesh.io.tag_in.bits.pushStore := false.B
    mesh.io.tag_in.bits.tag := tag_garbage
  }

  // Scratchpad writes
  val w_address = mesh.io.tag_out.tag.asTypeOf(sp_addr)
  val w_address_acc = mesh.io.tag_out.tag.asTypeOf(acc_addr)

  val write_to_acc = w_address_acc.is_acc_addr

  val w_bank = w_address.bank
  val w_row = Mux(write_to_acc, w_address_acc.row, w_address.row)

  val current_w_bank_address = Mux(current_dataflow === Dataflow.WS.id.U, w_row + output_counter.value,
    w_row + block_size.U - 1.U - output_counter.value)

  val is_garbage_addr = mesh.io.tag_out.tag === tag_garbage

  // Write to normal scratchpad
  for(i <- 0 until sp_banks) {
    val activated_wdata = VecInit(mesh.io.out.bits.map(v => VecInit(v.map { e =>
      val e_clipped = e.clippedToWidthOf(inputType)
      val e_act = MuxCase(e_clipped, Seq(
        (activation === Activation.RELU) -> e_clipped.relu,
        (activation === Activation.RELU6) -> e_clipped.relu6))

      e_act
    })))

    io.write(i).en := start_array_outputting && w_bank === i.U && !write_to_acc && !is_garbage_addr
    io.write(i).addr := current_w_bank_address
    io.write(i).data := activated_wdata.asUInt()
  }

  // Write to accumulator
  {
    io.acc.write.en := start_array_outputting && write_to_acc && !is_garbage_addr
    io.acc.write.addr := current_w_bank_address
    io.acc.write.data := mesh.io.out.bits
    io.acc.write.acc := w_address_acc.acc
  }

  when(mesh.io.out.fire() && !is_garbage_addr) {
    when(output_counter.inc()) {
      io.pushLoad.valid := mesh.io.tag_out.pushLoad
      io.pushStore.valid := mesh.io.tag_out.pushStore

      assert(!mesh.io.tag_out.pushLoad || io.pushLoad.ready)
      assert(!mesh.io.tag_out.pushStore || io.pushStore.ready)
    }
    start_array_outputting := true.B
  }
}

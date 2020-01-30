package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO handle reads from the same bank
// TODO don't flush all 4 time steps when shorter flushes will work
class ExecuteController[T <: Data](xLen: Int, tagWidth: Int, config: GemminiArrayConfig[T])
                                  (implicit p: Parameters, ev: Arithmetic[T]) extends Module {
  import config._
  import ev._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmdWithDeps(rob_entries)))

    val read  = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width))
    val write = Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, sp_width, (sp_width / (aligned_to * 8)) max 1))
    val acc = Flipped(new AccumulatorMemIO(acc_rows, Vec(meshColumns, Vec(tileColumns, accType)), Vec(meshColumns, Vec(tileColumns, inputType))))

    val completed = Valid(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
  })

  val block_size = meshRows*tileRows

  val tag_with_deps = new Bundle with TagQueueTag {
    val rob_id = UDValid(UInt(log2Up(rob_entries).W))
    val addr = local_addr_t.cloneType

    override def make_this_garbage(dummy: Int = 0): Unit = {
      rob_id.valid := false.B
      addr.make_this_garbage()
    }
  }

  val cmd_q_heads = 3
  assert(ex_queue_length >= cmd_q_heads)
  val (cmd, _) = MultiHeadedQueue(io.cmd, ex_queue_length, cmd_q_heads)
  cmd.pop := 0.U

  io.busy := false.B // cmd.valid(0)

  // STATE defines
  val waiting_for_cmd :: compute :: flush :: flushing :: Nil = Enum(4)
  val control_state = RegInit(waiting_for_cmd)

  // Instruction-related variables
  val current_dataflow = if (dataflow == Dataflow.BOTH) Reg(UInt(1.W)) else dataflow.id.U

  val functs = cmd.bits.map(_.cmd.inst.funct)
  val rs1s = VecInit(cmd.bits.map(_.cmd.rs1(tagWidth-1, 0)))
  val rs2s = VecInit(cmd.bits.map(_.cmd.rs2(tagWidth-1, 0)))

  val DoConfig = functs(0) === CONFIG_CMD
  val DoComputes = functs.map(f => f === COMPUTE_AND_FLIP_CMD || f === COMPUTE_AND_STAY_CMD)
  val DoPreloads = functs.map(_ === PRELOAD_CMD)

  val preload_cmd_place = Mux(DoPreloads(0), 0.U, 1.U)
  val a_address_place = Mux(current_dataflow === Dataflow.WS.id.U, 0.U, Mux(preload_cmd_place === 0.U, 1.U, 2.U))

  val in_prop = functs(0) === COMPUTE_AND_FLIP_CMD
  val in_prop_flush = Reg(Bool())
  when (current_dataflow === Dataflow.WS.id.U) {
    in_prop_flush := 0.U
  }

  val in_shift = Reg(UInt(log2Up(accType.getWidth).W))
  val acc_shift = Reg(UInt(log2Up(accType.getWidth).W))
  val relu6_shift = Reg(UInt(log2Up(accType.getWidth).W))
  val activation = Reg(UInt(2.W))

  // SRAM addresses of matmul operands
  val a_address_rs1__ = WireInit(rs1s(a_address_place).asTypeOf(local_addr_t))
  val d_address_rs2__ = WireInit(rs2s(0).asTypeOf(local_addr_t))
  val b_address_rs1__ = WireInit(rs1s(preload_cmd_place).asTypeOf(local_addr_t))
  val c_address_rs2 = WireInit(rs2s(preload_cmd_place).asTypeOf(local_addr_t))

  val multiply_garbage = a_address_rs1__.is_garbage()
  val accumulate_zeros = d_address_rs2__.is_garbage()
  val preload_zeros = b_address_rs1__.is_garbage()

  // Dependency stuff
  io.completed.valid := false.B
  io.completed.bits := DontCare

  val pending_completed_rob_id = Reg(UDValid(UInt(log2Up(rob_entries).W)))

  // Instantiate a queue which queues up signals which must be fed into the mesh
  val mesh_cntl_signals_q = Module(new Queue(new ComputeCntlSignals, mem_pipeline+1,
    pipe=true))

  val cntl_ready = mesh_cntl_signals_q.io.enq.ready
  val cntl_valid = mesh_cntl_signals_q.io.deq.valid
  val cntl = mesh_cntl_signals_q.io.deq.bits

  // Instantiate the actual mesh
  val mesh = Module(new MeshWithDelays(inputType, outputType, accType, tag_with_deps, dataflow, pe_latency,
    tileRows, tileColumns, meshRows, meshColumns, shifter_banks, shifter_banks))

  mesh.io.a__.valid := false.B
  mesh.io.d__.valid := false.B
  mesh.io.b__.valid := false.B
  mesh.io.tag_in.valid := false.B
  mesh.io.flush.valid := control_state === flush && !cntl_valid // We want to make sure that the mesh has absorbed all inputs before flushing

  mesh.io.a__.bits := DontCare
  mesh.io.d__.bits := DontCare
  mesh.io.b__.bits := DontCare
  mesh.io.tag_in.bits := DontCare
  mesh.io.pe_control.propagate := Mux(control_state === flush, in_prop_flush, cntl.prop)
  mesh.io.pe_control.dataflow := cntl.dataflow
  mesh.io.pe_control.shift := cntl.shift
  mesh.io.flush.bits := 0.U

  // Hazards
  val raw_hazard_pre = mesh.io.tags_in_progress.map { t =>
    val is_garbage = t.addr.is_garbage()
    val pre_raw_haz = t.addr.is_same_address(rs1s(0))
    val mul_raw_haz = t.addr.is_same_address(rs1s(1)) || t.addr.is_same_address(rs2s(1))

    !is_garbage && (pre_raw_haz || mul_raw_haz)
  }.reduce(_ || _)

  val raw_hazard_mulpre = mesh.io.tags_in_progress.map { t =>
    val is_garbage = t.addr.is_garbage()
    val pre_raw_haz = t.addr.is_same_address(rs1s(1))
    val mul_raw_haz = t.addr.is_same_address(rs1s(2)) || t.addr.is_same_address(rs2s(2))

    !is_garbage && (mul_raw_haz || pre_raw_haz)
  }.reduce(_ || _)

  // val matmul_in_progress = mesh.io.tags_in_progress.map(_.rob_ids(0).valid).reduce(_ || _)
  val matmul_in_progress = mesh.io.tags_in_progress.map(_.rob_id.valid).reduce(_ || _)

  // SRAM scratchpad
  val dataAbank__ = a_address_rs1__.sp_bank()
  val dataDbank__ = d_address_rs2__.sp_bank()
  val dataBbank__ = b_address_rs1__.sp_bank()

  val a_read_from_acc__ = a_address_rs1__.is_acc_addr
  val d_read_from_acc__ = d_address_rs2__.is_acc_addr
  val b_read_from_acc__ = b_address_rs1__.is_acc_addr

  val start_inputting_a__ = WireInit(false.B)
  val start_inputting_d__ = WireInit(false.B)
  val start_inputting_b__ = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)

  // TODO merge these into one enum
  val perform_single_preload = RegInit(false.B)
  val perform_single_mul = RegInit(false.B)
  val perform_mul_pre = RegInit(false.B)

  val performing_single_preload = WireInit(perform_single_preload && control_state === compute)
  val performing_single_mul = WireInit(perform_single_mul && control_state === compute)
  val performing_mul_pre = WireInit(perform_mul_pre && control_state === compute)

  // Fire counters which resolve same-bank accesses
  val a_fire_counter__ = Reg(UInt((log2Ceil(block_size) max 1).W))
  val d_fire_counter__ = Reg(UInt((log2Ceil(block_size) max 1).W))
  val b_fire_counter__ = Reg(UInt((log2Ceil(block_size) max 1).W))

  def same_bank(addr1: LocalAddr, addr2: LocalAddr, start_inputting1: Bool, start_inputting2: Bool): Bool = {
    val addr1_read_from_acc = addr1.is_acc_addr
    val addr2_read_from_acc = addr2.is_acc_addr

    val is_garbage = addr1.is_garbage() || addr2.is_garbage() ||
      !start_inputting1 || !start_inputting2

    !is_garbage && ((addr1_read_from_acc && addr2_read_from_acc) ||
      (!addr1_read_from_acc && !addr2_read_from_acc && addr1.sp_bank() === addr2.sp_bank()))
  }

  val a_ready__ = WireInit(true.B)
  val d_ready__ = WireInit(true.B)
  val b_ready__ = WireInit(true.B)

  case class Operand(addr: LocalAddr, start_inputting: Bool, counter: UInt, priority: Int)
  val a_operand = Operand(a_address_rs1__, start_inputting_a__, a_fire_counter__, 0)
  val b_operand = Operand(d_address_rs2__, start_inputting_d__, d_fire_counter__, 1)
  val d_operand = Operand(b_address_rs1__, start_inputting_b__, b_fire_counter__, 2)
  val operands = Seq(a_operand, b_operand, d_operand)

  val Seq(a_valid__, d_valid__, b_valid__) = operands.map { case Operand(addr, start_inputting, counter, priority) =>
      val others = operands.filter(_.priority != priority)

      val same_banks = others.map(o => same_bank(addr, o.addr, start_inputting, o.start_inputting))
      val same_counter = others.map(o => counter === o.counter)
      val one_ahead = others.map(o => counter === wrappingAdd(o.counter, 1.U, block_size))
      // val one_ahead = others.map(o => counter > o.counter || (counter === 0.U && o.counter === (block_size-1).U))
      val higher_priorities = others.map(o => (o.priority < priority).B)

      val must_wait_for = ((same_banks zip same_counter) zip (one_ahead zip higher_priorities)).map {
        case ((sb, sc), (oa, hp)) =>
          (sb && hp && sc) || oa
      }

      !must_wait_for.reduce(_ || _)
  }

  val a_fire__ = a_valid__ && a_ready__
  val d_fire__ = d_valid__ && d_ready__
  val b_fire__ = b_valid__ && b_ready__

  val firing = start_inputting_a__ || start_inputting_d__ || start_inputting_b__

  when (!firing) {
    a_fire_counter__ := 0.U
  }.elsewhen (firing && a_valid__ && a_ready__ && cntl_ready) {
    a_fire_counter__ := wrappingAdd(a_fire_counter__, 1.U, block_size)
  }

  when (!firing) {
    d_fire_counter__ := 0.U
  }.elsewhen (firing && d_valid__ && d_ready__ && cntl_ready) {
    d_fire_counter__ := wrappingAdd(d_fire_counter__, 1.U, block_size)
  }

  when (!firing) {
    b_fire_counter__ := 0.U
  }.elsewhen (firing && b_valid__ && b_ready__ && cntl_ready) {
    b_fire_counter__ := wrappingAdd(b_fire_counter__, 1.U, block_size)
  }

  // The last line in this (long) Boolean is just to make sure that we don't think we're done as soon as we begin firing
  // TODO change when square requirement lifted
  val about_to_fire_all_rows = ((a_fire_counter__ === (block_size-1).U && a_valid__) || a_fire_counter__ === 0.U) &&
                               ((d_fire_counter__ === (block_size-1).U && d_valid__) || d_fire_counter__ === 0.U) &&
                               ((b_fire_counter__ === (block_size-1).U && b_valid__) || b_fire_counter__ === 0.U) &&
                               (a_fire_counter__ =/= 0.U || d_fire_counter__ =/= 0.U || b_fire_counter__ =/= 0.U) &&
                               cntl_ready

  // Scratchpad reads
  for (i <- 0 until sp_banks) {
    val read_a__ = a_valid__ && !a_read_from_acc__ && dataAbank__ === i.U && start_inputting_a__ && !multiply_garbage
    val read_d__ = d_valid__ && !d_read_from_acc__ && dataDbank__ === i.U && start_inputting_d__ && !accumulate_zeros
    val read_b__ = b_valid__ && !b_read_from_acc__ && dataBbank__ === i.U && start_inputting_b__ && !preload_zeros

    Seq((read_a__, a_ready__), (read_d__, d_ready__), (read_b__, b_ready__)).foreach { case (rd, r) =>
      when (rd && !io.read(i).req.ready) {
        r := false.B
      }
    }

    io.read(i).req.valid := read_a__ || read_d__ || read_b__
    io.read(i).req.bits.fromDMA := false.B
    io.read(i).req.bits.addr := MuxCase(a_address_rs1__.sp_row() + a_fire_counter__,
      Seq(read_d__ -> (d_address_rs2__.sp_row() + d_fire_counter__),
        read_b__ -> (b_address_rs1__.sp_row() + block_size.U - 1.U - b_fire_counter__)))

    io.read(i).resp.ready := true.B
  }

  // Accumulator read // TODO can only handle one acc read for now
  {
    val read_a_from_acc = a_valid__ && a_read_from_acc__ && start_inputting_a__ && !multiply_garbage
    val read_b_from_acc = d_valid__ && d_read_from_acc__ && start_inputting_d__ && !accumulate_zeros
    val read_d_from_acc = b_valid__ && b_read_from_acc__ && start_inputting_b__ && !preload_zeros

    io.acc.read.req.valid := read_a_from_acc || read_b_from_acc || read_d_from_acc
    io.acc.read.req.bits.shift := acc_shift
    io.acc.read.req.bits.relu6_shift := relu6_shift
    io.acc.read.req.bits.act := activation
    io.acc.read.req.bits.fromDMA := false.B

    io.acc.read.req.bits.addr := MuxCase(a_address_rs1__.acc_row() + a_fire_counter__,
      Seq(read_b_from_acc -> (d_address_rs2__.acc_row() + d_fire_counter__),
        read_d_from_acc -> (b_address_rs1__.acc_row() + block_size.U - 1.U - b_fire_counter__)))

    io.acc.read.resp.ready := true.B
  }

  // FSM logic
  switch (control_state) {
    is (waiting_for_cmd) {
      // Default state
      perform_single_preload := false.B
      perform_mul_pre := false.B
      perform_single_mul := false.B

      when(cmd.valid(0))
      {
        when(DoConfig && !matmul_in_progress && !pending_completed_rob_id.valid) {
          activation := rs1s(0)(4, 3)
          in_shift := rs2s(0)(31, 0) // TODO magic number
          acc_shift := cmd.bits(0).cmd.rs1(xLen-1, 32) // TODO magic number
          relu6_shift := cmd.bits(0).cmd.rs2(xLen-1, 32) // TODO magic number

          if (dataflow == Dataflow.BOTH)
            current_dataflow := rs1s(0)(2)

          io.completed.valid := true.B
          io.completed.bits := cmd.bits(0).rob_id

          cmd.pop := 1.U
        }

        // Preload
        .elsewhen(DoPreloads(0) && cmd.valid(1) && !raw_hazard_pre) {
          perform_single_preload := true.B
          performing_single_preload := true.B

          start_inputting_a__ := current_dataflow === Dataflow.OS.id.U
          start_inputting_b__ := true.B

          control_state := compute
        }

        // Overlap compute and preload
        .elsewhen(DoComputes(0) && cmd.valid(1) && DoPreloads(1) && cmd.valid(2) && !raw_hazard_mulpre) {
          perform_mul_pre := true.B
          performing_mul_pre := true.B

          start_inputting_a__ := true.B
          start_inputting_d__ := true.B
          start_inputting_b__ := true.B

          control_state := compute
        }

        // Single mul
        .elsewhen(DoComputes(0)) {
          perform_single_mul := true.B
          performing_single_mul := true.B

          start_inputting_a__ := current_dataflow === Dataflow.WS.id.U
          start_inputting_d__ := true.B

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
        start_inputting_a__ := current_dataflow === Dataflow.OS.id.U
        start_inputting_b__ := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 1.U
          control_state := waiting_for_cmd

          pending_completed_rob_id.valid := c_address_rs2.is_garbage()
          pending_completed_rob_id.bits := cmd.bits(0).rob_id

          when (current_dataflow === Dataflow.OS.id.U) {
            in_prop_flush := !rs2s(0).asTypeOf(local_addr_t).is_garbage()
          }
        }
      }

      // Overlapping
      .elsewhen(perform_mul_pre)
      {
        start_inputting_a__ := true.B
        start_inputting_d__ := true.B
        start_inputting_b__ := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 2.U
          control_state := waiting_for_cmd

          pending_completed_rob_id.valid := c_address_rs2.is_garbage()
          pending_completed_rob_id.bits := cmd.bits(1).rob_id

          when (current_dataflow === Dataflow.OS.id.U) {
            in_prop_flush := !rs2s(1).asTypeOf(local_addr_t).is_garbage()
          }
        }
      }

      // Only compute
      .elsewhen(perform_single_mul) {
        start_inputting_a__ := current_dataflow === Dataflow.WS.id.U
        start_inputting_d__ := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 1.U
          control_state := waiting_for_cmd
        }
      }
    }
    is (flush) {
      when(mesh.io.flush.fire()) {
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
  val computing = performing_mul_pre || performing_single_mul || performing_single_preload

  class ComputeCntlSignals extends Bundle {
    val perform_mul_pre = Bool()
    val perform_single_mul = Bool()
    val perform_single_preload = Bool()

    val a_bank__ = UInt(log2Up(sp_banks).W)
    val d_bank__ = UInt(log2Up(sp_banks).W)
    val b_bank__ = UInt(log2Up(sp_banks).W)

    val a_read_from_acc__ = Bool()
    val d_read_from_acc__ = Bool()
    val b_read_from_acc__ = Bool()

    val a_garbage__ = Bool()
    val d_garbage__ = Bool()
    val b_garbage__ = Bool()

    val accumulate_zeros = Bool()
    val preload_zeros = Bool()

    val a_fire__ = Bool()
    val d_fire__ = Bool()
    val b_fire__ = Bool()

    val c_addr = local_addr_t.cloneType

    val rob_id = UDValid(UInt(log2Up(rob_entries).W))

    val dataflow = UInt(1.W)
    val prop = UInt(1.W)
    val shift = UInt(log2Up(accType.getWidth).W)
  }

  mesh_cntl_signals_q.io.enq.valid := computing

  mesh_cntl_signals_q.io.enq.bits.perform_mul_pre := performing_mul_pre
  mesh_cntl_signals_q.io.enq.bits.perform_single_mul := performing_single_mul
  mesh_cntl_signals_q.io.enq.bits.perform_single_preload := performing_single_preload

  mesh_cntl_signals_q.io.enq.bits.a_bank__ := dataAbank__
  mesh_cntl_signals_q.io.enq.bits.d_bank__ := dataDbank__
  mesh_cntl_signals_q.io.enq.bits.b_bank__ := dataBbank__

  mesh_cntl_signals_q.io.enq.bits.a_garbage__ := a_address_rs1__.is_garbage() || !start_inputting_a__
  mesh_cntl_signals_q.io.enq.bits.d_garbage__ := d_address_rs2__.is_garbage() || !start_inputting_d__
  mesh_cntl_signals_q.io.enq.bits.b_garbage__ := b_address_rs1__.is_garbage() || !start_inputting_b__

  mesh_cntl_signals_q.io.enq.bits.a_read_from_acc__ := a_read_from_acc__
  mesh_cntl_signals_q.io.enq.bits.d_read_from_acc__ := d_read_from_acc__
  mesh_cntl_signals_q.io.enq.bits.b_read_from_acc__ := b_read_from_acc__

  mesh_cntl_signals_q.io.enq.bits.accumulate_zeros := accumulate_zeros
  mesh_cntl_signals_q.io.enq.bits.preload_zeros := preload_zeros

  mesh_cntl_signals_q.io.enq.bits.a_fire__ := a_valid__ && a_ready__
  mesh_cntl_signals_q.io.enq.bits.d_fire__ := d_valid__ && d_ready__
  mesh_cntl_signals_q.io.enq.bits.b_fire__ := b_valid__ && b_ready__

  mesh_cntl_signals_q.io.enq.bits.c_addr := c_address_rs2

  mesh_cntl_signals_q.io.enq.bits.rob_id.valid := !performing_single_mul && !c_address_rs2.is_garbage()
  mesh_cntl_signals_q.io.enq.bits.rob_id.bits := Mux(performing_single_preload, cmd.bits(0).rob_id, cmd.bits(1).rob_id)
  // mesh_cntl_signals_q.io.enq.bits.rob_id_2.valid := performing_mul_pre
  // mesh_cntl_signals_q.io.enq.bits.rob_id_2.bits := cmd.bits(1).rob_id

  mesh_cntl_signals_q.io.enq.bits.dataflow := current_dataflow
  mesh_cntl_signals_q.io.enq.bits.prop := Mux(performing_single_preload, in_prop_flush, in_prop)
  mesh_cntl_signals_q.io.enq.bits.shift := in_shift

  val readData = VecInit(io.read.map(_.resp.bits.data))
  val accReadData = io.acc.read.resp.bits.data.asUInt()

  val readValid = VecInit(io.read.map(bank => bank.resp.valid && !bank.resp.bits.fromDMA))
  val accReadValid = io.acc.read.resp.valid && !io.acc.read.resp.bits.fromDMA

  mesh_cntl_signals_q.io.deq.ready := (!cntl.a_fire__ || mesh.io.a__.fire() || !mesh.io.a__.ready) &&
    (!cntl.d_fire__ || mesh.io.d__.fire() || !mesh.io.d__.ready) &&
    (!cntl.b_fire__ || mesh.io.b__.fire() || !mesh.io.b__.ready)

  val dataA_valid__ = cntl.a_garbage__ || Mux(cntl.a_read_from_acc__, accReadValid, readValid(cntl.a_bank__))
  val dataD_valid__ = cntl.d_garbage__ || MuxCase(readValid(cntl.d_bank__), Seq(
    cntl.accumulate_zeros -> false.B,
    cntl.d_read_from_acc__ -> accReadValid
  ))
  val dataB_valid__ = cntl.b_garbage__ || MuxCase(readValid(cntl.b_bank__), Seq(
    cntl.preload_zeros -> false.B,
    cntl.b_read_from_acc__ -> accReadValid
  ))

  val dataA__ = Mux(cntl.a_read_from_acc__, accReadData, readData(cntl.a_bank__))
  val dataD__ = MuxCase(readData(cntl.d_bank__), Seq(cntl.accumulate_zeros -> 0.U, cntl.d_read_from_acc__ -> accReadData))
  val dataB__ = MuxCase(readData(cntl.b_bank__), Seq(cntl.preload_zeros -> 0.U, cntl.b_read_from_acc__ -> accReadData))

  when (cntl_valid) {
    // Default inputs
    mesh.io.a__.valid := cntl.a_fire__ && dataA_valid__
    mesh.io.d__.valid := cntl.d_fire__ && dataD_valid__
    mesh.io.b__.valid := cntl.b_fire__ && dataB_valid__
    mesh.io.tag_in.valid := true.B

    mesh.io.a__.bits := dataA__.asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.d__.bits := dataD__.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
    mesh.io.b__.bits := dataB__.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))

    mesh.io.tag_in.bits.rob_id := cntl.rob_id
    mesh.io.tag_in.bits.addr := cntl.c_addr
  }

  when (cntl_valid && cntl.perform_single_preload) {
    mesh.io.a__.bits := Mux(cntl.dataflow === Dataflow.WS.id.U, 0.U, dataA__).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.d__.bits := 0.U.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
  }

  when (cntl_valid && cntl.perform_single_mul) {
    mesh.io.a__.bits := Mux(cntl.dataflow === Dataflow.OS.id.U, 0.U, dataA__).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.tag_in.bits.addr.make_this_garbage()
  }

  // Scratchpad writes
  val w_address = mesh.io.tag_out.addr
  val write_to_acc = w_address.is_acc_addr

  val w_bank = w_address.sp_bank()
  val w_row = Mux(write_to_acc, w_address.acc_row(), w_address.sp_row())

  val output_counter = new Counter(block_size)

  val current_w_bank_address = Mux(current_dataflow === Dataflow.WS.id.U, w_row + output_counter.value,
    w_row + block_size.U - 1.U - output_counter.value)

  val is_garbage_addr = w_address.is_garbage()
  // val is_garbage_addr_and_no_deps = is_garbage_addr && !mesh.io.tag_out.rob_ids(0).valid // TODO is this actually necessary?

  // Write to normal scratchpad
  for(i <- 0 until sp_banks) {
    val activated_wdata = VecInit(mesh.io.out.bits.map(v => VecInit(v.map { e =>
      val e_clipped = e.clippedToWidthOf(inputType)
      val e_act = MuxCase(e_clipped, Seq(
        (activation === Activation.RELU) -> e_clipped.relu,
        (activation === Activation.RELU6) -> e_clipped.relu6(relu6_shift)))

      e_act
    })))

    io.write(i).en := start_array_outputting && w_bank === i.U && !write_to_acc && !is_garbage_addr
    io.write(i).addr := current_w_bank_address
    io.write(i).data := activated_wdata.asUInt()
    io.write(i).mask := VecInit(Seq.fill(io.write(0).mask.length)(true.B))
  }

  // Write to accumulator
  {
    io.acc.write.en := start_array_outputting && write_to_acc && !is_garbage_addr
    io.acc.write.addr := current_w_bank_address
    io.acc.write.data := mesh.io.out.bits
    io.acc.write.acc := w_address.accumulate
  }

  // Handle dependencies and turn off outputs for garbage addresses
  val mesh_completed_rob_id_fire = WireInit(false.B)

  when(mesh.io.out.fire() && mesh.io.tag_out.rob_id.valid) {
    when(output_counter.inc()) {
      mesh_completed_rob_id_fire := true.B
      io.completed.valid := true.B
      io.completed.bits := mesh.io.tag_out.rob_id.bits
    }

    start_array_outputting :=  !is_garbage_addr
  }

  when (pending_completed_rob_id.valid && !mesh_completed_rob_id_fire) {
    io.completed.valid := true.B
    io.completed.bits := pending_completed_rob_id.pop()
  }

  when (reset.toBool()) {
    pending_completed_rob_id.valid := false.B
  }
}

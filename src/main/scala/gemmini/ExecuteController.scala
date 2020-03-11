package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import freechips.rocketchip.config.Parameters

// TODO handle reads from the same bank
// TODO don't flush all 4 time steps when shorter flushes will work
// TODO do we still need to flush when the dataflow is weight stationary? Won't the result just keep travelling through on its own?
// TODO allow the matmul result to not be DIM-by-DIM, similar to how the matmul inputs can be padded if they aren't DIM-by-DIM
class ExecuteController[T <: Data](xLen: Int, tagWidth: Int, config: GemminiArrayConfig[T])
                                  (implicit p: Parameters, ev: Arithmetic[T]) extends Module {
  import config._
  import ev._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(rob_entries)))

    val srams = new Bundle {
      val read = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width))
      val write = Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, sp_width, (sp_width / (aligned_to * 8)) max 1))
    }

    val acc = new Bundle {
      val read = Vec(acc_banks, new AccumulatorReadIO(acc_bank_entries, log2Up(accType.getWidth), Vec(meshColumns, Vec(tileColumns, inputType))))
      val write = Vec(acc_banks, new AccumulatorWriteIO(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))))
    }

    val completed = Valid(UInt(log2Up(rob_entries).W))

    val busy = Output(Bool())
  })

  val block_size = meshRows*tileRows

  val mesh_tag = new Bundle with TagQueueTag {
    val rob_id = UDValid(UInt(log2Up(rob_entries).W))
    val addr = local_addr_t.cloneType
    val rows = UInt(log2Up(block_size + 1).W)
    val cols = UInt(log2Up(block_size + 1).W)

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
  val rs1s = VecInit(cmd.bits.map(_.cmd.rs1))
  val rs2s = VecInit(cmd.bits.map(_.cmd.rs2))

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
  val a_address_rs1 = rs1s(a_address_place).asTypeOf(local_addr_t)
  val b_address_rs2 = rs2s(0).asTypeOf(local_addr_t)
  val d_address_rs1 = rs1s(preload_cmd_place).asTypeOf(local_addr_t)
  val c_address_rs2 = rs2s(preload_cmd_place).asTypeOf(local_addr_t)

  val multiply_garbage = a_address_rs1.is_garbage()
  val accumulate_zeros = b_address_rs2.is_garbage()
  val preload_zeros = d_address_rs1.is_garbage()

  val a_cols = rs1s(a_address_place)(32 + log2Up(block_size + 1) - 1, 32) // TODO magic numbers
  val a_rows = rs1s(a_address_place)(48 + log2Up(block_size + 1) - 1, 48) // TODO magic numbers
  val b_cols = rs2s(0)(32 + log2Up(block_size + 1) - 1, 32) // TODO magic numbers
  val b_rows = rs2s(0)(48 + log2Up(block_size + 1) - 1, 48) // TODO magic numbers
  val d_cols = rs1s(preload_cmd_place)(32 + log2Up(block_size + 1) - 1, 32) // TODO magic numbers
  val d_rows = rs1s(preload_cmd_place)(48 + log2Up(block_size + 1) - 1, 48) // TODO magic numbers
  val c_cols = rs2s(preload_cmd_place)(32 + log2Up(block_size + 1) - 1, 32) // TODO magic numbers
  val c_rows = rs2s(preload_cmd_place)(48 + log2Up(block_size + 1) - 1, 48) // TODO magic numbers

  // Dependency stuff
  io.completed.valid := false.B
  io.completed.bits := DontCare

  // val pending_completed_rob_id = Reg(UDValid(UInt(log2Up(rob_entries).W)))
  val pending_completed_rob_ids = Reg(Vec(2, UDValid(UInt(log2Up(rob_entries).W))))

  // Instantiate a queue which queues up signals which must be fed into the mesh
  val mesh_cntl_signals_q = Module(new Queue(new ComputeCntlSignals, mem_pipeline+1,
    pipe=true))

  val cntl_ready = mesh_cntl_signals_q.io.enq.ready
  val cntl_valid = mesh_cntl_signals_q.io.deq.valid
  val cntl = mesh_cntl_signals_q.io.deq.bits

  // Instantiate the actual mesh
  val mesh = Module(new MeshWithDelays(inputType, outputType, accType, mesh_tag, dataflow, pe_latency,
    tileRows, tileColumns, meshRows, meshColumns, shifter_banks, shifter_banks))

  mesh.io.a.valid := false.B
  mesh.io.b.valid := false.B
  mesh.io.d.valid := false.B
  mesh.io.tag_in.valid := false.B
  mesh.io.flush.valid := control_state === flush && !cntl_valid // We want to make sure that the mesh has absorbed all inputs before flushing

  mesh.io.a.bits := DontCare
  mesh.io.b.bits := DontCare
  mesh.io.d.bits := DontCare
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
  val dataAbank = a_address_rs1.sp_bank()
  val dataBbank = b_address_rs2.sp_bank()
  val dataDbank = d_address_rs1.sp_bank()

  val dataABankAcc = a_address_rs1.acc_bank()
  val dataBBankAcc = b_address_rs2.acc_bank()
  val dataDBankAcc = d_address_rs1.acc_bank()

  val a_read_from_acc = a_address_rs1.is_acc_addr
  val b_read_from_acc = b_address_rs2.is_acc_addr
  val d_read_from_acc = d_address_rs1.is_acc_addr

  val start_inputting_a = WireInit(false.B)
  val start_inputting_b = WireInit(false.B)
  val start_inputting_d = WireInit(false.B)
  val start_array_outputting = WireInit(false.B)

  // TODO merge these into one enum
  val perform_single_preload = RegInit(false.B)
  val perform_single_mul = RegInit(false.B)
  val perform_mul_pre = RegInit(false.B)

  val performing_single_preload = WireInit(perform_single_preload && control_state === compute)
  val performing_single_mul = WireInit(perform_single_mul && control_state === compute)
  val performing_mul_pre = WireInit(perform_mul_pre && control_state === compute)

  // Fire counters which resolve same-bank accesses
  val a_fire_counter = Reg(UInt(log2Up(block_size).W))
  val b_fire_counter = Reg(UInt(log2Up(block_size).W))
  val d_fire_counter = Reg(UInt(log2Up(block_size).W))

  // These "*_fire_started" variables are only needed for 2x2 systolic arrays
  val a_fire_started = RegInit(false.B)
  val d_fire_started = RegInit(false.B)
  val b_fire_started = RegInit(false.B)

  // These variables determine whether or not the row that is currently being read should be completely padded with 0
  val a_row_is_not_all_zeros = a_fire_counter < a_rows
  val b_row_is_not_all_zeros = b_fire_counter < b_rows
  val d_row_is_not_all_zeros = block_size.U - 1.U - d_fire_counter < d_rows

  def same_bank(addr1: LocalAddr, addr2: LocalAddr, start_inputting1: Bool, start_inputting2: Bool): Bool = {
    val addr1_read_from_acc = addr1.is_acc_addr
    val addr2_read_from_acc = addr2.is_acc_addr

    val is_garbage = addr1.is_garbage() || addr2.is_garbage() ||
      !start_inputting1 || !start_inputting2

    !is_garbage && ((addr1_read_from_acc && addr2_read_from_acc) ||
      (!addr1_read_from_acc && !addr2_read_from_acc && addr1.sp_bank() === addr2.sp_bank()))
  }

  val a_ready = WireInit(true.B)
  val b_ready = WireInit(true.B)
  val d_ready = WireInit(true.B)

  case class Operand(addr: LocalAddr, start_inputting: Bool, counter: UInt, started: Bool, priority: Int) {
    val done = counter === 0.U && started
  }
  val a_operand = Operand(a_address_rs1, start_inputting_a, a_fire_counter, a_fire_started, 0)
  val b_operand = Operand(b_address_rs2, start_inputting_b, b_fire_counter, b_fire_started, 1)
  val d_operand = Operand(d_address_rs1, start_inputting_d, d_fire_counter, d_fire_started, 2)
  val operands = Seq(a_operand, b_operand, d_operand)

  val Seq(a_valid, b_valid, d_valid) = operands.map { case Operand(addr, start_inputting, counter, started, priority) =>
      val others = operands.filter(_.priority != priority)

      val same_banks = others.map(o => same_bank(addr, o.addr, start_inputting, o.start_inputting))
      val same_counter = others.map(o => counter === o.counter)

      val one_ahead = {
        if (block_size > 2)
          others.map(o => counter === wrappingAdd(o.counter, 1.U, block_size))
        else {
          others.map(o => (started && !o.started && counter === 1.U && o.counter === 0.U) ||
            (started && o.started && counter === 0.U && o.counter === 1.U))
        }
      }

      val higher_priorities = others.map(o => (o.priority < priority).B)

      val must_wait_for = ((same_banks zip same_counter) zip (one_ahead zip higher_priorities)).map {
        case ((sb, sc), (oa, hp)) =>
          (sb && hp && sc) || oa
      }

      !must_wait_for.reduce(_ || _)
  }

  val a_fire = a_valid && a_ready
  val b_fire = b_valid && b_ready
  val d_fire = d_valid && d_ready

  val firing = start_inputting_a || start_inputting_b || start_inputting_d

  when (!firing) {
    a_fire_counter := 0.U
  }.elsewhen (firing && a_fire && cntl_ready) {
    a_fire_counter := wrappingAdd(a_fire_counter, 1.U, block_size)
    a_fire_started := true.B
  }

  when (!firing) {
    b_fire_counter := 0.U
  }.elsewhen (firing && b_fire && cntl_ready) {
    b_fire_counter := wrappingAdd(b_fire_counter, 1.U, block_size)
    b_fire_started := true.B
  }

  when (!firing) {
    d_fire_counter := 0.U
  }.elsewhen (firing && d_fire && cntl_ready) {
    d_fire_counter := wrappingAdd(d_fire_counter, 1.U, block_size)
    d_fire_started := true.B
  }

  // The last line in this (long) Boolean is just to make sure that we don't think we're done as soon as we begin firing
  // TODO change when square requirement lifted
  val about_to_fire_all_rows = ((a_fire_counter === (block_size-1).U && a_valid) || a_fire_counter === 0.U) &&
    ((b_fire_counter === (block_size-1).U && b_valid) || b_fire_counter === 0.U) &&
    ((d_fire_counter === (block_size-1).U && d_valid) || d_fire_counter === 0.U) &&
    (a_fire_counter =/= 0.U || b_fire_counter =/= 0.U || d_fire_counter =/= 0.U) &&
    cntl_ready

  if (block_size == 2) {
    when (about_to_fire_all_rows) {
      a_fire_started := false.B
      b_fire_started := false.B
      d_fire_started := false.B
    }
  }

  // Scratchpad reads
  for (i <- 0 until sp_banks) {
    val read_a = a_valid && !a_read_from_acc && dataAbank === i.U && start_inputting_a && !multiply_garbage && a_row_is_not_all_zeros
    val read_b = b_valid && !b_read_from_acc && dataBbank === i.U && start_inputting_b && !accumulate_zeros && b_row_is_not_all_zeros
    val read_d = d_valid && !d_read_from_acc && dataDbank === i.U && start_inputting_d && !preload_zeros && d_row_is_not_all_zeros

    Seq((read_a, a_ready), (read_b, b_ready), (read_d, d_ready)).foreach { case (rd, r) =>
      when (rd && !io.srams.read(i).req.ready) {
        r := false.B
      }
    }

    io.srams.read(i).req.valid := read_a || read_b || read_d
    io.srams.read(i).req.bits.fromDMA := false.B
    io.srams.read(i).req.bits.addr := MuxCase(a_address_rs1.sp_row() + a_fire_counter,
      Seq(read_b -> (b_address_rs2.sp_row() + b_fire_counter),
        read_d -> (d_address_rs1.sp_row() + block_size.U - 1.U - d_fire_counter)))

    io.srams.read(i).resp.ready := true.B
  }

  // Accumulator read
  for (i <- 0 until acc_banks) {
    val read_a_from_acc = a_valid && a_read_from_acc && dataABankAcc === i.U && start_inputting_a && !multiply_garbage && a_row_is_not_all_zeros
    val read_b_from_acc = b_valid && b_read_from_acc && dataBBankAcc === i.U && start_inputting_b && !accumulate_zeros && b_row_is_not_all_zeros
    val read_d_from_acc = d_valid && d_read_from_acc && dataDBankAcc === i.U && start_inputting_d && !preload_zeros && d_row_is_not_all_zeros

    Seq((read_a_from_acc, a_ready), (read_b_from_acc, b_ready), (read_d_from_acc, d_ready)).foreach { case (rd, r) =>
      when(rd && !io.acc.read(i).req.ready) {
        r := false.B
      }
    }

    io.acc.read(i).req.valid := read_a_from_acc || read_b_from_acc || read_d_from_acc
    io.acc.read(i).req.bits.shift := acc_shift
    io.acc.read(i).req.bits.relu6_shift := relu6_shift
    io.acc.read(i).req.bits.act := activation
    io.acc.read(i).req.bits.fromDMA := false.B

    io.acc.read(i).req.bits.addr := MuxCase(a_address_rs1.acc_row() + a_fire_counter,
      Seq(read_b_from_acc -> (b_address_rs2.acc_row() + b_fire_counter),
        read_d_from_acc -> (d_address_rs1.acc_row() + block_size.U - 1.U - d_fire_counter)))

    io.acc.read(i).resp.ready := true.B
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
        // when(DoConfig && !matmul_in_progress && !pending_completed_rob_id.valid) {
        when(DoConfig && !matmul_in_progress && !pending_completed_rob_ids.map(_.valid).reduce(_ || _)) {
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

          start_inputting_a := current_dataflow === Dataflow.OS.id.U
          start_inputting_d := true.B

          control_state := compute
        }

        // Overlap compute and preload
        .elsewhen(DoComputes(0) && cmd.valid(1) && DoPreloads(1) && cmd.valid(2) && !raw_hazard_mulpre) {
          perform_mul_pre := true.B
          performing_mul_pre := true.B

          start_inputting_a := true.B
          start_inputting_b := true.B
          start_inputting_d := true.B

          control_state := compute
        }

        // Single mul
        .elsewhen(DoComputes(0)) {
          perform_single_mul := true.B
          performing_single_mul := true.B

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

          // pending_completed_rob_id.valid := c_address_rs2.is_garbage()
          // pending_completed_rob_id.bits := cmd.bits(0).rob_id
          pending_completed_rob_ids(0).valid := c_address_rs2.is_garbage()
          pending_completed_rob_ids(0).bits := cmd.bits(0).rob_id

          when (current_dataflow === Dataflow.OS.id.U) {
            in_prop_flush := !rs2s(0).asTypeOf(local_addr_t).is_garbage()
          }
        }
      }

      // Overlapping
      .elsewhen(perform_mul_pre) {
        start_inputting_a := true.B
        start_inputting_b := true.B
        start_inputting_d := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 2.U
          control_state := waiting_for_cmd

          // pending_completed_rob_id.valid := c_address_rs2.is_garbage()
          // pending_completed_rob_id.bits := cmd.bits(1).rob_id
          pending_completed_rob_ids(0).push(cmd.bits(0).rob_id)
          pending_completed_rob_ids(1).valid := c_address_rs2.is_garbage()
          pending_completed_rob_ids(1).bits := cmd.bits(1).rob_id

          when (current_dataflow === Dataflow.OS.id.U) {
            in_prop_flush := !rs2s(1).asTypeOf(local_addr_t).is_garbage()
          }
        }
      }

      // Only compute
      .elsewhen(perform_single_mul) {
        start_inputting_a := current_dataflow === Dataflow.WS.id.U
        start_inputting_b := true.B

        when (about_to_fire_all_rows) {
          cmd.pop := 1.U
          control_state := waiting_for_cmd

          pending_completed_rob_ids(0).push(cmd.bits(0).rob_id)
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

    val a_bank = UInt(log2Up(sp_banks).W)
    val b_bank = UInt(log2Up(sp_banks).W)
    val d_bank = UInt(log2Up(sp_banks).W)

    val a_bank_acc = UInt(log2Up(acc_banks).W)
    val b_bank_acc = UInt(log2Up(acc_banks).W)
    val d_bank_acc = UInt(log2Up(acc_banks).W)

    val a_read_from_acc = Bool()
    val b_read_from_acc = Bool()
    val d_read_from_acc = Bool()

    val a_garbage = Bool()
    val b_garbage = Bool()
    val d_garbage = Bool()

    val accumulate_zeros = Bool()
    val preload_zeros = Bool()

    val a_fire = Bool()
    val b_fire = Bool()
    val d_fire = Bool()

    val a_unpadded_cols = UInt(log2Up(block_size + 1).W)
    val b_unpadded_cols = UInt(log2Up(block_size + 1).W)
    val d_unpadded_cols = UInt(log2Up(block_size + 1).W)

    val c_addr = local_addr_t.cloneType
    val c_rows = UInt(log2Up(block_size + 1).W)
    val c_cols = UInt(log2Up(block_size + 1).W)

    val rob_id = UDValid(UInt(log2Up(rob_entries).W))

    val dataflow = UInt(1.W)
    val prop = UInt(1.W)
    val shift = UInt(log2Up(accType.getWidth).W)
  }

  mesh_cntl_signals_q.io.enq.valid := computing

  mesh_cntl_signals_q.io.enq.bits.perform_mul_pre := performing_mul_pre
  mesh_cntl_signals_q.io.enq.bits.perform_single_mul := performing_single_mul
  mesh_cntl_signals_q.io.enq.bits.perform_single_preload := performing_single_preload

  mesh_cntl_signals_q.io.enq.bits.a_bank := dataAbank
  mesh_cntl_signals_q.io.enq.bits.b_bank := dataBbank
  mesh_cntl_signals_q.io.enq.bits.d_bank := dataDbank

  mesh_cntl_signals_q.io.enq.bits.a_bank_acc := dataABankAcc
  mesh_cntl_signals_q.io.enq.bits.b_bank_acc := dataBBankAcc
  mesh_cntl_signals_q.io.enq.bits.d_bank_acc := dataDBankAcc

  mesh_cntl_signals_q.io.enq.bits.a_garbage := a_address_rs1.is_garbage() || !start_inputting_a
  mesh_cntl_signals_q.io.enq.bits.b_garbage := b_address_rs2.is_garbage() || !start_inputting_b
  mesh_cntl_signals_q.io.enq.bits.d_garbage := d_address_rs1.is_garbage() || !start_inputting_d

  mesh_cntl_signals_q.io.enq.bits.a_read_from_acc := a_read_from_acc
  mesh_cntl_signals_q.io.enq.bits.b_read_from_acc := b_read_from_acc
  mesh_cntl_signals_q.io.enq.bits.d_read_from_acc := d_read_from_acc

  mesh_cntl_signals_q.io.enq.bits.accumulate_zeros := accumulate_zeros
  mesh_cntl_signals_q.io.enq.bits.preload_zeros := preload_zeros

  mesh_cntl_signals_q.io.enq.bits.a_unpadded_cols := Mux(a_row_is_not_all_zeros, a_cols, 0.U)
  mesh_cntl_signals_q.io.enq.bits.b_unpadded_cols := Mux(b_row_is_not_all_zeros, b_cols, 0.U)
  mesh_cntl_signals_q.io.enq.bits.d_unpadded_cols := Mux(d_row_is_not_all_zeros, d_cols, 0.U)

  mesh_cntl_signals_q.io.enq.bits.a_fire := a_fire
  mesh_cntl_signals_q.io.enq.bits.b_fire := b_fire
  mesh_cntl_signals_q.io.enq.bits.d_fire := d_fire

  mesh_cntl_signals_q.io.enq.bits.c_addr := c_address_rs2
  mesh_cntl_signals_q.io.enq.bits.c_rows := c_rows
  mesh_cntl_signals_q.io.enq.bits.c_cols := c_cols

  mesh_cntl_signals_q.io.enq.bits.rob_id.valid := !performing_single_mul && !c_address_rs2.is_garbage()
  mesh_cntl_signals_q.io.enq.bits.rob_id.bits := cmd.bits(preload_cmd_place).rob_id

  mesh_cntl_signals_q.io.enq.bits.dataflow := current_dataflow
  mesh_cntl_signals_q.io.enq.bits.prop := Mux(performing_single_preload, in_prop_flush, in_prop)
  mesh_cntl_signals_q.io.enq.bits.shift := in_shift

  val readData = VecInit(io.srams.read.map(_.resp.bits.data))
  val accReadData = VecInit(io.acc.read.map(_.resp.bits.data.asUInt()))

  val readValid = VecInit(io.srams.read.map(bank => bank.resp.valid && !bank.resp.bits.fromDMA))
  val accReadValid = VecInit(io.acc.read.map(bank => bank.resp.valid && !bank.resp.bits.fromDMA))

  mesh_cntl_signals_q.io.deq.ready := (!cntl.a_fire || mesh.io.a.fire() || !mesh.io.a.ready) &&
    (!cntl.b_fire || mesh.io.b.fire() || !mesh.io.b.ready) &&
    (!cntl.d_fire || mesh.io.d.fire() || !mesh.io.d.ready)

  val dataA_valid = cntl.a_garbage || cntl.a_unpadded_cols === 0.U || Mux(cntl.a_read_from_acc, accReadValid(cntl.a_bank_acc), readValid(cntl.a_bank))
  val dataB_valid = cntl.b_garbage || cntl.b_unpadded_cols === 0.U || MuxCase(readValid(cntl.b_bank), Seq(
    cntl.accumulate_zeros -> false.B,
    cntl.b_read_from_acc -> accReadValid(cntl.b_bank_acc)
  ))
  val dataD_valid = cntl.d_garbage || cntl.d_unpadded_cols === 0.U || MuxCase(readValid(cntl.d_bank), Seq(
    cntl.preload_zeros -> false.B,
    cntl.d_read_from_acc -> accReadValid(cntl.d_bank_acc)
  ))

  val dataA_unpadded = Mux(cntl.a_read_from_acc, accReadData(cntl.a_bank_acc), readData(cntl.a_bank))
  val dataB_unpadded = MuxCase(readData(cntl.b_bank), Seq(cntl.accumulate_zeros -> 0.U, cntl.b_read_from_acc -> accReadData(cntl.b_bank_acc)))
  val dataD_unpadded = MuxCase(readData(cntl.d_bank), Seq(cntl.preload_zeros -> 0.U, cntl.d_read_from_acc -> accReadData(cntl.d_bank_acc)))

  val dataA = VecInit(dataA_unpadded.asTypeOf(Vec(block_size, inputType)).zipWithIndex.map { case (d, i) => Mux(i.U < cntl.a_unpadded_cols, d, inputType.zero)})
  val dataB = VecInit(dataB_unpadded.asTypeOf(Vec(block_size, inputType)).zipWithIndex.map { case (d, i) => Mux(i.U < cntl.b_unpadded_cols, d, inputType.zero)})
  val dataD = VecInit(dataD_unpadded.asTypeOf(Vec(block_size, inputType)).zipWithIndex.map { case (d, i) => Mux(i.U < cntl.d_unpadded_cols, d, inputType.zero)})

  when (cntl_valid) {
    // Default inputs
    mesh.io.a.valid := cntl.a_fire && dataA_valid
    mesh.io.b.valid := cntl.b_fire && dataB_valid
    mesh.io.d.valid := cntl.d_fire && dataD_valid
    mesh.io.tag_in.valid := true.B

    mesh.io.a.bits := dataA.asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.b.bits := dataB.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
    mesh.io.d.bits := dataD.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))

    mesh.io.tag_in.bits.rob_id := cntl.rob_id
    mesh.io.tag_in.bits.addr := cntl.c_addr
    mesh.io.tag_in.bits.cols := cntl.c_cols
    mesh.io.tag_in.bits.rows := cntl.c_rows
  }

  when (cntl_valid && cntl.perform_single_preload) {
    mesh.io.a.bits := Mux(cntl.dataflow === Dataflow.WS.id.U, 0.U, dataA.asUInt).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.b.bits := 0.U.asTypeOf(Vec(meshColumns, Vec(tileColumns, inputType)))
  }

  when (cntl_valid && cntl.perform_single_mul) {
    mesh.io.a.bits := Mux(cntl.dataflow === Dataflow.OS.id.U, 0.U, dataA.asUInt).asTypeOf(Vec(meshRows, Vec(tileRows, inputType)))
    mesh.io.tag_in.bits.addr.make_this_garbage()
  }

  // Scratchpad writes
  val w_address = mesh.io.tag_out.addr
  val write_to_acc = w_address.is_acc_addr

  val w_bank = Mux(write_to_acc, w_address.acc_bank(), w_address.sp_bank())
  val w_row = Mux(write_to_acc, w_address.acc_row(), w_address.sp_row())

  val output_counter = new Counter(block_size)

  val current_w_bank_address = Mux(current_dataflow === Dataflow.WS.id.U, w_row + output_counter.value,
    w_row + block_size.U - 1.U - output_counter.value)

  val is_garbage_addr = w_address.is_garbage()

  val w_matrix_rows = mesh.io.tag_out.rows
  val w_matrix_cols = mesh.io.tag_out.cols

  val write_this_row = Mux(current_dataflow === Dataflow.WS.id.U, output_counter.value < w_matrix_rows,
    block_size.U - 1.U - output_counter.value < w_matrix_rows)
  val w_mask = (0 until block_size).map(_.U < w_matrix_cols) // This is an element-wise mask, rather than a byte-wise mask

  // Write to normal scratchpad
  for(i <- 0 until sp_banks) {
    val activated_wdata = VecInit(mesh.io.out.bits.map(v => VecInit(v.map { e =>
      val e_clipped = e.clippedToWidthOf(inputType)
      val e_act = MuxCase(e_clipped, Seq(
        (activation === Activation.RELU) -> e_clipped.relu,
        (activation === Activation.RELU6) -> e_clipped.relu6(relu6_shift)))

      e_act
    })))

    io.srams.write(i).en := start_array_outputting && w_bank === i.U && !write_to_acc && !is_garbage_addr && write_this_row
    io.srams.write(i).addr := current_w_bank_address
    io.srams.write(i).data := activated_wdata.asUInt()
    // io.srams.write(i).mask := VecInit(Seq.fill(io.srams.write(0).mask.length)(true.B))
    io.srams.write(i).mask := w_mask.flatMap(b => Seq.fill(inputType.getWidth / (aligned_to * 8))(b))
  }

  // Write to accumulator
  for (i <- 0 until acc_banks) {
    io.acc.write(i).en := start_array_outputting && w_bank === i.U && write_to_acc && !is_garbage_addr && write_this_row
    io.acc.write(i).addr := current_w_bank_address
    io.acc.write(i).data := mesh.io.out.bits
    io.acc.write(i).acc := w_address.accumulate
    // io.acc.write(i).mask := VecInit(Seq.fill(io.acc.write(0).mask.length)(true.B))
    io.acc.write(i).mask := w_mask.flatMap(b => Seq.fill(accType.getWidth / (aligned_to * 8))(b))
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

  /*
  when (pending_completed_rob_id.valid && !mesh_completed_rob_id_fire) {
    io.completed.valid := true.B
    io.completed.bits := pending_completed_rob_id.pop()
  }
  */

  when (!mesh_completed_rob_id_fire) {
    when (pending_completed_rob_ids(0).valid) {
      io.completed.valid := true.B
      io.completed.bits := pending_completed_rob_ids(0).pop()
    }.elsewhen (pending_completed_rob_ids(1).valid) {
      io.completed.valid := true.B
      io.completed.bits := pending_completed_rob_ids(1).pop()
    }
  }

  when (reset.toBool()) {
    // pending_completed_rob_id.valid := false.B
    pending_completed_rob_ids.foreach(_.valid := false.B)
  }
}

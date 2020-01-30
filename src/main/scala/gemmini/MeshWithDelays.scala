//DO NOT TOUCH
package gemmini

import chisel3._
import chisel3.util._

import gemmini.Util._

// TODO Add io.out.ready back in. Before it was removed, it didn't work when banking, and it seemed to assume that SRAM outputs stay steady when ren is low
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO do we flush for one cycle more than necessary?
// TODO make all inputs go straight into registers to help with physical design

class MeshWithDelays[T <: Data: Arithmetic, U <: TagQueueTag with Data]
  (inputType: T, val outputType: T, accType: T,
   tagType: U, df: Dataflow.Value, pe_latency: Int,
   tileRows: Int, tileColumns: Int, meshRows: Int, meshColumns: Int,
   leftBanks: Int, upBanks: Int, outBanks: Int = 1)
  extends Module {

  val A_TYPE__ = Vec(meshRows, Vec(tileRows, inputType))
  val D_TYPE__ = Vec(meshColumns, Vec(tileColumns, inputType))
  val C_TYPE__ = Vec(meshColumns, Vec(tileColumns, outputType))
  val B_TYPE__ = Vec(meshColumns, Vec(tileColumns, inputType))
  val S_TYPE = Vec(meshColumns, Vec(tileColumns, new PEControl(accType)))

  val tagqlen = (if (meshColumns == 1) 4 else 5) * (pe_latency+1) // TODO change the tag-queue so we can make this 3

  val io = IO(new Bundle {
    val a__ = Flipped(Decoupled(A_TYPE__))
    val d__ = Flipped(Decoupled(D_TYPE__))
    val b__ = Flipped(Decoupled(B_TYPE__))

    // TODO make pe_control a ready-valid interface as well
    val pe_control = Input(new PEControl(accType))

    val tag_in = Flipped(Decoupled(tagType))
    val tag_out = Output(tagType)
    val tags_in_progress = Output(Vec(tagqlen, tagType))

    val out = Valid(C_TYPE__) // TODO make this ready-valid

    val flush = Flipped(Decoupled(UInt(2.W)))
  })

  def shifted[T <: Data](x: Vec[Vec[T]], banks: Int, reverse: Boolean = false) = {
    assert(x.size % banks == 0, "cannot bank without clean divisors")
    assert(pe_latency == 0 || (tileRows == 1 && tileColumns == 1), "If tiles are larger than 1x1, then PEs must have 0 latency")

    val banked_len = x.size / banks
    val banked_x = x.grouped(banked_len).toSeq

    val indexes = if (reverse) banked_x.indices.reverse else banked_x.indices

    (banked_x zip indexes).flatMap { case (bx, i) =>
      val bxVec = VecInit(bx)
      val sram_shift = i * banked_len * (pe_latency+1)

      val SRAMShifted = Shifter(bxVec, sram_shift, true.B, true)

      val indexes = if (reverse) SRAMShifted.indices.reverse else SRAMShifted.indices
      val RegShifted = (SRAMShifted zip indexes).map { case (srs, j) =>
        ShiftRegister(srs, j*(pe_latency+1))
      }

      RegShifted
    }
  }

  assert(meshRows*tileRows == meshColumns*tileColumns)
  val block_size = meshRows*tileRows

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  val flushing = RegInit(false.B)
  val flushing_or_about_to = flushing || io.flush.fire()

  val fire_counter = RegInit(0.U((log2Ceil(block_size) max 1).W))
  val fire_started = RegInit(false.B)

  val a_buf__ = RegEnable(io.a__.bits, io.a__.fire())
  val d_buf__ = RegEnable(io.d__.bits, io.d__.fire())
  val b_buf__ = RegEnable(io.b__.bits, io.b__.fire())

  val in_prop_reg = Reg(UInt(1.W)) // TODO inelegant
  val in_prop = WireInit(in_prop_reg)

  val a_written = RegInit(false.B)
  val b_written = RegInit(false.B)
  val d_written = RegInit(false.B)

  val tag_written = RegInit(false.B)

  val buffering_done = fire_counter === 0.U && fire_started && tag_written
  val waiting_on_non_matrix_inputs = fire_counter === 0.U && !(tag_written || io.tag_in.fire()) // TODO change when more non-matrix inputs are buffered

  when (io.a__.fire()) {
    a_written := true.B
  }

  when (io.d__.fire()) {
    b_written := true.B
  }

  when (io.b__.fire()) {
    d_written := true.B
  }

  val next_row_input = (io.a__.fire() || a_written) && (io.d__.fire() || b_written) && (io.b__.fire() || d_written)

  when (next_row_input || flushing_or_about_to) {
    a_written := false.B
    b_written := false.B
    d_written := false.B

    fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
    fire_started := true.B // We only need to write to this here, rather than in a "when (buffering_done)" statement
  }

  io.a__.ready := !a_written
  io.d__.ready := !b_written
  io.b__.ready := !d_written

  val pause = (waiting_on_non_matrix_inputs || !next_row_input) && !flushing_or_about_to

  // Transposer
  val transposer = Module(new AlwaysOutTransposer(block_size, inputType))
  transposer.io.inRow.valid := !pause && io.pe_control.dataflow === Dataflow.OS.id.U
  transposer.io.inRow.bits := VecInit(Mux(io.a__.fire(), io.a__.bits, a_buf__).flatten)
  transposer.io.outCol.ready := true.B
  val a_transposed__ = VecInit(transposer.io.outCol.bits.grouped(tileRows).map(t => VecInit(t)).toSeq)

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(inputType, outputType, accType, df, pe_latency, tileRows, tileColumns, meshRows, meshColumns))

  // TODO wire only to *_buf here, instead of io.*.bits
  val a_shifter_in__ = WireInit(Mux(io.pe_control.dataflow === Dataflow.OS.id.U,
    a_transposed__, Mux(io.a__.fire(), io.a__.bits, a_buf__)))
  val d_shifter_in__ = WireInit(Mux(io.d__.fire(), io.d__.bits, d_buf__))
  val b_shifter_in__ = Mux(io.b__.fire(), io.b__.bits, b_buf__)

  mesh.io.in_a__ := shifted(a_shifter_in__, leftBanks)
  mesh.io.in_d__ := shifted(d_shifter_in__, upBanks)
  mesh.io.in_b__ := shifted(b_shifter_in__, upBanks)

  mesh.io.in_control.zipWithIndex.foreach { case (ss, i) =>
    // ss.foreach(_ := ShiftRegister(Cat(io.mesh_control.dataflow, in_s), i * (pe_latency + 1)))
    ss.foreach(_.dataflow := ShiftRegister(io.pe_control.dataflow, i * (pe_latency + 1)))
    ss.foreach(_.propagate := ShiftRegister(in_prop, i * (pe_latency + 1)))
  }
  val result_shift = RegNext(io.pe_control.shift) // TODO will this arrive at the right time if memory isn't pipelined?
  mesh.io.in_control.zipWithIndex.foreach { case (ctrl, i) =>
    ctrl.foreach(_.shift := ShiftRegister(result_shift, i * (pe_latency + 1)))
  }

  val not_paused_vec = VecInit(Seq.fill(meshColumns)(VecInit(Seq.fill(tileColumns)(!pause))))
  mesh.io.in_valid := shifted(not_paused_vec, upBanks)

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  // TODO these would actually overlap when we switch from output-stationary to weight-stationary
  // TODO should we use io.m, or the mode output of the mesh?
  io.out.bits := shifted(Mux(io.pe_control.dataflow === Dataflow.OS.id.U, mesh.io.out_b__, mesh.io.out_c__), outBanks, true)

  io.out.valid := shifted(mesh.io.out_valid, outBanks, reverse = true)(0)(0)

  // Tags
  val tag_queue = Module(new TagQueue(tagqlen, tagType)) // TODO understand the actual required size better

  val tag_garbage = Wire(tagType.cloneType)
  tag_garbage := DontCare
  tag_garbage.make_this_garbage()

  tag_queue.io.in.bits := Mux(flushing, tag_garbage, io.tag_in.bits)

  val tag_id_reg = RegInit(0.U(1.W)) // Used to keep track of when we should increment // TODO inelegant
  val tag_id = WireInit(tag_id_reg)
  // val tag_id_delayed = ShiftRegister(tag_id, meshRows + S_TYPE.size, 0.U, true.B)
  val tag_id_delayed = ShiftRegister(tag_id, (meshRows + S_TYPE.size - 1) * (pe_latency + 1), 0.U, true.B)

  tag_queue.io.out.next := tag_id_delayed =/= RegNext(tag_id_delayed, 0.U)

  when (io.tag_in.fire()) {
    tag_written := true.B
    tag_id := ~tag_id_reg
    tag_id_reg := tag_id
  }
  io.tag_in.ready := !tag_written
  tag_queue.io.in.valid := io.tag_in.fire()

  io.tag_out := tag_queue.io.out.bits(Mux(io.pe_control.dataflow === Dataflow.OS.id.U, 0.U, 1.U))
  io.tags_in_progress := tag_queue.io.out.all

  // Add helper signals to make sure the Mesh is propagating signals for the correct number of cycles
  val tag_id_delayed_shorter = ShiftRegister(tag_id, (meshRows - 1) * (pe_latency + 1), 0.U, true.B)
  val new_matrix_being_output = tag_id_delayed_shorter =/= RegNext(tag_id_delayed_shorter, 0.U)
  dontTouch(new_matrix_being_output)

  // Flipping logic
  when(buffering_done && (next_row_input || flushing_or_about_to)) {
    active := not_active

    io.tag_in.ready := true.B
    tag_written := io.tag_in.fire()

    tag_id := ~tag_id_reg
    tag_id_reg := tag_id

    when (!flushing) {
      in_prop := io.pe_control.propagate ^ in_prop_reg
      in_prop_reg := in_prop
    }
  }

  // Flushing logic
  val flush_counter = Reg(UInt(2.W))

  io.flush.ready := !flushing
  // assert(!(io.flush.valid && !buffering_done)) // TODO get rid of this once we get the ability to ignore D

  when (io.flush.fire()) {
    flushing := true.B
    flush_counter := io.flush.bits

    // Avoid overwriting accumulated values
    a_buf__ := 0.U.asTypeOf(A_TYPE__) // TODO make 0 an Arithmetic member function
    d_buf__ := 0.U.asTypeOf(D_TYPE__)
    a_shifter_in__ := 0.U.asTypeOf(A_TYPE__)
    d_shifter_in__ := 0.U.asTypeOf(D_TYPE__)
  }

  when (flushing) {
    Seq(io.a__.ready, io.d__.ready, io.b__.ready, io.tag_in.ready).foreach(_ := false.B)

    tag_written := true.B

    when (buffering_done) {
      flush_counter := flush_counter - 1.U
      tag_queue.io.in.valid := true.B
    }

    val about_to_finish_flushing = flush_counter === 0.U && fire_counter === (block_size-1).U // TODO change when non-square requirement lifted
    when (about_to_finish_flushing) {
      fire_counter := 0.U
      tag_queue.io.in.valid := true.B
      flushing := false.B
    }
  }
}

//DO NOT TOUCH
package systolic

import chisel3._
import chisel3.util._

import systolic.Util._

// TODO Add io.out.ready back in. Before it was removed, it didn't work when banking, and it seemed to assume that SRAM outputs stay steady when ren is low
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO Change S to an enum
// TODO cleanup tags to be like S
// TODO do we flush for one cycle more than necessary?

class MeshWithDelays[T <: Data: Arithmetic, U <: Data](inputType: T, val outputType: T, accType: T, tagType: U,
                                                       df: Dataflow.Value,
                                                       val tileRows: Int, val tileColumns: Int,
                                                       val meshRows: Int, val meshColumns: Int,
                                                       val leftBanks: Int, val upBanks: Int, val outBanks: Int = 1)
  extends Module {

  val A_TYPE = Vec(meshRows, Vec(tileRows, inputType))
  val B_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val C_TYPE = Vec(meshColumns, Vec(tileColumns, outputType))
  val D_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val S_TYPE = Vec(meshColumns, Vec(tileColumns, UInt(2.W)))

  val tagqlen = 4

  val io = IO(new Bundle {
    val a = Flipped(Decoupled(A_TYPE))
    val b = Flipped(Decoupled(B_TYPE))
    val d = Flipped(Decoupled(D_TYPE))

    val s = Input(UInt(1.W))
    val m = Input(UInt(1.W))
    val shift = Input(UInt(log2Ceil(accType.getWidth - inputType.getWidth + 1).W))

    val tag_in = Flipped(Decoupled(tagType))
    val tag_out = Output(tagType)
    val tags_in_progress = Output(Vec(tagqlen, tagType))
    val tag_garbage = Input(tagType) // TODO make this a constructor parameter instead

    val out = Valid(C_TYPE) // TODO make this ready-valid
    val out_s = Output(S_TYPE)

    val flush = Flipped(Decoupled(UInt(2.W)))
  })

  def shifted[T <: Data](x: Vec[Vec[T]], banks: Int, reverse: Boolean = false) = {
    assert(x.size % banks == 0, "cannot bank without clean divisors")

    val banked_len = x.size / banks
    val banked_x = x.grouped(banked_len).toSeq

    val indexes = if (reverse) banked_x.indices.reverse else banked_x.indices

    (banked_x zip indexes).flatMap { case (bx, i) =>
      val bxVec = VecInit(bx)
      val sram_shift = i * banked_len

      val SRAMShifted = Shifter(bxVec, sram_shift, !pause, true)

      val indexes = if (reverse) SRAMShifted.indices.reverse else SRAMShifted.indices
      val RegShifted = (SRAMShifted zip indexes).map { case (srs, j) =>
        ShiftRegister(srs, j, !pause)
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

  val a_buf = RegEnable(io.a.bits, io.a.fire())
  val b_buf = RegEnable(io.b.bits, io.b.fire())
  val d_buf = RegEnable(io.d.bits, io.d.fire())

  val in_s = Reg(UInt(1.W))

  val a_written = RegInit(false.B)
  val b_written = RegInit(false.B)
  val d_written = RegInit(false.B)

  val tag_written = RegInit(false.B)

  val buffering_done = fire_counter === 0.U && tag_written
  val waiting_on_non_matrix_inputs = fire_counter === 0.U && !buffering_done

  when (io.a.fire()) {
    a_written := true.B
  }

  when (io.b.fire()) {
    b_written := true.B
  }

  when (io.d.fire()) {
    d_written := true.B
  }

  val next_row_input = (io.a.fire() || a_written) && (io.b.fire() || b_written) && (io.d.fire() || d_written)

  when (next_row_input || flushing_or_about_to) {
    a_written := false.B
    b_written := false.B
    d_written := false.B

    fire_counter := wrappingAdd(fire_counter, 1.U, block_size)
  }

  io.a.ready := !a_written
  io.b.ready := !b_written
  io.d.ready := !d_written

  val pause = (waiting_on_non_matrix_inputs || !next_row_input) && !flushing_or_about_to

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(inputType, outputType, accType, df, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a := shifted(a_buf, leftBanks)
  mesh.io.in_b := shifted(b_buf, upBanks)
  mesh.io.in_d := shifted(d_buf, upBanks)

  mesh.io.in_s.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_ := ShiftRegister(Cat(io.m, in_s), i, !pause))
  }
  mesh.io.in_shift.zipWithIndex.foreach { case (shs, i) =>
    shs.foreach(_ := ShiftRegister(io.shift, i, !pause))
  }
  mesh.io.pause := pause

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  // TODO these would actually overlap when we switch from output-stationary to weight-stationary
  // TODO should we use io.m, or the mode output of the mesh?
  io.out.bits := shifted(Mux(io.m === Dataflow.OS.id.U, mesh.io.out_c, mesh.io.out_b), outBanks, true)
  io.out_s := mesh.io.out_s.zip(mesh.io.out_s.indices.reverse).map{case (s, i) => ShiftRegister(s, i, !pause)}

  io.out.valid := !pause

  // Tags
  val tag_queue = Module(new TagQueue(tagqlen, tagType)) // TODO understand the actual required size better
  tag_queue.io.in.bits := Mux(flushing, io.tag_garbage, io.tag_in.bits)
  tag_queue.io.garbage := io.tag_garbage

  val tag_id = RegInit(0.U(1.W)) // Used to keep track of when we should increment
  val tag_id_delayed = ShiftRegister(tag_id, meshRows + S_TYPE.size-1, 0.U, !pause)

  tag_queue.io.out.next := tag_id_delayed =/= RegNext(tag_id_delayed, 0.U)

  when (io.tag_in.fire()) { tag_written := true.B; tag_id := ~tag_id }
  io.tag_in.ready := !tag_written
  tag_queue.io.in.valid := io.tag_in.fire()

  io.tag_out := tag_queue.io.out.bits(Mux(io.m === Dataflow.OS.id.U, 0.U, 1.U))
  io.tags_in_progress := tag_queue.io.out.all

  // Flipping logic
  when(buffering_done && (next_row_input || flushing_or_about_to)) {
    active := not_active

    io.tag_in.ready := true.B
    tag_written := io.tag_in.fire()

    tag_id := ~tag_id

    when (!flushing) { in_s := io.s ^ in_s }
  }

  // Flushing logic
  val flush_counter = Reg(UInt(2.W))

  io.flush.ready := !flushing
  // assert(!(io.flush.valid && !buffering_done)) // TODO get rid of this once we get the ability to ignore D

  when (io.flush.fire()) {
    flushing := true.B
    // flush_counter := 2.U
    flush_counter := io.flush.bits

    // Avoid overwriting accumulated values
    a_buf := 0.U.asTypeOf(A_TYPE)
    b_buf := 0.U.asTypeOf(B_TYPE)
  }

  when (flushing) {
    Seq(io.a.ready, io.b.ready, io.d.ready, io.tag_in.ready).foreach(_ := false.B)

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

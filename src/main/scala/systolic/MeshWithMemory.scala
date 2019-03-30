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

class MeshWithMemory[T <: Data: Arithmetic](innerType: T, tagWidth: Int, df: Dataflow.Value,
                     val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int,
                     val leftBanks: Int, val upBanks: Int, val outBanks: Int = 1) extends Module {

  val A_TYPE = Vec(meshRows, Vec(tileRows, innerType))
  val B_TYPE = Vec(meshColumns, Vec(tileColumns, innerType))
  val C_TYPE = Vec(meshColumns, Vec(tileColumns, innerType))
  val D_TYPE = Vec(meshColumns, Vec(tileColumns, innerType))
  val S_TYPE = Vec(meshColumns, Vec(tileColumns, UInt(2.W)))

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

  val io = IO(new Bundle {
    val a = Flipped(Decoupled(A_TYPE))
    val b = Flipped(Decoupled(B_TYPE))
    val d = Flipped(Decoupled(D_TYPE))

    val s = Input(UInt(1.W))
    val m = Input(UInt(1.W))

    val tag_in = Flipped(Decoupled(UInt(tagWidth.W)))
    val tag_out = Output(UInt(tagWidth.W))

    val out = Valid(C_TYPE) // TODO make this ready-valid
    val out_s = Output(S_TYPE)

    // TODO make this a decoupled
    val flush = new Bundle {
      val ready = Output(Bool())
      val valid = Input(Bool())

      def fire() = ready && valid
    }
  })

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
  val mesh = Module(new Mesh(innerType, df, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a_vec := shifted(a_buf, leftBanks)
  mesh.io.in_b_vec := shifted(b_buf, upBanks)
  mesh.io.in_d_vec := shifted(d_buf, upBanks)

  mesh.io.in_s_vec.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_ := ShiftRegister(Cat(io.m, in_s), i, !pause))
  }
  mesh.io.pause := pause

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  // TODO these would actually overlap when we switch from output-stationary to weight-stationary
  // TODO should we use io.m, or the mode output of the mesh?
  io.out.bits := shifted(Mux(io.m === Dataflow.OS.id.U, mesh.io.out_c_vec, mesh.io.out_b_vec), outBanks, true)
  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i, !pause)}

  io.out.valid := !pause

  // Tags
  val tag_garbage = Cat(Seq.fill(tagWidth)(1.U(1.W)))
  val tag_queue = Module(new TagQueue(5, UInt(tagWidth.W))) // TODO understand the actual required size better. It seems there may be a bug with it
  tag_queue.io.in.bits := Mux(flushing, tag_garbage, io.tag_in.bits)
  tag_queue.io.garbage := tag_garbage

  val tag_id = RegInit(0.U(1.W)) // Used to keep track of when we should increment
  val tag_id_delayed = ShiftRegister(tag_id, meshRows + S_TYPE.size-1, 0.U, !pause)

  tag_queue.io.out.next := tag_id_delayed =/= RegNext(tag_id_delayed, 0.U)

  when (io.tag_in.fire()) { tag_written := true.B; tag_id := ~tag_id }
  io.tag_in.ready := !tag_written
  tag_queue.io.in.valid := io.tag_in.fire()

  io.tag_out := tag_queue.io.out.bits(Mux(io.m === Dataflow.OS.id.U, 0.U, 1.U))

  // Flipping logic
  when(buffering_done && (next_row_input || flushing_or_about_to)) {
    active := not_active

    io.tag_in.ready := true.B
    tag_written := io.tag_in.fire()

    tag_id := ~tag_id

    in_s := Mux(io.flush.fire(), ~in_s, io.s ^ in_s)
  }

  // Flushing logic
  val flush_counter = Reg(UInt(3.W))

  io.flush.ready := !flushing
  // assert(!(io.flush.valid && !buffering_done)) // TODO get rid of this once we get the ability to ignore D

  when (io.flush.fire()) {
    flushing := true.B
    flush_counter := 2.U
  }

  when (flushing) {
    Seq(io.a.ready, io.b.ready, io.d.ready, io.tag_in.ready).foreach(_ := false.B)

    tag_written := true.B

    when (buffering_done) {
      flush_counter := flush_counter - 1.U

      in_s := ~in_s

      tag_queue.io.in.valid := true.B

      when (flush_counter === 0.U) {
        tag_id := tag_id // TODO really inelegant...
        fire_counter := 0.U
        flushing := false.B
      }
    }
  }
}

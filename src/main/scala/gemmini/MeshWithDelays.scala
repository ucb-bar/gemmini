//DO NOT TOUCH
package gemmini

import chisel3._
import chisel3.util._

import gemmini.Util._

class MeshWithDelaysReq[T <: Data: Arithmetic, TagT <: TagQueueTag with Data](accType: T, tagType: TagT, block_size: Int) extends Bundle {
  val pe_control = new PEControl(accType)
  val a_transpose = Bool()
  val bd_transpose = Bool()
  val total_rows = UInt(log2Up(block_size+1).W)
  val tag = tagType
  val flush = UInt(2.W) // TODO magic number

  override def cloneType: MeshWithDelaysReq.this.type = new MeshWithDelaysReq(accType, tagType, block_size).asInstanceOf[this.type]
}

class MeshWithDelaysResp[T <: Data: Arithmetic, TagT <: TagQueueTag with Data](outType: T, meshCols: Int, tileCols: Int, block_size: Int, tagType: TagT) extends Bundle {
  val data = Vec(meshCols, Vec(tileCols, outType))
  val total_rows = UInt(log2Up(block_size+1).W)
  val tag = tagType
  val last = Bool()

  override def cloneType: MeshWithDelaysResp.this.type = new MeshWithDelaysResp(outType, meshCols, tileCols, block_size, tagType).asInstanceOf[this.type]
}

// TODO Add io.out.ready back in. Before it was removed, it didn't work when banking, and it seemed to assume that SRAM outputs stay steady when ren is low
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO do we flush for one cycle more than necessary?
// TODO make all inputs go straight into registers to help with physical design

class MeshWithDelays[T <: Data: Arithmetic, U <: TagQueueTag with Data]
  (inputType: T, val outputType: T, accType: T,
   tagType: U, df: Dataflow.Value, pe_latency: Int, output_delay: Int,
   tileRows: Int, tileColumns: Int, meshRows: Int, meshColumns: Int,
   leftBanks: Int, upBanks: Int, outBanks: Int = 1, n_simultaneous_matmuls: Int = -1)
  extends Module {

  val A_TYPE = Vec(meshRows, Vec(tileRows, inputType))
  val B_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val C_TYPE = Vec(meshColumns, Vec(tileColumns, outputType))
  val D_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val S_TYPE = Vec(meshColumns, Vec(tileColumns, new PEControl(accType)))

  assert(meshRows*tileRows == meshColumns*tileColumns)
  val block_size = meshRows*tileRows

  val max_simultaneous_matmuls = if (n_simultaneous_matmuls == -1) {
    5 * (pe_latency + 1)
  } else {
    n_simultaneous_matmuls
  }
  assert(max_simultaneous_matmuls >= 5 * (pe_latency + 1))

  val tagqlen = max_simultaneous_matmuls+1

  val io = IO(new Bundle {
    val a = Flipped(Decoupled(A_TYPE))
    val b = Flipped(Decoupled(B_TYPE))
    val d = Flipped(Decoupled(D_TYPE))

    val req = Flipped(Decoupled(new MeshWithDelaysReq(accType, tagType.cloneType, block_size)))

    val resp = Valid(new MeshWithDelaysResp(outputType, meshColumns, tileColumns, block_size, tagType.cloneType))

    val tags_in_progress = Output(Vec(tagqlen, tagType))
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

  val req = Reg(UDValid(new MeshWithDelaysReq(accType, tagType, block_size)))

  val matmul_id = RegInit(0.U(log2Up(max_simultaneous_matmuls).W))

  val total_fires = req.bits.total_rows
  val fire_counter = RegInit(0.U(log2Up(block_size).W))

  val a_buf = RegEnable(io.a.bits, io.a.fire())
  val b_buf = RegEnable(io.b.bits, io.b.fire())
  val d_buf = RegEnable(io.d.bits, io.d.fire())

  val a_written = RegInit(false.B)
  val b_written = RegInit(false.B)
  val d_written = RegInit(false.B)

  val in_prop = Reg(UInt(1.W)) // TODO inelegant

  val input_next_row_into_spatial_array = req.valid && ((a_written && b_written && d_written) || req.bits.flush > 0.U)

  val last_fire = fire_counter === total_fires - 1.U && input_next_row_into_spatial_array

  val preloads = RegInit(0.U(32.W))
  dontTouch(preloads)

  when (io.req.fire()) {
    req.push(io.req.bits)
    in_prop := io.req.bits.pe_control.propagate ^ in_prop
    matmul_id := wrappingAdd(matmul_id, 1.U, max_simultaneous_matmuls)

    preloads := preloads + io.req.bits.pe_control.propagate
  }.elsewhen (last_fire) {
    req.valid := req.bits.flush > 1.U
    req.bits.flush := req.bits.flush - 1.U
  }

  when (input_next_row_into_spatial_array) {
    a_written := false.B
    b_written := false.B
    d_written := false.B

    fire_counter := wrappingAdd(fire_counter, 1.U, total_fires)
  }

  when (io.a.fire()) {
    a_written := true.B
  }

  when (io.b.fire()) {
    b_written := true.B
  }

  when (io.d.fire()) {
    d_written := true.B
  }

  io.a.ready := !a_written || input_next_row_into_spatial_array || io.req.ready
  io.b.ready := !b_written || input_next_row_into_spatial_array || io.req.ready
  io.d.ready := !d_written || input_next_row_into_spatial_array || io.req.ready

  assert(req.valid || !input_next_row_into_spatial_array)

  val pause = !req.valid || !input_next_row_into_spatial_array

  // Transposer
  val a_is_from_transposer = Mux(req.bits.pe_control.dataflow === Dataflow.OS.id.U, !req.bits.a_transpose, req.bits.a_transpose)
  val b_is_from_transposer = req.bits.pe_control.dataflow === Dataflow.OS.id.U && req.bits.bd_transpose
  val d_is_from_transposer = req.bits.pe_control.dataflow === Dataflow.WS.id.U && req.bits.bd_transpose
  val transposer = Module(new AlwaysOutTransposer(block_size, inputType))

  transposer.io.inRow.valid := !pause && (a_is_from_transposer || b_is_from_transposer || d_is_from_transposer)
  transposer.io.inRow.bits := MuxCase(VecInit(a_buf.flatten), Seq(
    b_is_from_transposer -> VecInit(b_buf.flatten),
    d_is_from_transposer -> VecInit(d_buf.flatten.reverse),
  ))

  transposer.io.outCol.ready := true.B
  val transposer_out = VecInit(transposer.io.outCol.bits.grouped(tileRows).map(t => VecInit(t)).toSeq)

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(inputType, outputType, accType, df, pe_latency, max_simultaneous_matmuls, output_delay, tileRows, tileColumns, meshRows, meshColumns))

  // TODO wire only to *_buf here, instead of io.*.bits
  val a_shifter_in = WireInit(Mux(a_is_from_transposer, transposer_out, a_buf))
  val b_shifter_in = WireInit(Mux(b_is_from_transposer, transposer_out, b_buf))
  val d_shifter_in = WireInit(Mux(d_is_from_transposer,
    VecInit(transposer_out.flatten.reverse.grouped(tileRows).map(VecInit(_)).toSeq), d_buf))

  mesh.io.in_a := shifted(a_shifter_in, leftBanks)
  mesh.io.in_b := shifted(b_shifter_in, upBanks)
  mesh.io.in_d := shifted(d_shifter_in, upBanks)

  mesh.io.in_control.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_.dataflow := ShiftRegister(req.bits.pe_control.dataflow, i * (pe_latency + 1)))
    ss.foreach(_.propagate := ShiftRegister(in_prop, i * (pe_latency + 1)))
  }
  val result_shift = RegNext(req.bits.pe_control.shift) // TODO will this arrive at the right time if memory isn't pipelined?
  mesh.io.in_control.zipWithIndex.foreach { case (ctrl, i) =>
    ctrl.foreach(_.shift := ShiftRegister(result_shift, i * (pe_latency + 1)))
  }

  val not_paused_vec = VecInit(Seq.fill(meshColumns)(VecInit(Seq.fill(tileColumns)(!pause))))
  mesh.io.in_valid := shifted(not_paused_vec, upBanks)

  val matmul_id_vec = VecInit(Seq.fill(meshColumns)(VecInit(Seq.fill(tileColumns)(matmul_id))))
  mesh.io.in_id := shifted(matmul_id_vec, upBanks)

  val matmul_last_vec = VecInit(Seq.fill(meshColumns)(VecInit(Seq.fill(tileColumns)(last_fire))))
  mesh.io.in_last := shifted(matmul_last_vec, upBanks)

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  // TODO these would actually overlap when we switch from output-stationary to weight-stationary
  val out_pe_control = shifted(mesh.io.out_control, outBanks, reverse = true)(0)(0)
  io.resp.bits.data := shifted(Mux(out_pe_control.dataflow === Dataflow.OS.id.U, mesh.io.out_c, mesh.io.out_b), outBanks, true)

  io.resp.valid := shifted(mesh.io.out_valid, outBanks, reverse = true)(0)(0)

  val out_last = shifted(mesh.io.out_last, outBanks, reverse = true)(0)(0)
  io.resp.bits.last := out_last

  // Tags
  class TagWithIdAndTotalRows extends Bundle with TagQueueTag {
    val tag = tagType.cloneType
    val id = UInt(log2Up(max_simultaneous_matmuls).W)
    val total_rows = UInt(log2Up(block_size+1).W)

    override def make_this_garbage(dummy: Int=0): Unit = {
      total_rows := block_size.U
      tag.make_this_garbage()
    }

    override def cloneType: TagWithIdAndTotalRows.this.type = (new TagWithIdAndTotalRows).asInstanceOf[this.type]
  }

  val matmul_id_of_output = wrappingAdd(matmul_id, Mux(io.req.bits.pe_control.dataflow === Dataflow.OS.id.U, 3.U, 2.U), max_simultaneous_matmuls)
  val matmul_id_of_current = wrappingAdd(matmul_id, 1.U, max_simultaneous_matmuls)

  val tagq = Module(new TagQueue(new TagWithIdAndTotalRows, tagqlen))
  tagq.io.enq.valid := io.req.fire() && io.req.bits.flush === 0.U
  tagq.io.enq.bits.tag := io.req.bits.tag
  tagq.io.enq.bits.total_rows := DontCare
  tagq.io.enq.bits.id := matmul_id_of_output

  val tag_garbage = Wire(tagType.cloneType)
  tag_garbage := DontCare
  tag_garbage.make_this_garbage()

  val out_matmul_id = WireInit(shifted(mesh.io.out_id, outBanks, reverse = true)(0)(0))
  io.resp.bits.tag := Mux(tagq.io.deq.valid && out_matmul_id === tagq.io.deq.bits.id, tagq.io.deq.bits.tag, tag_garbage)

  dontTouch(out_matmul_id)

  tagq.io.deq.ready := io.resp.valid && io.resp.bits.last && out_matmul_id === tagq.io.deq.bits.id

  val total_rows_q = Module(new Queue(new TagWithIdAndTotalRows, tagqlen))
  total_rows_q.io.enq.valid := io.req.fire() && io.req.bits.flush === 0.U
  total_rows_q.io.enq.bits.tag := DontCare
  total_rows_q.io.enq.bits.total_rows := io.req.bits.total_rows
  total_rows_q.io.enq.bits.id := matmul_id_of_current

  io.resp.bits.total_rows := Mux(total_rows_q.io.deq.valid && out_matmul_id === total_rows_q.io.deq.bits.id,
    total_rows_q.io.deq.bits.total_rows, block_size.U)

  total_rows_q.io.deq.ready := io.resp.valid && io.resp.bits.last && out_matmul_id === total_rows_q.io.deq.bits.id

  io.req.ready := (!req.valid || last_fire) && tagq.io.enq.ready && total_rows_q.io.enq.ready
  io.tags_in_progress := tagq.io.all.map(_.tag)

  when (reset.asBool()) {
    req.valid := false.B
  }

  assert(!(io.req.fire() && !tagq.io.enq.ready && io.req.bits.flush === 0.U))
}

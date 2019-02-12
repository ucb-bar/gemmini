//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO add a flush option
// TODO add option to shift output with SRAM banking instead
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO Change S to an enum
// TODO Does it make sense to not pause when the output isn't valid?

class MeshWithMemory(val width: Int, df: Dataflow.Value,
                     val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int,
                     val sramEntries: Int, val banks: Int) extends Module {

  val A_TYPE = Vec(meshRows, Vec(tileRows, UInt(width.W)))
  val B_TYPE = Vec(meshColumns, Vec(tileColumns, UInt((2*width).W)))
  val C_TYPE = Vec(meshColumns, Vec(tileColumns, UInt((2*width).W)))
  val D_TYPE = Vec(meshColumns, Vec(tileColumns, UInt((2*width).W)))
  val S_TYPE = Vec(meshColumns, Vec(tileColumns, UInt(3.W)))

  val io = IO(new Bundle {
    val a = Flipped(Decoupled(A_TYPE))
    val b = Flipped(Decoupled(B_TYPE))
    val d = Flipped(Decoupled(D_TYPE))

    val s = Flipped(Decoupled(UInt(1.W)))
    val m = Flipped(Decoupled(UInt(1.W)))

    val out = Decoupled(Output(C_TYPE))
    val out_s = Output(S_TYPE)
  })

  assert(meshRows*tileRows == meshColumns*tileColumns && meshRows*tileRows == sramEntries)

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  // Double-buffers
  val a_buf = Module(new InputBuffer(sramEntries, A_TYPE, banks))
  val b_buf = Module(new InputBuffer(sramEntries, B_TYPE, banks))
  val d_buf = Module(new InputBuffer(meshRows*tileRows, D_TYPE, banks))

  a_buf.io.in := io.a.bits
  b_buf.io.in := io.b.bits
  d_buf.io.in := io.d.bits

  a_buf.io.valid := io.a.valid
  b_buf.io.valid := io.b.valid
  d_buf.io.valid := io.d.valid

  io.a.ready := a_buf.io.ready
  io.b.ready := b_buf.io.ready
  io.d.ready := d_buf.io.ready

  // TODO this seems inelegant...
  val last_output_retrieved = RegInit(false.B)
  val all_emptied = a_buf.io.emptied && b_buf.io.emptied && d_buf.io.emptied
  val compute_done = (all_emptied && io.out.ready) || (all_emptied && last_output_retrieved)
  when (compute_done && io.out.ready) { last_output_retrieved := true.B }

  val compute_stalling = compute_done && RegNext(compute_done) // TODO this also seems inelegant...

  val buffering_done = a_buf.io.full && b_buf.io.full && d_buf.io.full

  val flip = compute_done && buffering_done // When the double-buffers flip roles
  val pause = compute_stalling || !io.out.ready

  a_buf.io.fire := flip
  b_buf.io.fire := flip
  d_buf.io.fire := flip

  a_buf.io.pause := pause
  b_buf.io.pause := pause
  d_buf.io.pause := pause

  // TODO find a better name for these two
  val s_bufs = RegInit(VecInit(1.U, 0.U))
  val m_bufs = RegInit(VecInit(0.U, 0.U)) // TODO should m be removed and just routed from IO?

  val s_next_written = RegInit(false.B)
  val m_next_written = RegInit(false.B)

  io.s.ready := !s_next_written
  io.m.ready := !m_next_written

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(width, df, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a_vec := a_buf.io.out
  mesh.io.in_b_vec := b_buf.io.out
  mesh.io.in_d_vec := d_buf.io.out

  mesh.io.in_s_vec.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_ := ShiftRegister(Cat(m_bufs(active), s_bufs(active)), i, !pause))
  }
  mesh.io.pause := pause

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  val bottom_mesh_io = (mesh.io.out_b_vec, mesh.io.out_c_vec, mesh.io.out_s_vec).zipped.toSeq
  io.out.bits := bottom_mesh_io.zip(bottom_mesh_io.indices.reverse).map { case ((bs, cs, ss), i) =>
    // TODO these would actually overlap when we switch from output-stationary to weight-stationary
    // TODO should we use io.m, or the mode output of the mesh?
    ShiftRegister(Mux(io.m.bits === Dataflow.OS.id.U, cs, bs), i, !pause)
  }
  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i, !pause)}

  val out_is_valid = RegInit(true.B)
  when (!pause) { out_is_valid := true.B }.
    elsewhen (io.out.fire()) { out_is_valid := false.B }
  io.out.valid := out_is_valid

  /*
  printf(p"     active: $active,     compute_done: $compute_done,    buffering_done: $buffering_done,    s_buf(active): ${s_bufs(active)}\n")
  printf(p"     io.a: ${io.a.bits}, a_read: ${a_buf.io.out}\n")
  printf(p"     io.b: ${io.b.bits}, b_read: ${b_buf.io.out}\n")
  printf(p"     io.d: ${io.d.bits}, d_read: ${d_buf.io.out}\n")
  printf(p"     io.out: ${io.out.bits} (valid: ${io.out.valid}) (s: ${io.out_s(0)(0)})\n")
  */

  // Control logic for buffers
  when(io.s.fire() && !flip) {
    s_bufs(not_active) := io.s.bits
    s_next_written := true.B
  }

  when(io.m.fire() && !flip) {
    m_bufs(not_active) := io.m.bits
    m_next_written := true.B
  }

  when(flip) {
    active := not_active

    io.s.ready := true.B
    io.m.ready := true.B

    when(io.s.fire()) { s_bufs(active) := io.s.bits }
    when(io.m.fire()) { m_bufs(active) := io.m.bits }

    s_next_written := io.s.fire()
    m_next_written := io.m.fire()

    last_output_retrieved := false.B

    // printf(p"     Done!   (stalling: ${compute_stalling}) (a.valid: ${io.a.valid}) (a.ready: ${io.a.ready}) (out.ready: ${io.out.ready})\n\n")
  }.elsewhen(!compute_done) {
    // printf(p"     Computing!  (stalling: ${compute_stalling}) (a.valid: ${io.a.valid}) (a.ready: ${io.a.ready}) (out.ready: ${io.out.ready})\n\n")
  }.otherwise {
    // Pause systolic array
    // printf(p"     PAUSING  (stalling: ${compute_stalling}) (a.valid: ${io.a.valid}) (a.ready: ${io.a.ready}) (out.ready: ${io.out.ready})\n\n")
  }
}

class InputBuffer[T <: Data](n: Int, t: Vec[Vec[T]], banks: Int) extends Module {
  val io = IO(new Bundle {
    // TODO use the standard Chisel Decoupled interface for these two
    val in = Input(t)
    val out = Output(t)

    val ready = Output(Bool())
    val valid = Input(Bool())

    val full = Output(Bool()) // Refers to the buffer for the next computation
    val emptied = Output(Bool()) // Refers to the buffer for the current computation

    val pause = Input(Bool()) // Pauses the output, not the input

    // TODO add ability to fire without switching SRAMs
    val fire = Input(Bool())
  })

  val in_fire = io.ready && io.valid

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(n) max 1).W))))

  // We read from buf(read_from), and we write into buf(write_into)
  val read_from = RegInit(0.U(1.W))
  val write_into = (~read_from).asUInt()

  // val bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(n, t).io))
  val bufs = VecInit(Seq.fill(2)(Module(new BankedMem(n, t, banks)).io))
  bufs.foreach { b =>
    b.wdata := io.in
    b.ren := false.B; b.wen := false.B
  }

  bufs.zip(addrs).foreach { case (b, a) => b.addr := a }
  bufs.zip(addrs.reverse).foreach { case (b, ao) => b.addr_other := ao }
  bufs.zip(bufs.reverse).foreach { case (b1, b2) => b1.ren_other := b2.ren }

  val buffering_started = RegInit(false.B)
  val buffering_done = buffering_started && addrs(write_into) === 0.U
  val output_done = addrs(read_from) === 0.U

  io.ready := !buffering_done
  io.full := buffering_done
  io.emptied := output_done

  io.out := (bufs(read_from).rdata, bufs(write_into).rdata, bufs(read_from).valid).zipped.toSeq.zipWithIndex.map {
    case ((br, bw, v), i) =>
      ShiftRegister(Mux(v, br, bw), i % (t.size / banks), !io.pause)
  }

  when (in_fire && !buffering_done) {
    addrs(write_into) := Mux(addrs(write_into) === (n-1).U, 0.U, addrs(write_into) + 1.U)
    bufs(write_into).wen := true.B
    buffering_started := true.B
  }

  when (buffering_done && io.fire) {
    read_from := write_into

    bufs(write_into).ren := true.B
    bufs(read_from).wen := in_fire

    bufs.foreach(_.addr := 0.U)
    addrs(write_into) := 1.U
    addrs(read_from) := Mux(in_fire, 1.U, 0.U)

    buffering_started := in_fire
    io.ready := true.B
  }.elsewhen(!output_done && !io.pause) {
    bufs(read_from).ren := true.B
    addrs(read_from) := Mux(addrs(read_from) === (n-1).U, 0.U, addrs(read_from) + 1.U)
  }

  // assert(!(io.pause && io.fire), "fired when paused") // TODO add this back when possible
}

class BankedMem[T <: Data](n: Int, t: Vec[Vec[T]], banks: Int) extends Module {
  // This writes horizontally, but it reads in a staggered format
  val io = IO(new Bundle {
    val addr = Input(UInt((log2Ceil(n) max 1).W))
    val addr_other = Input(UInt((log2Ceil(n) max 1).W))
    val wdata = Input(t)
    val rdata = Output(t)
    val wen = Input(Bool())
    val ren = Input(Bool())
    val ren_other = Input(Bool())
    val valid = Output(Vec(t.size, Bool()))
  })

  assert(t.size % banks == 0, "banks must be divisible")

  val banked_t = Vec(t.size / banks, t.head.cloneType)
  val banked_mems = Seq.fill(banks)(SinglePortedSyncMem(n, banked_t).io)

  // TODO better names
  val wdata_banked = io.wdata.grouped(io.wdata.size / banks).toSeq
  val rdata_banked = io.rdata.grouped(io.rdata.size / banks).toSeq
  val valid_banked = io.valid.grouped(io.valid.size / banks).toSeq

  banked_mems.zipWithIndex.foreach { case (b, i) =>
    b.ren := (io.ren && io.addr >= i.U) || (io.ren_other && io.addr_other < i.U)
    b.wen := io.wen
    b.wdata := wdata_banked(i)

    b.addr := DontCare
    when (io.wen) {
      b.addr := io.addr
    }.elsewhen (io.ren) {
      b.addr := io.addr - i.U
    }.elsewhen(io.ren_other) {
      b.addr := io.addr_other + (banks - i).U
    }
  }

  for ((buf, out) <- banked_mems zip rdata_banked) {
    (out zip buf.rdata).foreach { case (o, b) => o := b }
  }

  valid_banked.zipWithIndex.foreach { case (v, i) =>
    v.foreach(_ := io.addr >= i.U)
  }

  assert(!(io.ren && io.ren_other), "both banks are being read from at the same time")
}

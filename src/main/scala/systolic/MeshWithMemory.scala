//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO add ready-vals for output
// TODO add option to shift inputs with SRAM banking instead
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO Change S and M to enums
// TODO Consider adding a FREEZE signal to the mesh, instead of passing in zeros

class MeshWithMemory(val width: Int, val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {

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

    val out = Output(C_TYPE)
    val out_s = Output(S_TYPE)
  })

  assert(meshRows*tileRows == meshColumns*tileColumns && meshRows*tileRows == sramEntries)

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  // Double-buffers
  val a_buf = Module(new InputBuffer(sramEntries, A_TYPE))
  val b_buf = Module(new InputBuffer(sramEntries, B_TYPE))
  val d_buf = Module(new InputBuffer(meshRows*tileRows, D_TYPE))
  val out_buf = Module(new OutputBuffer(sramEntries, C_TYPE))

  a_buf.io.in := io.a.bits
  b_buf.io.in := io.b.bits
  d_buf.io.in := io.d.bits

  a_buf.io.valid := io.a.valid
  b_buf.io.valid := io.b.valid
  d_buf.io.valid := io.d.valid

  io.a.ready := a_buf.io.ready
  io.b.ready := b_buf.io.ready
  io.d.ready := d_buf.io.ready
  out_buf.io.ready := true.B // TODO

  val compute_done = a_buf.io.emptied && b_buf.io.emptied && d_buf.io.emptied
  val buffering_done = a_buf.io.full && b_buf.io.full && d_buf.io.full

  a_buf.io.fire := compute_done && buffering_done
  b_buf.io.fire := compute_done && buffering_done
  d_buf.io.fire := compute_done && buffering_done

  val flip = compute_done && buffering_done // When the double-buffers flip roles
  val stalling = compute_done && RegNext(compute_done) // TODO this seems inelegant...

  // TODO find a better name for these
  val a_read = WireInit(a_buf.io.out)
  val b_read = WireInit(b_buf.io.out)
  val d_read = WireInit(d_buf.io.out)

  // TODO find a better name for these two
  val s_bufs = RegInit(VecInit(1.U, 0.U))
  val m_bufs = RegInit(VecInit(0.U, 0.U)) // TODO should m be removed and just routed from IO?

  val s_next_written = RegInit(false.B)
  val m_next_written = RegInit(false.B)

  io.s.ready := !s_next_written
  io.m.ready := !m_next_written

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(width, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a_vec := a_read.zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  mesh.io.in_b_vec := b_read.zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}
  mesh.io.in_d_vec := d_read.zipWithIndex.map{case (d, i) => ShiftRegister(d, i)}

  mesh.io.in_s_vec.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_ := ShiftRegister(Cat(stalling, m_bufs(active), s_bufs(active)), i))
  }

  io.out := out_buf.io.out
  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i+1)}

  out_buf.io.s := ShiftRegister(mesh.io.out_s_vec.head(0), mesh.io.out_s_vec.size-1)(0) // TODO magic number

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  val bottom_mesh_io = (mesh.io.out_b_vec, mesh.io.out_c_vec, mesh.io.out_s_vec).zipped.toSeq
  out_buf.io.in := bottom_mesh_io.zip(bottom_mesh_io.indices.reverse).map { case ((bs, cs, ss), i) =>
    // TODO these would actually overlap when we switch from output-stationary to weight-stationary
    // TODO get rid of magic numbers
    val mode = ss.head(1)
    ShiftRegister(Mux(io.m.bits === 0.U, cs, bs), i)
  }

  // printf(p"     compute_done: $compute_done,    buffering_done: $buffering_done\n")
  // printf(p"     a_read: $a_read\n")
  // printf(p"     b_read: $b_read\n")
  // printf(p"     d_read: $d_read\n")

  // Control logic for buffers
  when(io.s.fire() && !flip) {
    s_bufs(not_active) := io.s.bits
    s_next_written := true.B
  }

  when(io.m.fire() && !flip) {
    m_bufs(not_active) := io.m.bits
    m_next_written := true.B
  }

  when(stalling) {
    a_read.foreach(_.foreach(_ := 0.U))
    b_read.foreach(_.foreach(_ := 0.U))
  }

  when(flip) {
    active := not_active

    io.s.ready := true.B
    io.m.ready := true.B

    when(io.s.fire()) { s_bufs(active) := io.s.bits }
    when(io.m.fire()) { m_bufs(active) := io.m.bits }

    s_next_written := io.s.fire()
    m_next_written := io.m.fire()
    // printf(p"     Done!   (ready: ${io.ready})\n\n")
  }.elsewhen(!compute_done) {
    // printf(p"     Computing!  (ready: ${io.ready})\n\n")
  }.otherwise {
    // Pause systolic array
    // printf(p"     PAUSING  (ready: ${io.ready})\n\n")
  }
}

class InputBuffer[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val in = Input(t)
    val out = Output(t)

    val ready = Output(Bool())
    val valid = Input(Bool())

    val full = Output(Bool()) // Refers to the buffer for the next computation
    val emptied = Output(Bool()) // Refers to the buffer for the current computation

    // TODO add ability to fire without switching SRAMs
    val fire = Input(Bool())
  })

  val in_fire = io.ready && io.valid

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(n) max 1).W))))

  // We read from buf(read_from), and we write into buf(write_into)
  val read_from = RegInit(0.U(1.W))
  val write_into = (~read_from).asUInt()

  val bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(n, t).io))
  bufs.foreach { b =>
    b.wdata := io.in
    b.ren := false.B; b.wen := false.B
  }

  bufs.zip(addrs).foreach { case (b, a) => b.addr := a }

  val buffering_started = RegInit(false.B)
  val buffering_done = buffering_started && addrs(write_into) === 0.U
  val output_done = addrs(read_from) === 0.U

  io.out := bufs(read_from).rdata
  io.ready := !buffering_done
  io.full := buffering_done
  io.emptied := output_done

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
  }.elsewhen(!output_done) {
    bufs(read_from).ren := true.B
    addrs(read_from) := Mux(addrs(read_from) === (n-1).U, 0.U, addrs(read_from) + 1.U)
  }
}

class OutputBuffer[T <: Data](n: Int, t: T) extends Module {
  val io = IO(new Bundle {
    val s = Input(UInt(1.W))
    val in = Input(t)
    val out = Output(t)

    val ready = Input(Bool())
    val valid = Output(Bool())

    val empty = Output(Bool())
  })

  val fire = io.ready && io.valid

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(n) max 1).W))))

  // We read from buf(current_s), and we write into buf(not_current_s)
  val current_s = RegInit(io.s)
  val not_current_s = (~current_s).asUInt()

  val bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(n, t).io))
  bufs.foreach { b =>
    b.wdata := io.in
    b.ren := false.B; b.wen := false.B
  }

  bufs.zip(addrs).foreach { case (b, a) => b.addr := a }

  val output_started = RegInit(false.B)
  val output_done = output_started && addrs(current_s) === 0.U

  val flip = io.s =/= current_s

  io.out := bufs(current_s).rdata
  io.valid := !output_done
  io.empty := output_done

  when (fire) {
    addrs(current_s) := Mux(addrs(current_s) === (n-1).U, 0.U, addrs(current_s) + 1.U)
    output_started := true.B
  }

  when (flip) {
    current_s := io.s

    bufs(not_current_s).ren := true.B
    bufs(current_s).wen := fire

    bufs.foreach(_.addr := 0.U)
    addrs(not_current_s) := 1.U
    addrs(current_s) := Mux(fire, 1.U, 0.U)

    output_started := fire
    io.valid := true.B
  }.elsewhen(!output_done) {
    bufs(current_s).ren := true.B
    bufs(not_current_s).wen := true.B
    addrs(not_current_s) := Mux(addrs(not_current_s) === (n-1).U, 0.U, addrs(not_current_s) + 1.U)
  }

  // It is assumed that io.s won't be switched until the buffer is ready. We check that in simulation here
  val first_flip = RegInit(false.B) // TODO why can the first flip be ignored?
  when (flip) { first_flip := true.B }
  assert(!(first_flip && !output_done && flip), "output buffer: flipped when not ready")
}

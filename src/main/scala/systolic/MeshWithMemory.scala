//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO add output SRAM
// TODO add separate ready-vals for all inputs (with the associated new address counters)
// TODO add option to shift inputs with SRAM banking instead
// TODO Handle matrices where N1 =/= N2 =/= N3
// TODO Change S and M to enums
// TODO Consider adding a FREEZE signal to the mesh, instead of passing in zeros

class MeshWithMemory(val width: Int, val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {

  val io = IO(new Bundle {
    val a = Input(Vec(meshRows, Vec(tileRows, UInt(width.W))))
    val b = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val d = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))

    val s = Input(UInt(1.W))
    val m = Input(UInt(1.W))

    val out = Output(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val out_s = Output(Vec(meshColumns, Vec(tileColumns, UInt(3.W))))

    val ready = Output(Bool())
    val valid = Input(Bool())
  })

  assert(meshRows*tileRows == meshColumns*tileColumns && meshRows*tileRows == sramEntries)

  val buffer_is_empty = RegInit(true.B)
  val compute_has_started = RegInit(false.B)

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(sramEntries) max 1).W))))

  // Double-buffers
  val a_bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(sramEntries, io.a.cloneType).io))
  val b_bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(sramEntries, io.b.cloneType).io))
  val d_bufs = VecInit(Seq.fill(2)(SinglePortedSyncMem(meshRows*tileRows, io.d.cloneType).io))

  // TODO find a better name for these two
  val s_bufs = RegInit(VecInit(1.U, 0.U))
  val m_bufs = RegInit(VecInit(0.U, 0.U))

  val a_reads = WireInit(VecInit(a_bufs.map(_.rdata)))
  val b_reads = WireInit(VecInit(b_bufs.map(_.rdata)))
  val d_reads = WireInit(VecInit(d_bufs.map(_.rdata)))

  a_bufs.foreach(_.wdata := io.a)
  b_bufs.foreach(_.wdata := io.b)
  d_bufs.foreach(_.wdata := io.d)

  a_bufs.foreach{ab => ab.wen := false.B; ab.ren := false.B}
  b_bufs.foreach{bb => bb.wen := false.B; bb.ren := false.B}
  d_bufs.foreach{db => db.wen := false.B; db.ren := false.B}

  a_bufs.zip(addrs).foreach { case (ab, addr) => ab.addr := addr }
  b_bufs.zip(addrs).foreach { case (bb, addr) => bb.addr := addr }
  d_bufs.zip(addrs).foreach { case (db, addr) => db.addr := addr }

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  val fire = io.ready && io.valid
  io.ready := addrs(not_active) =/= 0.U || buffer_is_empty

  val compute_done = compute_has_started && addrs(active) === 0.U
  val buffering_done = !buffer_is_empty && addrs(not_active) === 0.U

  val stalling = compute_done && RegNext(compute_done) // TODO this seems inelegant...

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(width, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a_vec := a_reads(active).zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  mesh.io.in_b_vec := b_reads(active).zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}
  mesh.io.in_d_vec := d_reads(active).zipWithIndex.map{case (d, i) => ShiftRegister(d, i)}

  mesh.io.in_s_vec.zipWithIndex.foreach { case (ss, i) =>
    ss.foreach(_ := ShiftRegister(Cat(stalling, m_bufs(active), s_bufs(active)), i))
  }

  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i)}

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  val bottom_mesh_io = (mesh.io.out_b_vec, mesh.io.out_c_vec, mesh.io.out_s_vec).zipped.toSeq
  io.out := bottom_mesh_io.zip(bottom_mesh_io.indices.reverse).map { case ((bs, cs, ss), i) =>
    // TODO get rid of magic numbers
    val mode = ss.head(1)
    ShiftRegister(Mux(io.m === 0.U, cs, bs), i)
  }

  // printf(p"     active == $active / addrs(active) == ${addrs(active)} / addrs(not_active) == ${addrs(not_active)} / addrs(0) == ${addrs(0.U)} / addrs(1) == ${addrs(1.U)}\n")
  // printf(p"     a_read: ${a_reads(active)}\n")
  // printf(p"     b_read: ${b_reads(active)}\n")
  // printf(p"     d_read: ${d_reads(active)}\n")

  // Control logic for buffers
  when(fire && !buffering_done) {
    a_bufs(not_active).wen := true.B
    b_bufs(not_active).wen := true.B
    d_bufs(not_active).wen := true.B

    s_bufs(not_active) := io.s
    m_bufs(not_active) := io.m

    addrs(not_active) := Mux(addrs(not_active) === (sramEntries - 1).U, 0.U, addrs(not_active) + 1.U)
    buffer_is_empty := false.B

    // printf(p"    Fire! (${io.a})\n")
  }.elsewhen(!fire) {
    // TODO remove this when block. Its only here for debugging help
    // printf(p"    Miss!\n")
  }

  a_bufs(active).ren := !compute_done
  b_bufs(active).ren := !compute_done
  d_bufs(active).ren := !compute_done

  when(stalling) {
    a_reads(active).foreach(_.foreach(_ := 0.U))
    b_reads(active).foreach(_.foreach(_ := 0.U))
    d_reads(active).foreach(_.foreach(_ := 0.U))
  }

  when(compute_done && buffering_done) {
    addrs.foreach(_ := 1.U)

    a_bufs(not_active).ren := true.B
    b_bufs(not_active).ren := true.B
    d_bufs(not_active).ren := true.B

    io.ready := true.B
    a_bufs(active).wen := fire
    b_bufs(active).wen := fire
    d_bufs(active).wen := fire

    active := not_active
    buffer_is_empty := true.B
    compute_has_started := false.B
    // printf(p"     Done!   (ready: ${io.ready}, ${addrs(not_active) =/= 0.U || buffer_is_empty}; has_started: $compute_has_started; is_empty: $buffer_is_empty)\n\n")
  }.elsewhen(!compute_done) {
    addrs(active) := Mux(addrs(active) === (sramEntries - 1).U, 0.U, addrs(active) + 1.U)
    compute_has_started := true.B
    // printf(p"     Computing!  (ready: ${io.ready}, ${addrs(not_active) =/= 0.U || buffer_is_empty}; has_started: $compute_has_started; is_empty: $buffer_is_empty)\n\n")
  }.otherwise {
    // Pause systolic array
    // printf(p"     PAUSING  (ready: ${io.ready}, ${addrs(not_active) =/= 0.U || buffer_is_empty}; has_started: $compute_has_started; is_empty: $buffer_is_empty)\n\n")
  }
}

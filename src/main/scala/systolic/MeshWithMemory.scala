//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO add weight-stationary support
// TODO add support for user-given S and M
// TODO create new SRAM module (I don't like Chisel mems very much)
// TODO get rid of wasted cycle at end
// TODO add output SRAM
// TODO add separate ready-vals for all inputs (with the associated new address counters)
// TODO add option to shift inputs with SRAM banking instead
// TODO Handle matrices where N1 =/= N2 =/= N3

class MeshWithMemory(val width: Int, val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {

  val io = IO(new Bundle {
    val a = Input(Vec(meshRows, Vec(tileRows, UInt(width.W))))
    val b = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val d = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))

    val m = Input(UInt(1.W))

    val out = Output(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val out_s = Output(Vec(meshColumns, Vec(tileColumns, UInt(3.W))))

    val ready = Output(Bool())
    val valid = Input(Bool())
  })

  assert(meshRows*tileRows == meshColumns*tileColumns && meshRows*tileRows == sramEntries)

  val a_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.a.cloneType))
  val b_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.b.cloneType))
  val d_bufs = Seq.fill(2)(SyncReadMem(meshRows*tileRows, io.d.cloneType))

  val buffer_is_empty = RegInit(true.B)
  val compute_has_started = RegInit(false.B)

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(sramEntries) max 1).W))))

  val a_reads = Wire(Vec(2, io.a.cloneType))
  val b_reads = Wire(Vec(2, io.b.cloneType))
  val d_reads = Wire(Vec(2, io.d.cloneType))

  // The read signals can only be connected to the read ports in a "when" block further down, to avoid creating a double-ported SRAM
  a_reads := DontCare
  b_reads := DontCare
  d_reads := DontCare

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  val fire = io.ready && io.valid
  io.ready := addrs(not_active) =/= 0.U || buffer_is_empty

  val compute_done = addrs(active) === 0.U && compute_has_started
  val buffering_done = !buffer_is_empty && addrs(not_active) === 0.U

  val stalling = compute_done && RegNext(compute_done) // TODO this seems messy...

  // Wire up mesh's IO to this module's IO
  val mesh = Module(new Mesh(width, tileRows, tileColumns, meshRows, meshColumns))

  mesh.io.in_a_vec := a_reads(active).zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  mesh.io.in_b_vec := b_reads(active).zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}
  mesh.io.in_d_vec := d_reads(active).zipWithIndex.map{case (d, i) => ShiftRegister(d, i)}

  mesh.io.in_s_vec.zipWithIndex.foreach{case (s, i) => s.foreach(_ := ShiftRegister(Cat(stalling, io.m, not_active), i))}

  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i)}

  // We want to output C when we're output-stationary, but B when we're weight-stationary
  io.out := (mesh.io.out_b_vec, mesh.io.out_c_vec, mesh.io.out_c_vec.indices.reverse).zipped.map { case (b, c, i) =>
    ShiftRegister(Mux(io.m === 0.U, c, b), i) // TODO get rid of magic number and SAVE "m"
  }

  // printf(p"     active == $active / addrs(active) == ${addrs(active)} / addrs(not_active) == ${addrs(not_active)} / addrs(0) == ${addrs(0.U)} / addrs(1) == ${addrs(1.U)}\n")
  // printf(p"     a_read: ${a_reads(active)}\n")
  // printf(p"     b_read: ${b_reads(active)}\n")
  // printf(p"     d_read: ${d_reads(active)}\n")

  // Control logic for buffers
  for (i <- 0 until 2) {
    when(fire && active =/= i.U) {
      a_bufs(i).write(addrs(i), io.a)
      b_bufs(i).write(addrs(i), io.b)
      d_bufs(i).write(addrs(i), io.d)

      addrs(i) := Mux(addrs(i) === (sramEntries - 1).U, 0.U, addrs(i) + 1.U)
      buffer_is_empty := false.B

      // printf(p"    Fire! (${io.d})\n")
    }.otherwise {
      when(!fire && active =/= i.U) {
        // TODO remove this when block. Its only here for debugging help
        // printf(p"    Miss!\n")
      }

      a_reads(i) := a_bufs(i).read(addrs(i))
      b_reads(i) := b_bufs(i).read(addrs(i))
      d_reads(i) := d_bufs(i).read(addrs(i))

      when(!compute_has_started || stalling) {
        a_reads(i).foreach(_.foreach(_ := 0.U))
        b_reads(i).foreach(_.foreach(_ := 0.U))
        d_reads(i).foreach(_.foreach(_ := 0.U))
      }
    }
  }

  when(compute_done && buffering_done) {
    // addrs(active) := 0.U
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

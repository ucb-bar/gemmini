//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO Handle matrices where N1 =/= N2 =/= N3

class MeshWithMemory(val width: Int, val tileRows: Int, val tileColumns: Int,
                     val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {

  val io = IO(new Bundle {
    val a = Input(Vec(meshRows, Vec(tileRows, UInt(width.W))))
    val b = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val d = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))

    val out_c = Output(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val out_s = Output(Vec(meshColumns, Vec(tileColumns, UInt(2.W))))

    val ready = Output(Bool())
    val valid = Input(Bool())
  })

  val a_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.a.cloneType))
  val b_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.b.cloneType))

  val buffer_is_empty = RegInit(true.B)
  val compute_has_started = RegInit(false.B)

  val addrs = RegInit(VecInit(Seq.fill(2)(0.U((log2Ceil(sramEntries) max 1).W))))

  val a_reads = Wire(Vec(2, io.a.cloneType))
  val b_reads = Wire(Vec(2, io.b.cloneType))

  // The read signals can only be connected to the read ports in a "when" block further down, to avoid creating a double-ported SRAM
  a_reads := DontCare
  b_reads := DontCare

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  val fire = io.ready && io.valid
  io.ready := addrs(not_active) =/= 0.U || buffer_is_empty

  val compute_done = addrs(active) === 0.U && compute_has_started
  val buffering_done = !buffer_is_empty && addrs(not_active) === 0.U

  val mesh = Module(new Mesh(width, tileRows, tileColumns, meshRows, meshColumns))

  // Wire up mesh's IO to this module's IO
  mesh.io.in_s_vec.zipWithIndex.foreach{case (s, i) => s.foreach(_ := ShiftRegister(not_active, i))}
  mesh.io.in_propag_vec.foreach(_.foreach(_ := 0.U)) // TODO

  mesh.io.in_a_vec := a_reads(active).zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  mesh.io.in_b_vec := b_reads(active).zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}

  io.out_c := mesh.io.out_vec.zip(mesh.io.out_vec.indices.reverse).map{case (c, i) => ShiftRegister(c, i)}
  io.out_s := mesh.io.out_s_vec.zip(mesh.io.out_s_vec.indices.reverse).map{case (s, i) => ShiftRegister(s, i)}

  // printf(p"     active == $active / addrs(active) == ${addrs(active)} / addrs(not_active) == ${addrs(not_active)} / addrs(0) == ${addrs(0.U)} / addrs(1) == ${addrs(1.U)}\n")
  // printf(p"     a_read: ${a_reads(active)}\n")
  // printf(p"     b_read: ${b_reads(active)}\n")

  // Control logic for buffers
  for (i <- 0 until 2) {
    when(fire && active =/= i.U) {
      a_bufs(i).write(addrs(i), io.a)
      b_bufs(i).write(addrs(i), io.b)

      addrs(i) := Mux(addrs(i) === (sramEntries - 1).U, 0.U, addrs(i) + 1.U)
      buffer_is_empty := false.B

      // printf(p"    Fire! (${io.a})\n")
    }.otherwise {
      when(!fire && active =/= i.U) {
        // TODO remove this when block. Its only here for debugging help
        // printf(p"    Miss!\n")
      }

      a_reads(i) := a_bufs(i).read(addrs(i))
      b_reads(i) := b_bufs(i).read(addrs(i))

      when(!compute_has_started || (RegNext(compute_done) && compute_done)) {
        a_reads(i).foreach(_.foreach(_ := 0.U))
        b_reads(i).foreach(_.foreach(_ := 0.U))
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

//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

// TODO Handle matrices where N1 =/= N2 =/= N3

class GridWithMemory(val width: Int, val meshRows: Int, val meshColumns: Int,
                     val gridRows: Int, val gridColumns: Int, val sramEntries: Int) extends Module {

  val io = IO(new Bundle {
    val a = Input(Vec(gridRows, Vec(meshRows, UInt(width.W))))
    val b = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val c = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))

    val out_c = Output(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val out_s = Output(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))

    val ready = Output(Bool())
    val valid = Input(Bool())

    val my_reset = Input(Bool())
  })

  val a_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.a.cloneType))
  val b_bufs = Seq.fill(2)(SyncReadMem(sramEntries, io.b.cloneType))

  val is_empty = RegInit(true.B)

  val addrs = Reg(Vec(2, UInt(log2Ceil(sramEntries).W)))
  when (io.my_reset) {
    // TODO How do we create a RegInit of a Vec?
    printf(p"Reset!\n")
    addrs.foreach(_ := 0.U)
  }

  val a_reads = VecInit((a_bufs zip addrs).map{case (buf, addr) => buf.read(addr)})
  val b_reads = VecInit((b_bufs zip addrs).map{case (buf, addr) => buf.read(addr)})

  val grid = Module(new Grid(width, meshRows, meshColumns, gridRows, gridColumns))

  val fire = io.ready && io.valid
  io.ready := true.B

  val active = RegInit(0.U(1.W)) // Which buffer is currently being read from?
  val not_active = (~active).asUInt()

  val compute_done = addrs(active) === (sramEntries - 1).U
  val buffering_done = !is_empty && (addrs(not_active) === 0.U || (addrs(not_active) === (sramEntries - 1).U && fire))

  // Wire up grid's IO to this module's IO
  grid.io.in_s_vec.foreach(_.foreach(_ := not_active))
  // grid.io.in_s_vec.zipWithIndex.foreach{case (v, i) => v.foreach(_ := ShiftRegister(not_active, i))}
  grid.io.in_propag_vec := DontCare // TODO

  // printf(p"active == $active / addrs(active) == ${addrs(active)} / addrs(0) == ${addrs(0.U)} / addrs(1) == ${addrs(1.U)}\n")

  io.out_c := grid.io.out_vec
  io.out_s := grid.io.out_s_vec

  // TODO get rid of "ad" and "bd" after we add RegEnables
  val ad = Wire(a_reads(active).cloneType)
  val bd = Wire(b_reads(active).cloneType)
  ad := a_reads(active)
  bd := b_reads(active)

  // val a_delays = a_reads(active).zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  // val b_delays = b_reads(active).zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}
  val a_delays = ad.zipWithIndex.map{case (a, i) => ShiftRegister(a, i)}
  val b_delays = bd.zipWithIndex.map{case (b, i) => ShiftRegister(b, i)}

  grid.io.in_a_vec := a_delays
  grid.io.in_b_vec := b_delays

  // Control logic for buffers
  for (i <- 0 until 2) {
    when(fire && active =/= i.U) {
      a_bufs(i).write(addrs(i), io.a)
      b_bufs(i).write(addrs(i), io.b)

      addrs(i) := Mux(addrs(i) === (sramEntries - 1).U, 0.U, addrs(i) + 1.U)
      is_empty := false.B

      // printf(p"Fire!\n")
    }.elsewhen(!fire && active =/= i.U) {
      // TODO remove this elsewhen block. Its only here for debugging help
      // printf(p"Miss!\n")
    }
  }

  when(compute_done && buffering_done) {
    addrs(active) := 0.U
    active := not_active
    is_empty := true.B
    printf(p"     Done!\n")
  }.elsewhen(addrs(active) < (sramEntries - 1).U) {
    addrs(active) := addrs(active) + 1.U
    // printf(p"     Computing!\n")
  }.otherwise {
    // Pause systolic array
    // TODO replace this with regenables instead. This is especially important for weight-stationary arrays
    // printf(p"    Pausing\n")
    ad.foreach(_.foreach(_ := 0.U))
    bd.foreach(_.foreach(_ := 0.U))
  }
}

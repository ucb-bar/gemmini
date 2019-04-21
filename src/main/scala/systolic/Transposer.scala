package systolic

import chisel3._
import chisel3.util._

class Transposer[T <: Data](val dim: Int, datatype: T) extends Module {
  val io = IO(new Bundle {
    val inRow = Flipped(Decoupled(Vec(dim, datatype)))
    val outCol = Decoupled(Vec(dim, datatype))
  })

  val regArray = Seq.fill(dim, dim)(Reg(datatype))
  val regArrayT = regArray.transpose
  // state = 0 => filling regArray row-wise, state = 1 => draining regArray column-wise
  val state = RegInit(0.U(1.W))
  val countInc = io.inRow.fire() || io.outCol.fire()
  val (countValue, countWrap) = Counter(countInc, dim)

  io.inRow.ready := state === 0.U
  io.outCol.valid := state === 1.U

  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      when(countValue === i.U && io.inRow.fire()) {
        regArray(i)(j) := io.inRow.bits(j)
      }
    }
  }

  for (i <- 0 until dim) {
    io.outCol.bits(i) := 0.U
    for (j <- 0 until dim) {
      when(countValue === j.U) {
        io.outCol.bits(i) := regArrayT(j)(i)
      }
    }
  }

  when (io.inRow.fire() && countWrap) {
    state := 1.U
  }
  when (io.outCol.fire() && countWrap) {
    state := 0.U
  }

  assert(!(state === 0.U) || !io.outCol.fire())
  assert(!(state === 1.U) || !io.inRow.fire())
}

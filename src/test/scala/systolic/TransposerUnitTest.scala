package systolic

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import systolic.TestUtils.{Matrix, consecutive, identity, rand}

class TransposerTester[T <: Bits](c: Transposer[T], mats: Seq[Matrix[Int]]) extends PeekPokeTester(c) {
  reset()
  for (mat <- mats) {
    val matT = mat.transpose
    for (row <- mat) {
      poke(c.io.inRow.valid, 1)
      row.zipWithIndex.foreach { case(elem, i) =>
        poke(c.io.inRow.bits(i), elem)
      }
      step(1)
    }
    poke(c.io.inRow.valid, 0)
    expect(c.io.inRow.ready, 0)

    for (col <- 0 until c.dim) {
      poke(c.io.outCol.ready, 1)
      expect(c.io.outCol.valid, 1)
      (0 until c.dim).foreach { i =>
        expect(c.io.outCol.bits(i), matT(col)(i))
      }
      step(1)
    }
    poke(c.io.outCol.ready, 0)
  }
}

class TransposerUnitTest extends ChiselFlatSpec {
  val testerArgs = Array(
    "--backend-name", "treadle",
    "--generate-vcd-output", "on",
    "--target-dir", "test_run_dir/transposer",
    "--top-name", "transposer"
  )
  chisel3.iotesters.Driver.execute(testerArgs, () => new Transposer(4, UInt(8.W))) {
    c => new TransposerTester(c, Seq(identity(4), rand(4, 255), consecutive(4)))
  } should be (true)
}

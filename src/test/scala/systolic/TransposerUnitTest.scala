package systolic

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import systolic.TestUtils.{Matrix, consecutive, identity, rand}

// TODO: replace Transposer type parameter with wildcard (with LUB of Data)
class TransposerTester[+C <: Transposer[UInt]](c: C, mats: Seq[Matrix[Int]], dim: Int)
  extends PeekPokeTester[C](c) {
  val rowsToPush = mats.flatten
  val expectedCols = mats.map(_.transpose).flatten
  val actualCols = collection.mutable.Buffer.empty[Seq[Int]]

  reset()
  poke(c.io.outCol.ready, 1)

  rowsToPush.foreach { row =>
    poke(c.io.inRow.valid, 1)
    println(s"Pushing row: $row")
    row.zipWithIndex.foreach { case(elem, i) =>
      poke(c.io.inRow.bits(i), elem)
    }
    if (peek(c.io.inRow.ready) == 1) {
      if (peek(c.io.outCol.valid) == 1) {
        actualCols += peek(c.io.outCol.bits).map(_.intValue)
      }
      step(1)
    }
    else {
      while(peek(c.io.inRow.ready) != 1) {
        if (peek(c.io.outCol.valid) == 1) {
          actualCols += peek(c.io.outCol.bits).map(_.intValue)
        }
        step(1)
      }
      step(1)
    }
  }

  poke(c.io.inRow.valid, 0)
  while(peek(c.io.outCol.valid) != 0) {
    actualCols += peek(c.io.outCol.bits).map(_.intValue)
    step(1)
  }

  Predef.println(rowsToPush)
  Predef.println(expectedCols)
  Predef.println(actualCols)
  assert(expectedCols == actualCols)
}

class TransposerUnitTest extends ChiselFlatSpec {
  val testerArgs = Array(
    "--backend-name", "treadle",
    "--generate-vcd-output", "on",
    "--target-dir", "test_run_dir/transposer",
    "--top-name"
  )
  val dim = 2
  behavior of "NaiveTransposer"
  it should "transpose one matrix" in {
    chisel3.iotesters.Driver.execute(testerArgs :+ "naive_transposer", () => new NaiveTransposer(dim, UInt(8.W))) {
      c => new TransposerTester(c, Seq(identity(dim), rand(dim, 255), consecutive(dim)), dim)
    } should be (true)
  }

  behavior of "PipelinedTransposer"
  it should "transpose one matrix" in {
    chisel3.iotesters.Driver.execute(testerArgs :+ "pipe_transposer", () => new PipelinedTransposer(dim, UInt(8.W))) {
      c => new TransposerTester(c, Seq(consecutive(dim)), dim)
    } should be (true)
  }
}

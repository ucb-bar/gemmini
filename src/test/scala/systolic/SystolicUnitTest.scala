// See README.md for license details.

package systolic

import breeze.linalg.DenseMatrix
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class SystolicUnitTest(c: Mesh) extends PeekPokeTester(c) {
  val m = DenseMatrix(
    Array(1, 2),
    Array(4, 4)
  )
  reset()
  c.io.in_s_vec.foreach { s =>
    poke(s, 0)
  }

  val aFormat = Array(Array(1, 2, 0, 0, 0, 0), Array(0, 4, 4, 0, 0, 0))
  val bFormat = Array(Array(1, 4, 0, 0, 0, 0), Array(0, 2, 4, 0, 0, 0))
  val sFormat = Array(Array(0, 0, 1, 1, 0, 0), Array(0, 0, 0, 1, 1, 0))
  def strobeInputs(cycle: Int): Unit = {
    for (i <- 0 until c.io.in_a_vec.length) {
      poke(c.io.in_a_vec(i), aFormat(i)(cycle))
      poke(c.io.in_b_vec(i), bFormat(i)(cycle))
      poke(c.io.in_s_vec(i), sFormat(i)(cycle))
    }
  }
  for (cycle <- 0 until 6) {
    strobeInputs(cycle)
    step(1)
    println(peek(c.io.out_vec).map(_.toString).reduce(_ + "\t" + _))
  }
}

class SystolicTester extends ChiselFlatSpec {
  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"), () => new Mesh(16, 2, 2)) {
      c => new SystolicUnitTest(c)
    } should be (true)
  }
}

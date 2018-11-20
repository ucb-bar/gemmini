
package systolic

import chisel3._
import chisel3.iotesters._

class GridUnitTest(c: Grid, m1: Seq[Seq[Int]], m2: Seq[Seq[Int]]) extends PeekPokeTester(c) {
  def print2DArray[A](a: Array[Array[A]]): Unit = {
    a.map(_.mkString(", ")).foreach {
      line => println(line)
    }
  }
}

class GridTester extends ChiselFlatSpec {
  val m1 = Seq(
    Seq(10, 3),
    Seq(2, 13)
  )

  val m2 = Seq(
    Seq(2, 4),
    Seq(3, 9)
  )
  "GridTester" should "run matmul using a 2x2 grid with 2x2 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new Grid(16, 2, 2, 2, 2))
    {
      c => new GridUnitTest(c, m1, m2)
    } should be (true)
  }
}


package systolic

import chisel3._
import chisel3.iotesters._

import scala.collection.mutable

class GridUnitTest(c: Grid, m1: Seq[Seq[Int]], m2: Seq[Seq[Int]]) extends PeekPokeTester(c) {
  def generateA(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val numEltsInRow = m(0).length * math.ceil(m.length / (c.gridRows * c.meshRows).toDouble).toInt
    (0 until c.gridRows*c.meshRows).map { r =>
      val unPaddedA = m.drop(r).grouped(c.meshRows*c.gridRows).map(_.head).toList
      val paddedA =
        Seq.fill(r / c.meshRows)(0) ++
        unPaddedA.flatten ++
        Seq.fill(numEltsInRow - (r / c.meshRows) - unPaddedA.flatten.length)(0)
      paddedA
    }
  }
  Predef.println(generateA(m1))
  //Predef.println(generateA(m2.transpose))

  def generateB(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val mT = m.transpose
    val numEltsInCol = mT(0).length * math.ceil(mT.length / (c.gridColumns * c.meshColumns).toDouble).toInt
    (0 until c.gridColumns * c.meshColumns).map { r =>
      val unPaddedB = mT.drop(r).grouped(c.meshColumns * c.gridColumns).map(_.head).toList
      val paddedB =
        Seq.fill(r / c.meshColumns)(0) ++
          unPaddedB.flatten ++
          Seq.fill(numEltsInCol - (r / c.meshColumns) - unPaddedB.flatten.length)(0)
      paddedB
    }
  }
  Predef.println(generateB(m2))
/*
  def generateS: Array[Array[Int]] = {
    (0 until c.columns).map { i =>
      Array.fill(c.rows)(0) ++ Array.fill(c.rows)(1) ++ Array.fill(c.rows)(1)
    }.toArray
  }
*/
  def print2DArray[A](a: Array[Array[A]]): Unit = {
    a.map(_.mkString(", ")).foreach {
      line => println(line)
    }
  }
}

class GridTester extends ChiselFlatSpec {
  /*
  val m1 = Seq(
    Seq(10, 3),
    Seq(2, 13)
  )
  */
  // 6x4
  val m1 = Seq(
    Seq(1, 2, 3, 4),
    Seq(5, 6, 7, 8),
    Seq(9, 10, 11, 12),
    Seq(13, 14, 15, 16),
    Seq(17, 18, 19, 20),
    Seq(21, 22, 23, 24)
  )

  // 4x2
  val m2 = Seq(
    Seq(1, 2),
    Seq(3, 4),
    Seq(5, 6),
    Seq(7, 8)
  )
  "GridTester" should "run matmul using a 2x2 grid with 2x2 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new Grid(16, 3, 2, 2, 2))
    {
      c => new GridUnitTest(c, m1, m2)
    } should be (true)
  }
}

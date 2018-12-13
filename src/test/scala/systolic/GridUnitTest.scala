
package systolic

import chisel3._
import chisel3.iotesters._
import SystolicUtils.print2DArray

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
  val A = generateA(m1)

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
  val B = generateB(m2)

  def generateS: Seq[Seq[Int]] = {
    (0 until c.gridColumns*c.meshColumns).map { i =>
      Seq.fill(math.max(c.gridRows*c.meshRows, c.gridColumns*c.meshColumns))(0) ++
        Seq.fill(c.meshRows*c.gridRows)(1) ++
        Seq.fill(c.meshRows*c.gridRows + 2)(0)
    }
  }
  val S = generateS

  reset()
  c.io.in_s_vec.foreach { col =>
    col.foreach { colMesh =>
      poke(colMesh, 0)
    }
  }

  val Apad = A.map(_.padTo(S(0).length, 0))
  val Bpad = B.map(_.padTo(S(0).length, 0))
  val Agrouped = Apad.grouped(c.meshRows).toList
  val Bgrouped = Bpad.grouped(c.meshColumns).toList
  val Sgrouped = S.grouped(c.meshColumns).toList
  val Cgold = SystolicUtils.mult(m1, m2)
  println("A Padded:")
  print2DArray(Apad)
  println("B Padded:")
  print2DArray(Bpad)
  println("S:")
  print2DArray(S)
  def strobeInputs(cycle: Int): Unit = {
    for (gridRow <- 0 until c.gridRows) {
      for (meshRow <- 0 until c.meshRows) {
        poke(c.io.in_a_vec(gridRow)(meshRow), Agrouped(gridRow)(meshRow)(cycle))
      }
    }
    for (gridCol <- 0 until c.gridColumns) {
      for (meshCol <- 0 until c.meshColumns) {
        poke(c.io.in_b_vec(gridCol)(meshCol), Bgrouped(gridCol)(meshCol)(cycle))
        poke(c.io.in_s_vec(gridCol)(meshCol), Sgrouped(gridCol)(meshCol)(cycle))
      }
    }
  }

  println("Peeking output out_vec")
  var C: Seq[Seq[BigInt]] = Seq()
  for (cycle <- 0 until Apad(0).length) {
    strobeInputs(cycle)
    val peeked = peek(c.io.out_vec)
    step(1)

    println(peeked.map(_.toString).reduce(_ + "\t" + _))
    val outValid = peek(c.io.out_s_vec)
    if (outValid.exists(_ > 0)) {
      C = peeked.take(Cgold(0).length) +: C
    }
  }
  println("Got C:")
  print2DArray(C)
  println("Cgold:")
  print2DArray(Cgold)
  assert(C == Cgold)
}

class GridTester extends ChiselFlatSpec {
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

  // Hybrid
  "GridTester" should "run matmul using a 2x2 grid with 3x2 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new Grid(16, 3, 2, 2, 2))
    {
      c => new GridUnitTest(c, m1, m2)
    } should be (true)
  }
  // Fully pipelined
  "GridTester" should "run matmul using a 6x4 grid with 1x1 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new Grid(16, 1, 1, 6, 4))
    {
      c => new GridUnitTest(c, m1, m2)
    } should be (true)
  }
  // Fully combinational
  "GridTester" should "run matmul using a 1x1 grid with one 6x4 mesh" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new Grid(16, 6, 4, 1, 1))
    {
      c => new GridUnitTest(c, m1, m2)
    } should be (true)
  }
}

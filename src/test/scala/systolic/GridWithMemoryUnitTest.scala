
package systolic

import chisel3._
import chisel3.iotesters._
import systolic.SystolicUtils.print2DArray

class GridWithMemoryUnitTest(c: GridWithMemory, m1: Seq[Seq[Int]], m2: Seq[Seq[Int]]) extends PeekPokeTester(c) {
  def generateA(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    m.map(_ ++ Seq.fill(21)(0))
  }
  val A = generateA(m1)

  def generateB(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    (m.transpose ++ Seq.fill(4)(Seq.fill(m.transpose.head.size)(0))).map(_ ++ Seq.fill(21)(0))
  }
  val B = generateB(m2)

  val propag_pad = (0 until c.gridColumns * c.meshColumns).map { _ =>
         Seq.fill(math.max(c.gridRows*c.meshRows, c.gridColumns*c.meshColumns))(0) ++
         Seq.fill(c.meshRows*c.gridRows)(0) ++
         Seq.fill(c.meshRows*c.gridRows + 2)(0)
  }

  def generateS: Seq[Seq[Int]] = {
    (0 until c.gridColumns*c.meshColumns).map { i =>
      Seq.fill(math.max(c.gridRows*c.meshRows, c.gridColumns*c.meshColumns))(0) ++
        Seq.fill(c.meshRows*c.gridRows)(1) ++
        Seq.fill(c.meshRows*c.gridRows + 2)(0)
    }
  }
  val S = generateS

  val Apad = A.map(_.padTo(S(0).length, 0))
  val Bpad = B.map(_.padTo(S(0).length, 0))
  val Agrouped = Apad.grouped(c.meshRows).toList
  val Bgrouped = Bpad.grouped(c.meshColumns).toList
  val Propaggrouped = propag_pad.grouped(c.meshColumns).toList
  val Cgold = SystolicUtils.mult(m1, m2)
  println("A Padded:")
  print2DArray(Apad)
  println("B Padded:")
  print2DArray(Bpad)

  def strobeInputs(cycle: Int): Unit = {
    poke(c.io.valid, true)

    for (gridRow <- 0 until c.gridRows) {
      for (meshRow <- 0 until c.meshRows) {
        poke(c.io.a(gridRow)(meshRow), Agrouped(gridRow)(meshRow)(cycle))
      }
    }
    for (gridCol <- 0 until c.gridColumns) {
      for (meshCol <- 0 until c.meshColumns) {
        poke(c.io.b(gridCol)(meshCol), Bgrouped(gridCol)(meshCol)(cycle))
      }
    }
  }

  def strobeGarbage(nCycles: Int) = {

  }

  reset(50)
  poke(c.io.valid, false)
  poke(c.io.my_reset, true)
  step(1)
  poke(c.io.my_reset, false)

  println("Peeking output out_vec")
  var C: Seq[Seq[Int]] = Seq()
  for (cycle <- 0 until Apad(0).length) {
    strobeInputs(cycle)

    // Put in garbage while peeking
    for (_ <- 1 to 5) {
      val peeked = peek(c.io.out_c)

      println(peeked.zip(peek(c.io.out_s)).map(t => s"(${t._1}, ${t._2})").reduce(_ + "\t" + _))
      val outValid = peek(c.io.out_s)
      C = peeked.take(Cgold(0).length).map(_.toInt) +: C

      step(1)

      poke(c.io.valid, false)
    }
  }

  // TODO find a better way to get the correct matrix output
  val C_formatted = C dropWhile(_ != Cgold.head) take Cgold.size

  println("Got C:")
  print2DArray(C_formatted)
  println("Cgold:")
  print2DArray(Cgold)
  assert(Cgold == C_formatted)
}

class GridWithMemoryTester extends ChiselFlatSpec {
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

  // Fully pipelined
  "GridWithMemoryTester" should "run matmul using a 6x6 grid with 1x1 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new GridWithMemory(16, 1, 1, 6, 6, 6))
    {
      c => new GridWithMemoryUnitTest(c, m1, m2)
    } should be (true)
  }

  // Partially pipelined
  it should "run matmul using a 3x2 grid with 2x3 meshes" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new GridWithMemory(16, 2, 3, 3, 2, 6))
    {
      c => new GridWithMemoryUnitTest(c, m1, m2)
    } should be (true)
  }

  // Fully combinational
  it should "run matmul using a 1x1 grid with one 6x6 mesh" in {
    iotesters.Driver.execute(
      Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new GridWithMemory(16, 6, 6, 1, 1, 6))
    {
      c => new GridWithMemoryUnitTest(c, m1, m2)
    } should be (true)
  }
}

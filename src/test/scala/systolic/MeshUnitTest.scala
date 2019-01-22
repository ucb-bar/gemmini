
package systolic

import chisel3._
import chisel3.iotesters._
import SystolicUtils.print2DArray

class MeshUnitTest(c: Mesh, m1: Seq[Seq[Int]], m2: Seq[Seq[Int]]) extends PeekPokeTester(c) {
  def generateA(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val numEltsInRow = m(0).length * math.ceil(m.length / (c.meshRows * c.tileRows).toDouble).toInt
    (0 until c.meshRows*c.tileRows).map { r =>
      val unPaddedA = m.drop(r).grouped(c.tileRows*c.meshRows).map(_.head).toList
      val paddedA =
        Seq.fill(r / c.tileRows)(0) ++
        unPaddedA.flatten ++
        Seq.fill(numEltsInRow - (r / c.tileRows) - unPaddedA.flatten.length)(0)
      paddedA
    }
  }
  val A = generateA(m1)

  def generateB(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val mT = m.transpose
    val numEltsInCol = mT(0).length * math.ceil(mT.length / (c.meshColumns * c.tileColumns).toDouble).toInt
    (0 until c.meshColumns * c.tileColumns).map { r =>
      val unPaddedB = mT.drop(r).grouped(c.tileColumns * c.meshColumns).map(_.head).toList
      val paddedB =
        Seq.fill(r / c.tileColumns)(0) ++
          unPaddedB.flatten ++
          Seq.fill(numEltsInCol - (r / c.tileColumns) - unPaddedB.flatten.length)(0)
      paddedB
    }
  }
  val B = generateB(m2)

  val propag_pad = (0 until c.meshColumns * c.tileColumns).map { _ =>
         Seq.fill(math.max(c.meshRows*c.tileRows, c.meshColumns*c.tileColumns))(0) ++
         Seq.fill(c.tileRows*c.meshRows)(0) ++
         Seq.fill(c.tileRows*c.meshRows + 2)(0)
  }

  def generateS: Seq[Seq[Int]] = {
    (0 until c.meshColumns*c.tileColumns).map { i =>
      Seq.fill(math.max(c.meshRows*c.tileRows, c.meshColumns*c.tileColumns))(0) ++
        Seq.fill(c.tileRows*c.meshRows)(1) ++
        Seq.fill(c.tileRows*c.meshRows + 2)(0)
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
  val Agrouped = Apad.grouped(c.tileRows).toList
  val Bgrouped = Bpad.grouped(c.tileColumns).toList
  val Propaggrouped = propag_pad.grouped(c.tileColumns).toList
  val Sgrouped = S.grouped(c.tileColumns).toList
  val Cgold = SystolicUtils.mult(m1, m2)
  println("A Padded:")
  print2DArray(Apad)
  println("B Padded:")
  print2DArray(Bpad)
  println("S:")
  print2DArray(S)
  def strobeInputs(cycle: Int): Unit = {
    for (meshRow <- 0 until c.meshRows) {
      for (meshRow <- 0 until c.tileRows) {
        poke(c.io.in_a_vec(meshRow)(meshRow), Agrouped(meshRow)(meshRow)(cycle))
      }
    }
    for (mesh <- 0 until c.meshColumns) {
      for (meshCol <- 0 until c.tileColumns) {
        poke(c.io.in_b_vec(mesh)(meshCol), Bgrouped(mesh)(meshCol)(cycle))
        poke(c.io.in_s_vec(mesh)(meshCol), Sgrouped(mesh)(meshCol)(cycle))
        poke(c.io.in_propag_vec(mesh)(meshCol),Propaggrouped(mesh)(meshCol)(cycle))
      }
    }
  }

  println("Peeking output out_vec")
  var C: Seq[Seq[Int]] = Seq()
  for (cycle <- 0 until Apad(0).length) {
    strobeInputs(cycle)
    val peeked = peek(c.io.out_vec)
    step(1)

    println(peeked.map(_.toString).reduce(_ + "\t" + _))
    val outValid = peek(c.io.out_s_vec)
    if (outValid.exists(_ > 0)) {
      C = peeked.take(Cgold(0).length).map(_.toInt) +: C
    }
  }
  println("Got C:")
  print2DArray(C)
  println("Cgold:")
  print2DArray(Cgold)
  assert(C == Cgold)
}

class MeshTester extends ChiselFlatSpec {
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
  "MeshTester" should "run matmul using a 2x2 mesh with 3x2 meshes" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"),
      () => new Mesh(16, 3, 2, 2, 2))
    {
      c => new MeshUnitTest(c, m1, m2)
    } should be (true)
  }
  // Fully pipelined
  it should "run matmul using a 6x4 mesh with 1x1 meshes" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"),
      () => new Mesh(16, 1, 1, 6, 4))
    {
      c => new MeshUnitTest(c, m1, m2)
    } should be (true)
  }
  // Fully combinational
  it should "run matmul using a 1x1 mesh with one 6x4 mesh" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"),
      () => new Mesh(16, 6, 4, 1, 1))
    {
      c => new MeshUnitTest(c, m1, m2)
    } should be (true)
  }
}

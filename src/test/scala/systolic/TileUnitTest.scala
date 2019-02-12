// See README.md for license details.

package systolic

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import SystolicUtils.print2DArray

class TileUnitTest(c: Tile, m1: Seq[Seq[Int]], m2: Seq[Seq[Int]]) extends PeekPokeTester(c) {
  def generateA(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    (0 until c.rows).map { i =>
      m(i) ++ Seq.fill(c.rows*2)(0)
    }
  }

  def generateB(m: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val mT = m.transpose
    (0 until c.columns).map { i =>
      (mT(i) ++ Seq.fill(c.columns*2)(0))
    }
  }

  def generateS: Seq[Seq[Int]] = {
    (0 until c.columns).map { i =>
      Seq.fill(c.rows)(0) ++ Seq.fill(c.rows)(1) ++ Seq.fill(c.rows)(1)
    }
  }

  def generateC(cGold: Seq[Seq[Int]]): Seq[Seq[Tuple2[Int, Boolean]]]= {
    val cGoldT = cGold.transpose
    (0 until c.columns).map { i =>
      Seq.fill(c.rows)((0, false)) ++ cGoldT(i).reverse.map((_, true)) ++ Seq.fill(c.rows)((0, false))
    }.transpose
  }

  println("Generating a:")
  val aFormat = generateA(m1)
  print2DArray(aFormat)

  println("Generating b:")
  val bFormat = generateB(m2)
  print2DArray(bFormat)

  println("Generating s:")
  val sFormat = generateS
  print2DArray(sFormat)

  println("Generating cGold:")
  val cGold = SystolicUtils.mult(m1, m2)
  print2DArray(cGold)

  println("Generating C:")
  val C = generateC(cGold)
  print2DArray(C)

  reset()
  c.io.in_s_vec.foreach { s =>
    poke(s, 0)
  }

  def strobeInputs(cycle: Int): Unit = {
    for (i <- 0 until c.io.in_a_vec.length) {
      poke(c.io.in_a_vec(i), aFormat(i)(cycle))
      poke(c.io.in_b_vec(i), bFormat(i)(cycle))
      poke(c.io.in_s_vec(i), sFormat(i)(cycle))
    }
  }

  println("Peeking output out_vec")
  for (cycle <- aFormat(0).indices) {
    // TODO: ordering matters (strobe, peek, step) for combinational modules
    strobeInputs(cycle)
    val peeked = peek(c.io.out_c_vec)
    step(1)
    assert(peeked.zip(C(cycle)).forall {
      case (actual, (expected, true)) => actual == expected
      case (_, (_, false)) => true
    })
    println(peeked.map(_.toString).reduce(_ + "\t" + _))
  }
}

class TileTester extends ChiselFlatSpec {
  val m1 = Seq(
    Seq(10, 3),
    Seq(2, 13)
  )

  val m2 = Seq(
    Seq(2, 4),
    Seq(3, 9)
  )

  "Simple 2x2 combinational tile" should "calculate C correctly" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"),
      () => new Tile(16, Dataflow.BOTH, 2, 2)) {
      c => new TileUnitTest(c, m1, m2)
    } should be (true)
  }
}

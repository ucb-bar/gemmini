// See README.md for license details.

package systolic

import breeze.linalg.DenseMatrix
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class SystolicUnitTest(c: Mesh) extends PeekPokeTester(c) {
  val m = Array(
    Array(1, 2),
    Array(4, 4)
  )
  def generateA(m: Array[Array[Int]]): Array[Array[Int]] = {
    (0 until c.rows).map { i =>
      (Seq.fill(i)(0) ++ m(i) ++ Seq.fill(c.rows*2 - i)(0)).toArray
    }.toArray
  }

  def generateB(m: Array[Array[Int]]): Array[Array[Int]] = {
    (0 until c.columns).map { i =>
      (Seq.fill(i)(0) ++ m.flatMap(r => r.zipWithIndex.collect {
        case (x, j) if j == i => x
      }) ++ Seq.fill(c.columns*2 - i)(0)).toArray
    }.toArray
  }

  def generateS: Array[Array[Int]] = {
    (0 until c.columns).map { i =>
      Array.fill(i)(0) ++ Array.fill(c.rows)(0) ++ Array.fill(c.rows)(1) ++ Array.fill(c.rows - i)(0)
    }.toArray
  }

  reset()
  c.io.in_s_vec.foreach { s =>
    poke(s, 0)
  }

  println("Generating a:")
  val aFormat = generateA(m)
  for (i <- aFormat) {
    for (j <- i) {
      print(j.toString + " ")
    }
    println()
  }

  println("Generating b:")
  val bFormat = generateB(m)
  for (i <- bFormat) {
    for (j <- i) {
      print(j.toString + " ")
    }
    println()
  }

  println("Generating s:")
  val sFormat = generateS
  for (i <- sFormat) {
    for (j <- i) {
      print(j.toString + " ")
    }
    println()
  }

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

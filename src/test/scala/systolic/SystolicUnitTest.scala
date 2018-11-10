// See README.md for license details.

package systolic

import breeze.linalg.DenseMatrix
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class SystolicUnitTest(c: Mesh) extends PeekPokeTester(c) {
  val m1 = Array(
    Array(10, 3),
    Array(2, 13)
  )

  val m2 = Array(
    Array(2, 4),
    Array(3, 9)
  )

  def generateA(m: Array[Array[Int]]): Array[Array[Int]] = {
    (0 until c.rows).map { i =>
      (Seq.fill(i)(0) ++ m(i) ++ Seq.fill(c.rows*2 - i)(0)).toArray
    }.toArray
  }

  def generateB(m: Array[Array[Int]]): Array[Array[Int]] = {
    val mT = m.transpose
    (0 until c.columns).map { i =>
      (Seq.fill(i)(0) ++ mT(i) ++ Seq.fill(c.columns*2 - i)(0)).toArray
    }.toArray
  }

  def generateS: Array[Array[Int]] = {
    (0 until c.columns).map { i =>
      Array.fill(i)(0) ++ Array.fill(c.rows)(0) ++ Array.fill(c.rows)(1) ++ Array.fill(c.rows - i)(0)
    }.toArray
  }

  def mult[A](a: Array[Array[A]], b: Array[Array[A]])(implicit n: Numeric[A]) = {
    import n._
    for (row <- a)
      yield for(col <- b.transpose)
        yield (row zip col map Function.tupled(_*_) reduceLeft (_+_))
  }

  def generateC(cGold: Array[Array[Int]]): Array[Array[Tuple2[Int, Boolean]]]= {
    val cGoldT = cGold.transpose
    (0 until c.columns).map { i =>
      Array.fill(c.rows + i + 1)((0, false)) ++ cGoldT(i).reverse.map((_, true)) ++ Array.fill(c.rows - 1 - i)((0, false))
    }.toArray
  }

  reset()
  c.io.in_s_vec.foreach { s =>
    poke(s, 0)
  }

  println("Generating a:")
  val aFormat = generateA(m1)
  for (i <- aFormat) {
    for (j <- i) {
      print(j.toString + " ")
    }
    println()
  }

  println("Generating b:")
  val bFormat = generateB(m2)
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

  println("Generating cGold:")
  val cGold = mult(m1, m2)
  for (i <- cGold) {
    for (j <- i) {
      print(j.toString + " ")
    }
    println()
  }

  println("Generating C:")
  val C = generateC(cGold.map(_.toArray)).transpose
  for (i <- C) {
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
    val peeked = peek(c.io.out_vec)
    assert(peeked.zip(C(cycle)).forall {
      case (actual, (expected, true)) => actual == expected
      case (_, (_, false)) => true
    })
    println(peeked.map(_.toString).reduce(_ + "\t" + _))
  }
}

class SystolicTester extends ChiselFlatSpec {
  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle"), () => new Mesh(16, 2, 2)) {
      c => new SystolicUnitTest(c)
    } should be (true)
  }
}

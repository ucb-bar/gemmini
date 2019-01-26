
package systolic

import chisel3._
import chisel3.iotesters._
import systolic.SystolicUtils._

object x {
  type Matrix = Seq[Seq[Int]]
}

class MeshWithMemoryUnitTest(c: MeshWithMemory, ms: Seq[Tuple2[Matrix[Int], Matrix[Int]]], garbageCyles: () => Int)
  extends PeekPokeTester(c)
{
  def strobeInputs[T <: Bits](m: Seq[Int], input: Vec[Vec[T]]): Unit = {
    poke(c.io.valid, true)

    val slices = m.grouped(input.head.length).toList

    for ((slice, i) <- slices.zipWithIndex) {
      for ((elem, j) <- slice.zipWithIndex) {
        poke(input(i)(j), elem)
      }
    }
  }

  // The matrices must be perfectly sized for this unit test
  assert(ms.forall{ case (m1, m2) =>
    rows(m1) == c.meshRows * c.tileRows && cols(m2) == c.meshColumns * c.tileColumns &&
      cols(m1) == rows(m2) && cols(m1) == c.sramEntries
  }, "Array must be square and the matrices must be the same size as the array") // TODO get rid of square requirement

  var mesh_output = Seq.empty[Seq[Tuple2[Int, Int]]]

  def updateOutput(): Unit = {
    mesh_output = (peek(c.io.out_c).map(_.toInt) zip peek(c.io.out_s).map(_.toInt)) +: mesh_output
  }

  reset()

  // First, flush out the initial garbage output. Basically, we wait for the out_s signals to propagate through all
  // the shift registers to the output
  while (peek(c.io.out_s).contains(0))
    step(1)

  // Input all matrices
  for ((mA, mB) <- ms) {
    val A = mA.transpose
    val B = mB

    /*
    println("A:")
    print2DArray(A)
    println("B:")
    print2DArray(B)
    */

    for ((a, b) <- A zip B) {
      strobeInputs(a, c.io.a)
      strobeInputs(b, c.io.b)

      var garbage_cycles = garbageCyles() + 1

      do {
        step(1)
        updateOutput()

        // Put in garbage data
        poke(c.io.valid, false)
        garbage_cycles -= 1

      } while (peek(c.io.ready) == 0 // Wait for the systolic array to be ready for more inputs
        || garbage_cycles > 0)
    }
  }

  // Pass in garbage data till all the results are read out
  var cycles_left = (rows(ms.head._1) + c.meshColumns) * 2

  poke(c.io.valid, true)
  while (cycles_left > 0) {
    step (1)
    updateOutput()
    if (peek(c.io.ready) != 0)
      cycles_left -= 1
  }

  // println("Mesh output:")
  // print2DArray(mesh_output)

  // Extract the results from the output
  var output_matrices = Seq(Seq(mesh_output.head.map(_._1)))
  for (i <- 1 until mesh_output.length) {
    val current_s = mesh_output(i).head._2
    val last_s = mesh_output(i-1).head._2

    val current_c = mesh_output(i).map(_._1)

    if (current_s == last_s) {
      output_matrices = output_matrices.init :+ (output_matrices.last :+ current_c)
    } else {
      output_matrices = output_matrices :+ Seq(current_c)
    }
  }

  output_matrices = output_matrices.dropRight(2).takeRight(ms.length) // Drop initial garbage data from startup
  output_matrices = output_matrices.map(om => om takeRight rows(ms.head._1)) // Drop garbage output from each readout
  output_matrices = output_matrices.reverse

  // Get the gold results
  val golds = ms.map(t => mult(t._1, t._2))

  // Compare the gold results to the systolic array's outputs
  for ((out, gold) <- output_matrices zip golds) {
    /*
    println("Result:")
    print2DArray(out)
    println("Gold:")
    print2DArray(gold)
    println()
    */

    assert(out == gold, "Array output is not correct")
  }
}

class MeshWithMemoryTester extends ChiselFlatSpec {
  // Fully combinational
  "MeshWithMemoryTest" should "work fully combinationally with no delays" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new MeshWithMemory(16, 8, 8, 1, 1, 8))
    {
      c => new MeshWithMemoryUnitTest(c, Seq.fill(8)((rand(8, 8), rand(8, 8))), () => 0)
    } should be (true)
  }

  it should "work fully combinationally with random delays" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new MeshWithMemory(16, 8, 8, 1, 1, 8))
    {
      c => new MeshWithMemoryUnitTest(c, Seq.fill(8)((rand(8, 8), rand(8, 8))), () => scala.util.Random.nextInt(5))
    } should be (true)
  }

  // Fully pipelined
  it should "work fully pipelined with no delays" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new MeshWithMemory(16, 1, 1, 8, 8, 8))
    {
      c => new MeshWithMemoryUnitTest(c, Seq.fill(8)((rand(8, 8), rand(8, 8))), () => 0)
    } should be (true)
  }

  it should "work fully pipelined with random delays" in {
    iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new MeshWithMemory(16, 1, 1, 8, 8, 8))
    {
      c => new MeshWithMemoryUnitTest(c, Seq.fill(8)((rand(8, 8), rand(8, 8))), () => scala.util.Random.nextInt(5))
    } should be (true)
  }

  // Partly pipelined
  it should "work partially pipelined with no delays, as well as with random delays" in {
    val matrix_dim = 8
    val factors = (1 to matrix_dim).filter(matrix_dim % _ == 0)

    val sram_entries = matrix_dim // TODO this may change when the square requirement is lifted

    val delay_functions = Seq(() => 0, () => scala.util.Random.nextInt(5))

    for (pipeline_depth <- factors) {
      val tile_dim = pipeline_depth
      val mesh_dim = matrix_dim / pipeline_depth

      for (delay_function <- delay_functions) {
        iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
          () => new MeshWithMemory(16, tile_dim, tile_dim, mesh_dim, mesh_dim, sram_entries))
        {
          c => new MeshWithMemoryUnitTest(c, Seq.fill(8)((rand(matrix_dim, matrix_dim), rand(matrix_dim, matrix_dim))),
            delay_function)
        } should be(true)
      }
    }
  }
}

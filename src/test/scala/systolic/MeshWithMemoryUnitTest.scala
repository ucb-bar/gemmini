
package systolic

import chisel3._
import chisel3.iotesters._
import SystolicUtils._

// TODO add test for inputting in different orders
// TODO add test for switching dataflow at runtime
// TODO add test for randomly choosing S

abstract class MeshWithMemoryUnitTest(c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]],
                                      inputGarbageCycles: () => Int, outputGarbageCycles: () => Int) extends PeekPokeTester(c)
{
  case class MeshInput(A: Matrix[Int], B: Matrix[Int], D: Matrix[Int], S: Int, M: Int)

  def strobeInputs[T <: Bits](m: Seq[Int], input: Vec[Vec[T]], valid: Bool): Unit = {
    poke(valid, true)

    val slices = m.grouped(input.head.length).toList

    for ((slice, i) <- slices.zipWithIndex) {
      for ((elem, j) <- slice.zipWithIndex) {
        poke(input(i)(j), elem)
      }
    }
  }

  def pokeAllInputValids(v: Boolean): Unit = {
    Seq(c.io.a.valid, c.io.b.valid, c.io.d.valid, c.io.s.valid, c.io.m.valid).foreach(vpin => poke(vpin, v))
  }

  def allInputsAreReady(): Boolean = {
    // Ignore m and s here, since they're only supposed to be set once per multiplication
    Seq(c.io.a.ready, c.io.b.ready, c.io.d.ready).forall(r => peek(r) != 0)
  }

  // The matrices must be perfectly sized for this unit test
  assert(ms.forall{ case (m1, m2, m3) =>
    rows(m1) == c.meshRows * c.tileRows && cols(m2) == c.meshColumns * c.tileColumns &&
      cols(m1) == rows(m2) && cols(m1) == c.sramEntries &&
      rows(m3) == c.meshRows*c.tileRows && cols(m3) == c.meshColumns*c.tileColumns
  }, "Array must be square and the matrices must be the same size as the array") // TODO get rid of square requirement
  val dim = rows(ms.head._1)

  var mesh_output = Seq.empty[Seq[Tuple2[Int, Int]]]

  def updateOutput(): Unit = {
    val v = peek(c.io.out.valid)
    val peeked = peek(c.io.out.bits).map(_.toInt) zip peek(c.io.out_s).map(_.toInt % 2)

    // if (peek(c.io.out.valid) == 1)
    if (v == 1)
      mesh_output = peeked +: mesh_output
      // mesh_output = (peek(c.io.out.bits).map(_.toInt) zip peek(c.io.out_s).map(_.toInt % 2)) +: mesh_output
  }

  def formatMs(ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]]): Seq[MeshInput]
  def formatOut(outs: Seq[Matrix[Int]]): Seq[Matrix[Int]]

  reset()
  poke(c.io.out.ready, true)

  // Input all matrices
  val meshInputs = formatMs(ms)
  for (meshIn <- meshInputs) {
    /*
    println("A:")
    print2DArray(mA)
    println("B:")
    print2DArray(mB)
    println("D:")
    print2DArray(mD)
    */

    poke(c.io.s.bits, meshIn.S)
    poke(c.io.m.bits, meshIn.M)

    for ((a, b, d) <- (meshIn.A, meshIn.B, meshIn.D).zipped) {
      pokeAllInputValids(true)

      strobeInputs(a, c.io.a.bits, c.io.a.valid)
      strobeInputs(b, c.io.b.bits, c.io.b.valid)
      strobeInputs(d, c.io.d.bits, c.io.d.valid)

      var garbage_cycles = inputGarbageCycles() + 1

      // Feed in garbage data
      do {
        step(1)
        updateOutput()

        // Put in garbage data
        pokeAllInputValids(false)

        garbage_cycles -= 1

      } while (!allInputsAreReady() // Wait for the systolic array to be ready for more inputs
        || garbage_cycles > 0)

      // Now, refuse to read out data
      poke(c.io.out.ready, false)
      step(outputGarbageCycles())
      for (i <- 0 until 1) {
        // step(1)
        // updateOutput()
      }
      poke(c.io.out.ready, true)
    }
  }

  // Pass in garbage data till all the results are read out
  pokeAllInputValids(true)
  strobeInputs(Seq.fill(dim)(0), c.io.d.bits, c.io.d.valid)
  for (i <- 1 to 2) {
    poke(c.io.s.bits, (meshInputs.last.S+i)%2)

    var cycles = rows(ms.head._1) + c.meshColumns
    while (cycles > 0) {
      step(1)
      poke(c.io.s.valid, false)
      updateOutput()
      if (allInputsAreReady()) {
        cycles -= 1
      }
    }
  }

  // println("Mesh output:")
  // print2DArray(mesh_output)

  // Get rid of initial spurious output
  {
    var i = mesh_output.size-1
    var first_ones_reached = false
    while(!first_ones_reached) {
      if (mesh_output(i)(0)._2 == 1)
        first_ones_reached = true
      i -= 1
    }
    mesh_output = mesh_output.take(i+2)
  }

  // println("Mesh output (after pruning):")
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

  output_matrices = formatOut(output_matrices)

  // Get the gold results
  val golds = ms.map(t => add(t._3, mult(t._1, t._2)))

  /*
  // Compare the gold results to the systolic array's outputs
  for ((out, gold) <- output_matrices zip golds) {
    println("Result:")
    print2DArray(out)
    println("Gold:")
    print2DArray(gold)
    println()
  }
  for (gold <- golds drop output_matrices.size) {
    println("Gold (no result):")
    print2DArray(gold)
    println()
  }
  Console.flush()
  */

  assert(output_matrices == golds, "Array output is not correct")
}

class OSMeshWithMemoryUnitTest(c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]],
                               inputGarbageCyles: () => Int, outputGarbageCycles: () => Int)
  extends MeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCycles)
{
  override def formatMs(ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]]) = {
    // Shift the D matrices down so that they are input at the correct time
    val shifted = (zero(dim), zero(dim), ms.head._3) +:
      (ms.tail zip ms).map { case ((_, _, d), (a, b, _)) => (a, b, d) } :+
      (ms.last._1, ms.last._2, zero(dim))

    // Then, transpose A and reverse the rows of D
    val mats = shifted.map{case (a, b, d) => (a.transpose, b, d.reverse)}

    // Finally, add the S and M parameters
    mats.zipWithIndex.map { case ((m1,m2,m3),i) => MeshInput(m1, m2, m3, S=i%2, M=0)}
  }

  override def formatOut(outs: Seq[Matrix[Int]]) = {
    outs.dropRight(3).takeRight(ms.length). // Drop initial garbage data from startup
      map(om => om takeRight rows(ms.head._1)) // Drop garbage output from each readout
      .reverse
  }
}

class WSMeshWithMemoryUnitTest(c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]],
                               inputGarbageCyles: () => Int, outputGarbageCycles: () => Int)
  extends MeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCycles)
{
  override def formatMs(ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]]) = {
    // Shift the B matrices down so that they are input at the correct time
    val shifted = (zero(dim), ms.head._2, zero(dim)) +:
      (ms.tail zip ms).map { case ((_, b, _), (a, _, d)) => (a, b, d) } :+
      (ms.last._1, zero(dim), ms.last._3)

    // Then, reverse B and change the positions of A, B, and D since the IO names are only correct for output-stationary
    val mats = shifted.map{case (a, b, d) => (a, d, b.reverse)}

    // Finally, add the S and M parameters
    mats.zipWithIndex.map { case ((m1,m2,m3),i) => MeshInput(m1, m2, m3, S=i%2, M=1)}
  }

  override def formatOut(outs: Seq[Matrix[Int]]) = {
    outs.dropRight(2).takeRight(ms.length). // Drop initial garbage data from startup
      map(om => om.reverse take dim). // Drop garbage output from each readout and reverse the rows
      reverse
  }
}

class MeshWithMemoryTester extends ChiselFlatSpec
{
  val dataflows = Seq((c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]], inputGarbageCyles: () => Int, outputGarbageCyles: () => Int) => new OSMeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCyles),
    (c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]], inputGarbageCyles: () => Int, outputGarbageCyles: () => Int) => new WSMeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCyles))

  // val dataflows = Seq((c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]], inputGarbageCyles: () => Int, outputGarbageCyles: () => Int) => new OSMeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCyles))
  // val dataflows = Seq((c: MeshWithMemory, ms: Seq[Tuple3[Matrix[Int], Matrix[Int], Matrix[Int]]], inputGarbageCyles: () => Int, outputGarbageCyles: () => Int) => new WSMeshWithMemoryUnitTest(c, ms, inputGarbageCyles, outputGarbageCyles))

  "SimpleMeshWithMemoryTester" should "work" in {
    // This is really just here to help with debugging
    val dim = 2

    for (df <- dataflows.slice(0, 1)) {
      iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
        // () => new MeshWithMemory(16, 1, 1, dim, dim, dim)) {
        () => new MeshWithMemory(16, dim, dim, 1, 1, dim)) {
          c => new WSMeshWithMemoryUnitTest(c, Seq((zero(dim), zero(dim), consecutive(dim)), (zero(dim), zero(dim), consecutive(dim).map(_.map(_*10))), (identity(dim), identity(dim), identity(dim))), () => 1, () => 1)
      } should be(true)
    }
  }

  // Fully combinational
  "MeshWithMemoryTest" should "work fully combinationally with no delays" in {
    for (df <- dataflows) {
      iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
        () => new MeshWithMemory(16, 8, 8, 1, 1, 8)) {
        c => df(c, Seq.fill(8)((rand(8, 8), rand(8, 8), rand(8, 8))), () => 0, () => 0)
      } should be(true)
    }
  }

  it should "work fully combinationally with random delays" in {
    for (df <- dataflows) {
      iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
        () => new MeshWithMemory(16, 8, 8, 1, 1, 8)) {
        c => df(c, Seq.fill(8)((rand(8, 8), rand(8, 8), rand(8, 8))), () => scala.util.Random.nextInt(5), () => scala.util.Random.nextInt(5))
      } should be(true)
    }
  }

  // Fully pipelined
  it should "work fully pipelined with no delays" in {
    for (df <- dataflows) {
      iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
        () => new MeshWithMemory(16, 1, 1, 8, 8, 8)) {
        c => df(c, Seq.fill(8)((rand(8, 8), rand(8, 8), rand(8, 8))), () => 0, () => 0)
      } should be(true)
    }
  }

  it should "work fully pipelined with random delays" in {
    for (df <- dataflows) {
      iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
        () => new MeshWithMemory(16, 1, 1, 8, 8, 8)) {
        c => df(c, Seq.fill(8)((rand(8, 8), rand(8, 8), rand(8, 8))), () => scala.util.Random.nextInt(5), () => scala.util.Random.nextInt(5))
      } should be(true)
    }
  }

  // Partly pipelined
  it should "work partially pipelined with no delays, as well as with random delays" in {
    val matrix_dim = 8
    val factors = (1 to matrix_dim).filter(matrix_dim % _ == 0).tail.init

    val sram_entries = matrix_dim // TODO this may change when the square requirement is lifted

    val delay_functions = Seq(() => 0, () => scala.util.Random.nextInt(5))

    for (pipeline_depth <- factors) {
      val tile_dim = pipeline_depth
      val mesh_dim = matrix_dim / pipeline_depth

      for (delay_function <- delay_functions; df <- dataflows) {
        iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
          () => new MeshWithMemory(16, tile_dim, tile_dim, mesh_dim, mesh_dim, sram_entries)) {
            c => df(c, Seq.fill(8)((rand(matrix_dim, matrix_dim), rand(matrix_dim, matrix_dim),
              zero(matrix_dim))), delay_function, delay_function)
        } should be(true)
      }
    }
  }
}

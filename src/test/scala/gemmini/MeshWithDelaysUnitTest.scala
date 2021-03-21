
package gemmini

import chisel3._
import chisel3.iotesters._
import TestUtils._
import scala.util.Random.shuffle

// TODO add test for randomly choosing S
// TODO add test for inputting A, B, and D in different orders
// TODO add test for switching dataflow at runtime
// TODO get a better initialization strategy

case class MeshTesterInput(A: Matrix[Int], B: Matrix[Int], D: Matrix[Int], flipS: Boolean)

abstract class MeshWithDelaysUnitTest(c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput],
                                      inputGarbageCycles: () => Int, shift: Int = 0,
                                      verbose: Boolean = false)
  extends PeekPokeTester(c)
{
  case class MeshInput(A: Matrix[Int], B: Matrix[Int], D: Matrix[Int], S: Int, M: Int, tag: Int)
  case class MeshOutput(C: Matrix[Int], tag: Int)

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
    val valids = Seq(c.io.a.valid, c.io.b.valid, c.io.d.valid, c.io.s, c.io.tag_in.valid)
    valids.foreach(vpin => poke(vpin, v))
  }

  def allMatrixInputsAreReady(): Boolean = {
    // Ignore m and s here, since they're only supposed to be set once per multiplication
    Seq(c.io.a.ready, c.io.b.ready, c.io.d.ready).forall(r => peek(r) != 0)
  }

  assert(ms.head.flipS != 0, "Cannot re-use D for first input")

  // The matrices must be perfectly sized for this unit test
  assert(ms.forall{ case MeshTesterInput(a, b, d, _) => // case (m1, m2, m3) =>
    rows(d) == c.meshRows * c.tileRows && cols(d) == c.meshColumns * c.tileColumns &&
      rows(d) == cols(d) &&
      dims(a) == dims(d) && dims(b) == dims(d)
  }, "Array must be square and the matrices must be the same size as the array") // TODO get rid of square requirement
  val dim = rows(ms.head.D)

  type RawMeshOutputT = Tuple3[Seq[Int], Int, Int]
  var raw_mesh_output = Seq.empty[RawMeshOutputT]

  def updateOutput(): Unit = {
    if (peek(c.io.out.valid) == 1) {
      val peek_c = peek(c.io.out.bits).map(_.toInt)
      val peek_s = peek(c.io.out_s).map(_.toInt % 2).reduce { (acc, s) =>
          assert(acc == s, "s values aren't all the same")
          acc
      }
      val peek_tag = peek(c.io.tag_out).toInt
      if (peek_tag != -1)
        raw_mesh_output = (peek_c, peek_s, peek_tag) +: raw_mesh_output
    }
  }

  def startup(getOut: Boolean): Unit = {
    poke(c.io.tag_garbage, -1)
    poke(c.io.shift, shift)
    poke(c.io.flush.bits, 2)
    reset()
    poke(c.io.flush.valid, 1)
    do {
      step(1)
      poke(c.io.flush.valid, 0)
      if (getOut)
        updateOutput()
    } while (peek(c.io.flush.ready) == 0)
    reset()
  }

  def formatMs(ms: Seq[MeshTesterInput]): Seq[MeshInput]
  def formatOut(outs: Seq[Matrix[Int]], tags: Seq[Int]): Seq[MeshOutput]
  def goldResults(ms: Seq[MeshTesterInput]): Seq[Matrix[Int]]

  startup(false)

  // Input all matrices
  val meshInputs = formatMs(ms)
  for (meshIn <- meshInputs) {
    /*
    println(s"Tag: ${meshIn.tag}")
    println(s"FlipS: ${meshIn.S}")
    println("A:")
    print2DArray(meshIn.A)
    println("B:")
    print2DArray(meshIn.B)
    println("D:")
    print2DArray(meshIn.D)
    */

    poke(c.io.s, meshIn.S)
    poke(c.io.m, meshIn.M)
    poke(c.io.tag_in.bits, meshIn.tag)

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

      } while (!allMatrixInputsAreReady() // Wait for the systolic array to be ready for more inputs
        || garbage_cycles > 0)
    }
  }

  // Flush out the final results
  poke(c.io.s, 1)
  poke(c.io.flush.valid, 1)
  do {
    step(1)
    poke(c.io.flush.valid, 0)
    updateOutput()
  } while (peek(c.io.flush.ready) == 0)

  if (verbose) {
    println("Mesh output:")
    print2DArray(raw_mesh_output.map { case (seq, i, j) => seq.map((_, i, j)) })
    println("Mesh output (without tags):")
    print2DArray(raw_mesh_output.map { case (seq, i, _) => seq.map((_, i)) })
  }

  // Extract the results from the output
  var output_matrices = Seq(Seq(raw_mesh_output.head._1))
  var output_tags_arrays = Seq(Seq(raw_mesh_output.head._3))
  for (i <- 1 until raw_mesh_output.length) {
    val last_s = raw_mesh_output(i-1)._2
    val (current_c, current_s, current_tag) = raw_mesh_output(i)

    if (current_s == last_s) {
      output_matrices = output_matrices.init :+ (output_matrices.last :+ current_c)
      output_tags_arrays = output_tags_arrays.init :+ (output_tags_arrays.last :+ current_tag)
    } else {
      output_matrices = output_matrices :+ Seq(current_c)
      output_tags_arrays = output_tags_arrays :+ Seq(current_tag)
    }
  }

  // TODO add this back in when tag tests are fixed
  /*assert(output_tags_arrays.forall { ta =>
    ta.takeRight(dim).toSet.size == 1
  }, "output tags do not remain constant when they should")*/

  val output_tags = output_tags_arrays.map(_.last)
  val results = formatOut(output_matrices, output_tags)

  // Get the gold results
  val golds = goldResults(ms)

  // Compare the gold results to the systolic array's outputs
  if (verbose) {
    for ((MeshOutput(out, tag), gold) <- results zip golds) {
      println(s"Tag: $tag")
      println("Result:")
      print2DArray(out)
      println("Gold:")
      print2DArray(gold)
      println()
    }
    for (MeshOutput(out, tag) <- results drop golds.size) {
      println(s"Tag (no result): $tag")
      println("Result (no result):")
      print2DArray(out)
      println()
    }
    for (gold <- golds drop results.size) {
      println("Gold (no result):")
      print2DArray(gold)
      println()
    }
    Console.flush()
  }

  assert(results.map(_.C) == golds, "Array output is not correct")
  assert(results.map(_.tag) == meshInputs.init.map(_.tag), "Array tags are not correct")
}

class OSMeshWithDelaysUnitTest(c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput],
                               inputGarbageCyles: () => Int, shift: Int = 0,
                               verbose: Boolean = false)
  extends MeshWithDelaysUnitTest(c, ms, inputGarbageCyles, shift, verbose = verbose)
{
  override def formatMs(ms: Seq[MeshTesterInput]) = {
    // Shift the A and D matrices down so that they are input at the correct time
    val shifted = (ms.head.A, zero(dim), ms.head.D, true) +:
      (ms.tail zip ms).map { case (MeshTesterInput(a, _, d, _), MeshTesterInput(_, b, _, s)) => (a, b, d, s) } :+
      (zero(dim), ms.last.B, zero(dim), ms.last.flipS /* the last value needs to be read out, so we need to flip S */)

    // Then, reverse the rows of D
    val mats = shifted.map{case (a, b, d, s) => (a, b, d.reverse, s)}

    // Finally, add the S and M parameters
    mats.zipWithIndex.map { case ((m1,m2,m3,s),i) => MeshInput(m1, m2, m3, S=s.toInt, M=0, tag=i)}
  }

  override def formatOut(outs: Seq[Matrix[Int]], tags: Seq[Int])= {
    (outs zip tags).take(ms.length). // Drop initial garbage data from startup
      map(t => (t._1 takeRight dim, t._2)).
      reverse.
      map(t => MeshOutput(t._1, t._2))
  }

  override def goldResults(ms: Seq[MeshTesterInput]) = {
    def helper(ms: List[MeshTesterInput], last: Matrix[Int]): List[Matrix[Int]] = ms match {
      case Nil => Nil

      case MeshTesterInput(a, b, d, flipS) :: (mnext @ MeshTesterInput(_, _, _, nextFlipS)) :: mstail =>
        val preloaded = if (flipS) d else last
        val new_last = add(preloaded, mult(a,b))

        if (nextFlipS)
          new_last :: helper(mnext :: mstail, new_last)
        else
          helper(mnext :: mstail, new_last)

      case MeshTesterInput(a, b, d, flipS) :: mstail =>
        val preloaded = if (flipS) d else last
        val new_last = add(preloaded, mult(a,b))
        new_last +: helper(mstail, new_last)
    }

    def rounding_shift(x: Int, s: Int) = {
      val abs = if (x > 0) x else -x
      val div = 1 << s
      val abs_result = (abs + (div / 2)) / div
      if (x > 0) abs_result else -abs_result
    }

    helper(ms.toList, null).map(_.map(_.map(i => rounding_shift(i, shift) & ~((-1) << c.outputType.getWidth))))
  }
}

class WSMeshWithDelaysUnitTest(c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput],
                               inputGarbageCyles: () => Int,
                               verbose: Boolean = false)
  extends MeshWithDelaysUnitTest(c, ms, inputGarbageCyles, verbose = verbose) // WS just ignores shift
{
  override def formatMs(ms: Seq[MeshTesterInput]) = {
    // Shift the B matrices down so that they are input at the correct time
    val shifted = (zero(dim), ms.head.B, zero(dim), true) +:
      (ms.tail zip ms).map { case (MeshTesterInput(_, b, _, s), MeshTesterInput(a, _, d, _)) => (a, b, d, s) } :+
      (ms.last.A, zero(dim), ms.last.D, true)

    // Then, reverse B and change the positions of A, B, and D since the IO names are only correct for output-stationary
    val mats = shifted.map{case (a, b, d, s) => (a, d, b.reverse, s)}

    // Finally, add the S and M parameters
    mats.zipWithIndex.map { case ((m1,m2,m3,s),i) => MeshInput(m1, m2, m3, S=s.toInt, M=1, tag=i)}
  }

  override def formatOut(outs: Seq[Matrix[Int]], tags: Seq[Int]) = {
    // TODO
    (outs zip tags).takeRight(ms.length). // Drop initial garbage data from startup
      map(t => (t._1.reverse, t._2)). // Reverse the rows
      reverse.
      map(t => MeshOutput(t._1, t._2))
  }

  override def goldResults(ms: Seq[MeshTesterInput]) = {
    def helper(ms: List[MeshTesterInput], last: Matrix[Int]): List[Matrix[Int]] = ms match {
      case Nil => Nil
      case MeshTesterInput(a, b, d, flipS) :: mstail =>
        val preloaded = if (flipS) b else last
        val new_last = add(d, mult(a, preloaded))
        new_last +: helper(mstail, new_last)
    }

    helper(ms.toList, null)
  }
}

class MeshWithDelaysTester extends ChiselFlatSpec
{
  val dataflow_testers = Seq((c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput], inputGarbageCyles: () => Int, shift: Int) => new OSMeshWithDelaysUnitTest(c, ms, inputGarbageCyles, shift),
    (c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput], inputGarbageCyles: () => Int, shift: Int) => new WSMeshWithDelaysUnitTest(c, ms, inputGarbageCyles))

  "SimpleMeshWithDelaysTester" should "work" in {
    // This is really just here to help with debugging
    val dim = 4

    iotesters.Driver.execute(Array("--backend-name", "treadle", "--generate-vcd-output", "on"),
      () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), Dataflow.WS, 1, 1, dim, dim,1, 1)) {
        // c => new OSMeshWithDelaysUnitTest(c, Seq.fill(1)(MeshTesterInput(rand(dim), rand(dim), rand(dim), true)), () => 0, shift = 0, verbose = true)
        c => new WSMeshWithDelaysUnitTest(c, Seq.fill(1)(MeshTesterInput(rand(dim), rand(dim), rand(dim), true)), () => 0, verbose = true)
    } should be(true)
  }

  // Fully combinational
  "MeshWithDelaysTest" should "work fully combinationally with no delays" in {
    for (df <- dataflow_testers) {
      iotesters.Driver.execute(Array("--backend-name", "treadle"),
        () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), Dataflow.BOTH, 8, 8, 1, 1, 1, 1)) {
        c => df(c, Seq.fill(8)(MeshTesterInput(rand(8), rand(8), rand(8), true)), () => 0, 0)
      } should be(true)
    }
  }

  it should "work fully combinationally with random delays" in {
    for (df <- dataflow_testers) {
      iotesters.Driver.execute(Array("--backend-name", "treadle"),
        () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), Dataflow.BOTH, 8, 8, 1, 1, 1, 1)) {
        c => df(c, Seq.fill(8)(MeshTesterInput(rand(8), rand(8), rand(8), true)), () => scala.util.Random.nextInt(5), 0)
      } should be(true)
    }
  }

  // Fully pipelined
  it should "work fully pipelined with no delays" in {
    for (df <- dataflow_testers) {
      iotesters.Driver.execute(Array("--backend-name", "treadle"),
        () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), Dataflow.BOTH, 1, 1, 8, 8, 1, 1)) {
        c => df(c, Seq.fill(8)(MeshTesterInput(rand(8), rand(8), rand(8), true)), () => 0, 0)
      } should be(true)
    }
  }

  it should "work fully pipelined with random delays" in {
    for (df <- dataflow_testers) {
      iotesters.Driver.execute(Array("--backend-name", "treadle"),
        () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), Dataflow.BOTH, 1, 1, 8, 8, 1, 1)) {
        c => df(c, Seq.fill(8)(MeshTesterInput(rand(8), rand(8), rand(8), true)), () => scala.util.Random.nextInt(5), 0)
      } should be(true)
    }
  }

  // Arbitrarily pipelined
  it should "work arbitrarily pipelined with no delays, as well as with random delays, with all possible dataflows, with all possible banking strategies, with many different array sizes" in {
    // TODO add these back in
    /*val dataflows = Seq((Dataflow.OS, Seq((c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput], inputGarbageCyles: () => Int, shift: Int) => new OSMeshWithDelaysUnitTest(c, ms, inputGarbageCyles, shift))),
      (Dataflow.WS, Seq((c: MeshWithDelays[SInt, UInt], ms: Seq[MeshTesterInput], inputGarbageCyles: () => Int, shift: Int) => new WSMeshWithDelaysUnitTest(c, ms, inputGarbageCyles))),
      (Dataflow.BOTH, dataflow_testers))*/

    val dataflows = Seq((Dataflow.BOTH, dataflow_testers))

    val delay_functions = Seq(() => 0, () => scala.util.Random.nextInt(5))

    val shifts = Seq(0, 4, 8, 12) // TODO test more shifts later

    for (matrix_dim <- 8 to 8) { // TODO test more sizes later
      val factors = (1 to matrix_dim).filter(matrix_dim % _ == 0)

      // The for loops in here are written very strangely, mainly just so they can be parallelized
      // for (tile_height <- factors; tile_width <- factors) {
      shuffle(for (tile_height <- factors; tile_width <- factors) yield (tile_height, tile_width)).par.foreach { case (tile_height, tile_width) =>
        val mesh_height = matrix_dim / tile_height
        val mesh_width = matrix_dim / tile_width

        val left_bankings = (1 to mesh_height).filter(mesh_height % _ == 0)
        val up_bankings = (1 to mesh_width).filter(mesh_width % _ == 0)

        // for (in_delay <- delay_functions; left_banks <- left_bankings; up_banks <- up_bankings; out_banks <- up_bankings; df_with_tester <- dataflows; shift <- shifts) {
        shuffle(for (in_delay <- delay_functions; left_banks <- left_bankings; up_banks <- up_bankings; out_banks <- up_bankings; df_with_tester <- dataflows; shift <- shifts)
          yield (in_delay, left_banks, up_banks, out_banks, df_with_tester, shift)).par.foreach { case (in_delay, left_banks, up_banks, out_banks, df_with_tester, shift) =>

          val df = df_with_tester._1
          val df_testers = df_with_tester._2

          for (dft <- df_testers) {
            iotesters.Driver.execute(Array("--backend-name", "treadle"),
              () => new MeshWithDelays(SInt(8.W), SInt(16.W), SInt(32.W), UInt(32.W), df, tile_height, tile_width, mesh_height, mesh_width, left_banks, up_banks, out_banks)) {
              c =>
                dft(c, Seq.fill(8)(MeshTesterInput(rand(matrix_dim), rand(matrix_dim),
                  rand(matrix_dim), true)), in_delay, shift)
            } should be(true)
          }
        }
      }
    }
  }
}

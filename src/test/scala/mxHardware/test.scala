package mxHardware

import chisel3._
import circt.stage.ChiselStage
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MACUSpec extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "MACU"

  it should "multiply correctly for all mode combinations (with debug prints)" in {
    test(new MACU(true)).withAnnotations(Seq(VcsBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.reset.poke(true.B); c.clock.step(2); c.reset.poke(false.B)

      def mask(v: Int, mode: Int): Int = if (mode == 0) (v & 0x3) else (v & 0x7) 
      def b(x: Int, w: Int): String = String.format("%" + w + "s", x.toBinaryString).replace(' ', '0')

      def runCase(wMode: Int, aMode: Int, vecs: Seq[(Int, Int)]): Unit = {
        c.io.w_mode.poke(wMode.U)
        c.io.act_mode.poke(aMode.U)

        vecs.foreach { case (wIn, aIn) =>
          val ew = mask(wIn, wMode)
          val ea = mask(aIn, aMode)
          val expected = ew * ea

          c.io.w.poke(wIn.U)
          c.io.act.poke(aIn.U)
          c.io.enable.poke(true.B)

          // Advance one cycle (output valid next cycle)
          c.clock.step(1)

          val got = c.io.output.peek().litValue.toInt

          // Debug print: raw(3b), masked(2/3b), result(6b)
          val wWidth = if (wMode == 0) 2 else 3
          val aWidth = if (aMode == 0) 2 else 3
          println(
            s"[w_mode=$wMode a_mode=$aMode] " +
            s"w_raw=${b(wIn & 0x7, 3)} -> w=${b(ew, wWidth)}  | " +
            s"a_raw=${b(aIn & 0x7, 3)} -> a=${b(ea, aWidth)}  | " +
            s"out=${b(got, 6)} exp=${b(expected, 6)} " +
            (if (got == expected) "✓" else "✗")
          )

          c.io.output.expect(expected.U)
        }

        c.io.enable.poke(false.B)
      }

      // (w_mode=0, act_mode=0): 2-bit x 2-bit
      runCase(0, 0, Seq(
        (0, 0),   // 0
        (1, 3),   // 3
        (2, 3)    // 6
      ))

      // (w_mode=1, act_mode=0): 3-bit x 2-bit
      runCase(1, 0, Seq(
        (4, 3),   // 12
        (7, 2),   // 14
        (5, 1)    // 5
      ))

      // (w_mode=0, act_mode=1): 2-bit x 3-bit
      runCase(0, 1, Seq(
        (3, 7),   // 21
        (2, 5),   // 10
        (1, 4)    // 4
      ))

      // (w_mode=1, act_mode=1): 3-bit x 3-bit
      runCase(1, 1, Seq(
        (7, 7),   // 49
        (6, 5),   // 30
        (3, 2)    // 6
      ))

      println(ChiselStage.emitSystemVerilog(new MACU(true)))
    }
  }
}

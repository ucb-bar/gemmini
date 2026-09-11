package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

/** Exhaustive cross-check of the RTL requant encoders (BF16ToE4M3/E5M2/E3M2/E2M3 inside
  * BF16ScaleRoundToTiny) against the spike/golden codes, which are themselves proven bit-exact vs
  * microxcaling _quantize_elemwise(round='even'). Sweeps every bf16 value with |v| in [2^-12, 2)
  * (scale=127 -> no scaling). Reads the spike code table from $SPIKE_CODES.
  * Row format: bits e4m3 e5m2 e3m2 e2m3  (all hex; e3m2/e2m3 6-bit).
  */
class BF16ToMxExactTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  case class Row(bits: Int, e4m3: Int, e5m2: Int, e3m2: Int, e2m3: Int)

  val rows: Seq[Row] = {
    val path = sys.env.getOrElse("SPIKE_CODES",
      "/users/nicorakela/.claude/jobs/dd0fb166/tmp/spike_codes.txt")
    Source.fromFile(path).getLines().toList.filter(_.trim.nonEmpty).map { l =>
      val p = l.trim.split("\\s+")
      Row(Integer.parseInt(p(0), 16), Integer.parseInt(p(1), 16),
          Integer.parseInt(p(2), 16), Integer.parseInt(p(3), 16), Integer.parseInt(p(4), 16))
    }
  }

  // (name, dataType, altfmt, codeMask, magMask, spike-code selector)
  val formats = Seq(
    ("e4m3", 0, false, 0xFF, 0x7F, (r: Row) => r.e4m3),
    ("e5m2", 0, true,  0xFF, 0x7F, (r: Row) => r.e5m2),
    ("e3m2", 1, false, 0x3F, 0x1F, (r: Row) => r.e3m2),
    ("e2m3", 1, true,  0x3F, 0x1F, (r: Row) => r.e2m3),
  )

  behavior of "BF16ScaleRoundToTiny requant encoders (RTL vs spike golden, RNE)"

  it should s"match the spike golden exactly over ${rows.length} bf16 values for all 4 formats" in {
    test(new BF16ScaleRoundToTiny(outputnumLanes = 4)) { dut =>
      dut.io.scale_e8m0.poke(127.U)          // scale_exp_unbiased = 0 -> identity scaling
      dut.io.block_has_nan.poke(false.B)
      dut.io.block_has_inf.poke(false.B)
      for (i <- 0 until 4) dut.io.in_bf16(i).poke(0.U)

      for ((name, dt, altfmt, codeMask, magMask, sel) <- formats) {
        dut.io.dataType.poke(dt.U)
        dut.io.mx_fp8_altfmt.poke(altfmt.B)
        var mism = 0
        val examples = scala.collection.mutable.ArrayBuffer[String]()
        for (r <- rows) {
          dut.io.in_bf16(0).poke((r.bits & 0xFFFF).U)
          val got = dut.io.out(0).peek().litValue.toInt & codeMask
          val exp = sel(r) & codeMask
          val bothZero = (got & magMask) == 0 && (exp & magMask) == 0
          if (got != exp && !bothZero) {
            mism += 1
            if (examples.length < 8)
              examples += f"bits=0x${r.bits}%04x rtl=0x$got%02x spike=0x$exp%02x"
          }
        }
        println(f"[$name%-5s] mismatches: $mism / ${rows.length}")
        examples.foreach(e => println("    " + e))
        assert(mism == 0, s"$name: $mism mismatches vs spike golden")
      }
    }
  }
}

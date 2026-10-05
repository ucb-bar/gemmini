package gemmini

import chisel3._
import chiseltest._
import chiseltest.simulator.VcsBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

// Module-level simulation of the FP8 projection path through QuantLut:
// load the act_out LUT, feed FP8 values on quant_fp6, and check the projected
// 4-bit indices against a Scala golden (nearest FP8 LUT entry).
class TestQuantLutFp8 extends AnyFlatSpec with ChiselScalatestTester {

  val lutCfg = GemminiLUTConfig(
    numBits    = Seq(128, 128, 128),
    rdataWidth = 8,
    projFormat = LutFP8E4M3
  )
  val lanes = 32

  def decodeE4M3(b: Int): Double = {
    val sign = (b >> 7) & 1
    val exp  = (b >> 3) & 0xF
    val mant = b & 0x7
    val mag =
      if (exp == 0 && mant == 0) 0.0
      else if (exp == 0) (mant.toDouble / 8.0) * math.pow(2, -6)
      else (1.0 + mant.toDouble / 8.0) * math.pow(2, exp - 7)
    if (sign == 1) -mag else mag
  }
  def isNaNE4M3(b: Int): Boolean = (((b >> 3) & 0xF) == 0xF) && ((b & 0x7) == 0x7)

  def goldenNearest(inByte: Int, lut: Seq[Int]): Int = {
    val inV = decodeE4M3(inByte)
    var bestIdx = 0; var bestDist = Double.MaxValue
    for (i <- 0 until 16) {
      val d = math.abs(inV - decodeE4M3(lut(i)))
      if (d < bestDist) { bestDist = d; bestIdx = i }
    }
    bestIdx
  }

  behavior of "QuantLut FP8 projection"

  it should "project FP8 inputs to nearest LUT indices" in {
    test(new QuantLut(
      lutConfig = lutCfg,
      outputnumLanes = lanes,
      sp_bank_entries = 256,
      sp_banks = 4,
      sp_width = 256,
      sp_width_projected = 128,
      lut_update_regularity_w = 128,
      lut_update_regularity_act_in = 128,
      lut_update_regularity_act_out = 128,
      iterator_bitwidth = 16
    )).withAnnotations(Seq(VcsBackendAnnotation)) { dut =>
      // 16-entry act_out LUT (E4M3 codes, no NaN).
      val lut = Seq(0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38,
                    0x80, 0x88, 0x90, 0x98, 0xA0, 0xA8, 0xB0, 0xB8)

      // ---- init / tie-offs ----
      dut.io.lut_write_weight.valid.poke(false.B)
      dut.io.lut_write_act_in.valid.poke(false.B)
      dut.io.lut_write_act_out.valid.poke(false.B)
      dut.io.quant_fp6.valid.poke(false.B)
      dut.io.read_a.poke(false.B)
      dut.io.read_d.poke(false.B)
      dut.io.loop_bound_i.poke(64.U)   // large: no counter reset during the test
      dut.io.loop_bound_j.poke(64.U)
      dut.io.loop_bound_k.poke(64.U)
      dut.io.quant_lut_update_granularity.poke(8.U) // counter(8b) >> 8 == 0 -> always LUT row 0
      for (b <- 0 until 4) {
        dut.io.spad_projected_data(b).resp.valid.poke(false.B)
        dut.io.spad_projected_data(b).req.ready.poke(false.B)
        dut.io.spad_deprojected_data(b).req.valid.poke(false.B)
        dut.io.spad_deprojected_data(b).resp.ready.poke(false.B)
      }
      for (i <- 0 until lanes) dut.io.quant_fp6.bits(i).poke(0.U)
      dut.clock.step(1)

      // ---- write act_out LUT row 0 (16 entries x 8-bit, entry 0 in low bits) ----
      // The write handshake completes once the finder engine has consumed every table (one per cycle); the
      // derived tables land NearestFinderTableEngine.latency cycles after the last one.
      val packed = (0 until 16).foldRight(BigInt(0))((e, acc) => (acc << 8) | BigInt(lut(e)))
      for (l <- 0 until 64) dut.io.lut_write_act_out.bits.data(l).poke((if (l == 0) packed else BigInt(0)).U)
      dut.io.lut_write_act_out.bits.entry_bits.poke(8.U)
      dut.io.lut_write_act_out.valid.poke(true.B)
      var wrCycles = 0
      while (!dut.io.lut_write_act_out.ready.peek().litToBoolean) { dut.clock.step(1); wrCycles += 1 }
      dut.clock.step(1); wrCycles += 1
      dut.io.lut_write_act_out.valid.poke(false.B)
      assert(wrCycles == 64, s"act_out write took $wrCycles cycles, expected one per table (64)")
      dut.clock.step(NearestFinderTableEngine.latency + 1)

      // ---- project: drive all 32 lanes with distinct FP8 inputs, check each ----
      dut.io.quant_fp6.valid.poke(true.B)
      var checked = 0
      // sweep inputs across lanes in batches so counter-driven row stays 0
      val inputs = (0 until 256).filterNot(isNaNE4M3)
      for (batch <- inputs.grouped(lanes)) {
        val b = batch.toIndexedSeq
        for (i <- 0 until lanes) dut.io.quant_fp6.bits(i).poke(b(i % b.length).U)
        // combinational projection; peek this cycle
        for (i <- 0 until b.length) {
          val hw = dut.io.projected_data.bits(i).peek().litValue.toInt
          val gold = goldenNearest(b(i), lut)
          assert(hw == gold,
            f"in=0x${b(i)}%02x (${decodeE4M3(b(i))}): hw idx=$hw golden idx=$gold")
          checked += 1
        }
        dut.clock.step(1)
      }
      println(f"  ✓ QuantLut FP8 projection: $checked inputs matched golden")
    }
  }
}

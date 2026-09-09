package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Arithmetic.MxFloatArithmetic._

// Local reproduction of the E5M2-on-code0 requant PACKING bug (the ~82% code mismatch) without a full
// RTL/VCS rebuild. Drives the real MxRequantizer with the e5m2 config (LutFP8E5M2), loads a codebook,
// feeds a block of BF16 acc values with mx_mode=0/altfmt=1 (E5M2), and prints the packed 4-bit-LUT output
// so we can see how the 32 finder indices map into the output nibbles (and whether they get reordered).
//
// Compare mx_mode=0/altfmt=1 (E5M2, code0) vs mx_mode=1/altfmt=0 (E3M2, code1) for the SAME data: the
// code1 path is known-good, so a divergence in the nibble layout localizes the code0 packing reorder.
class E5M2RequantPackTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MxRequantizer E5M2 (code0) packing"

  val cfg = GemminiMxFPConfigs.e5m2MxFPConfig
  val lut = cfg.lut.get

  def mkDut = new MxRequantizer(
    sp_data_width       = cfg.sp_width,
    sp_addr_width       = log2Ceil(cfg.sp_bank_entries),
    scaleMem_data_width = cfg.scale_mem.get.ScaleMemWriteDataWidth,
    scaleMem_addr_width = cfg.scale_mem.get.ScaleMemWriteAddrWidth,
    scaleMemActWriteAddrWidth = cfg.scale_mem.get.addrBits - 1,
    scaleSize           = cfg.scaleSize,
    scaleMembasewrite   = 0,
    lutConfig           = lut,
    sp_bank_entries     = cfg.sp_bank_entries,
    sp_banks            = cfg.sp_banks,
    sp_width            = cfg.sp_width,
    sp_width_projected  = cfg.sp_width_projected,
    iterator_bitwidth   = 16,
    meshColumns         = cfg.meshColumns,
    tileColumns         = cfg.tileColumns,
    accType             = cfg.accType,
    weightTypeProjected = cfg.weightTypeProjected,
    config              = cfg.requantizer.get
  )

  // 16 arbitrary but valid E5M2 codebook codes (8-bit each) packed little-endian into one 128b word.
  val codebook = Seq(0x00, 0x3b, 0x37, 0xa4, 0x3f, 0x38, 0x34, 0xba,
                     0xc0, 0xb7, 0xb6, 0xbb, 0x35, 0x32, 0xb2, 0x36)
  val lutWord = codebook.zipWithIndex.map { case (c, i) => BigInt(c) << (8 * i) }.reduce(_ | _)

  it should "print the packed E5M2 (code0/altfmt1) output for inspection" in {
    test(mkDut).withAnnotations(Seq(VerilatorBackendAnnotation,
        chiseltest.simulator.VerilatorFlags(Seq("--no-pch")))) { dut =>
      // inert defaults
      dut.io.mxacc_req.mx_data_in.valid.poke(false.B)
      dut.io.mxacc_req.mx_data_out.ready.poke(true.B)
      dut.io.requant_data_in_gpu.valid.poke(false.B)
      dut.io.requant_data_out.ready.poke(true.B)
      dut.io.scaleMem_write.ready.poke(true.B)
      dut.io.scaleMem_write_act_resident.ready.poke(true.B)
      dut.io.lut0_write.valid.poke(false.B)
      dut.io.lut1_write.valid.poke(false.B)
      dut.io.lut2_write.valid.poke(false.B)
      dut.io.read_a.poke(false.B); dut.io.read_d.poke(false.B)
      dut.io.scale_resident.poke(false.B)
      dut.io.scale_mem_counter_reset_flag.poke(false.B)
      dut.io.scale_mem_mvout_base_addr_act.poke(0.U)
      dut.io.quant_lut_update_granularity.poke(0.U)
      dut.io.loop_bound_i.poke(1.U); dut.io.loop_bound_j.poke(1.U); dut.io.loop_bound_k.poke(1.U)
      dut.io.spad_projected_data.foreach { p => p.req.valid.poke(false.B); p.resp.ready.poke(true.B) }
      dut.io.spad_deprojected_data.foreach { p => p.resp.valid.poke(false.B); p.req.ready.poke(true.B) }
      dut.io.mxacc_req.mx_mode.poke(0.U)         // code0 (fp8)
      dut.io.mxacc_req.mx_fp8_altfmt.poke(1.U)   // E5M2
      dut.clock.step(2)

      // load the act-out codebook (group 0)
      dut.io.lut2_write.valid.poke(true.B)
      dut.io.lut2_write.bits.data(0).poke(lutWord.U)
      dut.clock.step(1)
      dut.io.lut2_write.valid.poke(false.B)
      dut.clock.step(2)

      // Feed one acc chunk: 8 lanes x 64b (each 64b = 4 bf16). Fill with a ramp of distinct bf16 values.
      def bf16(v: scala.Float): BigInt = {
        val bits = java.lang.Float.floatToIntBits(v)
        BigInt((bits >>> 16) & 0xFFFF)
      }
      dut.io.mxacc_req.mx_data_in.valid.poke(true.B)
      dut.io.mxacc_req.mx_data_in.bits.fromDMA.poke(true.B)
      dut.io.mxacc_req.mx_data_in.bits.chunk_id.poke(0.U)
      dut.io.mxacc_req.mx_data_in.bits.acc_bank_id.poke(0.U)
      for (i <- 0 until dut.io.mxacc_req.mx_data_in.bits.full_mx_data_in.length) {
        // pack 4 bf16 into the 64b element: lanes 4*i .. 4*i+3
        val packed = (0 until 4).map { j =>
          val lane = 4 * i + j
          bf16(-(lane + 1).toFloat * 0.5f).<<(16 * j)
        }.reduce(_ | _)
        dut.io.mxacc_req.mx_data_in.bits.full_mx_data_in(i)(0).bits.poke(packed.U)
      }
      dut.clock.step(1)
      dut.io.mxacc_req.mx_data_in.valid.poke(false.B)

      // let it flow through the pipeline, capture valid outputs to a file (forked-test stdout is not
      // in sbt's redirect, so write directly).
      val pw = new java.io.PrintWriter("/tmp/claude-200740/e5m2_out.txt")
      pw.println(s"codebook=${codebook.map(c => f"$c%02x").mkString(" ")}")
      for (c <- 0 until 24) {
        val v = dut.io.mxacc_req.mx_data_out.valid.peek().litToBoolean
        val g = dut.io.mxacc_req.mx_data_out.bits.is_garbage.peek().litToBoolean
        val out = dut.io.mxacc_req.mx_data_out.bits.quant_mx_data_out
        val flat = out.flatten.map(_.asUInt.peek().litValue)
        pw.println(f"[c$c%2d] valid=$v garbage=$g  nibbles=" + flat.take(64).map(x => f"$x%x").mkString(""))
        dut.clock.step(1)
      }
      pw.close()
    }
  }
}

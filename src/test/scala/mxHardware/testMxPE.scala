// package mxHardware

// import chisel3._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import hardfloat._
// import freechips.rocketchip.util._
// import freechips.rocketchip.tile._
// import circt.stage.ChiselStage
// import org.scalatest.matchers.should.Matchers
// import scala.util.Random

// // class MxPE_FP4_WidenHarness extends Module {
// //   val io = IO(new Bundle {
// //     // DUT I/O
// //     val mode   = Input(UInt(4.W))
// //     val in_a   = Input(UInt(16.W))
// //     val in_w   = Input(UInt(16.W))
// //     val enable = Input(Bool())

// //     // Observability
// //     val raw_out   = Output(UInt(32.W))                 // DUT e4m3 packed bytes
// //     val out_recH  = Output(Vec(4, UInt((FType.H.exp + FType.H.sig + 1).W))) // recoded FP16
// //     val out_ieeeH = Output(Vec(4, UInt(16.W)))         // IEEE FP16
// //   })

// //   // Instantiate your PE with the requested config
// //   val pe = Module(new MxPE(List(MxMode.Fp4Fp4), lut = false))
// //   pe.io.mode   := io.mode
// //   pe.io.in_a   := io.in_a
// //   pe.io.in_w   := io.in_w
// //   pe.io.enable := io.enable

// //   io.raw_out := pe.io.output

// //   // Generic recoded->recoded widener
// //   private def widen(in: UInt, inT: FType, outT: FType): UInt = {
// //     val w = Module(new hardfloat.RecFNToRecFN(inT.exp, inT.sig, outT.exp, outT.sig))
// //     w.io.in             := in
// //     w.io.roundingMode   := hardfloat.consts.round_near_even
// //     w.io.detectTininess := hardfloat.consts.tininess_afterRounding
// //     w.io.out
// //   }

// //   // Unpack 4 x e4m3 bytes from the 32-bit result
// //   val y = Wire(Vec(4, UInt(8.W)))
// //   y(0) := pe.io.output(7, 0)
// //   y(1) := pe.io.output(15, 8)
// //   y(2) := pe.io.output(23, 16)
// //   y(3) := pe.io.output(31, 24)

// //   // Recode each E4M3 byte, widen to FP16 (recoded), and export IEEE16
// //   for (i <- 0 until 4) {
// //     // If you have helper: val rec8 = FType.E4M3.recode(hardfloatHelper.fp8ToE4M3(y(i), false.B))
// //     val rec8  = FType.E5M3.recode(hardfloatHelper.fp8ToE5M3(y(i), false.B))
// //     val recH  = widen(rec8, FType.E5M3, FType.H)
// //     io.out_recH(i)  := recH
// //     io.out_ieeeH(i) := FType.H.ieee(recH)
// //   }
// // }

// // class MxPE_FP4_Mode0_Test extends AnyFlatSpec with ChiselScalatestTester {
// //   private def bin(x: BigInt, w: Int): String =
// //     (x & ((BigInt(1) << w) - 1)).toString(2).reverse.padTo(w, '0').reverse
// //
// //   behavior of "MxPE (mode 0) single op: (a0,a1) x (w0,w1,w2,w3) -> 4 products"
// //
// //   it should "pack (a0,a1) and (w0..w3) and print four outputs" in {
// //     test(new MxPE_FP4_WidenHarness).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
// //       // Pick concrete FP4 nibbles (E2M1 external)
// //       val a0 = "b0001".U  // +0.5 (subnormal)
// //       val a1 = "b0001".U  // +1.5
// //       val w0 = "b0000".U  // +0.5
// //       val w1 = "b0001".U  // +1.0
// //       val w2 = "b0011".U  // +1.5
// //       val w3 = "b0010".U  // +2.0
// //
// //       // Pack: in_a[7:4]=a1, [3:0]=a0; in_w[15:12]=w3, [11:8]=w2, [7:4]=w1, [3:0]=w0
// //       val in_a = "h0011".asUInt
// //       val in_w = "h2310".asUInt
// //
// //       dut.io.mode.poke(0.U)
// //       dut.io.in_a.poke(in_a)
// //       dut.io.in_w.poke(in_w)
// //       dut.io.enable.poke(true.B)
// //
// //       // Step once (increase if your design is pipelined)
// //       dut.clock.step(1)
// //
// //       val hRecW = FType.H.exp + FType.H.sig + 1
// //
// //       val yrec0 = dut.io.out_recH(0).peek().litValue
// //       val yrec1 = dut.io.out_recH(1).peek().litValue
// //       val yrec2 = dut.io.out_recH(2).peek().litValue
// //       val yrec3 = dut.io.out_recH(3).peek().litValue
// //
// //       val yie0  = dut.io.out_ieeeH(0).peek().litValue
// //       val yie1  = dut.io.out_ieeeH(1).peek().litValue
// //       val yie2  = dut.io.out_ieeeH(2).peek().litValue
// //       val yie3  = dut.io.out_ieeeH(3).peek().litValue
// //
// //       // Four concise product lines (adjust lane↔pair mapping if your RTL differs)
// //       println(s"(a0=${bin(a0.litValue,4)}, w0=${bin(w0.litValue,4)}) -> rec=${bin(yrec0, hRecW)}, ieee=${bin(yie0,16)}")
// //       println(s"(a0=${bin(a0.litValue,4)}, w1=${bin(w1.litValue,4)}) -> rec=${bin(yrec1, hRecW)}, ieee=${bin(yie1,16)}")
// //       println(s"(a1=${bin(a1.litValue,4)}, w2=${bin(w2.litValue,4)}) -> rec=${bin(yrec2, hRecW)}, ieee=${bin(yie2,16)}")
// //       println(s"(a1=${bin(a1.litValue,4)}, w3=${bin(w3.litValue,4)}) -> rec=${bin(yrec3, hRecW)}, ieee=${bin(yie3,16)}")
// //     }
// //   }
// // }

// // class MxPE_FP4_Mode0_Test extends AnyFlatSpec with ChiselScalatestTester {
// //   private def bin(x: BigInt, w: Int): String =
// //     (x & ((BigInt(1) << w) - 1)).toString(2).reverse.padTo(w, '0').reverse

// //   behavior of "MxPE (mode 0) FP4xFP4 -> FP16 (same pairs as FP4MulF16AccTest)"

// //   it should "multiply the exact same (a,b) pairs and print FP16 rec/IEEE" in {
// //     test(new MxPE_FP4_WidenHarness).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
// //       // SAME SET as FP4MulF16AccTest (order preserved)
// //       val vals: Seq[Int] = Seq(
// //         Integer.parseInt("0000", 2), // 0x0
// //         Integer.parseInt("0001", 2), // 0x1
// //         Integer.parseInt("0010", 2), // 0x2
// //         Integer.parseInt("0011", 2), // 0x3
// //         Integer.parseInt("0100", 2), // 0x4
// //         Integer.parseInt("0101", 2), // 0x5
// //         Integer.parseInt("0111", 2), // 0x7
// //         Integer.parseInt("1011", 2)  // 0xB
// //       )

// //       val hRecW = FType.H.exp + FType.H.sig + 1

// //       for (a <- vals) {
// //         // Process b’s in groups of 4 to get 4 outputs per step
// //         vals.grouped(4).foreach { grp =>
// //           val b0 = grp(0)
// //           val b1 = grp.lift(1).getOrElse(0) // (not needed here since size=4)
// //           val b2 = grp.lift(2).getOrElse(0)
// //           val b3 = grp.lift(3).getOrElse(0)

// //           // Pack with plain BigInt math (NO Chisel ops here)
// //           // in_a = { 8'b0, a1, a0 } with a0=a1=a
// //           val in_a_bits: BigInt = (BigInt(0) << 8) | (BigInt(a) << 4) | BigInt(a)
// //           // in_w = { w3, w2, w1, w0 }
// //           val in_w_bits: BigInt =
// //             (BigInt(b3) << 12) | (BigInt(b2) << 8) | (BigInt(b1) << 4) | BigInt(b0)

// //           // Drive DUT
// //           dut.io.mode.poke(MxMode.Fp4Fp4.mode.U)
// //           dut.io.in_a.poke(in_a_bits.U(16.W))
// //           dut.io.in_w.poke(in_w_bits.U(16.W))
// //           dut.io.enable.poke(true.B)

// //           // Step once (bump if design is deeper)
// //           dut.clock.step(1)

// //           // Read 4 lanes
// //           val yrec0 = dut.io.out_recH(0).peek().litValue
// //           val yrec1 = dut.io.out_recH(1).peek().litValue
// //           val yrec2 = dut.io.out_recH(2).peek().litValue
// //           val yrec3 = dut.io.out_recH(3).peek().litValue

// //           val yie0  = dut.io.out_ieeeH(0).peek().litValue
// //           val yie1  = dut.io.out_ieeeH(1).peek().litValue
// //           val yie2  = dut.io.out_ieeeH(2).peek().litValue
// //           val yie3  = dut.io.out_ieeeH(3).peek().litValue

// //           // Print exactly one line per (a,b) pair — same order as original test.
// //           // Lane mapping: (a, b0), (a, b1), (a, b2), (a, b3)
// //           println(s"a=${bin(a,4)} b=${bin(b0,4)}  ->  FP16(rec)=${bin(yrec0, hRecW)}  FP16(IEEE)=${bin(yie0,16)}")
// //           println(s"a=${bin(a,4)} b=${bin(b1,4)}  ->  FP16(rec)=${bin(yrec1, hRecW)}  FP16(IEEE)=${bin(yie1,16)}")
// //           println(s"a=${bin(a,4)} b=${bin(b2,4)}  ->  FP16(rec)=${bin(yrec2, hRecW)}  FP16(IEEE)=${bin(yie2,16)}")
// //           println(s"a=${bin(a,4)} b=${bin(b3,4)}  ->  FP16(rec)=${bin(yrec3, hRecW)}  FP16(IEEE)=${bin(yie3,16)}")
// //         }
// //       }
// //       println(ChiselStage.emitSystemVerilog(new MxPE(List(MxMode.Fp4Fp4), lut = false)))
// //     }
// //   }
// // }


// // --------- Reusable helpers (extend these when you add more modes) ----------
// object MxGold {
//   // Pack little "chunks" (LSB-first) of fixed width into a single Int
//   def packChunksLSB(chunks: Seq[Int], chunkW: Int): BigInt =
//     chunks.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
//       acc | (BigInt(v & ((1 << chunkW) - 1)) << (i * chunkW))
//     }

//   // Unpack N values of 'bitW' each from a packed LSB-first word
//   def unpackNLSB(x: Int, bitW: Int, n: Int): Seq[Int] =
//     (0 until n).map(i => (x >> (i * bitW)) & ((1 << bitW) - 1))

//   /**
//     * GOLDEN for MODE 0 (MxParams.fp4 only)
//     * Assumptions for mode0:
//     *   - actWidth = 2, weiWidth = 2
//     *   - actTotalWidth = 4  => in_a holds (a0 at [1:0], a1 at [3:2])
//     *   - weiTotalWidth = 8  => in_w holds (w0 at [1:0], w1 at [3:2], w2 at [5:4], w3 at [7:6])
//     *   - numOutputs = 4, outTotalWidth = 16
//     *   - Each product is 4 bits; output layout is 4 chunks of 4 bits (LSB-first).
//     *
//     * If your multiply uses non-integer semantics, replace the `*` below with
//     * your reference multiply (and clamp/round into 4 bits as appropriate).
//     */
//   def goldMode0(inA: Int, inW: Int): BigInt = {
//     val a = unpackNLSB(inA, bitW = 2, n = 2)         // a0, a1
//     val w = unpackNLSB(inW, bitW = 2, n = 4)         // w0..w3
//     val p0 = a(0) * w(0)                             // chunk 0 -> [3:0]
//     val p1 = a(0) * w(1)                             // chunk 1 -> [7:4]
//     val p2 = a(1) * w(2)                             // chunk 2 -> [11:8]
//     val p3 = a(1) * w(3)                             // chunk 3 -> [15:12]
//     packChunksLSB(Seq(p0, p1, p2, p3), chunkW = 4)
//   }

//   def goldMode1_2x3(inA: Int, inW: Int): BigInt = {
//     val a = unpackNLSB(inA, bitW = 2, n = 2)   // a0, a1
//     val w = unpackNLSB(inW, bitW = 3, n = 4)   // w0..w3
//     val p0 = a(0) * w(0)                       // 5-bit lanes
//     val p1 = a(0) * w(1)
//     val p2 = a(1) * w(2)
//     val p3 = a(1) * w(3)
//     packChunksLSB(Seq(p0, p1, p2, p3), chunkW = 5)  // 4×5 = 20b, LSB-first
//   }

//   def goldMode2_2x4_2lanes(inA: Int, inW: Int): BigInt = {
//     val a = unpackNLSB(inA, bitW = 2, n = 2)   // a0, a1
//     val w = unpackNLSB(inW, bitW = 4, n = 1)   // w0
//     val p0 = a(0) * w(0)                       // up to 6 bits (we pack into 7)
//     val p1 = a(1) * w(0)
//     packChunksLSB(Seq(p0, p1), chunkW = 7)     // LSB-first: [6:0]=p0, [13:7]=p1
//   }

//   // Uses your existing: unpackNLSB / packChunksLSB
//   def goldMode3_3x2(inA: Int, inW: Int): BigInt = {
//     val a = unpackNLSB(inA, bitW = 3, n = 2)   // a0, a1
//     val w = unpackNLSB(inW, bitW = 2, n = 4)   // w0..w3
//     val p0 = a(0) * w(0)   // 5-bit lanes (max 7*3=21)
//     val p1 = a(0) * w(1)
//     val p2 = a(1) * w(2)
//     val p3 = a(1) * w(3)
//     packChunksLSB(Seq(p0, p1, p2, p3), chunkW = 5) // 20 bits total
//   }

//   def goldMode1(inA: Int, inW: Int): BigInt = {
//     // 3-bit chunks now
//     val a = unpackNLSB(inA, bitW = 3, n = 2)   // a0, a1
//     val w = unpackNLSB(inW, bitW = 3, n = 4)   // w0..w3
//     val p0 = a(0) * w(0)                       // 6-bit lanes
//     val p1 = a(0) * w(1)
//     val p2 = a(1) * w(2)
//     val p3 = a(1) * w(3)
//     packChunksLSB(Seq(p0, p1, p2, p3), chunkW = 6) // LSB-first packing
//   }

//   def goldMode5_3x4_2lanes(inA: Int, inW: Int, twoWeightChunks: Boolean): BigInt = {
//     val a = unpackNLSB(inA, bitW = 3, n = 2)       // a0, a1 (0..7)
//     val w0 = inW & 0xF                              // always have w0
//     val w1 = if (twoWeightChunks) (inW >> 4) & 0xF else w0  // w1=reuse w0 if only one chunk
//     val p0 = a(0) * w0                              // up to 7 bits (7*15=105)
//     val p1 = a(1) * w1
//     packChunksLSB(Seq(p0, p1), chunkW = 7)          // [6:0]=p0, [13:7]=p1
//   }

//   // In MxGold (re-use packChunksLSB / unpackNLSB if you already have them)
//   def goldMode6_4x2_2lanes(inA: Int, inW: Int): BigInt = {
//     val a  = inA & 0xF                       // single 4-bit activation
//     val w  = unpackNLSB(inW, bitW = 2, n = 2) // w0, w1 (two 2-bit weights)
//     val p0 = a * w(0)                         // each fits in ≤ 7 bits (15*3=45)
//     val p1 = a * w(1)
//     packChunksLSB(Seq(p0, p1), chunkW = 7)    // [6:0]=p0, [13:7]=p1
//   }

//   // In MxGold (re-use your existing packChunksLSB / unpackNLSB)
//   def goldMode7_4x3_2lanes(inA: Int, inW: Int, chunk: Int): BigInt = {
//     val a  = inA & 0xF                         // 4-bit activation
//     val w  = unpackNLSB(inW, bitW = 3, n = 2)  // w0, w1 (two 3-bit weights)
//     val p0 = a * w(0)                          // ≤ 15*7 = 105 → fits in 7 bits
//     val p1 = a * w(1)
//     packChunksLSB(Seq(p0, p1), chunkW = chunk)     // [6:0]=p0, [13:7]=p1
//   }

//   def goldMode8(inA: Int, inW: Int): BigInt = (inA & 0xF) * (inW & 0xF)
// }

// // -------------------------- The actual tests -------------------------------
// class MxPE_Mode0_FP4_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

//   behavior of "MxPE(mode=0) with MxParams.fp4"

//   it should "pack four 2b×2b products into 16 bits (4b per lane) for a simple hand case" in {
//     test(new MxPE(MxParams.fp4, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       // Mode 0 selected
//       dut.io.mode.poke(0.U)
//       dut.io.enable.poke(true.B)

//       // Compose in_a = [a1 a0] with 2-bit chunks (LSB-first)
//       // Choose a0=1, a1=2  => in_a = 1 | (2 << 2) = 0b1001 = 9
//       val inA = 0x9
//       // Compose in_w = [w3 w2 w1 w0] with 2-bit chunks (LSB-first)
//       // Choose w0=1, w1=2, w2=3, w3=0  => in_w = 0b00111001 = 57
//       val inW = 0x39

//       dut.io.in_a.poke(inA.U)   // expects width == actTotalWidth (4)
//       dut.io.in_w.poke(inW.U)   // expects width == weiTotalWidth (8)

//       // Give one cycle for any internal regs; adjust if your design adds latency
//       dut.clock.step(1)

//       val expected = MxGold.goldMode0(inA = inA, inW = inW) // 0x0621
//       val got      = dut.io.output.peek().litValue

//       // Pretty print lanes (4 bits each, LSB-first)
//       def lanes(x: BigInt): Seq[Int] = (0 until 4).map(i => ((x >> (4*i)) & 0xF).toInt)

//       dut.io.output.expect(expected.U,
//         s"Got 0x${got.toString(16)} expected 0x${expected.toString(16)}")
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (mode 0)" in {
//     test(new MxPE(MxParams.fp4, lut = false)) { dut =>
//       dut.io.mode.poke(0.U)
//       dut.io.enable.poke(true.B)

//       val rand    = new Random(0xC0FFEE)
//       val trials  = 200

//       for (i <- 0 until trials) {
//         // in_a is 4 bits (two 2-bit activations)
//         val a0  = rand.nextInt(4)
//         val a1  = rand.nextInt(4)
//         val inA = a0 | (a1 << 2)

//         // in_w is 8 bits (four 2-bit weights)
//         val w0  = rand.nextInt(4)
//         val w1  = rand.nextInt(4)
//         val w2  = rand.nextInt(4)
//         val w3  = rand.nextInt(4)
//         val inW = w0 | (w1 << 2) | (w2 << 4) | (w3 << 6)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode0(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=0x$inA%X  inW=0x$inW%02X  got=0x${got.toString(16)}  exp=0x${expected.toString(16)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }

//   it should "change only the intended 4-bit lane when a single weight chunk changes" in {
//     test(new MxPE(MxParams.fp4, lut = false)) { dut =>
//       dut.io.mode.poke(0.U)
//       dut.io.enable.poke(true.B)

//       // Fix activations (ensure a1 != 0 so lane 2/3 are sensitive)
//       val a0 = 1; val a1 = 3
//       val inA = a0 | (a1 << 2)
//       dut.io.in_a.poke(inA.U)

//       // Start with w0..w3 = 0
//       var inW = 0
//       dut.io.in_w.poke(inW.U); dut.clock.step(1)
//       val base = MxGold.goldMode0(inA, inW).toInt

//       // Flip only w2 -> affects only bits [11:8]
//       inW = inW | (2 << 4) // w2 = 2
//       dut.io.in_w.poke(inW.U); dut.clock.step(1)
//       val out = dut.io.output.peek().litValue.toInt

//       def slice16(x: Int, i: Int) = (x >> (4 * i)) & 0xF
//       slice16(out, 2) should not equal slice16(base, 2) // lane 2 changes
//       // Other lanes should remain the same
//       slice16(out, 0) shouldEqual slice16(base, 0)
//       slice16(out, 1) shouldEqual slice16(base, 1)
//       slice16(out, 3) shouldEqual slice16(base, 3)
//     }
//   }
// }

// object BinFmt {
//   def bin(x: BigInt, w: Int): String = {
//     val s = x.toString(2)
//     "b" + ("0" * (w - s.length)) + s
//   }
//   def bin(x: Int, w: Int): String = bin(BigInt(x), w)
// }

// class MxPE_Mode1_FP6_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=1) with MxParams.fp6 (3b inputs, 6b lanes)"

//   // Hand case: prints inputs and got/exp in binary
//   it should "pack four 3b×3b products into 24 bits (6b per lane) for a simple hand case" in {
//     test(new MxPE(MxParams.fp6, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(1.U)
//       dut.io.enable.poke(true.B)

//       // in_a = [a1 a0], 3 bits each -> 6 bits total
//       val a0  = 3
//       val a1  = 5
//       val inA = a0 | (a1 << 3)

//       // in_w = [w3 w2 w1 w0], 3 bits each -> 12 bits total
//       val w0  = 1
//       val w1  = 7
//       val w2  = 2
//       val w3  = 4
//       val inW = w0 | (w1 << 3) | (w2 << 6) | (w3 << 9)

//       dut.io.in_a.poke(inA.U)   // width 6
//       dut.io.in_w.poke(inW.U)   // width 12
//       dut.clock.step(1)

//       val expected = MxGold.goldMode1(inA, inW) // 24 bits packed (4 lanes × 6b)
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, 6)}  inW=${bin(inW, 12)}  got=${bin(got, 24)}  exp=${bin(expected, 24)}")

//       dut.io.output.expect(expected.U)
//     }
//   }

//   // Fuzz: print only got/exp (binary) for the first 10 trials
//   it should "fuzz random vectors and match the golden packing (mode 1)" in {
//     test(new MxPE(MxParams.fp6, lut = false)) { dut =>
//       dut.io.mode.poke(1.U)
//       dut.io.enable.poke(true.B)

//       val rand   = new Random(0xBAD6EED)
//       val trials = 200

//       for (i <- 0 until trials) {
//         val a0  = rand.nextInt(8)
//         val a1  = rand.nextInt(8)
//         val inA = a0 | (a1 << 3)

//         val w0  = rand.nextInt(8)
//         val w1  = rand.nextInt(8)
//         val w2  = rand.nextInt(8)
//         val w3  = rand.nextInt(8)
//         val inW = w0 | (w1 << 3) | (w2 << 6) | (w3 << 9)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode1(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] got=${bin(got, 24)}  exp=${bin(expected, 24)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode8_FP8_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=8) with MxParams.fp8 (4b inputs → 8b product)"

//   it should "multiply two 4-bit inputs and return the 8-bit product (hand case)" in {
//     test(new MxPE(MxParams.fp8, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(8.U)
//       dut.io.enable.poke(true.B)

//       val inA = 0xA // 10
//       val inW = 0x7 // 7

//       dut.io.in_a.poke(inA.U)   // width 4
//       dut.io.in_w.poke(inW.U)   // width 4
//       dut.clock.step(1)

//       val expected = MxGold.goldMode8(inA, inW) // 70 = 0x46
//       val got      = dut.io.output.peek().litValue.toInt

//       println(s"[hand] inA=${bin(inA, 4)}  inW=${bin(inW, 4)}  got=${bin(got, 8)}  exp=${bin(expected, 8)}")

//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "exhaustively verify all 4b×4b combinations (256 cases)" in {
//     test(new MxPE(MxParams.fp8, lut = false)) { dut =>
//       dut.io.mode.poke(8.U)
//       dut.io.enable.poke(true.B)

//       var printed = 0
//       for (a <- 0 until 16; w <- 0 until 16) {
//         dut.io.in_a.poke(a.U)
//         dut.io.in_w.poke(w.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode8(a, w)
//         val got      = dut.io.output.peek().litValue.toInt

//         if (a == printed && w == 5) {
//           println(f"[case $printed%02d] inA=${bin(a, 4)}  inW=${bin(w, 4)}  got=${bin(got, 8)}  exp=${bin(expected, 8)}")
//           printed += 1
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode1_Mixed_2x3_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=1) with 2b activations, 3b weights → 5b lanes (outTotalWidth=20)"

//   // Hand-picked case (binary print)
//   it should "pack four (2b×3b) products into 20 bits (5b per lane) for a simple hand case" in {
//     // Build a PE that supports mode1; keeps widths aligned to (inA=4, inW=12, out=20)
//     test(new MxPE(MxParams(List(PE_MxMode.mode1)), lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(1.U)
//       dut.io.enable.poke(true.B)

//       // in_a: [a1 a0], 2 bits each → 4b total
//       val a0  = 1
//       val a1  = 3
//       val inA = a0 | (a1 << 2)

//       // in_w: [w3 w2 w1 w0], 3 bits each → 12b total
//       val w0  = 2
//       val w1  = 5
//       val w2  = 7
//       val w3  = 1
//       val inW = w0 | (w1 << 3) | (w2 << 6) | (w3 << 9)

//       dut.io.in_a.poke(inA.U)   // expect width 4
//       dut.io.in_w.poke(inW.U)   // expect width 12
//       dut.clock.step(1)

//       val expected = MxGold.goldMode1_2x3(inA, inW) // 20-bit packed
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, 4)}  inW=${bin(inW, 12)}  got=${bin(got, 20)}  exp=${bin(expected, 20)}")

//       dut.io.output.expect(expected.U)
//     }
//   }

//   // Fuzz (print only the first 10 got/exp in binary)
//   it should "fuzz random vectors and match the golden packing (mode 1: 2×3 → 5b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode1)), lut = false)) { dut =>
//       dut.io.mode.poke(1.U)
//       dut.io.enable.poke(true.B)

//       val rand   = new Random(0x12345)
//       val trials = 200

//       for (i <- 0 until trials) {
//         // in_a: two 2-bit activations → 4b total
//         val a0  = rand.nextInt(4)
//         val a1  = rand.nextInt(4)
//         val inA = a0 | (a1 << 2)

//         // in_w: four 3-bit weights → 12b total
//         val w0  = rand.nextInt(8)
//         val w1  = rand.nextInt(8)
//         val w2  = rand.nextInt(8)
//         val w3  = rand.nextInt(8)
//         val inW = w0 | (w1 << 3) | (w2 << 6) | (w3 << 9)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode1_2x3(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${BinFmt.bin(inA, 4)}  inW=${BinFmt.bin(inW, 12)}  " +
//                 s"got=${BinFmt.bin(got, 20)}  exp=${BinFmt.bin(expected, 20)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode2_Mixed_2x4_2lanes_Spec
//     extends AnyFlatSpec
//     with ChiselScalatestTester
//     with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode for 2b activations × 4b weights → 2 lanes of 7b each)"

//   it should "pack two (2b×4b) products into 14 bits (7b per lane) for a simple hand case" in {
//     // Build with the mode that matches these widths (PE_MxMode.mode2)
//     test(new MxPE(MxParams(List(PE_MxMode.mode2)), lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       // If your ID for this config is different, change 2.U accordingly.
//       dut.io.mode.poke(2.U)
//       dut.io.enable.poke(true.B)

//       // in_a: [a1 a0], 2 bits each → 4 bits total
//       val a0  = 3
//       val a1  = 2
//       val inA = a0 | (a1 << 2)

//       val inW = 12

//       dut.io.in_a.poke(inA.U)   // expect width 4
//       dut.io.in_w.poke(inW.U)   // expect width 8 (two 4-bit weights packed)
//       dut.clock.step(1)

//       val expected = MxGold.goldMode2_2x4_2lanes(inA, inW) // 14-bit packed
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, 4)}  inW=${bin(inW, 4)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")

//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (2×4 → two 7b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode2)), lut = false)) { dut =>
//       dut.io.mode.poke(2.U)  // adjust if your mode ID differs
//       dut.io.enable.poke(true.B)

//       val rand   = new Random(0x2A4B7)
//       val trials = 200

//       for (i <- 0 until trials) {
//         // Two 2-bit activations → 4-bit word
//         val a0  = rand.nextInt(4)
//         val a1  = rand.nextInt(4)
//         val inA = a0 | (a1 << 2)

//         // one 4-bit weight 
//         val inW  = rand.nextInt(4)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode2_2x4_2lanes(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${bin(inA, 4)}  inW=${bin(inW, 4)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode3_3x2_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

//   private def bin(x: BigInt, w: Int): String = {
//     val s = x.toString(2)
//     "b" + ("0" * (w - s.length)) + s
//   }
//   private def bin(x: Int, w: Int): String = bin(BigInt(x), w)

//   behavior of "MxPE(mode=3): 2×(3b activations) & 4×(2b weights) → 4 lanes × 5b (20b total)"

//   it should "pack four (3b×2b) products into 20 bits (5b per lane) for a simple hand case" in {
//     // Build with only mode3 so widths are (inA=6, inW=8, out=20)
//     test(new MxPE(MxParams(List(PE_MxMode.mode3)), lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(3.U)       // mode3
//       dut.io.enable.poke(true.B)

//       // in_a: [a1 a0], 3 bits each → 6 bits total
//       val a0  = 5                  // 0..7
//       val a1  = 6
//       val inA = a0 | (a1 << 3)

//       // in_w: [w3 w2 w1 w0], 2 bits each → 8 bits total
//       val w0  = 1                  // 0..3
//       val w1  = 3
//       val w2  = 2
//       val w3  = 0
//       val inW = w0 | (w1 << 2) | (w2 << 4) | (w3 << 6)

//       dut.io.in_a.poke(inA.U)     // 6 bits
//       dut.io.in_w.poke(inW.U)     // 8 bits
//       dut.clock.step(1)           // bump if your pipeline is deeper

//       val expected = MxGold.goldMode3_3x2(inA, inW) // 20-bit packed
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, 6)}  inW=${bin(inW, 8)}  got=${bin(got, 20)}  exp=${bin(expected, 20)}")
//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (mode 3: 3×2 → 5b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode3)), lut = false)) { dut =>
//       dut.io.mode.poke(3.U)
//       dut.io.enable.poke(true.B)

//       val rand   = new Random(0x33_22)
//       val trials = 200

//       for (i <- 0 until trials) {
//         // Two 3-bit activations → 6-bit word
//         val a0  = rand.nextInt(8)
//         val a1  = rand.nextInt(8)
//         val inA = a0 | (a1 << 3)

//         // Four 2-bit weights → 8-bit word
//         val w0  = rand.nextInt(4)
//         val w1  = rand.nextInt(4)
//         val w2  = rand.nextInt(4)
//         val w3  = rand.nextInt(4)
//         val inW = w0 | (w1 << 2) | (w2 << 4) | (w3 << 6)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode3_3x2(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${bin(inA, 6)}  inW=${bin(inW, 8)}  got=${bin(got, 20)}  exp=${bin(expected, 20)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode5_3x4_2lanes_Spec
//     extends AnyFlatSpec
//     with ChiselScalatestTester
//     with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=5): 2×(3b activations) & 1 or 2×(4b weights) → 2 lanes × 7b (14b total)"

//   it should "pack two (3b×4b) products into 14 bits (7b per lane) for a simple hand case" in {
//     // Build with only mode5 so widths are (inA=6, inW=4 or 8, out=14)
//     test(new MxPE(MxParams(List(PE_MxMode.mode5)), lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(5.U)
//       dut.io.enable.poke(true.B)

//       val inAWidth = dut.io.in_a.getWidth       // expect 6
//       val inWWidth = dut.io.in_w.getWidth       // 4 (single weight) or 8 (two weights)
//       val twoWeights = inWWidth >= 8

//       // in_a: [a1 a0], 3 bits each → 6 bits total
//       val a0  = 3             // 0..7
//       val a1  = 6
//       val inA = a0 | (a1 << 3)

//       // weights: if two chunks, [w1 w0] (4b each); else just w0
//       val w0  = 9             // 0..15
//       val w1  = 5
//       val inW = if (twoWeights) (w0 | (w1 << 4)) else w0

//       dut.io.in_a.poke(inA.U)
//       dut.io.in_w.poke(inW.U)
//       dut.clock.step(1)

//       val expected = MxGold.goldMode5_3x4_2lanes(inA, inW, twoWeights)
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, inAWidth)}  inW=${bin(inW, inWWidth)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (3×4 → two 7b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode5)), lut = false)) { dut =>
//       dut.io.mode.poke(5.U)
//       dut.io.enable.poke(true.B)

//       val inAWidth   = dut.io.in_a.getWidth    // 6
//       val inWWidth   = dut.io.in_w.getWidth    // 4 or 8
//       val twoWeights = inWWidth >= 8

//       val rand   = new Random(0x35_45)
//       val trials = 200

//       for (i <- 0 until trials) {
//         // Two 3-bit activations → 6-bit word
//         val a0  = rand.nextInt(8)
//         val a1  = rand.nextInt(8)
//         val inA = a0 | (a1 << 3)

//         // One or two 4-bit weights → 4 or 8-bit word
//         val w0  = rand.nextInt(16)
//         val w1  = rand.nextInt(16)
//         val inW = if (twoWeights) (w0 | (w1 << 4)) else w0

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode5_3x4_2lanes(inA, inW, twoWeights)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${bin(inA, inAWidth)}  inW=${bin(inW, inWWidth)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode6_4x2_2lanes_Spec
//     extends AnyFlatSpec
//     with ChiselScalatestTester
//     with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=6): 1×(4b activation) & 2×(2b weights) → 2 lanes × 7b (14b total)"

//   it should "pack two (4b×2b) products into 14 bits (7b per lane) for a simple hand case" in {
//     // Build only mode6 so widths are (inA=4, inW=4, out=14)
//     test(new MxPE(MxParams(List(PE_MxMode.mode6)), lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(6.U)
//       dut.io.enable.poke(true.B)

//       // in_a: single 4-bit activation
//       val a   = 0xD  // 13
//       val inA = a

//       // in_w: two 2-bit weights packed as [w1 w0]
//       val w0  = 0x3  // 3
//       val w1  = 0x1  // 1
//       val inW = w0 | (w1 << 2)  // 0b01_11 = 0x7

//       dut.io.in_a.poke(inA.U)   // expect width 4
//       dut.io.in_w.poke(inW.U)   // expect width 4 (two 2-bit weights)
//       dut.clock.step(1)

//       val expected = MxGold.goldMode6_4x2_2lanes(inA, inW) // 14-bit packed
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, 4)}  inW=${bin(inW, 4)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (4×2 → two 7b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode6)), lut = false)) { dut =>
//       dut.io.mode.poke(6.U)
//       dut.io.enable.poke(true.B)

//       val trials = 200
//       val rand   = new Random(0x46_22)

//       for (i <- 0 until trials) {
//         // Single 4-bit activation
//         val a   = rand.nextInt(16)
//         val inA = a

//         // Two 2-bit weights: w0 (LSBs), w1 (MSBs)
//         val w0  = rand.nextInt(4)
//         val w1  = rand.nextInt(4)
//         val inW = w0 | (w1 << 2)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode6_4x2_2lanes(inA, inW)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${bin(inA, 4)}  inW=${bin(inW, 4)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }

// class MxPE_Mode7_4x3_2lanes_Spec
//     extends AnyFlatSpec
//     with ChiselScalatestTester
//     with Matchers {
//   import BinFmt._

//   behavior of "MxPE(mode=7): 1×(4b activation) & 2×(3b weights) → 2 lanes × 7b (14b total)"

//   it should "pack two (4b×3b) products into 14 bits (7b per lane) for a simple hand case" in {
//     // Build only mode7 so widths are (inA=4, inW=6, out=14)
//     val params = MxParams.allfp8
//     // val params = MxParams(List(PE_MxMode.mode7))
//     test(new MxPE(params, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       dut.io.mode.poke(7.U)
//       dut.io.enable.poke(true.B)

//       // in_a: single 4-bit activation
//       val a   = 0x18  // 12
//       val inA = a
//       val checkA = 0xC

//       // in_w: two 3-bit weights packed as [w1 w0] (LSB-first chunks)
//       val w0  = 0x7  // 7
//       val w1  = 0x3  // 3
//       val inW = w0 | (w1 << 3) // 0b011_111 = 0x1F

//       dut.io.in_a.poke(inA.U)   // width 4
//       dut.io.in_w.poke(inW.U)   // width 6
//       dut.clock.step(1)

//       val expected = MxGold.goldMode7_4x3_2lanes(checkA, inW, params.outPE_width / 2) // 14-bit packed
//       val got      = dut.io.output.peek().litValue

//       println(s"[hand] inA=${bin(inA, params.inPE_act_totalWidth)}  inW=${bin(inW, params.inPE_wei_totalWidth)}  got=${bin(got, params.outPE_width)}  exp=${bin(expected, params.outPE_width)}")
//       dut.io.output.expect(expected.U)
//     }
//   }

//   it should "fuzz random vectors and match the golden packing (4×3 → two 7b lanes)" in {
//     test(new MxPE(MxParams(List(PE_MxMode.mode7)), lut = false)) { dut =>
//       dut.io.mode.poke(7.U)
//       dut.io.enable.poke(true.B)

//       val trials = 200
//       val rand   = new Random(0x47_33)

//       for (i <- 0 until trials) {
//         // Single 4-bit activation (0..15)
//         val a   = rand.nextInt(16)
//         val inA = a

//         // Two 3-bit weights (0..7): pack [w1 w0] into 6 bits
//         val w0  = rand.nextInt(8)
//         val w1  = rand.nextInt(8)
//         val inW = w0 | (w1 << 3)

//         dut.io.in_a.poke(inA.U)
//         dut.io.in_w.poke(inW.U)
//         dut.clock.step(1)

//         val expected = MxGold.goldMode7_4x3_2lanes(inA, inW, 7)
//         val got      = dut.io.output.peek().litValue

//         if (i < 10) {
//           println(f"[trial $i%02d] inA=${bin(inA, 4)}  inW=${bin(inW, 6)}  got=${bin(got, 14)}  exp=${bin(expected, 14)}")
//         }

//         dut.io.output.expect(expected.U)
//       }
//     }
//   }
// }
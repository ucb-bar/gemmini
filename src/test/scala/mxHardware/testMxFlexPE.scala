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

// // ------------ Formatting helpers ------------
// object Bin {
//   def bin(x: BigInt, w: Int): String = {
//     val s = x.toString(2)
//     "b" + ("0" * (w - s.length)) + s
//   }
//   def bin(x: Int, w: Int): String = bin(BigInt(x), w)
// }

// object PackFit {
//   // compute how many chunks of size reqBits can fit in portW with separation sep
//   private def chunksThatFit(reqBits: Int, sep: Int, portW: Int, want: Int): Int = {
//     // chunk i starts at i*sep and occupies [i*sep, i*sep+reqBits)
//     // fit if i*sep + reqBits <= portW
//     (0 until want).takeWhile(i => i * sep + reqBits <= portW).size
//   }

//   // Pack chunks into the port with given separation; returns (packedValue, usedChunks)
//   def packToPort(chunks: Seq[Int], reqBits: Int, sep: Int, portW: Int): (BigInt, Int) = {
//     val use = chunksThatFit(reqBits, sep, portW, chunks.length)
//     val packed =
//       chunks.take(use).zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
//         val masked = v & ((1 << reqBits) - 1)
//         acc | (BigInt(masked) << (i * sep))
//       }
//     (packed & ((BigInt(1) << portW) - 1), use)
//   }
// }

// // ------------ Golden math & packing helpers ------------
// object MxGold1 {
//   // Pack N little "chunks" of width chunkW into a word, LSB-first
//   def packChunksLSB(chunks: Seq[Int], chunkW: Int): BigInt =
//     chunks.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
//       acc | (BigInt(v & ((1 << chunkW) - 1)) << (i * chunkW))
//     }

//   // Unpack N chunks of width bitW from x, LSB-first
//   def unpackNLSB(x: Int, bitW: Int, n: Int): Seq[Int] =
//     (0 until n).map(i => (x >> (i * bitW)) & ((1 << bitW) - 1))

//   // ==== Goldens per mode shape (logical values, not physically packed) ====

//   // mode5: 3b activations (a0,a1) × 4b weights → 2 lanes × 7b
//   // If only one weight provided, replicate it.
//   def goldMode5_3x4_2lanes(a0:Int,a1:Int,w0:Int,w1Opt:Option[Int], width:Int): BigInt = {
//     val w1 = w1Opt.getOrElse(w0)
//     packChunksLSB(Seq(a0*w0, a1*w1), width)
//   }

//   // mode6: 4b activation (a) × 2b weights (w0,w1) → 2 lanes × 7b
//   def goldMode6_4x2_2lanes(a:Int,w0:Int,w1:Int, width:Int): BigInt =
//     packChunksLSB(Seq(a*w0, a*w1), width)

//   // mode7: 4b activation (a) × 3b weights (w0,w1) → 2 lanes × 7b
//   def goldMode7_4x3_2lanes(a:Int,w0:Int,w1:Int, width:Int): BigInt =
//     packChunksLSB(Seq(a*w0, a*w1), width)

//   // mode8: 4b × 4b → 8b single output (we’ll zero-extend to 14 for compare)
//   def goldMode8_4x4(a:Int,w:Int): Int = (a & 0xF) * (w & 0xF)

//   def goldMode1_2x3_4lanes(a0:Int,a1:Int,w0:Int,w1:Int,w2:Int,w3:Int, width:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a0*w1, a1*w2, a1*w3), chunkW = width)

//   // mode3: 3b acts × 2b weights, 4 lanes × 5b
//   def goldMode3_3x2_4lanes(a0:Int,a1:Int,w0:Int,w1:Int,w2:Int,w3:Int, width:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a0*w1, a1*w2, a1*w3), chunkW = width)

//   // mode4: 3b acts × 3b weights, 4 lanes × 6b
//   def goldMode4_3x3_4lanes(a0:Int,a1:Int,w0:Int,w1:Int,w2:Int,w3:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a0*w1, a1*w2, a1*w3), chunkW = 6)

//   def goldMode0_2x2_4lanes(a0:Int,a1:Int,w0:Int,w1:Int,w2:Int,w3:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a0*w1, a1*w2, a1*w3), chunkW = 5)


//   def goldMode2_2x4_2lanes(a0:Int,a1:Int,w0:Int,w1:Int, space:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a1*w1), space)

//   def goldMode0_2x2_4lanesW(a0:Int,a1:Int,w0:Int,w1:Int,w2:Int,w3:Int, width:Int): BigInt =
//     packChunksLSB(Seq(a0*w0, a0*w1, a1*w2, a1*w3), chunkW = width)

//   // ==== Generic input packing (physical) ====

//   // When packing into the DUT inputs, use separation = max(flexSep, requiredBits)
//   // This preserves information when the mode requires wider chunks than the flex bus.
//   private def sep(requiredBits: Int, flexSep: Int): Int = math.max(requiredBits, flexSep)

//   // Pack activation chunks using params.actflexMulInWidth
//   def packActs(chunks: Seq[Int], requiredBits: Int, actFlexSep: Int): BigInt = {
//     val step = sep(requiredBits, actFlexSep)
//     chunks.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
//       acc | (BigInt(v) << (i * step))
//     }
//   }

//   // Pack weight chunks using params.weiflexMulInWidth
//   def packWeis(chunks: Seq[Int], requiredBits: Int, weiFlexSep: Int): BigInt = {
//     val step = sep(requiredBits, weiFlexSep)
//     chunks.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
//       acc | (BigInt(v) << (i * step))
//     }
//   }
// }

// class MxPE_AllFp8_Modes_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import Bin._
//   import MxGold1._
//   import PackFit._

//   behavior of "MxPE(allfp8) runs modes 2,5,6,7,8 via mode input with proper (un)packing"

//   it should "exercise allfp8 (modes 2,5,6,7,8) on a single DUT" in {
//     val params = MxParams.allfp8
//     test(new MxPE(params, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       val rand   = new Random(0xA11F)
//       val trials = 80
//       val outW   = dut.io.output.getWidth         // should be 14 for allfp8
//       val aPortW = dut.io.in_a.getWidth
//       val wPortW = dut.io.in_w.getWidth

//         def pA(x: BigInt) = Bin.bin(x, aPortW)
//         def pW(x: BigInt) = Bin.bin(x, wPortW)
//         def pO(x: BigInt) = Bin.bin(x, dut.io.output.getWidth)

//         // ---------- MODE 2: a:2b×2, w:4b×(weiInputs=1) → 2×7b ----------
//         {
//         dut.io.mode.poke(2.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until 80) {
//             val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//             val w0 = rand.nextInt(16)  // only one 4b weight in this mode (weiInputs=1)

//             // activations: two 2b chunks, stride = params.actflexMulInWidth
//             val (inA_phys, aUsed) = packToPort(Seq(a0, a1), reqBits = 2, sep = params.actflexMulInWidth, portW = aPortW)
//             require(aUsed >= 2, s"Mode2 needs two activation chunks; only $aUsed fit in ${aPortW}b port")

//             // weights: one 4b chunk
//             val (inW_phys, wUsed) = packToPort(Seq(w0), reqBits = 4, sep = params.weiflexMulInWidth, portW = wPortW)
//             require(wUsed >= 1, s"Mode2 needs one weight chunk; none fit in ${wPortW}b port")

//             dut.io.in_a.poke(inA_phys.U(aPortW.W))
//             dut.io.in_w.poke(inW_phys.U(wPortW.W))
//             dut.clock.step(1)

//             val exp = MxGold1.goldMode2_2x4_2lanes(a0, a1, w0, w0, 7 /* single weight used twice per spec */)
//             val got = dut.io.output.peek().litValue
//             if (i < 3) println(s"[mode2] inA=${pA(inA_phys)} inW=${pW(inW_phys)} got=${pO(got)} exp=${pO(exp)}")
//             dut.io.output.expect(exp.U)
//         }
//         }

//         // ---------- MODE 5: a:3b×2, w:4b × (weiInputs=1) → 2×7b ----------
//         {
//         dut.io.mode.poke(5.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until 80) {
//             val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//             val w0 = rand.nextInt(16) // one 4b weight on the port in allfp8

//             val (inA_phys, aUsed) = packToPort(Seq(a0, a1), reqBits = 3, sep = params.actflexMulInWidth, portW = aPortW)
//             require(aUsed >= 2, s"Mode5 needs two activation chunks; only $aUsed fit")

//             val (inW_phys, wUsed) = packToPort(Seq(w0), reqBits = 4, sep = params.weiflexMulInWidth, portW = wPortW)
//             require(wUsed >= 1, s"Mode5 needs one weight chunk; none fit")

//             dut.io.in_a.poke(inA_phys.U(aPortW.W))
//             dut.io.in_w.poke(inW_phys.U(wPortW.W))
//             dut.clock.step(1)

//             val exp = MxGold1.goldMode5_3x4_2lanes(a0, a1, w0, None, params.outPE_width/2)  // replicate w0 for lane1 per your mode5 spec
//             val got = dut.io.output.peek().litValue
//             if (i < 3) println(s"[mode5] inA=${pA(inA_phys)} inW=${pW(inW_phys)} got=${pO(got)} exp=${pO(exp)}")
//             dut.io.output.expect(exp.U)
//         }
//         }

//         // ---------- MODE 6: a:4b×1, w:2b×2 → 2×7b ----------
//         {
//         dut.io.mode.poke(6.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until 80) {
//             val a  = rand.nextInt(16)
//             val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)

//             val (inA_phys, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = params.actflexMulInWidth, portW = aPortW)
//             require(aUsed >= 1, "Mode6 needs one activation chunk")

//             val (inW_phys, wUsed) = packToPort(Seq(w0, w1),      reqBits = 2, sep = params.weiflexMulInWidth, portW = wPortW)
//             require(wUsed >= 2, s"Mode6 needs two weight chunks; only $wUsed fit in ${wPortW}b port")

//             dut.io.in_a.poke(inA_phys.U(aPortW.W))
//             dut.io.in_w.poke(inW_phys.U(wPortW.W))
//             dut.clock.step(1)

//             val exp = MxGold1.goldMode6_4x2_2lanes(a, w0, w1, 7)
//             val got = dut.io.output.peek().litValue
//             if (i < 3) println(s"[mode6] inA=${pA(inA_phys)} inW=${pW(inW_phys)} got=${pO(got)} exp=${pO(exp)}")
//             dut.io.output.expect(exp.U)
//         }
//         }

//         // ---------- MODE 7: a:4b×1, w:3b×2 → 2×7b ----------
//         {
//         dut.io.mode.poke(7.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until 80) {
//             val a  = rand.nextInt(16)
//             val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)

//             val (inA_phys, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = params.actflexMulInWidth, portW = aPortW)
//             require(aUsed >= 1, "Mode7 needs one activation chunk")

//             val (inW_phys, wUsed) = packToPort(Seq(w0, w1),      reqBits = 3, sep = params.weiflexMulInWidth, portW = wPortW)
//             require(wUsed >= 2, s"Mode7 needs two 3-bit weight chunks; only $wUsed fit in ${wPortW}b port")

//             dut.io.in_a.poke(inA_phys.U(aPortW.W))
//             dut.io.in_w.poke(inW_phys.U(wPortW.W))
//             dut.clock.step(1)

//             val exp = MxGold1.goldMode7_4x3_2lanes(a, w0, w1, params.outPE_width/2)
//             val got = dut.io.output.peek().litValue
//             if (i < 3) println(s"[mode7] inA=${pA(inA_phys)} inW=${pW(inW_phys)} got=${pO(got)} exp=${pO(exp)}")
//             dut.io.output.expect(exp.U)
//         }
//         }

//         // ---------- MODE 8: 4b×4b → 8b (zero-extended on the DUT to 14) ----------
//         {
//         dut.io.mode.poke(8.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until 80) {
//             val a  = rand.nextInt(16)
//             val w  = rand.nextInt(16)

//             val (inA_phys, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = params.actflexMulInWidth, portW = aPortW)
//             require(aUsed >= 1)

//             val (inW_phys, wUsed) = packToPort(Seq(w),           reqBits = 4, sep = params.weiflexMulInWidth, portW = wPortW)
//             require(wUsed >= 1)

//             dut.io.in_a.poke(inA_phys.U(aPortW.W))
//             dut.io.in_w.poke(inW_phys.U(wPortW.W))
//             dut.clock.step(1)

//             val base8   = MxGold1.goldMode8_4x4(a, w)
//             val exp     = BigInt(base8) // DUT should zero-extend in 14-bit output
//             val got     = dut.io.output.peek().litValue
//             if (i < 3) println(s"[mode8] inA=${pA(inA_phys)} inW=${pW(inW_phys)} got=${pO(got)} exp=${pO(exp)}")
//             dut.io.output.expect(exp.U)
//         }
//         }
//     }
//   }
// }

// class MxPE_AllFp6_Modes_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import Bin._
//   import PackFit._
//   import MxGold1._

//   behavior of "MxPE(allfp6): single DUT runs modes 1,3,4,5,7 with flex packing"

//   it should "exercise modes 1, 3, 4, 5, 7 on one PE (MxParams.allfp6)" in {
//     val params = MxParams.allfp6
//     test(new MxPE(params, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       val rand    = new Random(0xA11F)
//       val trials  = 80

//       val outW    = dut.io.output.getWidth              // expect 24 for allfp6
//       val aPortW  = dut.io.in_a.getWidth               // expect 6
//       val wPortW  = dut.io.in_w.getWidth               // expect 12

//       def pA(x: BigInt) = bin(x, aPortW)
//       def pW(x: BigInt) = bin(x, wPortW)
//       def pO(x: BigInt) = bin(x, outW)

//       // Convenience: stride = max(requiredBits, flexWidth)
//       def strideAct(reqBits: Int) = math.max(reqBits, params.actflexMulInWidth)
//       def strideWei(reqBits: Int) = math.max(reqBits, params.weiflexMulInWidth)

//       // ---------------- MODE 1: 2b×3b, 4 lanes × 5b (20b total) ----------------
//       {
//         dut.io.mode.poke(1.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)
//           val w2 = rand.nextInt(8); val w3 = rand.nextInt(8)


//           val (inA, aUsed) = packToPort(Seq(a0, a1), reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode1 needs 2 act chunks; got $aUsed in $aPortW bits")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3), reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 4, s"mode1 needs 4 weight chunks; got $wUsed in $wPortW bits")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode1_2x3_4lanes(a0,a1,w0,w1,w2,w3, 6)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode1] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 3: 3b×2b, 4 lanes × 5b (20b total) ----------------
//       {
//         dut.io.mode.poke(3.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)
//           val w2 = rand.nextInt(4); val w3 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a0, a1), reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode3 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3), reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 4, s"mode3 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode3_3x2_4lanes(a0,a1,w0,w1,w2,w3,6)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode3] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 4: 3b×3b, 4 lanes × 6b (24b total) ----------------
//       {
//         dut.io.mode.poke(4.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)
//           val w2 = rand.nextInt(8); val w3 = rand.nextInt(8)

//           val (inA, aUsed) = packToPort(Seq(a0, a1), reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode4 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3), reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 4, s"mode4 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode4_3x3_4lanes(a0,a1,w0,w1,w2,w3)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode4] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 5: 3b×4b, 2 lanes × 7b (14b total) ----------------
//       {
//         dut.io.mode.poke(5.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(16)               // single 4-bit weight (weiInputs=1)

//           val (inA, aUsed) = packToPort(Seq(a0, a1), reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode5 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0),     reqBits = 4, sep = strideWei(4), portW = wPortW)
//           require(wUsed >= 1, s"mode5 needs 1 weight chunk; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           // Spec: reuse w0 for second lane
//           val exp = goldMode5_3x4_2lanes(a0,a1,w0,None, params.outPE_width/2)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode5] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 7: 4b×3b, 2 lanes × 7b (14b total) ----------------
//       {
//         dut.io.mode.poke(7.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a  = rand.nextInt(16)               // one 4b activation
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)


//           val (inA, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = strideAct(4), portW = aPortW)
//           require(aUsed >= 1, s"mode7 needs 1 act chunk; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0, w1),      reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 2, s"mode7 needs 2 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode7_4x3_2lanes(a, w0, w1, params.outPE_width/2)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode7] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }
//     }
//   }
// }

// class MxPE_AllFp4_Modes_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import Bin._
//   import PackFit._
//   import MxGold1._

//   behavior of "MxPE(allfp4): single DUT runs modes 0,1,2,3,6 with flex packing"

//   it should "exercise modes 0, 1, 2, 3, 6 on one PE (MxParams.allfp4)" in {
//     val params = MxParams.allfp4
//     test(new MxPE(params, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       val rand    = new Random(0xA11F)
//       val trials  = 80

//       val outW    = dut.io.output.getWidth              // expect 20 for allfp4
//       val aPortW  = dut.io.in_a.getWidth               // expect 6
//       val wPortW  = dut.io.in_w.getWidth               // expect 12

//       def pA(x: BigInt) = bin(x, aPortW)
//       def pW(x: BigInt) = bin(x, wPortW)
//       def pO(x: BigInt) = bin(x, outW)

//       def strideAct(reqBits: Int) = math.max(reqBits, params.actflexMulInWidth)
//       def strideWei(reqBits: Int) = math.max(reqBits, params.weiflexMulInWidth)

//       // ---------------- MODE 0: 2b×2b, 4 lanes × 4b (16b total) ----------------
//       {
//         dut.io.mode.poke(0.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)
//           val w2 = rand.nextInt(4); val w3 = rand.nextInt(4)

//         //   println(s"mode0 trial $i: a0=$a0 a1=$a1 w0=$w0 w1=$w1 w2=$w2 w3=$w3")

//           val (inA, aUsed) = packToPort(Seq(a0, a1),             reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode0 needs 2 act chunks; got $aUsed in $aPortW bits")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3),        reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 4, s"mode0 needs 4 weight chunks; got $wUsed in $wPortW bits")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode0_2x2_4lanes(a0,a1,w0,w1,w2,w3)  // 16b, zero-extend to outW
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode0] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 1: 2b×3b, 4 lanes × 5b (20b total) ----------------
//       {
//         dut.io.mode.poke(1.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)
//           val w2 = rand.nextInt(8); val w3 = rand.nextInt(8)

//           val (inA, aUsed) = packToPort(Seq(a0, a1),             reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode1 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3),        reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 4, s"mode1 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode1_2x3_4lanes(a0,a1,w0,w1,w2,w3, 5)   // 20b
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode1] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 2: 2b×4b, 2 lanes × 7b (14b total) ----------------
//       {
//         dut.io.mode.poke(2.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(16)                        // weiInputs=1 in mode2 (spec)
//           // pack only one 4b weight slice onto the port (replicate in golden)
//           val (inA, aUsed) = packToPort(Seq(a0, a1),       reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode2 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0),           reqBits = 4, sep = strideWei(4), portW = wPortW)
//           require(wUsed >= 1, s"mode2 needs 1 weight chunk; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode2_2x4_2lanes(a0,a1,w0,w0, params.outPE_width /2)    // reuse w0 for second lane
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode2] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 3: 3b×2b, 4 lanes × 5b (20b total) ----------------
//       {
//         dut.io.mode.poke(3.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)
//           val w2 = rand.nextInt(4); val w3 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a0, a1),       reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode3 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3),  reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 4, s"mode3 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode3_3x2_4lanes(a0,a1,w0,w1,w2,w3,5) // 20b
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode3] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 6: 4b×2b, 2 lanes × 7b (14b total) ----------------
//       {
//         dut.io.mode.poke(6.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a  = rand.nextInt(16)                        // one 4b activation
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = strideAct(4), portW = aPortW)
//           require(aUsed >= 1, s"mode6 needs 1 act chunk; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0, w1),      reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 2, s"mode6 needs 2 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W))
//           dut.io.in_w.poke(inW.U(wPortW.W))
//           dut.clock.step(1)

//           val exp = goldMode6_4x2_2lanes(a, w0, w1, params.outPE_width/2)       // 14b, zero-extend to outW
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode6] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }
//     }
//   }
// }

// class MxPE_All_Modes_Spec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
//   import Bin._
//   import PackFit._
//   import MxGold1._

//   behavior of "MxPE(all): single DUT runs modes 0..8 with flex packing"

//   it should "exercise all modes on one PE (MxParams.all)" in {
//     val params = MxParams.all
//     test(new MxPE(params, lut = false)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       val rand    = new Random(0xA14F)
//       val trials  = 64

//       val outW    = dut.io.output.getWidth          // expect 24 (max across all modes)
//       val aPortW  = dut.io.in_a.getWidth            // expect 6
//       val wPortW  = dut.io.in_w.getWidth            // expect 12

//       def pA(x: BigInt) = bin(x, aPortW)
//       def pW(x: BigInt) = bin(x, wPortW)
//       def pO(x: BigInt) = bin(x, outW)

//       // LSB-first lane widths used for golden packing:
//       val laneW4 = outW / 4    // for 4-lane modes (0,1,3,4) → typically 6
//       val laneW2 = outW / 2    // for 2-lane modes (2,5,6,7) per your spec
//       val laneW1 = outW           // for single-lane mode (8)

//       def strideAct(reqBits: Int) = math.max(reqBits, params.actflexMulInWidth)
//       def strideWei(reqBits: Int) = math.max(reqBits, params.weiflexMulInWidth)

//       // ---------------- MODE 0: 2b×2b, 4 lanes × laneW4 ----------------
//       {
//         dut.io.mode.poke(0.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)
//           val w2 = rand.nextInt(4); val w3 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a0,a1),            reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode0 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3),      reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 4, s"mode0 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode0_2x2_4lanesW(a0,a1,w0,w1,w2,w3, laneW4)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode0] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 1: 2b×3b, 4 lanes × laneW4 ----------------
//       {
//         dut.io.mode.poke(1.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)
//           val w2 = rand.nextInt(8); val w3 = rand.nextInt(8)

//           val (inA, aUsed) = packToPort(Seq(a0,a1),            reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode1 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3),      reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 4, s"mode1 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode1_2x3_4lanes(a0,a1,w0,w1,w2,w3, laneW4)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode1] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 2: 2b×4b, 2 lanes × 7b ----------------
//       {
//         dut.io.mode.poke(2.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(4); val a1 = rand.nextInt(4)
//           val w0 = rand.nextInt(16)                       // weiInputs=1 → one 4b weight

//           val (inA, aUsed) = packToPort(Seq(a0,a1),       reqBits = 2, sep = strideAct(2), portW = aPortW)
//           require(aUsed >= 2, s"mode2 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0),          reqBits = 4, sep = strideWei(4), portW = wPortW)
//           require(wUsed >= 1, s"mode2 needs 1 weight chunk; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode2_2x4_2lanes(a0,a1,w0,w0, laneW2)  // replicate w0
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode2] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))                  // zero-extend 14b to 24b
//         }
//       }

//       // ---------------- MODE 3: 3b×2b, 4 lanes × laneW4 ----------------
//       {
//         dut.io.mode.poke(3.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)
//           val w2 = rand.nextInt(4); val w3 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a0,a1),       reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode3 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3), reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 4, s"mode3 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode3_3x2_4lanes(a0,a1,w0,w1,w2,w3, laneW4)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode3] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 4: 3b×3b, 4 lanes × 6b (24b total) ----------------
//       {
//         dut.io.mode.poke(4.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)
//           val w2 = rand.nextInt(8); val w3 = rand.nextInt(8)

//           val (inA, aUsed) = packToPort(Seq(a0,a1),       reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode4 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1,w2,w3), reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 4, s"mode4 needs 4 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode4_3x3_4lanes(a0,a1,w0,w1,w2,w3) // 24b already
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode4] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 5: 3b×4b, 2 lanes × 7b ----------------
//       {
//         dut.io.mode.poke(5.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a0 = rand.nextInt(8); val a1 = rand.nextInt(8)
//           val w0 = rand.nextInt(16)                       // weiInputs=1

//           val (inA, aUsed) = packToPort(Seq(a0,a1),       reqBits = 3, sep = strideAct(3), portW = aPortW)
//           require(aUsed >= 2, s"mode5 needs 2 act chunks; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0),          reqBits = 4, sep = strideWei(4), portW = wPortW)
//           require(wUsed >= 1, s"mode5 needs 1 weight chunk; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode5_3x4_2lanes(a0,a1,w0,None, laneW2) // replicate w0
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode5] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 6: 4b×2b, 2 lanes × 7b ----------------
//       {
//         dut.io.mode.poke(6.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a  = rand.nextInt(16)
//           val w0 = rand.nextInt(4); val w1 = rand.nextInt(4)

//           val (inA, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = strideAct(4), portW = aPortW)
//           require(aUsed >= 1, s"mode6 needs 1 act chunk; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1),       reqBits = 2, sep = strideWei(2), portW = wPortW)
//           require(wUsed >= 2, s"mode6 needs 2 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode6_4x2_2lanes(a,w0,w1, laneW2)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode6] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 7: 4b×3b, 2 lanes × 7b ----------------
//       {
//         dut.io.mode.poke(7.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a  = rand.nextInt(16)
//           val w0 = rand.nextInt(8); val w1 = rand.nextInt(8)

//           val (inA, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = strideAct(4), portW = aPortW)
//           require(aUsed >= 1, s"mode7 needs 1 act chunk; got $aUsed")
//           val (inW, wUsed) = packToPort(Seq(w0,w1),       reqBits = 3, sep = strideWei(3), portW = wPortW)
//           require(wUsed >= 2, s"mode7 needs 2 weight chunks; got $wUsed")

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val exp = goldMode7_4x3_2lanes(a,w0,w1, laneW2)
//           val got = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode7] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))
//         }
//       }

//       // ---------------- MODE 8: 4b×4b, 1 lane × 8b ----------------
//       {
//         dut.io.mode.poke(8.U); dut.io.enable.poke(true.B)
//         for (i <- 0 until trials) {
//           val a  = rand.nextInt(16)
//           val w  = rand.nextInt(16)

//           val (inA, aUsed) = packToPort(Seq(a),           reqBits = 4, sep = strideAct(4), portW = aPortW)
//           require(aUsed >= 1)
//           val (inW, wUsed) = packToPort(Seq(w),           reqBits = 4, sep = strideWei(4), portW = wPortW)
//           require(wUsed >= 1)

//           dut.io.in_a.poke(inA.U(aPortW.W)); dut.io.in_w.poke(inW.U(wPortW.W)); dut.clock.step(1)

//           val base8 = goldMode8_4x4(a,w)
//           val exp   = BigInt(base8)                        // 8b LSBs; compare as 24b
//           val got   = dut.io.output.peek().litValue
//           if (i < 3) println(s"[mode8] inA=${pA(inA)}  inW=${pW(inW)}  got=${pO(got)}  exp=${pO(exp)}")
//           dut.io.output.expect(exp.U(outW.W))              // zero-extend 8→24
//         }
//       }
//     }
//   }
// }
package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import gemmini.vpu._
import scala.collection.mutable
import scala.util.Random

// VPU engine vs a Scala reference over a behavioral scratchpad (1-cycle read latency, 4096-row banks).
// Exact ops (add/sub/mul/scalar, max/absmax, FP32-accumulated sum) must be bit-exact; exp/rcp/rsqrt
// must be within 2 BF16 ulps of the ideal result.
class TestVpu extends AnyFlatSpec with ChiselScalatestTester {
  val lanes = 8
  val rnd = new Random(1)

  def toD(b: Int): Double = java.lang.Float.intBitsToFloat(b << 16).toDouble
  def bf(d: Double): Int = {   // RNE to BF16, incl. subnormals
    if (d.isNaN) return 0x7fc0
    val sign = if (d < 0 || (d == 0 && 1 / d < 0)) 0x8000 else 0
    val a = math.abs(d)
    val e = math.max(math.getExponent(a), -126)
    val ulp = math.pow(2, e - 7)
    val r = math.rint(a / ulp) * ulp
    if (r >= math.pow(2, 128)) sign | 0x7f80
    else sign | (java.lang.Float.floatToRawIntBits(r.toFloat) >>> 16)
  }
  def bfOfF(f: scala.Float): Int = bf(f.toDouble)
  def toF(b: Int): scala.Float = java.lang.Float.intBitsToFloat(b << 16)
  def ord(b: Int): Int = if ((b & 0x8000) != 0) (~b) & 0xffff else b ^ 0x8000
  def maxB(a: Int, b: Int): Int = if (ord(a) > ord(b)) a else b
  def ulpDist(a: Int, b: Int): Int = math.abs(ord(a) - ord(b))

  def randBf(eLo: Int, eHi: Int, signed: Boolean = true): Int = {
    val s = if (signed && rnd.nextBoolean()) 0x8000 else 0
    s | ((eLo + rnd.nextInt(eHi - eLo + 1)) << 7) | rnd.nextInt(128)
  }
  def pack(v: Seq[Int]): BigInt = v.zipWithIndex.map { case (x, l) => BigInt(x) << (16 * l) }.sum
  def unpack(r: BigInt): Seq[Int] = (0 until lanes).map(l => ((r >> (16 * l)) & 0xffff).toInt)

  // drives one command, models the scratchpad, returns cycles from issue to !busy
  def run(dut: Vpu, mem: mutable.Map[Int, BigInt], op: Int, src1: Int, src2: Int, dst: Int,
          rows: Int, rlen: Int = 1, bcast: Boolean = false, imm: Int = 0): Int = {
    val c = dut.io.cmd.bits
    dut.io.cmd.valid.poke(true.B)
    c.op.poke(op.U); c.src1.poke(src1.U); c.src2.poke(src2.U); c.dst.poke(dst.U)
    c.rows.poke(rows.U); c.rlen.poke(rlen.U); c.bcast.poke(bcast.B); c.imm.poke(imm.U)
    assert(dut.io.cmd.ready.peek().litToBoolean)
    var pend = Seq[Option[BigInt]](None, None)
    var cyc = 0
    var done = false
    while (!done) {
      for (p <- 0 until 2) dut.io.spad.rdata(p).poke(pend(p).getOrElse(BigInt(0)).U(128.W))
      pend = (0 until 2).map { p =>
        val rd = dut.io.spad.rd(p)
        if (rd.valid.peek().litToBoolean) Some(mem.getOrElse(rd.bits.peek().litValue.toInt, BigInt(0))) else None
      }
      if (pend(0).isDefined && pend(1).isDefined)
        assert((dut.io.spad.rd(0).bits.peek().litValue >> 12) != (dut.io.spad.rd(1).bits.peek().litValue >> 12),
          "two reads to the same bank in one cycle")
      if (dut.io.spad.wr.valid.peek().litToBoolean)
        mem(dut.io.spad.wr.bits.addr.peek().litValue.toInt) = dut.io.spad.wr.bits.data.peek().litValue
      dut.clock.step()
      if (cyc == 0) dut.io.cmd.valid.poke(false.B)
      cyc += 1
      done = !dut.io.busy.peek().litToBoolean
      assert(cyc < 20 * rows + 100, "VPU timeout")
    }
    cyc
  }

  def fill(mem: mutable.Map[Int, BigInt], base: Int, rows: Int, gen: => Int): Seq[Seq[Int]] =
    (0 until rows).map { r => val v = Seq.fill(lanes)(gen); mem(base + r) = pack(v); v }

  val A = 0x0100; val B = 0x1100; val Bsame = 0x0800; val D = 0x2100   // banks 0, 1, 0, 2
  val N = 64

  behavior of "Vpu"

  it should "do elementwise add/sub/mul/adds/muls bit-exact at 1 row/cycle" in {
    test(new Vpu) { dut =>
      for ((op, f) <- Seq[(Int, (Double, Double) => Double)](
             (VpuOp.ADD, _ + _), (VpuOp.SUB, _ - _), (VpuOp.MUL, _ * _));
           src2 <- Seq(B, Bsame)) {
        val mem = mutable.Map[Int, BigInt]()
        val a = fill(mem, A, N, randBf(110, 140)); val b = fill(mem, src2, N, randBf(110, 140))
        val cyc = run(dut, mem, op, A, src2, D, N)
        if (src2 == B) assert(cyc <= N + 8, s"op $op took $cyc cycles for $N rows")
        for (r <- 0 until N; l <- 0 until lanes) {
          val exp = bf(f(toD(a(r)(l)), toD(b(r)(l)))); val got = unpack(mem(D + r))(l)
          assert(got == exp, f"op $op src2 $src2%x row $r lane $l: ${a(r)(l)}%04x,${b(r)(l)}%04x -> $got%04x exp $exp%04x")
        }
      }
      for ((op, f) <- Seq[(Int, (Double, Double) => Double)]((VpuOp.ADDS, _ + _), (VpuOp.MULS, _ * _))) {
        val mem = mutable.Map[Int, BigInt]()
        val a = fill(mem, A, N, randBf(110, 140)); val imm = randBf(120, 130)
        run(dut, mem, op, A, 0, D, N, imm = imm)
        for (r <- 0 until N; l <- 0 until lanes)
          assert(unpack(mem(D + r))(l) == bf(f(toD(a(r)(l)), toD(imm))), s"op $op row $r lane $l")
      }
    }
  }

  it should "broadcast src2 once per logical row" in {
    test(new Vpu) { dut =>
      for (src2 <- Seq(B, Bsame)) {
        val mem = mutable.Map[Int, BigInt]()
        val rlen = 4
        val a = fill(mem, A, N, randBf(110, 140)); val b = fill(mem, src2, N / rlen, randBf(110, 140))
        run(dut, mem, VpuOp.MUL, A, src2, D, N, rlen = rlen, bcast = true)
        for (r <- 0 until N; l <- 0 until lanes)
          assert(unpack(mem(D + r))(l) == bf(toD(a(r)(l)) * toD(b(r / rlen)(l))), s"src2 $src2 row $r lane $l")
      }
    }
  }

  it should "reduce rows: max, absmax, sum (FP32 accumulate)" in {
    test(new Vpu) { dut =>
      for (rlen <- Seq(1, 4)) {
        val mem = mutable.Map[Int, BigInt]()
        val a = fill(mem, A, N, randBf(100, 140))
        val groups = a.grouped(rlen).toSeq
        run(dut, mem, VpuOp.RMAX, A, 0, D, N, rlen = rlen)
        run(dut, mem, VpuOp.RAMAX, A, 0, D + 0x100, N, rlen = rlen)
        run(dut, mem, VpuOp.RSUM, A, 0, D + 0x200, N, rlen = rlen)
        for ((grp, g) <- groups.zipWithIndex) {
          val mx = grp.flatten.reduce(maxB)
          val amx = grp.flatten.map(_ & 0x7fff).reduce(maxB)
          def rowSum(v: Seq[Int]): scala.Float = {
            def t(x: Seq[scala.Float]): scala.Float = if (x.size == 1) x.head else t(x.grouped(2).map(p => p(0) + p(1)).toSeq)
            t(v.map(toF))
          }
          val sum = grp.map(rowSum).reduce((acc, s) => acc + s)
          assert(unpack(mem(D + g)).forall(_ == mx), s"rmax rlen $rlen g $g")
          assert(unpack(mem(D + 0x100 + g)).forall(_ == amx), s"ramax rlen $rlen g $g")
          assert(unpack(mem(D + 0x200 + g)).forall(_ == bfOfF(sum)),
            f"rsum rlen $rlen g $g: ${unpack(mem(D + 0x200 + g)).head}%04x exp ${bfOfF(sum)}%04x")
        }
      }
    }
  }

  // VPU_DUMP=<file>: every BF16 input through EXP/RCP/RSQRT, lines "op x out" (checked against vpu_ref.h)
  it should "dump all unary results when VPU_DUMP is set" in {
    sys.env.get("VPU_DUMP").foreach { path =>
      test(new Vpu) { dut =>
        val out = new java.io.PrintWriter(path)
        for (op <- Seq(VpuOp.EXP, VpuOp.RCP, VpuOp.RSQRT)) {
          val mem = mutable.Map[Int, BigInt]()
          val rows = 65536 / lanes
          for (r <- 0 until rows) mem(r) = pack((0 until lanes).map(l => r * lanes + l))
          run(dut, mem, op, 0, 0, 0x2000, rows)
          for (r <- 0 until rows; l <- 0 until lanes) out.println(f"$op ${r * lanes + l}%04x ${unpack(mem(0x2000 + r))(l)}%04x")
        }
        out.close()
      }
    }
  }

  it should "compute exp/rcp/rsqrt within 2 ulps" in {
    test(new Vpu) { dut =>
      for ((op, f, gen) <- Seq[(Int, Double => Double, () => Int)](
             (VpuOp.EXP, math.exp, () => { var x = 0; do x = randBf(100, 133) while (math.abs(toD(x)) > 80); x }),
             (VpuOp.RCP, 1.0 / _, () => randBf(100, 154)),
             (VpuOp.RSQRT, d => 1.0 / math.sqrt(d), () => randBf(100, 154, signed = false)))) {
        val mem = mutable.Map[Int, BigInt]()
        val a = fill(mem, A, N, gen())
        run(dut, mem, op, A, 0, D, N)
        var worst = 0
        for (r <- 0 until N; l <- 0 until lanes) {
          val got = unpack(mem(D + r))(l); val exp = bf(f(toD(a(r)(l))))
          worst = math.max(worst, ulpDist(got, exp))
          assert(ulpDist(got, exp) <= 2, f"op $op x=${a(r)(l)}%04x got $got%04x ideal $exp%04x")
        }
        println(s"op $op worst error $worst ulp")
      }
      // specials
      val mem = mutable.Map[Int, BigInt]()
      mem(A) = pack(Seq(0x0000, 0x8000, 0x7f80, 0xff80, 0x4400, 0xc400, 0x3f80, 0xbf80))   // 0,-0,inf,-inf,512,-512,1,-1
      run(dut, mem, VpuOp.EXP, A, 0, D, 1)
      assert(unpack(mem(D)).take(6) == Seq(0x3f80, 0x3f80, 0x7f80, 0x0000, 0x7f80, 0x0000))
      run(dut, mem, VpuOp.RCP, A, 0, D, 1)
      assert(unpack(mem(D)).take(4) == Seq(0x7f80, 0xff80, 0x0000, 0x8000))
    }
  }
}

package gemmini

import chisel3._
import chiseltest._
import chiseltest.simulator.VcsBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

// Correctness of the optimized nearest finders (sorted codebook + code-space thresholds, NearestFinder.scala):
//  * against an exact Scala golden (nearest value in units of the smallest subnormal, lowest index on ties, every
//    exponent field treated as finite) for random codebooks with duplicates and +-0, every input code;
//  * against the original brute-force finders (NearestFinderRef.scala) wherever those are exact. The old finders
//    computed x - lut(i) in a fixedW+1-bit signed subtraction, which wraps when the input and an entry have
//    opposite signs and |x - lut(i)| >= 2^fixedW (E3M2: 8 bits, E4M3: 18, E5M2: 32) and then may return a far
//    entry; the old E3M2 finder also truncated its top binade (exp field 7) and the old E5M2 finder wrapped the
//    exp-field-31 codes. Those cases are reported, not asserted.
class TestNearestFinderEquiv extends AnyFlatSpec with ChiselScalatestTester {

  case class Fmt(name: String, expW: Int, mantW: Int, oldFixedW: Int, refBad: Int => Boolean) {   // refBad: codes the old finder mis-converted
    val w = 1 + expW + mantW
    def value(c: Int): Long = {   // signed, in units of the smallest subnormal
      val sign = (c >> (w - 1)) & 1; val exp = (c >> mantW) & ((1 << expW) - 1); val mant = c & ((1 << mantW) - 1)
      val mag: Long = if (exp == 0) mant.toLong else ((1L << mantW) + mant) << (exp - 1)
      if (sign == 1) -mag else mag
    }
    def golden(x: Int, lut: Seq[Int]): Int = {
      var best = Long.MaxValue; var bi = 0
      for (i <- 0 until 16) { val d = math.abs(value(x) - value(lut(i))); if (d < best) { best = d; bi = i } }
      bi
    }
    def refExact(x: Int, lut: Seq[Int]): Boolean =
      !refBad(x) && lut.forall(e => !refBad(e) && math.abs(value(x) - value(e)) < (1L << oldFixedW))
  }
  val E4M3 = Fmt("E4M3", 4, 3, 18, _ => false)
  val E5M2 = Fmt("E5M2", 5, 2, 32, c => ((c >> 2) & 0x1F) == 0x1F)   // inf/NaN codes: old conversion wrapped
  val E3M2 = Fmt("E3M2", 3, 2, 8, c => ((c >> 2) & 0x7) == 0x7)       // top binade: old conversion truncated
  val E2M3 = Fmt("E2M3", 2, 3, 8, _ => false)

  class Pair(w: Int, mkNew: () => RawModule, mkRef: () => RawModule) extends Module {
    val io = IO(new Bundle {
      val in     = Input(UInt(w.W))
      val in_lut = Input(Vec(16, UInt(w.W)))
      val idxNew = Output(UInt(4.W))
      val idxRef = Output(UInt(4.W))
    })
    val n = Module(mkNew()); val r = Module(mkRef())
    def wire(m: RawModule, out: UInt): Unit = m match {
      case f: FP8NearestFinder        => f.io.in := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
      case f: FP8NearestFinderRef     => f.io.in := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
      case f: FP6E3M2NearestFinder    => f.io.in_fp6 := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
      case f: FP6E3M2NearestFinderRef => f.io.in_fp6 := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
      case f: FP6E2M3NearestFinder    => f.io.in_fp6 := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
      case f: FP6E2M3NearestFinderRef => f.io.in_fp6 := io.in; f.io.in_lut := io.in_lut; out := f.io.nearestIdx
    }
    wire(n, io.idxNew); wire(r, io.idxRef)
  }

  def run(f: Fmt, mkNew: () => RawModule, mkRef: () => RawModule, luts: Int): Unit = {
    val rnd = new Random(7)
    val half = 1 << (f.w - 1)
    val codes = 0 until (1 << f.w)
    test(new Pair(f.w, mkNew, mkRef)).withAnnotations(Seq(VcsBackendAnnotation)) { dut =>
      var checked = 0; var refChecked = 0; var refSkipped = 0; var refWrong = 0
      for (t <- 0 until luts) {
        val lut = t % 3 match {
          case 0 => Seq.fill(16)(codes(rnd.nextInt(codes.length)))
          case 1 => val pool = Seq.fill(4)(codes(rnd.nextInt(codes.length))) :+ 0 :+ half   // duplicates, +0, -0
                    Seq.fill(16)(pool(rnd.nextInt(pool.length)))
          case _ => Seq.fill(16)(codes(rnd.nextInt(codes.length)) & (half | 0xF))            // small magnitudes: exact midpoints
        }
        for (i <- 0 until 16) dut.io.in_lut(i).poke(lut(i).U)
        for (x <- codes) {
          dut.io.in.poke(x.U)
          dut.clock.step(1)
          val gold = f.golden(x, lut)
          val a = dut.io.idxNew.peek().litValue.toInt; val b = dut.io.idxRef.peek().litValue.toInt
          val lutS = lut.map(c => f"0x$c%02x").mkString(",")
          assert(a == gold, f"${f.name} in=0x$x%02x: new idx=$a golden idx=$gold lut=$lutS")
          checked += 1
          if (f.refExact(x, lut)) {
            assert(b == gold, f"${f.name} in=0x$x%02x: old finder idx=$b golden idx=$gold (in its exact range) lut=$lutS")
            refChecked += 1
          } else { refSkipped += 1; if (b != gold) refWrong += 1 }
        }
      }
      println(f"  ✓ ${f.name}: $checked (lut, input) pairs match the exact golden; old finder identical on its exact range " +
              f"($refChecked pairs), outside it ($refSkipped pairs) the old finder was wrong $refWrong times")
    }
  }

  behavior of "NearestFinder"

  it should "be exact for E4M3 (and match the brute-force finder on its range)" in
    run(E4M3, () => new FP8NearestFinder(false), () => new FP8NearestFinderRef(false), 24)
  it should "be exact for E5M2 (and match the brute-force finder on its range)" in
    run(E5M2, () => new FP8NearestFinder(true), () => new FP8NearestFinderRef(true), 24)
  it should "be exact for E3M2 (and match the brute-force finder on its range)" in
    run(E3M2, () => new FP6E3M2NearestFinder, () => new FP6E3M2NearestFinderRef, 48)
  it should "be exact for E2M3 (and match the brute-force finder on its range)" in
    run(E2M3, () => new FP6E2M3NearestFinder, () => new FP6E2M3NearestFinderRef, 48)
}

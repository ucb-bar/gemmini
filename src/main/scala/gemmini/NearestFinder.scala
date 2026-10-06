package gemmini

import chisel3._
import chisel3.util._

// Nearest-codebook-entry search for the QuantLut act_out projection.
//
// The old finders (one FP8NearestFinder per lane) converted all 16 codebook entries to fixed point,
// subtracted, took |diff| and min-reduced, in every lane, every cycle. Here the per-table work is done
// once, when the codebook is loaded, and shared by every lane:
//   1. sort the 16 entries by value (stable: equal values keep their original index order),
//   2. for each adjacent pair compute the decision threshold as a *code-space key*: the smallest
//      monotonic key whose value is on the upper side of the pair's midpoint (ties resolved exactly as
//      the old reduce did: the lowest original index wins),
//   3. a lane then only compares its 8-bit key against the 15 thresholds (a thermometer), counts the
//      passes and maps the sorted position back to the original index through the stored permutation.
// Result: bit-identical indices to the old min-reduce for every finite input, ~15 small comparators per
// lane instead of 16 wide subtract/abs units plus a 15-level min tree.
//
// Value model: a w-bit sign-magnitude code (s | exp | mant) is treated as a finite number for every
// exponent field, in units of the smallest subnormal (as the old finders did). Codes that the OCP formats
// reserve for inf/NaN (E4M3 0x7F, E5M2 exp=31) therefore sort as the largest magnitudes; the old E5M2
// finder wrapped those (and the old E3M2 finder truncated its top binade), so only such codes differ.

/** Elaboration-time description of a code format used by the nearest finders. */
case class NfFormat(expW: Int, mantW: Int) {
  val w      = 1 + expW + mantW          // code width (8 for FP8, 6 for FP6)
  val sigW   = mantW + 1                 // implicit bit + mantissa
  val maxE   = (1 << expW) - 1           // largest exponent field (treated as finite)
  val fixedW = sigW + (maxE - 1)         // |value| in units of the smallest subnormal
  val half   = 1 << (w - 1)
}

object NfFormat {
  val E4M3 = NfFormat(4, 3)
  val E5M2 = NfFormat(5, 2)
  val E3M2 = NfFormat(3, 2)
  val E2M3 = NfFormat(2, 3)

  def of(f: LutProjFormat): NfFormat = f match {
    case LutFP8E4M3 => E4M3
    case LutFP8E5M2 => E5M2
    case LutFP6E3M2 => E3M2
    case LutFP6E2M3 => E2M3
  }

  /** Code width class of a format (8-bit or 6-bit codes). */
  def width(f: LutProjFormat): Int = of(f).w

  /** Threshold-set slot within a width class, selected at runtime by mx_fp8_altfmt:
    *  0 = E4M3 / E3M2 (altfmt = 0), 1 = E5M2 / E2M3 (altfmt = 1). */
  def slot(f: LutProjFormat): Int = f match {
    case LutFP8E4M3 | LutFP6E3M2 => 0
    case LutFP8E5M2 | LutFP6E2M3 => 1
  }
}

object NearestFinder {
  /** Monotonic key of a w-bit sign-magnitude code: key(a) < key(b) <=> value(a) < value(b), and equal keys
    * <=> equal values (-0 and +0 both map to `half`). Negative codes occupy [0, half-1], non-negative [half, 2^w-1]. */
  def key(code: UInt, w: Int): UInt = {
    val half = 1 << (w - 1)
    val mag  = code(w - 2, 0)
    Mux(code(w - 1),
      Mux(mag === 0.U, half.U(w.W), (half - 1).U(w.W) - mag),
      half.U(w.W) + mag)
  }

  /** |value| of a code in units of the smallest subnormal (fixedW bits). */
  def fixed(code: UInt, f: NfFormat): UInt = {
    val exp  = code(f.w - 2, f.mantW)
    val mant = code(f.mantW - 1, 0)
    val sig  = Cat(exp =/= 0.U, mant)                       // sigW bits
    val sh   = Mux(exp === 0.U, 0.U(f.expW.W), exp - 1.U)   // 0 .. maxE-1
    (sig << sh)(f.fixedW - 1, 0)
  }

  /** Stable rank sort of 16 codes by value. Returns (sorted codes, perm) with sorted(r) == codes(perm(r)). */
  def sort(codes: Seq[UInt], w: Int): (Vec[UInt], Vec[UInt]) = {
    require(codes.length == 16)
    val keys = codes.map(c => key(c(w - 1, 0), w))
    // lte(a)(b) for a < b: entry a is placed before entry b (ties: lower original index first)
    val lte = Array.tabulate(16, 16)((a, b) => if (a < b) keys(a) <= keys(b) else false.B)
    def before(j: Int, i: Int): Bool = if (j < i) lte(j)(i) else !lte(i)(j)   // j precedes i
    val rank = (0 until 16).map(i => PopCount((0 until 16).filter(_ != i).map(j => before(j, i))))
    val perm   = Wire(Vec(16, UInt(4.W)))
    val sorted = Wire(Vec(16, UInt(w.W)))
    for (r <- 0 until 16) {
      val sel = (0 until 16).map(i => rank(i) === r.U)
      perm(r)   := Mux1H(sel, (0 until 16).map(_.U(4.W)))
      sorted(r) := Mux1H(sel, codes.map(_(w - 1, 0)))
    }
    (sorted, perm)
  }

  /** For a doubled magnitude m (= |v_i + v_j| in fixed units): the code magnitude whose doubled value is the
    * smallest >= m (ceil), the largest <= m (floor), and whether some code hits m exactly. */
  def codeOf(m: UInt, f: NfFormat): (UInt, UInt, Bool) = {
    val mW   = f.fixedW + 1                       // |S| < 2^(fixedW+1)
    val mm   = m(mW - 1, 0)
    val subW = f.mantW + 1                        // doubled subnormal range: m < 2^(mantW+1)
    val isSub    = mm < (1 << subW).U
    val subFloor = (mm >> 1)(f.w - 1, 0)
    val subCeil  = ((mm +& 1.U) >> 1)(f.w - 1, 0)
    val subExact = !mm(0)
    // normal range: leading one at bit L (mantW+1 .. mW-1) -> exponent field e = L - mantW, significand = mm(L, L-mantW)
    val lead = (subW until mW).map { L => mm(L) && (if (L == mW - 1) true.B else mm(mW - 1, L + 1) === 0.U) }
    val cand = (subW until mW).map { L =>
      val sh    = L - f.mantW                    // == e, >= 1
      val sig   = mm(L, L - f.mantW)             // sigW bits incl. the leading one
      val rem   = mm(sh - 1, 0).orR
      val floor = ((sh - 1) << f.mantW).U(f.w.W) + sig
      (floor, rem)
    }
    val normFloor = Mux1H(lead, cand.map(_._1))
    val normRem   = Mux1H(lead, cand.map(_._2))
    val floor = Mux(isSub, subFloor, normFloor)
    val ceil  = Mux(isSub, subCeil, normFloor + normRem.asUInt)
    val exact = Mux(isSub, subExact, !normRem)
    (ceil, floor, exact)
  }

  /** Stage A of the threshold computation: per adjacent sorted pair, 2*midpoint (fixed point), duplicate flag
    * and the tie rule. */
  def pairs(sorted: Vec[UInt], perm: Vec[UInt], f: NfFormat): NfPairs = {
    val w = f.w
    val keys = sorted.map(c => key(c(w - 1, 0), w))
    val vals = sorted.map { c => val m = fixed(c(w - 1, 0), f); Mux(c(w - 1), -(m.zext), m.zext) }  // SInt fixedW+1
    val dup  = (0 until 15).map(i => keys(i) === keys(i + 1))
    // original index of the first entry of the run of equal values ending at position i
    val firstOrig = (0 until 16).foldLeft(Seq.empty[UInt]) { (acc, i) =>
      acc :+ (if (i == 0) perm(0) else Mux(dup(i - 1), acc(i - 1), perm(i)))
    }
    val out = Wire(new NfPairs(f))
    for (i <- 0 until 15) {
      out.s(i)        := vals(i) +& vals(i + 1)
      out.dup(i)      := dup(i)
      out.tieUpper(i) := perm(i + 1) < firstOrig(i)   // on an exact tie the old reduce kept the lowest original index
    }
    out
  }

  /** Stage B: midpoint -> nearest code magnitudes (ceil / floor) in code space. */
  def codes(p: NfPairs, f: NfFormat): NfCodes = {
    val out = Wire(new NfCodes(f))
    for (i <- 0 until 15) {
      val sNeg = p.s(i) < 0.S
      val m    = Mux(sNeg, -p.s(i), p.s(i)).asUInt
      val (ceil, floor, exact) = codeOf(m, f)
      out.ceil(i) := ceil; out.floor(i) := floor; out.exact(i) := exact; out.sNeg(i) := sNeg
    }
    out.dup := p.dup; out.tieUpper := p.tieUpper
    out
  }

  /** Stage C: decision thresholds (as keys, w+1 bits): a code x belongs to sorted position >= i+1 iff
    * key(x) >= thr(i). Duplicate values inherit the next distinct pair's threshold so the pass vector stays a
    * thermometer and the rank lands on the first (lowest original index) entry of a run. */
  def thresholdsFrom(c: NfCodes, f: NfFormat): Vec[UInt] = {
    val w = f.w; val tW = w + 1
    val raw = (0 until 15).map { i =>
      // smallest key whose value is >= midpoint
      val t0 = Mux(c.sNeg(i), (f.half - 1).U(tW.W) - c.floor(i), f.half.U(tW.W) + c.ceil(i))
      // exact tie at the midpoint: step past it unless the upper entry has the lower original index
      Mux(c.exact(i) && !c.tieUpper(i), t0 + 1.U, t0)
    }
    val never = (1 << w).U(tW.W)
    val thr = Wire(Vec(15, UInt(tW.W)))
    for (i <- 0 until 15) {
      // thr(i) = raw(j) for the first non-duplicate pair j >= i, else never
      val sel = (i until 15).map(j => !c.dup(j) && (i until j).map(c.dup(_)).foldLeft(true.B)(_ && _))
      val all = (i until 15).map(c.dup(_)).foldLeft(true.B)(_ && _)
      thr(i) := Mux1H(sel :+ all, (i until 15).map(raw(_)) :+ never)
    }
    thr
  }

  def thresholds(sorted: Vec[UInt], perm: Vec[UInt], f: NfFormat): Vec[UInt] =
    thresholdsFrom(codes(pairs(sorted, perm, f), f), f)

  /** Per-lane projection: original index of the nearest entry for a code with key k. */
  def lane(k: UInt, thr: Vec[UInt], perm: Vec[UInt]): UInt =
    perm(PopCount(thr.map(t => k >= t)))
}

class NfPairs(f: NfFormat) extends Bundle {
  val s        = Vec(15, SInt((f.fixedW + 2).W))
  val dup      = Vec(15, Bool())
  val tieUpper = Vec(15, Bool())
}

class NfCodes(f: NfFormat) extends Bundle {
  val ceil     = Vec(15, UInt(f.w.W))
  val floor    = Vec(15, UInt(f.w.W))
  val exact    = Vec(15, Bool())
  val sNeg     = Vec(15, Bool())
  val dup      = Vec(15, Bool())
  val tieUpper = Vec(15, Bool())
}

/** Derived per-table state kept in the act_out LUT cache instead of raw codes. */
class NearestFinderTable(nSlots: Int, thrW: Int) extends Bundle {
  val perm = Vec(16, UInt(4.W))
  val thr  = Vec(nSlots, Vec(15, UInt(thrW.W)))
  val is6  = Bool()     // codebook loaded with 6-bit entries (FP6 class) rather than 8-bit (FP8 class)
}

/** Pipelined table preprocessor: one codebook per cycle in, (perm, thresholds for every finder format) out
  * `latency` cycles later. Only the formats in `formats` are elaborated. */
class NearestFinderTableEngine(formats: Seq[LutProjFormat], codeW: Int, tagW: Int) extends Module {
  require(formats.nonEmpty)
  val fmts    = formats.distinct.map(NfFormat.of)
  val slots   = formats.distinct.map(NfFormat.slot)
  val nSlots  = slots.max + 1
  val widths  = fmts.map(_.w).distinct.sorted                // Seq(6), Seq(8) or Seq(6, 8)
  val thrW    = widths.max + 1
  val latency = NearestFinderTableEngine.latency

  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val codes = Vec(16, UInt(codeW.W))
      val is6   = Bool()
      val tag   = UInt(tagW.W)
    }))
    val out = Valid(new Bundle {
      val table = new NearestFinderTable(nSlots, thrW)
      val tag   = UInt(tagW.W)
    })
  })

  // stage 1: sort (per width class present), pick the class by the loaded entry width
  val sorts = widths.map(w => NearestFinder.sort(io.in.bits.codes, w))
  def byClass[T <: Data](is6: Bool, per: Seq[T]): T =
    if (widths.length == 1) per.head else Mux(is6, per(widths.indexOf(6)), per(widths.indexOf(8)))
  val s1_valid  = RegNext(io.in.valid, false.B)
  val s1_sorted = RegEnable(byClass(io.in.bits.is6, sorts.map(s => VecInit(s._1.map(_.pad(codeW))))), io.in.valid)
  val s1_perm   = RegEnable(byClass(io.in.bits.is6, sorts.map(_._2)), io.in.valid)
  val s1_is6    = RegEnable(io.in.bits.is6, io.in.valid)
  val s1_tag    = RegEnable(io.in.bits.tag, io.in.valid)

  // stage 2: pair midpoints, stage 3: code-space ceil/floor, stage 4: thresholds (per elaborated format)
  val s2_valid = RegNext(s1_valid, false.B)
  val s2_perm  = RegEnable(s1_perm, s1_valid)
  val s2_is6   = RegEnable(s1_is6, s1_valid)
  val s2_tag   = RegEnable(s1_tag, s1_valid)
  val s2_pairs = fmts.map(f => RegEnable(NearestFinder.pairs(s1_sorted, s1_perm, f), s1_valid))

  val s3_valid = RegNext(s2_valid, false.B)
  val s3_perm  = RegEnable(s2_perm, s2_valid)
  val s3_is6   = RegEnable(s2_is6, s2_valid)
  val s3_tag   = RegEnable(s2_tag, s2_valid)
  val s3_codes = fmts.zip(s2_pairs).map { case (f, p) => RegEnable(NearestFinder.codes(p, f), s2_valid) }

  val s4_valid = RegNext(s3_valid, false.B)
  val s4_perm  = RegEnable(s3_perm, s3_valid)
  val s4_is6   = RegEnable(s3_is6, s3_valid)
  val s4_tag   = RegEnable(s3_tag, s3_valid)
  val s4_thr   = fmts.zip(s3_codes).map { case (f, c) => RegEnable(NearestFinder.thresholdsFrom(c, f), s3_valid) }

  io.out.valid := s4_valid
  io.out.bits.tag := s4_tag
  io.out.bits.table.perm := s4_perm
  io.out.bits.table.is6  := s4_is6
  for (s <- 0 until nSlots) {
    val inSlot = formats.distinct.zipWithIndex.filter { case (f, _) => NfFormat.slot(f) == s }
    val never  = VecInit(Seq.fill(15)(((1 << widths.max)).U(thrW.W)))
    def padded(i: Int) = VecInit(s4_thr(i).map(_.pad(thrW)))
    val f6 = inSlot.find { case (f, _) => NfFormat.width(f) == 6 }.map { case (_, i) => padded(i) }
    val f8 = inSlot.find { case (f, _) => NfFormat.width(f) == 8 }.map { case (_, i) => padded(i) }
    io.out.bits.table.thr(s) := Mux(s4_is6, f6.getOrElse(never), f8.getOrElse(never))
  }
}

object NearestFinderTableEngine {
  val latency = 4
}

// ---------------------------------------------------------------------------------------------------------
// Single-table combinational finders with the historical interfaces (used by the unit tests and as a
// reference for the lane math). QuantLut no longer instantiates them.
// ---------------------------------------------------------------------------------------------------------

/** FP8 nearest finder: altfmt = false -> E4M3, true -> E5M2. */
class FP8NearestFinder(altfmt: Boolean) extends RawModule {
  val io = IO(new Bundle {
    val in         = Input(UInt(8.W))
    val in_lut     = Input(Vec(16, UInt(8.W)))
    val nearestIdx = Output(UInt(4.W))
  })
  val f = if (altfmt) NfFormat.E5M2 else NfFormat.E4M3
  val (sorted, perm) = NearestFinder.sort(io.in_lut, 8)
  io.nearestIdx := NearestFinder.lane(NearestFinder.key(io.in, 8), NearestFinder.thresholds(sorted, perm, f), perm)
}

/** FP6 E3M2 nearest finder (sign[5] | exp3[4:2] | man2[1:0]). */
class FP6E3M2NearestFinder extends RawModule {
  val io = IO(new Bundle {
    val in_fp6     = Input(UInt(6.W))
    val in_lut     = Input(Vec(16, UInt(6.W)))
    val nearestIdx = Output(UInt(4.W))
  })
  val (sorted, perm) = NearestFinder.sort(io.in_lut, 6)
  io.nearestIdx := NearestFinder.lane(NearestFinder.key(io.in_fp6, 6), NearestFinder.thresholds(sorted, perm, NfFormat.E3M2), perm)
}

/** FP6 E2M3 nearest finder (sign[5] | exp2[4:3] | man3[2:0]). */
class FP6E2M3NearestFinder extends RawModule {
  val io = IO(new Bundle {
    val in_fp6     = Input(UInt(6.W))
    val in_lut     = Input(Vec(16, UInt(6.W)))
    val nearestIdx = Output(UInt(4.W))
  })
  val (sorted, perm) = NearestFinder.sort(io.in_lut, 6)
  io.nearestIdx := NearestFinder.lane(NearestFinder.key(io.in_fp6, 6), NearestFinder.thresholds(sorted, perm, NfFormat.E2M3), perm)
}

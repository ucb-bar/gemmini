package gemmini

import chisel3._
import chisel3.util._

case class GemminiScalingFactorMemConfig(
  baseAddr: BigInt = 0x10000000L,
  sizeInBytes: BigInt = 16 << 10,
  subbankLineSizeInBytes: Int = 16,
  subbanksPerBank: Int = 2,
  numBanks: Int = 8,
  ScaleMemWriteDataWidth: Int = 256,
  ScaleMemWriteAddrWidth: Int = 33,
) {
  def depth: Int = (sizeInBytes / (subbankLineSizeInBytes) / numBanks).toInt
  def bankWidthBytes = subbankLineSizeInBytes * subbanksPerBank
  def bankWidthBits = bankWidthBytes * 8
  def addrBits = log2Ceil(sizeInBytes)
  def lineOffsetBits = log2Ceil(bankWidthBytes) 
}

// Lane count / output geometry come from MxGeom (DIM); BF16 in from the acc or SPAD_REQUANT only.
case class GemminiRequantizerConfig(
  inputBits: Int = 16,
  lutUpdateRegularityW : Int = 128,  // how many elements the LUT updates at once
  lutUpdateRegularityActIn : Int = 128,
  lutUpdateRegularityActOut : Int = 128,
  pipelineLatency: Int = 2,
)

// Source floating-point format that the LUT projects into 4-bit indices.
// Fixed at elaboration; defaults to FP6 so existing configs are unchanged.
sealed trait LutProjFormat
case object LutFP6E3M2 extends LutProjFormat
case object LutFP6E2M3 extends LutProjFormat
case object LutFP8E4M3 extends LutProjFormat
case object LutFP8E5M2 extends LutProjFormat

case class GemminiLUTConfig(
  numBits: Seq[Int] = Seq(96, 96, 96),
  numEntries: Seq[Int] = Seq(64, 64, 64),
  rdataWidth: Int = 6,
  raddrWidth: Int = 4,
  lutUpdateRegularityWidth: Int = 16,
  projFormat: LutProjFormat = LutFP6E3M2,
  // Deprojected code width per operand for the mesh feed (0 = rdataWidth). For asymmetric builds whose
  // two operands deproject to different widths (e.g. E5M2 act 8b x E3M2 wei 6b), set these so the
  // deproject packs each operand at its own width. Storage stays rdataWidth-wide.
  actCodeWidth: Int = 0,
  weiCodeWidth: Int = 0,
  // Nearest-finder formats the act_out projection must support at runtime. None -> the historical set implied
  // by projFormat (LutFP8E4M3 = every format, LutFP8E5M2 = E5M2 + E3M2, FP6 formats = themselves). Set it
  // (or let the Controller derive it from the operand MxConfig via restrictedTo) so a single-format build,
  // e.g. E4M3-only, elaborates only its own finder.
  finderFormats: Option[Seq[LutProjFormat]] = None,
) {
  def actCodeW = if (actCodeWidth > 0) actCodeWidth else rdataWidth
  def weiCodeW = if (weiCodeWidth > 0) weiCodeWidth else rdataWidth
  def isFp8Proj = projFormat == LutFP8E4M3 || projFormat == LutFP8E5M2

  def legacyFinderFormats: Seq[LutProjFormat] = projFormat match {
    case LutFP8E4M3 => Seq(LutFP8E4M3, LutFP8E5M2, LutFP6E3M2, LutFP6E2M3)
    case LutFP8E5M2 => Seq(LutFP8E5M2, LutFP6E3M2)
    case LutFP6E2M3 => Seq(LutFP6E2M3)
    case LutFP6E3M2 => Seq(LutFP6E3M2)
  }
  def resolvedFinderFormats: Seq[LutProjFormat] = finderFormats.getOrElse(legacyFinderFormats)

  /** Keep only the finders whose format the activation operand can take (FP4 and custom formats never project
    * through the LUT). Falls back to the current set when nothing matches, so odd configs still elaborate. */
  def restrictedTo(actFormats: Set[mxgen.MxFormat]): GemminiLUTConfig = {
    val wanted = actFormats.toSeq.collect {
      case mxgen.MxFormat.FP8_E4M3 => LutFP8E4M3
      case mxgen.MxFormat.FP8_E5M2 => LutFP8E5M2
      case mxgen.MxFormat.FP6_E3M2 => LutFP6E3M2
      case mxgen.MxFormat.FP6_E2M3 => LutFP6E2M3
    }
    val kept = resolvedFinderFormats.filter(wanted.contains)
    copy(finderFormats = Some(if (kept.nonEmpty) kept else resolvedFinderFormats))
  }

  require(resolvedFinderFormats.forall(f => !(f == LutFP8E4M3 || f == LutFP8E5M2) || rdataWidth == 8),
    s"8-bit finder formats in $resolvedFinderFormats need rdataWidth == 8 (projFormat $projFormat)")

  require(!isFp8Proj || rdataWidth == 8,
    "FP8 LUT projection requires rdataWidth == 8")

  // Each LUT holds 16 entries (2^raddrWidth) of rdataWidth bits, so every write
  // word must be exactly 16 * rdataWidth wide (96 for FP6, 128 for FP8).
  require(numBits.forall(_ == 16 * rdataWidth),
    s"GemminiLUTConfig.numBits must each equal 16 * rdataWidth (= ${16 * rdataWidth}); got $numBits")

  def apply(table: Int) = {
    (numEntries(table), numBits(table))
  }

  def numTables = numBits.length
}

// A scale Put with a byte mask: the requantizer's [GN][M] scale stream writes a block column's unaligned end word
// partially (the next column's stream writes the rest).
class ScalingFactorPutReq(addrWidth: Int, dataWidth: Int) extends ScalingFactorWriteReq(addrWidth, dataWidth) {
  val mask = UInt((dataWidth / 8).W)
}

class ScalingFactorWriteReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  def this(config: GemminiScalingFactorMemConfig) = {
    // writes two interleaved banks at once
    this(config.addrBits, config.ScaleMemWriteDataWidth)
  }
}


class ScalingFactorCntl(max_block: Int) extends Bundle {
  val counter_a = UInt(log2Up(max_block).W)
  val counter_b = UInt(log2Up(max_block).W)
  val fire_a = Bool()
  val fire_b = Bool()
  val baseAddress_act = UInt(32.W)
  val baseAddress_w = UInt(32.W)
  val scale_mem_read_w_sel = UInt(1.W)
  val scale_mem_read_act_sel = UInt(1.W)
  val loop_bound_i = UInt(9.W)
  val loop_bound_j = UInt(9.W)
  val loop_bound_k = UInt(9.W)
  val slots = new LutSlots   // per-slot bounds: an acc write's scale read uses its own halves' loop
  val scale_mem_counter_reset_flag = Bool()
}

class QuantLutWriteBundle(numEntries: Int, numBits: Int) extends Bundle {
  val data = Vec(numEntries, UInt(numBits.W))
  val entry_bits = UInt(6.W)   // width of the codebook entries as loaded (6 = FP6 class, 8 = FP8 class)
  val half_mode = Bool()       // write only codebooks half*numEntries/2 .. +numEntries/2-1 (loop-managed)
  val half = UInt(1.W)
  def this(config: (Int, Int)) = {
    this(config._1, config._2)
  }
}


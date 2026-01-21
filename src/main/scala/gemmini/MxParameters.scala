package gemmini 

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

// -----------------------------------------------------------------------------
// TYPE SUPPORT CONFIGURATION
// -----------------------------------------------------------------------------

trait HasMxPEParameters {
  val ts: TypeSupport

  lazy val inAWidth = ts.inAWidth
  lazy val inBWidth = ts.inBWidth

  lazy val peInAWidth = ts.peInAWidth
  lazy val peInBWidth = ts.peInBWidth
  lazy val peOutWidth = ts.peOutWidth
  lazy val expAdderWidths = ts.expAdderWidths

  lazy val mxparameters = ts.mxparameters

  lazy val actSupportFp4 = ts.actSupportFp4
  lazy val actSupportFp6_0 = ts.actSupportFp6_0
  lazy val actSupportFp6_1 = ts.actSupportFp6_1
  lazy val actSupportFp8_0 = ts.actSupportFp8_0
  lazy val actSupportFp8_1 = ts.actSupportFp8_1
  lazy val weiSupportFp4 = ts.weiSupportFp4
  lazy val weiSupportFp6_0 = ts.weiSupportFp6_0
  lazy val weiSupportFp6_1 = ts.weiSupportFp6_1
  lazy val weiSupportFp8_0 = ts.weiSupportFp8_0
  lazy val weiSupportFp8_1 = ts.weiSupportFp8_1
}

case class TypeSupport (
  actSupportFp4: Boolean = true,
  actSupportFp6_0: Boolean = false,
  actSupportFp6_1: Boolean = false,
  actSupportFp8_0: Boolean = false,
  actSupportFp8_1: Boolean = false,
  weiSupportFp4: Boolean = true,
  weiSupportFp6_0: Boolean = false,
  weiSupportFp6_1: Boolean = false,
  weiSupportFp8_0: Boolean = false,
  weiSupportFp8_1: Boolean = false
) {
  // input parameters
  val inAWidth = 12
  val inBWidth = 12

  // exp adder width
  val expAdderWidths = Seq(4, 3, 3, 3)

  val modes = (
    (if (actSupportFp4 && weiSupportFp4) List(PE_MxMode.mode0) else List()) ++
    (if (actSupportFp4 && weiSupportFp6_1) List(PE_MxMode.mode1) else List()) ++
    (if (actSupportFp4 && weiSupportFp6_0) List(PE_MxMode.mode2) else List()) ++
    (if (actSupportFp4 && weiSupportFp8_1) List(PE_MxMode.mode1) else List()) ++
    (if (actSupportFp4 && weiSupportFp8_0) List(PE_MxMode.mode2) else List()) ++

    (if (actSupportFp6_1 && weiSupportFp4) List(PE_MxMode.mode3) else List()) ++
    (if (actSupportFp6_1 && weiSupportFp6_1) List(PE_MxMode.mode4) else List()) ++
    (if (actSupportFp6_1 && weiSupportFp6_0) List(PE_MxMode.mode5) else List()) ++
    (if (actSupportFp6_1 && weiSupportFp8_1) List(PE_MxMode.mode4) else List()) ++
    (if (actSupportFp6_1 && weiSupportFp8_0) List(PE_MxMode.mode5) else List()) ++

    (if (actSupportFp6_0 && weiSupportFp4) List(PE_MxMode.mode6) else List()) ++
    (if (actSupportFp6_0 && weiSupportFp6_1) List(PE_MxMode.mode7) else List()) ++
    (if (actSupportFp6_0 && weiSupportFp6_0) List(PE_MxMode.mode8) else List()) ++
    (if (actSupportFp6_0 && weiSupportFp8_1) List(PE_MxMode.mode7) else List()) ++
    (if (actSupportFp6_0 && weiSupportFp8_0) List(PE_MxMode.mode8) else List()) ++

    (if (actSupportFp8_1 && weiSupportFp4) List(PE_MxMode.mode3) else List()) ++
    (if (actSupportFp8_1 && weiSupportFp6_1) List(PE_MxMode.mode4) else List()) ++
    (if (actSupportFp8_1 && weiSupportFp6_0) List(PE_MxMode.mode5) else List()) ++
    (if (actSupportFp8_1 && weiSupportFp8_1) List(PE_MxMode.mode4) else List()) ++
    (if (actSupportFp8_1 && weiSupportFp8_0) List(PE_MxMode.mode5) else List()) ++

    (if (actSupportFp8_0 && weiSupportFp4) List(PE_MxMode.mode6) else List()) ++
    (if (actSupportFp8_0 && weiSupportFp6_1) List(PE_MxMode.mode7) else List()) ++
    (if (actSupportFp8_0 && weiSupportFp6_0) List(PE_MxMode.mode8) else List()) ++
    (if (actSupportFp8_0 && weiSupportFp8_1) List(PE_MxMode.mode7) else List()) ++
    (if (actSupportFp8_0 && weiSupportFp8_0) List(PE_MxMode.mode8) else List())
  ).distinct

  val mxGemminiModes = PE_MxMode.mxGemminiConfig

  val mxparameters = MxParams(mxGemminiModes)
  val peInAWidth = mxparameters.inPE_act_totalWidth
  val peInBWidth = mxparameters.inPE_wei_totalWidth
  val peOutWidth = mxparameters.outPE_width 
}

// -----------------------------------------------------------------------------
// BUNDLES AND DECODING
// -----------------------------------------------------------------------------

class mxMode extends Bundle {
  val actWidth = UInt(3.W)
  val weiWidth = UInt(3.W) 
  val weiInputs = UInt(3.W)
  val actInputs = UInt(2.W)
  val shift = Vec(2, Vec(2, UInt(3.W)))
  val numOutputs = UInt(3.W)
}

class MxTypes extends Bundle {
  val exp = UInt(3.W)
  val sig = UInt(3.W)
}

case class MxFormats (
  val exponent: Int = 4,
  val significand: Int = 4
) {
  val exp = exponent
  val sig = significand
  val ieee = exp + sig
  val recoded = exp + sig + 1
  val bias: Int = (1 << (exponent - 1)) - 1
}

object MxFormats {
  def fp4 = MxFormats(2, 2)
  def fp6_0 = MxFormats(2, 4)
  def fp6_1 = MxFormats(3, 3)
  def fp8_0 = MxFormats(4, 4)
  def fp8_1 = MxFormats(5, 3)
}

object MxTypes {
  def apply(code: UInt, altfmt: Bool): MxTypes = {
    val tbl = VecInit(Seq(
      (new MxTypes).Lit(_.exp -> 2.U, _.sig -> 2.U), // 0 fp4
      (new MxTypes).Lit(_.exp -> 2.U, _.sig -> 4.U), // 1 fp6_0
      (new MxTypes).Lit(_.exp -> 4.U, _.sig -> 4.U), // 3 fp8_0
      (new MxTypes).Lit(_.exp -> 3.U, _.sig -> 3.U), // 2 fp6_1
      (new MxTypes).Lit(_.exp -> 5.U, _.sig -> 3.U)  // 4 fp8_1
  ))
    Mux(altfmt, WireInit(tbl(code +& 2.U)), WireInit(tbl(code)))
  }
}

// -----------------------------------------------------------------------------
// MODE DECODER
// -----------------------------------------------------------------------------

object mxModeDecode {
  private def litTable = VecInit(PE_MxMode.allModes.map { m =>
    (new mxMode).Lit(
      _.actWidth      -> m.actWidth.U,
      _.weiWidth      -> m.weiWidth.U,
      _.weiInputs     -> m.weiInputs.U,
      _.actInputs     -> m.actInputs.U,
      _.shift(0)(0)   -> m.shift(0)(0).U, _.shift(0)(1) -> m.shift(0)(1).U,
      _.shift(1)(0)   -> m.shift(1)(0).U, _.shift(1)(1) -> m.shift(1)(1).U,
      _.numOutputs    -> m.numOutputs.U
    )
  })
  def apply(mode: UInt): mxMode = litTable(mode)
}

case class PE_MxMode(
  actWidth: Int = 2,
  weiWidth: Int = 2,
  actTotalWidth: Int = 4,
  weiTotalWidth: Int = 4,
  weiInputs: Int = 2,
  actInputs: Int = 2,
  shift: Seq[Seq[Int]] = Seq.fill(2,2){0},
  outTotalWidth: Int = 16,
  numOutputs: Int = 4
)

object PE_MxMode {
  def mode0 = PE_MxMode()
  def mode1 = PE_MxMode().copy(
    weiWidth = 3, 
    weiTotalWidth = 12,
    outTotalWidth = 20 
  )
  def mode2 = PE_MxMode().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    weiWidth = 4,
    weiInputs = 1,
    shift = Seq(Seq(2,0), Seq(2,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode3 = PE_MxMode().copy(
    actWidth = 3, 
    actTotalWidth = 6,
    outTotalWidth = 20 
  )
  def mode4 = PE_MxMode().copy(
    actTotalWidth = 6,
    weiTotalWidth = 6,
    actWidth = 3,
    weiWidth = 3,
    outTotalWidth = 24 
  )
  def mode5 = PE_MxMode().copy(
    actTotalWidth = 6,
    weiTotalWidth = 4,
    actWidth = 3,
    weiWidth = 4,
    weiInputs = 1,
    shift = Seq(Seq(2,0), Seq(2,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode6 = PE_MxMode().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    weiInputs = 2,
    actInputs = 1,
    actWidth = 4,
    shift = Seq(Seq(2,2), Seq(0,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode7 = PE_MxMode().copy(
    actTotalWidth = 4,
    weiTotalWidth = 6,
    actWidth = 4,
    weiWidth = 3,
    weiInputs = 2,
    actInputs = 1,
    shift = Seq(Seq(2,2), Seq(0,0)),
    outTotalWidth = 14,
    numOutputs = 2
  )
  def mode8 = PE_MxMode().copy(
    actTotalWidth = 4,
    weiTotalWidth = 4,
    actWidth = 4,
    weiWidth = 4,
    weiInputs = 1,
    actInputs = 1,
    shift = Seq(Seq(4,2), Seq(2,0)),
    outTotalWidth = 8,
    numOutputs = 1
  )
  val allModes: List[PE_MxMode] = List(mode0, mode1, mode2, mode3, mode4, mode5, mode6, mode7, mode8)
  val mxGemminiConfig: List[PE_MxMode] = List(mode0, mode4, mode8)
}

case class MxParams (
  modesSupported: List[PE_MxMode] = List(PE_MxMode.mode0),
) {
  val inPE_act_width: Int = modesSupported.map(_.actWidth).max
  val actflexMulInWidth: Int = if (modesSupported.exists(m => m.actWidth == 3)) 3 else 2
  val inPE_wei_width: Int = modesSupported.map(_.weiWidth).max
  val weiflexMulInWidth: Int = if (modesSupported.exists(m => m.weiWidth == 3)) 3 else 2
  val inPE_act_totalWidth: Int = modesSupported.map(_.actTotalWidth).max
  val inPE_wei_totalWidth: Int = modesSupported.map(_.weiTotalWidth).max
  val numWeiInputs: Int = modesSupported.map(_.weiInputs).max
  val outPE_width: Int = modesSupported.map(_.outTotalWidth).max
  val multOutWidth = if (modesSupported.exists(m => m.actWidth == 3 && m.weiWidth == 3)) 6 
                   else if (modesSupported.exists(m => (m.actWidth, m.weiWidth) == (3,2) || (m.actWidth, m.weiWidth) == (2,3) ||
                                                        (m.actWidth, m.weiWidth) == (3,4) || (m.actWidth, m.weiWidth) == (4,3))) 5 
                   else 4
}

object MxParams {
  def fp4 = MxParams()
  def fp6 = MxParams(List(PE_MxMode.mode4, PE_MxMode.mode8, PE_MxMode.mode5, PE_MxMode.mode7))
  def fp6_0 = MxParams(List(PE_MxMode.mode2, PE_MxMode.mode5, PE_MxMode.mode6, PE_MxMode.mode7, PE_MxMode.mode8))
  def fp6_1 = MxParams(List(PE_MxMode.mode1, PE_MxMode.mode3, PE_MxMode.mode4, PE_MxMode.mode5, PE_MxMode.mode7))
  def fp8_0 = MxParams(List(PE_MxMode.mode2, PE_MxMode.mode5, PE_MxMode.mode6, PE_MxMode.mode7, PE_MxMode.mode8))
  def fp8_1 = MxParams(List(PE_MxMode.mode1, PE_MxMode.mode3, PE_MxMode.mode4, PE_MxMode.mode5, PE_MxMode.mode7))
  def allfp4 = MxParams(List(PE_MxMode.mode0, PE_MxMode.mode1, PE_MxMode.mode2, PE_MxMode.mode3, PE_MxMode.mode6))
  def allfp6 = MxParams(List(PE_MxMode.mode1, PE_MxMode.mode3, PE_MxMode.mode4, PE_MxMode.mode5, PE_MxMode.mode7))
  def allfp8 = MxParams(List(PE_MxMode.mode2, PE_MxMode.mode5, PE_MxMode.mode6, PE_MxMode.mode7, PE_MxMode.mode8))
  def all = MxParams(List(PE_MxMode.mode0, PE_MxMode.mode1, PE_MxMode.mode2, PE_MxMode.mode3, PE_MxMode.mode4, PE_MxMode.mode5, PE_MxMode.mode6, PE_MxMode.mode7, PE_MxMode.mode8))
}
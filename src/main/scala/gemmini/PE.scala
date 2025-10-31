// See README.md for license details.
package gemmini

import chisel3._
import chisel3.util._
//import mxHardware._  // Import MxFp types from mxHardware package

class PEControl[T <: Data : Arithmetic](accType: T) extends Bundle {
  val dataflow = UInt(1.W)
  val propagate = UInt(1.W)
  val shift = UInt(log2Up(accType.getWidth).W)
  
  // MxFp control signals
  val in_a_type = UInt(2.W)
  val a_altfmt = Bool()
  val in_w_type = UInt(2.W)
  val w_altfmt = Bool()
}

// MxFp MAC wrapper to match MacUnit interface
class MxFpMacUnit[T <: Data](
  inputType: T, 
  weightType: T, 
  cType: T, 
  dType: T,
  supportedTypes: TypeSupport,
  lut: Boolean
)(implicit ev: Arithmetic[T]) extends Module {
  
  val io = IO(new Bundle {
    val in_a  = Input(inputType)
    val in_b  = Input(weightType)
    val in_c  = Input(cType)
    val out_d = Output(dType)
    
    // MxFp specific controls
    val in_a_type = Input(UInt(2.W))
    val a_altfmt = Input(Bool())
    val in_w_type = Input(UInt(2.W))
    val w_altfmt = Input(Bool())
    val enable = Input(Bool())
  })

  // Instantiate MxFpMul
  val mxfp_mul = Module(new MxFpMul(supportedTypes, lut))
  
  // Connect inputs - convert Data types to UInt
  mxfp_mul.io.in_activation := io.in_a.asUInt
  mxfp_mul.io.in_a_type := io.in_a_type
  mxfp_mul.io.a_altfmt := io.a_altfmt
  mxfp_mul.io.in_weights := io.in_b.asUInt
  mxfp_mul.io.in_w_type := io.in_w_type
  mxfp_mul.io.w_altfmt := io.w_altfmt
  mxfp_mul.io.enable := io.enable
  mxfp_mul.io.rec_c := io.in_c.asUInt
  
  // Connect output - MxFpMul outputs 4 lanes, take the result
  // The exact extraction depends on PE configuration
  io.out_d := mxfp_mul.io.out.asTypeOf(dType)
}

class MacUnit[T <: Data](inputType: T, weightType: T, cType: T, dType: T) 
  (implicit ev: Arithmetic[T]) extends Module {
  import ev._
  val io = IO(new Bundle {
    val in_a  = Input(inputType)
    val in_b  = Input(weightType)
    val in_c  = Input(cType)
    val out_d = Output(dType)
  })

  io.out_d := io.in_c.mac(io.in_a, io.in_b)
}

class PE[T <: Data](
  inputType: T, 
  weightType: T, 
  outputType: T, 
  accType: T, 
  df: Dataflow.Value, 
  max_simultaneous_matmuls: Int,
  useMxFp: Boolean = false,
  mxfpSupportedTypes: Option[TypeSupport] = None,
  useMxfpLUT: Boolean = false
)(implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    val in_a = Input(inputType)
    val in_b = Input(outputType)
    val in_d = Input(outputType)
    val out_a = Output(inputType)
    val out_b = Output(outputType)
    val out_c = Output(outputType)

    val in_control = Input(new PEControl(accType))
    val out_control = Output(new PEControl(accType))

    val in_id = Input(UInt(log2Up(max_simultaneous_matmuls).W))
    val out_id = Output(UInt(log2Up(max_simultaneous_matmuls).W))

    val in_last = Input(Bool())
    val out_last = Output(Bool())

    val in_valid = Input(Bool())
    val out_valid = Output(Bool())

    val bad_dataflow = Output(Bool())
  })

  val cType = if (df == Dataflow.WS) inputType else accType

  // When creating PEs that support multiple dataflows, the
  // elaboration/synthesis tools often fail to consolidate and de-duplicate
  // MAC units. To force mac circuitry to be re-used, we create a "mac_unit"
  // module here which just performs a single MAC operation
  
  // Create the MAC unit based on configuration
  val (mac_standard, mac_mxfp) = if (useMxFp && mxfpSupportedTypes.isDefined) {
    (None, Some(Module(new MxFpMacUnit(inputType, weightType,
      if (df == Dataflow.WS) outputType else accType, outputType,
      mxfpSupportedTypes.get, useMxfpLUT))))
  } else {
    (Some(Module(new MacUnit(inputType, weightType,
      if (df == Dataflow.WS) outputType else accType, outputType))), None)
  }
  
  // Helper to access MAC IO regardless of type
  def mac_in_a = if (mac_mxfp.isDefined) mac_mxfp.get.io.in_a else mac_standard.get.io.in_a
  def mac_in_b = if (mac_mxfp.isDefined) mac_mxfp.get.io.in_b else mac_standard.get.io.in_b
  def mac_in_c = if (mac_mxfp.isDefined) mac_mxfp.get.io.in_c else mac_standard.get.io.in_c
  def mac_out_d = if (mac_mxfp.isDefined) mac_mxfp.get.io.out_d else mac_standard.get.io.out_d

  val a  = io.in_a
  val b  = io.in_b
  val d  = io.in_d
  val c1 = Reg(cType)
  val c2 = Reg(cType)
  val dataflow = io.in_control.dataflow
  val prop  = io.in_control.propagate
  val shift = io.in_control.shift
  val id = io.in_id
  val last = io.in_last
  val valid = io.in_valid

  io.out_a := a
  io.out_control.dataflow := dataflow
  io.out_control.propagate := prop
  io.out_control.shift := shift
  io.out_control.in_a_type := io.in_control.in_a_type
  io.out_control.a_altfmt := io.in_control.a_altfmt
  io.out_control.in_w_type := io.in_control.in_w_type
  io.out_control.w_altfmt := io.in_control.w_altfmt
  io.out_id := id
  io.out_last := last
  io.out_valid := valid

  mac_in_a := a

  // Connect MxFp specific signals if using MxFp
  if (useMxFp && mxfpSupportedTypes.isDefined) {
    mac_mxfp.get.io.in_a_type := io.in_control.in_a_type
    mac_mxfp.get.io.a_altfmt := io.in_control.a_altfmt
    mac_mxfp.get.io.in_w_type := io.in_control.in_w_type
    mac_mxfp.get.io.w_altfmt := io.in_control.w_altfmt
    mac_mxfp.get.io.enable := valid
  }

  val last_s = RegEnable(prop, valid)
  val flip = last_s =/= prop
  val shift_offset = Mux(flip, shift, 0.U)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  io.bad_dataflow := false.B
  when ((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && dataflow === OUTPUT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := (c1 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      mac_in_b := b.asTypeOf(weightType)
      mac_in_c := c2
      c2 := mac_out_d
      c1 := d.withWidthOf(cType)
    }.otherwise {
      io.out_c := (c2 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      mac_in_b := b.asTypeOf(weightType)
      mac_in_c := c1
      c1 := mac_out_d
      c2 := d.withWidthOf(cType)
    }
  }.elsewhen ((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := c1
      mac_in_b := c2.asTypeOf(weightType)
      mac_in_c := b
      io.out_b := mac_out_d
      c1 := d
    }.otherwise {
      io.out_c := c2
      mac_in_b := c1.asTypeOf(weightType)
      mac_in_c := b
      io.out_b := mac_out_d
      c2 := d
    }
  }.otherwise {
    io.bad_dataflow := true.B
    //assert(false.B, "unknown dataflow")
    io.out_c := DontCare
    io.out_b := DontCare
    mac_in_b := b.asTypeOf(weightType)
    mac_in_c := c2
  }

  when (!valid) {
    c1 := c1
    c2 := c2
    mac_in_b := DontCare
    mac_in_c := DontCare
  }
}
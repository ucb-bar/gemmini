// See README.md for license details.
package gemmini

import chisel3._
import chisel3.util._

class PEControl[T <: Data : Arithmetic](accType: T) extends Bundle {
  val dataflow = UInt(1.W) // TODO make this an Enum
  val propagate = UInt(1.W) // Which register should be propagated (and which should be accumulated)?
  val shift = UInt(log2Up(accType.getWidth).W)

  override def cloneType: PEControl.this.type = new PEControl(accType).asInstanceOf[this.type]
}

// TODO update documentation
/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  */
class PE[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value, latency: Int)
                   (implicit ev: Arithmetic[T]) extends Module { // Debugging variables
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

    val in_valid = Input(Bool())
    val out_valid = Output(Bool())
  })

  val cType = if (df == Dataflow.WS) inputType else accType

  val a  = ShiftRegister(io.in_a, latency)
  val b  = ShiftRegister(io.in_b, latency)
  val d  = ShiftRegister(io.in_d, latency)
  val c1 = Reg(cType)
  val c2 = Reg(cType)
  val dataflow = ShiftRegister(io.in_control.dataflow, latency)
  val prop  = ShiftRegister(io.in_control.propagate, latency)
  val shift = ShiftRegister(io.in_control.shift, latency)
  val valid = ShiftRegister(io.in_valid, latency) // TODO should we clockgate the rest of the ShiftRegisters based on the values in this ShiftRegisters

  io.out_a := a
  io.out_control.dataflow := dataflow
  io.out_control.propagate := prop
  io.out_control.shift := shift
  io.out_valid := valid

  val last_s = RegEnable(prop, valid)
  val flip = last_s =/= prop
  val shift_offset = Mux(flip, shift, 0.U)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  when ((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && dataflow === OUTPUT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := (c1 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c2 := c2.mac(a, b.withWidthOf(inputType))
      c1 := d
    }.otherwise {
      io.out_c := (c2 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c1 := c1.mac(a, b.withWidthOf(inputType))
      c2 := d
    }
  }.elsewhen ((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHT_STATIONARY)) {
    when(prop === PROPAGATE) {
      io.out_c := c1
      io.out_b := b.mac(a, c2.withWidthOf(inputType))
      c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := b.mac(a, c1.withWidthOf(inputType))
      c2 := d
    }
  }.otherwise {
    assert(false.B, "unknown dataflow")
    io.out_c := DontCare
    io.out_b := DontCare
  }

  when (!valid) {
    c1 := c1
    c2 := c2
  }
}

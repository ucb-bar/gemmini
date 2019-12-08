// See README.md for license details.
package gemmini

import chisel3._
import chisel3.util._

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
    val in_s = Input(UInt(2.W))
    val out_a = Output(inputType)
    val out_b = Output(outputType)
    val out_c = Output(outputType)
    val out_s = Output(UInt(2.W))

    val in_shift = Input(UInt(log2Ceil(accType.getWidth).W)) // TODO does this have to be able to shift everything?
    val out_shift = Output(UInt(log2Ceil(accType.getWidth).W))

    val in_garbage = Input(Bool())
    val out_garbage = Output(Bool())
  })

  val cType = if (df == Dataflow.WS) inputType else accType

  val a  = ShiftRegister(io.in_a, latency)
  val b  = ShiftRegister(io.in_b, latency)
  val d  = ShiftRegister(io.in_d, latency)
  val c1 = Reg(cType)
  val c2 = Reg(cType)
  val s  = ShiftRegister(io.in_s, latency)
  val shift = ShiftRegister(io.in_shift, latency)
  val garbage = ShiftRegister(io.in_garbage, latency) // TODO should we clockgate the rest of the ShiftRegisters based on the values in this ShiftRegisters

  io.out_s := s
  io.out_a := a
  io.out_shift := shift
  io.out_garbage := garbage

  val last_s = RegEnable(s, !garbage)
  val flip = last_s =/= s
  val shift_offset = Mux(flip, shift, 0.U)

  val select = s(0)
  val mode = s(1)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  when ((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && mode === OUTPUT_STATIONARY)) {
    when(select === PROPAGATE) {
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
  }.elsewhen ((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && mode === WEIGHT_STATIONARY)) {
    when(select === PROPAGATE) {
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

  when (garbage) {
    c1 := c1
    c2 := c2
  }
}

// See README.md for license details.
package systolic

import chisel3._
import chisel3.util._

// TODO update documentation
/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  */
class PE[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value)
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

    val in_shift = Input(UInt((accType.getWidth - inputType.getWidth).W))
    val out_shift = Output(UInt(log2Ceil(accType.getWidth - inputType.getWidth + 1).W))

    val in_garbage = Input(Bool())
    val out_garbage = Output(Bool())
  })

  val a  = io.in_a
  val b  = io.in_b
  val d  = io.in_d
  val c1 = Reg(accType)
  val c2 = Reg(accType)
  val s  = io.in_s

  val last_s = RegEnable(s, !io.in_garbage)
  val flip = last_s =/= s
  val shift_offset = Mux(flip, io.in_shift, 0.U)

  io.out_s := s
  io.out_a := a
  io.out_shift := io.in_shift
  io.out_garbage := io.in_garbage

  val select = s(0)
  val mode = s(1)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = Dataflow.OS.id.U(1.W)
  val WEIGHT_STATIONARY = Dataflow.WS.id.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  when ((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && mode === OUTPUT_STATIONARY)) {
    when(select === PROPAGATE){
      io.out_c := (c1 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c2 := (a * b.withWidthOf(inputType)) + c2
      c1 := d
    }.otherwise {
      io.out_c := (c2 >> shift_offset).clippedToWidthOf(outputType)
      io.out_b := b
      c1 := (a * b.withWidthOf(inputType)) + c1
      c2 := d
    }
  }.elsewhen ((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && mode === WEIGHT_STATIONARY)) {
    when(select === PROPAGATE){
      io.out_c := c1
      io.out_b := (a * c2.withWidthOf(inputType)) + b
      c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := (a * c1.withWidthOf(inputType)) + b
      c2 := d
    }
  }.otherwise {
    assert(false.B, "unknown dataflow")
    io.out_c := DontCare
    io.out_b := DontCare
  }

  when (io.in_garbage) {
    c1 := c1
    c2 := c2
  }
}

// See README.md for license details.
package systolic

import chisel3._
import chisel3.util._

/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  * @param pass_through If false, the PE pipelines in_a, in_b, in_d, in_s for 1 cycle
  */
class PE(width: Int, df: Dataflow.Value, pass_through: Boolean = true,
         should_print: Boolean = false, r:Int = 0, c: Int = 0) extends Module { // Debugging variables
  val io = IO(new Bundle {
    val in_a =   Input(UInt(width.W))
    val out_a =  Output(UInt(width.W))
    // TODO: why is in_b 2*width and not width
    val in_b =   Input(UInt((2*width).W))
    val out_b =  Output(UInt((2*width).W))
    val in_s =   Input(UInt(2.W))
    val out_s =  Output(UInt(2.W))
    val in_d =   Input(UInt((2*width).W))
    val out_c =  Output(UInt((2*width).W))

    // Global signals
    val pause = Input(Bool())
  })

  val a  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  val b  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  val d  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  // TODO: potential for overflow in internal accumulators (add assertion) (use explicit width)
  val c1  = RegInit(0.U((2*width).W))
  val c2  = RegInit(0.U((2*width).W))
  val s  = if (pass_through) Wire(UInt(3.W)) else RegInit(0.U)

  a := io.in_a
  b := io.in_b
  d := io.in_d
  s := io.in_s
  io.out_s := s
  io.out_a := a

  val select = s(0)
  val mode = s(1)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = (Dataflow.OS.id).U(1.W)
  val WEIGHT_STATIONARY = (Dataflow.WS.id).U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  def add_os_conditions() = {
    when(select === PROPAGATE){
      io.out_c := c1
      io.out_b := b
      c2 := (a*b) + c2
      c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := b
      c1 := (a*b) + c1
      c2 := d
    }
  }

  def add_ws_conditions() = {
    when(select === PROPAGATE){
      io.out_c := c1
      io.out_b := (a*c2) + b
      c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := (a*c1) + b
      c2 := d
    }
  }

  if (df == Dataflow.OS) {
    add_os_conditions()
  } else if (df == Dataflow.WS) {
    add_ws_conditions()
  } else { // if (df == Dataflow.BOTH) {
    when (mode === OUTPUT_STATIONARY) {
      add_os_conditions()
    }.otherwise {
      add_ws_conditions()
    }
  }

  when (io.pause) {
    if (!pass_through) {
      a := a
      b := b
      d := d
      s := s
    }

    c1 := c1
    c2 := c2
  }

  if (should_print) {
    when(!io.pause) {
      printf(p"($r, $c) a=$a, b=$b, c1=$c1, c2=$c2, out_c=${io.out_c}, io.d=${io.in_d}, in_s=${io.in_s(0)}, pause=${io.pause}\n")
    }
  }
}

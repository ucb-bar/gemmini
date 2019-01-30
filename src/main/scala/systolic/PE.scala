// See README.md for license details.
package systolic

import chisel3._
import chisel3.util._

/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  * @param pass_through If false, the PE pipelines in_a, in_b, in_d, in_s for 1 cycle
  */
class PE(width: Int, pass_through: Boolean = true, should_print: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val in_a =   Input(UInt(width.W))
    val out_a =  Output(UInt(width.W))
    // TODO: why is in_b 2*width and not width
    val in_b =   Input(UInt((2*width).W))
    val out_b =  Output(UInt((2*width).W))
    val in_s =   Input(UInt(3.W))
    val out_s =  Output(UInt(3.W))
    val in_d =   Input(UInt((2*width).W))
    val out_c =  Output(UInt((2*width).W))
  })

  val a  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  val b  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  val d  = if (pass_through) Wire(UInt()) else RegInit(0.U)
  // TODO: potential for overflow in internal accumulators (add assertion) (use explicit width)
  val c1  = RegInit(0.U)
  val c2  = RegInit(0.U)
  val s  = if (pass_through) Wire(UInt(3.W)) else RegInit(0.U)

  a := io.in_a
  b := io.in_b
  d := io.in_d
  s := io.in_s
  io.out_s := s
  io.out_a := a

  val select = s(0)
  val mode = s(1)
  val pause = s(2)

  // Which dataflow are we using?
  val OUTPUT_STATIONARY = 0.U(1.W)
  val WEIGHT_STATIONARY = 1.U(1.W)

  // Is c1 being computed on, or propagated forward (in the output-stationary dataflow)?
  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  // Is d being propagated forward or not?
  val CONTINUE = 0.U(1.W)
  val PAUSE = 1.U(1.W)

  when (mode === OUTPUT_STATIONARY) {
    when(select === PROPAGATE){
      io.out_c := c1
      io.out_b := b
      c2 := (a*b) + c2
      when (!pause) { c1 := d }
    }.otherwise {
      io.out_c := c2
      io.out_b := b
      c1 := (a*b) + c1
      when (!pause) { c2 := d }
    }
  }.otherwise {
    when(select === PROPAGATE){
      io.out_c := c1
      io.out_b := (a*c2) + b
      when (!pause) { c1 := d }
      // c1 := d
    }.otherwise {
      io.out_c := c2
      io.out_b := (a*c1) + b
      when (!pause) { c2 := d }
      // c2 := d
    }
  }

  if (should_print) {
    // printf(p"($a * $b) = $c1, $c2 ($pause)\n")
    printf(p"a=$a, b=$b, c1=$c1, c2=$c2\n")
  }
}

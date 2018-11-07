// See README.md for license details.

package systolic

import chisel3._

/**
  * Compute PE .
  */
class PE (width: Int) extends Module
{
  val io = IO(new Bundle {
    val in_a = Input(UInt(width.W))
    val out_a = Output(UInt(width.W))
    val in_b = Input(UInt((2*width).W))
    val in_s = Input(UInt(2.W))
    val out_s = Output(UInt(2.W))
    val out  = Output(UInt((2*width).W))
  })

  val a  = RegInit(0.U)
  val b  = RegInit(0.U)
  val c  = RegInit(0.U)
  val s  = RegInit(0.U)
   
  a := io.in_a
  b := io.in_b
  s := io.in_s
  io.out_s := s
  io.out_a := a

  val mode = s(1)
  val select = s(0)

  val OUTPUT_STATIONARY = 0.U(1.W)
  val WEIGHT_STATIONARY = 1.U(1.W)

  val COMPUTE = 0.U(1.W)
  val PROPAGATE = 1.U(1.W)

  when (mode === OUTPUT_STATIONARY) {
    when(select === PROPAGATE){
      io.out := c
      c := b
    }.otherwise {
      io.out := b
      c := (a*b) + c
    }
  }.otherwise {
    when(select === PROPAGATE){
      io.out := c
      c := b
    }.otherwise {
      io.out := (a*c) + b
      c := c
    }
  }
}

// See README.md for license details.

package systolic

import chisel3._

/**
  * Compute PE .
  */
class PE (width: Int) extends Module 
  val io = IO(new Bundle {
    val in_a = Input(UInt(width.W))
    val out_a = Output(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val in_s = Input(UInt(1.W))
    val out_s = Output(UInt(1.W))
    val out  = Output(UInt((2*width).W))
  })



  val a  = Reg(UInt())
  val b  = Reg(UInt())
  val c  = Reg(UInt())
  val s  = Reg(UInt())
   
  a := io.in_a
  b := io.in_b
  c := io.in_c
  s := io.in_s
  io.out_s := s
  io.out_a := a

  when(s === 0.U){
    io.out := c
    c := b
  }.otherwise {
    io.out := b
    c := (a*b) + c
    }
}


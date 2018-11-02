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
    val in_b = Input(UInt(width.W))
    val in_s = Input(UInt(1.W))
    val out_s = Output(UInt(1.W))
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

  when(s === 1.U){
    io.out := c
    c := b
  }.otherwise {
    io.out := b
    c := (a*b) + c
  }
}


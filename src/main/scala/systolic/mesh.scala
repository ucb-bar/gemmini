// See README.md for license details.

package systolic

import chisel3._

/**
  * Computes Mesh output .
  */
class Mesh (val width: Int, val rows: Int, val columns: Int) extends Module
{
  val io = IO(new Bundle {
    val in_a_vec = Input(Vec(rows,UInt(width.W)))
    val in_b_vec = Input(Vec(columns,UInt((2*width).W)))
    val in_s_vec = Input(Vec(columns,UInt(2.W)))
    val out_vec  = Output(Vec(columns,UInt((2*width).W)))
  })

  val mesh = (0 until rows).map{_ => (0 until columns).map{_ => Module(new PE(width))}}

  for(r <- 0 until rows-1; c <- 0 until columns-1) {
    mesh(r+1)(c).io.in_s := mesh(r)(c).io.out_s
    mesh(r+1)(c).io.in_b := mesh(r)(c).io.out
    mesh(r)(c+1).io.in_a := mesh(r)(c).io.out_a

  }
  for(c <- 0 until columns) {
    if (c < columns-1){	
      mesh(rows-1)(c+1).io.in_a := mesh(rows-1)(c).io.out_a
    }
    io.out_vec(c) := mesh(rows-1)(c).io.out
    mesh(0)(c).io.in_b := io.in_b_vec(c) 
    mesh(0)(c).io.in_s := io.in_s_vec(c)
  }

  for(r <- 0 until rows) {
    if (r < rows-1){	
      mesh(r+1)(columns-1).io.in_b := mesh(r)(columns-1).io.out
      mesh(r+1)(columns-1).io.in_s := mesh(r)(columns-1).io.out_s
    }
    mesh(r)(0).io.in_a := io.in_a_vec(r) 
  }
}
 

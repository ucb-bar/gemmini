// See README.md for license details.

package systolic

import chisel3._

/**
  * Computes Mesh output .
  */
class Mesh (val width: Int, val rows: Int, val columns: Int, pass_through: Boolean) extends Module
{
  val io = IO(new Bundle {
    val in_a_vec = Input(Vec(rows,UInt(width.W)))
    val in_b_vec = Input(Vec(columns,UInt((2*width).W)))
    val in_s_vec = Input(Vec(columns,UInt(2.W)))
    val out_vec  = Output(Vec(columns,UInt((2*width).W)))
    val out_s_vec = Output(Vec(columns, UInt(2.W)))
    val out_a_vec = Output(Vec(rows, UInt(width.W)))
  })

  val mesh: Seq[Seq[PE]] = (0 until rows).map {
    _ => (0 until columns).map {
      _ => Module(new PE(width,pass_through))
    }
  }
  val meshT = mesh.transpose

  // Chain a
  for (r <- 0 until rows) {
    mesh(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  // Chain mesh_out -> mesh_b_in (connect b across each column)
  for (c <- 0 until columns) {
    meshT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, pe) =>
        pe.io.in_b := in_b
        pe.io.out
    }
  }

  // Chain s (connect s across each column)
  for (c <- 0 until columns) {
    meshT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, pe) =>
        pe.io.in_s := in_s
        pe.io.out_s
    }
  }

  // Capture out_vec (pipeline the output of the bottom row)
  for (c <- 0 until columns) {
    io.out_vec(c) := mesh(rows-1)(c).io.out
    io.out_s_vec(c) := mesh(rows-1)(c).io.out_s
  }

  for (r <- 0 until rows) {
    io.out_a_vec(r) := mesh(r)(columns-1).io.out_a
  }
}

 

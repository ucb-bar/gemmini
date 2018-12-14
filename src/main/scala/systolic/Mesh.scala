// See README.md for license details.

package systolic

import chisel3._

/**
  * A Mesh is a purely combinational 2D array of passThrough PEs.
  * a, b, s, and in_propag are broadcast across the entire array and are passed through to the Mesh's outputs
  * @param width The data width of each PE in bits
  * @param rows Number of PEs on each row
  * @param columns Number of PEs on each column
  */
class Mesh(val width: Int, val rows: Int, val columns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a_vec      = Input(Vec(rows,UInt(width.W)))
    val in_b_vec      = Input(Vec(columns,UInt((2*width).W)))
    val in_propag_vec = Input(Vec(columns,UInt((2*width).W)))
    val in_s_vec      = Input(Vec(columns,UInt(2.W)))
    val out_a_vec     = Output(Vec(rows, UInt(width.W)))
    val out_vec       = Output(Vec(columns,UInt((2*width).W)))
    val out_b_vec     = Output(Vec(columns,UInt((2*width).W)))
    val out_s_vec     = Output(Vec(columns, UInt(2.W)))
  })

  val mesh: Seq[Seq[PE]] = (0 until rows).map {
    _ => (0 until columns).map {
      _ => Module(new PE(width, pass_through = true))
    }
  }
  val meshT = mesh.transpose

  // TODO: abstract hori/vert broadcast, all these connections look the same
  // Broadcast 'a' horizontally across the Mesh
  for (r <- 0 until rows) {
    mesh(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  // Broadcast 'b' vertically across the Mesh
  for (c <- 0 until columns) {
    meshT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, pe) =>
        pe.io.in_b := in_b
        pe.io.out_b
    }
  }

  // Broadcast 'in_propag' vertically across the Mesh
  for (c <- 0 until columns) {
    meshT(c).foldLeft(io.in_propag_vec(c)) {
      case (in_propag, pe) =>
        pe.io.in_propag := in_propag
        pe.io.out
    }
  }

  // Broadcast 's' vertically across the Mesh
  for (c <- 0 until columns) {
    meshT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, pe) =>
        pe.io.in_s := in_s
        pe.io.out_s
    }
  }

  // Drive the Mesh's bottom IO
  for (c <- 0 until columns) {
    io.out_vec(c) := mesh(rows-1)(c).io.out
    io.out_b_vec(c) := mesh(rows-1)(c).io.out_b
    io.out_s_vec(c) := mesh(rows-1)(c).io.out_s
  }

  // Drive the Mesh's right IO
  for (r <- 0 until rows) {
    io.out_a_vec(r) := mesh(r)(columns-1).io.out_a
  }
}

object MeshMain extends App {
  chisel3.Driver.execute(args, () => new Mesh(8, 16, 16))
}

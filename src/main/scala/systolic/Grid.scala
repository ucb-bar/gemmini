
package systolic

import chisel3._

class Grid(val width: Int, val meshRows: Int, val meshColumns: Int,
           val gridRows: Int, val gridColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a_vec = Input(Vec(gridRows, Vec(meshRows, UInt(width.W))))
    val in_b_vec = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val in_s_vec = Input(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))
    val out_vec  = Output(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val out_s_vec  = Output(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))
  })

  // grid(r, c) => Mesh at row r, column c
  val grid: Seq[Seq[Mesh]] = (0 until gridRows).map {
    _ => (0 until gridColumns).map {
      _ => Module(new Mesh(width, meshRows, meshColumns, true))
    }
  }
  val gridT = grid.transpose

  // Chain mesh_a_out -> mesh_a_in (pipeline a across each row)
  for (r <- 0 until gridRows) {
    grid(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, mesh) =>
        mesh.io.in_a_vec := RegNext(in_a)
        mesh.io.out_a_vec
    }
  }

  // Chain mesh_out -> mesh_b_in (pipeline b across each column)
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, mesh) =>
        mesh.io.in_b_vec := RegNext(in_b)
        mesh.io.out_vec
    }
  }

  // Chain s (pipeline s across each column)
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, mesh) =>
        mesh.io.in_s_vec := RegNext(in_s)
        mesh.io.out_s_vec
    }
  }

  // Capture out_vec (pipeline the output of the bottom row)
  for (c <- 0 until gridColumns) {
    io.out_vec(c) := RegNext(grid(gridRows-1)(c).io.out_vec)
    // TODO: we have to double register s_out to treat it as a valid signal, maybe there's a better way
    io.out_s_vec(c) := RegNext(RegNext(grid(gridRows-1)(c).io.out_s_vec))
  }
}

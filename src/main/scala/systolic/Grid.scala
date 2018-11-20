
package systolic

import chisel3._

class Grid(val width: Int, val meshRows: Int, val meshColumns: Int,
           val gridRows: Int, val gridColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a_vec = Input(Vec(gridRows, Vec(meshRows, UInt(width.W))))
    val in_b_vec = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val in_s_vec = Input(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))
    val out_vec  = Output(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
  })

  // grid(r, c) => Mesh at row r, column c
  val grid: Seq[Seq[Mesh]] = (0 until gridRows).map {
    _ => (0 until gridColumns).map {
      _ => Module(new Mesh(width, meshRows, meshColumns, true))
    }
  }
  val gridT = grid.transpose

  // Chain mesh_a_out -> mesh_a_in (chain a)
  for (r <- 0 until gridRows) {
    grid(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, mesh) =>
        mesh.io.in_a_vec := RegNext(in_a)
        mesh.io.out_a_vec
    }
  }

  // Chain mesh_out -> mesh_b_in (chain b)
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, mesh) =>
        mesh.io.in_b_vec := RegNext(in_b)
        mesh.io.out_vec
    }
  }

  // Chain s
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, mesh) =>
        mesh.io.in_s_vec := RegNext(in_s)
        mesh.io.out_s_vec
    }
  }

  // Capture out_vec
  for (c <- 0 until gridColumns) {
    io.out_vec(c) := grid(gridRows-1)(c).io.out_vec
  }
/*
  for(r <- 0 until gridRows-1; c <- 0 until gridColumns-1) {
    grid(r+1)(c).io.in_s_vec := RegNext(grid(r)(c).io.out_s_vec)
    grid(r+1)(c).io.in_b_vec := RegNext(grid(r)(c).io.out_vec)
    if (c < gridColumns - 1) {
      grid(r)(c+1).io.in_a_vec := RegNext(grid(r)(c).io.out_a_vec)
    }
  }

  for(c <- 0 until gridColumns) {
    grid(0)(c).io.in_b_vec := RegNext(io.in_b_vec(c))
    io.out_vec(c) := mesh(rows-1)(c).io.out
    mesh(0)(c).io.in_b := io.in_b_vec(c)
    if (c < gridColumns - 1) {
      grid(r)(c+1).io.in_a_vec := RegNext(grid(r)(c).io.out_a_vec)
    }
  }

  for(r <- 0 until rows) {
    if (r < rows-1){
      mesh(r+1)(columns-1).io.in_b := mesh(r)(columns-1).io.out
      mesh(r+1)(columns-1).io.in_s := mesh(r)(columns-1).io.out_s
    }
    mesh(r)(0).io.in_a := io.in_a_vec(r)
  }
  */
}

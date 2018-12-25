
package systolic

import chisel3._

/**
  * A Grid is a 2D array of Mesh modules with registers in between each mesh and
  * registers from the bottom row and rightmost column of meshes to the Grid outputs.
  * @param width
  * @param meshRows
  * @param meshColumns
  * @param gridRows
  * @param gridColumns
  */
class Grid(width: Int, val meshRows: Int, val meshColumns: Int,
           val gridRows: Int, val gridColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a_vec      = Input(Vec(gridRows, Vec(meshRows, UInt(width.W))))
    val in_b_vec      = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val in_propag_vec = Input(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val in_s_vec      = Input(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))
    val out_vec       = Output(Vec(gridColumns, Vec(meshColumns, UInt((2*width).W))))
    val out_s_vec     = Output(Vec(gridColumns, Vec(meshColumns, UInt(2.W))))
  })

  // grid(r)(c) => Mesh at row r, column c
  val grid: Seq[Seq[Mesh]] = Seq.fill(gridRows, gridColumns)(Module(new Mesh(width, meshRows, meshColumns)))
  val gridT = grid.transpose

  // Chain mesh_a_out -> mesh_a_in (pipeline a across each row)
  for (r <- 0 until gridRows) {
    grid(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, mesh) =>
        mesh.io.in_a_vec := RegNext(in_a)
        mesh.io.out_a_vec
    }
  }

  // Chain mesh_out_b -> mesh_b_in (pipeline b across each column)
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, mesh) =>
        mesh.io.in_b_vec := RegNext(in_b)
        mesh.io.out_b_vec
    }
  }

  // Chain mesh_out -> mesh_propag (pipeline output across each column)
  for (c <- 0 until gridColumns) {
    gridT(c).foldLeft(io.in_propag_vec(c)) {
      case (in_propag, mesh) =>
        mesh.io.in_propag_vec := RegNext(in_propag)
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

object GridMain extends App {
  chisel3.Driver.execute(args, () => new Grid(16, 3, 2, 2, 2))
}

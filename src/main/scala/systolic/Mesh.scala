
package systolic

import chisel3._

/**
  * A Grid is a 2D array of Tile modules with registers in between each tile and
  * registers from the bottom row and rightmost column of tiles to the Grid outputs.
  * @param width
  * @param tileRows
  * @param tileColumns
  * @param meshRows
  * @param meshColumns
  */
class Mesh(width: Int, val tileRows: Int, val tileColumns: Int,
           val meshRows: Int, val meshColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a_vec      = Input(Vec(meshRows, Vec(tileRows, UInt(width.W))))
    val in_b_vec      = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val in_propag_vec = Input(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val in_s_vec      = Input(Vec(meshColumns, Vec(tileColumns, UInt(2.W))))
    val out_vec       = Output(Vec(meshColumns, Vec(tileColumns, UInt((2*width).W))))
    val out_s_vec     = Output(Vec(meshColumns, Vec(tileColumns, UInt(2.W))))

    val en            = Input(Bool())
  })

  // mesh(r)(c) => Tile at row r, column c
  val mesh: Seq[Seq[Tile]] = Seq.fill(meshRows, meshColumns)(Module(new Tile(width, tileRows, tileColumns)))
  val meshT = mesh.transpose

  // Chain tile_a_out -> tile_a_in (pipeline a across each row)
  for (r <- 0 until meshRows) {
    mesh(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, tile) =>
        tile.io.in_a_vec := RegNext(in_a)
        tile.io.out_a_vec
    }
  }

  // Chain tile_out_b -> tile_b_in (pipeline b across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, tile) =>
        tile.io.in_b_vec := RegNext(in_b)
        tile.io.out_b_vec
    }
  }

  // Chain tile_out -> tile_propag (pipeline output across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_propag_vec(c)) {
      case (in_propag, tile) =>
        tile.io.in_propag_vec := RegNext(in_propag)
        tile.io.out_vec
    }
  }

  // Chain s (pipeline s across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, tile) =>
        tile.io.in_s_vec := RegNext(in_s)
        tile.io.out_s_vec
    }
  }

  // Capture out_vec (pipeline the output of the bottom row)
  for (c <- 0 until meshColumns) {
    io.out_vec(c) := RegNext(mesh(meshRows-1)(c).io.out_vec)
    // TODO: we have to double register s_out to treat it as a valid signal, maybe there's a better way
    // io.out_s_vec(c) := RegNext(RegNext(mesh(meshRows-1)(c).io.out_s_vec))
    io.out_s_vec(c) := RegNext(mesh(meshRows-1)(c).io.out_s_vec)
  }

  // Connect enable signals
  mesh.flatten.foreach(_.io.en := io.en)
}

object GridMain extends App {
  chisel3.Driver.execute(args, () => new Mesh(16, 3, 2, 2, 2))
}


package systolic

import chisel3._
import chisel3.util._

/**
  * A Grid is a 2D array of Tile modules with registers in between each tile and
  * registers from the bottom row and rightmost column of tiles to the Grid outputs.
  * @param width
  * @param tileRows
  * @param tileColumns
  * @param meshRows
  * @param meshColumns
  */
class Mesh[T <: Data](inputType: T, outputType: T, accType: T,
                      df: Dataflow.Value,
                      val tileRows: Int, val tileColumns: Int,
                      val meshRows: Int, val meshColumns: Int)(implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    val in_a   = Input(Vec(meshRows, Vec(tileRows, inputType)))
    val in_b   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_d   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_s   = Input(Vec(meshColumns, Vec(tileColumns, UInt(2.W))))
    val out_b  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val out_c  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val out_s  = Output(Vec(meshColumns, Vec(tileColumns, UInt(2.W))))

    val in_shift = Input(Vec(meshColumns, Vec(tileColumns, UInt(log2Ceil(accType.getWidth - outputType.getWidth + 1).W))))

    val pause = Input(Bool())
  })

  // mesh(r)(c) => Tile at row r, column c
  val mesh: Seq[Seq[Tile[T]]] = Seq.fill(meshRows, meshColumns)(Module(new Tile(inputType, outputType, accType, df, tileRows, tileColumns)))
  val meshT = mesh.transpose

  // Chain tile_a_out -> tile_a_in (pipeline a across each row)
  for (r <- 0 until meshRows) {
    mesh(r).foldLeft(io.in_a(r)) {
      case (in_a, tile) =>
        tile.io.in_a := RegEnable(in_a, !io.pause)
        tile.io.out_a
    }
  }

  // Chain tile_out_b -> tile_b_in (pipeline b across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_b(c)) {
      case (in_b, tile) =>
        tile.io.in_b := RegEnable(in_b, !io.pause)
        tile.io.out_b
    }
  }

  // Chain tile_out -> tile_propag (pipeline output across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_d(c)) {
      case (in_propag, tile) =>
        tile.io.in_d := RegEnable(in_propag, !io.pause)
        tile.io.out_c
    }
  }

  // Chain s (pipeline s across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_s(c)) {
      case (in_s, tile) =>
        tile.io.in_s := RegEnable(in_s, !io.pause)
        tile.io.out_s
    }
  }

  // Chain in_shift (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_shift(c)) {
      case (in_sh, tile) =>
        tile.io.in_shift := RegEnable(in_sh, !io.pause)
        tile.io.out_shift
    }
  }

  // Capture out_vec and out_s_vec (connect IO to bottom row of mesh)
  // (The only reason we have so many zips is because Scala doesn't provide a zipped function for Tuple4)
  for (((b, c), (s, tile)) <- (io.out_b zip io.out_c) zip (io.out_s zip mesh.last)) {
    b := tile.io.out_b
    c := tile.io.out_c
    s := tile.io.out_s
  }

  // Connect global pause signals
  mesh.flatten.foreach(_.io.pause := io.pause)
}

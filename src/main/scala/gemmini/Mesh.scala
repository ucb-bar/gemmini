
package gemmini

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
class Mesh[T <: Data : Arithmetic](inputType: T, outputType: T, accType: T,
                      df: Dataflow.Value, pe_latency: Int,
                      val tileRows: Int, val tileColumns: Int,
                      val meshRows: Int, val meshColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a   = Input(Vec(meshRows, Vec(tileRows, inputType)))
    val in_b   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_d   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_control   = Input(Vec(meshColumns, Vec(tileColumns, new PEControl(accType))))
    val out_b  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val out_c  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))

    val in_valid = Input(Vec(meshColumns, Vec(tileColumns, Bool())))
    val out_valid = Output(Vec(meshColumns, Vec(tileColumns, Bool())))
  })

  // mesh(r)(c) => Tile at row r, column c
  val mesh: Seq[Seq[Tile[T]]] = Seq.fill(meshRows, meshColumns)(Module(new Tile(inputType, outputType, accType, df, pe_latency, tileRows, tileColumns)))
  val meshT = mesh.transpose

  // Chain tile_a_out -> tile_a_in (pipeline a across each row)
  // TODO clock-gate A signals with in_garbage
  for (r <- 0 until meshRows) {
    mesh(r).foldLeft(io.in_a(r)) {
      case (in_a, tile) =>
        tile.io.in_a := RegNext(in_a)
        tile.io.out_a
    }
  }

  // Chain tile_out_b -> tile_b_in (pipeline b across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_b(c), io.in_valid(c))) {
      case ((in_b, valid), tile) =>
        tile.io.in_b := RegEnable(in_b, valid.head)
        (tile.io.out_b, tile.io.out_valid)
    }
  }

  // Chain tile_out -> tile_propag (pipeline output across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_d(c), io.in_valid(c))) {
      case ((in_propag, valid), tile) =>
        tile.io.in_d := RegEnable(in_propag, valid.head)
        (tile.io.out_c, tile.io.out_valid)
    }
  }

  // Chain control signals (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_control(c), io.in_valid(c))) {
      case ((in_ctrl, valid), tile) =>
        (tile.io.in_control, in_ctrl, valid).zipped.foreach { case (tile_ctrl, ctrl, v) =>
          tile_ctrl.shift := RegEnable(ctrl.shift, v)
          tile_ctrl.dataflow := RegEnable(ctrl.dataflow, v)
          tile_ctrl.propagate := RegEnable(ctrl.propagate, v)
        }
        (tile.io.out_control, tile.io.out_valid)
    }
  }

  // Chain in_valid (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_valid(c)) {
      case (in_v, tile) =>
        tile.io.in_valid := RegNext(in_v)
        tile.io.out_valid
    }
  }

  // Capture out_vec and out_control_vec (connect IO to bottom row of mesh)
  // (The only reason we have so many zips is because Scala doesn't provide a zipped function for Tuple4)
  for (((b, c), (v, tile)) <- ((io.out_b zip io.out_c), (io.out_valid zip mesh.last)).zipped) {
    // TODO we pipelined this to make physical design easier. Consider removing these if possible
    // TODO shouldn't we clock-gate these signals with "garbage" as well?
    b := RegNext(tile.io.out_b)
    c := RegNext(tile.io.out_c)
    v := RegNext(tile.io.out_valid)
  }
}

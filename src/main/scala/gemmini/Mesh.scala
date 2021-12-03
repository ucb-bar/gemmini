
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

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
                                   df: Dataflow.Value, tree_reduction: Boolean, tile_latency: Int,
                                   max_simultaneous_matmuls: Int, output_delay: Int,
                                   val tileRows: Int, val tileColumns: Int,
                                   val meshRows: Int, val meshColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(Vec(meshRows, Vec(tileRows, inputType)))
    val in_b = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_d = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_control = Input(Vec(meshColumns, Vec(tileColumns, new PEControl(accType))))
    val in_id = Input(Vec(meshColumns, Vec(tileColumns, UInt(log2Up(max_simultaneous_matmuls).W)))) // The unique id of this particular matmul
    val in_last = Input(Vec(meshColumns, Vec(tileColumns, Bool())))
    val out_b = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val out_c = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val in_valid = Input(Vec(meshColumns, Vec(tileColumns, Bool())))
    val out_valid = Output(Vec(meshColumns, Vec(tileColumns, Bool())))
    val out_control = Output(Vec(meshColumns, Vec(tileColumns, new PEControl(accType))))
    val out_id = Output(Vec(meshColumns, Vec(tileColumns, UInt(log2Up(max_simultaneous_matmuls).W))))
    val out_last = Output(Vec(meshColumns, Vec(tileColumns, Bool())))
  })

  // mesh(r)(c) => Tile at row r, column c
  val mesh: Seq[Seq[Tile[T]]] = Seq.fill(meshRows, meshColumns)(Module(new Tile(inputType, outputType, accType, df, tree_reduction, max_simultaneous_matmuls, tileRows, tileColumns)))
  val meshT = mesh.transpose

  def pipe[T <: Data](valid: Bool, t: T, latency: Int): T = {
    // The default "Pipe" function apparently resets the valid signals to false.B. We would like to avoid using global
    // signals in the Mesh, so over here, we make it clear that the reset signal will never be asserted
    chisel3.withReset(false.B) { Pipe(valid, t, latency).bits }
  }

  // Chain tile_a_out -> tile_a_in (pipeline a across each row)
  // TODO clock-gate A signals with in_garbage
  for (r <- 0 until meshRows) {
    mesh(r).foldLeft(io.in_a(r)) {
      case (in_a, tile) =>
        tile.io.in_a := ShiftRegister(in_a, tile_latency+1)
        tile.io.out_a
    }
  }

  // Chain tile_out_b -> tile_b_in (pipeline b across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_b(c), io.in_valid(c))) {
      case ((in_b, valid), tile) =>
        tile.io.in_b := pipe(valid.head, in_b, tile_latency+1)
        (tile.io.out_b, tile.io.out_valid)
    }
  }

  // Chain tile_out -> tile_propag (pipeline output across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_d(c), io.in_valid(c))) {
      case ((in_propag, valid), tile) =>
        tile.io.in_d := pipe(valid.head, in_propag, tile_latency+1)
        (tile.io.out_c, tile.io.out_valid)
    }
  }

  // Chain control signals (pipeline across each column)
  assert(!(mesh.map(_.map(_.io.bad_dataflow).reduce(_||_)).reduce(_||_)))
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft((io.in_control(c), io.in_valid(c))) {
      case ((in_ctrl, valid), tile) =>
        (tile.io.in_control, in_ctrl, valid).zipped.foreach { case (tile_ctrl, ctrl, v) =>
          tile_ctrl.shift := pipe(v, ctrl.shift, tile_latency+1)
          tile_ctrl.dataflow := pipe(v, ctrl.dataflow, tile_latency+1)
          tile_ctrl.propagate := pipe(v, ctrl.propagate, tile_latency+1)
        }
        (tile.io.out_control, tile.io.out_valid)
    }
  }

  // Chain in_valid (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_valid(c)) {
      case (in_v, tile) =>
        tile.io.in_valid := ShiftRegister(in_v, tile_latency+1)
        tile.io.out_valid
    }
  }

  // Chain in_id (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_id(c)) {
      case (in_id, tile) =>
        tile.io.in_id := ShiftRegister(in_id, tile_latency+1)
        tile.io.out_id
    }
  }

  // Chain in_last (pipeline across each column)
  for (c <- 0 until meshColumns) {
    meshT(c).foldLeft(io.in_last(c)) {
      case (in_last, tile) =>
        tile.io.in_last := ShiftRegister(in_last, tile_latency+1)
        tile.io.out_last
    }
  }

  // Capture out_vec and out_control_vec (connect IO to bottom row of mesh)
  // (The only reason we have so many zips is because Scala doesn't provide a zipped function for Tuple4)
  for (((((((b, c), v), ctrl), id), last), tile) <- io.out_b zip io.out_c zip io.out_valid zip io.out_control zip io.out_id zip io.out_last zip mesh.last) {
    // TODO we pipelined this to make physical design easier. Consider removing these if possible
    // TODO shouldn't we clock-gate these signals with "garbage" as well?
    b := ShiftRegister(tile.io.out_b, output_delay)
    c := ShiftRegister(tile.io.out_c, output_delay)
    v := ShiftRegister(tile.io.out_valid, output_delay)
    ctrl := ShiftRegister(tile.io.out_control, output_delay)
    id := ShiftRegister(tile.io.out_id, output_delay)
    last := ShiftRegister(tile.io.out_last, output_delay)
  }
}

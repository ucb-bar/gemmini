// See README.md for license details.

package systolic

import chisel3._

/**
  * A Tile is a purely combinational 2D array of passThrough PEs.
  * a, b, s, and in_propag are broadcast across the entire array and are passed through to the Tile's outputs
  * @param width The data width of each PE in bits
  * @param rows Number of PEs on each row
  * @param columns Number of PEs on each column
  */
class Tile(val width: Int, val rows: Int, val columns: Int, should_print: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val in_a_vec    = Input(Vec(rows,UInt(width.W)))
    val in_b_vec    = Input(Vec(columns,UInt((2*width).W)))
    val in_d_vec    = Input(Vec(columns,UInt((2*width).W)))
    val in_s_vec    = Input(Vec(columns,UInt(3.W)))
    val out_a_vec   = Output(Vec(rows, UInt(width.W)))
    val out_c_vec     = Output(Vec(columns,UInt((2*width).W)))
    val out_b_vec   = Output(Vec(columns,UInt((2*width).W)))
    val out_s_vec   = Output(Vec(columns, UInt(3.W)))
  })

  val tile = {
    for (r <- 0 until rows)
      yield for (c <- 0 until columns)
        yield Module(new PE(width, true, should_print = should_print && (r == 0 && c == 0)))
  }
  val tileT = tile.transpose

  // TODO: abstract hori/vert broadcast, all these connections look the same
  // Broadcast 'a' horizontally across the Tile
  for (r <- 0 until rows) {
    tile(r).foldLeft(io.in_a_vec(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  // Broadcast 'b' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_b_vec(c)) {
      case (in_b, pe) =>
        pe.io.in_b := in_b
        pe.io.out_b
    }
  }

  // Broadcast 'd' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_d_vec(c)) {
      case (in_d, pe) =>
        pe.io.in_d := in_d
        pe.io.out_c
    }
  }

  // Broadcast 's' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_s_vec(c)) {
      case (in_s, pe) =>
        pe.io.in_s := in_s
        pe.io.out_s
    }
  }

  // Drive the Tile's bottom IO
  for (c <- 0 until columns) {
    io.out_c_vec(c) := tile(rows-1)(c).io.out_c
    io.out_b_vec(c) := tile(rows-1)(c).io.out_b
    io.out_s_vec(c) := tile(rows-1)(c).io.out_s
  }

  // Drive the Tile's right IO
  for (r <- 0 until rows) {
    io.out_a_vec(r) := tile(r)(columns-1).io.out_a
  }
}

object TileMain extends App {
  chisel3.Driver.execute(args, () => new Tile(8, 16, 16))
}

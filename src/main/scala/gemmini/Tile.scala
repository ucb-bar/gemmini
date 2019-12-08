// See README.md for license details.

package gemmini

import chisel3._
import chisel3.util._

/**
  * A Tile is a purely combinational 2D array of passThrough PEs.
  * a, b, s, and in_propag are broadcast across the entire array and are passed through to the Tile's outputs
  * @param width The data width of each PE in bits
  * @param rows Number of PEs on each row
  * @param columns Number of PEs on each column
  */
class Tile[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value, pe_latency: Int, val rows: Int, val columns: Int)
                     (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    val in_a     = Input(Vec(rows,inputType))
    val in_b     = Input(Vec(columns,outputType)) // This is the output of the tile next to it
    val in_d     = Input(Vec(columns,outputType))
    val in_s     = Input(Vec(columns,UInt(2.W)))
    val out_a    = Output(Vec(rows, inputType))
    val out_c    = Output(Vec(columns,outputType))
    val out_b    = Output(Vec(columns,outputType))
    val out_s    = Output(Vec(columns, UInt(2.W)))

    val in_shift = Input(Vec(columns, UInt(log2Ceil(accType.getWidth).W)))
    val out_shift = Output(Vec(columns, UInt(log2Ceil(accType.getWidth).W)))

    val in_garbage = Input(Vec(columns, Bool()))
    val out_garbage = Output(Vec(columns, Bool()))
  })

  val tile = Seq.fill(rows, columns)(Module(new PE(inputType, outputType, accType, df, pe_latency)))
  val tileT = tile.transpose

  // TODO: abstract hori/vert broadcast, all these connections look the same
  // Broadcast 'a' horizontally across the Tile
  for (r <- 0 until rows) {
    tile(r).foldLeft(io.in_a(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  // Broadcast 'b' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_b(c)) {
      case (in_b, pe) =>
        pe.io.in_b := in_b
        pe.io.out_b
    }
  }

  // Broadcast 'd' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_d(c)) {
      case (in_d, pe) =>
        pe.io.in_d := in_d
        pe.io.out_c
    }
  }

  // Broadcast 's' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_s(c)) {
      case (in_s, pe) =>
        pe.io.in_s := in_s
        pe.io.out_s
    }
  }

  // Broadcast 'shift_offset' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_shift(c)) {
      case (sh, pe) =>
        pe.io.in_shift := sh
        pe.io.out_shift
    }
  }

  // Broadcast 'garbage' vertically across the Tile
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_garbage(c)) {
      case (g, pe) =>
        pe.io.in_garbage := g
        pe.io.out_garbage
    }
  }

  // Drive the Tile's bottom IO
  for (c <- 0 until columns) {
    io.out_c(c) := tile(rows-1)(c).io.out_c
    io.out_b(c) := tile(rows-1)(c).io.out_b
    io.out_s(c) := tile(rows-1)(c).io.out_s
    io.out_shift(c) := tile(rows-1)(c).io.out_shift
    io.out_garbage(c) := tile(rows-1)(c).io.out_garbage
  }

  // Drive the Tile's right IO
  for (r <- 0 until rows) {
    io.out_a(r) := tile(r)(columns-1).io.out_a
  }
}

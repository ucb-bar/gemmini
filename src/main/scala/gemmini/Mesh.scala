
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
                                   df: Dataflow.Value, pe_latency: Int,
                                   val tileRows: Int, val tileColumns: Int,
                                   val meshRows: Int, val meshColumns: Int) extends Module {
  val io = IO(new Bundle {
    val in_a__   = Input(Vec(meshRows, Vec(tileRows, inputType)))
    val in_b__   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_d__   = Input(Vec(meshColumns, Vec(tileColumns, inputType)))
    val in_control   = Input(Vec(meshColumns, Vec(tileColumns, new PEControl(accType))))
    val out_c__  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))
    val out_b__  = Output(Vec(meshColumns, Vec(tileColumns, outputType)))

    val in_valid = Input(Vec(meshColumns, Vec(tileColumns, Bool())))
    val out_valid = Output(Vec(meshColumns, Vec(tileColumns, Bool())))
  })

  val bb = Module(new MeshBlackBox(inputType=inputType, outputType=outputType,
    rows=meshRows*tileRows, columns=meshColumns*tileColumns))

  bb.io.clock := clock
  bb.io.reset := reset.asBool
  bb.io.in_a__ := io.in_a__.asUInt()
  bb.io.in_b__ := io.in_b__.asUInt()
  bb.io.in_d__ := io.in_d__.asUInt()

  bb.io.in_control_dataflow := Cat(io.in_control.flatten.map(_.dataflow).reverse)
  bb.io.in_control_propagate := Cat(io.in_control.flatten.map(_.propagate).reverse)

  io.out_b__ := bb.io.out_b__.asTypeOf(io.out_b__)
  io.out_c__ := bb.io.out_c__.asTypeOf(io.out_c__)

  bb.io.in_valid := io.in_valid.asUInt()
  io.out_valid := bb.io.out_valid.asTypeOf(io.out_valid)
}

class MeshBlackBox[T <: Data : Arithmetic](inputType: T, outputType: T,
                                           val rows: Int, val columns: Int) extends BlackBox(Map(
  "MESHROWS" -> rows,
  "MESHCOLUMNS" -> columns,
  "INPUT_BITWIDTH" -> inputType.getWidth,
  "OUTPUT_BITWIDTH" -> outputType.getWidth
)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val reset  = Input(Bool())
    val in_a__   = Input(UInt((rows * inputType.getWidth).W))
    val in_d__   = Input(UInt((columns * inputType.getWidth).W))
    val in_b__   = Input(UInt((columns * inputType.getWidth).W))

    val in_control_dataflow = Input(UInt(columns.W))
    val in_control_propagate = Input(UInt(columns.W))

    val out_b__  = Output(UInt((columns * outputType.getWidth).W))
    val out_c__  = Output(UInt((columns * outputType.getWidth).W))

    val in_valid = Input(UInt(columns.W))
    val out_valid = Output(UInt(columns.W))
  })

  addResource("/vsrc/MeshBlackBox.v")
}

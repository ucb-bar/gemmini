package gemmini

import chisel3._
import chisel3.util._
import Util._

trait Transposer[T <: Data] extends Module {
  def dim: Int
  def dataType: T
  val io = IO(new Bundle {
    val inRow = Flipped(Decoupled(Vec(dim, dataType)))
    val outCol = Decoupled(Vec(dim, dataType))
  })
}

class PipelinedTransposer[T <: Data](val dim: Int, val dataType: T) extends Transposer[T] {
  require(isPow2(dim))
  val regArray = Seq.fill(dim, dim)(Reg(dataType))
  val regArrayT = regArray.transpose
  val sMoveUp :: sMoveLeft :: Nil = Enum(2)
  val state = RegInit(sMoveUp)
  val leftCounter = RegInit(0.U(log2Ceil(dim+1).W)) //(io.inRow.fire && state === sMoveLeft, dim+1)
  val upCounter = RegInit(0.U(log2Ceil(dim+1).W)) //Counter(io.inRow.fire && state === sMoveUp, dim+1)

  io.outCol.valid := 0.U
  io.inRow.ready := 0.U
  switch(state) {
    is(sMoveUp) {
      io.inRow.ready := upCounter <= dim.U
      io.outCol.valid := leftCounter > 0.U
      when(io.inRow.fire) {
        upCounter := upCounter + 1.U
      }
      when(upCounter === (dim-1).U) {
        state := sMoveLeft
        leftCounter := 0.U
      }
      when(io.outCol.fire) {
        leftCounter := leftCounter - 1.U
      }
    }
    is(sMoveLeft) {
      io.inRow.ready := leftCounter <= dim.U // TODO: this is naive
      io.outCol.valid := upCounter > 0.U
      when(leftCounter === (dim-1).U) {
        state := sMoveUp
      }
      when(io.inRow.fire) {
        leftCounter := leftCounter + 1.U
        upCounter := 0.U
      }
      when(io.outCol.fire) {
        upCounter := upCounter - 1.U
      }
    }
  }

  // Propagate input from bottom row to top row systolically in the move up phase
  // TODO: need to iterate over columns to connect Chisel values of type T
  // Should be able to operate directly on the Vec, but Seq and Vec don't mix (try Array?)
  for (colIdx <- 0 until dim) {
    regArray.foldRight(io.inRow.bits(colIdx)) {
      case (regRow, prevReg) =>
        when (state === sMoveUp) {
          regRow(colIdx) := prevReg
        }
        regRow(colIdx)
    }
  }

  // Propagate input from right side to left side systolically in the move left phase
  for (rowIdx <- 0 until dim) {
    regArrayT.foldRight(io.inRow.bits(rowIdx)) {
      case (regCol, prevReg) =>
        when (state === sMoveLeft) {
          regCol(rowIdx) := prevReg
        }
        regCol(rowIdx)
    }
  }

  // Pull from the left side or the top side based on the state
  for (idx <- 0 until dim) {
    when (state === sMoveUp) {
      io.outCol.bits(idx) := regArray(0)(idx)
    }.elsewhen(state === sMoveLeft) {
      io.outCol.bits(idx) := regArrayT(0)(idx)
    }.otherwise {
      io.outCol.bits(idx) := DontCare
    }
  }
}

class AlwaysOutTransposer[T <: Data](val dim: Int, val dataType: T) extends Transposer[T] {
  require(isPow2(dim))

  val LEFT_DIR = 0.U(1.W)
  val UP_DIR = 1.U(1.W)

  class PE extends Module {
    val io = IO(new Bundle {
      val inR = Input(dataType)
      val inD = Input(dataType)
      val outL = Output(dataType)
      val outU = Output(dataType)
      val dir = Input(UInt(1.W))
      val en = Input(Bool())
    })

    val reg = RegEnable(Mux(io.dir === LEFT_DIR, io.inR, io.inD), io.en)

    io.outU := reg
    io.outL := reg
  }

  val pes = Seq.fill(dim,dim)(Module(new PE))
  val counter = RegInit(0.U((log2Ceil(dim) max 1).W)) // TODO replace this with a standard Chisel counter
  val dir = RegInit(LEFT_DIR)

  // Wire up horizontal signals
  for (row <- 0 until dim; col <- 0 until dim) {
    val right_in = if (col == dim-1) io.inRow.bits(row) else pes(row)(col+1).io.outL
    pes(row)(col).io.inR := right_in
  }

  // Wire up vertical signals
  for (row <- 0 until dim; col <- 0 until dim) {
    val down_in = if (row == dim-1) io.inRow.bits(col) else pes(row+1)(col).io.outU
    pes(row)(col).io.inD := down_in
  }

  // Wire up global signals
  pes.flatten.foreach(_.io.dir := dir)
  pes.flatten.foreach(_.io.en := io.inRow.fire)

  io.outCol.valid := true.B
  io.inRow.ready := true.B

  val left_out = VecInit(pes.transpose.head.map(_.io.outL))
  val up_out = VecInit(pes.head.map(_.io.outU))

  io.outCol.bits := Mux(dir === LEFT_DIR, left_out, up_out)

  when (io.inRow.fire) {
    counter := wrappingAdd(counter, 1.U, dim)
  }

  when (counter === (dim-1).U && io.inRow.fire) {
    dir := ~dir
  }
}

class NaiveTransposer[T <: Data](val dim: Int, val dataType: T) extends Transposer[T] {
  val regArray = Seq.fill(dim, dim)(Reg(dataType))
  val regArrayT = regArray.transpose
  // state = 0 => filling regArray row-wise, state = 1 => draining regArray column-wise
  val state = RegInit(0.U(1.W))
  val countInc = io.inRow.fire || io.outCol.fire
  val (countValue, countWrap) = Counter(countInc, dim)

  io.inRow.ready := state === 0.U
  io.outCol.valid := state === 1.U

  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      when(countValue === i.U && io.inRow.fire) {
        regArray(i)(j) := io.inRow.bits(j)
      }
    }
  }

  for (i <- 0 until dim) {
    io.outCol.bits(i) := 0.U
    for (j <- 0 until dim) {
      when(countValue === j.U) {
        io.outCol.bits(i) := regArrayT(j)(i)
      }
    }
  }

  when (io.inRow.fire && countWrap) {
    state := 1.U
  }
  when (io.outCol.fire && countWrap) {
    state := 0.U
  }

  assert(!(state === 0.U) || !io.outCol.fire)
  assert(!(state === 1.U) || !io.inRow.fire)
}

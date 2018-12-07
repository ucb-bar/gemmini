//DO NOT TOUCH
package systolic
import chisel3._
import chisel3.util._

class GridIO (val width: Int, val meshRows: Int, val meshColumns: Int,
val gridRows: Int, val gridColumns: Int, val sramEntries: Int){


  val top_mem = Mem(sramEntries,UInt((width*meshColumns*gridColumns).W))
  val left_mem = Mem(sramEntries,UInt((width*meshRows*gridRows).W))
  //val dataOut_top = Wire(Vec(meshColumns*gridColumns, UInt(width.W)))
  //val dataOut_left= Wire(Vec(meshRows*gridRows, UInt(width.W)))

  //sram_address_counter := Mux(sram_address_counter === (sramEntries.U - 1.U), 0.U, sram_address_counter + 1.U) //
  val sram_address_counter = Counter(sramEntries)

  val dataOut_top = top_mem.read(sram_address_counter.value)
  val dataOut_left = left_mem.read(sram_address_counter.value)
  val myGrid = Module(new Grid(width, meshRows, meshColumns, gridRows, gridColumns))

  // myGrid.io.in_a_vec := dataOut_left
  // myGrid.io.in_b_vec := dataOut_top

  val top_delays = (0 to gridColumns).map(i => ShiftRegister(dataOut_top(i*width*meshColumns+width*meshColumns-1, i*width*meshColumns), i))
  val left_delays = (0 to gridRows).map(i => ShiftRegister(dataOut_left(i*width*meshRows+width*meshRows-1, i*width*meshRows), i))

  myGrid.io.in_a_vec := top_delays.reduce(Cat(_, _))
  myGrid.io.in_b_vec := left_delays.reduce(Cat(_, _))
}

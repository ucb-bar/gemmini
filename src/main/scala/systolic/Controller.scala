// See README.md for license details.

package systolic

import chisel3._
import chisel3.util._

/**
  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
  * @param width Data width of operands
  * @param pass_through If false, the PE pipelines in_a, in_b, in_propag, in_s for 1 cycle
  */
val address_width = 32 //bits

class Controller(val width: Int, val tileRows: Int, val tileColumns: Int,
                 val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {
  val io = IO(new Bundle {
    val in_cmd = Input(UInt(2.W))
    val in_address = Input(UInt(32.W))
    val in_size = Input(UInt(32.W))
    val in_num_of_rows = Input(UInt(32.W))
    val in_buffer = Input(UInt(512.W)) // Cache line
    val cpu_valid = Input(Bool())
    val sys_valid = Input(Bool())
    val busy =  Output(Bool())
  })

  val meshwithmemory = Module(new MeshWithMemory(width, tileRows, tileColumns, meshRows, meshColumns,sramEntries))

  val INIT1 = 0.U(2.W)
  val INIT2 = 1.U(2.W)
  val INIT3 = 2.U(2.W)
  val virtual_address_a = RegInit(0.U)
  val virtual_address_b = RegInit(0.U)
  val virtual_address_out = RegInit(0.U)
  val physical_address_a = RegInit(0.U)
  val physical_address_b = RegInit(0.U)
  val physical_address_out = RegInit(0.U)
  val jump_size_a = RegInit(0.U)
  val jump_size_b = RegInit(0.U)
  val jump_size_out = RegInit(0.U)
  val num_of_rows = RegInit(0.U)
  val a_read_in_buffer = RegInit(0.U) // cache line read
  val b_read_in_buffer = RegInit(0.U)// cache line read
  val a_read_buffer = RegInit(0.U)
  val b_read_buffer = RegInit(0.U)
  val c_write_out_buffer = RegInit(0.U)

  val BLOCK_SIZE_a = meshwithmemory.io.a.cloneType
  val BLOCK_SIZE_b = meshwithmemory.io.b.cloneType
  val BLOCK_SIZE_c = meshwithmemory.io.out_c.cloneType

  val idle :: initialize1 :: initialize2 :: initialize3 :: address_translation :: load_data :: compute :: Nil = Enum(7)
  val state = RegInit(idle)

  switch (state) {
    is (idle) {
      io.busy := false.B
      when (io.cpu_valid && io.in_cmd === INIT1) {
        virtual_address_a := io.in_address
        num_of_rows := io.in_num_of_rows
        jump_size_a := io.in_size
        state := initialize2

      }
      meshwithmemory.io.valid := false.B

    }
    is (initialize2) {
      when (io.cpu_valid && io.in_cmd === INIT2) {
        virtual_address_b := io.in_address
        jump_size_b := io.in_size
        state := initialize3
      }
      meshwithmemory.io.valid := false.B

    }
    is (initialize3) {
      when(io.cpu_valid && io.in_cmd === INIT3) {
        virtual_address_out := io.in_address
        jump_size_out := io.in_size
        state := address_translation
      }
      meshwithmemory.io.valid := false.B
      io.busy := true.B

    }
    is(address_translation) {
      physical_address_a := get_physical_address(virtual_address_a)
      physical_address_b := get_physical_address(virtual_address_b)
      physical_address_out := get_physical_address(virtual_address_out)
      state := load_data
      meshwithmemory.io.valid := false.B

    }
    is(load_data) {
      a_read_in_buffer := load_data(physical_address_a,BLOCK_SIZE_a)
      b_read_in_buffer := load_data(physical_address_b,BLOCK_SIZE_b)
      physical_address_a := physical_address_a + jump_size_a
      physical_address_b := physical_address_b + jump_size_b
      physical_address_out := physical_address_out + jump_size_out
      meshwithmemory.io.valid := false.B

    }
    is(compute) {
      num_of_rows := num_of_rows - 1
      when(num_of_rows > 0.U) {
        meshwithmemory.io.a := a_read_in_buffer
        meshwithmemory.io.b := b_read_in_buffer
        meshwithmemory.io.valid := true.B
        when(num_of_rows > 1.U) {
            a_read_in_buffer := load_data(physical_address_a, BLOCK_SIZE_a)
            b_read_in_buffer := load_data(physical_address_b, BLOCK_SIZE_b)
            physical_address_a := physical_address_a + jump_size_a
            physical_address_b := physical_address_b + jump_size_b
        }
        // c_write_out_buffer := meshwithmemory.io.out_c

      }.otherwise {
        meshwithmemory.io.valid := false.B
        state := idle
        io.busy := false.B
      }
      when(io.sys_valid) {
        store_data(c_write_out_buffer, physical_address_out, BLOCK_SIZE_c)
        physical_address_out := physical_address_out + jump_size_out

      }

    }

    }

}

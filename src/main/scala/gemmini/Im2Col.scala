package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

import Util._

class Im2ColReadReq[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val addr = new LocalAddr(config.sp_banks, config.sp_bank_entries, config.acc_banks, config.acc_bank_entries)
  val fire_counter = UInt(log2Up(config.meshColumns * config.tileColumns).W)
  val input_width = UInt(4.W)
  val weight_width = UInt(3.W)
  val channel = UInt(3.W)

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)

  override def cloneType: Im2ColReadResp.this.type = new Im2ColReadResp(config).asInstanceOf[this.type]

}

class Im2Col[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Module {
  import config._

  val block_size = meshRows*tileRows

  val io = new Bundle {
    val req = Flipped(Decoupled(new Im2ColReadReq(config))) // from ExecuteController
    val resp = Decoupled(new Im2ColReadResp(config)) // to ExecuteController

    /*
    val req = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())
      val bits = Input(new Im2ColReadReq(config))
    }

    val resp = new Bundle {
      val valid = Output(Bool())
      val ready = Input(Bool())
      val bits = Output(new Im2ColReadResp(config))
    }
    */

    val sram_reads = Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, sp_width)) // from Scratchpad
  }

  object State extends ChiselEnum {
    val idle, busy = Value
  }
  import State._

  val state = RegInit(idle)

  val req = Reg(new Im2ColReadReq(config))

//  io.req.ready := state === idle


  when (io.req.fire()) { //ready and valid
    req := io.req.bits
    //state := busy
  }

  when (io.req.ready) {
    io.sram_reads(req.addr.sp_bank()).req.valid := true.B
    io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()
  }

  val output_width = req.input_width - req.weight_width + 1.U
  //first do when input dimension is really small
  //val a_fire_counter = Reg(UInt(log2Up(block_size).W))
  val filter_dim2 = req.weight_width*req.weight_width
  val im2col_width = filter_dim2 * req.channel
  val im2col_height = output_width * output_width

  // first do simple case: channel 4, output width: 3x3
  val row_counter = RegInit(0.U(log2Up(block_size).W)) // im2col_width
  val column_counter = RegInit(0.U(log2Up(block_size).W))
  val window_row_counter = RegInit(0.U(log2Up(block_size).W))
  val window_start_counter = RegInit(0.U(log2Up(block_size).W))
  val window_counter = RegInit(0.U((window_row_counter.getWidth).W))
  val im2col_reg = RegInit(0.S.asTypeOf(Vec(block_size, inputType)))
  val valid_reg = RegInit(false.B)
  val busy_reg = RegInit(false.B)
  io.resp.bits.a_im2col := im2col_reg
  io.resp.valid := valid_reg
  io.req.ready := busy_reg
  io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row() + window_start_counter + window_row_counter
  val im2col_end = RegInit(false.B)


  when (!io.req.valid) {
    //window_jump_counter := 0.U
    busy_reg := false.B //??
    valid_reg := false.B
    //im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))
  }.otherwise {//firing && a_fire && cntl_ready
    when(im2col_width <= block_size.U){ //when whole im2col fits within 1 mesh
      row_counter := wrappingAdd(row_counter, 1.U, filter_dim2) //elements to multiply with weight window
      when((row_counter + 1.U) % req.weight_width === 0.U){
        when(row_counter === filter_dim2 - 1.U){ // finish 1 weight filter (1 im2col row); need to jump on next im2col row
          window_row_counter := 0.U // reset to new im2col row
          column_counter := wrappingAdd(column_counter, 1.U, im2col_height) // count y axis of im2col matrix
          when((column_counter + 1.U) % output_width === 0.U){ // end of one im2col row
            when((column_counter === output_width*output_width - 1.U) && (row_counter === filter_dim2 - 1.U)){ //im2col reached end
              im2col_end := true.B
            }.otherwise{
              window_start_counter := window_start_counter + req.input_width - output_width + 1.U
            }
          }.otherwise{
            window_start_counter := window_start_counter + 1.U
          }
        }.otherwise{ //still within 1 im2col row
          when((window_row_counter + 1.U) % req.weight_width === 0.U){
            window_row_counter := window_row_counter + req.input_width - req.weight_width + 1.U
            //should implement when window_row_counter reaches end?
          }.otherwise{
            window_row_counter := window_row_counter + 1.U
          }
        }
      }.otherwise{ //still within one weight row
        window_row_counter := window_row_counter + 1.U
      }

      when(im2col_end === false.B) {
        for (i <- 0 until 3) {
          when(i.U < req.channel) {
            when(i.U * filter_dim2 + row_counter < block_size.U) {
              im2col_reg(i.U * filter_dim2 + row_counter) := io.sram_reads(req.addr.sp_bank()).resp.bits.data //matching data
            } //.otherwise{im2col_reg(i) := DontCare}
          } //.otherwise{io.a_im2col(i) := DontCare}
        }
      }.otherwise{im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))}
    }.otherwise{
      //ToDo
    }

    when((column_counter === output_width*output_width - 1.U) && (row_counter === filter_dim2 - 1.U)){
      busy_reg := false.B //finished im2col
      valid_reg := true.B
    }.elsewhen(row_counter === filter_dim2 - 1.U){
      valid_reg := true.B //one row of im2col is finished
    }.otherwise{
      busy_reg := true.B
      valid_reg := false.B
    }

    //io.a_row_counter := wrappingAdd(io.a_row_counter, 1.U, block_size)
  }

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}

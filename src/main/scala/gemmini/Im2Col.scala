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
  val bank = UInt(3.W)

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)
  val im2col_end = Bool()
  val im2col_begin = Bool()

  override def cloneType: Im2ColReadResp.this.type = new Im2ColReadResp(config).asInstanceOf[this.type]

}

class Im2Col[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Module {
  import config._

  val block_size = meshRows*tileRows

  val io = IO(new Bundle {
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
  })

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

  io.sram_reads.foreach { sr =>
    sr.req.valid := false.B
    sr.req.bits := DontCare
    sr.resp.ready := true.B
  }

  val window_address = WireInit(0.U(log2Up(2*block_size).W))
  window_address := window_start_counter + window_row_counter
  val sram_read_req = RegInit(true.B)


  io.sram_reads(req.addr.sp_bank()).req.valid := sram_read_req
  val sram_read_valid = (req.bank === req.addr.sp_bank()) && io.sram_reads(req.addr.sp_bank()).req.valid
  io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row() + window_address
  dontTouch(window_start_counter)
  dontTouch(window_row_counter)
  dontTouch(window_address)
  val im2col_fin = RegInit(false.B) //when reading out necessary row is done
  val im2col_fin_reg = RegInit(false.B) // when 16 rows are all created - to outside
  val im2col_start = RegInit(false.B) //starting pulse of im2col
  io.resp.bits.im2col_end := im2col_fin_reg
  val sram_read_valid_d = RegInit(false.B)
  val sram_resp_valid = (req.bank === req.addr.sp_bank()) && io.sram_reads(req.addr.sp_bank()).resp.valid
  dontTouch(sram_resp_valid)
  im2col_start := sram_read_valid && !sram_read_valid_d
  io.resp.bits.im2col_begin := im2col_start
  when(column_counter === block_size.U - 1.U){
    im2col_fin_reg := true.B
  }.otherwise{ // when tiling, need to make it false again
    im2col_fin_reg := false.B
  }
  when(im2col_fin_reg){
    sram_read_req := false.B
  }.elsewhen(!io.req.valid){ //when tiling, need to make the request true again
    //sram_read_req := true.B
  }

  when (!io.req.valid) {
    //window_jump_counter := 0.U
    //busy_reg := false.B //??
    //valid_reg := false.B
    //im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))
  }.elsewhen(sram_resp_valid && sram_read_valid) { //firing && a_fire && cntl_ready
    sram_read_valid_d := sram_read_valid //delay to make start pulse
    //im2col_fin_reg := im2col_fin //delay once for synchronize with resp_valid
    when(im2col_width <= block_size.U) { //when whole im2col fits within 1 mesh
        row_counter := wrappingAdd(row_counter, 1.U, filter_dim2) //elements to multiply with weight window
      when(!im2col_fin) {
        when((row_counter + 1.U) % req.weight_width === 0.U) {
          when(row_counter === filter_dim2 - 1.U) { // finish 1 weight filter (1 im2col row); need to jump on next im2col row
            window_row_counter := 0.U // reset to new im2col row
            //column_counter := wrappingAdd(column_counter, 1.U, im2col_height) // count y axis of im2col matrix
            column_counter := column_counter + 1.U
            when((column_counter + 1.U) % output_width === 0.U) { // end of one im2col row
              when((column_counter === output_width * output_width - 1.U) && (row_counter === filter_dim2 - 1.U)) { //im2col reached end
                im2col_fin := true.B
                im2col_start := false.B
              }.otherwise {
                window_start_counter := window_start_counter + req.input_width - output_width + 1.U
              }
            }.otherwise {
              window_start_counter := window_start_counter + 1.U
            }
          }.otherwise { //still within 1 im2col row
            when((window_row_counter + 1.U) % req.weight_width === 0.U) {
              window_row_counter := window_row_counter + req.input_width - req.weight_width + 1.U
              //should implement when window_row_counter reaches end?
            }.otherwise {
              window_row_counter := window_row_counter + 1.U
            }
          }
        }.otherwise { //still within one weight row
          window_row_counter := window_row_counter + 1.U
        }
      }.otherwise{ //if im2col is finished first
        column_counter := wrappingAdd(column_counter, 1.U, block_size)
      }

        when(!im2col_fin) {
          for (i <- 0 until 4) {
            when(i.U < req.channel) {
              when(i.U * filter_dim2 + row_counter < block_size.U) {
                im2col_reg(i.U * filter_dim2 + row_counter) := io.sram_reads(req.addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType))(i) //matching data
              } //.otherwise{im2col_reg(i) := DontCare}
            } //.otherwise{io.a_im2col(i) := DontCare}
          }
        }.otherwise {
          im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))
        }
      }.otherwise {
        //ToDo
      }
  }

  when(io.req.valid){
    when(column_counter >= output_width*output_width){
      busy_reg := true.B
      valid_reg := true.B
    }.otherwise {
      when((column_counter === output_width * output_width - 1.U) && (row_counter === filter_dim2 - 1.U)) {
        //busy_reg := false.B //finished im2col
        valid_reg := true.B
      }.elsewhen(row_counter === filter_dim2 - 1.U) {
        valid_reg := true.B //one row of im2col is finished
      }.otherwise {
        busy_reg := true.B
        valid_reg := false.B
      }
      //io.a_row_counter := wrappingAdd(io.a_row_counter, 1.U, block_size)
    }
  }.otherwise{
    busy_reg := false.B
    valid_reg := false.B
  }

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}

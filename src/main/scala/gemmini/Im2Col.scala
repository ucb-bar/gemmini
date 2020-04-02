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
  val im2col_cmd = Bool()
  val start_inputting = Bool() //start_inputting_a
  val turn = UInt(9.W) //how many turns done out of im2col output block
  val ch_per_turn = UInt(3.W)
  val in_leftover = UInt(3.W)

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)
  val im2col_end = Bool()
  val im2col_begin = Bool()
  val continue_fire = Bool()
  val im2col_turn = UInt(9.W)

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
  /*
    object State extends ChiselEnum {
      val idle, busy = Value
    }
    import State._

    val state = RegInit(idle)
  */
  val req = Reg(new Im2ColReadReq(config))

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
  val start_inputting_A = req.start_inputting
  val weight = req.weight_width
  val input = req.input_width

  // first do simple case: channel 4, output width: 3x3
  val row_counter = RegInit(0.U(log2Up(2*block_size).W)) // im2col_width
  val column_counter = RegInit(0.U(log2Up(2*block_size).W))
  val window_row_counter = RegInit(0.U(log2Up(2*block_size).W))
  val window_start_counter = RegInit(0.U(log2Up(2*block_size).W))
  val window_counter = RegInit(0.U((window_row_counter.getWidth).W))
  val im2col_reg = RegInit(0.S.asTypeOf(Vec(block_size, inputType)))
  val valid_reg = RegInit(false.B)
  val busy_reg = RegInit(false.B)
  val im2col_start = RegInit(false.B) //starting pulse of im2col
  val im2col_fin_reg = RegInit(false.B) // when 16 rows are all created - to outside
  val im2col_fin = RegInit(false.B) //when reading out necessary row is done
  val tile_im2col_en = RegInit(false.B) //larger matrix (for future)
  val sram_resp_valid = WireInit(false.B)
  val im2col_en = WireInit(false.B)
  val sram_read_req = RegInit(true.B)
  val copy_addr = RegInit(io.req.bits.addr)

  val nothing_to_do :: waiting_for_im2col :: preparing_im2col :: doing_im2col :: im2col_done :: Nil = Enum(5)
  val im2col_state = RegInit(nothing_to_do)
  val busy = ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col) || (im2col_state === waiting_for_im2col))// or including prepare?
  val im2col_turn = RegInit(req.turn)
  val im2col_reg_d = RegNext(im2col_reg)

  io.resp.bits.a_im2col := im2col_reg //im2col_reg_d
  io.resp.valid := valid_reg
  io.resp.bits.im2col_begin := im2col_start //preparing for im2col
  io.resp.bits.im2col_end := im2col_fin
  io.resp.bits.continue_fire := tile_im2col_en
  io.resp.bits.im2col_turn := im2col_turn

  io.req.ready := busy

  val sram_read_signals_q = Module(new Queue(new im2colRowSignals, mem_pipeline+1,
    pipe=true))

  val sram_read_valid = start_inputting_A && io.sram_reads(req.addr.sp_bank()).req.valid

  when(io.req.bits.im2col_cmd){
    im2col_en := true.B
    req := io.req.bits
  }

  //im2col state machine
  switch(im2col_state){
    is(nothing_to_do){
      im2col_start := false.B
      sram_read_req := false.B
      when(im2col_en){
        im2col_state := waiting_for_im2col
      }
    }
    is(waiting_for_im2col){ //default state
      im2col_start := true.B
      //sram_read_req := true.B
      im2col_turn := io.req.bits.turn
      sram_read_req := false.B
      copy_addr := io.req.bits.addr
      when(req.start_inputting) { //receive im2col command from instruction
        im2col_state := preparing_im2col
        //im2col_turn := req.turn
        sram_read_req := true.B
        when(im2col_turn > 1.U){
          tile_im2col_en := true.B
        }.otherwise{tile_im2col_en := false.B}
      }
    }
    is(preparing_im2col){
      im2col_start := false.B
      sram_read_req := true.B
      when(sram_resp_valid){
        im2col_state := doing_im2col
        im2col_start := false.B //here?
      }
    }
    is(doing_im2col){
      im2col_start := false.B
      when(im2col_fin_reg|| !sram_resp_valid){
        sram_read_req := false.B
      }
      when(!sram_read_valid && !sram_resp_valid){
        when(im2col_turn === 1.U) {
          im2col_state := im2col_done
          tile_im2col_en := false.B
          im2col_turn := 0.U
        }.elsewhen(im2col_turn === 2.U){
          tile_im2col_en := false.B
          im2col_turn := 1.U
          sram_read_req := true.B
          im2col_state := preparing_im2col
          //im2col_fin := false.B
        }.otherwise{
          tile_im2col_en := true.B
          sram_read_req := true.B
          im2col_state := preparing_im2col
          im2col_turn := im2col_turn - 1.U
          //im2col_fin := false.B
        }
      }
    }
    is(im2col_done){
      im2col_start := false.B
      //      when(tile_im2col_en){
      //        im2col_state :=  preparing_im2col
      //      }
    }

  }

  io.sram_reads.foreach { sr =>
    sr.req.valid := false.B
    sr.req.bits := DontCare
    sr.resp.ready := true.B
  }

  val window_address = WireInit(0.U(log2Up(2*block_size).W))
  window_address := window_start_counter + window_row_counter


  //io.sram_reads(req.addr.sp_bank()).req.valid := sram_read_req && !im2col_fin_reg
  //io.sram_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row() + window_address
  io.sram_reads(copy_addr.sp_bank()).req.valid := sram_read_req && !im2col_fin_reg
  io.sram_reads(copy_addr.sp_bank()).req.bits.addr := copy_addr.sp_row() + window_address
  dontTouch(window_start_counter)
  dontTouch(window_row_counter)
  dontTouch(window_address)
  val start_inputting_A_deq = RegInit(false.B)
  when(sram_read_signals_q.io.deq.bits.start_inputting){
    start_inputting_A_deq := true.B
  }.otherwise{start_inputting_A_deq := false.B}

  //sram_resp_valid := (start_inputting_A || start_inputting_A_deq) && io.sram_reads(copy_addr.sp_bank()).resp.valid// io.sram_reads(req.addr.sp_bank()).resp.valid
  sram_resp_valid := ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col)) && io.sram_reads(copy_addr.sp_bank()).resp.valid
  dontTouch(sram_resp_valid)
  dontTouch(start_inputting_A_deq)


  when(column_counter === block_size.U - 1.U && row_counter === filter_dim2 - 1.U){
    im2col_fin_reg := true.B
  }.otherwise{ // when tiling, need to make it false again
    im2col_fin_reg := false.B
  }

  val row_counter_deq = sram_read_signals_q.io.deq.bits.row_counter
  val row_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.row_counter) //Seah: changed - 1 clock delayed version
  val column_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.column_counter)
  //val sram_req_output = io.sram_reads(req.addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType)) //Reg or Wire?
  val sram_req_output = io.sram_reads(copy_addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType))

  //caculating sram read address
  //  when (!io.req.valid) {
  when(!io.req.valid && !start_inputting_A && !sram_resp_valid){
    window_start_counter := 0.U
    window_row_counter := 0.U
    column_counter := 0.U
    row_counter := 0.U
    im2col_fin := false.B
  }.elsewhen(sram_resp_valid && sram_read_req) { //firing && a_fire && cntl_ready
    when(im2col_width <= block_size.U) { //when whole im2col fits within 1 mesh
      when(!im2col_fin) {
        row_counter := wrappingAdd(row_counter, 1.U, filter_dim2) //elements to multiply with weight window
        when((row_counter + 1.U) % weight === 0.U) {
          when(row_counter === filter_dim2 - 1.U) { // finish 1 weight filter (1 im2col row); need to jump on next im2col row
            window_row_counter := 0.U // reset to new im2col row
            //column_counter := wrappingAdd(column_counter, 1.U, im2col_height) // count y axis of im2col matrix
            column_counter := column_counter + 1.U
            when((column_counter + 1.U) % output_width === 0.U) { // end of one im2col row
              when((column_counter === output_width * output_width - 1.U) && (row_counter === filter_dim2 - 1.U)) { //im2col reached end
                im2col_fin := true.B
                row_counter := filter_dim2 - 1.U
                //im2col_start := false.B
              }.otherwise {
                window_start_counter := window_start_counter + input - output_width + 1.U
              }
            }.otherwise {
              window_start_counter := window_start_counter + 1.U
            }
          }.otherwise { //still within 1 im2col row
            when((window_row_counter + 1.U) % weight === 0.U) {
              window_row_counter := window_row_counter + input - weight + 1.U
              //should implement when window_row_counter reaches end?
            }.otherwise {
              window_row_counter := window_row_counter + 1.U
            }
          }
        }.otherwise { //still within one weight row
          window_row_counter := window_row_counter + 1.U
        }
      }.otherwise{ //if im2col is finished first
        //column_counter := wrappingAdd(column_counter, 1.U, block_size)
        window_row_counter := 0.U
        window_start_counter := 0.U
        //row counter stays at the highest value
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width){
          column_counter := column_counter + 1.U
        }
      }
    }.otherwise {
      //ToDo
    }
  }.elsewhen(!sram_read_valid && !sram_resp_valid){
    when(tile_im2col_en){im2col_fin := false.B}
    window_row_counter := 0.U
    window_start_counter := 0.U
    column_counter := 0.U
    row_counter := 0.U
  }

  val im2col_fin_pulse = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin_pulse)
  val sram_req_deq_valid_d = RegNext(sram_read_signals_q.io.deq.valid)
  //making output data valid signal
  //when(sram_resp_valid && sram_read_req){
  when(sram_req_deq_valid_d){
    when(im2col_fin_pulse){
      valid_reg := false.B
    }.elsewhen(column_counter_deq_d >= output_width*output_width && valid_reg){ //if finished earlier
      busy_reg := true.B
      valid_reg := true.B
    }.otherwise {
      when((column_counter_deq_d === output_width * output_width - 1.U) && (row_counter_deq_d === filter_dim2 - 1.U)) {
        //busy_reg := false.B //finished im2col
        valid_reg := true.B
      }.elsewhen(row_counter_deq_d === filter_dim2 - 1.U) {
        valid_reg := true.B //one row of im2col is finished
      }.otherwise {
        busy_reg := true.B
        valid_reg := false.B
      }
    }
  }.otherwise{
    busy_reg := false.B
    valid_reg := false.B
  }

  val starting_channel = (req.turn - sram_read_signals_q.io.deq.bits.im2col_turn) * req.ch_per_turn
  val sram_read_im2col_fin_d = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin)
  val sram_read_valid_d = RegNext(sram_read_signals_q.io.deq.valid)

  //generating output data
  when(!sram_read_im2col_fin_d&& sram_read_valid_d) {
    for (i <- 0 until 4) {
      when(i.U < req.ch_per_turn) { //assume 16 is divisible by weight^2
        when(i.U * filter_dim2 + row_counter_deq_d < block_size.U) {
          im2col_reg(i.U * filter_dim2 + row_counter_deq_d) := sram_req_output(i.U + starting_channel) //matching data
        }
      }
    }
  }.otherwise {
    im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))
  }

  class im2colRowSignals extends Bundle {
    val row_counter = UInt(log2Up(block_size).W)
    val column_counter = UInt(log2Up(block_size).W)
    val im2col_fin = Bool()
    val im2col_fin_pulse = Bool()
    val im2col_turn = UInt(9.W)
    val start_inputting = Bool()
    //val read_output = Vec(block_size, inputType)

  }
  sram_read_signals_q.io.enq.valid := sram_read_req
  sram_read_signals_q.io.enq.bits.row_counter := row_counter
  sram_read_signals_q.io.enq.bits.column_counter := column_counter
  sram_read_signals_q.io.enq.bits.im2col_fin := im2col_fin
  sram_read_signals_q.io.enq.bits.im2col_fin_pulse := im2col_fin_reg
  sram_read_signals_q.io.enq.bits.im2col_turn := im2col_turn
  sram_read_signals_q.io.enq.bits.start_inputting := start_inputting_A

  sram_read_signals_q.io.deq.ready := true.B//sram_resp_valid

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}
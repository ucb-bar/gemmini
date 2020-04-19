/*
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
  val channel = UInt(5.W)
  val im2col_cmd = Bool()
  val start_inputting = Bool() //start_inputting_a
  val turn = UInt(9.W) //how many turns done out of im2col output block
  val ch_per_turn = UInt(3.W)
  val row_turn = UInt(7.W) // step 4: for overflowing rows

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)
  val im2col_end = Bool()
  val im2col_end_reg = Bool() //added
  val im2col_begin = Bool()
  val continue_fire = Bool()
  val im2col_turn = UInt(9.W)
  val row_turn = UInt(7.W)

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
  val row_counter = RegInit(0.U(log2Up(block_size*block_size).W)) // im2col_width
  val column_counter = RegInit(0.U(log2Up(block_size*block_size).W))
  val window_row_counter = RegInit(0.U((column_counter.getWidth).W))
  val window_start_counter = RegInit(0.U((column_counter.getWidth).W))
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

  val im2col_turn = RegInit(req.turn)
  val turn_done = req.turn - im2col_turn
  val im2col_reg_d = RegNext(im2col_reg)
  val im2col_fin_reset = RegInit(false.B)

  val im2col_row_turn_counter = RegInit(0.U((io.req.bits.row_turn.getWidth).W))
  //val im2col_row_counter_update = RegInit(false.B)
  val im2col_row_turn_counter_d = RegInit(im2col_row_turn_counter)
  when(im2col_state === waiting_for_im2col){
    im2col_row_turn_counter_d := im2col_row_turn_counter
  }

  val block_done = io.req.bits.row_turn - im2col_row_turn_counter

  //for state transition & im2col_turn counter
  val im2col_fin_d = RegNext(im2col_fin)
  val im2col_state_pulse = im2col_fin_d && !im2col_fin

  io.resp.bits.a_im2col := im2col_reg //im2col_reg_d
  io.resp.valid := valid_reg
  io.resp.bits.im2col_begin := im2col_start //preparing for im2col
  io.resp.bits.im2col_end := im2col_fin
  io.resp.bits.im2col_end_reg := im2col_fin_reg
  io.resp.bits.continue_fire := tile_im2col_en
  io.resp.bits.im2col_turn := im2col_turn
  io.resp.bits.row_turn := block_done

  when(io.req.bits.im2col_cmd){
    im2col_en := true.B
    req := io.req.bits
  }
  val im2col_en_d = RegNext(im2col_en)

  val sram_read_signals_q = Module(new Queue(new im2colRowSignals, mem_pipeline+1,
    pipe=true))

  val sram_read_valid = start_inputting_A && io.sram_reads(req.addr.sp_bank()).req.valid
  val busy = ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col) || (im2col_state === waiting_for_im2col) || (im2col_en && !im2col_en_d))//&& sram_read_valid))// or including prepare?

  io.req.ready := busy


  val row_turn_counter_noreset = RegInit(false.B)
  val im2col_fin_pulse = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin_pulse)

  //im2col state machine
  switch(im2col_state){
    is(nothing_to_do){
      im2col_fin_reset := false.B
      im2col_start := false.B
      sram_read_req := false.B
      when(im2col_en){
        im2col_state := waiting_for_im2col
      }
    }
    is(waiting_for_im2col){ //default state
      //im2col_row_counter_update := true.B
      im2col_start := true.B
      im2col_turn := io.req.bits.turn
      im2col_fin_reset := true.B//added for im2col_fin reset pulse
      //added for row overflowing
      when(!row_turn_counter_noreset){
        im2col_row_turn_counter := io.req.bits.row_turn
      }
      sram_read_req := false.B
      copy_addr := io.req.bits.addr
      when(req.start_inputting) { //receive im2col command from instruction
        sram_read_req := true.B
        im2col_fin_reset := false.B
        when(im2col_turn > 1.U){
          tile_im2col_en := true.B
        }.otherwise{tile_im2col_en := false.B}
      }
      when(sram_read_valid){
        im2col_state := preparing_im2col // where state transition?
      }
    }
    is(preparing_im2col){
      im2col_fin_reset := false.B//added for im2col_fin reset pulse
      im2col_start := false.B
      sram_read_req := true.B
      when(sram_resp_valid && !im2col_fin_reset){ //added im2col_fin_reset for sync at perform_mul_pre
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
          when(im2col_row_turn_counter === 0.U){
            //im2col_state := im2col_done
            tile_im2col_en := false.B
          }.otherwise{
            im2col_row_turn_counter := im2col_row_turn_counter - 1.U // row is outer loop (finish preloaded one first)
            //tile_im2col_en := true.B
          }
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
      im2col_fin_reset := true.B //added for im2col_fin reset pulse
      im2col_start := false.B
      //when(im2col_row_turn_counter_d =/= 0.U){
      when(im2col_row_turn_counter_d =/= 0.U && !im2col_fin) { //added !im2col_fin to sync transition
        im2col_state := waiting_for_im2col
        //im2col_row_counter_update := true.B //added for im2col_row_counter sync
      }
      row_turn_counter_noreset := true.B
    }

  }

  io.sram_reads.foreach { sr =>
    sr.req.valid := false.B
    sr.req.bits := DontCare
    sr.resp.ready := true.B
  }


  //saving bigger weight dimension than 4x4
  val row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) //saves place in the weight

  //saving for not 2x2
  val window_row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) //saves place within row

  val window_address = WireInit(0.U(log2Up(block_size*block_size).W))
  window_address := window_start_counter + window_row_counter + window_row_counter_saver


  io.sram_reads(copy_addr.sp_bank()).req.valid := sram_read_req && !im2col_fin_reg
  io.sram_reads(copy_addr.sp_bank()).req.bits.addr := copy_addr.sp_row() + window_address
  dontTouch(window_start_counter)
  dontTouch(window_row_counter)
  dontTouch(window_address)
  val start_inputting_A_deq = RegInit(false.B)
  when(sram_read_signals_q.io.deq.bits.start_inputting){
    start_inputting_A_deq := true.B
  }.otherwise{start_inputting_A_deq := false.B}

  sram_resp_valid := ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col)) && io.sram_reads(copy_addr.sp_bank()).resp.valid && !im2col_fin_reset
  dontTouch(sram_resp_valid)
  dontTouch(start_inputting_A_deq)

  val channel_wrap = RegInit(0.U((column_counter.getWidth).W)) // for overflowing detection
  val row_residue = (im2col_width - turn_done * block_size.U) // row left on the last horizontal block (im2col_turn = 1)

  //when(column_counter === block_size.U - 1.U && row_counter === filter_dim2 - 1.U){
  when(column_counter === block_size.U - 1.U && ((row_counter === filter_dim2 - 1.U) || (row_counter === block_size.U - 1.U) || (row_counter === row_residue - 1.U))){ //changed for row-wise multiple im2col blocks
    im2col_fin_reg := true.B
  }.elsewhen(!start_inputting_A || im2col_fin_reset){
   im2col_fin_reg := false.B
   window_row_counter_saver := 0.U
   //channel_wrap := 0.U //need to initialize wrapping when horizontal block ends
  }.otherwise{ // when tiling, need to make it false again
    im2col_fin_reg := false.B
    window_row_counter_saver := 0.U //where to initialize saver?
  }

  val row_counter_deq = sram_read_signals_q.io.deq.bits.row_counter
  val row_counter_deq_d = RegInit(sram_read_signals_q.io.deq.bits.row_counter) //Seah: changed - 1 clock delayed version
  val column_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.column_counter)
  val sram_req_output = io.sram_reads(copy_addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType))
  val window_start_counter_prev = RegInit(0.U((window_start_counter.getWidth).W)) //to store in which window_start_counter on this phase, and loaded it when the next vertical block starts
  val window_row_left = RegInit(0.U((window_start_counter.getWidth).W)) // to figure out starting point

  when(column_counter === block_size.U - 1.U && ((row_counter_deq_d === filter_dim2 - 1.U) || (row_counter_deq_d === block_size.U - 1.U) || (row_counter_deq_d === row_residue - 1.U)) && valid_reg){
    when((req.turn - im2col_turn + 1.U)*(block_size.U - filter_dim2*req.ch_per_turn) - filter_dim2 * channel_wrap >= filter_dim2) { // have to test
      channel_wrap := channel_wrap + 1.U
    }
  }.elsewhen(!start_inputting_A || im2col_fin_reset){
    channel_wrap := 0.U
  }

  when(im2col_state =/= im2col_done) {
    when(req.ch_per_turn === 0.U) { //when filter_dim > block_size
      when(row_counter === block_size.U - 1.U && im2col_turn =/= 1.U && (column_counter + block_done * block_size.U === output_width * output_width - 1.U || column_counter === block_size.U - 1.U)) {
        when((row_counter + 1.U + row_counter_saver) % weight === 0.U) { //when finishing 1 weight row
          window_row_left := window_row_counter + input - weight + 1.U //copy window_row_counter below
        }.otherwise {
          window_row_left := window_row_counter + 1.U
        }
      }
      when(im2col_fin_reg) { //only have to update row_counter_saver once
        when(row_counter_saver >= filter_dim2 - block_size.U) {
          row_counter_saver := row_counter_saver + block_size.U - filter_dim2
        }.otherwise {
          row_counter_saver := row_counter_saver + block_size.U
        }
      }
    }.elsewhen(row_counter === filter_dim2 - (im2col_width - (req.turn - 1.U) * block_size.U) && im2col_turn =/= 1.U) {
      window_row_left := window_row_counter
    }
  }.otherwise{
    row_counter_saver := 0.U // reset
    window_row_left := 0.U //reset
  }

  //caculating sram read address
  when((!io.req.valid && !start_inputting_A && !sram_resp_valid) || im2col_fin_reset){
    window_start_counter := 0.U
    window_row_counter := 0.U
    column_counter := 0.U
    row_counter := 0.U
    im2col_fin := false.B
  }.elsewhen(!sram_resp_valid&&sram_read_req){ // transition -> loading saved values
    window_start_counter := window_start_counter_prev //load
    when(req.ch_per_turn === 0.U){
      window_row_counter := window_row_left
    }.elsewhen(im2col_turn === 1.U && (row_residue < filter_dim2)){
      window_row_counter := window_row_left // for unnecessary reading out for last horizontal block
    }
  }.elsewhen(sram_resp_valid && sram_read_req) { //firing && a_fire && cntl_ready
    when(im2col_width <= block_size.U) { //when whole im2col fits within 1 mesh
      when(!im2col_fin) {
        row_counter := wrappingAdd(row_counter, 1.U, filter_dim2) //elements to multiply with weight window
        when((row_counter + 1.U) % weight === 0.U) {
          when(row_counter === filter_dim2 - 1.U) { // finish 1 weight filter (1 im2col row); need to jump on next im2col row
            window_row_counter := 0.U // reset to new im2col row
            column_counter := column_counter + 1.U
            when(im2col_row_turn_counter === 0.U) { // on the last block of row
              when((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) { // end of one im2col row
                //when((column_counter === output_width * output_width - 1.U) && (row_counter === filter_dim2 - 1.U)) { //im2col reached end (filter_dim2 -> assume 2x2 weight matrix => have to fix later)
                when((column_counter + block_done * block_size.U === output_width * output_width - 1.U)) { // on the last block -> finish
                  im2col_fin := true.B
                  row_counter := filter_dim2 - 1.U
                  //im2col_start := false.B
                }.otherwise {
                  window_start_counter := window_start_counter + input - output_width + 1.U
                }
              }.otherwise {
                window_start_counter := window_start_counter + 1.U
              }
            }.otherwise{ //not the last block
              when((column_counter === block_size.U - 1.U) ){//&& (row_counter === filter_dim2 - 1.U)){
                im2col_fin := true.B
                row_counter := filter_dim2 - 1.U
              }.otherwise{
                when((column_counter + 1.U + block_done * 16.U) % output_width === 0.U){
                  window_start_counter := window_start_counter + input - output_width + 1.U
                }.otherwise{
                  window_start_counter := window_start_counter + 1.U
                }
              }
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
        when(im2col_turn === 1.U) {
          window_start_counter_prev := window_start_counter + 1.U //store (plus 1??)
          when((column_counter + block_done * block_size.U) % output_width === 0.U){
            window_start_counter_prev := window_start_counter + input - output_width + 1.U
          }
        }
        //row counter stays at the highest value
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width - block_done*block_size.U){
          column_counter := column_counter + 1.U
        }
      }
    }.elsewhen(req.ch_per_turn === 0.U) { //when it does not fit into 1 mesh (row-wise)
      //ToDo
      //row_counter_saver implement
      when(!im2col_fin) {
        row_counter := wrappingAdd(row_counter, 1.U, block_size) //elements to multiply with weight window (does row_counter reset? -> check)
        window_row_counter := window_row_counter + 1.U

        when(im2col_turn =/= 1.U || (row_residue >= filter_dim2 && row_residue >= block_size.U)) { // not on the last horizontal block
          //when(im2col_row_turn_counter === 0.U) { // on the last vertical block
          when((row_counter + 1.U + row_counter_saver) % weight === 0.U) { //when finishing 1 weight row
            window_row_counter := window_row_counter + input - weight + 1.U
            when(window_row_counter === input * (weight - 1.U) + weight - 1.U){
              window_row_counter := 0.U
            } //when it hits the last counter value
          }
          when(row_counter === block_size.U - 1.U){
            column_counter := column_counter + 1.U
            window_row_counter := window_row_left
            when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) || column_counter === block_size.U - 1.U){
              im2col_fin := true.B
              row_counter := filter_dim2 - 1.U
            }.elsewhen((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U){
              window_start_counter := window_start_counter + input - output_width + 1.U
            }.otherwise{
              window_start_counter := window_start_counter + 1.U
            }
          }

        }.otherwise{ //on the last horizontal block (can merge with previous case)
          row_counter := wrappingAdd(row_counter, 1.U, row_residue) //count up to remainder
          //when(im2col_row_turn_counter === 0.U) { // on the last vertical block
            when((row_counter + 1.U + row_counter_saver) % weight === 0.U) { //when finishing 1 weight row
              window_row_counter := window_row_counter + input - weight + 1.U
            }
            when(row_counter === row_residue - 1.U) { // count up to last row -> need to start new column
              column_counter := column_counter + 1.U
              window_row_counter := window_row_left
              when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) || column_counter === block_size.U - 1.U) { // condition for finishing im2col
                im2col_fin := true.B
                row_counter := filter_dim2 - 1.U
              }.elsewhen((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) { //finishing one output row
                window_start_counter := window_start_counter + input - output_width + 1.U
              }.otherwise {
                window_start_counter := window_start_counter + 1.U
              } //default
            }
          //}

        }
      }.otherwise { //when im2col_fin
        window_start_counter := 0.U
        when(im2col_turn === 1.U) {
          window_start_counter_prev := window_start_counter + 1.U //store (plus 1??)
          when((column_counter + block_done * block_size.U) % output_width === 0.U){
            window_start_counter_prev := window_start_counter + input - output_width + 1.U
          }
        }
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width - block_done*block_size.U){
          column_counter := column_counter + 1.U
        }
      }
    }.otherwise { //when it does not fit into 1 mesh (row-wise) but still filter_dim2<16 (row savers are all 0)
      //ToDo
      when(!im2col_fin) {
        when(im2col_turn =/= 1.U || (row_residue >= filter_dim2)) { // not on the last horizontal block
          row_counter := wrappingAdd(row_counter, 1.U, filter_dim2) //elements to multiply with weight window (does row_counter reset? -> check)
          when((row_counter + 1.U) % weight === 0.U) {
            when(row_counter === filter_dim2 - 1.U) { // finish 1 weight filter (1 im2col row); need to jump on next im2col row
              window_row_counter := 0.U // reset to new im2col row
              column_counter := column_counter + 1.U
              when(im2col_row_turn_counter === 0.U) { // on the last vertical block
                when((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) { // end of one im2col row
                  when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) && (row_counter === filter_dim2 - 1.U)) { // on the last block -> finish
                    im2col_fin := true.B
                    row_counter := filter_dim2 - 1.U
                    //im2col_start := false.B
                  }.otherwise {
                    window_start_counter := window_start_counter + input - output_width + 1.U
                  }
                }.otherwise {
                  window_start_counter := window_start_counter + 1.U
                }
              }.otherwise { //not the last block
                when((column_counter === block_size.U - 1.U)){// && (row_counter === filter_dim2 - 1.U)) {
                  im2col_fin := true.B
                  row_counter := filter_dim2 - 1.U
                }.otherwise {
                  when((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) {
                    window_start_counter := window_start_counter + input - output_width + 1.U
                  }.otherwise {
                    window_start_counter := window_start_counter + 1.U
                  }
                }
              }

            }.otherwise { //still within 1 im2col row
              when((window_row_counter + 1.U + window_row_counter_saver) % weight === 0.U) {
                window_row_counter := window_row_counter + input - weight + 1.U
                //should implement when window_row_counter reaches end?
              }.otherwise {
                window_row_counter := window_row_counter + 1.U
              }
            }
          }.otherwise { //still within one weight row
            window_row_counter := window_row_counter + 1.U
          }
        }.otherwise{ //on the last horizontal block, don't have to read out all rows
          row_counter := wrappingAdd(row_counter, 1.U, row_residue) //does it overwrite?
          when((row_counter + 1.U + filter_dim2 - row_residue) % weight === 0.U) {
            when(row_counter === row_residue - 1.U) { // finish 1 weight filter (1 im2col row); need to jump on next im2col row
              window_row_counter := window_row_left // reset to new im2col row
              column_counter := column_counter + 1.U
              when(im2col_row_turn_counter === 0.U) { // on the last vertical block
                when((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) { // end of one im2col row
                  when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) && (row_counter === row_residue - 1.U)) { // on the last block -> finish
                    im2col_fin := true.B
                    row_counter := filter_dim2 - 1.U
                    //im2col_start := false.B
                  }.otherwise {
                    window_start_counter := window_start_counter + input - output_width + 1.U
                  }
                }.otherwise {
                  window_start_counter := window_start_counter + 1.U
                }
              }.otherwise { //not the last block
                when((column_counter === block_size.U - 1.U)) {// && (row_counter === row_residue - 1.U)) {
                  im2col_fin := true.B
                  row_counter := filter_dim2 - 1.U
                }.otherwise {
                  when((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U) {
                    window_start_counter := window_start_counter + input - output_width + 1.U
                  }.otherwise {
                    window_start_counter := window_start_counter + 1.U
                  }
                }
              }

            }.otherwise { //still within 1 im2col row
              when((window_row_counter + 1.U + window_row_counter_saver) % weight === 0.U) {
                window_row_counter := window_row_counter + input - weight + 1.U
                //should implement when window_row_counter reaches end?
              }.otherwise {
                window_row_counter := window_row_counter + 1.U
              }
            }
          }.otherwise { //still within one weight row
            window_row_counter := window_row_counter + 1.U
          }
        }
      }.otherwise{ //if im2col is finished first
        //column_counter := wrappingAdd(column_counter, 1.U, block_size)
        window_row_counter := 0.U
        window_start_counter := 0.U
        when(im2col_turn === 1.U) {
          window_start_counter_prev := window_start_counter + 1.U //store (plus 1??)
          when((column_counter + block_done * block_size.U) % output_width === 0.U){
            window_start_counter_prev := window_start_counter + input - output_width + 1.U
          }
        }
        //row counter stays at the highest value
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width - block_done*block_size.U){
          column_counter := column_counter + 1.U
        }
      }

    }
  }.elsewhen(!sram_read_valid && !sram_resp_valid){
    when(tile_im2col_en){im2col_fin := false.B}
    window_row_counter := 0.U
    window_start_counter := 0.U
    column_counter := 0.U
    row_counter := 0.U
  }


  val sram_req_deq_valid_d = RegNext(sram_read_signals_q.io.deq.valid)
  val row_counter_valid = RegInit(sram_read_signals_q.io.deq.bits.row_counter)
  when(sram_read_signals_q.io.deq.bits.im2col_turn === 1.U){
    row_counter_valid := sram_read_signals_q.io.deq.bits.row_counter
    when(sram_read_signals_q.io.deq.bits.row_residue < filter_dim2) {
      row_counter_valid := sram_read_signals_q.io.deq.bits.row_counter + filter_dim2 * req.ch_per_turn - sram_read_signals_q.io.deq.bits.row_residue
    }
    when(req.ch_per_turn === 0.U){ //when filter_dim > block_size
      //row_counter_valid := sram_read_signals_q.io.deq.bits.row_counter + (block_size.U - sram_read_signals_q.io.deq.bits.row_residue)
      row_counter_valid := sram_read_signals_q.io.deq.bits.row_counter_save + sram_read_signals_q.io.deq.bits.row_counter
    }
  }.otherwise{
    row_counter_valid := sram_read_signals_q.io.deq.bits.row_counter
  }

  when(sram_read_signals_q.io.deq.bits.im2col_turn === 1.U && sram_read_signals_q.io.deq.bits.row_residue < filter_dim2){
    row_counter_deq_d := filter_dim2 - (sram_read_signals_q.io.deq.bits.row_residue - sram_read_signals_q.io.deq.bits.row_counter - 1.U) - 1.U
    when(req.ch_per_turn === 0.U){
      row_counter_deq_d := sram_read_signals_q.io.deq.bits.row_counter + sram_read_signals_q.io.deq.bits.row_counter_save
    }
  }.otherwise{
    row_counter_deq_d := sram_read_signals_q.io.deq.bits.row_counter
    when(req.ch_per_turn === 0.U){ //when filter_dim > block_size -> need to check
      row_counter_deq_d := sram_read_signals_q.io.deq.bits.row_counter + sram_read_signals_q.io.deq.bits.row_counter_save
      when(sram_read_signals_q.io.deq.bits.row_counter + sram_read_signals_q.io.deq.bits.row_counter_save >= filter_dim2){
        row_counter_deq_d := sram_read_signals_q.io.deq.bits.row_counter + sram_read_signals_q.io.deq.bits.row_counter_save - filter_dim2
      }
    }
  }

  //making output data valid signal
  //when(sram_resp_valid && sram_read_req){
  when(sram_req_deq_valid_d){
    when(im2col_fin_pulse){
      valid_reg := false.B
    }.elsewhen(column_counter_deq_d >= (output_width*output_width - sram_read_signals_q.io.deq.bits.block_done * block_size.U) && valid_reg){ //if finished earlier
      busy_reg := true.B
      valid_reg := true.B
    }.otherwise {
      when((column_counter_deq_d === (output_width * output_width - sram_read_signals_q.io.deq.bits.block_done * block_size.U) - 1.U) && (row_counter_valid === filter_dim2 - 1.U)) {
        valid_reg := true.B
      }.elsewhen(sram_read_signals_q.io.deq.bits.im2col_turn === 1.U){
        when(req.ch_per_turn === 0.U && row_counter_valid === filter_dim2 - 1.U){
          valid_reg := true.B
        }.elsewhen(req.ch_per_turn =/= 0.U && (row_counter_valid === block_size.U - 1.U || row_counter_valid === filter_dim2 - 1.U)){
          valid_reg := true.B
        }.otherwise{valid_reg := false.B}
      }.elsewhen(row_counter_valid === filter_dim2 - 1.U || row_counter_valid === block_size.U - 1.U) {
        valid_reg := true.B
      }.otherwise {
        busy_reg := true.B
        valid_reg := false.B
      }
    }
  }.otherwise{
    busy_reg := false.B
    valid_reg := false.B
  }

  val assign_turn_done = (req.turn - sram_read_signals_q.io.deq.bits.im2col_turn)
  val starting_channel = (req.turn - sram_read_signals_q.io.deq.bits.im2col_turn) * req.ch_per_turn + sram_read_signals_q.io.deq.bits.channel_wrap //added wrapping
  val sram_read_im2col_fin_d = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin)
  val sram_read_valid_d = RegNext(sram_read_signals_q.io.deq.valid)

  //generating output data
  when(!sram_read_im2col_fin_d&& sram_read_valid_d) {
    for (i <- 0 until 4) {
      //when(i.U < req.ch_per_turn) { //assume 16 is divisible by weight^2
      when((i.U + starting_channel) < req.channel){ //added 1.U
        when(((i.U + starting_channel)* filter_dim2 + row_counter_deq_d - block_size.U * assign_turn_done) < block_size.U) {
          im2col_reg((i.U + starting_channel)* filter_dim2 + row_counter_deq_d - block_size.U * assign_turn_done) := sram_req_output(i.U + starting_channel) //matching data
        }
      }
    }
  }.otherwise {
    im2col_reg := 0.S.asTypeOf(Vec(block_size, inputType))
  }

  class im2colRowSignals extends Bundle {
    val row_counter = UInt(log2Up(block_size*block_size).W)
    val column_counter = UInt(log2Up(block_size*block_size).W)
    val im2col_fin = Bool()
    val im2col_fin_pulse = Bool()
    val im2col_turn = UInt(9.W)
    val block_done = UInt(9.W)
    val start_inputting = Bool()
    val channel_wrap = UInt(5.W)
    val row_residue = UInt(log2Up(block_size*block_size).W)
    val row_counter_save = UInt(log2Up(block_size*block_size).W)
    //val read_output = Vec(block_size, inputType)

  }

  val row_address = WireInit(row_counter)
  when((row_counter + row_counter_saver) > filter_dim2 - 1.U){
    row_address := row_counter - filter_dim2
  }.otherwise{row_address := row_counter} //saver for 0 now

  sram_read_signals_q.io.enq.valid := sram_read_req
  sram_read_signals_q.io.enq.bits.row_counter := row_counter//row_address
  sram_read_signals_q.io.enq.bits.column_counter := column_counter
  sram_read_signals_q.io.enq.bits.im2col_fin := im2col_fin
  sram_read_signals_q.io.enq.bits.im2col_fin_pulse := im2col_fin_reg
  sram_read_signals_q.io.enq.bits.im2col_turn := im2col_turn
  sram_read_signals_q.io.enq.bits.block_done := block_done
  sram_read_signals_q.io.enq.bits.start_inputting := start_inputting_A
  sram_read_signals_q.io.enq.bits.channel_wrap := channel_wrap
  sram_read_signals_q.io.enq.bits.row_residue := row_residue
  sram_read_signals_q.io.enq.bits.row_counter_save := row_counter_saver

  sram_read_signals_q.io.deq.ready := true.B//sram_resp_valid

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}
 */
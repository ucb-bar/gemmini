package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._

import Util._

class Im2ColReadReq[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val addr = new LocalAddr(config.sp_banks, config.sp_bank_entries, config.acc_banks, config.acc_bank_entries)
  val fire_counter = UInt(log2Up(config.meshColumns * config.tileColumns).W)
  val input_width = UInt(4.W)
  val weight_width = UInt(4.W)
  val channel = UInt(10.W)
  val im2col_cmd = Bool()
  val start_inputting = Bool() //start_inputting_a
  val turn = UInt(9.W) //how many turns done out of im2col output block
  val ch_per_turn = UInt(5.W)
  val row_turn = UInt(7.W) // step 4: for overflowing rows

  override def cloneType: Im2ColReadReq.this.type = new Im2ColReadReq(config).asInstanceOf[this.type]

}

class Im2ColReadResp[T <: Data: Arithmetic](config: GemminiArrayConfig[T]) extends Bundle {

  val a_im2col = Vec(config.meshColumns * config.tileColumns, config.inputType)
  val im2col_end = Bool()
  val im2col_end_reg = Bool() //added
  val im2col_begin = Bool()
  //val continue_fire = Bool()
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

  //when (io.req.ready && io.req.valid) {
  when(io.req.ready){
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
  //io.resp.bits.continue_fire := tile_im2col_en
  io.resp.bits.im2col_turn := im2col_turn
  io.resp.bits.row_turn := block_done

  when(io.req.bits.im2col_cmd){
    im2col_en := true.B
    req := io.req.bits
  }
  val im2col_en_d = RegNext(im2col_en)

  val sram_read_signals_q = Module(new Queue(new im2colRowSignals, mem_pipeline+1,
    pipe=true))

  val sram_read_valid = io.sram_reads(req.addr.sp_bank()).req.valid && start_inputting_A
  val busy = ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col) || (im2col_state === waiting_for_im2col) || (im2col_en && !im2col_en_d))  //&& sram_read_valid))// or including prepare?

  io.req.ready := busy


  val row_turn_counter_noreset = RegInit(false.B)
  val im2col_fin_pulse = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin_pulse)
  val starting_channel = RegInit(0.U(10.W))
  val turn_count_save = RegInit(im2col_turn)
  val channel_wrap = RegInit(0.U((column_counter.getWidth).W)) // for overflowing detection
  val channel_done = req.ch_per_turn * turn_done + channel_wrap // how many channels were done
  val assign_data_sub = RegInit(channel_done) // to subtract for im2col_reg data assignment

  //im2col state machine
  switch(im2col_state){
    is(nothing_to_do){
      im2col_fin_reset := false.B
      im2col_start := false.B
      sram_read_req := false.B
      when(im2col_en){
        im2col_state := waiting_for_im2col
        im2col_turn := io.req.bits.turn
        window_start_counter := 0.U
        //window_row_counter := 0.U
        column_counter := 0.U
        //row_counter := 0.U
      }
    }
    is(waiting_for_im2col){ //default state
      //im2col_row_counter_update := true.B
      starting_channel := 0.U
      turn_count_save := io.req.bits.turn
      im2col_start := true.B
      when(im2col_turn === 0.U){im2col_turn := io.req.bits.turn}
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
      assign_data_sub := Mux(req.channel < 16.U, turn_done * block_size.U - channel_done * req.channel, 0.U)
      im2col_fin_reset := false.B//added for im2col_fin reset pulse
      im2col_start := false.B
      sram_read_req := true.B
      when(sram_resp_valid && !im2col_fin_reset && req.start_inputting){ //added start_inputting (have to check - whether to go im2col done)
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
        //assign_data_sub := Mux(req.channel < 16.U, turn_done * block_size.U - channel_done * req.channel, 0.U)
        when(turn_count_save - im2col_turn + 1.U === weight*weight){ //for after finishing 16 channels
          starting_channel := starting_channel + 1.U
          turn_count_save := im2col_turn - 1.U
        }
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
      starting_channel := 0.U
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


  val row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) // count how much channel counted using row_counter when channel < 16
  val window_row_counter_saver = RegInit(0.U((column_counter.getWidth).W)) //save window_row_counter starting point for channel < 16

  val window_address = WireInit(0.U(log2Up(block_size*block_size).W))
  window_address := window_start_counter + window_row_counter + starting_channel * req.input_width * req.input_width//((req.turn - im2col_turn) >> (log2Up(block_size)).U).asUInt * output_width * output_width// asUInt due to Intellij error
  when(req.channel < 16.U){
    window_address := window_start_counter + window_row_counter
  }

  io.sram_reads(copy_addr.sp_bank()).req.valid := sram_read_req && !im2col_fin_reg
  io.sram_reads(copy_addr.sp_bank()).req.bits.addr := copy_addr.sp_row() + window_address
  dontTouch(window_start_counter)
  //dontTouch(window_row_counter)
  dontTouch(window_address)
  val start_inputting_A_deq = RegInit(false.B)
  when(sram_read_signals_q.io.deq.bits.start_inputting){
    start_inputting_A_deq := true.B
  }.otherwise{start_inputting_A_deq := false.B}

  sram_resp_valid := ((im2col_state === doing_im2col) || (im2col_state === preparing_im2col)) && io.sram_reads(copy_addr.sp_bank()).resp.valid && !im2col_fin_reset
  dontTouch(sram_resp_valid)
  dontTouch(start_inputting_A_deq)

  //val row_residue = (im2col_width - turn_done * block_size.U) // row left on the last horizontal block (im2col_turn = 1)

  //when(column_counter === block_size.U - 1.U && row_counter === req.ch_per_turn + channel_wrap){
  when(im2col_fin_reg && im2col_state =/= im2col_done) {
    row_counter_saver := row_counter_saver + req.ch_per_turn
    //channel_wrap := 0.U
    when((req.turn - im2col_turn + 1.U) * (block_size.U - req.channel * req.ch_per_turn) - req.channel * channel_wrap >= req.channel) { // have to test
      channel_wrap := channel_wrap + 1.U
      //channel_wrap := 1.U
      row_counter_saver := row_counter_saver + req.ch_per_turn + 1.U
    }
    //}.elsewhen(!start_inputting_A || im2col_fin_reset){
  }.elsewhen(im2col_state === im2col_done){
    channel_wrap := 0.U
    //channel_wrap_count := 0.U
  }

  val channel_turn = Mux(req.channel(0) === 0.U, req.ch_per_turn, req.ch_per_turn + 1.U)

  //when(column_counter === block_size.U - 1.U && row_counter === filter_dim2 - 1.U){
  when(column_counter === block_size.U - 1.U && row_counter === channel_turn - 1.U){ //changed for row-wise multiple im2col blocks
    im2col_fin_reg := true.B
  }.elsewhen(req.channel >= 16.U && column_counter === block_size.U - 1.U){
    im2col_fin_reg := true.B
  }.elsewhen(!start_inputting_A || im2col_fin_reset){
   im2col_fin_reg := false.B
   //window_row_counter_saver := 0.U
   //channel_wrap := 0.U //need to initialize wrapping when horizontal block ends
  }.otherwise{ // when tiling, need to make it false again
    im2col_fin_reg := false.B
    //window_row_counter_saver := 0.U //where to initialize saver?
  }

  //val row_counter_deq = sram_read_signals_q.io.deq.bits.row_counter
  val row_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.row_counter) //Seah: changed - 1 clock delayed version
  val column_counter_deq_d = RegNext(sram_read_signals_q.io.deq.bits.column_counter)
  val sram_req_output = io.sram_reads(copy_addr.sp_bank()).resp.bits.data.asTypeOf(Vec(block_size, inputType))
  val window_start_counter_prev = RegInit(0.U((window_start_counter.getWidth).W)) //to store in which window_start_counter on this phase, and loaded it when the next vertical block starts
  val window_row_left = RegInit(0.U((window_start_counter.getWidth).W)) // to figure out starting point

  val req_valid = RegInit(false.B)
  val valid_count = RegInit(0.U(5.W))
  when(!req_valid){
    when(io.req.valid){
      req_valid := true.B
    }
  }.otherwise{
    when(valid_reg){
      valid_count := wrappingAdd(valid_count, 1.U, block_size.U)
      when(valid_count === block_size.U - 1.U){
        req_valid := false.B
        when(io.req.valid){req_valid := true.B}
      }.otherwise{req_valid := true.B}
    }
  }

/*
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
*/

  //when((!io.req.valid && !start_inputting_A && !sram_resp_valid) || im2col_fin_reset){
  when((!req_valid && !start_inputting_A && !sram_resp_valid) || im2col_fin_reset){
    column_counter := 0.U
    im2col_fin := false.B
    row_counter := 0.U
  }.elsewhen(!sram_resp_valid && sram_read_req){
    //loading previous values after one moving out
    window_start_counter := window_start_counter_prev //vertical
    //window_row_counter := window_row_left //should add im2col turn related value for more than 16 channel afterwards (horizontal)
  }.elsewhen(sram_resp_valid && sram_read_req && (req_valid||io.req.valid)){ //starting counting & valid requesting
    when(req.channel >= 16.U){ // 16 channel cases first (simplest)
      row_counter := 0.U
      when(!im2col_fin){
        column_counter := wrappingAdd(column_counter, 1.U, block_size)
        when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) || column_counter === block_size.U - 1.U){
          im2col_fin := true.B
        }.elsewhen((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U){
          window_start_counter := window_start_counter + input - output_width + 1.U
        }.otherwise{window_start_counter := window_start_counter + 1.U}
      }.otherwise { //when im2col_fin
        window_start_counter := 0.U
        when(im2col_turn === 1.U) {
          window_start_counter_prev := window_start_counter + 1.U //store (plus 1??)
          when((block_size.U + block_done * block_size.U) % output_width === 0.U){
            window_start_counter_prev := window_start_counter + input - output_width + 1.U
          }
        }
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width - block_done*block_size.U){
          column_counter := column_counter + 1.U
        }
      }
    }.otherwise{ //when channel < 16
      when(!im2col_fin){
        row_counter := wrappingAdd(row_counter, 1.U, channel_turn)
        when(row_counter === channel_turn - 1.U){
          window_row_counter := window_row_counter_saver //load saved value
          column_counter := wrappingAdd(column_counter, 1.U, block_size)
          when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) || column_counter === block_size.U - 1.U) {
            im2col_fin := true.B
            row_counter := channel_turn - 1.U
            window_row_counter_saver := window_row_counter //save current window_row_counter to resume next turn
          }.elsewhen((column_counter + 1.U + block_done * block_size.U) % output_width === 0.U){
            window_start_counter := window_start_counter + input - output_width + 1.U
          }.otherwise{window_start_counter := window_start_counter + 1.U}
        }.otherwise{
          when((row_counter + 1.U + row_counter_saver) % weight === 0.U) { //when finishing 1 weight row
            window_row_counter := window_row_counter + input - weight + 1.U
          }.otherwise{window_row_counter := window_row_counter + 1.U}
        }
      }.otherwise{ //when im2col_fin
        window_start_counter := 0.U
        when(im2col_turn === 1.U) {
          window_start_counter_prev := window_start_counter + 1.U //store (plus 1??)
          when((block_size.U + block_done * block_size.U) % output_width === 0.U){
            window_start_counter_prev := window_start_counter + input - output_width + 1.U
          }
        }
        when(column_counter === block_size.U - 1.U){
          column_counter := 0.U
        }.elsewhen(column_counter >= output_width*output_width - block_done*block_size.U){
          column_counter := column_counter + 1.U
        }
      }
    }
  }.elsewhen(!sram_read_valid && !sram_resp_valid) {
    when(tile_im2col_en) {
      im2col_fin := false.B
    }
    window_start_counter := 0.U
    column_counter := 0.U
    row_counter := 0.U
  }.elsewhen(!io.req.valid){
    column_counter := 0.U // when req.valid goes off and ended
    when((column_counter + block_done * block_size.U === output_width * output_width - 1.U) || column_counter === block_size.U - 1.U){
      im2col_fin := true.B
    }
    window_start_counter := 0.U
  }

  when(im2col_state =/= im2col_done) {
    when(im2col_fin_reg) {
      when(req.channel >= 16.U) {
        when(turn_count_save - im2col_turn + 1.U === weight * weight) { // when 16 channels finished
          window_row_counter := 0.U
        }.elsewhen((req.turn - im2col_turn + 1.U) % weight === 0.U) { //when finishing 1 weight row
          window_row_counter := window_row_counter + input - weight + 1.U //copy window_row_counter below
        }.otherwise {
          window_row_counter := window_row_counter + 1.U
        }
      }.otherwise {
        when(im2col_turn === 1.U){
          window_row_counter := 0.U
        }.otherwise{window_row_counter := window_row_counter_saver}
      }
    }
  }.otherwise{
    window_row_counter := 0.U
    window_row_counter_saver := 0.U
  }

  val sram_req_deq_valid_d = RegNext(sram_read_signals_q.io.deq.valid) // or just deq.valid?
  val sram_channel_turn = RegNext(sram_read_signals_q.io.deq.bits.channel_wrap)
  val sram_resp_valid_deq_d = RegNext(sram_read_signals_q.io.deq.bits.sram_resp_valid)

  //making output data valid signal
  //when(sram_resp_valid && sram_read_req){
  when(sram_req_deq_valid_d && sram_resp_valid_deq_d){
    when(im2col_fin_pulse){
      valid_reg := false.B
    }.elsewhen(column_counter_deq_d >= (output_width*output_width - sram_read_signals_q.io.deq.bits.block_done * block_size.U) && valid_reg){ //if finished earlier
      busy_reg := true.B
      valid_reg := true.B
    }.otherwise {
      busy_reg := true.B
      valid_reg := true.B
      when(req.channel < 16.U){
        valid_reg := false.B
        when(row_counter_deq_d === sram_channel_turn - 1.U){valid_reg := true.B}
      }
    }
  }.otherwise{
    busy_reg := false.B
    valid_reg := false.B
  }

  val assign_turn_done = (req.turn - sram_read_signals_q.io.deq.bits.im2col_turn)
  val starting_channel_deq = RegNext(sram_read_signals_q.io.deq.bits.starting_channel)

  val channel_left = req.channel - starting_channel_deq
  val sram_read_im2col_fin_d = RegNext(sram_read_signals_q.io.deq.bits.im2col_fin)
  val assign_data = row_counter_deq_d * req.channel
  dontTouch(assign_data)
  val assign_sub = RegNext(sram_read_signals_q.io.deq.bits.assign_sub)

  val assign_leftover = WireInit(0.U(4.W))
  when(req.channel < 16.U && im2col_turn === 1.U){ // on the last turn, empty columns
    assign_leftover := (turn_done + 1.U) * block_size.U - req.channel * weight * weight // no need to compute these
  }

  when(!sram_read_im2col_fin_d && sram_req_deq_valid_d && sram_resp_valid_deq_d){
    when((starting_channel_deq + block_size.U) <= req.channel && req.channel >= 16.U){
      im2col_reg := sram_req_output
    }.otherwise{
      for(i <- 0 until block_size){
        when(i.U < channel_left){
          when(i.U + assign_data - assign_sub < block_size.U - assign_leftover && i.U + assign_data - assign_sub >= 0.U) {
            im2col_reg(i.U + assign_data - assign_sub) := sram_req_output(i.U)
          }
        }//.otherwise{im2col_reg(i) := 0.S}
      }
    }
  }.otherwise{
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
    val starting_channel = UInt(10.W)
    val channel_wrap = UInt(10.W)
    val assign_sub = UInt(5.W)
    val sram_resp_valid = Bool()
    //val row_residue = UInt(log2Up(block_size*block_size).W)
    //val row_counter_save = UInt(log2Up(block_size*block_size).W)
    //val read_output = Vec(block_size, inputType)

  }

  /*
  val row_address = WireInit(row_counter)
  when((row_counter + row_counter_saver) > filter_dim2 - 1.U){
    row_address := row_counter - filter_dim2
  }.otherwise{row_address := row_counter} //saver for 0 now
   */


  sram_read_signals_q.io.enq.valid := sram_read_req && io.req.valid//(sram_resp_valid && sram_read_valid)
  sram_read_signals_q.io.enq.bits.row_counter := row_counter//row_address
  sram_read_signals_q.io.enq.bits.column_counter := column_counter
  sram_read_signals_q.io.enq.bits.im2col_fin := im2col_fin
  sram_read_signals_q.io.enq.bits.im2col_fin_pulse := im2col_fin_reg
  sram_read_signals_q.io.enq.bits.im2col_turn := im2col_turn
  sram_read_signals_q.io.enq.bits.block_done := block_done
  sram_read_signals_q.io.enq.bits.start_inputting := start_inputting_A
  sram_read_signals_q.io.enq.bits.starting_channel := starting_channel*block_size.U
  sram_read_signals_q.io.enq.bits.channel_wrap := channel_turn
  sram_read_signals_q.io.enq.bits.assign_sub := assign_data_sub
  sram_read_signals_q.io.enq.bits.sram_resp_valid := sram_resp_valid
  //sram_read_signals_q.io.enq.bits.row_residue := row_residue
  //sram_read_signals_q.io.enq.bits.row_counter_save := row_counter_saver
  when(req.channel < 16.U){sram_read_signals_q.io.enq.valid := sram_read_req}
  //when(weird_case){sram_read_signals_q.io.enq.valid := sram_read_req && io.req.valid}

  sram_read_signals_q.io.deq.ready := true.B//sram_resp_valid

  /* Example of how to interface with scratchpad
  io.spad_reads(req.addr.sp_bank()).req.valid := true.B
  io.spad_reads(req.addr.sp_bank()).req.bits.addr := req.addr.sp_row()

  io.spad_reads(req.addr.sp_bank()).resp
  */
}
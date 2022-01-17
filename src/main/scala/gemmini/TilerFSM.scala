//===========================================================================
// TilerController's Internal FSM implementation
//===========================================================================
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import GemminiISA._
import Util.regwire

class TilerFSM[T <: Data : Arithmetic, U <: Data, V <: Data]
  (config: GemminiArrayConfig[T,U,V])(implicit val p: Parameters)
  extends Module with HasCoreParameters {
  import config._

  //=========================================================================
  // interface
  //=========================================================================
  val io = IO(new Bundle {
    val cmd_in    = Flipped(Decoupled(new TilerCmd(LOG2_ROB_ENTRIES)))
    val sched_out = Decoupled(new RoCCCommand)
    val busy      = Output(Bool())
  })
  // hardcode 8 entries with up to 2 pushes per cycle
  val schedq = Module(new MultiTailedQueue(new RoCCCommand, 8, 2))
  io.sched_out <> schedq.io.deq

  val sched = schedq.io.enq
  val cmd   = io.cmd_in.bits
  val busy  = io.busy

  // initialize ports/pins
  for (i <- 0 to 1) {
    sched.bits(i) := DontCare
    sched.bits(i).status := io.cmd_in.bits.status
  }
  sched.push := 0.U
  io.cmd_in.ready := false.B
  busy := true.B

  //=========================================================================
  // FSM states (see diagram for what each state does)
  //=========================================================================
  val (s_IDLE ::
      s_RESET_OUTPUT_GROUP ::
      s_RESET_A_TILE_SUBCOL ::
      s_MOVE_FIRST_B_TILE_INTO_SP ::
      s_RESET_B_TILE_SUBCOL_IN_SUBROW ::
      s_MAYBE_MOVE_NEXT_B_TILE_INTO_SP ::
      s_RESET_A_TILE_SUBROW_IN_SUBCOL ::
      s_MAYBE_MOVE_A_TILE_INTO_SP ::
      s_MAYBE_MOVE_D_TILE_INTO_ACC ::
      s_PRELOAD_B_TILE_INTO_ARRAY_AND_SET_C_ADDR_IN_ACC ::
      s_DO_MATMUL ::
      s_MAYBE_MOVE_C_TILE_INTO_MEM ::
      s_NEXT_A_TILE_SUBROW_IN_SUBCOL ::
      s_NEXT_B_TILE_SUBCOL_IN_SUBROW ::
      s_NEXT_A_TILE_SUBCOL ::
      s_NEXT_OUTPUT_GROUP ::
      Nil) = Enum(16)

  val state = RegInit(s_IDLE)

  //=========================================================================
  // Internal State
  //=========================================================================

  //------------------------------------------------------------------------
  // input data-specific constants
  //------------------------------------------------------------------------
  val g_HAS_BIAS              = Reg(Bool())
  val g_REPEATING_BIAS        = Reg(Bool())
  // exec-configs
  val g_DATAFLOW              = Reg(UInt(1.W))
  val g_ACTIVATION            = Reg(UInt(2.W))
  val g_SYSTOLIC_OUT_RSHIFT   = Reg(UInt(LOG2_OTYPE_BITS.W))
  val g_ACC_OUT_RSHIFT        = Reg(UInt(LOG2_OTYPE_BITS.W))
  val g_RELU6_IN_LSHIFT       = Reg(UInt(LOG2_OTYPE_BITS.W))
  // mem-addr of A-matrix
  val g_A_MEM_ADDR            = Reg(UInt(xLen.W))
  val g_B_MEM_ADDR            = Reg(UInt(xLen.W))
  val g_C_MEM_ADDR            = Reg(UInt(xLen.W))
  val g_D_MEM_ADDR            = Reg(UInt(xLen.W))
  // bytes in A-matrix row * rows-per-tile
  val g_A_BYTES_PER_TILE_ROW  = Reg(UInt(LOG2_MNK_BYTES_PER_TILE_ROW.W))
  val g_B_BYTES_PER_TILE_ROW  = Reg(UInt(LOG2_MNK_BYTES_PER_TILE_ROW.W))
  val g_C_BYTES_PER_TILE_ROW  = Reg(UInt(LOG2_MNK_BYTES_PER_TILE_ROW.W))
  val g_D_BYTES_PER_TILE_ROW  = Reg(UInt(LOG2_MNK_BYTES_PER_TILE_ROW.W))
  // bytes in A-matrix row
  val g_A_BYTES_PER_ROW       = Reg(UInt(LOG2_MNK_BYTES.W))
  val g_B_BYTES_PER_ROW       = Reg(UInt(LOG2_MNK_BYTES.W))
  val g_C_BYTES_PER_ROW       = Reg(UInt(LOG2_MNK_BYTES.W))
  val g_D_BYTES_PER_ROW       = Reg(UInt(LOG2_MNK_BYTES.W))
  // needed for 0-padding last rows/cols
  val g_LAST_M_ITEMS          = Reg(UInt(LOG2_DIM_COUNT.W))
  val g_LAST_N_ITEMS          = Reg(UInt(LOG2_DIM_COUNT.W))
  val g_LAST_K_ITEMS          = Reg(UInt(LOG2_DIM_COUNT.W))
  // last (x,y,x) tile idx in (C,C,A) matrix
  val g_TILE_ROW_END          = Reg(UInt(LOG2_TILE_IDX.W))
  val g_TILE_COL_END          = Reg(UInt(LOG2_TILE_IDX.W))
  val g_K_TILE_COL_END        = Reg(UInt(LOG2_TILE_IDX.W))

  //------------------------------------------------------------------------
  // combinational calculation of optimal output-groups. this is updated at
  // the s_IDLE -> s_RESET_OUTPUT_GROUP state transition
  //------------------------------------------------------------------------
  val g_OG_DIM_SELECT = OG_HEIGHT_MAP.zipWithIndex.map{ case(h,i) =>
    val w = TOTAL_ACC_TILES/h
    if (h < w)      WireDefault(g_TILE_ROW_END < h.U)
    else if(h > w)  WireDefault(g_TILE_COL_END < w.U)
    else            WireDefault((i == (OG_HEIGHT_MAP.size-1)).asBool)
  }
  val g_TILE_ROWS_PER_GROUP = MuxCase(
    OG_HEIGHT_MAP(OG_HEIGHT_MAP.size-1).U,
    OG_HEIGHT_MAP.zipWithIndex.map{
      case(h,i) => (g_OG_DIM_SELECT(i) -> h.U)
    })
  val g_TILE_COLS_PER_GROUP = MuxCase(
    (TOTAL_ACC_TILES / OG_HEIGHT_MAP(OG_HEIGHT_MAP.size-1)).U,
    OG_HEIGHT_MAP.zipWithIndex.map{
      case(h,i) => (g_OG_DIM_SELECT(i) -> (TOTAL_ACC_TILES/h).U)
    })
  val g_I_BYTE_COLS_PER_GROUP = g_TILE_COLS_PER_GROUP <<I_TILE_BYTE_WIDTH_LOG2
  val g_O_BYTE_COLS_PER_GROUP = g_TILE_COLS_PER_GROUP <<O_TILE_BYTE_WIDTH_LOG2

  //------------------------------------------------------------------------
  // global state persistent across all loops
  //------------------------------------------------------------------------
  // current output-group tile (y,x)
  val (gbl_tile_row, gbl_tile_row_n) = regwire(LOG2_TILE_IDX)
  val (gbl_tile_col, gbl_tile_col_n) = regwire(LOG2_TILE_IDX)
  // how many elements (tall,wide) is this tile
  val gbl_item_rows = Reg(UInt(LOG2_DIM_COUNT.W))
  val gbl_item_cols = Reg(UInt(LOG2_DIM_COUNT.W))
  // which tmp-slot in sp being used now, and which is the alternate
  val gbl_B_cur_sp_row_addr = Reg(UInt(LOG2_SP_ROWS.W))
  val gbl_B_alt_sp_row_addr = Reg(UInt(LOG2_SP_ROWS.W))

  //------------------------------------------------------------------------
  // global state that is reset for each output-group
  //------------------------------------------------------------------------
  // where to put next C/D-tile in acc
  val gbl_CD_acc_row_addr = Reg(UInt(LOG2_ACC_ROWS.W))

  //------------------------------------------------------------------------
  // loop1-local state
  //------------------------------------------------------------------------
  // (ul-x,ul-y,br-x,br-y) tile x in output-group
  val loop1_tile_col_start = Reg(UInt(LOG2_TILE_IDX.W))
  val loop1_tile_col_end   = Reg(UInt(LOG2_TILE_IDX.W))
  val loop1_tile_row_start = Reg(UInt(LOG2_TILE_IDX.W))
  val loop1_tile_row_end   = Reg(UInt(LOG2_TILE_IDX.W))
  // initialized from global-constants
  val loop1_A_mem_addr = Reg(UInt(xLen.W))
  val loop1_B_mem_addr = Reg(UInt(xLen.W))
  val loop1_C_mem_addr = Reg(UInt(xLen.W))
  val loop1_D_mem_addr = Reg(UInt(xLen.W))

  //------------------------------------------------------------------------
  // loop2-local state
  //------------------------------------------------------------------------
  // which tile-column in A we are in
  val (loop2_k_tile_col, loop2_k_tile_col_n) = regwire(LOG2_TILE_IDX)
  // how many elems in k-dim is this tile
  val loop2_k_item_dims = Reg(UInt(LOG2_DIM_COUNT.W))
  // initialized from loop1 values
  val loop2_A_mem_addr = Reg(UInt(xLen.W))
  val loop2_B_mem_addr = Reg(UInt(xLen.W))
  val loop2_C_mem_addr = Reg(UInt(xLen.W))
  val loop2_D_mem_addr = Reg(UInt(xLen.W))

  //------------------------------------------------------------------------
  // loop3-local state
  //------------------------------------------------------------------------
  // initialized from loop2 values
  val loop3_A_mem_addr = Reg(UInt(xLen.W))
  val loop3_B_mem_addr = Reg(UInt(xLen.W))
  val loop3_C_mem_addr = Reg(UInt(xLen.W))
  val loop3_D_mem_addr = Reg(UInt(xLen.W))

  //------------------------------------------------------------------------
  // loop4-local state
  //------------------------------------------------------------------------
  // where in the sp is the next A tile
  val loop4_A_sp_row_addr = Reg(UInt(LOG2_SP_ROWS.W))
  // initialized from loop3 values
  val loop4_A_mem_addr = Reg(UInt(xLen.W))
  val loop4_B_mem_addr = Reg(UInt(xLen.W))
  val loop4_C_mem_addr = Reg(UInt(xLen.W))
  val loop4_D_mem_addr = Reg(UInt(xLen.W))

  //=========================================================================
  // utilies used by FSM core
  //=========================================================================

  // continuous assigns (only added in the switch-cases that call this!)
  def update_tile_dims(dummy: Int = 0) = {
    gbl_item_rows     := Mux(gbl_tile_row_n === g_TILE_ROW_END,
                             g_LAST_M_ITEMS, DIM.U)
    gbl_item_cols     := Mux(gbl_tile_col_n === g_TILE_COL_END,
                             g_LAST_N_ITEMS, DIM.U)
    loop2_k_item_dims := Mux(loop2_k_tile_col_n === g_K_TILE_COL_END,
                             g_LAST_K_ITEMS, DIM.U)
  }

  def MIN(a: UInt, b: UInt) = Mux(a < b, a, b)

  //=========================================================================
  // FSM core
  //=========================================================================
  switch (state) {
    is (s_IDLE) {
      val l_HAS_BIAS      = (cmd.addr_d =/= 0.U)
      val l_A_BYTE_WIDTH  = WireDefault(cmd.k << LOG2_ITYPE_BYTES.U)
      val l_BC_BYTE_WIDTH = WireDefault(cmd.n << LOG2_ITYPE_BYTES.U)
      val l_D_BYTE_WIDTH  = WireDefault(cmd.n << LOG2_OTYPE_BYTES.U)

      g_HAS_BIAS              := l_HAS_BIAS
      g_REPEATING_BIAS        := cmd.repeating_bias

      g_DATAFLOW              := Dataflow.WS.id.U
      g_ACTIVATION            := cmd.activation
      g_SYSTOLIC_OUT_RSHIFT   := cmd.in_rshift
      g_ACC_OUT_RSHIFT        := cmd.acc_rshift
      g_RELU6_IN_LSHIFT       := cmd.relu6_lshift

      g_A_MEM_ADDR            := cmd.addr_a
      g_B_MEM_ADDR            := cmd.addr_b
      g_C_MEM_ADDR            := cmd.addr_c
      g_D_MEM_ADDR            := cmd.addr_d

      g_A_BYTES_PER_TILE_ROW  := l_A_BYTE_WIDTH  << LOG2_DIM.U
      g_B_BYTES_PER_TILE_ROW  := l_BC_BYTE_WIDTH << LOG2_DIM.U
      g_C_BYTES_PER_TILE_ROW  := l_BC_BYTE_WIDTH << LOG2_DIM.U
      g_D_BYTES_PER_TILE_ROW  := l_D_BYTE_WIDTH  << LOG2_DIM.U

      g_A_BYTES_PER_ROW       := l_A_BYTE_WIDTH
      g_B_BYTES_PER_ROW       := l_BC_BYTE_WIDTH
      g_C_BYTES_PER_ROW       := l_BC_BYTE_WIDTH
      g_D_BYTES_PER_ROW       := l_D_BYTE_WIDTH

      g_LAST_M_ITEMS := Mux(cmd.m(LOG2_DIM-1,0).orR,cmd.m(LOG2_DIM-1,0),DIM.U)
      g_LAST_N_ITEMS := Mux(cmd.n(LOG2_DIM-1,0).orR,cmd.n(LOG2_DIM-1,0),DIM.U)
      g_LAST_K_ITEMS := Mux(cmd.k(LOG2_DIM-1,0).orR,cmd.k(LOG2_DIM-1,0),DIM.U)

      g_TILE_ROW_END   := (cmd.m >> LOG2_DIM) + cmd.m(LOG2_DIM-1,0).orR - 1.U
      g_TILE_COL_END   := (cmd.n >> LOG2_DIM) + cmd.n(LOG2_DIM-1,0).orR - 1.U
      g_K_TILE_COL_END := (cmd.k >> LOG2_DIM) + cmd.k(LOG2_DIM-1,0).orR - 1.U

      // update interface signals. we are only ready when an input cmd is
      // ready AND the output queue has 2 slots available to write to
      io.cmd_in.ready := (sched.ready >= 2.U)

      // issue gemmini commands
      // NOTE: the "h10000".U(17) is because a_addr_stride was added to ExecuteController
      when(io.cmd_in.fire) {
        sched.push               := 2.U
        sched.bits(0).inst.funct := CONFIG_CMD
        sched.bits(0).rs1        := (g_ACC_OUT_RSHIFT << 32) |
                                    "h10000".U(17.W) |
                                    (g_ACTIVATION << 3) |
                                    (g_DATAFLOW << 2) |
                                    CONFIG_EX
        sched.bits(0).rs2        := (g_RELU6_IN_LSHIFT << 32) |
                                    g_SYSTOLIC_OUT_RSHIFT
        sched.bits(1).inst.funct := CONFIG_CMD
        sched.bits(1).rs1        := CONFIG_STORE
        sched.bits(1).rs2        := g_C_BYTES_PER_ROW

        // update next state
        state := s_RESET_OUTPUT_GROUP
      }
      .otherwise {
        // if we are sitting in idle state, we are not busy!
        busy := false.B
      }
    }
    //=======================================================================
    is (s_RESET_OUTPUT_GROUP) {
      // define mutable gbl state, persist across all ogs
      gbl_tile_row_n        := 0.U
      gbl_tile_col_n        := 0.U
      gbl_B_cur_sp_row_addr := GBL_B_SP_ROW_ADDR_1.U
      gbl_B_alt_sp_row_addr := GBL_B_SP_ROW_ADDR_2.U
      update_tile_dims()

      // define mutable gbl state, reset after each og
      gbl_CD_acc_row_addr := 0.U

      loop1_tile_col_start := gbl_tile_col_n
      loop1_tile_col_end   := MIN(gbl_tile_col_n + g_TILE_COLS_PER_GROUP-1.U,
                                  g_TILE_COL_END)
      loop1_tile_row_start := gbl_tile_row_n
      loop1_tile_row_end   := MIN(gbl_tile_row_n + g_TILE_ROWS_PER_GROUP-1.U,
                                  g_TILE_ROW_END)

      // derived pointers to matrices in memory for this og
      loop1_A_mem_addr := g_A_MEM_ADDR
      loop1_B_mem_addr := g_B_MEM_ADDR
      loop1_C_mem_addr := g_C_MEM_ADDR
      loop1_D_mem_addr := g_D_MEM_ADDR

      // update next state
      state := s_RESET_A_TILE_SUBCOL
    }
    //=======================================================================
    is (s_RESET_A_TILE_SUBCOL) {
      loop2_k_tile_col_n  := 0.U
      gbl_tile_row_n      := loop1_tile_row_start
      gbl_tile_col_n      := loop1_tile_col_start
      gbl_CD_acc_row_addr := 0.U
      update_tile_dims()

      loop2_A_mem_addr := loop1_A_mem_addr
      loop2_B_mem_addr := loop1_B_mem_addr
      loop2_C_mem_addr := loop1_C_mem_addr
      loop2_D_mem_addr := loop1_D_mem_addr

      // update next state
      state := s_MOVE_FIRST_B_TILE_INTO_SP
    }
    //=======================================================================
    is (s_MOVE_FIRST_B_TILE_INTO_SP) {
      // calculate mvin parameters
      val B_mem_addr    = loop2_B_mem_addr
      val B_mem_stride  = g_B_BYTES_PER_ROW

      val B_sp_row_addr = Wire(UInt(32.W))
      val B_item_rows   = Wire(UInt(16.W))
      val B_item_cols   = Wire(UInt(16.W))
      B_sp_row_addr := gbl_B_cur_sp_row_addr
      B_item_rows   := loop2_k_item_dims
      B_item_cols   := gbl_item_cols

      // issue gemmini commands
      when(sched.ready >= 2.U) {
        sched.push          := 2.U
        // NOTE: set 33rd bit to 1 so LoadController scaling works
        sched.bits(0).rs1   := "h100000000".U(33.W) | CONFIG_LOAD
        sched.bits(0).rs2   := B_mem_stride
        sched.bits(0).inst.funct := CONFIG_CMD
        sched.bits(1).rs1   := B_mem_addr
        sched.bits(1).rs2   := Cat(B_item_rows,B_item_cols,B_sp_row_addr)
        sched.bits(1).inst.funct := LOAD_CMD

        // update next state
        state := s_RESET_B_TILE_SUBCOL_IN_SUBROW
      }
    }
    //=======================================================================
    is (s_RESET_B_TILE_SUBCOL_IN_SUBROW) {
      loop3_A_mem_addr := loop2_A_mem_addr
      loop3_B_mem_addr := loop2_B_mem_addr
      loop3_C_mem_addr := loop2_C_mem_addr
      loop3_D_mem_addr := loop2_D_mem_addr

      // update next state
      state := s_MAYBE_MOVE_NEXT_B_TILE_INTO_SP
    }
    //=======================================================================
    is (s_MAYBE_MOVE_NEXT_B_TILE_INTO_SP) {
      // calculate mvin parameters
      val B_mem_addr    = loop3_B_mem_addr + I_TILE_BYTE_WIDTH.U
      val B_mem_stride  = g_B_BYTES_PER_ROW

      val B_sp_row_addr = Wire(UInt(32.W))
      val B_item_rows   = Wire(UInt(16.W))
      val B_item_cols   = Wire(UInt(16.W))
      B_sp_row_addr := gbl_B_alt_sp_row_addr
      B_item_rows   := loop2_k_item_dims
      B_item_cols   := Mux(gbl_tile_col === g_TILE_COL_END-1.U,
                           g_LAST_N_ITEMS, DIM.U)

      // can't load next B-tile if we are already on the last one
      when (gbl_tile_col === loop1_tile_col_end) {
        state := s_RESET_A_TILE_SUBROW_IN_SUBCOL
      }
      .elsewhen (sched.ready >= 2.U) {
        sched.push          := 2.U
        sched.bits(0).rs1   := "h100000000".U(33.W) | CONFIG_LOAD
        sched.bits(0).rs2   := B_mem_stride
        sched.bits(0).inst.funct := CONFIG_CMD
        sched.bits(1).rs1   := B_mem_addr
        sched.bits(1).rs2   := Cat(B_item_rows,B_item_cols,B_sp_row_addr)
        sched.bits(1).inst.funct := LOAD_CMD

        // update next state
        state := s_RESET_A_TILE_SUBROW_IN_SUBCOL
      }
    }
    //=======================================================================
    is (s_RESET_A_TILE_SUBROW_IN_SUBCOL) {
      // this scope modifies: gbl_tile_row
      //                      gbl_CD_acc_row_addr
      loop4_A_mem_addr := loop3_A_mem_addr
      loop4_B_mem_addr := loop3_B_mem_addr
      loop4_C_mem_addr := loop3_C_mem_addr
      loop4_D_mem_addr := loop3_D_mem_addr

      loop4_A_sp_row_addr := 0.U

      // update next state
      state := s_MAYBE_MOVE_A_TILE_INTO_SP
    }
    //=======================================================================
    is (s_MAYBE_MOVE_A_TILE_INTO_SP) {
      // calculate mvin parameters
      val A_mem_addr    = loop4_A_mem_addr
      val A_mem_stride  = g_A_BYTES_PER_ROW

      val A_sp_row_addr = Wire(UInt(32.W))
      val A_item_rows   = Wire(UInt(16.W))
      val A_item_cols   = Wire(UInt(16.W))
      A_sp_row_addr := loop4_A_sp_row_addr
      A_item_rows   := gbl_item_rows
      A_item_cols   := loop2_k_item_dims

      // only move A-tiles in during first column of tiles in the og
      when (gbl_tile_col =/= loop1_tile_col_start) {
        state := s_MAYBE_MOVE_D_TILE_INTO_ACC
      }
      .elsewhen (sched.ready >= 2.U) {
        sched.push          := 2.U
        sched.bits(0).rs1   := "h100000000".U(33.W) | CONFIG_LOAD
        sched.bits(0).rs2   := A_mem_stride
        sched.bits(0).inst.funct := CONFIG_CMD
        sched.bits(1).rs1   := A_mem_addr
        sched.bits(1).rs2   := Cat(A_item_rows,A_item_cols,A_sp_row_addr)
        sched.bits(1).inst.funct := LOAD_CMD

        // update next state
        state := s_MAYBE_MOVE_D_TILE_INTO_ACC
      }
    }
    //=======================================================================
    is (s_MAYBE_MOVE_D_TILE_INTO_ACC) {
      // calculate mvin parameters (NOTE: we know D is valid at this point)
      val D_mem_addr     = loop4_D_mem_addr
      val D_mem_stride   = Mux(g_REPEATING_BIAS, 0.U, g_D_BYTES_PER_ROW)

      val D_acc_row_addr = Wire(UInt(32.W))
      val D_item_rows    = Wire(UInt(16.W))
      val D_item_cols    = Wire(UInt(16.W))
      D_acc_row_addr := "h80000000".U | gbl_CD_acc_row_addr
      D_item_rows    := gbl_item_rows
      D_item_cols    := gbl_item_cols

      // only move D-tiles in during first partial-sum in an output-group
      when((loop2_k_tile_col =/= 0.U) || !g_HAS_BIAS) {
        state := s_PRELOAD_B_TILE_INTO_ARRAY_AND_SET_C_ADDR_IN_ACC
      }
      .elsewhen (sched.ready >= 2.U) {
        sched.push          := 2.U
        sched.bits(0).rs1   := "h100000000".U(33.W) | CONFIG_LOAD
        sched.bits(0).rs2   := D_mem_stride
        sched.bits(0).inst.funct := CONFIG_CMD
        sched.bits(1).rs1   := D_mem_addr
        sched.bits(1).rs2   := Cat(D_item_rows,D_item_cols,D_acc_row_addr)
        sched.bits(1).inst.funct := LOAD_CMD

        // update next state
        state := s_PRELOAD_B_TILE_INTO_ARRAY_AND_SET_C_ADDR_IN_ACC
      }
    }
    //=======================================================================
    is (s_PRELOAD_B_TILE_INTO_ARRAY_AND_SET_C_ADDR_IN_ACC) {
      // on first tile in 4th loop: preload this B-tile
      // else:                      preload garbage B-tile (no spad load)
      val B_sp_row_addr = Wire(UInt(32.W))
      val B_item_rows   = Wire(UInt(16.W))
      val B_item_cols   = Wire(UInt(16.W))
      B_sp_row_addr := Mux(gbl_tile_row === loop1_tile_row_start,
                           gbl_B_cur_sp_row_addr,
                           GARBAGE_ADDR)
      B_item_rows   := loop2_k_item_dims
      B_item_cols   := gbl_item_cols

      // if has D-bias already loaded: accumulate c in accumulator
      // elif first k-col in 2nd loop: overwrite c in accumulator
      // else:                         accumulate c in accumulator
      val C_acc_row_addr = Wire(UInt(32.W))
      val C_item_rows    = Wire(UInt(16.W))
      val C_item_cols    = Wire(UInt(16.W))
      C_acc_row_addr := Mux(g_HAS_BIAS || (loop2_k_tile_col > 0.U),
                            "hC0000000".U | gbl_CD_acc_row_addr,
                            "h80000000".U | gbl_CD_acc_row_addr)
      C_item_rows    := gbl_item_rows
      C_item_cols    := gbl_item_cols

      when (sched.ready >= 1.U) {
        sched.push          := 1.U
        sched.bits(0).rs1   := Cat(B_item_rows,B_item_cols,B_sp_row_addr)
        sched.bits(0).rs2   := Cat(C_item_rows,C_item_cols,C_acc_row_addr)
        sched.bits(0).inst.funct := PRELOAD_CMD

        // update next state
        state := s_DO_MATMUL
      }
    }
    //=======================================================================
    is (s_DO_MATMUL) {
      // calculate compute parameters
      val A_sp_row_addr = Wire(UInt(32.W))
      val A_item_rows   = Wire(UInt(16.W))
      val A_item_cols   = Wire(UInt(16.W))
      A_sp_row_addr := loop4_A_sp_row_addr
      A_item_rows   := gbl_item_rows
      A_item_cols   := loop2_k_item_dims

      val D_acc_row_addr = GARBAGE_ADDR
      val D_item_rows    = Wire(UInt(16.W))
      val D_item_cols    = Wire(UInt(16.W))
      D_item_rows    := gbl_item_rows
      D_item_cols    := gbl_item_cols

      // on first tile in 4th loop: compute_preloaded
      // else: compute_accumulated
      when (sched.ready >= 1.U) {
        sched.push          := 1.U
        sched.bits(0).rs1   := Cat(A_item_rows,A_item_cols,A_sp_row_addr)
        sched.bits(0).rs2   := Cat(D_item_rows,D_item_cols,D_acc_row_addr)
        when (gbl_tile_row === loop1_tile_row_start) {
          sched.bits(0).inst.funct := COMPUTE_AND_FLIP_CMD
        }
        .otherwise {
          sched.bits(0).inst.funct := COMPUTE_AND_STAY_CMD
        }
        // update next state
        state := s_MAYBE_MOVE_C_TILE_INTO_MEM
      }
    }
    //=======================================================================
    is (s_MAYBE_MOVE_C_TILE_INTO_MEM) {
      val C_mem_addr     = loop4_C_mem_addr
      val C_mem_stride   = g_C_BYTES_PER_ROW

      val C_acc_row_addr = Wire(UInt(32.W))
      val C_item_rows    = Wire(UInt(16.W))
      val C_item_cols    = Wire(UInt(16.W))
      C_acc_row_addr := "h80000000".U | gbl_CD_acc_row_addr
      C_item_rows    := gbl_item_rows
      C_item_cols    := gbl_item_cols

      when(loop2_k_tile_col =/= g_K_TILE_COL_END) {
        state := s_NEXT_A_TILE_SUBROW_IN_SUBCOL
      }
      .elsewhen (sched.ready >= 1.U) {
        sched.push          := 1.U
        sched.bits(0).rs1   := C_mem_addr
        sched.bits(0).rs2   := Cat(C_item_rows,C_item_cols,C_acc_row_addr)
        sched.bits(0).inst.funct := STORE_CMD

        // update next state
        state := s_NEXT_A_TILE_SUBROW_IN_SUBCOL
      }
    }
    //=======================================================================
    is (s_NEXT_A_TILE_SUBROW_IN_SUBCOL) {
      when (gbl_tile_row === loop1_tile_row_end) {
        // just finished the final row of tiles in the 4th loop
        state := s_NEXT_B_TILE_SUBCOL_IN_SUBROW
      }
      .otherwise {
        // modify global state
        gbl_tile_row_n      := gbl_tile_row        + 1.U
        gbl_CD_acc_row_addr := gbl_CD_acc_row_addr + BYTE_ROWS_PER_TILE.U
        update_tile_dims()

        // modify loop4-local state
        loop4_A_mem_addr    := loop4_A_mem_addr + g_A_BYTES_PER_TILE_ROW
        loop4_B_mem_addr    := loop4_B_mem_addr
        loop4_C_mem_addr    := loop4_C_mem_addr + g_C_BYTES_PER_TILE_ROW
        loop4_D_mem_addr    := loop4_D_mem_addr +
                                Mux(g_HAS_BIAS && !g_REPEATING_BIAS,
                                    g_D_BYTES_PER_TILE_ROW, 0.U)

        loop4_A_sp_row_addr := loop4_A_sp_row_addr + BYTE_ROWS_PER_TILE.U

        // update next state
        state := s_MAYBE_MOVE_A_TILE_INTO_SP
      }
    }
    //=======================================================================
    is (s_NEXT_B_TILE_SUBCOL_IN_SUBROW) {
      when (gbl_tile_col === loop1_tile_col_end) {
        // we have already done the last column in the output-group
        state := s_NEXT_A_TILE_SUBCOL
      }
      .otherwise {
        // modify global state
        gbl_tile_row_n      := loop1_tile_row_start
        gbl_tile_col_n      := gbl_tile_col        + 1.U
        gbl_CD_acc_row_addr := gbl_CD_acc_row_addr + BYTE_ROWS_PER_TILE.U
        update_tile_dims()

        gbl_B_cur_sp_row_addr := gbl_B_alt_sp_row_addr
        gbl_B_alt_sp_row_addr := gbl_B_cur_sp_row_addr

        // modify loop3-local state
        loop3_A_mem_addr := loop3_A_mem_addr + 0.U
        loop3_B_mem_addr := loop3_B_mem_addr + I_TILE_BYTE_WIDTH.U
        loop3_C_mem_addr := loop3_C_mem_addr + I_TILE_BYTE_WIDTH.U
        loop3_D_mem_addr := loop3_D_mem_addr + O_TILE_BYTE_WIDTH.U

        // update next state
        state := s_MAYBE_MOVE_NEXT_B_TILE_INTO_SP
      }
    }
    //=======================================================================
    is (s_NEXT_A_TILE_SUBCOL) {
      when (loop2_k_tile_col === g_K_TILE_COL_END) {
        state := s_NEXT_OUTPUT_GROUP
      }
      .otherwise {
        loop2_k_tile_col_n  := loop2_k_tile_col + 1.U
        gbl_tile_row_n      := loop1_tile_row_start
        gbl_tile_col_n      := loop1_tile_col_start
        gbl_CD_acc_row_addr := 0.U
        update_tile_dims()

        loop2_A_mem_addr := loop2_A_mem_addr + I_TILE_BYTE_WIDTH.U
        loop2_B_mem_addr := loop2_B_mem_addr + g_B_BYTES_PER_TILE_ROW
        loop2_C_mem_addr := loop2_C_mem_addr + 0.U
        loop2_D_mem_addr := loop2_D_mem_addr + 0.U

        // swap current/alternate B-tile scratchpad addrs
        gbl_B_cur_sp_row_addr := gbl_B_alt_sp_row_addr
        gbl_B_alt_sp_row_addr := gbl_B_cur_sp_row_addr

        // update next state
        state := s_MOVE_FIRST_B_TILE_INTO_SP
      }
    }
    //=======================================================================
    is (s_NEXT_OUTPUT_GROUP) {
      val l_did_row_incr = WireDefault(false.B)
      val l_did_col_incr = WireDefault(false.B)

      when (gbl_tile_col === g_TILE_COL_END &&
            gbl_tile_row === g_TILE_ROW_END) {
        // update next state
        state := s_IDLE
      }
      .otherwise {
        when (gbl_tile_col === g_TILE_COL_END) {
          gbl_tile_row_n := gbl_tile_row + 1.U
          gbl_tile_col_n := 0.U
          update_tile_dims()
          l_did_row_incr := true.B
        }
        .otherwise {
          // TODO: this is a bug!
          gbl_tile_row_n := gbl_tile_row
          gbl_tile_col_n := gbl_tile_col + 1.U
          update_tile_dims()
          l_did_col_incr := true.B
        }

        // reset global state that resets for each new output-group
        gbl_CD_acc_row_addr := 0.U

        // update the start/end tiles for this output-group (inclusive)
        val l_tile_col_start = gbl_tile_col_n
        val l_tile_col_end  = MIN(gbl_tile_col_n + g_TILE_COLS_PER_GROUP-1.U,
                                  g_TILE_COL_END)
        val l_tile_row_start = gbl_tile_row_n
        val l_tile_row_end  = MIN(gbl_tile_row_n + g_TILE_ROWS_PER_GROUP-1.U,
                                  g_TILE_ROW_END)

        loop1_tile_col_start := l_tile_col_start
        loop1_tile_col_end   := l_tile_col_end

        loop1_tile_row_start := l_tile_row_start
        loop1_tile_row_end   := l_tile_row_end


        // update all derived pointers to matrices in memory
        when(l_did_row_incr) {
          loop1_A_mem_addr := g_A_MEM_ADDR + (l_tile_row_start *
                                              g_A_BYTES_PER_TILE_ROW)
          loop1_B_mem_addr := g_B_MEM_ADDR
          loop1_C_mem_addr := g_C_MEM_ADDR + (l_tile_row_start *
                                              g_C_BYTES_PER_TILE_ROW)
          loop1_D_mem_addr := Mux(!g_HAS_BIAS, 0.U,
                                Mux(g_REPEATING_BIAS, g_D_MEM_ADDR,
                                 (g_D_MEM_ADDR + (l_tile_row_start *
                                                  g_D_BYTES_PER_TILE_ROW))))
        }
        .elsewhen (l_did_col_incr) {
          loop1_A_mem_addr := loop1_A_mem_addr + 0.U
          loop1_B_mem_addr := loop1_B_mem_addr + g_I_BYTE_COLS_PER_GROUP
          loop1_C_mem_addr := loop1_C_mem_addr + g_I_BYTE_COLS_PER_GROUP
          loop1_D_mem_addr := loop1_D_mem_addr +
                              Mux(!g_HAS_BIAS, 0.U, g_O_BYTE_COLS_PER_GROUP)
        }

        // update next state
        state := s_RESET_A_TILE_SUBCOL
      }
    }
  }
}

object TilerFSM {
  def apply[T <: Data: Arithmetic, U <: Data, V <: Data]
    (config: GemminiArrayConfig[T,U,V])(implicit p: Parameters)
      = Module(new TilerFSM(config))
}

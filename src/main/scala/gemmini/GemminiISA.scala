
package gemmini

import chisel3._

object GemminiISA {
  // funct values
  val CONFIG_CMD = 0.U
  val LOAD2_CMD = 1.U
  val LOAD_CMD = 2.U
  val STORE_CMD = 3.U
  val COMPUTE_AND_FLIP_CMD = 4.U
  val COMPUTE_AND_STAY_CMD = 5.U
  val PRELOAD_CMD = 6.U
  val FLUSH_CMD = 7.U

  val LOOP_WS = 8.U
  val LOOP_WS_CONFIG_BOUNDS = 9.U
  val LOOP_WS_CONFIG_ADDRS_AB = 10.U
  val LOOP_WS_CONFIG_ADDRS_DC = 11.U
  val LOOP_WS_CONFIG_STRIDES_AB = 12.U
  val LOOP_WS_CONFIG_STRIDES_DC = 13.U

  val LOAD3_CMD = 14.U

  // TODO add orows and ocols to this as well
  val LOOP_CONV_WS = 15.U // no_bias, wrot180, trans_output_1203, trans_weight_1203, trans_input_3120, max_pixels_per_row | no_pool, downsample, input_dilated, act
  val LOOP_CONV_WS_CONFIG_1 = 16.U // batch_size, in_dim, in_channels, out_channels | out_dim, pool_out_dim, stride, padding
  val LOOP_CONV_WS_CONFIG_2 = 17.U // kernel_dim, pool_size, pool_stride, pool_padding | batches, porows, pocols, pochs
  val LOOP_CONV_WS_CONFIG_3 = 18.U // krows, kcols, kchs, lpad | rpad, upad, dpad, plpad
  val LOOP_CONV_WS_CONFIG_4 = 19.U // prad, pupad, pdpad, orows | ocols, kernel_dilation
  val LOOP_CONV_WS_CONFIG_5 = 20.U // *weights | *output
  val LOOP_CONV_WS_CONFIG_6 = 21.U // *bias, *input

  val CLKGATE_EN = 22.U

  // rs1[2:0] values
  val CONFIG_EX = 0.U
  val CONFIG_LOAD = 1.U
  val CONFIG_STORE = 2.U
  val CONFIG_IM2COL = 3.U

  //==========================================================================
  // cisc-gemmini opcodes
  //==========================================================================
  // TODO the numbers here overlap with the LOOP_WS commands
  val CISC_CONFIG  = 10.U(7.W) // same as COMPUTE_AND_FLIP
  val ADDR_AB      = 11.U(7.W)
  val ADDR_CD      = 12.U(7.W)
  val SIZE_MN      = 13.U(7.W)
  val SIZE_K       = 14.U(7.W)
  val RPT_BIAS     = 15.U(7.W)
  val RESET        = 16.U(7.W)
  val COMPUTE_CISC = 17.U(7.W)

  val COUNTER_OP   = 126.U(7.W)

  //==========================================================================
  // dataflow configuration
  //==========================================================================
  val GARBAGE_ADDR      = "hffffffff".U(32.W)

  val MVIN_RS2_ADDR_WIDTH = 32
  val MVIN_RS2_COLS_WIDTH = 16
  val MVIN_RS2_ROWS_WIDTH = 16

  class MvinRs2(mvin_rows_bits: Int, mvin_cols_bits: Int, local_addr_t: LocalAddr) extends Bundle {
    val _spacer2 = UInt((MVIN_RS2_ROWS_WIDTH - mvin_rows_bits).W)
    val num_rows = UInt(mvin_rows_bits.W)
    val _spacer1 = UInt((MVIN_RS2_COLS_WIDTH - mvin_cols_bits).W)
    val num_cols = UInt(mvin_cols_bits.W)
    val _spacer0 = UInt((MVIN_RS2_ADDR_WIDTH - local_addr_t.getWidth).W)
    val local_addr = local_addr_t.cloneType
  }

  val MVOUT_RS2_ADDR_WIDTH = 32
  val MVOUT_RS2_COLS_WIDTH = 16
  val MVOUT_RS2_ROWS_WIDTH = 16

  class MvoutRs2(mvout_rows_bits: Int, mvout_cols_bits: Int, local_addr_t: LocalAddr) extends Bundle {
    val _spacer2 = UInt((MVOUT_RS2_ROWS_WIDTH - mvout_rows_bits).W)
    val num_rows = UInt(mvout_rows_bits.W)
    val _spacer1 = UInt((MVOUT_RS2_COLS_WIDTH - mvout_cols_bits).W)
    val num_cols = UInt(mvout_cols_bits.W)
    val _spacer0 = UInt((MVOUT_RS2_ADDR_WIDTH - local_addr_t.getWidth).W)
    val local_addr = local_addr_t.cloneType
  }

  val CONFIG_MVIN_RS1_UNUSED_WIDTH = 2
  val CONFIG_MVIN_RS1_SHRINK_WIDTH = 1
  val CONFIG_MVIN_RS1_STATE_ID_WIDTH = 2
  val CONFIG_MVIN_RS1_SPACER_WIDTH = 8 - 2 - 1 - 2
  val CONFIG_MVIN_RS1_PIXEL_REPEAT_WIDTH = 8
  val CONFIG_MVIN_RS1_STRIDE_WIDTH = 16
  val CONFIG_MVIN_RS1_SCALE_WIDTH = 32

  class ConfigMvinRs1(scale_bits: Int, stride_bits: Int, pixel_repeat_bits: Int) extends Bundle {
    val _spacer3 = UInt((CONFIG_MVIN_RS1_SCALE_WIDTH - scale_bits).W)
    val scale = UInt(scale_bits.W)
    val _spacer2 = UInt((CONFIG_MVIN_RS1_STRIDE_WIDTH - stride_bits).W)
    val stride = UInt(stride_bits.W)
    val _spacer1 = UInt((CONFIG_MVIN_RS1_PIXEL_REPEAT_WIDTH - pixel_repeat_bits).W)
    val pixel_repeats = UInt(pixel_repeat_bits.W)
    val _spacer0 = UInt(CONFIG_MVIN_RS1_SPACER_WIDTH.W)
    val state_id = UInt(CONFIG_MVIN_RS1_STATE_ID_WIDTH.W)
    val shrink = UInt(CONFIG_MVIN_RS1_SHRINK_WIDTH.W)
    val _unused = UInt(CONFIG_MVIN_RS1_UNUSED_WIDTH.W)
  }

  val CONFIG_MVOUT_RS1_UNUSED_WIDTH = 2
  val CONFIG_MVOUT_RS1_ACTIVATION_WIDTH = 2
  val CONFIG_MVOUT_RS1_MAX_POOLING_STRIDE_WIDTH = 2
  val CONFIG_MVOUT_RS1_MAX_POOLING_WINDOW_SIZE_WIDTH = 2
  val CONFIG_MVOUT_RS1_UPPER_ZERO_PADDING_WIDTH = 2
  val CONFIG_MVOUT_RS1_LEFT_ZERO_PADDING_WIDTH = 2
  val CONFIG_MVOUT_RS1_SPACER_WIDTH = (24 - 2 * 6)
  val CONFIG_MVOUT_RS1_POOL_OUT_DIM_WIDTH = 8
  val CONFIG_MVOUT_RS1_POOL_OUT_ROWS_WIDTH = 8
  val CONFIG_MVOUT_RS1_POOL_OUT_COLS_WIDTH = 8
  val CONFIG_MVOUT_RS1_OUT_ROWS_WIDTH = 8
  val CONFIG_MVOUT_RS1_OUT_COLS_WIDTH = 8

  class ConfigMvoutRs1 extends Bundle {
    val ocols = UInt(CONFIG_MVOUT_RS1_OUT_COLS_WIDTH.W)
    val orows = UInt(CONFIG_MVOUT_RS1_OUT_ROWS_WIDTH.W)
    val pocols = UInt(CONFIG_MVOUT_RS1_POOL_OUT_COLS_WIDTH.W)
    val porows = UInt(CONFIG_MVOUT_RS1_POOL_OUT_ROWS_WIDTH.W)
    val pool_out_dim = UInt(CONFIG_MVOUT_RS1_POOL_OUT_DIM_WIDTH.W)
    val _spacer = UInt(CONFIG_MVOUT_RS1_SPACER_WIDTH.W)
    val lpad = UInt(CONFIG_MVOUT_RS1_LEFT_ZERO_PADDING_WIDTH.W)
    val upad = UInt(CONFIG_MVOUT_RS1_UPPER_ZERO_PADDING_WIDTH.W)
    val pool_size = UInt(CONFIG_MVOUT_RS1_MAX_POOLING_WINDOW_SIZE_WIDTH.W)
    val pool_stride = UInt(CONFIG_MVOUT_RS1_MAX_POOLING_STRIDE_WIDTH.W)
    val activation = UInt(CONFIG_MVOUT_RS1_ACTIVATION_WIDTH.W)
    val _unused = UInt(CONFIG_MVOUT_RS1_UNUSED_WIDTH.W)
  }

  val CONFIG_MVOUT_RS2_ACC_SCALE_WIDTH = 32
  val CONFIG_MVOUT_RS2_STRIDE_WIDTH = 32

  class ConfigMvoutRs2(acc_scale_bits: Int, stride_bits: Int) extends Bundle {
    val _spacer1 = UInt((CONFIG_MVOUT_RS2_ACC_SCALE_WIDTH - acc_scale_bits).W)
    val acc_scale = UInt(acc_scale_bits.W)
    val _spacer0 = UInt((CONFIG_MVOUT_RS2_STRIDE_WIDTH - stride_bits).W)
    val stride = UInt(stride_bits.W)
  }

  val CONFIG_EX_RS1_CMD_TYPE_WIDTH = 2
  val CONFIG_EX_RS1_DATAFLOW_WIDTH = 1
  val CONFIG_EX_RS1_ACTIVATION_WIDTH = 2
  val CONFIG_EX_RS1_SPACER0_WIDTH = (7 - 2 - 1 - 2)
  val CONFIG_EX_RS1_SET_ONLY_STRIDES_WIDTH = 1
  val CONFIG_EX_RS1_A_TRANSPOSE_WIDTH = 1
  val CONFIG_EX_RS1_B_TRANSPOSE_WIDTH = 1
  val CONFIG_EX_RS1_SPACER1_WIDTH = (16 - 10)
  val CONFIG_EX_RS1_A_STRIDE_WIDTH = 16
  val CONFIG_EX_RS1_ACC_SCALE_WIDTH = 32

  class ConfigExRs1(acc_scale_bits: Int) extends Bundle {
    val _spacer2 = UInt((CONFIG_EX_RS1_ACC_SCALE_WIDTH - acc_scale_bits).W)
    val acc_scale = UInt(acc_scale_bits.W)
    val a_stride = UInt(CONFIG_EX_RS1_A_STRIDE_WIDTH.W)
    val _spacer1 = UInt(CONFIG_EX_RS1_SPACER1_WIDTH.W)
    val b_transpose = UInt(CONFIG_EX_RS1_B_TRANSPOSE_WIDTH.W)
    val a_transpose = UInt(CONFIG_EX_RS1_A_TRANSPOSE_WIDTH.W)
    val set_only_strides = UInt(CONFIG_EX_RS1_SET_ONLY_STRIDES_WIDTH.W)
    val _spacer0 = UInt(CONFIG_EX_RS1_SPACER0_WIDTH.W)
    val activation = UInt(CONFIG_EX_RS1_ACTIVATION_WIDTH.W)
    val dataflow = UInt(CONFIG_EX_RS1_DATAFLOW_WIDTH.W)
    val cmd_type = UInt(CONFIG_EX_RS1_CMD_TYPE_WIDTH.W)
  }

  val CONFIG_EX_RS2_IN_SHIFT_WIDTH = 32
  val CONFIG_EX_RS2_RELU6_SHIFT_WIDTH = 16
  val CONFIG_EX_RS2_C_STRIDE_WIDTH = 16

  class ConfigExRs2 extends Bundle {
    val c_stride = UInt(CONFIG_EX_RS2_C_STRIDE_WIDTH.W)
    val relu6_shift = UInt(CONFIG_EX_RS2_RELU6_SHIFT_WIDTH.W)
    val in_shift = UInt(CONFIG_EX_RS2_IN_SHIFT_WIDTH.W)
  }

  val PRELOAD_RS_ADDR_WIDTH = 32
  val PRELOAD_RS_COLS_WIDTH = 16
  val PRELOAD_RS_ROWS_WIDTH = 16

  class PreloadRs(preload_rows_bits: Int, preload_cols_bits: Int, local_addr_t: LocalAddr) extends Bundle {
    val _spacer2 = UInt((PRELOAD_RS_ROWS_WIDTH - preload_rows_bits).W)
    val num_rows = UInt(preload_rows_bits.W)
    val _spacer1 = UInt((PRELOAD_RS_COLS_WIDTH - preload_cols_bits).W)
    val num_cols = UInt(preload_cols_bits.W)
    val _spacer0 = UInt((PRELOAD_RS_ADDR_WIDTH - local_addr_t.getWidth).W)
    val local_addr = local_addr_t.cloneType
  }

  val COMPUTED_RS_ADDR_WIDTH = 32
  val COMPUTED_RS_COLS_WIDTH = 16
  val COMPUTED_RS_ROWS_WIDTH = 16

  class ComputeRs(compute_rows_bits: Int, compute_cols_bits: Int, local_addr_t: LocalAddr) extends Bundle {
    val _spacer2 = UInt((COMPUTED_RS_ROWS_WIDTH - compute_rows_bits).W)
    val num_rows = UInt(compute_rows_bits.W)
    val _spacer1 = UInt((COMPUTED_RS_COLS_WIDTH - compute_cols_bits).W)
    val num_cols = UInt(compute_cols_bits.W)
    val _spacer0 = UInt((COMPUTED_RS_ADDR_WIDTH - local_addr_t.getWidth).W)
    val local_addr = local_addr_t.cloneType
  }
}


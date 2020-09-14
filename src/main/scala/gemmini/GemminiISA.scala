package gemmini

import chisel3._

object GemminiISA {
  // funct values
  val CONFIG_CMD = 0.U
  val LOAD_CMD = 2.U
  val STORE_CMD = 3.U
  val COMPUTE_AND_FLIP_CMD = 4.U
  val COMPUTE_AND_STAY_CMD = 5.U
  val PRELOAD_CMD = 6.U
  val FLUSH_CMD = 7.U
  val LOOP_WS = 8.U

  val LOOP_MATMUL_1 = 8.U // Sets *A and *B
  val LOOP_MATMUL_2 = 9.U // Sets *C and *D
  val LOOP_MATMUL_3 = 10.U // Sets I, J, K, pad_I, pad_J, and pad_K, no_bias, repeating_bias
  val LOOP_MATMUL_4 = 11.U // Sets A_row_stride, B_row_stride
  val LOOP_MATMUL_5 = 12.U // Sets C_row_stride, D_row_stride
  val LOOP_MATMUL = 13.U // Start matmul

  // rs1[2:0] values
  val CONFIG_LOAD = 1.U
  val CONFIG_STORE = 2.U
  val CONFIG_EX = 0.U
}

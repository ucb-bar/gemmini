package gemmini

import chisel3._

object Activation {
  val NONE = 0.U
  val RELU = 1.U
  val RELU6 = 2.U
}

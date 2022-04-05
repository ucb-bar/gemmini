package gemmini

import chisel3._

object Activation {
  val NONE = 0.U
  val RELU = 1.U
  val LAYERNORM_OR_SOFTMAX = 2.U
  val IGELU = 3.U
}

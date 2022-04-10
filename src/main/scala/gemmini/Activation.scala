package gemmini

import chisel3._

object Activation {
  val NONE = 0.U
  val RELU = 1.U
  val LAYERNORM = 2.U
  val IGELU = 3.U

  val bitwidth = 3
}

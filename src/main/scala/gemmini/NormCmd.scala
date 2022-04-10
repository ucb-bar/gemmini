package gemmini

import chisel3._
import chisel3.experimental.ChiselEnum

object NormCmd extends ChiselEnum {
  val RESET, SUM, MEAN, VARIANCE, INV_STDDEV = Value

  def writes_to_main_memory(cmd: Type): Bool = {
    cmd === RESET
  }
}

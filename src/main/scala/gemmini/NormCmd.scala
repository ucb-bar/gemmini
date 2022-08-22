
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object NormCmd extends ChiselEnum {
  val RESET, SUM, MEAN, VARIANCE, INV_STDDEV, MAX, SUM_EXP, INV_SUM_EXP = Value

  def writes_to_main_memory(cmd: Type): Bool = {
    cmd === RESET
  }

  def non_reset_version(cmd: Type): Type = {
    MuxCase(cmd, Seq(
      (cmd === MEAN) -> SUM,
      (cmd === MAX) -> MAX,
      (cmd === INV_STDDEV) -> VARIANCE,
      (cmd === INV_SUM_EXP) -> SUM_EXP
    ))
  }
}

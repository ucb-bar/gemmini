package gemmini

import chisel3._
import chisel3.util._

object DMAWriteCommandTracker {
  def apply[T <: Data](nCmds: Int, nRows: Int, tag_t: => T) = Module(new DMAReadCommandTracker(nCmds = nCmds,
    maxBytes = nRows, tag_t = tag_t))
}

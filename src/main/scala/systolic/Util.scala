
package systolic

import chisel3._

object Util {
  def wrappingAdd(u: UInt, n: UInt, max: Int): UInt = {
    assert(n <= max.U, "cannot wrapAdd when n is larger than max")
    Mux(u >= max.U - n + 1.U && n =/= 0.U, n - (max.U - u) - 1.U, u + n)
  }
}

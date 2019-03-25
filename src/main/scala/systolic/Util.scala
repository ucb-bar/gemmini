
package systolic

import chisel3._

object Util {
  def wrappingAdd(u: UInt, n: UInt, max_plus_one: Int): UInt = {
    val max = max_plus_one - 1
    assert(n <= max.U, "cannot wrapAdd when n is larger than max")
    Mux(u >= max.U - n + 1.U && n =/= 0.U, n - (max.U - u) - 1.U, u + n)
  }

  def wrappingSub(u: UInt, n: UInt, max_plus_one: Int): UInt = {
    val max = max_plus_one - 1
    assert(n <= max.U, "cannot wrapSub when n is larger than max")
    Mux(u < n, max.U - (n-u) + 1.U, u - n)
  }

  def maxOf(us: UInt*) = {
    us.reduce((acc, u) => Mux(acc > u, acc, u))
  }

  def minOf(us: UInt*) = {
    us.reduce((acc, u) => Mux(acc < u, acc, u))
  }
}

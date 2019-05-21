package systolic

import chisel3._
import chisel3.util._

object Util {
  def wrappingAdd(u: UInt, n: UInt, max_plus_one: Int): UInt = {
    val max = max_plus_one - 1
    if (max == 0) {
      0.U
    } else {
      assert(n <= max.U, "cannot wrapAdd when n is larger than max")
      Mux(u >= max.U - n + 1.U && n =/= 0.U, n - (max.U - u) - 1.U, u + n)
    }
  }

  def wrappingAdd(u: UInt, n: UInt, max_plus_one: UInt): UInt = {
    val max = max_plus_one - 1.U
    assert(n <= max, "cannot wrapAdd when n is larger than max")
    Mux(u >= max - n + 1.U && n =/= 0.U, n - (max - u) - 1.U, u + n)
  }

  def wrappingSub(u: UInt, n: UInt, max_plus_one: Int): UInt = {
    val max = max_plus_one - 1
    assert(n <= max.U, "cannot wrapSub when n is larger than max")
    Mux(u < n, max.U - (n-u) + 1.U, u - n)
  }

  def closestLowerPowerOf2(u: UInt): UInt = {
    // TODO figure out a more efficient way of doing this. Is this many muxes really necessary?
    val exp = u.toBools().zipWithIndex.map { case (b, i) =>
        Mux(b, i.U, 0.U)
    }.reduce((acc, u) => Mux(acc > u, acc, u))

    (1.U << exp).asUInt()
  }

  def closestAlignedLowerPowerOf2(u: UInt, addr: UInt, rowBytes: Int): UInt = {
    val lgRowBytes = log2Ceil(rowBytes)

    // TODO figure out a more efficient way of doing this. Is this many muxes really necessary?
    val exp = u.toBools().zipWithIndex.map { case (b, i) =>
      Mux(b && addr(i + lgRowBytes - 1, 0) === 0.U, i.U, 0.U)
    }.reduce((acc, u) => Mux(acc > u, acc, u))

    (1.U << exp).asUInt()
  }
}

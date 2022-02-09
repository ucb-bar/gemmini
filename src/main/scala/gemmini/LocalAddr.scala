package gemmini

import chisel3._
import chisel3.util._

class LocalAddr(sp_banks: Int, sp_bank_entries: Int, acc_banks: Int, acc_bank_entries: Int) extends Bundle {
  private val localAddrBits = 32 // TODO magic number

  private val spAddrBits = log2Ceil(sp_banks * sp_bank_entries)
  private val accAddrBits = log2Ceil(acc_banks * acc_bank_entries)
  private val maxAddrBits = spAddrBits max accAddrBits

  private val spBankBits = log2Up(sp_banks)
  private val spBankRowBits = log2Up(sp_bank_entries)

  private val accBankBits = log2Up(acc_banks)
  val accBankRowBits = log2Up(acc_bank_entries)

  val spRows = sp_banks * sp_bank_entries

  val is_acc_addr = Bool()
  val accumulate = Bool()
  val read_full_acc_row = Bool()
  val garbage = UInt(((localAddrBits - maxAddrBits - 4) max 0).W)
  val garbage_bit = if (localAddrBits - maxAddrBits >= 4) UInt(1.W) else UInt(0.W)
  val data = UInt(maxAddrBits.W)

  def sp_bank(dummy: Int = 0) = if (spAddrBits == spBankRowBits) 0.U else data(spAddrBits - 1, spBankRowBits)
  def sp_row(dummy: Int = 0) = data(spBankRowBits - 1, 0)
  def acc_bank(dummy: Int = 0) = if (accAddrBits == accBankRowBits) 0.U else data(accAddrBits - 1, accBankRowBits)
  def acc_row(dummy: Int = 0) = data(accBankRowBits - 1, 0)

  def full_sp_addr(dummy: Int = 0) = data(spAddrBits - 1, 0)
  def full_acc_addr(dummy: Int = 0) = data(accAddrBits - 1, 0)

  def is_same_address(other: LocalAddr): Bool = is_acc_addr === other.is_acc_addr && data === other.data
  def is_same_address(other: UInt): Bool = is_same_address(other.asTypeOf(this))
  def is_garbage(dummy: Int = 0) = is_acc_addr && accumulate && read_full_acc_row && data.andR() &&
    (if (garbage_bit.getWidth > 0) garbage_bit.asBool() else true.B)

  def +(other: UInt) = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val result = WireInit(this)
    result.data := data + other
    result
  }

  def <=(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() <= other.full_acc_addr(), full_sp_addr() <= other.full_sp_addr())

  def <(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() < other.full_acc_addr(), full_sp_addr() < other.full_sp_addr())

  def >(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() > other.full_acc_addr(), full_sp_addr() > other.full_sp_addr())

  def add_with_overflow(other: UInt): Tuple2[LocalAddr, Bool] = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val sum = data +& other

    val overflow = Mux(is_acc_addr, sum(accAddrBits), sum(spAddrBits))

    val result = WireInit(this)
    result.data := sum(maxAddrBits - 1, 0)

    (result, overflow)
  }

  // This function can only be used with non-accumulator addresses. Returns both new address and underflow
  def floorSub(other: UInt, floor: UInt): (LocalAddr, Bool) = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val underflow = data < (floor +& other)

    val result = WireInit(this)
    result.data := Mux(underflow, floor, data - other)

    (result, underflow)
  }

  def make_this_garbage(dummy: Int = 0): Unit = {
    is_acc_addr := true.B
    accumulate := true.B
    read_full_acc_row := true.B
    garbage_bit := 1.U
    data := ~(0.U(maxAddrBits.W))
  }

}

object LocalAddr {
  def cast_to_local_addr[T <: Data](local_addr_t: LocalAddr, t: T): LocalAddr = {
    // This convenience function is basically the same as calling "asTypeOf(local_addr_t)". However, this convenience
    // function will also cast unnecessary garbage bits to 0, which may help reduce multiplier/adder bitwidths
    val result = WireInit(t.asTypeOf(local_addr_t))
    if (result.garbage_bit.getWidth > 0) result.garbage := 0.U
    result
  }

  def cast_to_sp_addr[T <: Data](local_addr_t: LocalAddr, t: T): LocalAddr = {
    // This function is a wrapper around cast_to_local_addr, but it assumes that the input will not be the garbage
    // address
    val result = WireInit(cast_to_local_addr(local_addr_t, t))
    result.is_acc_addr := false.B
    result.accumulate := false.B
    result.read_full_acc_row := false.B

    // assert(!result.garbage_bit, "cast_to_sp_addr doesn't work on garbage addresses")

    result
  }

  def cast_to_acc_addr[T <: Data](local_addr_t: LocalAddr, t: T, accumulate: Bool, read_full: Bool): LocalAddr = {
    // This function is a wrapper around cast_to_local_addr, but it assumes that the input will not be the garbage
    // address
    val result = WireInit(cast_to_local_addr(local_addr_t, t))
    result.is_acc_addr := true.B
    result.accumulate := accumulate
    result.read_full_acc_row := read_full

    // assert(!result.garbage_bit, "cast_to_acc_addr doesn't work on garbage addresses")

    result
  }

  def garbage_addr(local_addr_t: LocalAddr): LocalAddr = {
    val result = Wire(chiselTypeOf(local_addr_t))
    result := DontCare
    result.make_this_garbage()
    result
  }
}

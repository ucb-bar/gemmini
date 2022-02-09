package gemmini

import chisel3._
import chisel3.util._
import gemmini.Util.UDValid
import midas.targetutils.SynthesizePrintf

class XactTrackerEntry[U <: Data](maxShift: Int, spadWidth: Int, accWidth: Int,
                                  spadRows: Int, accRows: Int, maxReqBytes: Int, mvin_scale_t_bits: Int,
                                  nCmds: Int) extends Bundle {
  val shift = UInt(log2Up(maxShift).W)
  val addr = UInt(log2Up(spadRows max accRows).W)
  val is_acc = Bool()
  val accumulate = Bool()
  val has_acc_bitwidth = Bool()
  val scale = UInt(mvin_scale_t_bits.W)
  val repeats = UInt(16.W) // TODO magic number
  val pixel_repeats = UInt(8.W) // TODO magic number
  val len = UInt(16.W) // TODO magic number
  val block_stride = UInt(16.W) // TODO magic number
  val spad_row_offset = UInt(log2Up(spadWidth max accWidth).W)
  val lg_len_req = UInt(log2Up(log2Up(maxReqBytes+1)+1).W)
  val bytes_to_read = UInt(log2Up(maxReqBytes+1).W)
  val cmd_id = UInt(log2Up(nCmds).W)

}

class XactTrackerAllocIO[U <: Data](nXacts: Int, maxShift: Int, spadWidth: Int, accWidth :Int,
                                    spadRows: Int, accRows: Int, maxReqBytes: Int, mvin_scale_t_bits: Int, nCmds: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())

  val xactid = Input(UInt(log2Up(nXacts).W))
  val entry = Output(new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds))

  def fire(dummy: Int = 0) = valid && ready

}

class XactTrackerPeekIO[U <: Data](val nXacts: Int, val maxShift: Int, val spadWidth: Int, val accWidth: Int,
                                   val spadRows: Int, val accRows: Int, val maxReqBytes: Int, mvin_scale_t_bits: Int, nCmds: Int)
  extends Bundle {
  val xactid = Input(UInt(log2Up(nXacts).W))
  val pop = Input(Bool())
  val entry = Output(new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds))
}

/*
  maxShift: the maximum number of bytes in the beginning of a TileLink response which may be discarded
  spadWidth: the width of an spad row in bytes
  spadRows: the total number of rows in the spad
  maxReqBytes:
  Removed:
    maxMatrices: the maximum number of rows from different matrices which can be packed into one request
 */
class XactTracker[U <: Data](nXacts: Int, maxShift: Int, spadWidth: Int, accWidth: Int,
                             spadRows: Int, accRows: Int, maxReqBytes: Int, mvin_scale_t_bits: Int, nCmds: Int,
                             use_firesim_simulation_counters: Boolean) extends Module {
  val io = IO(new Bundle {
    val alloc = Flipped(new XactTrackerAllocIO(nXacts, maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds))
    val peek = new XactTrackerPeekIO(nXacts, maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds)
    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  val entries = Reg(Vec(nXacts, UDValid(new XactTrackerEntry(maxShift, spadWidth, accWidth, spadRows, accRows, maxReqBytes, mvin_scale_t_bits, nCmds))))

  val free_entry = MuxCase((nXacts-1).U, entries.zipWithIndex.map { case (e, i) => !e.valid -> i.U })
  io.alloc.ready := !entries.map(_.valid).reduce(_ && _)
  io.alloc.xactid := free_entry

  io.peek.entry := entries(io.peek.xactid).bits

  io.busy := entries.map(_.valid).reduce(_ || _)

  when (io.alloc.fire()) {
    entries(free_entry).valid := true.B
    entries(free_entry).bits := io.alloc.entry
  }

  when (io.peek.pop) {
    entries(io.peek.xactid).valid := false.B
    assert(entries(io.peek.xactid).valid)
  }

  when (reset.asBool()) {
    entries.foreach(_.valid := false.B)
  }

  // Performance counters
  CounterEventIO.init(io.counter)

  val total_latency = RegInit(0.U(CounterExternal.EXTERNAL_WIDTH.W))
  when (io.counter.external_reset) {
    total_latency := 0.U
  }.otherwise {
    total_latency := total_latency + PopCount(entries.map(_.valid))
  }

  io.counter.connectExternalCounter(CounterExternal.RDMA_TOTAL_LATENCY, total_latency)

  if (use_firesim_simulation_counters) {
    val cntr = Counter(500000)
    when(cntr.inc()) {
      printf(SynthesizePrintf("RDMA total latency: %d\n", total_latency))
    }
  }
}

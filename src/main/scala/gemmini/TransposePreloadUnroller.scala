package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chipsalliance.rocketchip.config.Parameters
import Util._
import midas.targetutils.PerfCounter

class TransposePreloadUnroller[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
                                                                 (implicit p: Parameters) extends Module {
  import config._
  import GemminiISA._

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(reservation_station_entries)))
    val out = Decoupled(new GemminiCmd(reservation_station_entries))
    val counter = new CounterEventIO()
  })

  object State extends ChiselEnum {
    val idle = Value
    val first_compute, second_preload = Value
  }
  import State._
  val state = RegInit(idle)

  val garbage_addr = ~0.U(32.W)

  val (q, len)  = MultiHeadedQueue(io.in, entries=2, heads=2, maxpop = 1)

  val cmds = q.bits
  val valids = q.valid
  val functs = cmds.map(_.cmd.inst.funct)

  val first_preload = valids(0) && functs(0) === PRELOAD_CMD && state === idle

  val b_transposed_and_ws = Reg(Bool())
  val unroll_preload = b_transposed_and_ws && valids(1) && functs(1) === COMPUTE_AND_FLIP_CMD

  val first_preload_cmd = WireInit(cmds(0))
  first_preload_cmd.cmd.rs2 := Cat(cmds(0).cmd.rs2(63, 32), garbage_addr)
  first_preload_cmd.rob_id.valid := false.B

  val first_compute_cmd = WireInit(cmds(1))
  first_compute_cmd.cmd.inst.rs1 := Cat(cmds(1).cmd.rs1(63, 32), garbage_addr)
  first_compute_cmd.cmd.inst.rs2 := Cat(cmds(1).cmd.rs2(63, 32), garbage_addr)
  first_compute_cmd.cmd.inst.funct := COMPUTE_AND_STAY_CMD
  first_compute_cmd.rob_id.valid := false.B

  val second_preload_cmd = WireInit(cmds(0))
  second_preload_cmd.cmd.rs1 := Cat(cmds(0).cmd.rs1(63, 32), garbage_addr)

  val config_cmd_type = cmds(0).cmd.rs1(1,0) // TODO magic numbers
  val is_config = functs(0) === CONFIG_CMD && config_cmd_type === CONFIG_EX

  io.out.valid := MuxCase(valids(0), Seq(
    first_preload -> (!b_transposed_and_ws || valids(1)),
    (state > first_compute) -> true.B
  ))

  io.out.bits := MuxCase(cmds(0), Seq(
    (first_preload && unroll_preload) -> first_preload_cmd,
    (state === first_compute) -> first_compute_cmd,
    (state === second_preload) -> second_preload_cmd,
  ))

  q.pop := Mux(io.out.fire && !(first_preload && unroll_preload) && state =/= first_compute, 1.U, 0.U)

  when (io.out.fire) {
    when (is_config) {
      val set_only_strides = cmds(0).cmd.rs1(7)
      when (!set_only_strides) {
        b_transposed_and_ws := ((dataflow == Dataflow.WS).B || cmds(0).cmd.rs1(2) === Dataflow.WS.id.U) && cmds(0).cmd.rs1(9)
      }
    }.elsewhen (first_preload && unroll_preload) {
      state := first_compute
    }.elsewhen (state >= first_compute) {
      state := state.next
    }
  }

  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.TRANSPOSE_PRELOAD_UNROLLER_ACTIVE_CYCLES, state =/= idle)
}

object TransposePreloadUnroller {
  def apply[T <: Data, U <: Data, V <: Data](in: ReadyValidIO[GemminiCmd], config: GemminiArrayConfig[T, U, V], counter: CounterEventIO)(implicit p: Parameters): DecoupledIO[GemminiCmd] = {
    val mod = Module(new TransposePreloadUnroller(config))
    mod.io.in <> in
    counter.collect(mod.io.counter)
    mod.io.out
  }
}

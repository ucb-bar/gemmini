package gemmini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._

class NormalizedInput[T <: Data: Arithmetic, U <: Data](max_len: Int, num_stats: Int, fullDataType: Vec[Vec[T]],
                                                        scale_t: U) extends Bundle {
  val acc_read_resp = new AccumulatorReadResp[T,U](fullDataType, scale_t)
  val len = UInt(log2Up(max_len+1).W)
  val stats_id = UInt(log2Up(num_stats).W)
  val cmd = NormCmd()
}

class NormalizedOutput[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val acc_read_resp = new AccumulatorReadResp[T,U](fullDataType, scale_t)
  val mean = fullDataType.head.head.cloneType
  val inv_stddev = scale_t.cloneType
}

class AccumulationLanes[T <: Data](num_stats: Int, acc_t: T, n_lanes: Int, latency: Int)(implicit ev: Arithmetic[T]) extends Module {
  // Each lane computes a sum, or an error-squared sum

  import ev._
  import NormCmd._

  class LaneOutput extends Bundle {
    val result = acc_t.cloneType
    val stats_id = UInt(log2Up(num_stats).W)
  }

  val io = IO(new Bundle {
    val ins = Flipped(Valid(new Bundle {
      val len = UInt(log2Up(n_lanes+1).W)
      val data = Vec(n_lanes, acc_t)
      val mean = acc_t.cloneType
      val cmd = NormCmd()
      val stats_id = UInt(log2Up(num_stats).W)
    }))

    val out = Valid(new LaneOutput)

    val busy = Output(Bool())
  })

  val cmd = io.ins.bits.cmd
  val mean = io.ins.bits.mean

  val data = io.ins.bits.data.zipWithIndex.map { case (d, i) =>
    Mux(i.U < io.ins.bits.len,
      Mux(cmd === NormCmd.VARIANCE || cmd === NormCmd.INV_STDDEV, (d-mean)*(d-mean), d).withWidthOf(acc_t),
      d.zero)
  }

  val result = data.reduce(_ + _)

  val pipe = Module(new Pipeline[LaneOutput](new LaneOutput, latency)())

  pipe.io.in.valid := io.ins.valid
  // io.ins.ready := pipe.io.in.ready
  pipe.io.in.bits.result := result
  pipe.io.in.bits.stats_id := io.ins.bits.stats_id

  io.out.valid := pipe.io.out.valid
  pipe.io.out.ready := true.B
  // pipe.io.out.ready := io.out.ready
  io.out.bits := pipe.io.out.bits

  io.busy := pipe.io.busy
}

class Normalizer[T <: Data, U <: Data](max_len: Int, num_reduce_lanes: Int, num_stats: Int, latency: Int,
                                       fullDataType: Vec[Vec[T]], scale_t: U)
                                      (implicit ev: Arithmetic[T]) extends Module {
  import ev._
  val acc_t = fullDataType.head.head.cloneType
  val vec_size = fullDataType.flatten.size
  val n_lanes = if (num_reduce_lanes < 0) vec_size else num_reduce_lanes

  assert(isPow2(n_lanes))

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new NormalizedInput[T,U](max_len, num_stats, fullDataType, scale_t)))
    val out = Decoupled(new NormalizedOutput(fullDataType, scale_t))
  })

  object State extends ChiselEnum {
    // NOTE: We assume that "idle" and "output" are the first two states. We also assume that all the enums on the same
    //   line keep the order below
    val idle, output = Value
    val get_sum = Value
    val get_mean, waiting_for_mean = Value
    val get_variance, waiting_for_variance, get_stddev, waiting_for_stddev, get_inv_stddev, waiting_for_inv_stddev = Value
  }
  import State._

  // Buffers for normalization stats
  class Stats extends Bundle {
    val req = new NormalizedInput[T,U](max_len, num_stats, fullDataType, scale_t)
    val state = State()

    // Running state
    val sum = acc_t.cloneType
    val count = UInt(16.W) // TODO magic number

    // Iterative state
    val mean = acc_t.cloneType
    val inv_stddev = acc_t.cloneType

    val elems_left = req.len.cloneType

    def vec_grouped = VecInit(req.acc_read_resp.data.flatten.grouped(n_lanes).map(v => VecInit(v)).toSeq)
    def vec_groups_left = elems_left / n_lanes.U + (elems_left % n_lanes.U =/= 0.U)

    def cmd = req.cmd

    def waiting_for_lanes_to_drain =
      (cmd === NormCmd.MEAN && (state === get_sum || state === get_mean)) ||
        (cmd === NormCmd.INV_STDDEV && (state === get_sum || state === get_variance))
  }

  val stats = Reg(Vec(num_stats, new Stats))
  val done_with_functional_units = Wire(Vec(num_stats, Bool()))
  val next_states = Wire(Vec(num_stats, State()))

  (stats.map(_.state) zip next_states).foreach { case (s, ns) => s := ns }

  // IO
  val in_stats_id = io.in.bits.stats_id
  io.in.ready := (stats(in_stats_id).state === idle || done_with_functional_units(in_stats_id)) &&
    stats.map(!_.waiting_for_lanes_to_drain).reduce(_ && _)

  val out_stats_id = MuxCase((num_stats-1).U,
    stats.zipWithIndex.map { case (s,i) => (s.state === output) -> i.U }
  )

  io.out.valid := stats(out_stats_id).state === output
  io.out.bits.acc_read_resp := stats(out_stats_id).req.acc_read_resp
  io.out.bits.mean := stats(out_stats_id).mean
  io.out.bits.inv_stddev := stats(out_stats_id).inv_stddev.asTypeOf(scale_t)

  // Lanes and functional units
  val lanes = Module(new AccumulationLanes(num_stats, acc_t, n_lanes, latency))

  {
    // Lanes input
    val in_lanes_stats_id = MuxCase((num_stats-1).U,
      stats.zipWithIndex.map { case (s,i) => (s.state === get_sum) -> i.U }
    )

    val stat = stats(in_lanes_stats_id)

    val len = Mux(stat.elems_left % n_lanes.U === 0.U, n_lanes.U, stat.elems_left % n_lanes.U)

    lanes.io.ins.valid := stat.state === get_sum && stat.vec_groups_left > 0.U
    lanes.io.ins.bits.data := stat.vec_grouped(stat.vec_groups_left-1.U)
    lanes.io.ins.bits.mean := stat.mean
    lanes.io.ins.bits.cmd := stat.cmd
    lanes.io.ins.bits.len := len
    lanes.io.ins.bits.stats_id := in_lanes_stats_id

    when (lanes.io.ins.fire()) {
      stat.elems_left := stat.elems_left - len
    }
  }

  {
    // Lanes output
    val out_lanes_stats_id = lanes.io.out.bits.stats_id

    val stat = stats(out_lanes_stats_id)

    when (lanes.io.out.fire()) {
      stat.sum := stat.sum + lanes.io.out.bits.result
    }
  }

  val sum_to_divide_id = MuxCase((num_stats-1).U,
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_mean || s.state === get_variance) -> i.U }
  )
  val sum_to_divide = stats(sum_to_divide_id).sum
  val (divider_in, divider_out) = sum_to_divide.divider(stats.head.count).get

  {
    // Divider input
    val stat = stats(sum_to_divide_id)

    divider_in.valid := (stat.state === get_mean || stat.state === get_variance) && !lanes.io.busy
    divider_in.bits := stat.count
  }

  {
    // Divider output
    val waiting_for_divide_id = MuxCase((num_stats-1).U,
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_mean || s.state === waiting_for_variance) -> i.U }
    )
    val stat = stats(waiting_for_divide_id)

    divider_out.ready := stat.state === waiting_for_mean || stat.state === waiting_for_variance

    when(stat.state === waiting_for_mean) {
      stat.mean := divider_out.bits
    }.elsewhen(stat.state === waiting_for_variance) {
      stat.inv_stddev := divider_out.bits
    }
  }

  val variance_to_sqrt_id = MuxCase((num_stats-1).U,
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_stddev) -> i.U }
  )
  val variance_to_sqrt = stats(variance_to_sqrt_id).inv_stddev
  val (sqrt_in, sqrt_out) = variance_to_sqrt.sqrt.get

  {
    // Sqrt input
    val stat = stats(variance_to_sqrt_id)

    sqrt_in.valid := stat.state === get_stddev
  }

  {
    // Sqrt output
    val waiting_for_sqrt_id = MuxCase((num_stats-1).U,
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_stddev) -> i.U }
    )
    val stat = stats(waiting_for_sqrt_id)

    sqrt_out.ready := stat.state === waiting_for_stddev

    // TODO this fallback for stddev === 0 only works if acc_t is an SInt
    assert(acc_t.isInstanceOf[SInt])

    when (stat.state === waiting_for_stddev) {
      stat.inv_stddev := Mux(sqrt_out.bits.asUInt() === acc_t.zero.asUInt(),
        1.S(acc_t.getWidth.W).asTypeOf(acc_t),
        sqrt_out.bits
      )
    }
  }

  val stddev_to_inv_id = MuxCase((num_stats-1).U,
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_inv_stddev) -> i.U }
  )
  val stddev_to_inv = stats(stddev_to_inv_id).inv_stddev
  val (reciprocal_in, reciprocal_out) = stddev_to_inv.reciprocal(scale_t).get

  {
    // Reciprocal input
    val stat = stats(stddev_to_inv_id)

    reciprocal_in.valid := stat.state === get_inv_stddev
    reciprocal_in.bits := DontCare
  }

  {
    // Reciprocal output
    val waiting_for_reciprocal_id = MuxCase((num_stats-1).U,
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_inv_stddev) -> i.U }
    )
    val stat = stats(waiting_for_reciprocal_id)

    reciprocal_out.ready := stat.state === waiting_for_inv_stddev

    when (stat.state === waiting_for_inv_stddev) {
      stat.inv_stddev := reciprocal_out.bits.asTypeOf(stat.inv_stddev)
    }
  }

  // State transitions
  for (((stat, next_state), id) <- (stats zip next_states).zipWithIndex) {
    val state = stat.state
    val cmd = stat.cmd

    val done = done_with_functional_units(id)

    when (state === idle) {
      // We have a different "when" statement below to support the case where a new row is input into the normalizer
      next_state := idle
      done := DontCare
    }.elsewhen(state === output) {
      next_state := Mux(io.out.fire() && out_stats_id === id.U, idle, state)
      done := io.out.fire() && out_stats_id === id.U
    }.elsewhen(state === get_sum) {
      val is_last_lane_input = stat.vec_groups_left === 0.U ||
        (stat.vec_groups_left === 1.U &&
          lanes.io.ins.bits.stats_id === id.U &&
          lanes.io.ins.fire())

      next_state := Mux(cmd === NormCmd.SUM || cmd === NormCmd.VARIANCE,
        Mux(is_last_lane_input, idle, state),
        Mux(is_last_lane_input,
          Mux(cmd === NormCmd.MEAN, get_mean, get_variance),
          state)
      )

      done := is_last_lane_input && cmd =/= NormCmd.MEAN && cmd =/= NormCmd.INV_STDDEV
    }.elsewhen(state === get_mean || state === get_variance) {
      next_state := Mux(divider_in.fire() && sum_to_divide_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_mean) {
      next_state := Mux(divider_out.fire(), idle, state)
      done := divider_out.fire()
    }.elsewhen(state === waiting_for_variance) {
      next_state := Mux(divider_out.fire(), get_stddev, state)
      done := false.B
    }.elsewhen(state === get_stddev) {
      next_state := Mux(sqrt_in.fire() && variance_to_sqrt_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_stddev) {
      next_state := Mux(sqrt_out.fire(), state.next, state)
      done := false.B
    }.elsewhen(state === get_inv_stddev) {
      next_state := Mux(reciprocal_in.fire() && stddev_to_inv_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_inv_stddev) {
      next_state := Mux(reciprocal_out.fire(), idle, state)
      done := reciprocal_out.fire()
    }.otherwise {
      assert(false.B, "invalid state in Normalizer")
      next_state := DontCare
      done := DontCare
    }

    when (io.in.fire() && in_stats_id === id.U) {
      next_state := Mux(io.in.bits.cmd === NormCmd.RESET, output, get_sum)
    }
  }

  // Update stats variables
  for (((stat, next_state), id) <- (stats zip next_states).zipWithIndex) {
    val state = stat.state

    val reset_running_state =
      state === output ||
        (state === get_mean && next_state =/= get_mean) ||
        (state === get_variance && next_state =/= get_variance)

    val is_input = io.in.fire() && in_stats_id === id.U

    when (is_input) {
      stat.req := io.in.bits
      stat.count := stat.count + io.in.bits.len
      stat.elems_left := io.in.bits.len
    }

    when(reset_running_state) {
      stat.sum := acc_t.zero
      stat.count := Mux(is_input, io.in.bits.len, 0.U)
    }
  }

  dontTouch(stats)

  // Assertions
  assert(PopCount(stats.map(s => s.state === waiting_for_mean || s.state === waiting_for_variance)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  assert(PopCount(stats.map(_.state === waiting_for_stddev)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  assert(PopCount(stats.map(_.state === waiting_for_inv_stddev)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  assert(PopCount(stats.map(_.state === output)) <= 1.U, "multiple outputs at same time")
  assert(acc_t.getWidth == scale_t.getWidth, "we use the same variable to hold both the variance and the inv-stddev, so we need them to see the width")

  // Resets
  when (reset.asBool()) {
    stats.foreach(_.state := idle)
    stats.foreach(_.sum := acc_t.zero)
    stats.foreach(_.count := 0.U)
  }
}

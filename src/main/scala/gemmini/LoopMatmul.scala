package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._
import firrtl.transforms.DontTouchAnnotation

/*
for k in K:
	for j in J:
		for i in I:

For A: We can overwrite (i,k) when k_old > k_new
For B: We can overwrite (k,j) when k_old > k_new or (k_old == k_new and j_old > j_new)
*/

// TODO add double-buffering (to do this, make B's address "-" rather than "+")

// LdA

class LoopMatmulLdAReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int) extends Bundle {
  val max_i = UInt(iterator_bitwidth.W)
  val max_k = UInt(iterator_bitwidth.W)
  val pad_i = UInt(log2Up(block_size).W)
  val pad_k = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
}

class LoopMatmulLdA(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int)
                   (implicit p: Parameters) extends Module {
  val MAX_BLOCK_LEN = 4 // TODO get this from configs

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth)))
    val cmd = Decoupled(Output(new RoCCCommand))
    val i = Output(UInt(iterator_bitwidth.W))
    val k = Output(UInt(iterator_bitwidth.W))
    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val addr_start = Output(UInt(log2Up(max_addr).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulLdAReq(block_size, coreMaxAddrBits, iterator_bitwidth))

  val i = Reg(UInt(iterator_bitwidth.W))
  val k = Reg(UInt(iterator_bitwidth.W))

  val max_blocks = Mux(req.max_k <= MAX_BLOCK_LEN.U, req.max_k, MAX_BLOCK_LEN.U)

  val sp_addr_start = 0.U
  io.addr_start := sp_addr_start

  val dram_addr = req.dram_addr + (i * req.dram_stride + k) * block_size.U
  val sp_addr = sp_addr_start + (i * req.max_k + k) * block_size.U
  val blocks = Mux(k + max_blocks <= req.max_k, max_blocks, req.max_k-k)
  val cols = (blocks * block_size.U) - Mux(k + blocks >= req.max_k, req.pad_k, 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (rows << 48).asUInt() | (cols << 32).asUInt() | sp_addr

  io.req.ready := state === idle
  io.i := i
  io.k := k
  io.idle := state === idle

  io.cmd.valid := state =/= idle && !io.rob_overloaded
  io.cmd.bits := mvin_cmd

  when (io.cmd.fire()) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_k = floorAdd(k, max_blocks, req.max_k, next_i === 0.U)

    i := next_i
    k := next_k

    when (next_i === 0.U && next_k === 0.U) {
      state := idle
    }
  }

  when (io.req.fire()) {
    req := io.req.bits
    state := ld
    i := 0.U
    k := 0.U
  }
}

// LdB

class LoopMatmulLdBReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int) extends Bundle {
  val max_k = UInt(iterator_bitwidth.W)
  val max_j = UInt(iterator_bitwidth.W)
  val pad_k = UInt(log2Up(block_size).W)
  val pad_j = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
}

class LoopMatmulLdB(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int)
                   (implicit p: Parameters) extends Module {
  val MAX_BLOCK_LEN = 4 // TODO get this from configs

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth)))
    val cmd = Decoupled(Output(new RoCCCommand))

//    val ldA_k = Input(UInt(iterator_bitwidth.W))
//    val ldA_i = Input(UInt(iterator_bitwidth.W))
//    val ldA_completed = Input(Bool())

    val k = Output(UInt(iterator_bitwidth.W))
    val j = Output(UInt(iterator_bitwidth.W))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val addr_start = Output(UInt(log2Up(max_addr).W))
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulLdBReq(block_size, coreMaxAddrBits, iterator_bitwidth))

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))

  val max_blocks = Mux(req.max_k <= MAX_BLOCK_LEN.U, req.max_k, MAX_BLOCK_LEN.U)

  val sp_addr_start = max_addr.U - req.max_k * req.max_j * block_size.U
  io.addr_start := sp_addr_start

  val dram_addr = req.dram_addr + (k * req.dram_stride + j) * block_size.U
  val sp_addr = sp_addr_start + (k * req.max_j + j) * block_size.U
  val blocks = Mux(j + max_blocks <= req.max_j, max_blocks, req.max_j-j)
  val cols = (blocks * block_size.U) - Mux(j + blocks >= req.max_j, req.pad_j, 0.U)
  val rows = block_size.U - Mux(k === req.max_k-1.U, req.pad_k, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (rows << 48).asUInt() | (cols << 32).asUInt() | sp_addr

  io.req.ready := state === idle
  io.k := k
  io.j := j
  io.idle := state === idle

  // val ldA_ahead = io.ldA_k > k || io.ldA_completed

  io.cmd.valid := state =/= idle && !io.rob_overloaded // && ldA_ahead
  io.cmd.bits := mvin_cmd

  when (io.cmd.fire()) {
    // The order here is k, j, i
    val next_j = floorAdd(j, max_blocks, req.max_j)
    val next_k = floorAdd(k, 1.U, req.max_k, next_j === 0.U)

    k := next_k
    j := next_j

    when (next_j === 0.U && next_k === 0.U) {
      state := idle
    }
  }

  when (io.req.fire()) {
    req := io.req.bits
    state := ld
    j := 0.U
    k := 0.U
  }
}

// Compute
class LoopMatmulExecuteReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int) extends Bundle {
  val max_j = UInt(iterator_bitwidth.W)
  val max_k = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_k = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val bias = Bool()
}

class LoopMatmulExecute(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int, max_addr: Int)
                       (implicit p: Parameters) extends Module {
  val MAX_BLOCK_LEN = 4 // TODO get this from configs
  val GARBAGE_ADDR = (~0.U(32.W)).asUInt()

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulExecuteReq(block_size, coreMaxAddrBits, iterator_bitwidth)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val k = Output(UInt(iterator_bitwidth.W))
    val j = Output(UInt(iterator_bitwidth.W))
    val i = Output(UInt(iterator_bitwidth.W))

    val ld_k = Input(UInt(iterator_bitwidth.W))
    val ld_j = Input(UInt(iterator_bitwidth.W))
    val ld_i = Input(UInt(iterator_bitwidth.W))
    val ld_completed = Input(Bool())

    val a_addr_start = Input(UInt(log2Up(max_addr).W))
    val b_addr_start = Input(UInt(log2Up(max_addr).W))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
  })

  val d_addr_start = (BigInt(1) << 31).U
  val c_addr_start = (BigInt(3) << 30).U

  object State extends ChiselEnum {
    val idle, pre, comp = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulExecuteReq(block_size, coreMaxAddrBits, iterator_bitwidth))

  val k = Reg(UInt(iterator_bitwidth.W))
  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))

  val a_addr = io.a_addr_start + (i * req.max_k + k) * block_size.U
  val b_addr = io.b_addr_start + (k * req.max_j + j) * block_size.U
  val d_addr = d_addr_start + (i * req.max_j + j) * block_size.U
  val c_addr = c_addr_start + (i * req.max_j + j) * block_size.U

  val a_cols = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)
  val a_rows = block_size.U - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)
  val b_cols = block_size.U - Mux(j === req.max_j - 1.U, req.pad_j, 0.U)
  val b_rows = block_size.U - Mux(k === req.max_k - 1.U, req.pad_k, 0.U)
  val c_cols = block_size.U - Mux(j === req.max_j - 1.U, req.pad_j, 0.U)
  val c_rows = block_size.U - Mux(i === req.max_i - 1.U, req.pad_i, 0.U)

  val pre_addr = Mux(i === 0.U, b_addr, GARBAGE_ADDR)
  val out_addr = Mux(req.bias || k =/= 0.U, c_addr, d_addr)

  val pre_cmd = Wire(new RoCCCommand)
  pre_cmd := DontCare
  pre_cmd.inst.funct := PRELOAD_CMD
  pre_cmd.rs1 := pre_addr | (b_cols << 32).asUInt() | (b_rows << 48).asUInt()
  pre_cmd.rs2 := out_addr | (c_cols << 32).asUInt() | (c_rows << 48).asUInt()

  val comp_cmd = Wire(new RoCCCommand())
  comp_cmd := DontCare
  comp_cmd.inst.funct := Mux(i === 0.U, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)
  comp_cmd.rs1 := a_addr | (a_cols << 32).asUInt() | (a_rows << 48).asUInt()
  comp_cmd.rs2 := GARBAGE_ADDR | (block_size.U << 32).asUInt() | (block_size.U << 48).asUInt()

  io.req.ready := state === idle
  io.k := k
  io.j := j
  io.i := i
  io.idle := state === idle

  // The order here is k, j, i
  val ld_ahead = io.ld_completed || io.ld_j > j || (io.ld_j === j && io.ld_k > k) ||
    (io.ld_j === j && io.ld_k === k && io.ld_i > i)

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ld_ahead
  io.cmd.bits := Mux(state === pre, pre_cmd, comp_cmd)

  when (io.cmd.fire()) {
    when (state === pre) {
      state := comp
    }.otherwise {
      val next_i = floorAdd(i, 1.U, req.max_i)
      val next_j = floorAdd(j, 1.U, req.max_j, next_i === 0.U)
      val next_k = floorAdd(k, 1.U, req.max_k, next_j === 0.U && next_i === 0.U)

      k := next_k
      j := next_j
      i := next_i

      state := Mux(next_k === 0.U && next_j === 0.U && next_i === 0.U, idle, pre)
    }
  }

  when (io.req.fire()) {
    req := io.req.bits
    state := pre
    j := 0.U
    k := 0.U
    i := 0.U
  }
}

// StC

class LoopMatmulStCReq(val block_size: Int, val coreMaxAddrBits: Int, val iterator_bitwidth: Int) extends Bundle {
  val max_j = UInt(iterator_bitwidth.W)
  val max_i = UInt(iterator_bitwidth.W)
  val pad_j = UInt(log2Up(block_size).W)
  val pad_i = UInt(log2Up(block_size).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val dram_stride = UInt(coreMaxAddrBits.W)
}

class LoopMatmulStC(block_size: Int, coreMaxAddrBits: Int, iterator_bitwidth: Int)
                   (implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopMatmulStCReq(block_size, coreMaxAddrBits, iterator_bitwidth)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val ex_j = Input(UInt(iterator_bitwidth.W))
    val ex_i = Input(UInt(iterator_bitwidth.W))
    val ex_completed = Input(Bool())

    val j = Output(UInt(iterator_bitwidth.W))
    val i = Output(UInt(iterator_bitwidth.W))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
  })

  object State extends ChiselEnum {
    val idle, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopMatmulStCReq(block_size, coreMaxAddrBits, iterator_bitwidth))

  val j = Reg(UInt(iterator_bitwidth.W))
  val i = Reg(UInt(iterator_bitwidth.W))

  val sp_addr_start = (BigInt(1) << 31).U

  val dram_addr = req.dram_addr + (i * req.dram_stride + j) * block_size.U
  val sp_addr = sp_addr_start + (i * req.max_j + j) * block_size.U
  val cols = block_size.U - Mux(j + 1.U >= req.max_j, req.pad_j, 0.U)
  val rows = block_size.U - Mux(i === req.max_i-1.U, req.pad_i, 0.U)

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := STORE_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (rows << 48).asUInt() | (cols << 32).asUInt() | sp_addr

  io.req.ready := state === idle
  io.j := j
  io.i := i
  io.idle := state === idle

  // The order here is k, j, i
  val ex_ahead = io.ex_completed || io.ex_j > j || (io.ex_j === j && io.ex_i > i)

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ex_ahead
  io.cmd.bits := mvin_cmd

  when (io.cmd.fire()) {
    // The order here is k, j, i
    val next_i = floorAdd(i, 1.U, req.max_i)
    val next_j = floorAdd(j, 1.U, req.max_j, next_i === 0.U)

    i := next_i
    j := next_j

    when (next_i === 0.U && next_j === 0.U) {
      state := idle
    }
  }

  when (io.req.fire()) {
    req := io.req.bits
    state := ld
    j := 0.U
    i := 0.U
  }
}

// Combined loop

class LoopMatmul(block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int, max_addr: Int)
                (implicit p: Parameters) extends Module {
  val iterator_bitwidth = 16

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val ld_utilization = Input(UInt(log2Up(rob_size).W))
    val st_utilization = Input(UInt(log2Up(rob_size).W))
    val ex_utilization = Input(UInt(log2Up(rob_size).W))
    val busy = Output(Bool())
  })

  object State extends ChiselEnum {
    val idle, running = Value
  }
  import State._
  val state = RegInit(idle)

  // Create inner modules
  val ldA = Module(new LoopMatmulLdA(block_size, coreMaxAddrBits, 16, max_addr))
  val ldB = Module(new LoopMatmulLdB(block_size, coreMaxAddrBits, 16, max_addr))
  val ex = Module(new LoopMatmulExecute(block_size, coreMaxAddrBits, 16, max_addr))
  val stC = Module(new LoopMatmulStC(block_size, coreMaxAddrBits, 16))

  // Create command queue
  val cmd = Queue(io.in, 2)

  io.busy := cmd.valid || state =/= idle

  // Create ld arbiter
  // val ld_arb = Module(new RRArbiter(new RoCCCommand(), 2))
  // ld_arb.io.in(0) <> ldA.io.cmd
  // ld_arb.io.in(1) <> ldB.io.cmd

  val ld_arb = Module(new WeightedArbiter(new RoCCCommand(), weightA=3))
  ld_arb.io.inA <> ldA.io.cmd
  ld_arb.io.inB <> ldB.io.cmd

  // Create global arbiter
  val arb = Module(new Arbiter(new RoCCCommand(), 3))
  arb.io.in(0) <> stC.io.cmd
  arb.io.in(1) <> ex.io.cmd
  arb.io.in(2) <> ld_arb.io.out
  val unrolled_cmd = arb.io.out

  // Wire up unrolled command output
  val is_loop_cmd = cmd.bits.inst.funct === LOOP_WS
  val is_loop_config_cmd = cmd.bits.inst.funct >= LOOP_WS_CONFIG_BOUNDS && cmd.bits.inst.funct <= LOOP_WS_CONFIG_STRIDES_DC

  io.out.bits := Mux(is_loop_cmd, unrolled_cmd.bits, cmd.bits)
  io.out.bits.status := cmd.bits.status
  io.out.valid := Mux(is_loop_cmd, unrolled_cmd.valid, cmd.valid) && !is_loop_config_cmd

  cmd.ready := !is_loop_cmd && io.out.ready
  arb.io.out.ready := is_loop_cmd && io.out.ready

  // Wire up overloaded signals
  ldA.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ldB.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ex.io.rob_overloaded := io.ex_utilization >= max_exs.U
  stC.io.rob_overloaded := io.st_utilization >= max_sts.U

  // Wire up iterator inputs
  ex.io.ld_completed := ldA.io.idle && ldB.io.idle
  ex.io.ld_k := MuxCase(minOf(ldA.io.k, ldB.io.k), Seq(ldA.io.idle -> ldB.io.k, ldB.io.idle -> ldA.io.k))
  ex.io.ld_j := ldB.io.j
  ex.io.ld_i := ldA.io.i

  stC.io.ex_completed := ex.io.idle
  stC.io.ex_j := ex.io.j
  stC.io.ex_i := ex.io.i

  // Wire up sp addresses
  ex.io.a_addr_start := ldA.io.addr_start
  ex.io.b_addr_start := ldB.io.addr_start

  // Create config registers
  val max_k = Reg(UInt(iterator_bitwidth.W))
  val max_j = Reg(UInt(iterator_bitwidth.W))
  val max_i = Reg(UInt(iterator_bitwidth.W))

  val pad_k = Reg(UInt(iterator_bitwidth.W))
  val pad_j = Reg(UInt(iterator_bitwidth.W))
  val pad_i = Reg(UInt(iterator_bitwidth.W))

  val a_dram_addr = Reg(UInt(coreMaxAddrBits.W))
  val b_dram_addr = Reg(UInt(coreMaxAddrBits.W))
  val d_dram_addr = Reg(UInt(coreMaxAddrBits.W))
  val c_dram_addr = Reg(UInt(coreMaxAddrBits.W))

  val a_dram_stride = Reg(UInt(coreMaxAddrBits.W))
  val b_dram_stride = Reg(UInt(coreMaxAddrBits.W))
  val d_dram_stride = Reg(UInt(coreMaxAddrBits.W))
  val c_dram_stride = Reg(UInt(coreMaxAddrBits.W))

  val bias = cmd.bits.rs1(0)

  when(cmd.valid && is_loop_config_cmd && state === idle) {
    cmd.ready := true.B

    switch (cmd.bits.inst.funct) {
      is (LOOP_WS_CONFIG_BOUNDS) {
        max_k := cmd.bits.rs2(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        max_j := cmd.bits.rs2(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        max_i := cmd.bits.rs2(iterator_bitwidth-1, 0)

        pad_k := cmd.bits.rs1(iterator_bitwidth * 3 - 1, iterator_bitwidth * 2)
        pad_j := cmd.bits.rs1(iterator_bitwidth * 2 - 1, iterator_bitwidth)
        pad_i := cmd.bits.rs1(iterator_bitwidth-1, 0)
      }

      is (LOOP_WS_CONFIG_ADDRS_AB) {
        a_dram_addr := cmd.bits.rs1
        b_dram_addr := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_ADDRS_DC) {
        d_dram_addr := cmd.bits.rs1
        c_dram_addr := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_AB) {
        a_dram_stride := cmd.bits.rs1
        b_dram_stride := cmd.bits.rs2
      }

      is (LOOP_WS_CONFIG_STRIDES_DC) {
        d_dram_stride := cmd.bits.rs1
        c_dram_stride := cmd.bits.rs2
      }
    }
  }

  // Wire up request signals
  ldA.io.req.bits.max_k := max_k
  ldA.io.req.bits.max_i := max_i
  ldA.io.req.bits.pad_k := pad_k
  ldA.io.req.bits.pad_i := pad_i
  ldA.io.req.bits.dram_addr := a_dram_addr
  ldA.io.req.bits.dram_stride := a_dram_stride

  ldA.io.req.valid := state === idle && cmd.valid && is_loop_cmd

  ldB.io.req.bits.max_j := max_j
  ldB.io.req.bits.max_k := max_k
  ldB.io.req.bits.pad_j := pad_j
  ldB.io.req.bits.pad_k := pad_k
  ldB.io.req.bits.dram_addr := b_dram_addr
  ldB.io.req.bits.dram_stride := b_dram_stride

  ldB.io.req.valid := state === idle && cmd.valid && is_loop_cmd

  ex.io.req.bits.max_j := max_j
  ex.io.req.bits.max_k := max_k
  ex.io.req.bits.max_i := max_i
  ex.io.req.bits.pad_j := pad_j
  ex.io.req.bits.pad_k := pad_k
  ex.io.req.bits.pad_i := pad_i
  ex.io.req.bits.bias := bias

  ex.io.req.valid := state === idle && cmd.valid && is_loop_cmd

  stC.io.req.bits.max_j := max_j
  stC.io.req.bits.max_i := max_i
  stC.io.req.bits.pad_j := pad_j
  stC.io.req.bits.pad_i := pad_i
  stC.io.req.bits.dram_addr := c_dram_addr
  stC.io.req.bits.dram_stride := c_dram_stride

  stC.io.req.valid := state === idle && cmd.valid && is_loop_cmd

  // State changes
  switch (state) {
    is (idle) {
      when (cmd.valid && is_loop_cmd) {
        state := running
      }
    }

    is (running) {
      when (ldA.io.idle && ldB.io.idle && ex.io.idle && stC.io.idle) {
        state := idle
        cmd.ready := true.B
      }
    }
  }

  // Counters
  val preloads_counter = RegInit(0.U(32.W))
  val compute_and_flips_counter = RegInit(0.U(32.W))
  val compute_and_stay_counter = RegInit(0.U(32.W))

  val computes_counter = compute_and_flips_counter + compute_and_stay_counter
  val matmuls_counter = preloads_counter + computes_counter

  dontTouch(preloads_counter)
  dontTouch(compute_and_flips_counter)
  dontTouch(compute_and_stay_counter)
  dontTouch(computes_counter)
  dontTouch(matmuls_counter)

  when (io.out.fire()) {
    when (io.out.bits.inst.funct === PRELOAD_CMD) {
      preloads_counter := preloads_counter + 1.U
    }.elsewhen(io.out.bits.inst.funct === COMPUTE_AND_FLIP_CMD) {
      compute_and_flips_counter := compute_and_flips_counter + 1.U
    }.elsewhen(io.out.bits.inst.funct === COMPUTE_AND_STAY_CMD) {
      compute_and_stay_counter := compute_and_stay_counter + 1.U
    }
  }
}

object LoopMatmul {
  def apply(in: DecoupledIO[RoCCCommand], ld_utilization: UInt, st_utilization: UInt, ex_utilization: UInt,
            block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int, max_addr: Int)
           (implicit p: Parameters): Tuple2[DecoupledIO[RoCCCommand], Bool] = {
    val mod = Module(new LoopMatmul(block_size, coreMaxAddrBits, rob_size, max_lds, max_exs, max_sts, max_addr))
    mod.io.in <> in
    mod.io.ld_utilization := ld_utilization
    mod.io.st_utilization := st_utilization
    mod.io.ex_utilization := ex_utilization
    (mod.io.out, mod.io.busy)
  }
}

package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.config.Parameters
import GemminiISA._
import Util._

class LoopConvOuterBounds(val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int) extends Bundle {
  val batch_size = UInt(large_iterator_bitwidth.W)
  val in_dim = UInt(small_iterator_bitwidth.W)
  val in_channels = UInt(large_iterator_bitwidth.W)
  val out_channels = UInt(large_iterator_bitwidth.W)
  val out_dim = UInt(small_iterator_bitwidth.W)
  val pool_out_dim = UInt(small_iterator_bitwidth.W)
  val stride = UInt(tiny_iterator_bitwidth.W)
  val padding = UInt(tiny_iterator_bitwidth.W)
  val kernel_dim = UInt(tiny_iterator_bitwidth.W)
  val pool_size = UInt(tiny_iterator_bitwidth.W)
  val pool_stride = UInt(tiny_iterator_bitwidth.W)
  val pool_padding = UInt(tiny_iterator_bitwidth.W)
}

class LoopConvInnerBounds(val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int) extends Bundle {
  val batches = UInt(large_iterator_bitwidth.W)
  val porows = UInt(small_iterator_bitwidth.W)
  val pocols = UInt(small_iterator_bitwidth.W)
  val pochs = UInt(large_iterator_bitwidth.W)
  val krows = UInt(tiny_iterator_bitwidth.W)
  val kcols = UInt(tiny_iterator_bitwidth.W)
  val kchs = UInt(large_iterator_bitwidth.W)
  val lpad = UInt(tiny_iterator_bitwidth.W)
  val rpad = UInt(tiny_iterator_bitwidth.W)
  val upad = UInt(tiny_iterator_bitwidth.W)
  val dpad = UInt(tiny_iterator_bitwidth.W)
  val plpad = UInt(tiny_iterator_bitwidth.W)
  val prad = UInt(tiny_iterator_bitwidth.W)
  val pupad = UInt(tiny_iterator_bitwidth.W)
  val pdpad = UInt(tiny_iterator_bitwidth.W)
  val orows = UInt(small_iterator_bitwidth.W)
  val ocols = UInt(small_iterator_bitwidth.W)
}

class LoopConvDerivedParams(val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int) extends Bundle {
  val ochs = UInt(large_iterator_bitwidth.W)

  val irows = UInt(small_iterator_bitwidth.W)
  val icols = UInt(small_iterator_bitwidth.W)
  val irows_unpadded = UInt(small_iterator_bitwidth.W)
  val icols_unpadded = UInt(small_iterator_bitwidth.W)
  val ichs = UInt(large_iterator_bitwidth.W)

  val out_channels_per_bank = UInt(small_iterator_bitwidth.W) // TODO this won't work for systolic arrays above 256 in size

  val bias_spad_stride = UInt(large_iterator_bitwidth.W)
  val input_spad_stride = UInt(large_iterator_bitwidth.W)
  val weight_spad_stride = UInt(large_iterator_bitwidth.W)

  val ex_overwrite = Bool()
}

class LoopConvLdBiasReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val no_bias = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvLdBias(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_acc_addr: Int, acc_w: Int,
                     max_block_len_acc: Int, concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val MVIN_SCALE_IDENTITY = 0x3f800000.U // TODO get this from configs somehow

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvLdBiasReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth: Int, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val wait_for_prev_loop = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, config, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvLdBiasReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth: Int, max_acc_addr, concurrent_loops))
  import req.inner_bounds._
  import req.derived_params._

  val acc_addr_start = (BigInt(1) << 31).U | req.addr_start

  // Derived parameters
  val max_ochs_per_mvin = Mux(ochs < (max_block_len_acc * block_size).U, ochs, (max_block_len_acc * block_size).U)

  val skip = req.no_bias || (req.dram_addr === 0.U)

  // Iterators
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))
  val och = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = req.dram_addr +& och * (acc_w/8).U
  val spad_addr = acc_addr_start +& (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  // Sizes
  val I = Mux(ocols - ocol > block_size.U, block_size.U, ocols - ocol)
  val J = Mux(ochs - och > max_ochs_per_mvin, max_ochs_per_mvin, ochs - och)

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (req.derived_params.bias_spad_stride << 16.U) | (2.U << 3) | 1.U
  config_cmd.rs2 := 0.U

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD3_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (I << 48.U) | (J << 32.U) | spad_addr

  // Inputs and outputs
  io.req.ready := state === idle
  io.idle := state === idle
  io.loop_id := req.loop_id

  io.cmd.valid := state =/= idle && !io.rob_overloaded && !io.wait_for_prev_loop && !skip
  io.cmd.bits := Mux(state === config, config_cmd, mvin_cmd)

  // Sending outputs
  when (skip) {
    state := idle
  }.elsewhen(io.cmd.fire()) {
    when (state === config) {
      state := ld
    }.otherwise {
      val next_och = floorAdd(och, max_ochs_per_mvin, ochs)
      val next_ocol = floorAdd(ocol, block_size.U, ocols, next_och === 0.U)
      val next_orow = floorAdd(orow, 1.U, orows, next_ocol === 0.U && next_och === 0.U)
      val next_b = floorAdd(b, 1.U, batches, next_orow === 0.U && next_ocol === 0.U && next_och === 0.U)

      och := next_och
      ocol := next_ocol
      orow := next_orow
      b := next_b

      state := Mux(next_b === 0.U && next_orow === 0.U && next_ocol === 0.U && next_och === 0.U,
        idle, ld)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := config
    b := 0.U
    orow := 0.U
    ocol := 0.U
    och := 0.U
  }
}

class LoopConvLdInputReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvLdInput(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                      max_block_len: Int, concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val MVIN_SCALE_IDENTITY = 0x3f800000.U // TODO get this from configs somehow

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvLdInputReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val wait_for_prev_loop = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, config, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvLdInputReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  // Derived parameters
  val max_ichs_per_mvin = Mux(ichs < (max_block_len * block_size).U, ichs, (max_block_len * block_size).U).zext()

  // Iterators
  val b = Reg(SInt(large_iterator_bitwidth.W))
  val irow = Reg(SInt(small_iterator_bitwidth.W))
  val icol = Reg(SInt(small_iterator_bitwidth.W))
  val ich = Reg(SInt(large_iterator_bitwidth.W))

  // Calculated params
  val irow_padded = irow +& upad.zext()
  val icol_padded = icol +& lpad.zext()
  val is_zeros = irow < 0.S || irow >= irows_unpadded.zext() || icol < 0.S || icol >= icols_unpadded.zext()

  // Addresses
  val dram_addr = Mux(is_zeros, 0.U,
    req.dram_addr +& (((b * in_dim * in_dim +& irow*in_dim +& icol) * in_channels +& ich) * (input_w/8).U).asUInt())
  val spad_addr = req.addr_start.zext() +& (ich / block_size.S) * batches * irows * icols +& b * irows * icols +& irow_padded * icols +& icol_padded

  // Sizes
  val I = MuxCase(
    Mux(icols_unpadded.zext() -& icol > block_size.S, block_size.S, icols_unpadded.zext() -& icol),
    Seq(
      (icol < 0.S) -> Mux((0.S-&icol) > block_size.S, block_size.S, 0.S-&icol),
      (icol >= icols_unpadded.zext()) -> Mux(icols_unpadded.zext() +& rpad.zext() -& icol > block_size.S, block_size.S, icols_unpadded.zext() +& rpad.zext() -& icol)
    )
  )
  val K = Mux(ichs.zext() -& ich > max_ichs_per_mvin, max_ichs_per_mvin, ichs.zext() -& ich)

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (req.derived_params.input_spad_stride << 16.U) | (0.U << 3) | 1.U
  config_cmd.rs2 := in_channels * (input_w/8).U

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (I << 48.U).asUInt() | (K << 32.U).asUInt() | spad_addr.asUInt()

  // Inputs and outputs
  io.req.ready := state === idle
  io.idle := state === idle
  io.loop_id := req.loop_id

  io.cmd.valid := state =/= idle && !io.wait_for_prev_loop && !io.rob_overloaded
  io.cmd.bits := Mux(state === config, config_cmd, mvin_cmd)

  // Sending outputs
  when(io.cmd.fire()) {
    when (state === config) {
      state := ld
    }.otherwise {
      val next_ich = sFloorAdd(ich, max_ichs_per_mvin.asUInt(), ichs.zext(), 0.S)
      val next_icol = sFloorAdd(icol, I.asUInt(), (icols_unpadded +& rpad).zext(), 0.S-&lpad.zext(),
        next_ich === 0.S)
      val next_irow = sFloorAdd(irow, 1.U, (irows_unpadded +& dpad).zext(), 0.S-&upad.zext(),
        next_icol === 0.S-&lpad.zext() && next_ich === 0.S)
      val next_b = sFloorAdd(b, 1.U, batches.zext(), 0.S,
        next_irow === 0.S-&upad.zext() && next_icol === 0.S-&lpad.zext() && next_ich === 0.S)

      ich := next_ich
      icol := next_icol
      irow := next_irow
      b := next_b

      state := Mux(next_b === 0.S && next_irow === 0.S-&upad.zext() && next_icol === 0.S-&lpad.zext() && next_ich === 0.S,
        idle, ld)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := config
    b := 0.S
    irow := 0.S -& io.req.bits.inner_bounds.upad.zext()
    icol := 0.S -& io.req.bits.inner_bounds.lpad.zext()
    ich := 0.S
  }
}

class LoopConvLdWeightReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_end = UInt(log2Up(max_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvLdWeight(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                       max_block_len: Int, concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val MVIN_SCALE_IDENTITY = 0x3f800000.U // TODO get this from configs somehow

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvLdWeightReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())
    val wait_for_prev_loop = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, config, ld = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvLdWeightReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  // Derived parameters
  val max_ochs_per_mvin = Mux(ochs < (max_block_len * block_size).U, ochs, (max_block_len * block_size).U)
  val B_rows = out_channels_per_bank * kcols * krows * kchs
  val addr_start = req.addr_end - B_rows

  // Iterators
  val och = Reg(UInt(large_iterator_bitwidth.W))
  val krow = Reg(UInt(tiny_iterator_bitwidth.W))
  val kcol = Reg(UInt(tiny_iterator_bitwidth.W))
  val kch = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = req.dram_addr +& ((krow*kernel_dim*in_channels +& kcol*in_channels +& kch) * out_channels +& och) * (input_w/8).U
  val spad_addr = addr_start + (och / block_size.U) * krows * kcols * kchs + krow * kcols * kchs + kcol * kchs + kch

  // Sizes
  val J = Mux(ochs - och > max_ochs_per_mvin, max_ochs_per_mvin, ochs - och)
  val K = Mux(kchs - kch > block_size.U, block_size.U, kchs - kch)

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (req.derived_params.weight_spad_stride << 16.U) | (1.U << 3) | 1.U
  config_cmd.rs2 := out_channels * (input_w/8).U

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD2_CMD
  mvin_cmd.rs1 := dram_addr
  mvin_cmd.rs2 := (K << 48.U) | (J << 32.U) | spad_addr

  // Inputs and outputs
  io.req.ready := state === idle
  io.idle := state === idle
  io.loop_id := req.loop_id

  io.cmd.valid := state =/= idle && !io.wait_for_prev_loop && !io.rob_overloaded
  io.cmd.bits := Mux(state === config, config_cmd, mvin_cmd)

  // Sending outputs
  when(io.cmd.fire()) {
    when (state === config) {
      state := ld
    }.otherwise {
      val next_kch = floorAdd(kch, block_size.U, kchs)
      val next_kcol = floorAdd(kcol, 1.U, kcols, next_kch === 0.U)
      val next_krow = floorAdd(krow, 1.U, krows, next_kcol === 0.U && next_kch === 0.U)
      val next_och = floorAdd(och, max_ochs_per_mvin, ochs, next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)

      kch := next_kch
      kcol := next_kcol
      krow := next_krow
      och := next_och

      state := Mux(next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U,
        idle, ld)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := config
    kch := 0.U
    kcol := 0.U
    krow := 0.U
    och := 0.U
  }
}

class LoopConvExecuteReq(val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_addr: Int, val max_acc_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr).W)
  val c_addr_start = UInt(log2Up(max_acc_addr).W)
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvExecute(block_size: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int,
                      max_acc_addr: Int, concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val GARBAGE_ADDR = (~0.U(32.W)).asUInt()

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvExecuteReq(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val lda_completed = Input(Bool())
    val ldb_completed = Input(Bool())
    val ldd_completed = Input(Bool())

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, pre, comp = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvExecuteReq(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth,
    max_addr, max_acc_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  // Derived parameters
  val B_rows = out_channels_per_bank * kcols * krows * kchs

  val a_addr_start = req.a_addr_start
  val b_addr_start = req.b_addr_end - B_rows
  val d_addr_start = (BigInt(1) << 31).U | req.c_addr_start
  val c_addr_start = (BigInt(3) << 30).U | req.c_addr_start

  // Iterators
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))
  val och = Reg(UInt(large_iterator_bitwidth.W))
  val krow = Reg(UInt(tiny_iterator_bitwidth.W))
  val kcol = Reg(UInt(tiny_iterator_bitwidth.W))
  val kch = Reg(UInt(large_iterator_bitwidth.W))

  val irow = orow * stride +& krow
  val icol = ocol * stride +& kcol

  val I = Mux(ocols - ocol > block_size.U, block_size.U, ocols - ocol)
  val J = Mux(ochs - och > block_size.U, block_size.U, ochs - och)
  val K = Mux(kchs - kch > block_size.U, block_size.U, kchs - kch)

  // Addresses
  val a_addr = a_addr_start +& (kch / block_size.U) * batches * irows * icols +& b * irows * icols +& irow * icols +& icol
  val b_addr = b_addr_start +& (och / block_size.U) * krows * kcols * kchs +& krow * kcols * kchs +& kcol * kchs +& kch
  val c_addr = Mux(ex_overwrite && krow === 0.U && kcol === 0.U && kch === 0.U, d_addr_start, c_addr_start) +&
    (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  // Commands
  val pre_cmd = Wire(new RoCCCommand)
  pre_cmd := DontCare
  pre_cmd.inst.funct := PRELOAD_CMD
  pre_cmd.rs1 := (K << 48) | (J << 32) | b_addr
  pre_cmd.rs2 := (I << 48) | (J << 32) | c_addr

  val comp_cmd = Wire(new RoCCCommand())
  comp_cmd := DontCare
  comp_cmd.inst.funct := COMPUTE_AND_FLIP_CMD
  comp_cmd.rs1 := (I << 48) | (K << 32) | a_addr
  comp_cmd.rs2 := (I << 48) | (J << 32) | GARBAGE_ADDR

  // Inputs and outputs
  io.req.ready := state === idle
  io.idle := state === idle

  val ld_ahead = io.lda_completed && io.ldb_completed && io.ldd_completed

  io.cmd.valid := state =/= idle && !io.rob_overloaded && ld_ahead
  io.cmd.bits := Mux(state === pre, pre_cmd, comp_cmd)

  io.loop_id := req.loop_id

  // Sending outputs
  when (io.cmd.fire()) {
    when (state === pre) {
      state := comp
    }.otherwise {
      val next_kch = floorAdd(kch, block_size.U, kchs)
      val next_kcol = floorAdd(kcol, 1.U, kcols, next_kch === 0.U)
      val next_krow = floorAdd(krow, 1.U, krows, next_kcol === 0.U && next_kch === 0.U)
      val next_och = floorAdd(och, block_size.U, ochs,
        next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)
      val next_ocol = floorAdd(ocol, block_size.U, ocols,
        next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)
      val next_orow = floorAdd(orow, 1.U, orows,
        next_ocol === 0.U && next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)
      val next_b = floorAdd(b, 1.U, batches, next_orow === 0.U &&
        next_ocol === 0.U && next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)

      kch := next_kch
      kcol := next_kcol
      krow := next_krow
      och := next_och
      ocol := next_ocol
      orow := next_orow
      b := next_b

      state := Mux(next_b === 0.U && next_orow === 0.U && next_ocol === 0.U &&
        next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U,
        idle, pre)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := pre

    b := 0.U
    orow := 0.U
    ocol := 0.U
    och := 0.U
    krow := 0.U
    kcol := 0.U
    kch := 0.U
  }
}

class LoopConvStReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val no_pool = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvSt(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int, concurrent_loops: Int)(implicit p: Parameters) extends Module {
  val MVIN_SCALE_IDENTITY = 0x3f800000.U // TODO get this from configs somehow

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new LoopConvStReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth: Int, max_acc_addr, concurrent_loops)))
    val cmd = Decoupled(Output(new RoCCCommand))

    val ex_completed = Input(Bool())

    val idle = Output(Bool())
    val rob_overloaded = Input(Bool())

    val loop_id = Output(UInt(log2Up(concurrent_loops).W))
  })

  object State extends ChiselEnum {
    val idle, st = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvStReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth: Int, max_acc_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  val acc_addr_start = (BigInt(1) << 31).U | req.addr_start

  // Derived parameters
  val skip = !(req.no_pool && (req.dram_addr =/= 0.U))

  // Iterators
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))
  val och = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = req.dram_addr + ((b*out_dim*out_dim + orow*out_dim + ocol) * out_channels + och) * (input_w/8).U
  val spad_addr = acc_addr_start +& (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  // Sizes
  val I = Mux(ocols - ocol > block_size.U, block_size.U, ocols - ocol)
  val J = Mux(ochs - och > block_size.U, block_size.U, ochs - och)

  // Commands
  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := dram_addr
  mvout_cmd.rs2 := (I << 48.U) | (J << 32.U) | spad_addr

  // Inputs and outputs
  io.req.ready := state === idle
  io.idle := state === idle
  io.loop_id := req.loop_id

  io.cmd.valid := state =/= idle && !io.rob_overloaded && !skip && io.ex_completed
  io.cmd.bits := mvout_cmd

  // Sending outputs
  when (skip) {
    state := idle
  }.elsewhen(io.cmd.fire()) {
    val next_och = floorAdd(och, block_size.U, ochs)
    val next_ocol = floorAdd(ocol, block_size.U, ocols, next_och === 0.U)
    val next_orow = floorAdd(orow, 1.U, orows, next_ocol === 0.U && next_och === 0.U)
    val next_b = floorAdd(b, 1.U, batches, next_orow === 0.U && next_ocol === 0.U && next_och === 0.U)

    och := next_och
    ocol := next_ocol
    orow := next_orow
    b := next_b

    state := Mux(next_b === 0.U && next_orow === 0.U && next_ocol === 0.U && next_och === 0.U,
      idle, st)
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := st

    b := 0.U
    orow := 0.U
    ocol := 0.U
    och := 0.U
  }
}

class LoopConvState(val block_size: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val coreMaxAddrBits: Int, val max_addr: Int, val max_acc_addr: Int) extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)

  val bias_dram_addr = UInt(coreMaxAddrBits.W)
  val weights_dram_addr = UInt(coreMaxAddrBits.W)
  val input_dram_addr = UInt(coreMaxAddrBits.W)
  val output_dram_addr = UInt(coreMaxAddrBits.W)

  val no_bias = Bool()
  val no_pool = Bool()

  val configured = Bool()

  val running = Bool()

  val ld_bias_started = Bool()
  val ld_input_started = Bool()
  val ld_weights_started = Bool()
  val ex_started = Bool()
  val st_started = Bool()

  val ld_bias_completed = Bool()
  val ld_input_completed = Bool()
  val ld_weights_completed = Bool()
  val ex_completed = Bool()
  val st_completed = Bool()

  def all_completed(dummy: Int=0): Bool = ld_bias_completed && ld_input_completed && ld_weights_completed && ex_completed && st_completed

  val a_addr_start = UInt(log2Up(max_addr).W)
  val b_addr_end = UInt(log2Up(max_addr).W)

  def derived_params(dummy: Int=0): LoopConvDerivedParams = {
    import outer_bounds.stride
    import inner_bounds.{batches, pochs, orows, ocols, krows, kcols, upad, dpad, lpad, rpad, kchs}

    val result = Wire(new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth))

    result.ochs := pochs

    result.irows := orows * stride +& krows - 1.U
    result.icols := ocols * stride +& kcols - 1.U
    result.irows_unpadded := result.irows - upad - dpad
    result.icols_unpadded := result.icols - lpad - rpad
    result.ichs := kchs

    result.out_channels_per_bank := result.ochs / block_size.U +& (result.ochs % block_size.U =/= 0.U)

    result.bias_spad_stride := batches * orows * ocols
    result.input_spad_stride := batches * result.irows * result.icols
    result.weight_spad_stride := krows * kcols * kchs

    result.ex_overwrite := bias_dram_addr =/= 0.U && no_bias

    result
  }

  def reset(): Unit = {
    configured := false.B

    running := false.B

    ld_bias_started := false.B
    ld_input_started := false.B
    ld_weights_started := false.B
    ex_started := false.B
    st_started := false.B

    ld_bias_completed := false.B
    ld_input_completed := false.B
    ld_weights_completed := false.B
    ex_completed := false.B
    st_completed := false.B
  }
}

class LoopConv (block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
  max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int)
  (implicit p: Parameters) extends Module {
  val large_iterator_bitwidth = 16
  val small_iterator_bitwidth = 8
  val tiny_iterator_bitwidth = 4

  val max_block_len = (dma_max_bytes / (block_size * (input_w / 8))) max 1
  val max_block_len_acc = (dma_max_bytes / (block_size * (acc_w / 8))) max 1

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCCommand)
    val ld_utilization = Input(UInt(log2Up(rob_size+1).W))
    val st_utilization = Input(UInt(log2Up(rob_size+1).W))
    val ex_utilization = Input(UInt(log2Up(rob_size+1).W))
    val busy = Output(Bool())
  })

  // Create states
  val concurrent_loops = 2
  val loops = Reg(Vec(concurrent_loops, new LoopConvState(block_size, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, coreMaxAddrBits, max_addr, max_acc_addr)))
  val head_loop_id = RegInit(0.U(log2Up(concurrent_loops).W))
  val tail_loop_id = (~head_loop_id).asUInt() // This is the loop that we always try to configure if available
  val head_loop = loops(head_loop_id)
  val tail_loop = loops(tail_loop_id)

  val loop_configured = loops.map(_.configured).reduce(_ || _)

  val loop_being_configured_id = Mux(head_loop.configured, tail_loop_id, head_loop_id)
  val loop_being_configured = loops(loop_being_configured_id)

  // Create inner modules
  val ld_bias = Module(new LoopConvLdBias(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_acc_addr, acc_w, max_block_len_acc, concurrent_loops))
  val ld_input = Module(new LoopConvLdInput(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops))
  val ld_weights = Module(new LoopConvLdWeight(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops))
  val ex = Module(new LoopConvExecute(block_size, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops))
  val st = Module(new LoopConvSt(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_acc_addr, input_w, concurrent_loops))

  // Create command queue
  val cmd = Queue(io.in)

  io.busy := cmd.valid || loop_configured

  // Create arbiter
  val arb = Module(new Arbiter(new RoCCCommand, 5))
  arb.io.in(0) <> st.io.cmd
  arb.io.in(1) <> ex.io.cmd
  arb.io.in(2) <> ld_bias.io.cmd
  arb.io.in(3) <> ld_weights.io.cmd
  arb.io.in(4) <> ld_input.io.cmd
  val unrolled_cmd = arb.io.out

  // Wire up unrolled command output
  val is_loop_run_cmd = cmd.bits.inst.funct === LOOP_CONV_WS
  val is_loop_config_cmd = cmd.bits.inst.funct >= LOOP_CONV_WS_CONFIG_1 && cmd.bits.inst.funct <= LOOP_CONV_WS_CONFIG_6
  val is_loop_cmd = is_loop_run_cmd || is_loop_config_cmd

  io.out.bits := Mux(loop_configured, unrolled_cmd.bits, cmd.bits)
  io.out.bits.status := cmd.bits.status // TODO This is not guaranteed to be the correct fix! We must fix this
  io.out.valid := Mux(loop_configured, unrolled_cmd.valid, cmd.valid && !is_loop_config_cmd && !is_loop_run_cmd)

  cmd.ready := Mux(is_loop_cmd, !loop_being_configured.configured, !loop_configured && io.out.ready)
  arb.io.out.ready := io.out.ready

  // Wire up waiting-for-loads signals
  val ex_is_waiting_for_loads = loops(ex.io.loop_id).ex_started && !loops(ex.io.loop_id).ex_completed &&
    !(loops(ex.io.loop_id).ld_input_completed && loops(ex.io.loop_id).ld_weights_completed &&
      loops(ex.io.loop_id).ld_bias_completed)

  ld_bias.io.wait_for_prev_loop := ex_is_waiting_for_loads && ld_bias.io.loop_id =/= ex.io.loop_id
  ld_weights.io.wait_for_prev_loop := ex_is_waiting_for_loads && ld_weights.io.loop_id =/= ex.io.loop_id
  ld_input.io.wait_for_prev_loop := ex_is_waiting_for_loads && ld_input.io.loop_id =/= ex.io.loop_id

  // Wire up overloaded signals
  ld_bias.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ld_input.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ld_weights.io.rob_overloaded := io.ld_utilization >= max_lds.U
  ex.io.rob_overloaded := io.ex_utilization >= max_exs.U
  st.io.rob_overloaded := io.st_utilization >= max_sts.U

  // Wire up iterator inputs
  ex.io.lda_completed := (ld_input.io.loop_id =/= ex.io.loop_id) || ld_input.io.idle
  ex.io.ldb_completed := (ld_weights.io.loop_id =/= ex.io.loop_id) || ld_weights.io.idle
  ex.io.ldd_completed := (ld_bias.io.loop_id =/= ex.io.loop_id) || ld_bias.io.idle
  st.io.ex_completed := (ex.io.loop_id =/= st.io.loop_id) || ex.io.idle

  // Create config registers
  when(cmd.valid && is_loop_cmd && !loop_being_configured.configured) {

    switch (cmd.bits.inst.funct) {
      is (LOOP_CONV_WS_CONFIG_1) {
        loop_being_configured.outer_bounds.out_channels := cmd.bits.rs1(63, 48)
        loop_being_configured.outer_bounds.in_channels := cmd.bits.rs1(47, 32)
        loop_being_configured.outer_bounds.in_dim := cmd.bits.rs1(31, 16)
        loop_being_configured.outer_bounds.batch_size := cmd.bits.rs1(15, 0)

        loop_being_configured.outer_bounds.padding := cmd.bits.rs2(63, 48)
        loop_being_configured.outer_bounds.stride := cmd.bits.rs2(47, 32)
        loop_being_configured.outer_bounds.pool_out_dim := cmd.bits.rs2(31, 16)
        loop_being_configured.outer_bounds.out_dim := cmd.bits.rs2(15, 0)
      }

      is (LOOP_CONV_WS_CONFIG_2) {
        loop_being_configured.outer_bounds.kernel_dim := cmd.bits.rs1(63, 48)
        loop_being_configured.outer_bounds.pool_size := cmd.bits.rs1(47, 32)
        loop_being_configured.outer_bounds.pool_stride := cmd.bits.rs1(31, 16)
        loop_being_configured.outer_bounds.pool_padding := cmd.bits.rs1(15, 0)

        loop_being_configured.inner_bounds.batches := cmd.bits.rs2(63, 48)
        loop_being_configured.inner_bounds.porows := cmd.bits.rs2(47, 32)
        loop_being_configured.inner_bounds.pocols := cmd.bits.rs2(31, 16)
        loop_being_configured.inner_bounds.pochs := cmd.bits.rs2(15, 0)
      }

      is (LOOP_CONV_WS_CONFIG_3) {
        loop_being_configured.inner_bounds.krows := cmd.bits.rs1(63, 48)
        loop_being_configured.inner_bounds.kcols := cmd.bits.rs1(47, 32)
        loop_being_configured.inner_bounds.kchs := cmd.bits.rs1(31, 16)
        loop_being_configured.inner_bounds.lpad := cmd.bits.rs1(15, 0)

        loop_being_configured.inner_bounds.rpad := cmd.bits.rs2(63, 48)
        loop_being_configured.inner_bounds.upad := cmd.bits.rs2(47, 32)
        loop_being_configured.inner_bounds.dpad := cmd.bits.rs2(31, 16)
        loop_being_configured.inner_bounds.plpad := cmd.bits.rs2(15, 0)
      }

      is (LOOP_CONV_WS_CONFIG_4) {
        loop_being_configured.inner_bounds.orows := cmd.bits.rs1(63, 48)
        loop_being_configured.inner_bounds.prad := cmd.bits.rs1(47, 32)
        loop_being_configured.inner_bounds.pupad := cmd.bits.rs1(31, 16)
        loop_being_configured.inner_bounds.pdpad := cmd.bits.rs1(15, 0)

        loop_being_configured.inner_bounds.ocols := cmd.bits.rs2(15, 0)
      }

      is (LOOP_CONV_WS_CONFIG_5) {
        loop_being_configured.weights_dram_addr := cmd.bits.rs1

        loop_being_configured.output_dram_addr := cmd.bits.rs2
      }

      is (LOOP_CONV_WS_CONFIG_6) {
        loop_being_configured.bias_dram_addr := cmd.bits.rs1

        loop_being_configured.input_dram_addr := cmd.bits.rs2
      }

      is (LOOP_CONV_WS) {
        loop_being_configured.no_bias := cmd.bits.rs1(0)

        loop_being_configured.no_pool := cmd.bits.rs2(0)

        loop_being_configured.configured := true.B
      }
    }
  }

  // Wire up request signals
  val ld_bias_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val ex_c_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))
  val st_addr_start = RegInit(0.U(log2Up(max_acc_addr).W))

  val loop_requesting_ld_bias_id = Mux(head_loop.ld_bias_started, tail_loop_id, head_loop_id)
  val loop_requesting_ld_bias = loops(loop_requesting_ld_bias_id)
  ld_bias.io.req.bits.outer_bounds := loop_requesting_ld_bias.outer_bounds
  ld_bias.io.req.bits.inner_bounds := loop_requesting_ld_bias.inner_bounds
  ld_bias.io.req.bits.derived_params := loop_requesting_ld_bias.derived_params()
  ld_bias.io.req.bits.addr_start := ld_bias_addr_start
  ld_bias.io.req.bits.dram_addr := loop_requesting_ld_bias.bias_dram_addr
  ld_bias.io.req.bits.no_bias := loop_requesting_ld_bias.no_bias
  ld_bias.io.req.bits.loop_id := loop_requesting_ld_bias_id

  ld_bias.io.req.valid := !loop_requesting_ld_bias.ld_bias_started && loop_requesting_ld_bias.configured

  when (ld_bias.io.req.fire()) {
    loop_requesting_ld_bias.running := true.B
    loop_requesting_ld_bias.ld_bias_started := true.B

    // when (loop_requesting_ld_bias.bias_dram_addr =/= 0.U) {
    when (loop_requesting_ld_bias.output_dram_addr =/= 0.U) {
      ld_bias_addr_start := floorAdd(ld_bias_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  val loop_requesting_ld_input_id = Mux(head_loop.ld_input_started, tail_loop_id, head_loop_id)
  val loop_requesting_ld_input = loops(loop_requesting_ld_input_id)
  ld_input.io.req.bits.outer_bounds := loop_requesting_ld_input.outer_bounds
  ld_input.io.req.bits.inner_bounds := loop_requesting_ld_input.inner_bounds
  ld_input.io.req.bits.derived_params := loop_requesting_ld_input.derived_params()
  ld_input.io.req.bits.addr_start := loop_requesting_ld_input.a_addr_start
  ld_input.io.req.bits.dram_addr := loop_requesting_ld_input.input_dram_addr
  ld_input.io.req.bits.loop_id := loop_requesting_ld_input_id

  ld_input.io.req.valid := !loop_requesting_ld_input.ld_input_started && loop_requesting_ld_input.configured

  when (ld_input.io.req.fire()) {
    loop_requesting_ld_input.running := true.B
    loop_requesting_ld_input.ld_input_started := true.B
  }

  val loop_requesting_ld_weights_id = Mux(head_loop.ld_weights_started, tail_loop_id, head_loop_id)
  val loop_requesting_ld_weights = loops(loop_requesting_ld_weights_id)
  ld_weights.io.req.bits.outer_bounds := loop_requesting_ld_weights.outer_bounds
  ld_weights.io.req.bits.inner_bounds := loop_requesting_ld_weights.inner_bounds
  ld_weights.io.req.bits.derived_params := loop_requesting_ld_weights.derived_params()
  ld_weights.io.req.bits.addr_end := loop_requesting_ld_weights.b_addr_end
  ld_weights.io.req.bits.dram_addr := loop_requesting_ld_weights.weights_dram_addr
  ld_weights.io.req.bits.loop_id := loop_requesting_ld_weights_id

  ld_weights.io.req.valid := !loop_requesting_ld_weights.ld_weights_started && loop_requesting_ld_weights.configured

  when (ld_weights.io.req.fire()) {
    loop_requesting_ld_weights.running := true.B
    loop_requesting_ld_weights.ld_weights_started := true.B
  }

  val loop_requesting_ex_id = Mux(head_loop.ex_started, tail_loop_id, head_loop_id)
  val loop_requesting_ex = loops(loop_requesting_ex_id)
  ex.io.req.bits.outer_bounds := loop_requesting_ex.outer_bounds
  ex.io.req.bits.inner_bounds := loop_requesting_ex.inner_bounds
  ex.io.req.bits.derived_params := loop_requesting_ex.derived_params()
  ex.io.req.bits.a_addr_start := loop_requesting_ex.a_addr_start
  ex.io.req.bits.b_addr_end := loop_requesting_ex.b_addr_end
  ex.io.req.bits.c_addr_start := ex_c_addr_start
  ex.io.req.bits.loop_id := loop_requesting_ex_id

  ex.io.req.valid := !loop_requesting_ex.ex_started && loop_requesting_ex.ld_bias_started &&
    loop_requesting_ex.ld_input_started && loop_requesting_ex.ld_weights_started && loop_requesting_ex.configured

  when (ex.io.req.fire()) {
    loop_requesting_ex.running := true.B
    loop_requesting_ex.ex_started := true.B

    when (loop_requesting_ex.output_dram_addr =/= 0.U) {
      ex_c_addr_start := floorAdd(ex_c_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  val loop_requesting_st_id = Mux(head_loop.st_started, tail_loop_id, head_loop_id)
  val loop_requesting_st = loops(loop_requesting_st_id)
  st.io.req.bits.outer_bounds := loop_requesting_st.outer_bounds
  st.io.req.bits.inner_bounds := loop_requesting_st.inner_bounds
  st.io.req.bits.derived_params := loop_requesting_st.derived_params()
  st.io.req.bits.addr_start := st_addr_start
  st.io.req.bits.dram_addr := loop_requesting_st.output_dram_addr
  st.io.req.bits.no_pool := loop_requesting_st.no_pool
  st.io.req.bits.loop_id := loop_requesting_st_id

  st.io.req.valid := !loop_requesting_st.st_started && loop_requesting_st.ex_started && loop_requesting_st.configured

  when (st.io.req.fire()) {
    loop_requesting_st.running := true.B
    loop_requesting_st.st_started := true.B

    when (loop_requesting_st.output_dram_addr =/= 0.U) {
      st_addr_start := floorAdd(st_addr_start, (max_acc_addr / concurrent_loops).U, max_acc_addr.U)
    }
  }

  // Handle completed signals
  when (ld_bias.io.idle && loops(ld_bias.io.loop_id).running && loops(ld_bias.io.loop_id).ld_bias_started) {
    loops(ld_bias.io.loop_id).ld_bias_completed := true.B
  }

  when (ld_input.io.idle && loops(ld_input.io.loop_id).running && loops(ld_input.io.loop_id).ld_input_started) {
    loops(ld_input.io.loop_id).ld_input_completed := true.B
  }

  when (ld_weights.io.idle && loops(ld_weights.io.loop_id).running && loops(ld_weights.io.loop_id).ld_weights_started) {
    loops(ld_weights.io.loop_id).ld_weights_completed := true.B
  }

  when (ex.io.idle && loops(ex.io.loop_id).running && loops(ex.io.loop_id).ex_started) {
    loops(ex.io.loop_id).ex_completed := true.B
  }

  when (st.io.idle && loops(st.io.loop_id).running && loops(st.io.loop_id).st_started) {
    loops(st.io.loop_id).st_completed := true.B
  }

  when (head_loop.running && head_loop.all_completed()) {
    head_loop.reset()
    head_loop_id := ~head_loop_id
  }

  // Resets
  when (reset.toBool()) {
    loops.zipWithIndex.foreach { case (l, i) =>
      l.reset()
      l.a_addr_start := (i * (max_addr / concurrent_loops)).U
      l.b_addr_end := ((i+1) * (max_addr / concurrent_loops) - block_size).U
    }
  }
}

object LoopConv {
  def apply(in: DecoupledIO[RoCCCommand], ld_utilization: UInt, st_utilization: UInt, ex_utilization: UInt,
            block_size: Int, coreMaxAddrBits: Int, rob_size: Int, max_lds: Int, max_exs: Int, max_sts: Int,
            max_addr: Int, max_acc_addr: Int, input_w: Int, acc_w: Int, dma_max_bytes: Int)
           (implicit p: Parameters): Tuple2[DecoupledIO[RoCCCommand], Bool] = {
    val mod = Module(new LoopConv(block_size, coreMaxAddrBits, rob_size, max_lds, max_exs, max_sts,
      max_addr, max_acc_addr, input_w, acc_w, dma_max_bytes))
    mod.io.in <> in
    mod.io.ld_utilization := ld_utilization
    mod.io.st_utilization := st_utilization
    mod.io.ex_utilization := ex_utilization
    (mod.io.out, mod.io.busy)
  }
}

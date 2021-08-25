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
  val out_dim = UInt(large_iterator_bitwidth.W)
  val pool_out_dim = UInt(small_iterator_bitwidth.W)
  val stride = UInt(tiny_iterator_bitwidth.W)
  val padding = UInt(tiny_iterator_bitwidth.W)
  val kernel_dim = UInt(tiny_iterator_bitwidth.W)
  val kernel_dilation = UInt(tiny_iterator_bitwidth.W)
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
  val in_channels_per_bank = UInt(small_iterator_bitwidth.W) // TODO this won't work for systolic arrays above 256 in size

  val bias_spad_stride = UInt(large_iterator_bitwidth.W)
  val input_spad_stride = UInt(large_iterator_bitwidth.W)
  val weight_spad_stride = UInt(large_iterator_bitwidth.W)

  // val ex_overwrite = Bool()
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
                     max_block_len_acc: Int, concurrent_loops: Int, latency: Int)(implicit p: Parameters) extends Module {
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

  val skip = req.dram_addr === 0.U

  // Iterators
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))
  val och = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = Mux(req.no_bias, 0.U, req.dram_addr +& och * (acc_w/8).U)
  val spad_addr = acc_addr_start +& (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  // Sizes
  val I = Mux(ocols - ocol > block_size.U, block_size.U, ocols - ocol)
  val J = Mux(ochs - och > max_ochs_per_mvin, max_ochs_per_mvin, ochs - och)

  class RoCCCommandWithAddr extends Bundle {
    val cmd = new RoCCCommand
    val dram_addr = UInt()
    val spad_addr = UInt()
    val I = UInt()
    val J = UInt()
  }
  val command_p = Module(new Pipeline(new RoCCCommandWithAddr, latency)())

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (req.derived_params.bias_spad_stride << 16.U) | (2.U << 3) | 1.U
  config_cmd.rs2 := 0.U

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD3_CMD
  mvin_cmd.rs1 := 0.U //dram_addr
  mvin_cmd.rs2 := 0.U //(I << 48.U) | (J << 32.U) | spad_addr

  // Inputs and outputs
  io.req.ready := state === idle && !command_p.io.busy
  io.idle := state === idle && !command_p.io.busy
  io.loop_id := req.loop_id

  command_p.io.in.valid := state =/= idle && !io.wait_for_prev_loop && !skip
  command_p.io.in.bits.cmd := Mux(state === config, config_cmd, mvin_cmd)
  command_p.io.in.bits.dram_addr := dram_addr
  command_p.io.in.bits.spad_addr := spad_addr
  command_p.io.in.bits.I := I
  command_p.io.in.bits.J := J

  command_p.io.out.ready := io.cmd.ready && !io.rob_overloaded
  io.cmd.valid := command_p.io.out.valid && !io.rob_overloaded
  io.cmd.bits := command_p.io.out.bits.cmd
  when (command_p.io.out.bits.cmd.inst.funct === LOAD3_CMD) {
    val o = command_p.io.out.bits
    io.cmd.bits.rs1 := o.dram_addr
    io.cmd.bits.rs2 := (o.I << 48.U) | (o.J << 32.U) | o.spad_addr
  }

  // Sending outputs
  when (skip) {
    state := idle
  }.elsewhen(command_p.io.in.fire()) {
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
  val downsample = Bool()
  val input_dilated = Bool()
  val trans_input_3120 = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvLdInput(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                      max_block_len: Int, concurrent_loops: Int, latency: Int)(implicit p: Parameters) extends Module {
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

  def undilated(x: UInt): UInt = (x +& req.input_dilated) >> req.input_dilated

  // Derived parameters
  val max_ichs_per_mvin = Mux(ichs < (max_block_len * block_size).U, ichs, (max_block_len * block_size).U).zext()
  val max_batches_per_mvin = Mux(batches < (max_block_len * block_size).U, batches, (max_block_len * block_size).U).zext()
  val max_chs_per_mvin = Mux(req.trans_input_3120, max_batches_per_mvin, max_ichs_per_mvin)

  // Iterators
  val b = Reg(SInt(large_iterator_bitwidth.W))
  val irow = Reg(SInt(small_iterator_bitwidth.W))
  val icol = Reg(SInt(small_iterator_bitwidth.W))
  val ich = Reg(SInt(large_iterator_bitwidth.W))

  // Calculated params
  val irow_padded = irow +& undilated(upad).zext()
  val icol_padded = icol +& undilated(lpad).zext()
  val is_zeros = irow < 0.S || irow >= irows_unpadded.zext() || icol < 0.S || icol >= icols_unpadded.zext()

  val dram_stride = Mux(req.trans_input_3120, batch_size * (input_w/8).U, in_channels * (input_w/8).U)

  // Addresses
  val dram_addr = MuxCase(req.dram_addr +& (((b * in_dim * in_dim +& irow*in_dim +& icol) * in_channels +& ich) * (input_w/8).U).asUInt(), Seq(
    is_zeros -> 0.U,
    req.trans_input_3120 -> (req.dram_addr +& (((ich * in_dim * in_dim +& irow*in_dim +& icol) * batches +& b) * (input_w/8).U).asUInt())
  ))
  val spad_addr = Mux(req.trans_input_3120,
    req.addr_start.zext() +& (b / block_size.S) * input_spad_stride +& ich * (irows >> req.downsample) * (icols >> req.downsample) +& (irow_padded >> req.downsample) * (icols >> req.downsample) +& (icol_padded >> req.downsample),
    req.addr_start.zext() +& (ich / block_size.S) * input_spad_stride +& b * (irows >> req.downsample) * (icols >> req.downsample) +& (irow_padded >> req.downsample) * (icols >> req.downsample) +& (icol_padded >> req.downsample))

  // Sizes
  val block_size_downsampled = (block_size.U << req.downsample).asUInt().zext()

  val I = MuxCase(
    Mux(icols_unpadded.zext() -& icol > block_size_downsampled, block_size_downsampled, icols_unpadded.zext() -& icol),
    Seq(
      (icol < 0.S) -> Mux((0.S-&icol) > block_size.S, block_size.S, 0.S-&icol),
      (icol >= icols_unpadded.zext()) -> Mux(icols_unpadded.zext() +& undilated(rpad).zext() -& icol > block_size.S, block_size.S, icols_unpadded.zext() +& undilated(rpad).zext() -& icol)
    )
  )
  val K = Mux(req.trans_input_3120,
    Mux(batches.zext() -& b > max_chs_per_mvin, max_chs_per_mvin, batches.zext() -& b),
    Mux(ichs.zext() -& ich > max_chs_per_mvin, max_chs_per_mvin, ichs.zext() -& ich))

  class RoCCCommandWithAddr extends Bundle {
    val cmd = new RoCCCommand
    val dram_addr = UInt()
    val spad_addr = SInt()
    val I = SInt()
    val K = SInt()
  }
  val command_p = Module(new Pipeline(new RoCCCommandWithAddr, latency)())
  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (input_spad_stride << 16.U) | (0.U << 3) | 1.U
  config_cmd.rs2 := dram_stride << req.downsample

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD_CMD
  mvin_cmd.rs1 := 0.U //dram_addr
  mvin_cmd.rs2 := 0.U // ((I >> req.downsample) << 48.U).asUInt() | (K << 32.U).asUInt() | spad_addr.asUInt()

  // Inputs and outputs
  io.req.ready := state === idle && !command_p.io.busy
  io.idle := state === idle && !command_p.io.busy
  io.loop_id := req.loop_id

  command_p.io.in.valid := state =/= idle && !io.wait_for_prev_loop
  command_p.io.in.bits.cmd := Mux(state === config, config_cmd, mvin_cmd)
  command_p.io.in.bits.dram_addr := dram_addr
  command_p.io.in.bits.spad_addr := spad_addr
  command_p.io.in.bits.I := I
  command_p.io.in.bits.K := K

  command_p.io.out.ready := io.cmd.ready && !io.rob_overloaded
  io.cmd.valid := command_p.io.out.valid && !io.rob_overloaded
  io.cmd.bits := command_p.io.out.bits.cmd
  when (command_p.io.out.bits.cmd.inst.funct === LOAD_CMD) {
    val o = command_p.io.out.bits
    io.cmd.bits.rs1 := o.dram_addr
    io.cmd.bits.rs2 := ((o.I >> req.downsample) << 48).asUInt | (o.K << 32).asUInt | o.spad_addr.asUInt
  }

  // Sending outputs
  when(command_p.io.in.fire()) {
    when (state === config) {
      state := ld
    }.otherwise {
      val b_it = Mux(req.trans_input_3120, max_chs_per_mvin.asUInt(), 1.U)
      val ich_it = Mux(req.trans_input_3120, 1.U, max_chs_per_mvin.asUInt())

      val next_ich = sFloorAdd(ich, ich_it, ichs.zext(), 0.S)
      val next_icol = sFloorAdd(icol, I.asUInt(), (icols_unpadded +& undilated(rpad)).zext(), 0.S-&undilated(lpad).zext(),
        next_ich === 0.S)
      val next_irow = sFloorAdd(irow, 1.U << req.downsample, (irows_unpadded +& undilated(dpad)).zext(), 0.S-&undilated(upad).zext(),
        next_icol === 0.S-&undilated(lpad).zext() && next_ich === 0.S)
      val next_b = sFloorAdd(b, b_it, batches.zext(), 0.S,
        next_irow === 0.S-&undilated(upad).zext() && next_icol === 0.S-&undilated(lpad).zext() && next_ich === 0.S)

      ich := next_ich
      icol := next_icol
      irow := next_irow
      b := next_b

      state := Mux(next_b === 0.S && next_irow === 0.S-&undilated(upad).zext() && next_icol === 0.S-&undilated(lpad).zext() && next_ich === 0.S,
        idle, ld)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := config
    b := 0.S
    irow := 0.S -& ((io.req.bits.inner_bounds.upad +& io.req.bits.input_dilated) >> io.req.bits.input_dilated).zext()
    icol := 0.S -& ((io.req.bits.inner_bounds.lpad +& io.req.bits.input_dilated) >> io.req.bits.input_dilated).zext()
    ich := 0.S
  }
}

class LoopConvLdWeightReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_end = UInt(log2Up(max_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val trans_weight_1203 = Bool()
  val trans_weight_0132 = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvLdWeight(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int, input_w: Int,
                       max_block_len: Int, concurrent_loops: Int, latency: Int)(implicit p: Parameters) extends Module {
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
  val max_chs_per_mvin = {
    val max_ochs_per_mvin = Mux(ochs < (max_block_len * block_size).U, ochs, (max_block_len * block_size).U)
    val max_kchs_per_mvin = Mux(kchs < (max_block_len * block_size).U, kchs, (max_block_len * block_size).U)
    Mux(req.trans_weight_0132, max_kchs_per_mvin, max_ochs_per_mvin)
  }

  val B_rows = Mux(req.trans_weight_0132, in_channels_per_bank * kcols * krows * ochs,
    out_channels_per_bank * kcols * krows * kchs)
  val addr_start = req.addr_end - B_rows

  val dram_stride = MuxCase(out_channels, Seq(
    req.trans_weight_1203 -> (kernel_dim * kernel_dim * out_channels),
    req.trans_weight_0132 -> in_channels
  )) * (input_w/8).U

  // Iterators
  val och = Reg(UInt(large_iterator_bitwidth.W))
  val krow = Reg(UInt(tiny_iterator_bitwidth.W))
  val kcol = Reg(UInt(tiny_iterator_bitwidth.W))
  val kch = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = MuxCase(req.dram_addr +& ((krow*kernel_dim*in_channels +& kcol*in_channels +& kch) * out_channels +& och) * (input_w/8).U, Seq(
    req.trans_weight_1203 -> (req.dram_addr +& ((kch*kernel_dim*kernel_dim +& krow*kernel_dim +& kcol) * out_channels +& och) * (input_w/8).U),
    req.trans_weight_0132 -> (req.dram_addr +& ((krow*kernel_dim*out_channels +& kcol*out_channels +& och) * in_channels +& kch) * (input_w/8).U)
  ))

  val spad_addr = Mux(req.trans_weight_0132,
    addr_start + (kch / block_size.U) * krows * kcols * ochs + krow * kcols * ochs + kcol * ochs + och,
    addr_start + (och / block_size.U) * krows * kcols * kchs + krow * kcols * kchs + kcol * kchs + kch)

  // Sizes
  val J = Mux(req.trans_weight_0132,
    Mux(kchs - kch > max_chs_per_mvin, max_chs_per_mvin, kchs - kch),
    Mux(ochs - och > max_chs_per_mvin, max_chs_per_mvin, ochs - och))
  val K = Mux(req.trans_weight_0132,
    Mux(ochs - och > block_size.U, block_size.U, ochs - och),
    Mux(kchs - kch > block_size.U, block_size.U, kchs - kch))

  class RoCCCommandWithAddr extends Bundle {
    val cmd = new RoCCCommand
    val dram_addr = UInt()
    val spad_addr = UInt()
    val K = UInt()
    val J = UInt()
  }
  val command_p = Module(new Pipeline(new RoCCCommandWithAddr, latency)())

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := (MVIN_SCALE_IDENTITY << 32.U) | (req.derived_params.weight_spad_stride << 16.U) | (1.U << 3) | 1.U
  config_cmd.rs2 := dram_stride

  val mvin_cmd = Wire(new RoCCCommand)
  mvin_cmd := DontCare
  mvin_cmd.inst.funct := LOAD2_CMD
  mvin_cmd.rs1 := 0.U//dram_addr
  mvin_cmd.rs2 := 0.U//(K << 48.U) | (J << 32.U) | spad_addr

  // Inputs and outputs
  io.req.ready := state === idle && !command_p.io.busy
  io.idle := state === idle && !command_p.io.busy
  io.loop_id := req.loop_id

  command_p.io.in.valid := state =/= idle && !io.wait_for_prev_loop
  command_p.io.in.bits.cmd := Mux(state === config, config_cmd, mvin_cmd)
  command_p.io.in.bits.dram_addr := dram_addr
  command_p.io.in.bits.spad_addr := spad_addr
  command_p.io.in.bits.K := K
  command_p.io.in.bits.J := J

  command_p.io.out.ready := io.cmd.ready && !io.rob_overloaded
  io.cmd.valid := command_p.io.out.valid && !io.rob_overloaded
  io.cmd.bits := command_p.io.out.bits.cmd
  when (command_p.io.out.bits.cmd.inst.funct === LOAD2_CMD) {
    val o = command_p.io.out.bits
    io.cmd.bits.rs1 := o.dram_addr
    io.cmd.bits.rs2 := (o.K << 48) | (o.J << 32) | o.spad_addr
  }

  // Sending outputs
  when(command_p.io.in.fire()) {
    when (state === config) {
      state := ld
    }.otherwise {
      val och_it = Mux(req.trans_weight_0132, block_size.U, max_chs_per_mvin)
      val kch_it = Mux(req.trans_weight_0132, max_chs_per_mvin, block_size.U)

      val next_kch = floorAdd(kch, kch_it, kchs)
      val next_kcol = floorAdd(kcol, 1.U, kcols, next_kch === 0.U)
      val next_krow = floorAdd(krow, 1.U, krows, next_kcol === 0.U && next_kch === 0.U)
      val next_och = floorAdd(och, och_it, ochs, next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U)

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
  val wrot180 = Bool()
  val downsample = Bool()
  val input_dilated = Bool()
  val trans_weight_0132 = Bool()
  val trans_input_3120 = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvExecute(block_size: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_addr: Int,
                      max_acc_addr: Int, concurrent_loops: Int, latency: Int)(implicit p: Parameters) extends Module {
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
    val idle, config, pre, comp = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvExecuteReq(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth,
    max_addr, max_acc_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  def undilated(x: UInt): UInt = (x +& req.input_dilated) >> req.input_dilated

  // Derived parameters
  val B_rows = Mux(req.trans_weight_0132, in_channels_per_bank * kcols * krows * ochs,
    out_channels_per_bank * kcols * krows * kchs)

  val a_addr_start = req.a_addr_start
  val b_addr_start = req.b_addr_end - B_rows
  // val d_addr_start = (BigInt(1) << 31).U | req.c_addr_start
  val c_addr_start = (BigInt(3) << 30).U | req.c_addr_start

  // Iterators
  val och = Reg(UInt(large_iterator_bitwidth.W))
  val krow = Reg(UInt(tiny_iterator_bitwidth.W))
  val kcol = Reg(UInt(tiny_iterator_bitwidth.W))
  val kch = Reg(UInt(large_iterator_bitwidth.W))
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))

  // TODO kernel-dilation and input-dilation can never be activated at the same time, so we can optimize out some multiplications by kernel_dilation
  val skip_iteration = state >= pre && req.input_dilated && (((krow * kernel_dilation +& orow -& upad)(0) & req.input_dilated).asBool() ||
    ((kcol * kernel_dilation +& ocol -& lpad)(0) & req.input_dilated).asBool())

  val irow = undilated(orow * stride +& krow * kernel_dilation)
  val icol = undilated(ocol * stride +& kcol * kernel_dilation)

  val I = Mux(req.trans_input_3120,
    Mux(batches - b > block_size.U, block_size.U, batches - b),
    undilated(Mux(ocols - ocol > (block_size.U << req.input_dilated).asUInt(), (block_size.U << req.input_dilated).asUInt(), ocols - ocol)))
  val J = Mux(ochs - och > block_size.U, block_size.U, ochs - och)
  val K = Mux(kchs - kch > block_size.U, block_size.U, kchs - kch)

  // Addresses
  val a_addr = Mux(req.trans_input_3120,
    a_addr_start +& (b / block_size.U) * input_spad_stride +& kch * (irows >> req.downsample) * (icols >> req.downsample) +& (irow >> req.downsample) * (icols >> req.downsample) +& (icol >> req.downsample),
    a_addr_start +& (kch / block_size.U) * input_spad_stride +& b * (irows >> req.downsample) * (icols >> req.downsample) +& (irow >> req.downsample) * (icols >> req.downsample) +& (icol >> req.downsample))

  // val c_addr = Mux(ex_overwrite && krow === 0.U && kcol === 0.U && kch === 0.U, d_addr_start, c_addr_start) +&
  //   (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  val c_addr = c_addr_start +&
    (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  // val new_weights = b === 0.U && orow === 0.U && ocol === 0.U
  val new_weights = Reg(Bool())
  val krow_ = Mux(req.wrot180, krows - krow - 1.U, krow)
  val kcol_ = Mux(req.wrot180, kcols - kcol - 1.U, kcol)

  val b_addr = Mux(req.trans_weight_0132,
    b_addr_start +& (kch / block_size.U) * krows * kcols * ochs +& krow_ * kcols * ochs +& kcol_ * ochs +& och,
    b_addr_start +& (och / block_size.U) * krows * kcols * kchs +& krow_ * kcols * kchs +& kcol_ * kchs +& kch)

  val pre_addr = Mux(new_weights, b_addr, GARBAGE_ADDR)

  class RoCCCommandWithAddr extends Bundle {
    val cmd = new RoCCCommand
    val a_addr = UInt()
    val pre_addr = UInt()
    val c_addr = UInt()
    val I = UInt()
    val J = UInt()
    val K = UInt()
  }
  val command_p = Module(new Pipeline(new RoCCCommandWithAddr, latency)())

  // Commands
  val config_cmd = Wire(new RoCCCommand)
  config_cmd := DontCare
  config_cmd.inst.funct := CONFIG_CMD
  config_cmd.rs1 := ((irows * icols) << 16) | (1.U << 7)
  config_cmd.rs2 := (orows * ocols) << 48

  val pre_cmd = Wire(new RoCCCommand)
  pre_cmd := DontCare
  pre_cmd.inst.funct := PRELOAD_CMD
  pre_cmd.rs1 := 0.U//(K << 48) | (J << 32) | pre_addr
  pre_cmd.rs2 := 0.U//(I << 48) | (J << 32) | c_addr

  val comp_cmd = Wire(new RoCCCommand())
  comp_cmd := DontCare
  comp_cmd.inst.funct := Mux(new_weights, COMPUTE_AND_FLIP_CMD, COMPUTE_AND_STAY_CMD)
  comp_cmd.rs1 := 0.U//(I << 48) | (K << 32) | a_addr
  comp_cmd.rs2 := 0.U//(I << 48) | (J << 32) | GARBAGE_ADDR

  val ld_ahead = io.lda_completed && io.ldb_completed && io.ldd_completed

  // Inputs and outputs
  io.req.ready := state === idle && !command_p.io.busy
  io.idle := state === idle && !command_p.io.busy
  io.loop_id := req.loop_id

  command_p.io.in.valid := state =/= idle && !skip_iteration && ld_ahead
  command_p.io.in.bits.cmd := MuxCase(config_cmd, Seq((state === pre) -> pre_cmd, (state === comp) -> comp_cmd))
  command_p.io.in.bits.a_addr := a_addr
  command_p.io.in.bits.pre_addr := pre_addr
  command_p.io.in.bits.c_addr := c_addr
  command_p.io.in.bits.I := I
  command_p.io.in.bits.J := J
  command_p.io.in.bits.K := K

  command_p.io.out.ready := io.cmd.ready && !io.rob_overloaded
  io.cmd.valid := command_p.io.out.valid && !io.rob_overloaded
  io.cmd.bits := command_p.io.out.bits.cmd
  when (command_p.io.out.bits.cmd.inst.funct === PRELOAD_CMD) {
    val o = command_p.io.out.bits
    io.cmd.bits.rs1 := (o.K << 48) | (o.J << 32) | o.pre_addr
    io.cmd.bits.rs2 := (o.I << 48) | (o.J << 32) | o.c_addr
  }.elsewhen(command_p.io.out.bits.cmd.inst.funct =/= CONFIG_CMD) {
    val o = command_p.io.out.bits
    io.cmd.bits.rs1 := (o.I << 48) | (o.K << 32) | o.a_addr
    io.cmd.bits.rs2 := (o.I << 48) | (o.J << 32) | GARBAGE_ADDR
  }

  // Updating "new_weights"
  when (state === comp && command_p.io.in.fire()) {
    new_weights := false.B
  }

  // Sending outputs
  when (command_p.io.in.fire() || skip_iteration) {
    when (state === config) {
      state := pre
    }.elsewhen (state === pre) {
      state := comp
    }.otherwise {
      val b_it = Mux(req.trans_input_3120, block_size.U, 1.U)
      val ocol_it = Mux(skip_iteration || req.trans_input_3120, 1.U, block_size.U << req.input_dilated).asUInt()

      val next_ocol = floorAdd(ocol, ocol_it, ocols)
      val next_orow = floorAdd(orow, 1.U, orows, next_ocol === 0.U)
      val next_b = floorAdd(b, b_it, batches, next_orow === 0.U && next_ocol === 0.U)
      val next_kch = floorAdd(kch, block_size.U, kchs,
        next_b === 0.U && next_orow === 0.U && next_ocol === 0.U)
      val next_kcol = floorAdd(kcol, 1.U, kcols,
        next_kch === 0.U && next_b === 0.U && next_orow === 0.U && next_ocol === 0.U)
      val next_krow = floorAdd(krow, 1.U, krows,
        next_kcol === 0.U && next_kch === 0.U && next_b === 0.U && next_orow === 0.U && next_ocol === 0.U)
      val next_och = floorAdd(och, block_size.U, ochs, next_krow === 0.U &&
        next_kcol === 0.U && next_kch === 0.U && next_b === 0.U && next_orow === 0.U && next_ocol === 0.U)

      ocol := next_ocol
      orow := next_orow
      b := next_b
      kch := next_kch
      kcol := next_kcol
      krow := next_krow
      och := next_och

      when (next_b === 0.U && next_orow === 0.U && next_ocol === 0.U) {
        new_weights := true.B
      }

      state := Mux(next_och === 0.U && next_krow === 0.U && next_kcol === 0.U && next_kch === 0.U && next_b === 0.U &&
        next_orow === 0.U && next_ocol === 0.U,
        idle, pre)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := Mux(io.req.bits.trans_input_3120, config, pre)

    b := 0.U
    orow := 0.U
    ocol := 0.U
    och := 0.U
    krow := 0.U
    kcol := 0.U
    kch := 0.U

    new_weights := true.B
  }
}

class LoopConvStReq(val coreMaxAddrBits: Int, val large_iterator_bitwidth: Int, val small_iterator_bitwidth: Int, val tiny_iterator_bitwidth: Int, val max_acc_addr: Int, val concurrent_loops: Int)  extends Bundle {
  val outer_bounds = new LoopConvOuterBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val inner_bounds = new LoopConvInnerBounds(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val derived_params = new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth)
  val addr_start = UInt(log2Up(max_acc_addr).W)
  val dram_addr = UInt(coreMaxAddrBits.W)
  val no_pool = Bool()
  val trans_output_1203 = Bool()
  val loop_id = UInt(log2Up(concurrent_loops).W)
}

class LoopConvSt(block_size: Int, coreMaxAddrBits: Int, large_iterator_bitwidth: Int, small_iterator_bitwidth: Int, tiny_iterator_bitwidth: Int, max_acc_addr: Int, input_w: Int, concurrent_loops: Int, latency: Int)(implicit p: Parameters) extends Module {
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
    val idle, st, pre_pool_config, pool, post_pool_config = Value
  }
  import State._
  val state = RegInit(idle)

  val req = Reg(new LoopConvStReq(coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth: Int, max_acc_addr, concurrent_loops))
  import req.outer_bounds._
  import req.inner_bounds._
  import req.derived_params._

  val acc_addr_start = (BigInt(1) << 31).U | req.addr_start

  // Derived parameters
  val skip = req.dram_addr === 0.U

  // Iterators
  val b = Reg(UInt(large_iterator_bitwidth.W))
  val orow = Reg(UInt(small_iterator_bitwidth.W))
  val ocol = Reg(UInt(small_iterator_bitwidth.W))
  val och = Reg(UInt(large_iterator_bitwidth.W))

  // Addresses
  val dram_addr = Mux(req.trans_output_1203,
    req.dram_addr + ((orow*out_dim*batch_size +& ocol*batch_size +& b) * out_channels +& och) * (input_w/8).U,
    req.dram_addr + ((b*out_dim*out_dim +& orow*out_dim +& ocol) * out_channels +& och) * (input_w/8).U)
  val spad_addr = acc_addr_start +& (och / block_size.U) * batches * orows * ocols +& b * orows * ocols +& orow * ocols +& ocol

  val pool_dram_addr = req.dram_addr + ((b * pool_out_dim * pool_out_dim) * out_channels + och) * (input_w/8).U
  val pool_spad_addr = acc_addr_start +& (och / block_size.U) * batches * orows * ocols +& b * orows * ocols

  // Sizes
  val I = Mux(ocols - ocol > block_size.U, block_size.U, ocols - ocol)
  val J = Mux(ochs - och > block_size.U, block_size.U, ochs - och)

  val channels = J

  class RoCCCommandWithAddr extends Bundle {
    val cmd = new RoCCCommand
    val dram_addr = UInt()
    val spad_addr = UInt()
    val pool_dram_addr = UInt()
    val pool_spad_addr = UInt()
    val channels = UInt()
    val is_pool = Bool()
    val I = UInt()
    val J = UInt()
  }
  val command_p = Module(new Pipeline(new RoCCCommandWithAddr, latency)())
  // Commands
  val mvout_cmd = Wire(new RoCCCommand)
  mvout_cmd := DontCare
  mvout_cmd.inst.funct := STORE_CMD
  mvout_cmd.rs1 := 0.U//dram_addr
  mvout_cmd.rs2 := 0.U//(I << 48.U) | (J << 32.U) | spad_addr

  val pre_pool_config_cmd = Wire(new RoCCCommand)
  pre_pool_config_cmd := DontCare
  pre_pool_config_cmd.inst.funct := CONFIG_CMD
  pre_pool_config_cmd.rs1 := (ocols << 56) | (orows << 48) | (pocols << 40) | (porows << 32) | (pool_out_dim << 24) |
    (plpad << 10) | (pupad << 8) | (pool_size << 6) | (pool_stride << 4) | // TODO magic numbers
    CONFIG_STORE
  pre_pool_config_cmd.rs2 := out_channels * (input_w / 8).U

  val post_pool_config_cmd = Wire(new RoCCCommand)
  post_pool_config_cmd := DontCare
  post_pool_config_cmd.inst.funct := CONFIG_CMD
  post_pool_config_cmd.rs1 := CONFIG_STORE
  post_pool_config_cmd.rs2 := out_channels * (input_w / 8).U

  val pool_cmd = Wire(new RoCCCommand)
  pool_cmd := DontCare
  pool_cmd.inst.funct := STORE_CMD
  pool_cmd.rs1 := 0.U//pool_dram_addr
  pool_cmd.rs2 := 0.U//(channels << 32.U) | pool_spad_addr

  // Inputs and outputs
  io.req.ready := state === idle && !command_p.io.busy
  io.idle := state === idle && !command_p.io.busy
  io.loop_id := req.loop_id

  command_p.io.in.valid := state =/= idle && !skip && io.ex_completed
  command_p.io.in.bits.cmd := MuxLookup(state.asUInt, mvout_cmd, Seq(
    pre_pool_config.asUInt -> pre_pool_config_cmd,
    pool.asUInt -> pool_cmd,
    post_pool_config.asUInt -> post_pool_config_cmd)
  )
  command_p.io.in.bits.is_pool := state === pool
  command_p.io.in.bits.dram_addr := dram_addr
  command_p.io.in.bits.spad_addr := spad_addr
  command_p.io.in.bits.pool_spad_addr := pool_spad_addr
  command_p.io.in.bits.pool_dram_addr := pool_dram_addr
  command_p.io.in.bits.channels := channels
  command_p.io.in.bits.I := I
  command_p.io.in.bits.J := J

  command_p.io.out.ready := io.cmd.ready && !io.rob_overloaded
  io.cmd.valid := command_p.io.out.valid && !io.rob_overloaded
  io.cmd.bits := command_p.io.out.bits.cmd
  when (command_p.io.out.bits.cmd.inst.funct === STORE_CMD) {
    val o = command_p.io.out.bits
    when (o.is_pool) {
      io.cmd.bits.rs1 := o.pool_dram_addr
      io.cmd.bits.rs2 := (o.channels << 32.U) | o.pool_spad_addr
    } .otherwise {
      io.cmd.bits.rs1 := o.dram_addr
      io.cmd.bits.rs2 := (o.I << 48) | (o.J << 32) | o.spad_addr
    }
  }

  // Sending outputs
  when (skip) {
    state := idle
  }.elsewhen(command_p.io.in.fire()) {
    when (req.no_pool) {
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
    }.elsewhen(state === pre_pool_config) {
      state := pool
    }.elsewhen(state === post_pool_config) {
      state := idle
    }.otherwise {
      val next_och = floorAdd(och, block_size.U, ochs)
      val next_b = floorAdd(b, 1.U, batches, next_och === 0.U)

      och := next_och
      b := next_b

      state := Mux(next_b === 0.U && next_och === 0.U,
        post_pool_config, pool)
    }
  }

  // Accepting requests
  when (io.req.fire()) {
    req := io.req.bits
    state := Mux(io.req.bits.no_pool, st, pre_pool_config)

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
  val wrot180 = Bool()
  val no_pool = Bool()
  val downsample = Bool()
  val input_dilated = Bool()
  val trans_output_1203 = Bool()
  val trans_weight_1203 = Bool()
  val trans_weight_0132 = Bool()
  val trans_input_3120 = Bool()

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
    import outer_bounds.{stride, kernel_dilation}
    import inner_bounds.{batches, pochs, orows, ocols, krows, kcols, upad, dpad, lpad, rpad, kchs}

    val result = Wire(new LoopConvDerivedParams(large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth))

    result.ochs := pochs

    val dilated_krows = krows + (kernel_dilation - 1.U)*(krows - 1.U)
    val dilated_kcols = kcols + (kernel_dilation - 1.U)*(kcols - 1.U)

    val irows_without_dilation = orows * stride +& dilated_krows -& 1.U
    val icols_without_dilation = ocols * stride +& dilated_kcols -& 1.U
    val irows_unpadded_without_dilation = irows_without_dilation -& upad -& dpad
    val icols_unpadded_without_dilation = icols_without_dilation -& lpad -& rpad

    def undilated(x: UInt): UInt = (x +& input_dilated) >> input_dilated

    val irows_unpadded = undilated(irows_unpadded_without_dilation)
    val icols_unpadded = undilated(icols_unpadded_without_dilation)

    result.irows := Mux(input_dilated, irows_unpadded +& undilated(upad) +& undilated(dpad), irows_without_dilation)
    result.icols := Mux(input_dilated, icols_unpadded +& undilated(lpad) +& undilated(rpad), icols_without_dilation)

    result.irows_unpadded := irows_unpadded
    result.icols_unpadded := icols_unpadded

    result.ichs := kchs

    result.out_channels_per_bank := result.ochs / block_size.U +& (result.ochs % block_size.U =/= 0.U)
    result.in_channels_per_bank := result.ichs / block_size.U +& (result.ichs % block_size.U =/= 0.U)

    result.bias_spad_stride := batches * orows * ocols
    result.input_spad_stride := Mux(trans_input_3120,
      result.ichs * (result.irows >> downsample) * (result.icols >> downsample),
      batches * (result.irows >> downsample) * (result.icols >> downsample))
    result.weight_spad_stride := Mux(trans_weight_0132, krows * kcols * pochs, krows * kcols * kchs)

    // result.ex_overwrite := bias_dram_addr =/= 0.U && no_bias

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
  val small_iterator_bitwidth = 16 // 8
  val tiny_iterator_bitwidth = 16 // 4

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
  val latency = 2
  val ld_bias = Module(new LoopConvLdBias(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_acc_addr, acc_w, max_block_len_acc, concurrent_loops, latency))
  val ld_input = Module(new LoopConvLdInput(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops, latency))
  val ld_weights = Module(new LoopConvLdWeight(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, input_w, max_block_len, concurrent_loops, latency))
  val ex = Module(new LoopConvExecute(block_size, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_addr, max_acc_addr, concurrent_loops, latency))
  val st = Module(new LoopConvSt(block_size, coreMaxAddrBits, large_iterator_bitwidth, small_iterator_bitwidth, tiny_iterator_bitwidth, max_acc_addr, input_w, concurrent_loops, latency))

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
        loop_being_configured.outer_bounds.kernel_dilation := cmd.bits.rs2(31, 16)
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
        loop_being_configured.wrot180 := cmd.bits.rs1(1)
        loop_being_configured.trans_output_1203 := cmd.bits.rs1(2)
        loop_being_configured.trans_weight_1203 := cmd.bits.rs1(3)
        loop_being_configured.trans_weight_0132 := cmd.bits.rs1(4)
        loop_being_configured.trans_input_3120 := cmd.bits.rs1(5)

        loop_being_configured.no_pool := cmd.bits.rs2(0)
        loop_being_configured.downsample := cmd.bits.rs2(1)
        loop_being_configured.input_dilated := cmd.bits.rs2(2)

        loop_being_configured.configured := true.B

        // assert(!loop_being_configured.input_dilated || loop_being_configured.outer_bounds.stride === 1.U)
        // assert(!loop_being_configured.downsample || (loop_being_configured.outer_bounds.kernel_dim === 1.U && loop_being_configured.outer_bounds.stride === 2.U)) // TODO add the rest of the conditions that must be true for "downsample" to be enabled
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
  ld_input.io.req.bits.downsample := loop_requesting_ld_input.downsample
  ld_input.io.req.bits.input_dilated := loop_requesting_ld_input.input_dilated
  ld_input.io.req.bits.trans_input_3120 := loop_requesting_ld_input.trans_input_3120
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
  ld_weights.io.req.bits.trans_weight_1203 := loop_requesting_ld_weights.trans_weight_1203
  ld_weights.io.req.bits.trans_weight_0132 := loop_requesting_ld_weights.trans_weight_0132
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
  ex.io.req.bits.wrot180 := loop_requesting_ex.wrot180
  ex.io.req.bits.downsample := loop_requesting_ex.downsample
  ex.io.req.bits.input_dilated := loop_requesting_ex.input_dilated
  ex.io.req.bits.trans_weight_0132 := loop_requesting_ex.trans_weight_0132
  ex.io.req.bits.trans_input_3120 := loop_requesting_ex.trans_input_3120
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
  st.io.req.bits.trans_output_1203 := loop_requesting_st.trans_output_1203
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
  when (reset.asBool()) {
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

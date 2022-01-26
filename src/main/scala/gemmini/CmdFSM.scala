//===========================================================================
// Command FSM
// * Receive RoCC commands
// * Check validity of configuration data
// * Hand off to Tiling on COMPUTE requests
//===========================================================================
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import GemminiISA._

class CmdFSM[T <: Data: Arithmetic, U <: Data, V <: Data]
  (config: GemminiArrayConfig[T,U,V])(implicit val p: Parameters)
  extends Module with HasCoreParameters {
  import config._
  //==========================================================================
  // module ports
  //==========================================================================
  val io = IO(new Bundle {
    val cmd         = Flipped(Decoupled(new RoCCCommand))
    val tiler       = Decoupled(new TilerCmd(LOG2_OTYPE_BITS))
    val flush_retry = Output(Bool())
    val flush_skip  = Output(Bool())
    val busy        = Output(Bool())
  });

  //==========================================================================
  // FSM states
  //==========================================================================
  val (s_LISTENING :: s_EX_PENDING :: s_ERROR :: Nil) = Enum(3)
  val state = RegInit(s_LISTENING)

  //==========================================================================
  // local state registers
  //==========================================================================
  val m              = RegInit(0.U(32.W))
  val n              = RegInit(0.U(32.W))
  val k              = RegInit(0.U(32.W))
  val addr_a         = RegInit(0.U(xLen.W))
  val addr_b         = RegInit(0.U(xLen.W))
  val addr_c         = RegInit(0.U(xLen.W))
  val addr_d         = RegInit(0.U(xLen.W))
  val in_rshift      = RegInit(0.U(log2Up(accType.getWidth).W))
  val acc_rshift     = RegInit(0.U(log2Up(accType.getWidth).W))
  val relu6_lshift   = RegInit(0.U(log2Up(accType.getWidth).W))
  val activation     = RegInit(0.U(2.W))
  val repeating_bias = RegInit(0.U(1.W))

  // Valid fields
  val addr_ab_valid = RegInit(false.B)
  val addr_cd_valid = RegInit(false.B)
  val size0_valid = RegInit(false.B)
  val size1_valid = RegInit(false.B)
  val config_ex_valid = RegInit(false.B)
  val bias_valid = RegInit(false.B)

  // pass CSR status bits to tiler (TODO: fix this api)
  val status = Reg(new MStatus)
  status := DontCare

  //==========================================================================
  // Combinational Output Defaults
  //==========================================================================
  io.cmd.ready         := false.B
  io.tiler.valid       := false.B
  io.tiler.bits.status := status
  io.flush_retry       := false.B
  io.flush_skip        := false.B

  io.tiler.bits.m              := m
  io.tiler.bits.n              := n
  io.tiler.bits.k              := k
  io.tiler.bits.addr_a         := addr_a
  io.tiler.bits.addr_b         := addr_b
  io.tiler.bits.addr_c         := addr_c
  io.tiler.bits.addr_d         := addr_d
  io.tiler.bits.in_rshift      := in_rshift
  io.tiler.bits.acc_rshift     := acc_rshift
  io.tiler.bits.relu6_lshift   := relu6_lshift
  io.tiler.bits.activation     := activation
  io.tiler.bits.repeating_bias := repeating_bias

  // we block if we have accepted the COMPUTE_ALL command, but the tiler has
  // not started executing it yet.
  io.busy := (state === s_EX_PENDING)

  //==========================================================================
  // FSM
  //==========================================================================
  def reset_and_listen(): Unit = {
    // Reset all data-validity
    addr_ab_valid := false.B
    addr_cd_valid := false.B
    size0_valid := false.B
    size1_valid := false.B
    config_ex_valid := false.B
    bias_valid := false.B
    // And go back to listening for commands
    state := s_LISTENING
  }

  when (state === s_EX_PENDING) {
    // Pending s_EX ongoing
    // Wait for tiling/ execution to complete,
    // let any further commands queue up
    io.tiler.valid := true.B
    when (io.tiler.fire) {
      state := s_LISTENING
    }
  }.elsewhen (state === s_ERROR) {
    // In s_ERROR state - only update based on RESET commands
    io.cmd.ready := true.B
    when (io.cmd.fire) {
      val cmd = io.cmd.bits
      val funct = cmd.inst.funct
      when (funct === RESET) {
        reset_and_listen()
      } // All other commands are ignored
    }
  }.otherwise { // s_LISTENING State
    io.cmd.ready := true.B
    when (io.cmd.fire) {
      val cmd = io.cmd.bits
      val funct = cmd.inst.funct
      val rs1 = cmd.rs1
      val rs2 = cmd.rs2

      // always update status bits on a successful RoCC command
      status := cmd.status

      // Execute command
      when (funct === FLUSH_CMD) {
        val skip = cmd.rs1(0)
        io.flush_retry := !skip
        io.flush_skip  := skip
      }
      .elsewhen (funct === COMPUTE_CISC) {
        // Signal to the Tiler, and move to our EXEC state
        // FIXME: check all valid
        io.tiler.valid := true.B
        when (io.tiler.fire) {
          state := s_LISTENING
        }.otherwise {
          state := s_EX_PENDING
        }
      }
      .elsewhen (funct === CISC_CONFIG) {
        activation := rs1(4,3)
        acc_rshift := rs1(63,32)
        in_rshift := rs2(31,0)
        relu6_lshift := rs2(63,32)
        config_ex_valid := true.B
      }
      .elsewhen (funct === ADDR_AB) {
        addr_a := rs1
        addr_b := rs2
        addr_ab_valid := true.B
      }
      .elsewhen (funct === ADDR_CD) {
        addr_c := rs1
        addr_d := rs2
        addr_cd_valid := true.B
      }
      .elsewhen (funct === SIZE_MN) {
        m := rs1
        n := rs2
        size0_valid := true.B
      }
      .elsewhen (funct === SIZE_K) {
        k := rs1
        size1_valid := true.B
      }
      .elsewhen (funct === RPT_BIAS) {
        repeating_bias := rs1(0).asBool
        bias_valid := true.B
      }
      .elsewhen (funct === RESET) {
        reset_and_listen()
      }
      .otherwise {
        // Invalid command type
        // FIXME: error-cause setup
        state := s_ERROR
      }
    }
  }
}

object CmdFSM {
  def apply[T <: Data: Arithmetic, U <: Data, V <: Data]
    (config: GemminiArrayConfig[T,U,V])(implicit p: Parameters)
      = Module(new CmdFSM(config))
}

package gemmini.vpu

import chisel3._
import chisel3.util._

// VPU: BF16 vector engine on the Gemmini scratchpad. One lane row = one scratchpad row (lanes x BF16).
// A command streams `rows` scratchpad rows: dst[i] = op(src1[i], src2[i or i/rlen] | imm), or, for the row
// reductions, reduces each logical row of `rlen` scratchpad rows to one row (value in every lane) at dst[g].
object VpuOp {
  val ADD = 0;  val SUB = 1;  val MUL = 2       // src1 (op) src2
  val ADDS = 3; val MULS = 4                    // src1 (op) imm
  val EXP = 5;  val RCP = 6;  val RSQRT = 7     // unary
  val RMAX = 8; val RSUM = 9; val RAMAX = 10    // row reductions over rlen rows
  val width = 4
}

class VpuCmd(val addrW: Int) extends Bundle {
  val op    = UInt(VpuOp.width.W)
  val src1  = UInt(addrW.W)
  val src2  = UInt(addrW.W)
  val dst   = UInt(addrW.W)
  val rows  = UInt(16.W)     // scratchpad rows of src1 to process
  val rlen  = UInt(10.W)     // scratchpad rows per logical row (reductions / src2 broadcast)
  val bcast = Bool()         // src2 advances once per logical row (row-broadcast operand)
  val imm   = UInt(16.W)     // BF16 scalar for ADDS / MULS
}

object VpuCmd {
  // VPU_EXEC: rs1 = src1[13:0] | src2[27:14] | dst[41:28] | rows[57:42]
  //           rs2 = op[3:0] | bcast[4] | rlen[14:5] | imm[31:16]
  def decode(rs1: UInt, rs2: UInt, addrW: Int): VpuCmd = {
    val c = Wire(new VpuCmd(addrW))
    c.src1 := rs1(13, 0); c.src2 := rs1(27, 14); c.dst := rs1(41, 28); c.rows := rs1(57, 42)
    c.op := rs2(3, 0); c.bcast := rs2(4); c.rlen := rs2(14, 5); c.imm := rs2(31, 16)
    c
  }
}

// Fixed-latency scratchpad port: read data is valid exactly one cycle after rd(i).valid, never stalled;
// writes are always accepted. Addresses are full scratchpad row addresses.
class VpuWrite(val addrW: Int, val rowW: Int) extends Bundle {
  val addr = UInt(addrW.W)
  val data = UInt(rowW.W)
}

class VpuSpadIO(val addrW: Int, val rowW: Int) extends Bundle {
  val rd    = Vec(2, Valid(UInt(addrW.W)))
  val rdata = Input(Vec(2, UInt(rowW.W)))
  val wr    = Valid(new VpuWrite(addrW, rowW))
}

class Vpu(addrW: Int = 14, bankRowBits: Int = 12, lanes: Int = 8) extends Module {
  import VpuOp._
  val rowW = lanes * 16
  val io = IO(new Bundle {
    val cmd  = Flipped(Decoupled(new VpuCmd(addrW)))
    val spad = new VpuSpadIO(addrW, rowW)
    val busy = Output(Bool())
  })

  // ---------------- issue FSM ----------------
  val active = RegInit(false.B)
  val c      = Reg(new VpuCmd(addrW))
  val i      = Reg(UInt(16.W))   // src1 row
  val g      = Reg(UInt(16.W))   // logical row
  val j      = Reg(UInt(10.W))   // row within the logical row
  val phase  = RegInit(false.B)  // bank conflict: src2 read this cycle, src1 next

  val isRed   = c.op >= RMAX.U
  val useSrc2 = c.op <= MUL.U
  val needB   = useSrc2 && (!c.bcast || j === 0.U)
  val a_addr  = c.src1 + i
  val b_addr  = c.src2 + Mux(c.bcast, g, i)
  val conflict = needB && (a_addr >> bankRowBits) === (b_addr >> bankRowBits)
  val issueA  = active && (!conflict || phase)
  val issueB  = active && needB && (!conflict || !phase)
  val lastJ   = j === c.rlen - 1.U

  io.spad.rd(0).valid := issueA; io.spad.rd(0).bits := a_addr
  io.spad.rd(1).valid := issueB; io.spad.rd(1).bits := b_addr

  class Meta extends Bundle {
    val wrAddr = UInt(addrW.W)
    val first  = Bool()          // first row of a logical row (reductions)
    val last   = Bool()          // last row of a logical row (reductions write here)
    val bFresh = Bool()          // src2 for this row was read in the same cycle as src1
  }
  val meta = Wire(new Meta)
  meta.wrAddr := c.dst + Mux(isRed, g, i)
  meta.first := j === 0.U
  meta.last := lastJ
  meta.bFresh := issueA && issueB

  when (io.cmd.fire) {
    active := io.cmd.bits.rows =/= 0.U
    c := io.cmd.bits
    i := 0.U; g := 0.U; j := 0.U; phase := false.B
  } .elsewhen (active) {
    when (conflict && !phase) {
      phase := true.B
    } .otherwise {
      phase := false.B
      i := i + 1.U
      j := Mux(lastJ, 0.U, j + 1.U)
      g := Mux(lastJ, g + 1.U, g)
      when (i === c.rows - 1.U) { active := false.B }
    }
  }

  // ---------------- S1: operands (data arrives one cycle after the read) ----------------
  val s0_valid = RegNext(issueA, false.B)
  val s0_meta  = RegNext(meta)
  val s0_bRd   = RegNext(issueB, false.B)
  val bHold    = Reg(UInt(rowW.W))
  when (s0_bRd) { bHold := io.spad.rdata(1) }
  val opA = io.spad.rdata(0).asTypeOf(Vec(lanes, UInt(16.W)))
  val opB = Mux(s0_meta.bFresh, io.spad.rdata(1), bHold).asTypeOf(Vec(lanes, UInt(16.W)))

  val s1_valid = RegNext(s0_valid, false.B)
  val s1_meta  = RegNext(s0_meta)
  val s1_a     = RegNext(opA)
  val s1_b     = RegNext(opB)
  val op = c.op   // the command register is stable until the pipeline drains (cmd.ready below)

  // ---------------- stage A: elementwise ops, exp/rsqrt first half, per-row reduction tree ----------------
  val immVec = VecInit(Seq.fill(lanes)(c.imm))
  val bIn = Mux(op === ADDS.U || op === MULS.U, immVec, s1_b)
  val midA = VecInit((0 until lanes).map { l =>
    val a = s1_a(l); val b = bIn(l)
    val sum = VpuMath.add(a, b, op === SUB.U)
    val prd = VpuMath.mul(a, b)
    MuxLookup(op, a.pad(VpuMath.expMidW))(Seq(
      ADD.U -> sum.pad(VpuMath.expMidW), SUB.U -> sum.pad(VpuMath.expMidW), ADDS.U -> sum.pad(VpuMath.expMidW),
      MUL.U -> prd.pad(VpuMath.expMidW), MULS.U -> prd.pad(VpuMath.expMidW),
      EXP.U -> VpuMath.expStageA(a),
      RSQRT.U -> VpuMath.sqrt(a).pad(VpuMath.expMidW)))
  })
  // row reduction across the lanes of one scratchpad row
  def tree[T](xs: Seq[T], f: (T, T) => T): T = if (xs.size == 1) xs.head else tree(xs.grouped(2).map(p => f(p(0), p(1))).toSeq, f)
  val laneIn = Mux(op === RAMAX.U, VecInit(s1_a.map(VpuMath.abs)), s1_a)
  val rowMax = tree(laneIn.toSeq, VpuMath.max)
  val rowSum = tree(s1_a.map(VpuMath.bf16ToF32Rec).toSeq, VpuMath.addF32Rec)

  val s2_valid = RegNext(s1_valid, false.B)
  val s2_meta  = RegNext(s1_meta)
  val s2_mid   = RegNext(midA)
  val s2_rmax  = RegNext(rowMax)
  val s2_rsum  = RegNext(rowSum)

  // ---------------- stage B: exp / rcp finish, reduction accumulate ----------------
  val outB = VecInit((0 until lanes).map { l =>
    val m = s2_mid(l)
    MuxLookup(op, m(15, 0))(Seq(
      EXP.U -> VpuMath.expStageB(m),
      RCP.U -> VpuMath.rcp(m(15, 0)),
      RSQRT.U -> VpuMath.rcp(m(15, 0))))
  })
  val accMax = Reg(UInt(16.W))
  val accSum = Reg(UInt(33.W))
  val newMax = Mux(s2_meta.first, s2_rmax, VpuMath.max(accMax, s2_rmax))
  val newSum = Mux(s2_meta.first, s2_rsum, VpuMath.addF32Rec(accSum, s2_rsum))
  when (s2_valid) { accMax := newMax; accSum := newSum }
  val redOut = Mux(op === RSUM.U, VpuMath.f32RecToBf16(newSum), newMax)

  val wrValid = s2_valid && (!isRed || s2_meta.last)
  io.spad.wr.valid := RegNext(wrValid, false.B)
  io.spad.wr.bits.addr := RegNext(s2_meta.wrAddr)
  io.spad.wr.bits.data := RegNext(Mux(isRed, Fill(lanes, redOut), outB.asUInt))

  val inFlight = s0_valid || s1_valid || s2_valid || io.spad.wr.valid
  io.cmd.ready := !active && !inFlight
  io.busy := active || inFlight
}

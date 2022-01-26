//===========================================================================
// TilerController's Internal Scheduler implementation
//===========================================================================
package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import GemminiISA._
import Util._

class TilerScheduler[T <: Data: Arithmetic, U <: Data, V <: Data]
  (config: GemminiArrayConfig[T,U,V])(implicit val p: Parameters)
  extends Module with HasCoreParameters {
  import config._

  //=========================================================================
  // interface
  //=========================================================================
  val io = IO(new Bundle {
    val cmd_in = Flipped(Decoupled(new RoCCCommand))
    val issue = new Bundle {
      val exec  = Decoupled(new GemminiCmd(ROB_ENTRIES))
      val load  = Decoupled(new GemminiCmd(ROB_ENTRIES))
      val store = Decoupled(new GemminiCmd(ROB_ENTRIES))
    }
    val completed = Flipped(Decoupled(UInt(LOG2_ROB_ENTRIES.W)))
    val busy = Output(Bool())
  })
  io.completed.ready := true.B

  //=========================================================================
  // ...
  //=========================================================================
  val ldq :: stq :: exq :: Nil = Enum(3)
  val q_t = ldq.cloneType

  val MAX_CMD_ID   = 1000000            // FOR DEBUGGING ONLY
  val debug_cycle  = RegInit(0.U(32.W)) // FOR DEBUGGING ONLY

  debug_cycle := debug_cycle + 1.U      // FOR DEBUGGING ONLY
  val cmd_id = Counter(MAX_CMD_ID)      // FOR DEBUGGING ONLY

  // scratchpad range, used for tracking RAW, WAR, and WAW deps
  class SPRange extends Bundle {
    val valid  = Bool()
    val is_acc = Bool()
    val start  = UInt(30.W) // TODO magic number
    val end    = UInt(30.W) // TODO magic number
    def overlaps(other: SPRange) = valid && other.valid &&
                                   (is_acc === other.is_acc) &&
                                   (start < other.end) &&
                                   (end > other.start)
  }

  class Entry extends Bundle {
    val q = q_t.cloneType

    val is_config = Bool()

    val cmd_id     = UInt(log2Up(MAX_CMD_ID).W)   // FOR DEBUGGING ONLY
    val is_load    = Bool()  // also true on config_load.  FOR DEBUGGING ONLY
    val is_store   = Bool()  // also true on config_store. FOR DEBUGGING ONLY
    val is_exec    = Bool()  // also true on config_ex.    FOR DEBUGGING ONLY
    val is_preload = Bool()  // FOR DEBUGGING ONLY

    val op1 = new SPRange()
    val op2 = new SPRange()
    val dst = new SPRange()

    val issued = Bool()

    val complete_on_issue = Bool()

    val cmd = new RoCCCommand

    val deps = Vec(ROB_ENTRIES, Bool())
    def ready(dummy: Int = 0): Bool = !deps.reduce(_ || _)
  }

  val entries = Reg(Vec(ROB_ENTRIES, UDValid(new Entry)))

  val empty = !entries.map(_.valid).reduce(_ || _)
  val full = entries.map(_.valid).reduce(_ && _)

  io.busy := !empty

  // Read in commands to the buffer
  io.cmd_in.ready := !full

  val last_allocated = Reg(UInt(log2Up(ROB_ENTRIES).W))

  val new_entry = Wire(new Entry)
  new_entry := DontCare
  val new_entry_id = MuxCase((ROB_ENTRIES-1).U, entries.zipWithIndex.map {
                                        case (e, i) => !e.valid -> i.U })
  val alloc_fire = io.cmd_in.fire

  when (io.cmd_in.fire) {
    val cmd = io.cmd_in.bits
    val funct = cmd.inst.funct
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD ||
                           funct === COMPUTE_AND_FLIP_CMD
    val funct_is_compute_preload = funct === COMPUTE_AND_FLIP_CMD
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    new_entry.issued := false.B
    new_entry.cmd := cmd

    new_entry.is_config := funct === CONFIG_CMD

    new_entry.op1.valid  := funct === PRELOAD_CMD || funct_is_compute
    new_entry.op1.is_acc := cmd.rs1(31)
    new_entry.op1.start  := cmd.rs1(29,0)
    new_entry.op1.end    := cmd.rs1(29,0) + DIM.U

    val mvin_mvout_rows = cmd.rs2(63, 48)
    val mvin_mvout_cols = cmd.rs2(47, 32)

    new_entry.op2.valid := funct_is_compute || funct === STORE_CMD
    new_entry.op2.is_acc := cmd.rs2(31)
    new_entry.op2.start := cmd.rs2(29,0)
    new_entry.op2.end   := cmd.rs2(29,0) +
                           Mux(funct_is_compute, DIM.U, mvin_mvout_rows)

    new_entry.dst.valid := funct === PRELOAD_CMD || funct === LOAD_CMD
    new_entry.dst.is_acc := cmd.rs2(31)
    new_entry.dst.start := cmd.rs2(29,0)
    new_entry.dst.end   := cmd.rs2(29,0) +
                           Mux(funct === PRELOAD_CMD, DIM.U,
                            mvin_mvout_rows)

    val is_load    = (funct === LOAD_CMD) ||
                     (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store   = (funct === STORE_CMD) ||
                     (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_exec    = funct === PRELOAD_CMD ||
                     funct_is_compute ||
                     (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    val is_preload = funct === PRELOAD_CMD

    // never allow this to wrap.FOR DEBUGGING ONLY
    assert(!cmd_id.inc())
    new_entry.cmd_id     := cmd_id.value // FOR DEBUGGING ONLY
    new_entry.is_load    := is_load      // FOR DEBUGGING ONLY
    new_entry.is_store   := is_store     // FOR DEBUGGING ONLY
    new_entry.is_exec    := is_exec      // FOR DEBUGGING ONLY
    new_entry.is_preload := is_preload   // FOR DEBUGGING ONLY
    //======================================================================
    // debug
    //======================================================================
    when(new_entry.is_config) {
      when (new_entry.is_load) {
        printf(
          "cycle[%d], entry[%d], accept[%d], config_mvin[stride=%x]\n",
          debug_cycle, new_entry_id, cmd_id.value,
          new_entry.cmd.rs2)
      }
      .elsewhen (new_entry.is_store) {
        printf(
          "cycle[%d], entry[%d], accept[%d], config_mvout[stride=%x]\n",
          debug_cycle, new_entry_id, cmd_id.value,
          new_entry.cmd.rs2)
      }
      .otherwise {
        assert(new_entry.is_exec)
        printf(
          "cycle[%d], entry[%d], accept[%d], " +
          "config_ex[matmul_rshift=%x, acc_rshift=%x, relu6_lshift=%x]\n",
          debug_cycle, new_entry_id, cmd_id.value,
          cmd.rs1(63,32), cmd.rs2(31,0), cmd.rs2(63,32))
      }
    }
    .elsewhen (new_entry.is_load) {
      printf(
        "cycle[%d], entry[%d], accept[%d], " +
        "mvin[dram=%x, spad=%x, rows=%x, cols=%x]\n",
        debug_cycle, new_entry_id, cmd_id.value,
        cmd.rs1, cmd.rs2(31,0), cmd.rs2(63,48), cmd.rs2(47,32))
    }
    .elsewhen (new_entry.is_store) {
      printf(
        "cycle[%d], entry[%d], accept[%d], " +
        "mvout[dram=%x, spad=%x, rows=%x, cols=%x]\n",
        debug_cycle, new_entry_id, cmd_id.value,
        cmd.rs1, cmd.rs2(31,0), cmd.rs2(63,48), cmd.rs2(47,32))
    }
    .elsewhen (new_entry.is_preload) {
      printf(
        "cycle[%d], entry[%d], accept[%d], preload[B=%x, C=%x]\n",
        debug_cycle, new_entry_id, cmd_id.value,
        cmd.rs1(31,0), cmd.rs2(31,0))
    }
    .otherwise {
      assert(new_entry.is_exec)
      when (funct_is_compute_preload) {
        printf(
          "cycle[%d], entry[%d], accept[%d], ex.pre[A=%x, D=%x]\n",
          debug_cycle, new_entry_id, cmd_id.value,
          cmd.rs1(31,0), cmd.rs2(31,0))
      }
      .otherwise {
        printf(
          "cycle[%d], entry[%d], accept[%d], ex.acc[A=%x, D=%x]\n",
          debug_cycle, new_entry_id, cmd_id.value,
          cmd.rs1(31,0), cmd.rs2(31,0))
      }
    }

    //======================================================================
    new_entry.q := Mux1H(Seq(
      is_load  -> ldq,
      is_store -> stq,
      is_exec  -> exq,
    ))

    // We search for all entries which write to an address which we read from
    val raws = entries.map { e => e.valid && (
      e.bits.dst.overlaps(new_entry.op1) ||
      e.bits.dst.overlaps(new_entry.op2)
    )}

    // We search for all entries which read from an address that we write to
    val wars = entries.map { e => e.valid && (
      new_entry.dst.overlaps(e.bits.op1) ||
      new_entry.dst.overlaps(e.bits.op2)
    )}

    // We search for all entries which write to an address that we write to
    val waws = entries.map { e => e.valid &&
      new_entry.dst.overlaps(e.bits.dst)
    }

    val older_in_same_q = entries.map { e => e.valid &&
      e.bits.q === new_entry.q &&
      !e.bits.issued
    }

    val is_st_and_must_wait_for_prior_ex_config = entries.map { e =>
      (e.valid && e.bits.q === exq && e.bits.is_config) &&
      (new_entry.q === stq && !new_entry.is_config)
    }

    val is_ex_config_and_must_wait_for_prior_st = entries.map { e =>
      (e.valid && e.bits.q === stq && !e.bits.is_config) &&
      (new_entry.q === exq && new_entry.is_config)
    }

    new_entry.deps := (Cat(raws) |
                       Cat(wars) |
                       Cat(waws) |
                       Cat(older_in_same_q) |
                       Cat(is_st_and_must_wait_for_prior_ex_config) |
                       Cat(is_ex_config_and_must_wait_for_prior_st)
                      ).asBools().reverse

    new_entry.complete_on_issue := new_entry.is_config && new_entry.q =/= exq

    entries(new_entry_id).valid := true.B
    entries(new_entry_id).bits := new_entry

    last_allocated := new_entry_id
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.load),
      (stq, io.issue.store),
      (exq, io.issue.exec)).foreach { case (q, io) =>
    val issue_id = MuxCase((ROB_ENTRIES-1).U, entries.zipWithIndex.map {
      case (e, i) => (e.valid && e.bits.ready() &&
                      !e.bits.issued && e.bits.q === q) -> i.U
    })
    io.valid := entries.map(e => e.valid && e.bits.ready() && !e.bits.issued
                                 && e.bits.q === q).reduce(_ || _)
    io.bits.cmd := entries(issue_id).bits.cmd
    io.bits.rob_id.push(issue_id)

    // ssteff: added for debug
    when(io.fire) {
      //======================================================================
      // debug
      //======================================================================
      when(entries(issue_id).bits.is_config) {
        when (entries(issue_id).bits.is_load) {
          printf(
            "cycle[%d], entry[%d],  issue[%d], config_mvin\n",
            debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
          printf(
            "cycle[%d], entry[%d],  final[%d], config_mvin\n",
            debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
        }
        .elsewhen (entries(issue_id).bits.is_store) {
          printf(
            "cycle[%d], entry[%d],  issue[%d], config_mvout\n",
            debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
          printf(
            "cycle[%d], entry[%d],  final[%d], config_mvout\n",
            debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
        }
        .otherwise {
          assert(entries(issue_id).bits.is_exec)
          printf(
            "cycle[%d], entry[%d],  issue[%d], config_ex\n",
            debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
        }
      }
      .elsewhen (entries(issue_id).bits.is_load) {
        printf(
          "cycle[%d], entry[%d],  issue[%d], mvin\n",
          debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
      }
      .elsewhen (entries(issue_id).bits.is_store) {
        printf(
          "cycle[%d], entry[%d],  issue[%d], mvout\n",
          debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
      }
      .elsewhen (entries(issue_id).bits.is_preload) {
        printf(
          "cycle[%d], entry[%d],  issue[%d], preload\n",
          debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
      }
      .otherwise {
        assert(entries(issue_id).bits.is_exec)
        printf(
          "cycle[%d], entry[%d],  issue[%d], ex\n",
          debug_cycle, issue_id, entries(issue_id).bits.cmd_id)
      }
      //======================================================================

      entries(issue_id).bits.issued := true.B

      // Clear out all the dependency bits for instructions which
      // depend on the same queue
      entries.zipWithIndex.foreach { case (e, i) =>
        val is_same_q = Mux(alloc_fire && new_entry_id === i.U,
          new_entry.q === entries(issue_id).bits.q,
          e.bits.q === entries(issue_id).bits.q)

        when (is_same_q || entries(issue_id).bits.complete_on_issue) {
          e.bits.deps(issue_id) := false.B
        }
      }

      entries(issue_id).valid := !entries(issue_id).bits.complete_on_issue
    }
  }

  // Mark entries as completed once they've returned
  when (io.completed.fire) {
    //======================================================================
    // debug
    //======================================================================
    //  entries(io.completed.bits).bits.cmd.rs2))
    when (entries(io.completed.bits).bits.is_config) {
      assert(entries(io.completed.bits).bits.is_exec)
      printf(
        "cycle[%d], entry[%d],  final[%d], config_ex\n",
        debug_cycle, io.completed.bits,
        entries(io.completed.bits).bits.cmd_id)
    }
    .elsewhen (entries(io.completed.bits).bits.is_load) {
      printf(
        "cycle[%d], entry[%d],  final[%d], mvin\n",
        debug_cycle, io.completed.bits,
        entries(io.completed.bits).bits.cmd_id)
    }
    .elsewhen (entries(io.completed.bits).bits.is_store) {
      printf(
        "cycle[%d], entry[%d],  final[%d], mvout\n",
        debug_cycle, io.completed.bits,
        entries(io.completed.bits).bits.cmd_id)
    }
    .elsewhen (entries(io.completed.bits).bits.is_preload) {
      printf(
        "cycle[%d], entry[%d],  final[%d], preload\n",
        debug_cycle, io.completed.bits,
        entries(io.completed.bits).bits.cmd_id)
    }
    .otherwise {
      assert(entries(io.completed.bits).bits.is_exec)
      printf(
        "cycle[%d], entry[%d],  final[%d], ex\n",
        debug_cycle, io.completed.bits,
        entries(io.completed.bits).bits.cmd_id)
    }
    //======================================================================

    entries.foreach(_.bits.deps(io.completed.bits) := false.B)

    entries(io.completed.bits).valid := false.B
    assert(entries(io.completed.bits).valid)
  }

  val util = PopCount(entries.map(e => e.valid))
  val util_ld_q_unissued = PopCount(entries.map(e => e.valid &&
                                                     !e.bits.issued &&
                                                     e.bits.q === ldq))
  val util_st_q_unissued = PopCount(entries.map(e => e.valid &&
                                                     !e.bits.issued &&
                                                     e.bits.q === stq))
  val util_ex_q_unissued = PopCount(entries.map(e => e.valid &&
                                                     !e.bits.issued &&
                                                     e.bits.q === exq))
  val util_ld_q = PopCount(entries.map(e => e.valid && e.bits.q === ldq))
  val util_st_q = PopCount(entries.map(e => e.valid && e.bits.q === stq))
  val util_ex_q = PopCount(entries.map(e => e.valid && e.bits.q === exq))

  val packed_deps = VecInit(entries.map(e => Cat(e.bits.deps)))
  dontTouch(packed_deps)

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid, PopCount(e.bits.deps), 0.U)))
  val min_pop_count = pop_count_packed_deps.reduce((acc, d) => minOf(acc, d))
  // assert(min_pop_count < 2.U)
  dontTouch(pop_count_packed_deps)
  dontTouch(min_pop_count)

  val cycles_since_issue = RegInit(0.U(32.W))

  when (io.issue.load.fire ||
        io.issue.store.fire ||
        io.issue.exec.fire ||
        !io.busy) {
    cycles_since_issue := 0.U
  } .elsewhen (io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < 10000.U, "pipeline stall")

  val cntr = Counter(10000000)
  when (cntr.inc()) {
    printf(p"Utilization: $util\n")
    printf(p"Utilization ld q (incomplete): $util_ld_q_unissued\n")
    printf(p"Utilization st q (incomplete): $util_st_q_unissued\n")
    printf(p"Utilization ex q (incomplete): $util_ex_q_unissued\n")
    printf(p"Utilization ld q: $util_ld_q\n")
    printf(p"Utilization st q: $util_st_q\n")
    printf(p"Utilization ex q: $util_ex_q\n")
    printf(p"Packed deps: $packed_deps\n")
    printf(p"Last allocated: $last_allocated\n\n")
  }

  when (reset.asBool()) {
    entries.foreach(_.valid := false.B)
  }
}

object TilerScheduler {
  def apply[T <: Data: Arithmetic, U <: Data, V <: Data]
    (config: GemminiArrayConfig[T,U,V])(implicit p: Parameters)
      = Module(new TilerScheduler(config))
}

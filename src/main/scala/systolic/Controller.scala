package systolic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import SystolicISA._

case class SystolicArrayConfig[T <: Data : Arithmetic] (
  tileRows: Int,
  tileColumns: Int,
  meshRows: Int,
  meshColumns: Int,
  ld_str_queue_length: Int,
  ex_queue_length: Int,
  sp_banks: Int,
  sp_bank_entries: Int,
  sp_width: Int,
  shifter_banks: Int,
  depq_len: Int,
  dataflow: Dataflow.Value,
  acc_rows: Int,
  mem_pipeline: Int,
  dma_maxbytes: Int,
  dma_buswidth: Int,
  inputType: T,
  outputType: T,
  accType: T,
  headerFileName: String = "systolic_params.h"
) {
  def generateHeader(guard: String = "SYSTOLIC_PARAMS_H"): String = {
    // Returns the (min,max) values for a dataType
    def limitsOfDataType(dataType: Data): (String, String) = {
      assert(dataType.getWidth <= 32) // Above 32 bits, we need to append UL to the number, which isn't done yet
      if (dataType.isInstanceOf[UInt]) {
        ("0", BigInt(2).pow(dataType.getWidth).-(1).toString)
      } else if (dataType.isInstanceOf[SInt]) {
        ("-" + BigInt(2).pow(dataType.getWidth - 1).toString ,BigInt(2).pow(dataType.getWidth - 1).-(1).toString)
      } else {
        throw new IllegalArgumentException(s"Data type $dataType isn't an integer")
      }
    }

    assert(tileColumns*meshColumns == tileRows*meshRows)
    assert(Set(8, 16, 32, 64).contains(inputType.getWidth))
    // assert(Set(8, 16, 32, 64).contains(outputType.getWidth))
    assert(Set(8, 16, 32, 64).contains(accType.getWidth))

    val header = new StringBuilder()
    header ++= s"#ifndef $guard\n"
    header ++= s"#define $guard\n\n"

    header ++= s"#include <stdint.h>\n"
    header ++= s"#include <limits.h>\n\n"

    header ++= s"#define DIM ${tileColumns*meshColumns}\n"
    header ++= s"#define ADDR_LEN 32\n"
    header ++= s"#define BANK_NUM $sp_banks\n"
    header ++= s"#define BANK_ROWS $sp_bank_entries\n" // TODO: check
    header ++= s"#define ACC_ROWS $acc_rows\n" // TODO: check
    header ++= s"#define MAX_BYTES 64\n"
    header ++= s"#define MAX_BLOCK_LEN (MAX_BYTES/(DIM*${inputType.getWidth/8}))\n"
    header ++= s"#define MAX_BLOCK_LEN_ACC (MAX_BYTES/(DIM*${accType.getWidth/8}))\n\n"

    header ++= s"#define MAX_Q_LEN $depq_len\n\n"

    // Datatype of the systolic array
    val limits = limitsOfDataType(inputType)
    header ++= s"typedef int${inputType.getWidth}_t elem_t;\n"
    header ++= s"elem_t elem_t_max = ${limits._2};\n"
    header ++= s"elem_t elem_t_min = ${limits._1};\n"
    header ++= s"typedef int${accType.getWidth}_t acc_t;\n\n"

    header ++= s"#define row_align(blocks) __attribute__((aligned(blocks*DIM*sizeof(elem_t))))\n"
    header ++= s"#define row_align_acc(blocks) __attribute__((aligned(blocks*DIM*sizeof(acc_t))))\n\n"


    header ++= s"#endif // $guard"
    header.toString()
  }
}

class SystolicDeps extends Bundle {
  val pushStore = Bool()
  val pushLoad = Bool()
  val pushEx = Bool()
  val pullStore = Bool()
  val pullLoad = Bool()
  val pullEx = Bool()
}

class SystolicCmdWithDeps(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val deps = new SystolicDeps

  override def cloneType: this.type = (new SystolicCmdWithDeps).asInstanceOf[this.type]
}

class SPAddr(xLen: Int, sp_banks: Int, sp_bank_entries: Int) extends Bundle {
  val junk = UInt((xLen-log2Ceil(sp_bank_entries)-log2Ceil(sp_banks)).W)
  val bank = UInt(log2Ceil(sp_banks).W)
  val row = UInt(log2Ceil(sp_bank_entries).W)

  override def cloneType: this.type = new SPAddr(xLen, sp_banks, sp_bank_entries).asInstanceOf[this.type]
}

class AccAddr(xLen: Int, acc_rows: Int, tagWidth: Int) extends Bundle {
  val junk = UInt((xLen - tagWidth - log2Ceil(acc_rows)).W)
  val is_acc_addr = Bool()
  val acc = Bool()
  val junk2 = UInt((tagWidth - log2Ceil(acc_rows)-2).W)
  val row = UInt(log2Ceil(acc_rows).W)

  override def cloneType: this.type = new AccAddr(xLen, acc_rows, tagWidth).asInstanceOf[this.type]
}

class SystolicArray[T <: Data : Arithmetic](opcodes: OpcodeSet, val config: SystolicArrayConfig[T])
                                           (implicit p: Parameters)
  extends LazyRoCC (
    opcodes = OpcodeSet.custom3,
    nPTWPorts = 1) {

  Files.write(Paths.get(config.headerFileName), config.generateHeader().getBytes(StandardCharsets.UTF_8))

  val xLen = p(XLen)
  val spad = LazyModule(new Scratchpad(
    config.sp_banks, config.sp_bank_entries, config.sp_width,
    new SPAddr(xLen, config.sp_banks, config.sp_bank_entries) // TODO unify this with the other sp_addr
    , config))
  override lazy val module = new SystolicArrayModule(this)
  override val tlNode = spad.id_node
}

// TODO add WS support
class SystolicArrayModule[T <: Data: Arithmetic]
    (outer: SystolicArray[T])
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val sp_addr_t = new SPAddr(xLen, sp_banks, sp_bank_entries)
  val funct_t = new Bundle {
    val push1 = UInt(1.W)
    val pop1 = UInt(1.W)
    val push2 = UInt(1.W)
    val pop2 = UInt(1.W)
    val funct = UInt(3.W)
  }

  val tagWidth = 32
  val acc_addr = new AccAddr(xLen, acc_rows, tagWidth)

  // TLB
  implicit val edge = outer.tlNode.edges.out.head
  val tlb = Module(new FrontendTLB(1, 4))
  tlb.io.clients(0) <> outer.spad.module.io.tlb
  tlb.io.exp.flush_skip := false.B
  tlb.io.exp.flush_retry := false.B
  io.ptw.head <> tlb.io.ptw

  // Controllers
  val cmd = io.cmd
  cmd.ready := false.B

  val funct = cmd.bits.inst.funct.asTypeOf(funct_t).funct
  val push1 = cmd.bits.inst.funct.asTypeOf(funct_t).push1
  val pop1 = cmd.bits.inst.funct.asTypeOf(funct_t).pop1
  val push2 = cmd.bits.inst.funct.asTypeOf(funct_t).push2
  val pop2 = cmd.bits.inst.funct.asTypeOf(funct_t).pop2

  val load_controller = Module(new LoadController(outer.config, xLen, sp_addr_t, acc_addr))
  val store_controller = Module(new StoreController(outer.config, xLen, sp_addr_t, acc_addr))
  val ex_controller = Module(new ExecuteController(xLen, tagWidth, outer.config, sp_addr_t, acc_addr))

  val dma_arbiter = Module(new DMAArbiter(sp_banks, sp_bank_entries, acc_rows))

  val load_to_store_depq = Queue(load_controller.io.pushStore, depq_len)
  val load_to_ex_depq = Queue(load_controller.io.pushEx, depq_len)
  val store_to_load_depq = Queue(store_controller.io.pushLoad, depq_len)
  val store_to_ex_depq = Queue(store_controller.io.pushEx, depq_len)
  val (ex_to_load_depq, ex_to_load_depq_len) = MultiHeadedQueue(ex_controller.io.pushLoad, depq_len, 1)
  val (ex_to_store_depq, ex_to_store_depq_len) = MultiHeadedQueue(ex_controller.io.pushStore, depq_len, 1)
  
  // Wire up commands to controllers
  load_controller.io.cmd.valid := false.B
  load_controller.io.cmd.bits.cmd := cmd.bits
  load_controller.io.cmd.bits.cmd.inst.funct := funct
  load_controller.io.cmd.bits.deps.pushLoad := false.B
  load_controller.io.cmd.bits.deps.pullLoad := false.B
  load_controller.io.cmd.bits.deps.pushStore := push1
  load_controller.io.cmd.bits.deps.pullStore := pop1
  load_controller.io.cmd.bits.deps.pushEx := push2
  load_controller.io.cmd.bits.deps.pullEx := pop2

  store_controller.io.cmd.valid := false.B
  store_controller.io.cmd.bits.cmd := cmd.bits
  store_controller.io.cmd.bits.cmd.inst.funct := funct
  store_controller.io.cmd.bits.deps.pushLoad := push1
  store_controller.io.cmd.bits.deps.pullLoad := pop1
  store_controller.io.cmd.bits.deps.pushStore := false.B
  store_controller.io.cmd.bits.deps.pullStore := false.B
  store_controller.io.cmd.bits.deps.pushEx := push2
  store_controller.io.cmd.bits.deps.pullEx := pop2

  ex_controller.io.cmd.valid := false.B
  ex_controller.io.cmd.bits.cmd := cmd.bits
  ex_controller.io.cmd.bits.cmd.inst.funct := funct
  ex_controller.io.cmd.bits.deps.pushLoad := push1
  ex_controller.io.cmd.bits.deps.pullLoad := pop1
  ex_controller.io.cmd.bits.deps.pushStore := push2
  ex_controller.io.cmd.bits.deps.pullStore := pop2
  ex_controller.io.cmd.bits.deps.pushEx := false.B
  ex_controller.io.cmd.bits.deps.pullEx := false.B
  ex_controller.io.pushLoadLeft := depq_len.U - ex_to_load_depq_len
  ex_controller.io.pushStoreLeft := depq_len.U - ex_to_store_depq_len

  // Wire up scratchpad to controllers
  spad.module.io.dma <> dma_arbiter.io.dma
  dma_arbiter.io.load <> load_controller.io.dma
  dma_arbiter.io.store <> store_controller.io.dma
  ex_controller.io.read <> spad.module.io.read
  ex_controller.io.write <> spad.module.io.write
  ex_controller.io.acc <> spad.module.io.acc

  // Wire up controllers to dependency queues
  load_controller.io.pullStore <> store_to_load_depq
  load_controller.io.pullEx.valid := ex_to_load_depq.valid(0)
  load_controller.io.pullEx.bits := ex_to_load_depq.bits(0)
  ex_to_load_depq.pop := load_controller.io.pullEx.ready
  
  store_controller.io.pullLoad <> load_to_store_depq
  store_controller.io.pullEx.valid := ex_to_store_depq.valid(0)
  store_controller.io.pullEx.bits := ex_to_store_depq.bits(0)
  ex_to_store_depq.pop := store_controller.io.pullEx.ready
  
  ex_controller.io.pullLoad <> load_to_ex_depq
  ex_controller.io.pullStore <> store_to_ex_depq

  // Wire up global RoCC signals
  io.busy := load_controller.io.busy || store_controller.io.busy
  io.interrupt := tlb.io.exp.interrupt

  // Issue commands to controllers
  // TODO we combinationally couple cmd.ready and cmd.valid signals here
  when (cmd.valid) {
    val config_cmd_type = cmd.bits.rs1(1,0) // TODO magic numbers

    val is_flush = funct === FLUSH_CMD
    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = (funct === COMPUTE_AND_FLIP_CMD || funct === COMPUTE_AND_STAY_CMD || funct === PRELOAD_CMD) ||
      (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)

    when (is_flush) {
      val skip = cmd.bits.rs1(0)
      tlb.io.exp.flush_skip := skip
      tlb.io.exp.flush_retry := !skip

      cmd.ready := true.B // TODO should we wait for an acknowledgement from the TLB?
    }

    .elsewhen (is_load) {
      load_controller.io.cmd.valid := true.B

      when (load_controller.io.cmd.fire()) {
        cmd.ready := true.B
      }
    }

    .elsewhen (is_store) {
      store_controller.io.cmd.valid := true.B

      when (store_controller.io.cmd.fire()) {
        cmd.ready := true.B
      }
    }

    .otherwise {
      ex_controller.io.cmd.valid := true.B

      when (ex_controller.io.cmd.fire()) {
        cmd.ready := true.B
      }

      assert(is_ex, "unknown systolic command")
    }
  }
}

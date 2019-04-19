package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import SystolicISA._
import Util._

case class SystolicArrayConfig(
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
                                acc_rows: Int = 32,
                              )
case object SystolicArrayKey extends Field[SystolicArrayConfig]

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

  override def cloneType: SystolicCmdWithDeps.this.type = (new SystolicCmdWithDeps).asInstanceOf[this.type]
}

class SPAddr(xLen: Int, sp_banks: Int, sp_bank_entries: Int) extends Bundle {
  val junk = UInt((xLen-log2Ceil(sp_bank_entries)-log2Ceil(sp_banks)).W)
  val bank = UInt(log2Ceil(sp_banks).W)
  val row = UInt(log2Ceil(sp_bank_entries).W)

  override def cloneType: SPAddr.this.type = new SPAddr(xLen, sp_banks, sp_bank_entries).asInstanceOf[this.type]
}

class SystolicArray[T <: Data: Arithmetic](inputType: T, outputType: T, accType: T, opcodes: OpcodeSet)
                                          (implicit p: Parameters) extends LazyRoCC (
    opcodes = OpcodeSet.custom3,
    nPTWPorts = 1) {
  val config = p(SystolicArrayKey)
  val spad = LazyModule(new Scratchpad(
    config.sp_banks, config.sp_bank_entries, config.sp_width, accType, config))
  override lazy val module = new SystolicArrayModule(this, inputType, outputType, accType)
  override val tlNode = spad.node
}

// TODO add WS support
class SystolicArrayModule[T <: Data: Arithmetic]
    (outer: SystolicArray[T], inputType: T, outputType: T, accType: T)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val sp_addr = new SPAddr(xLen, sp_banks, sp_bank_entries)
  val funct_t = new Bundle {
    val push1 = UInt(1.W)
    val pop1 = UInt(1.W)
    val push2 = UInt(1.W)
    val pop2 = UInt(1.W)
    val funct = UInt(3.W)
  }

  // TLB
  implicit val edge = outer.tlNode.edges.out.head
  val tlb = Module(new FrontendTLB(1, 4))
  tlb.io.clients(0) <> outer.spad.module.io.tlb
  io.ptw.head <> tlb.io.ptw

  // Controllers
  val cmd = io.cmd
  cmd.ready := false.B

  val funct = cmd.bits.inst.funct.asTypeOf(funct_t).funct
  val push1 = cmd.bits.inst.funct.asTypeOf(funct_t).push1
  val pop1 = cmd.bits.inst.funct.asTypeOf(funct_t).pop1
  val push2 = cmd.bits.inst.funct.asTypeOf(funct_t).push2
  val pop2 = cmd.bits.inst.funct.asTypeOf(funct_t).pop2

  val load_controller = Module(new LoadController(outer.config, xLen, sp_addr))
  val store_controller = Module(new StoreController(outer.config, xLen, sp_addr))
  val ex_controller = Module(new ExecuteController(xLen, outer.config, sp_addr, inputType, outputType, accType))

  val dma_arbiter = Module(new DMAArbiter(sp_banks, sp_bank_entries))

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
  ex_controller.io.acc_read <> spad.module.io.acc_read
  ex_controller.io.acc_write <> spad.module.io.acc_write

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

  // Wire up busy signals
  io.busy := load_controller.io.busy || store_controller.io.busy || ex_controller.io.busy

  // Issue commands to controllers
  when (cmd.valid) {
    val config_cmd_type = cmd.bits.rs1(1,0)

    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)

    when (is_load) {
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
    }
  }
}

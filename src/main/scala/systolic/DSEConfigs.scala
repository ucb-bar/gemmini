package systolic

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}

// -----------------------------
// Design Space Exploration Mixins
// -----------------------------

object DSEBaseConfig {
  val baseConfig = SystolicArrayConfig(
    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,
    ld_str_queue_length = 10,
    ex_queue_length = 10,
    sp_banks = 4,
    sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
    sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    depq_len = 65536,
    dataflow = Dataflow.OS,
    acc_rows = 16 * 1024 * 8 / (16 * 32),
    mem_pipeline = 1,
    dma_maxbytes = 128, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    inputType = SInt(8.W),
    outputType = SInt(16.W),
    accType = SInt(32.W)
  )
}
object DSEConfigs{
  import DSEBaseConfig.{baseConfig => base}
  val baseConfig = base.copy(headerFileName = "systolic_params_dse1.h")
  val wsOnlyConfig = baseConfig.copy(dataflow = Dataflow.WS, headerFileName = "systolic_params_dse2.h")
  val bothDataflowsConfig = baseConfig.copy(dataflow = Dataflow.BOTH, headerFileName = "systolic_params_dse3.h")
  val highBitwidthConfig = baseConfig.copy(inputType = SInt(32.W), outputType = SInt(32.W), sp_width = 32*16, headerFileName = "systolic_params_dse4.h")
  val largerDimConfig = baseConfig.copy(meshRows = 32, meshColumns = 32, sp_bank_entries = 64*1024*8/(4*32*8),
    sp_width = 8*32, acc_rows = 16*1024*8 / (32*32), headerFileName = "systolic_params_dse5.h")
  val fullyCombinationalConfig = baseConfig.copy(tileRows = 16, tileColumns = 16, meshRows = 1, meshColumns = 1, headerFileName = "systolic_params_dse6.h")
  val moreMemoryConfig = baseConfig.copy(sp_bank_entries = 256*1024*8 / (4*16*8),
    acc_rows = 64*1024*8/(16*32), shifter_banks = 16, headerFileName = "systolic_params_dse7.h") // 256kB
  val lessBanksConfig = baseConfig.copy(sp_banks = 1, sp_bank_entries = 64*1024*8/(1*16*8), headerFileName = "systolic_params_dse8.h")
  val narrowerBusConfig = baseConfig.copy(dma_maxbytes = 64, dma_buswidth = 64, headerFileName = "systolic_params_dse10.h")
}

//===========BASELINE=========
class SystolicParamsDSE1 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.baseConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========DATAFLOW CHANGE: WS=========
class SystolicParamsDSE2 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.wsOnlyConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========DATAFLOW CHANGE: BOTH=========
class SystolicParamsDSE3 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.bothDataflowsConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========BITWIDTH CHANGE: 32 BITS=========
class SystolicParamsDSE4 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.highBitwidthConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========DIMENSIONS CHANGE: 32x32=========
class SystolicParamsDSE5 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.largerDimConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========PIPELINE DEPTH CHANGE: Fully Combinational=========
class SystolicParamsDSE6 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.fullyCombinationalConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========MEMORY CAPACITY CHANGE: 256 KB=========
class SystolicParamsDSE7 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.moreMemoryConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========MEMORY BANKS CHANGE: 2 Banks=========
class SystolicParamsDSE8 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.lessBanksConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========BUS WIDTH CHANGE: 64 bits=========
class SystolicParamsDSE10 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(OpcodeSet.custom3, DSEConfigs.narrowerBusConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 8)
})


// -----------------------------
// Design Space Exploration Top Level Configs
// -----------------------------

class SystolicDSE1Config extends Config(new SystolicParamsDSE1 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE2Config extends Config(new SystolicParamsDSE2 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE3Config extends Config(new SystolicParamsDSE3 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE4Config extends Config(new SystolicParamsDSE4 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE5Config extends Config(new SystolicParamsDSE5 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE6Config extends Config(new SystolicParamsDSE6 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE7Config extends Config(new SystolicParamsDSE7 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE8Config extends Config(new SystolicParamsDSE8 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE10Config extends Config(new SystolicParamsDSE10 ++
                                    new freechips.rocketchip.system.DefaultConfig)

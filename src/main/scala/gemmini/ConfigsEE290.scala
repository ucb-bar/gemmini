package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}

// -----------------------------
// EE290 Class Config Mixins
// -----------------------------

object GemminiEE290Configs{
  import GemminiConfigs.{defaultConfig => base}
  val defaultConfig = base.copy()

  // Change meshRows and meshColumns to change the size of your systolic array
  val Lab2Config = defaultConfig.copy(meshRows = 8, meshColumns = 8, dataflow = Dataflow.WS)

  // For Lab2 Part 3.6
  val Lab2LargeSPConfig = defaultConfig.copy(meshRows = 8, meshColumns = 8, dataflow = Dataflow.WS, sp_capacity = CapacityInKilobytes(2048))

  // For Lab3
  val Lab3Config = defaultConfig.copy(meshRows = 32, meshColumns = 32, dataflow = Dataflow.WS, outputType = SInt(21.W))

  // For Lab3, small scratchpad
  val Lab3SmallSPConfig = defaultConfig.copy(meshRows = 32, meshColumns = 32, dataflow = Dataflow.WS, sp_capacity = CapacityInKilobytes(128), acc_capacity = CapacityInKilobytes(32), outputType = SInt(21.W))
}

//===========EE290 Lab2 Config=========
class GemminiEE290Lab2Config extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab2Config))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========EE290 Lab2 Large Scratchpad Config=========
class GemminiEE290Lab2LargeSPConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab2LargeSPConfig))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========EE290 Lab3 Config=========
class GemminiEE290Lab3Config extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab3Config))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========EE290 Lab3 Small Scratchpad Config=========
class GemminiEE290Lab3SmallSPConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab3SmallSPConfig))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

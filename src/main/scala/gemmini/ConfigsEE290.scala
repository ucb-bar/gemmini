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

  // For Part 3.6
  val Lab2LargeSPConfig = defaultConfig.copy(meshRows = 8, meshColumns = 8, dataflow = Dataflow.WS, sp_capacity = CapacityInKilobytes(2048))
}


//===========EE290 Lab2 Config=========
class GemminiEE290Lab2Config extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab2Config))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========EE290 Lab2 Large Scratchpad Config=========
class GemminiEE290Lab2LargeSPConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiEE290Configs.Lab2LargeSPConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

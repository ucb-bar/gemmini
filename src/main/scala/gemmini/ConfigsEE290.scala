package gemmini

import chisel3._
import org.chipsalliance.cde.config.{Config, Parameters}
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
  val lab3Config = defaultConfig.copy(meshRows = 8, meshColumns = 8, dataflow = Dataflow.WS)

  // For Lab3 Part 3.6
  val lab3LargeSPConfig = defaultConfig.copy(meshRows = 8, meshColumns = 8, dataflow = Dataflow.WS, sp_capacity = CapacityInKilobytes(2048))
}

//===========EE290 Lab3 Config=========
class GemminiEE290Lab3Config extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(GemminiEE290Configs.lab3Config.copy(
            opcodes = OpcodeSet.custom3
        )))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========EE290 Lab3 Large Scratchpad Config=========
class GemminiEE290Lab3LargeSPConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(GemminiEE290Configs.lab3LargeSPConfig.copy(
            opcodes = OpcodeSet.custom3
        )))
        gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})
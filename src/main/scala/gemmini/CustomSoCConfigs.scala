/*
package chipyard

import freechips.rocketchip.config.{Config}

class CustomGemminiSoCConfig extends Config(
  new gemmini.GemminiCustomConfig ++
  new freechips.rocketchip.subsystem.WithInclusiveCache(
    nBanks = 1,
    nWays = 8,
    capacityKB = 512,
    outerLatencyCycles = 40
  ) ++
  new chipyard.CustomGemmminiCPUConfigs.CustomCPU(1) ++
  new chipyard.config.AbstractConfig
)
*/

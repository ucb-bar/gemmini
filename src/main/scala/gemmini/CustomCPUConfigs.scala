package gemmini

import boom.common._
import freechips.rocketchip.subsystem._

object CustomCPUConfigs {
  // Default CPU configs
  type RocketBigCores = WithNBigCores
  type RocketMedCores = WithNMedCores
  type RocketSmallCores = WithNSmallCores

  type BoomLargeCores = WithNLargeBooms
  type BoomMedCores = WithNMediumBooms
  type BoomSmallCores = WithNMediumBooms
}

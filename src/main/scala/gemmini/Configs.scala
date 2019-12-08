package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet, XLen}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._
import gemmini.Arithmetic.SIntArithmetic


// ------------------
// Multi-RoCC Support
// ------------------

/**
 * Map from a hartId to a particular RoCC accelerator
 */
case object MultiRoCCKey extends Field[Map[Int, Seq[Parameters => LazyRoCC]]](Map.empty[Int, Seq[Parameters => LazyRoCC]])

/**
 * Mixin to enable different RoCCs based on the hartId
 */
class WithMultiRoCC extends Config((site, here, up) => {
  case BuildRoCC => site(MultiRoCCKey).getOrElse(site(TileKey).hartId, Nil)
})


// -----------------------
// Component Mixin Configs
// -----------------------

object GemminiConfigs {
  val defaultConfig = GemminiArrayConfig(
    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,
    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,
    rob_entries = 16,
    sp_banks = 4,
    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.BOTH,
    acc_capacity = CapacityInKilobytes(64),
    mem_pipeline = 1,
    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    inputType = SInt(8.W),
    outputType = SInt(19.W),
    accType = SInt(32.W),
    pe_latency = 0
  )
}

/**
 * Mixin which sets the default parameters for a systolic array accelerator.
   Also sets the system bus width to 128 bits (instead of the deafult 64 bits) to
   allow for the default 16x16 8-bit systolic array to be attached.
 */
class DefaultGemminiConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **replaces** the default host rocket (assuming a single core config).
   This is useful for hierarcical physical design purposes.
 */
class GemminiHostMiniCore extends Config((site, here, up) => {
  case RocketTilesKey => Seq(
    RocketTileParams(
      core = RocketCoreParams(
        useVM = true,
        mulDiv = Some(MulDivParams(mulUnroll = 8))),
      btb = Some(BTBParams(nEntries = 14, nRAS = 2)),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64, // 4Kb
        nWays = 4,  // 4Kb * 4
//        nTLBEntries = 16,
        nMSHRs = 0,
//        blockBytes = site(CacheBlockBytes)
        )),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 4,
//        nTLBEntries = 16,
//        blockBytes = site(CacheBlockBytes)
        )),
      hartId = up(RocketTilesKey, site).length - 1
        ))
  case MultiRoCCKey => up(MultiRoCCKey, site) +
    (up(RocketTilesKey, site).length - 1 ->
      Seq((p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig))
      }))
})



/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **adds** the small core to the default host rocket (as the last hart).
   This is useful for software development purposes for the gemmini accelerator
   as a device with a control processor (rather than a rocc-attached accelerator)
 */
class WithGemminiHostMiniCore extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) :+
    RocketTileParams(
      core = RocketCoreParams(
        useVM = true,
        mulDiv = Some(MulDivParams(mulUnroll = 8))),
      btb = Some(BTBParams(nEntries = 14, nRAS = 2)),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64, // 4Kb scratchpad
        nWays = 4,
//        nTLBEntries = 16,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 4,
//        nTLBEntries = 16,
        blockBytes = site(CacheBlockBytes))),
      hartId = up(RocketTilesKey, site).length
        )
  case MultiRoCCKey => up(MultiRoCCKey, site) +
    (up(RocketTilesKey, site).length ->
      Seq((p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiConfigs.defaultConfig))
      }))
})



// -----------------------------
// Example TopLevel Configs
// -----------------------------


/**
 * Top level config with a default single core rocket and gemmini rocc accelerator.
   Useful for gemmini performance evaluation and debugging.
 */
class GemminiConfig extends Config(new DefaultGemminiConfig ++
                                    new freechips.rocketchip.system.DefaultConfig)


/**
 * Top level config with a small host rocket and gemmini rocc accelerator.
   Useful for physical design.
 */
class GemminiAcceleratorConfig extends Config(
  new GemminiHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultGemminiConfig ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)


/**
 * Top level config with a default single core rocket,
   and a small mini-core rocket attached to a gemmini rocc accelerator.
   Useful for device driver development
 */
class GemminiAcceleratorDeviceConfig extends Config(
  new WithGemminiHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultGemminiConfig ++
  new WithMultiRoCC ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)



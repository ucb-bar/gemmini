package systolic

import chisel3._
import freechips.rocketchip.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet, XLen}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._


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

/**
 * Mixin which sets the default parameters for a systolic array accelerator.
   Also sets the system bus width to 128 bits (instead of the deafult 64 bits) to
   allow for the default 16x16 8-bit systolic array to be attached.
 */
class DefaultSystolicConfig extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 256 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.BOTH,
      acc_rows = 64 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **replaces** the default host rocket (assuming a single core config).
   This is useful for hierarcical physical design purposes.
 */
class SystolicHostMiniCore extends Config((site, here, up) => {
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
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
      }))
})



/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **adds** the small core to the default host rocket (as the last hart).
   This is useful for software development purposes for the systolic accelerator
   as a device with a control processor (rather than a rocc-attached accelerator)
 */
class WithSystolicHostMiniCore extends Config((site, here, up) => {
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
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
      }))
})



// -----------------------------
// Example TopLevel Configs
// -----------------------------


/**
 * Top level config with a default single core rocket and systolic rocc accelerator.
   Useful for systolic performance evaluation and debugging.
 */
class SystolicConfig extends Config(new DefaultSystolicConfig ++
                                    new freechips.rocketchip.system.DefaultConfig)


/**
 * Top level config with a small host rocket and systolic rocc accelerator.
   Useful for physical design.
 */
class SystolicAcceleratorConfig extends Config(
  new SystolicHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultSystolicConfig ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)


/**
 * Top level config with a default single core rocket, 
   and a small mini-core rocket attached to a systolic rocc accelerator.
   Useful for device driver development
 */
class SystolicAcceleratorDeviceConfig extends Config(
  new WithSystolicHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultSystolicConfig ++
  new WithMultiRoCC ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)



package chipyard

import org.chipsalliance.cde.config.Config

// ------------------------------
// Configs with Gemmini RoCC
// ------------------------------

// DOC include start: GemminiRocketConfig
class GemminiRocketConfig extends Config(
  new gemmini.DefaultGemminiConfig ++                            // use Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
// DOC include end: GemminiRocketConfig

class FPGemminiRocketConfig extends Config(
  new gemmini.GemminiFP32DefaultConfig ++                         // use FP32Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class LeanGemminiRocketConfig extends Config(
  new gemmini.LeanGemminiConfig ++                                 // use Lean Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class LeanGemminiPrintfRocketConfig extends Config(
  new gemmini.LeanGemminiPrintfConfig ++                                 // use Lean Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class ReRoCCManyGemminiConfig extends Config(
  new rerocc.WithReRoCC ++
  new gemmini.LeanGemminiConfig ++                              // rerocc tile3 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile2 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile1 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile0 is gemmini
  new freechips.rocketchip.rocket.WithNHugeCores(4) ++           // 4 rocket cores
  new chipyard.config.AbstractConfig)

class GemminiShuttleConfig extends Config(
  new gemmini.DefaultGemminiConfig ++                            // use Gemmini systolic array GEMM accel
  new shuttle.common.WithNShuttleCores ++
  new chipyard.config.AbstractConfig)

class MxGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPStandaloneConfig ++                      // standalone MX twin (internal spad + MMIO requant)
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxE5M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE5M2StandaloneConfig ++                  // FP8 E5M2 via LUT (Option A variant)
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxAllGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAllStandaloneConfig ++                  // ALL MX formats {FP4,E3M2,E2M3,E4M3,E5M2}, modes {0,4,8,9}
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Back-compat alias: the old E4M3-LUT build is now a strict subset of the all-formats build.
class MxE4M3LutGemminiRocketConfig extends MxAllGemminiRocketConfig

class TestMxGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPTestConfig ++                         // use FP32Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class TestRequantizerLutMxGemminiRocketConfig extends Config(
  new gemmini.GemminiRequantizerLutMxFPTestConfig ++                         // use FP32Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNSmallCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new chipyard.config.AbstractConfig)
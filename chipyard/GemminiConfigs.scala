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

// FULL build: all 5 MX formats on both operands + ALL 12 PE modes -> every sym + asym combo (25 pairs;
// 36 counting E4M3 single vs quad) in one elaborated mesh.
class MxAllAsymGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAllAsymStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Single-format builds: each supports ONLY its MX format; all other format hardware is elaboration-gated.
class MxFp4OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPFp4OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxE3M2OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE3M2OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxE2M3OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE2M3OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxE4M3OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE4M3OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxE5M2OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE5M2OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Asymmetric FP4-act x FP6_E3M2-wei build (mode1, 4 products/PE).
class MxAsymFp4Fp6GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymFp4Fp6StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Opposite asymmetric FP6_E3M2-act x FP4-wei build (mode3).
class MxAsymFp6Fp4GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymFp6Fp4StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Asymmetric FP8_E5M2-act x FP4-wei build (mode3, wide E5M2 activation).
class MxAsymE5M2Fp4GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE5M2Fp4StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Opposite asymmetric FP4-act x FP8_E5M2-wei build (mode1, wide E5M2 weight).
class MxAsymFp4E5M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymFp4E5M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Dual-LUT asymmetric FP8_E5M2-act x FP6_E3M2-wei build (mode4, per-operand altfmt + deproject width).
class MxAsymE5M2E3M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE5M2E3M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Opposite dual-LUT asymmetric FP6_E3M2-act x FP8_E5M2-wei build (mode4).
class MxAsymE3M2E5M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE3M2E5M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Mixed-quad asymmetric FP8_E4M3-act x FP4-wei build (mode10, E4M3 via quad LUT).
class MxAsymE4M3Fp4GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE4M3Fp4StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Opposite mixed-quad asymmetric FP4-act x FP8_E4M3-wei build (mode11, E4M3 weight via quad LUT).
class MxAsymFp4E4M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymFp4E4M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// E4M3-quad x sig3 dual-LUT builds (mode10 / mode11), both directions x {e3m2, e5m2}.
class MxAsymE4M3E3M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE4M3E3M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxAsymE4M3E5M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE4M3E5M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxAsymE3M2E4M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE3M2E4M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class MxAsymE5M2E4M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE5M2E4M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// E2M3 mixed-quad builds (mode10/mode11), E2M3 x {fp4, e3m2, e5m2} both directions.
class MxAsymE2M3Fp4GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE2M3Fp4StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymE2M3E3M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE2M3E3M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymE2M3E5M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE2M3E5M2StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymFp4E2M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymFp4E2M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymE3M2E2M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE3M2E2M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymE5M2E2M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE5M2E2M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

// Dual-sig4 quad E2M3 x E4M3 builds (mode9, mixed 6/8-bit codes + per-operand altfmt).
class MxAsymE2M3E4M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE2M3E4M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
class MxAsymE4M3E2M3GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAsymE4M3E2M3StandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

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
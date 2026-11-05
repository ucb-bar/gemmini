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

// Every Mx*GemminiRocketConfig uses one memory system: a 128-bit memory bus (WithEdgeDataBits(128)) and otherwise
// the default L2 (1 bank, default MSHRs), so builds differ only in the Gemmini/VPU configuration.
class MxGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPStandaloneConfig ++                      // standalone MX twin (internal spad + MMIO requant)
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxE4M3SingleGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE4M3SingleNoLutStandaloneConfig ++       // standalone MX, E4M3 single throughput only, no QuantLut
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(256) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxAllGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAllStandaloneConfig ++                  // ALL MX formats {FP4,E3M2,E2M3,E4M3,E5M2}, modes {0,4,8,9}
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// FULL build: all 5 MX formats on both operands + ALL 12 PE modes -> every sym + asym combo (25 pairs;
// 36 counting E4M3 single vs quad) in one elaborated mesh.
class MxAllAsymGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPAllAsymStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxDim32AllGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim32AllStandaloneConfig ++              // all MX formats {FP4,E3M2,E2M3,E4M3,E5M2} on the 32x32 mesh, 8-bit LUT
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxDim32AllAsymGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim32AllAsymStandaloneConfig ++         // all formats + ALL 12 PE modes (sym+asym) on the 32x32 mesh
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxDim8AllAsymGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim8AllAsymStandaloneConfig ++         // all formats + ALL 12 PE modes (sym+asym) on the 8x8 mesh
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// Multi-format VPU + SPAD_REQUANT builds (FireSim targets). 4x4: FP4 + E3M2.
class MxDim4Fp4E3M2VpuGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim4Fp4E3M2VpuStandaloneConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

// 8x8: FP4 + E3M2 + E4M3.
class MxDim8Fp4E3M2E4M3VpuGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim8Fp4E3M2E4M3VpuStandaloneConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

// 16x16: FP4 + E3M2 + E4M3.
class MxFp4E3M2E4M3VpuGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPFp4E3M2E4M3VpuStandaloneConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

// BF16 baselines (plain non-MX Gemmini) on the same SoC / memory system as the MX builds above.
class BF16Dim8GemminiRocketConfig extends Config(
  new gemmini.GemminiBF16Dim8BaselineConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class BF16Dim16GemminiRocketConfig extends Config(
  new gemmini.GemminiBF16Dim16BaselineConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class BF16Dim32GemminiRocketConfig extends Config(
  new gemmini.GemminiBF16Dim32BaselineConfig ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

// 32x32 E4M3 + VPU (2 x 16 BF16 lanes) + SPAD_REQUANT (FireSim target).
class MxDim32E4M3VpuWideMemGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDim32E4M3VpuStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxE4M3OnlyGemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPE4M3OnlyStandaloneConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// Two MX Gemminis per core: 16x16 (custom3) + 32x32 (custom2). FP4 sym, E4M3s sym, A8W4 (E4M3s act x FP4 wei).
class MxDualA8W4GemminiBoomConfig extends Config(
  new gemmini.GemminiMxFPDualA8W4Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// Rocket twin, plus E3M2 sym.
class MxDualA8W4E3M2GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDualA8W4E3M2Config ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

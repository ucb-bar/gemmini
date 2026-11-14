package chipyard

import org.chipsalliance.cde.config.Config

// AXI id width of the DRAM port (default 4).
class WithMemPortIdBits(n: Int) extends Config((site, here, up) => {
  case freechips.rocketchip.subsystem.ExtMem => up(freechips.rocketchip.subsystem.ExtMem).map(m => m.copy(master = m.master.copy(idBits = n)))
})

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

// The two dual configs above with 16x16 (custom3) + 8x8 (custom2).
class MxDualA8W4Dim8GemminiBoomConfig extends Config(
  new gemmini.GemminiMxFPDualA8W4Dim8Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxDualA8W4E3M2Dim8GemminiRocketConfig extends Config(
  new gemmini.GemminiMxFPDualA8W4E3M2Dim8Config ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// One Gemmini 16x16: E4M3 + E4M3 LUT, 2 VPUs, full BF16 PE arithmetic; 512-bit sbus, 32 B DRAM port, 4 L2 banks.
class MxE4M3LutBf16Dim16GemminiBoomConfig extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim16Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(4) ++   // 4 L2 banks x 40 MSHRs (outerLatencyCycles unchanged)
  new WithMemPortIdBits(8) ++   // 256 AXI ids: SimDRAM holds one read per id, 4 bits capped DRAM at 16 in flight
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(256) ++   // one 32 B DRAM port
  new chipyard.config.AbstractConfig)

// One Gemmini 32x32: E4M3 + E4M3 LUT, 2 VPUs, full BF16 PE arithmetic; 512-bit sbus, 32 B DRAM port, 4 L2 banks.
class MxE4M3LutBf16Dim32GemminiBoomConfig extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim32Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(4) ++   // 4 L2 banks x 40 MSHRs (outerLatencyCycles unchanged)
  new WithMemPortIdBits(8) ++   // 256 AXI ids: SimDRAM holds one read per id, 4 bits capped DRAM at 16 in flight
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(256) ++   // one 32 B DRAM port
  new chipyard.config.AbstractConfig)

// The E4M3 LUT BOOM configs with 2x DRAM bandwidth: 2 L2 banks (16 B data-array writes), 16 B memory bus per channel,
// 2 DRAM channels (two DRAM models, 32 B total); the mbus crossbar spreads both banks over both channels.
class MxE4M3LutBf16Dim16GemminiBoomBw2Config extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim16Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(2) ++
  new chipyard.config.WithInclusiveCacheWriteBytes(16) ++
  new freechips.rocketchip.subsystem.WithNMemoryChannels(2) ++
  new WithMemPortIdBits(8) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

class MxE4M3LutBf16Dim32GemminiBoomBw2Config extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim32Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(2) ++
  new chipyard.config.WithInclusiveCacheWriteBytes(16) ++
  new freechips.rocketchip.subsystem.WithNMemoryChannels(2) ++
  new WithMemPortIdBits(8) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// DIM32 LUT with 128 reads in flight (5 mvins tracked instead of 3): on the original memory system, and on Bw2.
class MxE4M3LutBf16Dim32GemminiBoomIF128Config extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim32IF128Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(4) ++
  new WithMemPortIdBits(8) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(256) ++
  new chipyard.config.AbstractConfig)

class MxE4M3LutBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3LutBf16Dim32IF128Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(2) ++
  new chipyard.config.WithInclusiveCacheWriteBytes(16) ++
  new freechips.rocketchip.subsystem.WithNMemoryChannels(2) ++
  new WithMemPortIdBits(8) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// The Bw2IF128 memory system above, shared by the format/gating variants below.
class WithMxBw2Boom extends Config(
  new boom.v3.common.WithNMediumBooms(1) ++
  new freechips.rocketchip.subsystem.WithNBanks(2) ++
  new chipyard.config.WithInclusiveCacheWriteBytes(16) ++
  new freechips.rocketchip.subsystem.WithNMemoryChannels(2) ++
  new WithMemPortIdBits(8) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(128) ++
  new chipyard.config.AbstractConfig)

// C1: E4M3 runtime LUT + FP4 both sides
class MxE4M3Fp4LutBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3Fp4LutBf16Dim32IF128Config ++ new WithMxBw2Boom)
// C2: C1 + E3M2 (LUT via uselut=1, direct via uselut=0)
class MxE4M3Fp4E3M2LutBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3Fp4E3M2LutBf16Dim32IF128Config ++ new WithMxBw2Boom)
// C3: FP4 + E4M3 single + E3M2 direct, no QuantLut
class MxE4M3Fp4E3M2NoLutBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3Fp4E3M2NoLutBf16Dim32IF128Config ++ new WithMxBw2Boom)
// C4: C2 without PE lane/zero gating and mesh clock gating
class MxE4M3Fp4E3M2LutNoPeMeshGateBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3Fp4E3M2LutNoPeMeshGateBf16Dim32IF128Config ++ new WithMxBw2Boom)
// C5: C2 without accumulator clock gating
class MxE4M3Fp4E3M2LutNoAccGateBf16Dim32GemminiBoomBw2IF128Config extends Config(
  new gemmini.GemminiMxFPE4M3Fp4E3M2LutNoAccGateBf16Dim32IF128Config ++ new WithMxBw2Boom)

// Synthesis / power / FireSim set: C2 Gemmini (FP4 + E4M3 runtime LUT + E3M2 LUT or direct, BF16 PE, 2 VPUs) on the Bw2 memory system.
class MxPwrBaseDim16GemminiBoomBw2Config extends Config(   // base, 16x16
  new gemmini.GemminiMxFPPwrBaseDim16Config ++ new WithMxBw2Boom)
class MxPwrBaseDim32GemminiBoomBw2Config extends Config(   // base, 32x32
  new gemmini.GemminiMxFPPwrBaseDim32Config ++ new WithMxBw2Boom)
class MxPwrNoLaneGateDim16GemminiBoomBw2Config extends Config(   // no PE lane gating, 16x16
  new gemmini.GemminiMxFPPwrNoLaneGateDim16Config ++ new WithMxBw2Boom)
class MxPwrNoLaneGateDim32GemminiBoomBw2Config extends Config(   // no PE lane gating, 32x32
  new gemmini.GemminiMxFPPwrNoLaneGateDim32Config ++ new WithMxBw2Boom)
class MxPwrAcc1Dim16GemminiBoomBw2Config extends Config(   // no accumulator double buffer (acc_banks = 1), 16x16
  new gemmini.GemminiMxFPPwrAcc1Dim16Config ++ new WithMxBw2Boom)
class MxPwrAcc1Dim32GemminiBoomBw2Config extends Config(   // no accumulator double buffer (acc_banks = 1), 32x32
  new gemmini.GemminiMxFPPwrAcc1Dim32Config ++ new WithMxBw2Boom)
class MxPwrFixG1Dim16GemminiBoomBw2Config extends Config(   // base with LUT G fixed at 1, 16x16
  new gemmini.GemminiMxFPPwrFixG1Dim16Config ++ new WithMxBw2Boom)
class MxPwrFixG1Dim32GemminiBoomBw2Config extends Config(   // base with LUT G fixed at 1, 32x32
  new gemmini.GemminiMxFPPwrFixG1Dim32Config ++ new WithMxBw2Boom)

// One Gemmini 16x16: E4M3 act x E2M1 wei (+ FP4 sym, E4M3 sym), no LUT, 2 VPUs, full BF16 PE arithmetic; 512-bit sbus, 32 B DRAM port.
class MxA8W4Bf16Dim16GemminiBoomConfig extends Config(
  new gemmini.GemminiMxFPA8W4Bf16Dim16Config ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(256) ++
  new chipyard.config.AbstractConfig)

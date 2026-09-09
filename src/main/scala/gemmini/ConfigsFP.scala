package gemmini

import chisel3._
import org.chipsalliance.cde.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}

// -----------------------------
// Floating Point Config Mixins
// -----------------------------


object GemminiFPConfigs {
  import Arithmetic.FloatArithmetic._
  val defaultFPConfig = GemminiArrayConfig[Float, Float, Float](
    opcodes = OpcodeSet.custom3,
    tileRows = 1,
    tileColumns = 1,
    meshRows = 4,
    meshColumns = 4,

    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,

    reservation_station_entries_ld = 8,
    reservation_station_entries_st = 4,
    reservation_station_entries_ex = 16,

    sp_banks = 4,
    sp_singleported = true,
    acc_banks = 1,
    acc_latency = 2,
    acc_singleported = false,
    acc_sub_banks = -1,
    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.BOTH,
    acc_capacity = CapacityInKilobytes(64),
    spad_read_delay = 1,

    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_mem_reqs = 16,
    use_dedicated_tl_port = false,
    use_shared_ext_mem = false,
    // MX scaling is the default; this is a non-MX (FP) config
    use_mx_scaling = false,
    inputType = Float(8, 24),
    weightType = Float(8, 24),
    accType = Float(8, 24),
    weightTypeProjected = Float(8, 24), //projected to lower precision
    inputTypeProjected = Float(8, 24),
    accTypeProjected = Float(8, 24),
    spatialArrayInputType = Float(8, 24),
    spatialArrayWeightType = Float(8, 24),
    spatialArrayOutputType = Float(8, 24),

    mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
    mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
    mvin_scale_shared = false,

    acc_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0",
      c_str = "((x) * (scale))"
    )),
    acc_read_full_width = true,
    acc_read_small_width = true,

    tile_latency = 1,

    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true,

    hardcode_d_to_garbage_addr = false,

    mesh_output_delay = 0,

    has_training_convs = false,
    has_max_pool = true,
    has_nonlinear_activations = true,

    num_counter = 8,
  )
  
  //FP32 Single Precision Configuration
  val FP32DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 24), weightType = Float(8, 24), accType = Float(8, 24), spatialArrayInputType = Float(8, 24), spatialArrayWeightType = Float(8, 24), spatialArrayOutputType = Float(8, 24),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
 
  //FP16 Half Precision Configuration
  val FP16DefaultConfig = defaultFPConfig.copy(inputType = Float(5, 11), weightType = Float(5, 11), accType = Float(8, 24), spatialArrayInputType = Float(5, 11), spatialArrayWeightType = Float(5, 11), spatialArrayOutputType = Float(5, 11),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
  
  //Bfloat16 Brain-half Precision Configuration
  val BF16DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24), spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  //Bfloat16 Brain-half Precision Configuration 8x8 array
  val BF16Default8Config = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24), spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               meshRows = 8, meshColumns = 8,
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )


  val chipFP32Config = FP32DefaultConfig.copy(sp_capacity=CapacityInKilobytes(32), acc_capacity=CapacityInKilobytes(8), dataflow=Dataflow.WS,
    acc_scale_args = Some(ScaleArguments((t: Float, u: Float) => {t}, 1, Float(8, 24), -1, identity = "1.0",
      c_str = "((x))"
    )),
    mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 3, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
    mvin_scale_acc_args=None,
    acc_singleported=false,
    acc_sub_banks = 1,
    acc_banks = 2,
    mesh_output_delay = 2,
    tile_latency = 1,
    acc_latency = 3,
    ex_read_from_acc=false,
    ex_write_to_spad=false,
    has_training_convs = false,
    hardcode_d_to_garbage_addr = true,
    acc_read_full_width = false,
    max_in_flight_mem_reqs = 16,
    headerFileName = "gemmini_params_fp32.h",
    num_counter = 0,
    clock_gate = true 
  )
}

//===========FP32 Default Config=========
class GemminiFP32DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiFPConfigs.FP32DefaultConfig))
    }
  )
})

class ChipFP32GemminiConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiFPConfigs.chipFP32Config
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})


//===========FP16 Default Config=========
class GemminiFP16DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiFPConfigs.FP16DefaultConfig))
    }
  )
})

//===========BFLOAT16 Default Config=========
class GemminiBF16DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiFPConfigs.BF16DefaultConfig))
    }
  )
})

class GemminiBF16DefaultHighPerfConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      val gemmini = LazyModule(new Gemmini(GemminiFPConfigs.BF16DefaultConfig.copy(
        ex_read_from_acc = false,
        ex_write_to_spad = false,
      )))
      gemmini
    }
  )
})

//===========BFLOAT16 Default Config 8x8=========
class GemminiBF16Default8Config extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiFPConfigs.BF16Default8Config))
    }
  )
})

object GemminiMxFPConfigs {
  import Arithmetic.MxFloatArithmetic._
  import mxgen.MxConfig
  val defaultMxFPConfig = GemminiArrayConfig[MxFloat, Float, Float](
    opcodes = OpcodeSet.custom3,
    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,

    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,

    reservation_station_entries_ld = 8,
    reservation_station_entries_st = 4,
    reservation_station_entries_ex = 16,

    sp_banks = 4,
    sp_singleported = true,
    acc_banks = 1,
    acc_latency = 2,
    acc_singleported = false,
    acc_sub_banks = 1,
    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.WS,
    acc_capacity = CapacityInKilobytes(64),
    spad_read_delay = 1,

    dma_maxbytes = 64, // These are overwritten by radiance
    dma_buswidth = 512, // These are overwritten by radiance
    max_spad_writer_bytes = 64,
    spad_writer_dma_width = 512,
    aligned_to = 1,
    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_mem_reqs = 16,
    use_dedicated_tl_port = false,
    use_shared_ext_mem = false,
    use_mx_scaling = true,

    inputType = MxFloat(3, 3, 2, pad=false),
    weightType = MxFloat(3, 3, 2, pad=false),
    accType = MxFloat(8, 8, 4),
    weightTypeProjected = MxFloat(2, 2, 2, pad=false),
    inputTypeProjected = MxFloat(2, 2, 2, pad=false), 
    accTypeProjected = MxFloat(8, 8, 4, pad=false),

    spatialArrayInputType = MxFloat(3, 3, 2, pad=false), 
    spatialArrayWeightType = MxFloat(3, 3, 2, pad=false),
    spatialArrayOutputType = MxFloat(8, 8, 4, true, false),

    meshProdPrecisionList = Seq.fill(16) {MxFloat(4, 4, 4, true, false)},

    meshAccPrecisionList = Seq.fill(8) {MxFloat(4, 5, 4, true, false)} ++
      Seq.fill(2) {MxFloat(4, 6, 4, true, false)} ++
      Seq.fill(5) {MxFloat(4, 7, 4, true, false)} ++
      Seq.fill(1) {MxFloat(8, 8, 4, true, false)},

    scaleSize = 32,
    enable_lut = true,
    mvin_scale_args = None,
    mvin_scale_acc_args = None,
    mvin_scale_shared = false,

    acc_read_full_width = true,
    acc_read_small_width = true,

    tile_latency = 1,

    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true,

    hardcode_d_to_garbage_addr = false,
    has_normalizations = false,

    mesh_output_delay = 0,

    has_training_convs = false,
    has_max_pool = false,
    has_nonlinear_activations = false,

    num_counter = 8,
    requantizer = Some(GemminiRequantizerConfig( 
      baseAddr = 0x10000000L,
      numInputLanes = 64,
      numOutputLanes = 32,
      gpuMaxFactor = 2,
      gpuWordSize = 4,
      inputBits = 16,
      minOutputBits = 4,
      maxOutputBits = 8,
      outputIdBits = 3
    )),
    scale_mem = Some(GemminiScalingFactorMemConfig(
        baseAddr = 0x10000000L + 0x8000,
        sizeInBytes = 16 << 10,
        subbankLineSizeInBytes = 16,
        subbanksPerBank = 2,
        numBanks = 8,
        ))
  )

  val testMxFPConfig = defaultMxFPConfig.copy(testConfig = true)
  val testRequantizerLutMxFPConfig = defaultMxFPConfig.copy(
    testConfig = true,
    enable_lut = true,
    lut = Some(GemminiLUTConfig())
  )

  // Standalone twin of the Radiance MX flow: same functional params, but internal scratchpad + MMIO
  // requant path instead of shared SRAM.
  val standaloneMxFPConfig = defaultMxFPConfig.copy(
    ex_read_from_acc = false,
    ex_write_to_spad = true,   // requant FP8 -> internal spad
    sp_singleported = false,
    spad_read_delay = 4,
    tile_latency = 0,
    mesh_output_delay = 1,
    acc_latency = 3,
    num_counter = 0,
    lut = Some(GemminiLUTConfig()),
    // Scale-factor RAM window 0x20000000..0x20003fff: weight scales at +0x0000, activation at +0x2000.
    scale_mem = Some(defaultMxFPConfig.scale_mem.get.copy(baseAddr = 0x20000000L)),
    mx_mmio_base = Some(0x20010000L)   // LUT/requant regmap, off the scale window
  )

  // FP8 E5M2 via LUT: E5M2 stored 4-bit and up-projected to 8-bit, operand lane MxFloat(5,3,2).
  val e5m2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat(5, 3, 2, pad=false),
    weightType = MxFloat(5, 3, 2, pad=false),
    spatialArrayInputType  = MxFloat(5, 3, 2, pad=false),
    spatialArrayWeightType = MxFloat(5, 3, 2, pad=false),
    lut = Some(GemminiLUTConfig(
      numBits    = Seq(128, 128, 128),
      numEntries = Seq(64, 64, 64),
      rdataWidth = 8,
      raddrWidth = 4,
      projFormat = LutFP8E5M2,
    )),
  )

  // All MX formats in one mesh {FP4, E3M2, E2M3, E4M3, E5M2}, modes {0,4,8,9}; operand lane MxFloat(4,4,2).
  // Runtime lut_en promotes E4M3 to the 4-wide quad path.
  val allMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat(4, 4, 2, pad=false),
    weightType = MxFloat(4, 4, 2, pad=false),
    spatialArrayInputType  = MxFloat(4, 4, 2, pad=false),
    spatialArrayWeightType = MxFloat(4, 4, 2, pad=false),
    lut = Some(GemminiLUTConfig(
      numBits    = Seq(128, 128, 128),
      numEntries = Seq(64, 64, 64),
      rdataWidth = 8,
      raddrWidth = 4,
      projFormat = LutFP8E4M3,
    )),
  )
  // Back-compat alias.
  val e4m3LutMxFPConfig = allMxFPConfig

  // Single-format builds: the operand descriptor carries an explicit MxConfig so the PE elaborates only
  // that format's decode + mode(s), and the LUT projFormat picks that format's finder(s).
  val fp4OnlyMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 2, 2, MxConfig.fp4Only),
    weightType = MxFloat.withConfig(2, 2, 2, MxConfig.fp4Only),
    spatialArrayInputType  = MxFloat.withConfig(2, 2, 2, MxConfig.fp4Only),
    spatialArrayWeightType = MxFloat.withConfig(2, 2, 2, MxConfig.fp4Only),
  )
  val e3m2OnlyMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(3, 3, 2, MxConfig.e3m2Only),
    weightType = MxFloat.withConfig(3, 3, 2, MxConfig.e3m2Only),
    spatialArrayInputType  = MxFloat.withConfig(3, 3, 2, MxConfig.e3m2Only),
    spatialArrayWeightType = MxFloat.withConfig(3, 3, 2, MxConfig.e3m2Only),
  )
  // E2M3-only: 6-bit LUT (up-project 4-bit indices to 6-bit E2M3), 12-bit operand lane (2x6).
  val e2m3OnlyMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 4, 2, MxConfig.e2m3Only),
    weightType = MxFloat.withConfig(2, 4, 2, MxConfig.e2m3Only),
    spatialArrayInputType  = MxFloat.withConfig(2, 4, 2, MxConfig.e2m3Only),
    spatialArrayWeightType = MxFloat.withConfig(2, 4, 2, MxConfig.e2m3Only),
    lut = Some(GemminiLUTConfig(projFormat = LutFP6E2M3)),
  )
  val e4m3OnlyMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(4, 4, 2, MxConfig.e4m3Only),
    weightType = MxFloat.withConfig(4, 4, 2, MxConfig.e4m3Only),
    spatialArrayInputType  = MxFloat.withConfig(4, 4, 2, MxConfig.e4m3Only),
    spatialArrayWeightType = MxFloat.withConfig(4, 4, 2, MxConfig.e4m3Only),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3)),
  )
  val e5m2OnlyMxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(5, 3, 2, MxConfig.e5m2Only),
    weightType = MxFloat.withConfig(5, 3, 2, MxConfig.e5m2Only),
    spatialArrayInputType  = MxFloat.withConfig(5, 3, 2, MxConfig.e5m2Only),
    spatialArrayWeightType = MxFloat.withConfig(5, 3, 2, MxConfig.e5m2Only),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2)),
  )

  // Asymmetric FP4-act x FP6_E3M2-wei (mode1): activation fed direct (4-bit), weight LUT-deprojected to
  // 6-bit E3M2. Inherits the default LutFP6E3M2 codebook from standaloneMxFPConfig.
  val asymFp4Fp6MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4Fp6),
    weightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymFp4Fp6),
    spatialArrayInputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4Fp6),
    spatialArrayWeightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymFp4Fp6),
  )

  // Opposite asymmetric: FP6_E3M2-act (LUT-deprojected) x FP4-wei (direct), mode3. Default LutFP6E3M2
  // codebook (inherited) up-projects the fp6 activation.
  val asymFp6Fp4MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymFp6Fp4),
    weightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp6Fp4),
    spatialArrayInputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymFp6Fp4),
    spatialArrayWeightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp6Fp4),
  )

  // Asymmetric: FP8_E5M2-act (LUT-deprojected to 8-bit) x FP4-wei (direct), mode3. E5M2 LUT codebook.
  val asymE5M2Fp4MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2Fp4),
    weightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE5M2Fp4),
    spatialArrayInputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2Fp4),
    spatialArrayWeightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE5M2Fp4),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2)),
  )

  // Opposite: FP4-act (direct) x FP8_E5M2-wei (LUT-deprojected to 8-bit), mode1. E5M2 LUT codebook.
  val asymFp4E5M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E5M2),
    weightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymFp4E5M2),
    spatialArrayInputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E5M2),
    spatialArrayWeightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymFp4E5M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2)),
  )

  // Dual-LUT asymmetric: FP8_E5M2-act (8-bit codes) x FP6_E3M2-wei (6-bit codes), mode4. Per-operand
  // deproject width: act packs at 8b, wei at 6b (storage stays 8b).
  val asymE5M2E3M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E3M2),
    weightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE5M2E3M2),
    spatialArrayInputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E3M2),
    spatialArrayWeightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE5M2E3M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2, actCodeWidth = 8, weiCodeWidth = 6)),
  )

  // Opposite dual-LUT asymmetric: FP6_E3M2-act (6-bit codes) x FP8_E5M2-wei (8-bit codes), mode4.
  val asymE3M2E5M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E5M2),
    weightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE3M2E5M2),
    spatialArrayInputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E5M2),
    spatialArrayWeightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE3M2E5M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2, actCodeWidth = 6, weiCodeWidth = 8)),
  )

  // Mixed quad: FP8_E4M3-act (LUT-deprojected to 8-bit, 2/lane) x FP4-wei (direct), mode10. E4M3 LUT.
  val asymE4M3Fp4MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3Fp4),
    weightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE4M3Fp4),
    spatialArrayInputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3Fp4),
    spatialArrayWeightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE4M3Fp4),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3)),
  )

  // Opposite mixed quad: FP4-act (direct) x FP8_E4M3-wei (LUT-deprojected to 8-bit, 2/lane), mode11.
  val asymFp4E4M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E4M3),
    weightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymFp4E4M3),
    spatialArrayInputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E4M3),
    spatialArrayWeightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymFp4E4M3),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3, weiCodeWidth = 8)),
  )

  // E4M3-quad x sig3 dual-LUT combos, mode10 (E4M3 act) / mode11 (E4M3 wei). Per-operand code widths:
  // E4M3 8-bit, e3m2 6-bit, e5m2 8-bit. E5M2 pairs also need the per-operand altfmt bit (set in the test).
  val asymE4M3E3M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3E3M2),
    weightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE4M3E3M2),
    spatialArrayInputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3E3M2),
    spatialArrayWeightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE4M3E3M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3, actCodeWidth = 8, weiCodeWidth = 6)),
  )
  val asymE4M3E5M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3E5M2),
    weightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE4M3E5M2),
    spatialArrayInputType  = MxFloat.withConfig(4, 4, 2, MxConfig.asymE4M3E5M2),
    spatialArrayWeightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE4M3E5M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3, actCodeWidth = 8, weiCodeWidth = 8)),
  )
  val asymE3M2E4M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E4M3),
    weightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymE3M2E4M3),
    spatialArrayInputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E4M3),
    spatialArrayWeightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymE3M2E4M3),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3, actCodeWidth = 6, weiCodeWidth = 8)),
  )
  val asymE5M2E4M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E4M3),
    weightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymE5M2E4M3),
    spatialArrayInputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E4M3),
    spatialArrayWeightType = MxFloat.withConfig(4, 4, 2, MxConfig.asymE5M2E4M3),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E4M3, actCodeWidth = 8, weiCodeWidth = 8)),
  )

  // E2M3-quad (sig4, exp2, fp6/LUT 6-bit codes) mixed-quad combos. mode10 (E2M3 act) / mode11 (E2M3 wei).
  val asymE2M3Fp4MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3Fp4),
    weightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE2M3Fp4),
    spatialArrayInputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3Fp4),
    spatialArrayWeightType = MxFloat.withConfig(2, 2, 2, MxConfig.asymE2M3Fp4),
    lut = Some(GemminiLUTConfig(Seq(96, 96, 96), Seq(64, 64, 64), rdataWidth = 6, raddrWidth = 4,
      projFormat = LutFP6E2M3)),
  )
  val asymE2M3E3M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3E3M2),
    weightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE2M3E3M2),
    spatialArrayInputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3E3M2),
    spatialArrayWeightType = MxFloat.withConfig(3, 3, 2, MxConfig.asymE2M3E3M2),
    lut = Some(GemminiLUTConfig(Seq(96, 96, 96), Seq(64, 64, 64), rdataWidth = 6, raddrWidth = 4,
      projFormat = LutFP6E2M3)),
  )
  val asymE2M3E5M2MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3E5M2),
    weightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE2M3E5M2),
    spatialArrayInputType  = MxFloat.withConfig(2, 4, 2, MxConfig.asymE2M3E5M2),
    spatialArrayWeightType = MxFloat.withConfig(5, 3, 2, MxConfig.asymE2M3E5M2),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2, actCodeWidth = 6, weiCodeWidth = 8)),
  )
  val asymFp4E2M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E2M3),
    weightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymFp4E2M3),
    spatialArrayInputType  = MxFloat.withConfig(2, 2, 2, MxConfig.asymFp4E2M3),
    spatialArrayWeightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymFp4E2M3),
    lut = Some(GemminiLUTConfig(Seq(96, 96, 96), Seq(64, 64, 64), rdataWidth = 6, raddrWidth = 4,
      projFormat = LutFP6E2M3)),
  )
  val asymE3M2E2M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E2M3),
    weightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymE3M2E2M3),
    spatialArrayInputType  = MxFloat.withConfig(3, 3, 2, MxConfig.asymE3M2E2M3),
    spatialArrayWeightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymE3M2E2M3),
    lut = Some(GemminiLUTConfig(Seq(96, 96, 96), Seq(64, 64, 64), rdataWidth = 6, raddrWidth = 4,
      projFormat = LutFP6E2M3)),
  )
  val asymE5M2E2M3MxFPConfig = standaloneMxFPConfig.copy(
    inputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E2M3),
    weightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymE5M2E2M3),
    spatialArrayInputType  = MxFloat.withConfig(5, 3, 2, MxConfig.asymE5M2E2M3),
    spatialArrayWeightType = MxFloat.withConfig(2, 4, 2, MxConfig.asymE5M2E2M3),
    lut = Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4,
      projFormat = LutFP8E5M2, actCodeWidth = 8, weiCodeWidth = 6)),
  )
}

// =========== MxFP Config ==========
class GemminiMxFPDefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.defaultMxFPConfig))
    }
  )
})

class GemminiMxFPStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.standaloneMxFPConfig))
    }
  )
})

class GemminiMxFPE5M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.e5m2MxFPConfig))
    }
  )
})

class GemminiMxFPAllStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.allMxFPConfig))
    }
  )
})

// Back-compat alias: the old E4M3-LUT fragment is now the all-formats build.
class GemminiMxFPE4M3LutStandaloneConfig extends GemminiMxFPAllStandaloneConfig

// Single-format builds (one MX format each; all other format hardware elaboration-gated).
class GemminiMxFPFp4OnlyStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.fp4OnlyMxFPConfig))
  })
})
class GemminiMxFPE3M2OnlyStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.e3m2OnlyMxFPConfig))
  })
})
class GemminiMxFPE2M3OnlyStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.e2m3OnlyMxFPConfig))
  })
})
class GemminiMxFPE4M3OnlyStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.e4m3OnlyMxFPConfig))
  })
})
class GemminiMxFPE5M2OnlyStandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.e5m2OnlyMxFPConfig))
  })
})
class GemminiMxFPAsymFp4Fp6StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymFp4Fp6MxFPConfig))
  })
})
class GemminiMxFPAsymFp6Fp4StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymFp6Fp4MxFPConfig))
  })
})
class GemminiMxFPAsymE5M2Fp4StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE5M2Fp4MxFPConfig))
  })
})
class GemminiMxFPAsymFp4E5M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymFp4E5M2MxFPConfig))
  })
})
class GemminiMxFPAsymE5M2E3M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE5M2E3M2MxFPConfig))
  })
})
class GemminiMxFPAsymE3M2E5M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE3M2E5M2MxFPConfig))
  })
})
class GemminiMxFPAsymE4M3Fp4StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE4M3Fp4MxFPConfig))
  })
})
class GemminiMxFPAsymFp4E4M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymFp4E4M3MxFPConfig))
  })
})
class GemminiMxFPAsymE4M3E3M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE4M3E3M2MxFPConfig))
  })
})
class GemminiMxFPAsymE4M3E5M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE4M3E5M2MxFPConfig))
  })
})
class GemminiMxFPAsymE3M2E4M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE3M2E4M3MxFPConfig))
  })
})
class GemminiMxFPAsymE5M2E4M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE5M2E4M3MxFPConfig))
  })
})
class GemminiMxFPAsymE2M3Fp4StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE2M3Fp4MxFPConfig))
  })
})
class GemminiMxFPAsymE2M3E3M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE2M3E3M2MxFPConfig))
  })
})
class GemminiMxFPAsymE2M3E5M2StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE2M3E5M2MxFPConfig))
  })
})
class GemminiMxFPAsymFp4E2M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymFp4E2M3MxFPConfig))
  })
})
class GemminiMxFPAsymE3M2E2M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE3M2E2M3MxFPConfig))
  })
})
class GemminiMxFPAsymE5M2E2M3StandaloneConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiMxFPConfigs.asymE5M2E2M3MxFPConfig))
  })
})

class GemminiMxFPTestConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.testMxFPConfig))
    }
  )
})

class GemminiRequantizerLutMxFPTestConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiMxFPConfigs.testRequantizerLutMxFPConfig))
    }
  )
})

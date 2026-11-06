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

  // BF16 baselines for the standalone MX builds: plain (non-MX) Gemmini (hardfloat BF16 FMA PEs, FP32 accumulator), no
  // LUT / scale factors / requantizer, plus the dual VPU. Memory side (spad/acc capacity, acc banks, DMA widths and
  // in-flight reqs, RS depth, WS dataflow) as standaloneMxFPConfig. One spad row = DIM BF16, so the VPU has DIM lanes.
  lazy val BF16BaselineConfig = BF16DefaultConfig.copy(
    // spad rows / elem_t follow the projected types, which defaultFPConfig leaves at FP32
    inputTypeProjected = Float(8, 8),
    weightTypeProjected = Float(8, 8),
    dataflow = Dataflow.WS,
    sp_singleported = false,
    acc_banks = 2,
    acc_sub_banks = 1,
    dma_maxbytes = 64,
    dma_buswidth = 512,
    max_spad_writer_bytes = 64,
    spad_writer_dma_width = 512,
    max_in_flight_mem_reqs = 32,
    reservation_station_entries_ld = 32,
  )
  // Mesh rows sum in BF16 (= spatialArrayOutputType, as upstream Gemmini BF16); the empty-list default would make the
  // tiles FP32 behind a BF16 mesh port.
  private def bf16Dim(dim: Int) = BF16BaselineConfig.copy(meshRows = dim, meshColumns = dim,
    meshAccPrecisionList = Seq.fill(dim)(Float(8, 8)),
    has_vpu = true, vpu_params = gemmini.vpu.VpuParams(units = 2, lanes = dim, expSub = true, expSum = true),
    headerFileName = s"gemmini_params_bf16_dim${dim}_vpu.h")
  lazy val BF16Dim8BaselineConfig  = bf16Dim(8)
  lazy val BF16Dim16BaselineConfig = bf16Dim(16)
  lazy val BF16Dim32BaselineConfig = bf16Dim(32).copy(max_in_flight_mem_reqs = 64)   // as dim32E4M3VpuMxFPConfig


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

//===========BFLOAT16 baselines for the MX builds (8x8, 16x16, 32x32)=========
class GemminiBF16Dim8BaselineConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiFPConfigs.BF16Dim8BaselineConfig))
  })
})
class GemminiBF16Dim16BaselineConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiFPConfigs.BF16Dim16BaselineConfig))
  })
})
class GemminiBF16Dim32BaselineConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(GemminiFPConfigs.BF16Dim32BaselineConfig))
  })
})

object GemminiMxFPConfigs {
  import Arithmetic.MxFloatArithmetic._
  import mxgen.MxConfig

  type MxCfg = GemminiArrayConfig[MxFloat, Float, Float]
  type Lane = (Int, Int, Int)

  def prodPrecision(dim: Int): Seq[MxFloat] = Seq.fill(dim)(MxFloat(4, 4, 4, true, false))
  def accPrecision(dim: Int): Seq[MxFloat] = {
    def bf16 = MxFloat(8, 8, 4, true, false)
    if (dim <= 8) Seq.fill(dim)(bf16)
    else Seq.fill(8)(MxFloat(4, 5, 4, true, false)) ++ Seq.fill(2)(MxFloat(4, 6, 4, true, false)) ++
      Seq.fill(5)(MxFloat(4, 7, 4, true, false)) ++ Seq.fill(dim - 15)(bf16)
  }

  val fp4: Lane = (2, 2, 2)
  val e3m2: Lane = (3, 3, 2)
  val e2m3: Lane = (2, 4, 2)
  val e4m3: Lane = (4, 4, 2)
  val e5m2: Lane = (5, 3, 2)
  val e4m3s: Lane = (4, 4, 1)

  def lut8(proj: LutProjFormat, act: Int = 0, wei: Int = 0): Option[GemminiLUTConfig] =
    Some(GemminiLUTConfig(Seq(128, 128, 128), Seq(64, 64, 64), rdataWidth = 8, raddrWidth = 4, projFormat = proj,
      actCodeWidth = act, weiCodeWidth = wei))
  def lut6(proj: LutProjFormat): Option[GemminiLUTConfig] =
    Some(GemminiLUTConfig(Seq(96, 96, 96), Seq(64, 64, 64), rdataWidth = 6, raddrWidth = 4, projFormat = proj))

  def lanes(c: MxCfg, act: Lane, wei: Lane, cfg: Option[MxConfig]): MxCfg = {
    def a = MxFloat(act._1, act._2, act._3, false, false, cfg)
    def w = MxFloat(wei._1, wei._2, wei._3, false, false, cfg)
    c.copy(inputType = a, weightType = w, spatialArrayInputType = a, spatialArrayWeightType = w)
  }
  def sym(c: MxCfg, lane: Lane, cfg: MxConfig): MxCfg = lanes(c, lane, lane, Some(cfg))

  def mesh(c: MxCfg, dim: Int, accBanks: Int): MxCfg = c.copy(
    meshRows = dim, meshColumns = dim,
    meshProdPrecisionList = prodPrecision(dim), meshAccPrecisionList = accPrecision(dim),
    scale_mem = c.scale_mem.map(_.copy(subbankLineSizeInBytes = dim)),
    acc_banks = accBanks)
  def vpu(c: MxCfg): MxCfg = c.copy(
    has_vpu = true,
    vpu_params = gemmini.vpu.VpuParams(units = 2, lanes = c.meshColumns / 2, expSub = true, expSum = true),
    reservation_station_entries_ld = 32)

  def all(c: MxCfg): MxCfg = lanes(c, e4m3, e4m3, None).copy(lut = lut8(LutFP8E4M3))
  def allAsym(c: MxCfg): MxCfg = sym(c, e4m3, MxConfig.allAsym).copy(lut = lut8(LutFP8E4M3, 8, 8))
  def fp4Only(c: MxCfg): MxCfg = sym(c, fp4, MxConfig.fp4Only)
  def e3m2Only(c: MxCfg): MxCfg = sym(c, e3m2, MxConfig.e3m2Only)
  def mxGemmini(c: MxCfg): MxCfg = sym(c, e3m2, MxConfig.mxGemmini)
  def e4m3Single(c: MxCfg): MxCfg = sym(c, e4m3s, MxConfig.e4m3SingleOnly).copy(lut = None)
  def asym(cfg: MxConfig, act: Lane, wei: Lane, lut: Option[GemminiLUTConfig] = standaloneMxFPConfig.lut): MxCfg =
    lanes(standaloneMxFPConfig, act, wei, Some(cfg)).copy(lut = lut)

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
    shifter_banks = 1,
    dataflow = Dataflow.WS,
    acc_capacity = CapacityInKilobytes(64),
    spad_read_delay = 1,
    dma_maxbytes = 64,
    dma_buswidth = 512,
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
    meshProdPrecisionList = prodPrecision(16),
    meshAccPrecisionList = accPrecision(16),
    scaleSize = 32,
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
    requantizer = Some(GemminiRequantizerConfig()),
    scale_mem = Some(GemminiScalingFactorMemConfig(
      baseAddr = 0x10000000L + 0x8000,
      sizeInBytes = 16 << 10,
      subbankLineSizeInBytes = 16,
      subbanksPerBank = 2,
      numBanks = 8,
    ))
  )

  val testMxFPConfig = defaultMxFPConfig.copy(testConfig = true)
  val testRequantizerLutMxFPConfig = defaultMxFPConfig.copy(testConfig = true, lut = Some(GemminiLUTConfig()))

  val standaloneMxFPConfig = defaultMxFPConfig.copy(
    ex_read_from_acc = false,
    ex_write_to_spad = true,
    sp_singleported = false,
    spad_read_delay = 4,
    tile_latency = 0,
    mesh_output_delay = 1,
    acc_latency = 3,
    num_counter = 8,
    max_in_flight_mem_reqs = 32,
    acc_banks = 2,
    lut = Some(GemminiLUTConfig()),
    scale_mem = Some(defaultMxFPConfig.scale_mem.get.copy(baseAddr = 0x20000000L))
  )

  val dim32MxFPConfig = mesh(standaloneMxFPConfig, 32, accBanks = 1)
  val dim8MxFPConfig = mesh(standaloneMxFPConfig, 8, accBanks = 1)
  val dim4MxFPConfig = vpu(mesh(standaloneMxFPConfig, 4, accBanks = 2)).copy(sp_capacity = CapacityInKilobytes(128))

  val dim32AllMxFPConfig = all(dim32MxFPConfig)
  val dim32AllAsymMxFPConfig = allAsym(dim32MxFPConfig)
  val dim32E4M3VpuMxFPConfig = vpu(e4m3Single(dim32MxFPConfig))
    .copy(acc_banks = 2, max_in_flight_mem_reqs = 64, headerFileName = "gemmini_params_dim32_e4m3_vpu.h")
  val dim32E4M3VpuAcc2MxFPConfig = dim32E4M3VpuMxFPConfig.copy(headerFileName = "gemmini_params_dim32_e4m3_vpu_acc2.h")
  val dim32E3M2VpuMxFPConfig = vpu(e3m2Only(dim32MxFPConfig)).copy(headerFileName = "gemmini_params_dim32_e3m2_vpu.h")
  val dim32Fp4VpuMxFPConfig = vpu(fp4Only(dim32MxFPConfig)).copy(headerFileName = "gemmini_params_dim32_fp4_vpu.h")

  val dim8AllMxFPConfig = all(dim8MxFPConfig)
  val dim8AllAsymMxFPConfig = allAsym(dim8MxFPConfig)
  val dim8E3M2OnlyMxFPConfig = e3m2Only(dim8MxFPConfig).copy(acc_banks = 2, headerFileName = "gemmini_params_dim8_e3m2.h")
  val dim8E3M2VpuMxFPConfig = vpu(dim8E3M2OnlyMxFPConfig).copy(headerFileName = "gemmini_params_dim8_e3m2_vpu.h")
  val dim8Fp4OnlyMxFPConfig = fp4Only(dim8MxFPConfig).copy(acc_banks = 2, headerFileName = "gemmini_params_dim8_fp4.h")
  val dim8Fp4VpuMxFPConfig = vpu(dim8Fp4OnlyMxFPConfig).copy(headerFileName = "gemmini_params_dim8_fp4_vpu.h")
  val dim8Fp4E3M2E4M3VpuMxFPConfig = vpu(mxGemmini(dim8MxFPConfig))
    .copy(acc_banks = 2, headerFileName = "gemmini_params_dim8_fp4_e3m2_e4m3_vpu.h")

  val dim4Fp4VpuMxFPConfig = fp4Only(dim4MxFPConfig).copy(headerFileName = "gemmini_params_dim4_fp4_vpu.h")
  val dim4E3M2VpuMxFPConfig = e3m2Only(dim4MxFPConfig).copy(headerFileName = "gemmini_params_dim4_e3m2_vpu.h")
  val dim4Fp4E3M2VpuMxFPConfig = mxGemmini(dim4MxFPConfig).copy(headerFileName = "gemmini_params_dim4_fp4_e3m2_vpu.h")

  val fp4E3M2E4M3VpuMxFPConfig = vpu(mxGemmini(standaloneMxFPConfig)).copy(headerFileName = "gemmini_params_fp4_e3m2_e4m3_vpu.h")
  val e5m2MxFPConfig = lanes(standaloneMxFPConfig, e5m2, e5m2, None).copy(lut = lut8(LutFP8E5M2))
  val allMxFPConfig = all(standaloneMxFPConfig)
  val e4m3LutMxFPConfig = allMxFPConfig
  val allAsymMxFPConfig = allAsym(standaloneMxFPConfig)
  val fp4OnlyMxFPConfig = fp4Only(standaloneMxFPConfig)
  val e3m2OnlyMxFPConfig = e3m2Only(standaloneMxFPConfig)
  val e3m2VpuMxFPConfig = vpu(e3m2OnlyMxFPConfig)
  val fp4VpuMxFPConfig = vpu(fp4OnlyMxFPConfig).copy(headerFileName = "gemmini_params_fp4_vpu.h")
  val e2m3OnlyMxFPConfig = sym(standaloneMxFPConfig, e2m3, MxConfig.e2m3Only).copy(lut = lut6(LutFP6E2M3))
  val e4m3OnlyMxFPConfig = sym(standaloneMxFPConfig, e4m3, MxConfig.e4m3Only).copy(lut = lut8(LutFP8E4M3))
  val e4m3SingleNoLutMxFPConfig = e4m3Single(standaloneMxFPConfig)
  val e4m3TrimmedMxFPConfig = vpu(e4m3SingleNoLutMxFPConfig).copy(
    has_loop_conv = false,
    has_first_layer_optimizations = false,
    has_im2col = false,
    has_transposer = false,
    has_acc_mvin = false,
    has_nonlinear_activations = true,
    has_normalizations = false)
  val e4m3Fp4TrimmedMxFPConfig = sym(e4m3TrimmedMxFPConfig, e4m3s, MxConfig.e4m3SingleFp4)
  val e5m2OnlyMxFPConfig = sym(standaloneMxFPConfig, e5m2, MxConfig.e5m2Only).copy(lut = lut8(LutFP8E5M2))

  // Dual-Gemmini SoC pair (16x16 on custom3, 32x32 on custom2): FP4 sym (mode0) + E4M3s sym (mode8) + A8W4 (mode6).
  val a8w4Mx = MxConfig(Set(mxgen.MxFormat.FP4, mxgen.MxFormat.FP8_E4M3), Set(mxgen.MxFormat.FP4, mxgen.MxFormat.FP8_E4M3),
    productFormat = mxgen.MxFormat.Custom(4, 4),
    modesOverride = Some(List(mxgen.MxPEParams.mode0, mxgen.MxPEParams.mode6, mxgen.MxPEParams.mode8)),
    inActBusWidth = 8, inWeiBusWidth = 8, expAdderWidths = Seq(4, 4, 4, 4))
  // Same plus E3M2 sym (mode4); the 12b E3M2 lane carries the 8b codes too.
  val a8w4E3M2Mx = MxConfig(
    Set(mxgen.MxFormat.FP4, mxgen.MxFormat.FP6_E3M2, mxgen.MxFormat.FP8_E4M3),
    Set(mxgen.MxFormat.FP4, mxgen.MxFormat.FP6_E3M2, mxgen.MxFormat.FP8_E4M3),
    productFormat = mxgen.MxFormat.Custom(4, 4),
    modesOverride = Some(List(mxgen.MxPEParams.mode0, mxgen.MxPEParams.mode4, mxgen.MxPEParams.mode6,
      mxgen.MxPEParams.mode8)),
    inActBusWidth = 12, inWeiBusWidth = 12, expAdderWidths = Seq(4, 4, 4, 4))
  private def dual(c: MxCfg, lane: Lane, cfg: MxConfig, op: OpcodeSet, hdr: String,
                   lut: Option[GemminiLUTConfig] = None): MxCfg =
    vpu(sym(c, lane, cfg).copy(lut = lut, opcodes = op, headerFileName = hdr, acc_banks = 2))  // VPU + store/compute overlap
  val a8w4Dim16MxFPConfig = dual(standaloneMxFPConfig, e4m3s, a8w4Mx, OpcodeSet.custom3, "gemmini_params_a8w4_dim16.h")
  val a8w4Dim32MxFPConfig = dual(dim32MxFPConfig, e4m3s, a8w4Mx, OpcodeSet.custom2, "gemmini_params_a8w4_dim32.h")
  val a8w4E3M2Dim16MxFPConfig = dual(standaloneMxFPConfig, e3m2, a8w4E3M2Mx, OpcodeSet.custom3,
    "gemmini_params_a8w4_e3m2_dim16.h", standaloneMxFPConfig.lut)  // E3M2 needs the QuantLut
  val a8w4E3M2Dim32MxFPConfig = dual(dim32MxFPConfig, e3m2, a8w4E3M2Mx, OpcodeSet.custom2,
    "gemmini_params_a8w4_e3m2_dim32.h", standaloneMxFPConfig.lut)  // E3M2 needs the QuantLut
  val a8w4Dim8MxFPConfig = dual(dim8MxFPConfig, e4m3s, a8w4Mx, OpcodeSet.custom2, "gemmini_params_a8w4_dim8.h")
  val a8w4E3M2Dim8MxFPConfig = dual(dim8MxFPConfig, e3m2, a8w4E3M2Mx, OpcodeSet.custom2,
    "gemmini_params_a8w4_e3m2_dim8.h", standaloneMxFPConfig.lut)  // E3M2 needs the QuantLut

  val asymFp4Fp6MxFPConfig = asym(MxConfig.asymFp4Fp6, fp4, e3m2)
  val asymFp6Fp4MxFPConfig = asym(MxConfig.asymFp6Fp4, e3m2, fp4)
  val asymE5M2Fp4MxFPConfig = asym(MxConfig.asymE5M2Fp4, e5m2, fp4, lut8(LutFP8E5M2))
  val asymFp4E5M2MxFPConfig = asym(MxConfig.asymFp4E5M2, fp4, e5m2, lut8(LutFP8E5M2))
  val asymE5M2E3M2MxFPConfig = asym(MxConfig.asymE5M2E3M2, e5m2, e3m2, lut8(LutFP8E5M2, 8, 6))
  val asymE3M2E5M2MxFPConfig = asym(MxConfig.asymE3M2E5M2, e3m2, e5m2, lut8(LutFP8E5M2, 6, 8))
  val asymE4M3Fp4MxFPConfig = asym(MxConfig.asymE4M3Fp4, e4m3, fp4, lut8(LutFP8E4M3))
  val asymFp4E4M3MxFPConfig = asym(MxConfig.asymFp4E4M3, fp4, e4m3, lut8(LutFP8E4M3, wei = 8))
  val asymE4M3E3M2MxFPConfig = asym(MxConfig.asymE4M3E3M2, e4m3, e3m2, lut8(LutFP8E4M3, 8, 6))
  val asymE4M3E5M2MxFPConfig = asym(MxConfig.asymE4M3E5M2, e4m3, e5m2, lut8(LutFP8E4M3, 8, 8))
  val asymE3M2E4M3MxFPConfig = asym(MxConfig.asymE3M2E4M3, e3m2, e4m3, lut8(LutFP8E4M3, 6, 8))
  val asymE5M2E4M3MxFPConfig = asym(MxConfig.asymE5M2E4M3, e5m2, e4m3, lut8(LutFP8E4M3, 8, 8))
  val asymE2M3Fp4MxFPConfig = asym(MxConfig.asymE2M3Fp4, e2m3, fp4, lut6(LutFP6E2M3))
  val asymE2M3E3M2MxFPConfig = asym(MxConfig.asymE2M3E3M2, e2m3, e3m2, lut6(LutFP6E2M3))
  val asymE2M3E5M2MxFPConfig = asym(MxConfig.asymE2M3E5M2, e2m3, e5m2, lut8(LutFP8E5M2, 6, 8))
  val asymFp4E2M3MxFPConfig = asym(MxConfig.asymFp4E2M3, fp4, e2m3, lut6(LutFP6E2M3))
  val asymE3M2E2M3MxFPConfig = asym(MxConfig.asymE3M2E2M3, e3m2, e2m3, lut6(LutFP6E2M3))
  val asymE5M2E2M3MxFPConfig = asym(MxConfig.asymE5M2E2M3, e5m2, e2m3, lut8(LutFP8E5M2, 8, 6))
  val asymE2M3E4M3MxFPConfig = asym(MxConfig.asymE2M3E4M3, e2m3, e4m3, lut8(LutFP8E4M3, 6, 8))
  val asymE4M3E2M3MxFPConfig = asym(MxConfig.asymE4M3E2M3, e4m3, e2m3, lut8(LutFP8E4M3, 8, 6))
}

class WithMxFPGemmini(cfg: GemminiMxFPConfigs.MxCfg) extends Config((site, here, up) => {
  case BuildRoCC => Seq((p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(cfg))
  })
})


class GemminiMxFPDefaultConfig extends WithMxFPGemmini(GemminiMxFPConfigs.defaultMxFPConfig)
class GemminiMxFPStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.standaloneMxFPConfig)
class GemminiMxFPDim32StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32MxFPConfig)
class GemminiMxFPDim32AllStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32AllMxFPConfig)
class GemminiMxFPDim32AllAsymStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32AllAsymMxFPConfig)
class GemminiMxFPDim8E3M2OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8E3M2OnlyMxFPConfig)
class GemminiMxFPDim8E3M2VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8E3M2VpuMxFPConfig)
class GemminiMxFPDim8Fp4VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8Fp4VpuMxFPConfig)
class GemminiMxFPDim4Fp4VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim4Fp4VpuMxFPConfig)
class GemminiMxFPDim4E3M2VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim4E3M2VpuMxFPConfig)
class GemminiMxFPDim4Fp4E3M2VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim4Fp4E3M2VpuMxFPConfig)
class GemminiMxFPDim8Fp4E3M2E4M3VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8Fp4E3M2E4M3VpuMxFPConfig)
class GemminiMxFPFp4E3M2E4M3VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.fp4E3M2E4M3VpuMxFPConfig)
class GemminiMxFPDim32E4M3VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32E4M3VpuMxFPConfig)
class GemminiMxFPDim32E4M3VpuAcc2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32E4M3VpuAcc2MxFPConfig)
class GemminiMxFPDim32E3M2VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32E3M2VpuMxFPConfig)
class GemminiMxFPDim32Fp4VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim32Fp4VpuMxFPConfig)
class GemminiMxFPDim8Fp4OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8Fp4OnlyMxFPConfig)
class GemminiMxFPDim8AllStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8AllMxFPConfig)
class GemminiMxFPDim8AllAsymStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.dim8AllAsymMxFPConfig)
class GemminiMxFPE5M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e5m2MxFPConfig)
class GemminiMxFPAllStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.allMxFPConfig)
class GemminiMxFPE4M3LutStandaloneConfig extends GemminiMxFPAllStandaloneConfig
class GemminiMxFPAllAsymStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.allAsymMxFPConfig)
class GemminiMxFPFp4OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.fp4OnlyMxFPConfig)
class GemminiMxFPE3M2OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e3m2OnlyMxFPConfig)
class GemminiMxFPE3M2VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e3m2VpuMxFPConfig)
class GemminiMxFPFp4VpuStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.fp4VpuMxFPConfig)
class GemminiMxFPE2M3OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e2m3OnlyMxFPConfig)
class GemminiMxFPE4M3OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e4m3OnlyMxFPConfig)
class GemminiMxFPE4M3TrimmedStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e4m3TrimmedMxFPConfig)
class GemminiMxFPE4M3Fp4TrimmedStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e4m3Fp4TrimmedMxFPConfig)
class GemminiMxFPE4M3SingleNoLutStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e4m3SingleNoLutMxFPConfig)
class GemminiMxFPE5M2OnlyStandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.e5m2OnlyMxFPConfig)
class GemminiMxFPAsymFp4Fp6StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymFp4Fp6MxFPConfig)
class GemminiMxFPAsymFp6Fp4StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymFp6Fp4MxFPConfig)
class GemminiMxFPAsymE5M2Fp4StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE5M2Fp4MxFPConfig)
class GemminiMxFPAsymFp4E5M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymFp4E5M2MxFPConfig)
class GemminiMxFPAsymE5M2E3M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE5M2E3M2MxFPConfig)
class GemminiMxFPAsymE3M2E5M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE3M2E5M2MxFPConfig)
class GemminiMxFPAsymE4M3Fp4StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE4M3Fp4MxFPConfig)
class GemminiMxFPAsymFp4E4M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymFp4E4M3MxFPConfig)
class GemminiMxFPAsymE4M3E3M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE4M3E3M2MxFPConfig)
class GemminiMxFPAsymE4M3E5M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE4M3E5M2MxFPConfig)
class GemminiMxFPAsymE3M2E4M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE3M2E4M3MxFPConfig)
class GemminiMxFPAsymE5M2E4M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE5M2E4M3MxFPConfig)
class GemminiMxFPAsymE2M3Fp4StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE2M3Fp4MxFPConfig)
class GemminiMxFPAsymE2M3E3M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE2M3E3M2MxFPConfig)
class GemminiMxFPAsymE2M3E5M2StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE2M3E5M2MxFPConfig)
class GemminiMxFPAsymFp4E2M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymFp4E2M3MxFPConfig)
class GemminiMxFPAsymE3M2E2M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE3M2E2M3MxFPConfig)
class GemminiMxFPAsymE5M2E2M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE5M2E2M3MxFPConfig)
class GemminiMxFPAsymE2M3E4M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE2M3E4M3MxFPConfig)
class GemminiMxFPAsymE4M3E2M3StandaloneConfig extends WithMxFPGemmini(GemminiMxFPConfigs.asymE4M3E2M3MxFPConfig)
class GemminiMxFPTestConfig extends WithMxFPGemmini(GemminiMxFPConfigs.testMxFPConfig)
class GemminiRequantizerLutMxFPTestConfig extends WithMxFPGemmini(GemminiMxFPConfigs.testRequantizerLutMxFPConfig)

// Two independent MX Gemminis on one tile: 16x16 (custom3) + a 32x32 or 8x8 (custom2), nothing shared but the tile's
// RoCC ports.
class WithDualMxFPGemmini(c16: GemminiMxFPConfigs.MxCfg, c2: GemminiMxFPConfigs.MxCfg) extends Config((site, here, up) => {
  case BuildRoCC => Seq(c16, c2).map { cfg => (p: Parameters) => {
    implicit val q = p; implicit val v = implicitly[ValName]
    LazyModule(new Gemmini(cfg))
  }: freechips.rocketchip.tile.LazyRoCC }
})
class GemminiMxFPDualA8W4Config extends WithDualMxFPGemmini(
  GemminiMxFPConfigs.a8w4Dim16MxFPConfig, GemminiMxFPConfigs.a8w4Dim32MxFPConfig)
class GemminiMxFPDualA8W4E3M2Config extends WithDualMxFPGemmini(
  GemminiMxFPConfigs.a8w4E3M2Dim16MxFPConfig, GemminiMxFPConfigs.a8w4E3M2Dim32MxFPConfig)
// 16x16 (custom3) + 8x8 (custom2)
class GemminiMxFPDualA8W4Dim8Config extends WithDualMxFPGemmini(
  GemminiMxFPConfigs.a8w4Dim16MxFPConfig, GemminiMxFPConfigs.a8w4Dim8MxFPConfig)
class GemminiMxFPDualA8W4E3M2Dim8Config extends WithDualMxFPGemmini(
  GemminiMxFPConfigs.a8w4E3M2Dim16MxFPConfig, GemminiMxFPConfigs.a8w4E3M2Dim8MxFPConfig)

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
    weightStorageType = Float(8, 24),
    inputStorageType = Float(8, 24),
    accStorageType = Float(8, 24),
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
  val FP16DefaultConfig = defaultFPConfig.copy(inputType = Float(5, 11), weightType = Float(5, 11), accType = Float(8, 24), inputStorageType = Float(5, 11), weightStorageType = Float(5, 11), accStorageType = Float(8, 24), spatialArrayInputType = Float(5, 11), spatialArrayWeightType = Float(5, 11), spatialArrayOutputType = Float(5, 11),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
  
  //Bfloat16 Brain-half Precision Configuration
  val BF16DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24), inputStorageType = Float(8, 8), weightStorageType = Float(8, 8), accStorageType = Float(8, 24), spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  //Bfloat16 Brain-half Precision Configuration 8x8 array
  val BF16Default8Config = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24), inputStorageType = Float(8, 8), weightStorageType = Float(8, 8), accStorageType = Float(8, 24), spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               meshRows = 8, meshColumns = 8,
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  // BF16 baselines for the MX builds: plain Gemmini (BF16 FMA PEs, FP32 acc) + dual VPU, memory side as
  // standaloneMxFPConfig
  lazy val BF16BaselineConfig = BF16DefaultConfig.copy(
    // spad rows / elem_t follow the storage types, which defaultFPConfig leaves at FP32
    inputStorageType = Float(8, 8),
    weightStorageType = Float(8, 8),
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
  // mesh rows sum in BF16 (= spatialArrayOutputType), not FP32
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

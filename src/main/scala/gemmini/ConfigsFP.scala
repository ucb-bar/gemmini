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
  // import Arithmetic.FloatArithmetic._
  import Arithmetic.MxFloatArithmetic._
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

    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_mem_reqs = 16,
    use_dedicated_tl_port = false,
    use_shared_ext_mem = false,

    inputType = MxFloat(3, 3, 2), //consider worst case for total bit length, inputType maximal is 12bits? same case for projection? 
    weightType = MxFloat(3, 3, 2),
    accType = MxFloat(8, 8, 4),
    // weightTypeProjected = MxFloat(2, 2, 4), //projected to lower precision
    weightTypeProjected = MxFloat(2, 2, 2), // should be projected, but errors if weightType != weightTypeProjected
    inputTypeProjected = MxFloat(2, 2, 2), 
    accTypeProjected = MxFloat(8, 8, 4),

    spatialArrayInputType = MxFloat(3, 3, 2), 
    spatialArrayWeightType = MxFloat(3, 3, 2),
    spatialArrayOutputType = MxFloat(8, 8, 4, true, false),

    meshProdPrecisionList = Seq.fill(4) {(8, 8)} ++ 
                                 Seq.fill(4) {(8, 8)} ++ 
                                 Seq.fill(4) {(8, 8)} ++ 
                                 Seq.fill(4) {(8, 8)}, 

    meshAccPrecisionList =     Seq.fill(4) {MxFloat(8, 8, 4, true, false)} ++ 
                                 Seq.fill(4) {MxFloat(8, 8, 4, true, false)} ++ 
                                 Seq.fill(4) {MxFloat(8, 8, 4, true, false)} ++ 
                                 Seq.fill(4) {MxFloat(8, 8, 4, true, false)},

                                 // 16x16 mesh with varying precisions
    scaleMem_data_width = 128,
    scaleMem_bank_entries = 256,
    scaleSize = 32,
    enable_lut = true,
    mvin_scale_args = None,
    mvin_scale_acc_args = None,
    mvin_scale_shared = false,

    // acc_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 8), -1, identity = "1.0",
    //   c_str = "((x) * (scale))"
    // )),
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
        sizeInBytes = 16 << 10,
        sramLineSizeInBytes = 256 / 8,
        numBanks = 8,
        ))
  )

  val testMxFPConfig = defaultMxFPConfig.copy(testConfig = true)
  val testRequantizerLutMxFPConfig = defaultMxFPConfig.copy(
    testConfig = true,
    enable_lut = true,
    lut = Some(GemminiLUTConfig())
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

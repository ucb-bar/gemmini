
package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
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
  val FP32DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 24), weightType = Float(8, 24), accType = Float(8, 24),
                                               spatialArrayInputType = Float(8, 24), spatialArrayWeightType = Float(8, 24), spatialArrayOutputType = Float(8, 24),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
 
  //FP16 Half Precision Configuration
  val FP16DefaultConfig = defaultFPConfig.copy(inputType = Float(5, 11), weightType = Float(5, 11), accType = Float(8, 24),
                                               spatialArrayInputType = Float(5, 11), spatialArrayWeightType = Float(5, 11), spatialArrayOutputType = Float(5, 11),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
  
  //Bfloat16 Brain-half Precision Configuration
  val BF16DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24),
                                               spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  //Bfloat16 Brain-half Precision Configuration 8x8 array
  val BF16Default8Config = defaultFPConfig.copy(inputType = Float(8, 8), weightType = Float(8, 8), accType = Float(8, 24),
                                               spatialArrayInputType = Float(8, 8), spatialArrayWeightType = Float(8, 8), spatialArrayOutputType = Float(8, 8),
                                               meshRows = 8, meshColumns = 8,
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  val FP8TransformerConfig = defaultFPConfig.copy(
    inputType = Float(4, 4), weightType = Float(4, 4), accType = Float(8, 24),

    spatialArrayInputType = Float(4, 4, isRecoded = true), spatialArrayWeightType = Float(4, 4, isRecoded = true), spatialArrayOutputType = Float(5, 11, isRecoded = true),

    has_normalizations = false, // TODO this should be true
    has_dw_convs = false, has_training_convs = false, has_max_pool = false, has_first_layer_optimizations = false, has_nonlinear_activations = false,

    dataflow=Dataflow.WS, max_in_flight_mem_reqs = 64, acc_read_full_width = false, ex_read_from_acc = false, ex_write_to_spad = false, hardcode_d_to_garbage_addr = true,

    meshRows = 16, meshColumns = 16,
    acc_capacity = CapacityInKilobytes(128),

    tile_latency = 2,
  )

  val HybridFP8TransformerConfig = FP8TransformerConfig.copy(
    weightType = Float(5, 3),
    spatialArrayInputType = Float(5, 4, isRecoded = true), spatialArrayWeightType = Float(5, 4, isRecoded = true),
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

// FP8 Transformer configs
class GemminiFP8TransformerConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      LazyModule(new Gemmini(GemminiFPConfigs.FP8TransformerConfig))
    }
  )
})

class GemminiHybridFP8TransformerConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      LazyModule(new Gemmini(GemminiFPConfigs.HybridFP8TransformerConfig))
    }
  )
})

package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}
import freechips.rocketchip.diplomacy._


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
    acc_banks = 2,
    acc_latency = 2,
    acc_singleported = false,
    acc_sub_banks = -1,
    sp_capacity = CapacityInKilobytes(128),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.WS,
    acc_capacity = CapacityInKilobytes(64),
    spad_read_delay = 1,

    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_mem_reqs = 64,
    //use_dedicated_tl_port = false,
    use_shared_ext_mem = false,
    inputType = Float(8, 24),
    spatialArrayOutputType = Float(8, 24),
    accType = Float(8, 24),

    mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
    mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
    mvin_scale_shared = false,

    acc_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0",
      c_str = "((x) * (scale))"
    )),
    acc_read_full_width = false,
    acc_read_small_width = true,

    tile_latency = 1,

    ex_read_from_spad = true,
    ex_read_from_acc = false,
    ex_write_to_spad = false,
    ex_write_to_acc = true,

    hardcode_d_to_garbage_addr = true,

    mesh_output_delay = 0,

    has_training_convs = false,
    has_max_pool = true,
    has_nonlinear_activations = true,

    num_counter = 0,
    use_firesim_simulation_counters = false,
  )
  
  //FP32 Single Precision Configuration
  val FP32DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 24), spatialArrayOutputType = Float(8, 24), accType = Float(8, 24),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
 
  //FP16 Half Precision Configuration
  val FP16DefaultConfig = defaultFPConfig.copy(inputType = Float(5, 11), spatialArrayOutputType = Float(5, 11), accType = Float(8, 24),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(5, 11), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )
  
  //Bfloat16 Brain-half Precision Configuration
  val BF16DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 8), spatialArrayOutputType = Float(8, 8), accType = Float(8, 24),
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  //Bfloat16 Brain-half Precision Configuration 8x8 array
  val BF16Default8Config = defaultFPConfig.copy(inputType = Float(8, 8), spatialArrayOutputType = Float(8, 8), accType = Float(8, 24),
                                               meshRows = 8, meshColumns = 8,
                                               tile_latency = 2,
                                               mvin_scale_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                               mvin_scale_acc_args = Some(ScaleArguments((t: Float, u: Float) => t * u, 4, Float(8, 24), -1, identity = "1.0", c_str="((x) * (scale))")),
                                              )

  val dummyFP32Config = GemminiArrayConfig[DummySInt, Float, Float](
    inputType = DummySInt(32),
    accType = DummySInt(32),
    spatialArrayOutputType = DummySInt(32),
    tileRows     = FP32DefaultConfig.tileRows,
    tileColumns  = FP32DefaultConfig.tileColumns,
    meshRows     = FP32DefaultConfig.meshRows,
    meshColumns  = FP32DefaultConfig.meshColumns,
    dataflow     = FP32DefaultConfig.dataflow,
    sp_capacity  = FP32DefaultConfig.sp_capacity,
    acc_capacity = FP32DefaultConfig.acc_capacity,
    sp_banks     = FP32DefaultConfig.sp_banks,
    acc_banks    = FP32DefaultConfig.acc_banks,
    sp_singleported = FP32DefaultConfig.sp_singleported,
    acc_singleported = FP32DefaultConfig.acc_singleported,
    has_training_convs = FP32DefaultConfig.has_training_convs,
    has_max_pool = FP32DefaultConfig.has_max_pool,
    has_nonlinear_activations = FP32DefaultConfig.has_nonlinear_activations,
    //reservation_station_full_entries = FP32DefaultConfig.reservation_station_full_entries,
    //reservation_station_partial_entries = FP32DefaultConfig.reservation_station_partial_entries,
    ld_queue_length = FP32DefaultConfig.ld_queue_length,
    st_queue_length = FP32DefaultConfig.st_queue_length,
    ex_queue_length = FP32DefaultConfig.ex_queue_length,
    max_in_flight_mem_reqs = FP32DefaultConfig.max_in_flight_mem_reqs,
    dma_maxbytes = FP32DefaultConfig.dma_maxbytes,
    dma_buswidth = FP32DefaultConfig.dma_buswidth,
    tlb_size = FP32DefaultConfig.tlb_size,

    mvin_scale_args = Some(ScaleArguments(
      (t: DummySInt, f: Float) => t.dontCare,
      4, Float(8, 24), 4,
      identity = "1.0",
      c_str="((x) * (scale))"
      //c_str = "({float y = ROUND_NEAR_EVEN((x) * (scale)); y > INT8_MAX ? INT8_MAX : (y < INT8_MIN ? INT8_MIN : (elem_t)y);})"
    )),

    use_firesim_simulation_counters = false,//true,

    mvin_scale_acc_args = None,
    mvin_scale_shared = FP32DefaultConfig.mvin_scale_shared,

    acc_scale_args = Some(ScaleArguments(
      (t: DummySInt, f: Float) => t.dontCare,
      1, Float(8, 24), -1,
      identity = "1.0",
      c_str="((x) * (scale))"
      //c_str = "({float y = ROUND_NEAR_EVEN((x) * (scale)); y > INT8_MAX ? INT8_MAX : (y < INT8_MIN ? INT8_MIN : (acc_t)y);})"
    )),

    num_counter = FP32DefaultConfig.num_counter,

    acc_read_full_width = FP32DefaultConfig.acc_read_full_width,
    acc_read_small_width = FP32DefaultConfig.acc_read_small_width,

    ex_read_from_spad = FP32DefaultConfig.ex_read_from_spad,
    ex_read_from_acc = FP32DefaultConfig.ex_read_from_acc,
    ex_write_to_spad = FP32DefaultConfig.ex_write_to_spad,
    ex_write_to_acc = FP32DefaultConfig.ex_write_to_acc,
  )

}


//===========FP32 Default Config=========
class GemminiFP32DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(GemminiFPConfigs.FP32DefaultConfig))
    }
  )
})

class GemminiFP32VectorConfig extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      val gemmini = LazyModule(new Gemmini(GemminiFPConfigs.FP32DefaultConfig.copy(
        opcodes=OpcodeSet.custom2,
        sp_capacity=CapacityInKilobytes(64), 
        acc_capacity=CapacityInKilobytes(32),
        headerFileName="gemmini_vector_params_fp32.h"
      )))
      InModuleBody {
        println(gemmini.config.opcodes.opcodes, gemmini.config.dataflow)
      }
      gemmini
    }
  )
})

class DummyFP32DefaultGemminiConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiFPConfigs.dummyFP32Config
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig.copy(opcodes=OpcodeSet.custom3, headerFileName = "gemmini_params_fp32.h")))
      InModuleBody {
        println(gemmini.config.opcodes.opcodes, gemmini.config.dataflow)
      }
      gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
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


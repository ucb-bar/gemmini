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
  val defaultFPConfig = GemminiArrayConfig[Float, Float](
    tileRows = 1,
    tileColumns = 1,
    meshRows = 4,
    meshColumns = 4,
    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,
    rob_entries = 16,
    sp_banks = 4,
    acc_banks = 1,
    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.BOTH,
    acc_capacity = CapacityInKilobytes(64),
    mem_pipeline = 1,
    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    inputType = Float(8, 24),
    outputType = Float(8, 24),
    accType = Float(8, 24),
    mvin_scale_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
    mvin_scale_acc_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
    mvin_scale_shared = false,
    acc_read_full_width = true,
    acc_read_small_width = true,
    pe_latency = 1
    //pe_latency = 0
  )
  
  //FP32 Single Precision Configuration
  val FP32DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 24), outputType = Float(8, 24), accType = Float(8, 24),
                                               pe_latency = 2,
                                               mvin_scale_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
                                               mvin_scale_acc_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24)))
                                              )
  
  //FP16 Half Precision Configuration
  val FP16DefaultConfig = defaultFPConfig.copy(inputType = Float(5, 11), outputType = Float(5, 11), accType = Float(8, 24),
                                               mvin_scale_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(5, 11))),
                                               mvin_scale_acc_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(5, 11)))
                                              )
  
  //Bfloat16 Brain-half Precision Configuration
  val BF16DefaultConfig = defaultFPConfig.copy(inputType = Float(8, 8), outputType = Float(8, 8), accType = Float(8, 24),
                                               mvin_scale_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24))),
                                               mvin_scale_acc_args = Some(MvinScaleArguments((t: Float, u: Float) => t * u, 0, Float(8, 24)))
                                              )
}


//===========FP32 Default Config=========
class GemminiFP32DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiFPConfigs.FP32DefaultConfig))
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
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiFPConfigs.FP16DefaultConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========BFLOAT16 Default Config=========
class GemminiBF16DefaultConfig extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(OpcodeSet.custom3, GemminiFPConfigs.BF16DefaultConfig))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


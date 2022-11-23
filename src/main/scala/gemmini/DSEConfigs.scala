
package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}

// -----------------------------
// Design Space Exploration Mixins
// -----------------------------

object DSEBaseConfig {
  val baseConfig = GemminiArrayConfig[SInt, Bool, UInt](
    opcodes = OpcodeSet.custom3,
    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,
    ld_queue_length = 4,
    st_queue_length = 2,
    ex_queue_length = 8,
    reservation_station_entries_ld = 8,
    reservation_station_entries_st = 4,
    reservation_station_entries_ex = 16,


  sp_banks = 4, // TODO support one-bank designs
    acc_banks = 1,
    acc_singleported = false,
    acc_latency = 2,
    sp_capacity = CapacityInKilobytes(64),
    sp_singleported = false,
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.OS,
    acc_capacity = CapacityInKilobytes(16),
    spad_read_delay = 1,
    dma_maxbytes = 128, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 16,
    inputType = SInt(8.W),
    spatialArrayOutputType = SInt(19.W),
    accType = SInt(32.W),
    mvin_scale_args = None,
    mvin_scale_acc_args = None,
    mvin_scale_shared = false,
    acc_scale_args = Some(ScaleArguments(
      (t: SInt, u: UInt) => {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, t(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, t.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = t(u)

        val r = (point_five & (zeros | ones_digit)).asBool()

        (t >> u).asSInt() + Mux(r, 1.S, 0.S)
      }, 0, UInt(8.W), -1)),
    acc_read_full_width = true,
    acc_read_small_width = true,
    use_dedicated_tl_port = false,

    use_shared_ext_mem = true,
    tile_latency = 0,

    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true,

    hardcode_d_to_garbage_addr = false,

    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_mem_reqs = 16,

    mesh_output_delay = 1,

    has_training_convs = false,
    has_max_pool = true,
    has_nonlinear_activations = true,

    num_counter = 8,

    clock_gate = false,
  )
}

object DSEConfigs{
  import DSEBaseConfig.{baseConfig => base}
  val baseConfig = base.copy(headerFileName = "gemmini_params_dse1.h")
  val wsOnlyConfig = baseConfig.copy(dataflow = Dataflow.WS, headerFileName = "gemmini_params_dse2.h")
  val bothDataflowsConfig = baseConfig.copy(dataflow = Dataflow.BOTH, headerFileName = "gemmini_params_dse3.h")
  val highBitwidthConfig = baseConfig.copy(inputType = SInt(32.W), spatialArrayOutputType = SInt(32.W),
    headerFileName = "gemmini_params_dse4.h")
  val largerDimConfig = baseConfig.copy(meshRows = 32, meshColumns = 32, spatialArrayOutputType = SInt(20.W),
    headerFileName = "gemmini_params_dse5.h")
  val fullyCombinationalConfig = baseConfig.copy(tileRows = 16, tileColumns = 16, meshRows = 1, meshColumns = 1,
    headerFileName = "gemmini_params_dse6.h")
  val moreMemoryConfig = baseConfig.copy(sp_capacity = CapacityInKilobytes(256), acc_capacity = CapacityInKilobytes(64),
    headerFileName = "gemmini_params_dse7.h") // 256kB
  val moreBanksConfig = baseConfig.copy(sp_banks = 32, headerFileName = "gemmini_params_dse8.h")
  val narrowerBusConfig = baseConfig.copy(dma_maxbytes = 64, dma_buswidth = 64, headerFileName = "gemmini_params_dse10.h")
  val pnr16Config = baseConfig.copy(sp_capacity = CapacityInKilobytes(256), acc_capacity = CapacityInKilobytes(64),
    dataflow = Dataflow.BOTH, headerFileName = "gemmini_params_pnr16.h")
  val pnr32Config = baseConfig.copy(sp_capacity = CapacityInKilobytes(512), acc_capacity = CapacityInKilobytes(128),
    meshRows = 32, meshColumns = 32, spatialArrayOutputType = SInt(20.W), dataflow = Dataflow.BOTH,
    headerFileName = "gemmini_params_pnr32.h")
}

//===========BASELINE=========
class GemminiParamsDSE1 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.baseConfig))
    }
  )
})

//===========DATAFLOW CHANGE: WS=========
class GemminiParamsDSE2 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.wsOnlyConfig))
    }
  )
})

//===========DATAFLOW CHANGE: BOTH=========
class GemminiParamsDSE3 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.bothDataflowsConfig))
    }
  )
})

//===========BITWIDTH CHANGE: 32 BITS=========
class GemminiParamsDSE4 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.highBitwidthConfig))
    }
  )
})

//===========DIMENSIONS CHANGE: 32x32=========
class GemminiParamsDSE5 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.largerDimConfig))
    }
  )
})

//===========PIPELINE DEPTH CHANGE: Fully Combinational=========
class GemminiParamsDSE6 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.fullyCombinationalConfig))
    }
  )
})

//===========MEMORY CAPACITY CHANGE: 256 KB=========
class GemminiParamsDSE7 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.moreMemoryConfig))
    }
  )
})

//===========MEMORY BANKS CHANGE: 33 Banks=========
class GemminiParamsDSE8 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.moreBanksConfig))
    }
  )
})

//===========BUS WIDTH CHANGE: 64 bits=========
class GemminiParamsDSE10 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.narrowerBusConfig))
    }
  )
})

//===========PnR 16-by-16=========
class GemminiParamsPnR16 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      LazyModule(new Gemmini(DSEConfigs.pnr16Config))
    }
  )
})

//===========PnR 32-by-32=========
class GemminiParamsPnR32 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      LazyModule(new Gemmini(DSEConfigs.pnr32Config))
    }
  )
})

//===========Scalar Processor Change=========
class GemminiParamsDSE11 extends Config((site, here, up) => {
  case BuildRoCC => Seq(
      (p: Parameters) => {
        implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new Gemmini(DSEConfigs.baseConfig))
    }
  )
})

// -----------------------------
// Design Space Exploration Top Level Configs
// -----------------------------

class GemminiDSE1Config extends Config(new GemminiParamsDSE1 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE2Config extends Config(new GemminiParamsDSE2 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE3Config extends Config(new GemminiParamsDSE3 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE4Config extends Config(new GemminiParamsDSE4 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE5Config extends Config(new GemminiParamsDSE5 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE6Config extends Config(new GemminiParamsDSE6 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE7Config extends Config(new GemminiParamsDSE7 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE8Config extends Config(new GemminiParamsDSE8 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiDSE10Config extends Config(new GemminiParamsDSE10 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiPnr16Config extends Config(new GemminiParamsPnR16 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class GemminiPnr32Config extends Config(new GemminiParamsPnR32 ++
                                    new freechips.rocketchip.system.DefaultConfig)

/* class GemminiDSE11Config extends Config(new GemminiParamsDSE11 ++
                                    new boom.system.SmallBoomConfig)*/

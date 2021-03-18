
package gemmini

import chisel3._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet, XLen}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy._
import gemmini.Arithmetic.SIntArithmetic
import hardfloat._


// ------------------
// Multi-RoCC Support
// ------------------

/**
 * Map from a hartId to a particular RoCC accelerator
 */
case object MultiRoCCKey extends Field[Map[Int, Seq[Parameters => LazyRoCC]]](Map.empty[Int, Seq[Parameters => LazyRoCC]])

/**
 * Mixin to enable different RoCCs based on the hartId
 */
class WithMultiRoCC extends Config((site, here, up) => {
  case BuildRoCC => site(MultiRoCCKey).getOrElse(site(TileKey).hartId, Nil)
})


// -----------------------
// Component Mixin Configs
// -----------------------

object GemminiConfigs {
  val defaultConfig = GemminiArrayConfig[SInt, Float, Float](
    opcodes = OpcodeSet.custom3,

    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,

    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,

    rob_entries = 16,
    hasIm2col = false, //declare im2col block

    sp_banks = 4,
    sp_singleported = true,
    acc_banks = 2,
    acc_latency = 2,
    acc_singleported = false,
    num_acc_sub_banks = -1,

    sp_capacity = CapacityInKilobytes(256),
    shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
    dataflow = Dataflow.BOTH,
    acc_capacity = CapacityInKilobytes(64),
    mem_pipeline = 4,
    dma_maxbytes = 64, // TODO get this from cacheblockbytes
    dma_buswidth = 128, // TODO get this from SystemBusKey
    aligned_to = 1,
    tlb_size = 4,
    use_tlb_register_filter = true,
    max_in_flight_reqs = 16,
    use_dedicated_tl_port = false,
    use_shared_ext_mem = false,
    inputType = SInt(8.W),
    outputType = SInt(20.W),
    accType = SInt(32.W),

    mvin_scale_args = Some(ScaleArguments(
      (t: SInt, f: Float) => {
        val f_rec = recFNFromFN(f.expWidth, f.sigWidth, f.bits)

        val in_to_rec_fn = Module(new INToRecFN(t.getWidth, f.expWidth, f.sigWidth))
        in_to_rec_fn.io.signedIn := true.B
        in_to_rec_fn.io.in := t.asTypeOf(UInt(t.getWidth.W))
        in_to_rec_fn.io.roundingMode := consts.round_near_even
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val t_rec = in_to_rec_fn.io.out

        val muladder = Module(new MulAddRecFN(f.expWidth, f.sigWidth))
        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := t_rec
        muladder.io.b := f_rec
        muladder.io.c := 0.U

        val rec_fn_to_in = Module(new RecFNToIN(f.expWidth, f.sigWidth, t.getWidth))
        rec_fn_to_in.io.in := muladder.io.out
        rec_fn_to_in.io.roundingMode := consts.round_near_even
        rec_fn_to_in.io.signedOut := true.B

        val overflow = rec_fn_to_in.io.intExceptionFlags(1)
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        val sign = rawFloatFromRecFN(f.expWidth, f.sigWidth, rec_fn_to_in.io.in).sign
        val sat = Mux(sign, minsat, maxsat)

        Mux(overflow, sat, rec_fn_to_in.io.out.asTypeOf(t))
      },
      4, Float(8, 24), 4,
      identity = "1.0",
      c_str = "({float y = ROUND_NEAR_EVEN((x) * (scale)); y > INT8_MAX ? INT8_MAX : (y < INT8_MIN ? INT8_MIN : (elem_t)y);})"
    )),
    mvin_scale_acc_args = None,
    mvin_scale_shared = false,

    acc_scale_args = ScaleArguments(
      (t: SInt, f: Float) => {
        val f_rec = recFNFromFN(f.expWidth, f.sigWidth, f.bits)

        val in_to_rec_fn = Module(new INToRecFN(t.getWidth, f.expWidth, f.sigWidth))
        in_to_rec_fn.io.signedIn := true.B
        in_to_rec_fn.io.in := t.asTypeOf(UInt(t.getWidth.W))
        in_to_rec_fn.io.roundingMode := consts.round_near_even
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val t_rec = in_to_rec_fn.io.out

        val muladder = Module(new MulAddRecFN(f.expWidth, f.sigWidth))
        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := t_rec
        muladder.io.b := f_rec
        muladder.io.c := 0.U

        val rec_fn_to_in = Module(new RecFNToIN(f.expWidth, f.sigWidth, t.getWidth))
        rec_fn_to_in.io.in := muladder.io.out
        rec_fn_to_in.io.roundingMode := consts.round_near_even
        rec_fn_to_in.io.signedOut := true.B

        val overflow = rec_fn_to_in.io.intExceptionFlags(1)
        val maxsat = ((1 << (t.getWidth-1))-1).S
        val minsat = (-(1 << (t.getWidth-1))).S
        val sign = rawFloatFromRecFN(f.expWidth, f.sigWidth, rec_fn_to_in.io.in).sign
        val sat = Mux(sign, minsat, maxsat)

        Mux(overflow, sat, rec_fn_to_in.io.out.asTypeOf(t))
      },
      1, Float(8, 24), -1, // TODO pipelining should be 5
      identity = "1.0",
      c_str = "({float y = ROUND_NEAR_EVEN((x) * (scale)); y > INT8_MAX ? INT8_MAX : (y < INT8_MIN ? INT8_MIN : (acc_t)y);})"
    ),

    acc_read_full_width = true,
    acc_read_small_width = true,

    pe_latency = 0,

    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true
  )

  val chipConfig = defaultConfig.copy(sp_capacity=CapacityInKilobytes(64), acc_capacity=CapacityInKilobytes(32), dataflow=Dataflow.WS,
    acc_scale_args=defaultConfig.acc_scale_args.copy(latency=4),
    acc_singleported=true,
    num_acc_sub_banks=2,
    ex_read_from_acc=false,
    ex_write_to_spad=false
  )
  val largeChipConfig = chipConfig.copy(sp_capacity=CapacityInKilobytes(128), acc_capacity=CapacityInKilobytes(64),
    meshRows=32, meshColumns=32
  )

  val highPerfConfig = defaultConfig.copy(dataflow=Dataflow.WS, acc_read_full_width = false, ex_read_from_acc = false, ex_write_to_spad = false, max_in_flight_reqs = 64)
}

/**
 * Mixin which sets the default parameters for a systolic array accelerator.
   Also sets the system bus width to 128 bits (instead of the deafult 64 bits) to
   allow for the default 16x16 8-bit systolic array to be attached.
 */
class DefaultGemminiConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiConfigs.defaultConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


class DualGemminiConfig extends Config((site, here, up) => {
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
  case BuildRoCC => {
    var int_gemmini: Gemmini[_,_,_] = null
    var fp_gemmini: Gemmini[_,_,_] = null
    val int_fn = (p: Parameters) => {
      implicit val q = p
      int_gemmini = LazyModule(new Gemmini(GemminiConfigs.chipConfig.copy(
        opcodes = OpcodeSet.custom3,
        use_shared_ext_mem = true
      )))
      int_gemmini
    }
    val fp_fn = (p: Parameters) => {
      implicit val q = p
      fp_gemmini = LazyModule(new Gemmini(GemminiFPConfigs.BF16DefaultConfig.copy(
        opcodes = OpcodeSet.custom2,
        sp_capacity=CapacityInKilobytes(64), acc_capacity=CapacityInKilobytes(32),
        meshColumns = 8, meshRows = 8,
        acc_singleported = true, acc_banks = 2, num_acc_sub_banks = 2,
        use_shared_ext_mem = true,
        headerFileName = "gemmini_params_bf16.h",
	acc_latency = 3,
        dataflow = Dataflow.WS
      )))
      InModuleBody {
        require(int_gemmini.config.sp_banks == fp_gemmini.config.sp_banks)
        require(int_gemmini.config.acc_banks == fp_gemmini.config.acc_banks)
        require(int_gemmini.config.num_acc_sub_banks == fp_gemmini.config.num_acc_sub_banks)
        require(int_gemmini.config.sp_singleported && fp_gemmini.config.sp_singleported)
        require(int_gemmini.config.acc_singleported && fp_gemmini.config.acc_singleported)

        require(int_gemmini.config.sp_bank_entries == fp_gemmini.config.sp_bank_entries)
        require(int_gemmini.spad.module.spad_mems(0).mask_len == fp_gemmini.spad.module.spad_mems(0).mask_len)
        require(int_gemmini.spad.module.spad_mems(0).mask_elem.getWidth == fp_gemmini.spad.module.spad_mems(0).mask_elem.getWidth)

        println(int_gemmini.config.acc_bank_entries, fp_gemmini.config.acc_bank_entries)
        println(int_gemmini.spad.module.acc_mems(0).mask_len, fp_gemmini.spad.module.acc_mems(0).mask_len)
        println(int_gemmini.spad.module.acc_mems(0).mask_elem.getWidth, fp_gemmini.spad.module.acc_mems(0).mask_elem.getWidth)

        require(int_gemmini.config.acc_bank_entries == fp_gemmini.config.acc_bank_entries / 2)
        require(int_gemmini.config.num_acc_sub_banks == fp_gemmini.config.num_acc_sub_banks)
        require(int_gemmini.spad.module.acc_mems(0).mask_len == fp_gemmini.spad.module.acc_mems(0).mask_len * 2)
        require(int_gemmini.spad.module.acc_mems(0).mask_elem.getWidth == fp_gemmini.spad.module.acc_mems(0).mask_elem.getWidth)

        val spad_mask_len = int_gemmini.spad.module.spad_mems(0).mask_len
        val spad_data_len = int_gemmini.spad.module.spad_mems(0).mask_elem.getWidth
        val acc_mask_len = int_gemmini.spad.module.acc_mems(0).mask_len
        val acc_data_len = int_gemmini.spad.module.acc_mems(0).mask_elem.getWidth

        val shared_mem = Module(new SharedExtMem(
          int_gemmini.config.sp_banks, int_gemmini.config.acc_banks, int_gemmini.config.num_acc_sub_banks,
          int_gemmini.config.sp_bank_entries, spad_mask_len, spad_data_len,
          int_gemmini.config.acc_bank_entries / int_gemmini.config.num_acc_sub_banks, acc_mask_len, acc_data_len
        ))
        shared_mem.io.in(0) <> int_gemmini.module.ext_mem_io.get
        shared_mem.io.in(1) <> fp_gemmini.module.ext_mem_io.get
      }
      fp_gemmini
    }
    up(BuildRoCC) ++ Seq(int_fn, fp_fn)
  }
})

/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **replaces** the default host rocket (assuming a single core config).
   This is useful for hierarcical physical design purposes.
 */
class GemminiHostMiniCore extends Config((site, here, up) => {
  case RocketTilesKey => Seq(
    RocketTileParams(
      core = RocketCoreParams(
        useVM = true,
        mulDiv = Some(MulDivParams(mulUnroll = 8))),
      btb = Some(BTBParams(nEntries = 14, nRAS = 2)),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64, // 4Kb
        nWays = 4,  // 4Kb * 4
//        nTLBEntries = 16,
        nMSHRs = 0,
//        blockBytes = site(CacheBlockBytes)
        )),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 4,
//        nTLBEntries = 16,
//        blockBytes = site(CacheBlockBytes)
        )),
      hartId = up(RocketTilesKey, site).length - 1
        ))
  case MultiRoCCKey => up(MultiRoCCKey, site) +
    (up(RocketTilesKey, site).length - 1 ->
      Seq((p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(GemminiConfigs.defaultConfig))
        gemmini
      }))
})



/**
 * Mixin which configures a smaller host processor for the systolic array.
   This mixin **adds** the small core to the default host rocket (as the last hart).
   This is useful for software development purposes for the gemmini accelerator
   as a device with a control processor (rather than a rocc-attached accelerator)
 */
class WithGemminiHostMiniCore extends Config((site, here, up) => {
  case RocketTilesKey => up(RocketTilesKey, site) :+
    RocketTileParams(
      core = RocketCoreParams(
        useVM = true,
        mulDiv = Some(MulDivParams(mulUnroll = 8))),
      btb = Some(BTBParams(nEntries = 14, nRAS = 2)),
      dcache = Some(DCacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64, // 4Kb scratchpad
        nWays = 4,
//        nTLBEntries = 16,
        nMSHRs = 0,
        blockBytes = site(CacheBlockBytes))),
      icache = Some(ICacheParams(
        rowBits = site(SystemBusKey).beatBits,
        nSets = 64,
        nWays = 4,
//        nTLBEntries = 16,
        blockBytes = site(CacheBlockBytes))),
      hartId = up(RocketTilesKey, site).length
        )
  case MultiRoCCKey => up(MultiRoCCKey, site) +
    (up(RocketTilesKey, site).length ->
      Seq((p: Parameters) => {
        implicit val q = p
        val gemmini = LazyModule(new Gemmini(GemminiConfigs.defaultConfig))
        gemmini
      }))
})



// -----------------------------
// Example TopLevel Configs
// -----------------------------


/**
 * Top level config with a default single core rocket and gemmini rocc accelerator.
   Useful for gemmini performance evaluation and debugging.
 */
class GemminiConfig extends Config(new DefaultGemminiConfig ++
                                    new freechips.rocketchip.system.DefaultConfig)


/**
 * Top level config with a small host rocket and gemmini rocc accelerator.
   Useful for physical design.
 */
class GemminiAcceleratorConfig extends Config(
  new GemminiHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultGemminiConfig ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)


/**
 * Top level config with a default single core rocket,
   and a small mini-core rocket attached to a gemmini rocc accelerator.
   Useful for device driver development
 */
class GemminiAcceleratorDeviceConfig extends Config(
  new WithGemminiHostMiniCore ++
  new WithMultiRoCC ++
  new DefaultGemminiConfig ++
  new WithMultiRoCC ++
  new WithoutTLMonitors ++
  new freechips.rocketchip.system.DefaultConfig
)

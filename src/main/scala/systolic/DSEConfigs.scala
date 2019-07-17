package systolic

import chisel3._
import freechips.rocketchip.config.{Field, Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet, XLen}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.system._


// -----------------------------
// Design Space Exploration Mixins
// -----------------------------


//===========BASELINE==================

class SystolicParamsDSE1 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})

//===========DATAFLOW CHANGE: WS=========

class SystolicParamsDSE2 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.WS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


//===========DATAFLOW CHANGE: BOTH=========

class SystolicParamsDSE3 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.BOTH,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


//===========BITWIDTH CHANGE: 32 BITS=========

class SystolicParamsDSE4 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 32), // has to be a multiply of meshRows*tileRows
      sp_width = 32 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(32.W), SInt(32.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})



//===========DIMENSIONS CHANGE: 32x32=========

class SystolicParamsDSE5 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 32,
      meshColumns = 32,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 32 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 32, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (32 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


//===========PIPELINE DEPTH CHANGE: Fully Combinational=========

class SystolicParamsDSE6 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 16,
      tileColumns = 16,
      meshRows = 1,
      meshColumns = 1,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


//===========MEMORY CAPACITY CHANGE: 256 KB=========

class SystolicParamsDSE7 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 256 * 1024 * 8 / (4 * 16*8), // has to be a multiply of meshRows*tileRows
      sp_width = 16 * 8, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      acc_rows = 64 * 1024 * 8 / (16*32),
      shifter_banks = 16,
      depq_len = 65536,
      dataflow = Dataflow.OS
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})


//===========MEMORY BANKS CHANGE: 33 Banks=========

class SystolicParamsDSE8 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 32,
      sp_bank_entries = 64 * 1024 * 8 / (32 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 16)
})



//===========BUS WIDTH CHANGE: 64 bits=========
/*
class SystolicParamsDSE10 extends Config((site, here, up) => {
  case SystolicArrayKey =>
    SystolicArrayConfig(
      tileRows = 1,
      tileColumns = 1,
      meshRows = 16,
      meshColumns = 16,
      ld_str_queue_length = 10,
      ex_queue_length = 10,
      sp_banks = 4,
      sp_bank_entries = 64 * 1024 * 8 / (4 * 16 * 8), // has to be a multiply of meshRows*tileRows
      sp_width = 8 * 16, // has to be meshRows*tileRows*dataWidth // TODO should this be changeable?
      shifter_banks = 1, // TODO add separate parameters for left and up shifter banks
      depq_len = 65536,
      dataflow = Dataflow.OS,
      acc_rows = 16 * 1024 * 8 / (16 * 32),
      mem_pipeline = 1
    )
  case BuildRoCC => Seq(
      (p: Parameters) => {
         implicit val q = p
        implicit val v = implicitly[ValName]
        LazyModule(new SystolicArray(SInt(8.W), SInt(16.W), SInt(32.W), OpcodeSet.custom3))
    }
  )
  case SystemBusKey => up(SystemBusKey).copy(beatBytes = 8)
})
*/


// -----------------------------
// Design Space Exploration Top Level Configs
// -----------------------------

class SystolicDSE1Config extends Config(new SystolicParamsDSE1 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE2Config extends Config(new SystolicParamsDSE2 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE3Config extends Config(new SystolicParamsDSE3 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE4Config extends Config(new SystolicParamsDSE4 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE5Config extends Config(new SystolicParamsDSE5 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE6Config extends Config(new SystolicParamsDSE6 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE7Config extends Config(new SystolicParamsDSE7 ++
                                    new freechips.rocketchip.system.DefaultConfig)

class SystolicDSE8Config extends Config(new SystolicParamsDSE8 ++
                                    new freechips.rocketchip.system.DefaultConfig)
/*
class SystolicDSE10Config extends Config(new SystolicParamsDSE10 ++
                                    new freechips.rocketchip.system.DefaultConfig)
*/

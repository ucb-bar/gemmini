
package gemmini

import scala.math.{pow,sqrt}
import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._

sealed abstract trait GemminiMemCapacity
case class CapacityInKilobytes(kilobytes: Int) extends GemminiMemCapacity
case class CapacityInMatrices(matrices: Int) extends GemminiMemCapacity

case class ScaleArguments[T <: Data, U <: Data](scale_func: (T, U) => T, latency: Int, multiplicand_t: U,
                                                num_scale_units: Int,
                                                identity: String="0", c_str: String="ROUNDING_RIGHT_SHIFT(x, scale)")

case class GemminiArrayConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
                                                                             opcodes: OpcodeSet = OpcodeSet.custom3,

                                                                             inputType: T,
                                                                             spatialArrayOutputType: T,
                                                                             accType: T,

                                                                             dataflow: Dataflow.Value = Dataflow.BOTH,

                                                                             tileRows: Int = 1,
                                                                             tileColumns: Int = 1,
                                                                             meshRows: Int = 16,
                                                                             meshColumns: Int = 16,

                                                                             ld_queue_length: Int = 8,
                                                                             st_queue_length: Int = 2,
                                                                             ex_queue_length: Int = 8,

                                                                             reservation_station_full_entries: Int = 16,
                                                                             reservation_station_partial_entries: Int = 8,

                                                                             sp_banks: Int = 4, // TODO support one-bank designs
                                                                             sp_singleported: Boolean = false,
                                                                             sp_capacity: GemminiMemCapacity = CapacityInKilobytes(256),
                                                                             spad_read_delay: Int = 4,

                                                                             acc_banks: Int = 2,
                                                                             acc_singleported: Boolean = false,
                                                                             acc_sub_banks: Int = -1,
                                                                             acc_capacity: GemminiMemCapacity = CapacityInKilobytes(64),
                                                                             acc_latency: Int = 2,

                                                                             dma_maxbytes: Int = 64, // TODO get this from cacheblockbytes
                                                                             dma_buswidth: Int = 128, // TODO get this from SystemBusKey

                                                                             shifter_banks: Int = 1, // TODO add separate parameters for left and up shifter banks

                                                                             aligned_to: Int = 1, // TODO we should align to inputType and accType instead

                                                                             mvin_scale_args: Option[ScaleArguments[T, U]] = None,
                                                                             mvin_scale_acc_args: Option[ScaleArguments[T, U]] = None,
                                                                             mvin_scale_shared: Boolean = false,
                                                                             acc_scale_args: Option[ScaleArguments[T, V]] = None,

                                                                             acc_read_full_width: Boolean = true,
                                                                             acc_read_small_width: Boolean = true,
                                                                             use_dedicated_tl_port: Boolean = true,

                                                                             tlb_size: Int = 4,
                                                                             use_tlb_register_filter: Boolean = true,
                                                                             max_in_flight_mem_reqs: Int = 16,

                                                                             ex_read_from_spad: Boolean = true,
                                                                             ex_read_from_acc: Boolean = true,
                                                                             ex_write_to_spad: Boolean = true,
                                                                             ex_write_to_acc: Boolean = true,

                                                                             hardcode_d_to_garbage_addr: Boolean = false,
                                                                             use_shared_tlb: Boolean = true,

                                                                             tile_latency: Int = 0,
                                                                             mesh_output_delay: Int = 1,

                                                                             use_tree_reduction_if_possible: Boolean = true,

                                                                             num_counter: Int = 8,

                                                                             has_training_convs: Boolean = true,
                                                                             has_max_pool: Boolean = true,
                                                                             has_nonlinear_activations: Boolean = true,

                                                                             has_first_layer_optimizations: Boolean = true,

                                                                             use_firesim_simulation_counters: Boolean = false,

                                                                             use_shared_ext_mem: Boolean = false,
                                                                             clock_gate: Boolean = false,

                                                                             headerFileName: String = "gemmini_params.h"
                                                       ) {
  val sp_width = meshColumns * tileColumns * inputType.getWidth
  val sp_bank_entries = sp_capacity match {
    case CapacityInKilobytes(kb) => kb * 1024 * 8 / (sp_banks * sp_width)
    case CapacityInMatrices(ms) => ms * meshRows * tileRows / sp_banks
  }
  val acc_bank_entries = acc_capacity match {
    case CapacityInKilobytes(kb) => kb * 1024 * 8 / (acc_banks * meshColumns * tileColumns * accType.getWidth)
    case CapacityInMatrices(ms) => ms * meshRows * tileRows / acc_banks
  }
  require (!acc_singleported || (acc_sub_banks <= 4 && isPow2(acc_sub_banks)))

  val local_addr_t = new LocalAddr(sp_banks, sp_bank_entries, acc_banks, acc_bank_entries)

  val mvin_scale_t = mvin_scale_args match {
    case Some(ScaleArguments(_, _, t, _, _, _)) => t
    case None => Bool() // TODO replace this with UInt(0.W)
  }

  val mvin_scale_acc_t = mvin_scale_acc_args match {
    case Some(ScaleArguments(_, _, t, _, _, _)) => t
    case None => Bool() // TODO replace this with UInt(0.W)
  }

  val mvin_scale_t_bits = mvin_scale_t.getWidth max mvin_scale_acc_t.getWidth
  val mvin_scale_same = (mvin_scale_args.isEmpty && mvin_scale_acc_args.isEmpty) || mvin_scale_shared

  // If the user doesn't specify an "acc_scale_args", then for now, we will still say in the header file that
  // acc_scale_t is Float32. TODO: don't put an acc_scale_t in the header file at all if the user doesn't specify one
  val acc_scale_t = acc_scale_args match {
    case Some(args) => args.multiplicand_t
    case None => Float(8, 24)
  }

  val acc_scale_t_bits = acc_scale_t.getWidth

  val acc_scale_identity = acc_scale_args match {
    case Some(args) => args.identity
    case None => "0"
  }

  val acc_scale_c_str = acc_scale_args match {
    case Some(args) => args.c_str
    case None => "(x)"
  }

  val acc_scale_func = acc_scale_args match {
    case Some(args) => args.scale_func
    case None => (t: T, _: V) => t
  }

  val acc_scale_num_units = acc_scale_args match {
    case Some(args) => args.num_scale_units
    case None => -1
  }

  val acc_scale_latency = acc_scale_args match {
    case Some(args) => args.latency
    case None => 1
  }
  assert(acc_scale_latency > 0)

  val mvin_cols_bits = log2Up(((dma_maxbytes / (inputType.getWidth / 8)) max (meshColumns * tileColumns)) + 1)
  val mvin_rows_bits = log2Up(meshRows * tileRows + 1)
  val mvout_cols_bits = log2Up(((dma_maxbytes / (inputType.getWidth / 8)) max (meshColumns * tileColumns)) + 1)
  val mvout_rows_bits = log2Up(meshRows * tileRows + 1)

  val load_states = 3
  val block_stride_bits = 16 min (log2Up(acc_banks * acc_bank_entries) max log2Up(sp_banks * sp_bank_entries))

  val a_stride_bits = 16 min (log2Up(acc_banks * acc_bank_entries) max log2Up(sp_banks * sp_bank_entries))
  val c_stride_bits = 16 min (log2Up(acc_banks * acc_bank_entries) max log2Up(sp_banks * sp_bank_entries))

  val pixel_repeats_bits = 8 min log2Up(meshColumns * tileColumns + 1)

  val hasIm2Col = false

  val tree_reduction = use_tree_reduction_if_possible && dataflow == Dataflow.WS && tileRows > 1

  //==========================================================================
  // sanity check mesh size
  //==========================================================================
  val BLOCK_ROWS = tileRows * meshRows
  val BLOCK_COLS = tileColumns * meshColumns
  require(BLOCK_ROWS == BLOCK_COLS, "BLOCK_ROWS != BLOCK_COLS!")

  val DIM             = BLOCK_ROWS
  val LOG2_DIM        = log2Up(DIM)
  val LOG2_DIM_COUNT  = log2Up(DIM + 1)
  require(DIM >= 2, "the systolic array must have DIM of at least 2")

  //==========================================================================
  // cisc-gemmini miscellaneous constants (some redundant with above)
  //==========================================================================
  val rob_entries      = reservation_station_full_entries + reservation_station_partial_entries
  val ROB_ENTRIES      = rob_entries
  val LOG2_ROB_ENTRIES = log2Up(rob_entries)

  //==========================================================================
  // cisc-gemmini hardware-specific compile-time global constants
  //==========================================================================

  val cisc_dim = (meshRows * tileRows) / 2

  val ITYPE_BITS       = inputType.getWidth
  val ITYPE_BYTES      = (inputType.getWidth+cisc_dim-1) / cisc_dim
  val LOG2_ITYPE_BYTES = if(ITYPE_BYTES <= 1) 0 else log2Up(ITYPE_BYTES)

  val OTYPE_BITS       = accType.getWidth
  val LOG2_OTYPE_BITS  = log2Up(OTYPE_BITS)
  val OTYPE_BYTES      = (accType.getWidth+cisc_dim-1) / cisc_dim
  val LOG2_OTYPE_BYTES = if(OTYPE_BYTES <= 1) 0 else log2Up(OTYPE_BYTES)

  val SP_BANKS        = sp_banks
  val SP_BANK_ROWS    = sp_bank_entries
  val SP_ROWS         = SP_BANKS * SP_BANK_ROWS
  val LOG2_SP_ROWS    = log2Up(SP_ROWS)

  val ACC_BANKS       = acc_banks
  val ACC_BANK_ROWS   = acc_bank_entries
  val ACC_ROWS        = ACC_BANKS * ACC_BANK_ROWS
  val LOG2_ACC_ROWS   = log2Up(ACC_ROWS)

  val MNK_BYTES                   = Int.MaxValue / DIM  // TODO: upper bound?
  val LOG2_MNK_BYTES              = log2Up(MNK_BYTES)
  val MNK_BYTES_PER_TILE_ROW      = MNK_BYTES * DIM
  val LOG2_MNK_BYTES_PER_TILE_ROW = log2Up(MNK_BYTES_PER_TILE_ROW)
  val TILE_IDX                    = MNK_BYTES / (DIM / cisc_dim)
  val LOG2_TILE_IDX               = log2Up(TILE_IDX)

  //--------------------------------------------------------------------------
  val I_TILE_BYTE_WIDTH = DIM * ((inputType.getWidth+cisc_dim-1) / cisc_dim)
  val O_TILE_BYTE_WIDTH = DIM * ((accType.getWidth+cisc_dim-1) / cisc_dim)
  val I_TILE_BYTE_WIDTH_LOG2 = log2Up(I_TILE_BYTE_WIDTH)
  val O_TILE_BYTE_WIDTH_LOG2 = log2Up(O_TILE_BYTE_WIDTH)
  require(pow(2,I_TILE_BYTE_WIDTH_LOG2) == I_TILE_BYTE_WIDTH,
    s"I_TILE_BYTE_WIDTH is not power of 2: $I_TILE_BYTE_WIDTH")
  require(pow(2,O_TILE_BYTE_WIDTH_LOG2) == O_TILE_BYTE_WIDTH,
    s"O_TILE_BYTE_WIDTH is not power of 2: $O_TILE_BYTE_WIDTH")

  val GBL_B_SP_ROW_ADDR_1 = (SP_BANKS * SP_BANK_ROWS) - 2*DIM
  val GBL_B_SP_ROW_ADDR_2 = (SP_BANKS * SP_BANK_ROWS) - 1*DIM

  val USABLE_SP_TILES = (SP_ROWS / DIM) - 2
  val TOTAL_ACC_TILES = (ACC_ROWS / DIM)
  val SQRT_ACC_TILES = sqrt(TOTAL_ACC_TILES).toInt
  assert(USABLE_SP_TILES >= TOTAL_ACC_TILES, 
    s"SP_TILES($USABLE_SP_TILES) + 2 < ACC_TILES($TOTAL_ACC_TILES)")

  // prioritize sizes that cause the output-group to be further from square
  val OG_HEIGHT_MAP = (1 to TOTAL_ACC_TILES).sortWith((h1, h2) => {
    (h1 - SQRT_ACC_TILES).abs > (h2 - SQRT_ACC_TILES).abs
  })

  val BYTE_ROWS_PER_TILE = DIM

  //==========================================================================
  // other stuff
  //==========================================================================

  require(num_counter < 256, "Number of counters cannot exceed 256")

  require(isPow2(sp_bank_entries), "each SRAM bank must have a power-of-2 rows, to simplify address calculations") // TODO remove this requirement
  require(sp_bank_entries % (meshRows * tileRows) == 0, "the number of rows in a bank must be a multiple of the dimensions of the systolic array")
  require(meshColumns * tileColumns == meshRows * tileRows, "the systolic array must be square") // TODO remove this requirement
  require(meshColumns * tileColumns >= 2, "the systolic array must have a dimension of at least 2") // TODO remove this requirement
  require(isPow2(meshColumns * tileColumns), "the systolic array's dimensions must be powers of 2") // TODO remove this requirement
  require(acc_bank_entries % (meshRows * tileRows) == 0, "the number of rows in an accumulator bank must be a multiple of the dimensions of the systolic array")
  require(!mvin_scale_shared || (mvin_scale_shared && mvin_scale_args.isDefined && mvin_scale_acc_args.isEmpty && inputType.getWidth == accType.getWidth)) // TODO is there a better way to check whether inputType and accType are the same?
  require((mvin_scale_args.isEmpty || mvin_scale_acc_args.isEmpty) || (mvin_scale_t.getWidth == mvin_scale_acc_t.getWidth), "currently, the mvin scale types for both the srams and the accumulator must have the same width") // TODO remove this requirement

  def generateHeader(guard: String = "GEMMINI_PARAMS_H"): String = {
    // Returns the (min,max) values for a dataType
    def limitsOfDataType(dataType: Data): (String, String) = {
      assert(dataType.getWidth <= 32) // Above 32 bits, we need to append UL to the number, which isn't done yet

      dataType match {
        case dt: SInt => ("-" + BigInt(2).pow(dt.getWidth - 1).toString, BigInt(2).pow(dt.getWidth - 1).-(1).toString)
        case dt: Float =>
          (dt.expWidth, dt.sigWidth) match {
            case (8, 24) => (scala.Float.MinValue.toString, scala.Float.MaxValue.toString)
            case (11, 53) => (scala.Double.MinValue.toString, scala.Double.MaxValue.toString)
            case (e, s) => (((Range(-1,-(s),-1).map(-Math.pow(2, _)).foldLeft(-1.0)(_ + _)) * Math.pow(2, Math.pow(2, e - 1) - 1)).toString, ((Range(-1,-(s),-1).map(Math.pow(2, _)).foldLeft(1.0)(_ + _)) * Math.pow(2, Math.pow(2, e - 1) - 1)).toString)
          }
        case dt => ("0", BigInt(2).pow(dt.getWidth).-(1).toString)
        // case _ => throw new IllegalArgumentException(s"Data type $dataType is unknown")
      }
    }

    def c_type(dataType: Data): String = {
      dataType match {
        case dt: SInt => s"int${dt.getWidth}_t"
        case dt: Float =>
          (dt.expWidth, dt.sigWidth) match {
            case (8, 24) => "float"
            case (11, 53) => "double"
            case (e, s) => s"uint" + (Math.pow(2, Math.ceil(Math.log(e + s)/Math.log(2.0)))).toInt.toString + s"_t"
          }
        case dt => s"uint${dt.getWidth}_t"
      }
    }

    def full_c_type(dataType: Data): String = {
      dataType match {
        case _: UInt => "uint64_t"
        case _: SInt => "int64_t"
        case _: Float => "double"
        case _ => "uint64_t"
        // case _ => throw new IllegalArgumentException(s"Data type $dataType is unknown")
      }
    }

    assert(tileColumns*meshColumns == tileRows*meshRows)
    assert(Set(8, 16, 32, 64).contains(inputType.getWidth))
    // assert(Set(8, 16, 32, 64).contains(outputType.getWidth))
    assert(Set(8, 16, 32, 64).contains(accType.getWidth))

    val header = new StringBuilder()
    header ++= s"#ifndef $guard\n"
    header ++= s"#define $guard\n\n"

    header ++= s"#include <stdint.h>\n"
    header ++= s"#include <limits.h>\n\n"

    val opcodeid = Seq(
      OpcodeSet.custom0, OpcodeSet.custom1, OpcodeSet.custom2, OpcodeSet.custom3
    ).indexWhere(o => o.opcodes(0).litValue == opcodes.opcodes(0).litValue)
    println(opcodeid, opcodes.opcodes)
    require (opcodeid != -1 && opcodes.opcodes.size == 1)
    header ++= s"#define XCUSTOM_ACC $opcodeid\n"

    header ++= s"#define DIM ${tileColumns*meshColumns}\n"
    header ++= s"#define ADDR_LEN 32\n"
    header ++= s"#define BANK_NUM $sp_banks\n"
    header ++= s"#define BANK_ROWS $sp_bank_entries\n"
    header ++= s"#define ACC_ROWS ${acc_banks * acc_bank_entries}\n" // TODO add ACC_BANKS as well

    val max_bytes = 64
    header ++= s"#define MAX_BYTES $max_bytes\n"

    if (tileColumns*meshColumns*inputType.getWidth/8 <= max_bytes) {
      header ++= s"#define MAX_BLOCK_LEN (MAX_BYTES/(DIM*${inputType.getWidth/8}))\n"
    } else {
      header ++= s"#define MAX_BLOCK_LEN 1\n"
    }

    if (tileColumns*meshColumns*accType.getWidth/8 <= max_bytes) {
      header ++= s"#define MAX_BLOCK_LEN_ACC (MAX_BYTES/(DIM*${accType.getWidth / 8}))\n\n"
    } else {
      header ++= s"#define MAX_BLOCK_LEN_ACC 1\n\n"
    }

    // Datatype of the systolic array
    val limits = limitsOfDataType(inputType)
    header ++= s"typedef ${c_type(inputType)} elem_t;\n"
    if (inputType.isInstanceOf[Float] && !((inputType.asInstanceOf[Float].expWidth, inputType.asInstanceOf[Float].sigWidth) == (8, 24) || (inputType.asInstanceOf[Float].expWidth, inputType.asInstanceOf[Float].sigWidth) == (11, 53)))
    {
      header ++= "#define ELEM_T_IS_LOWPREC_FLOAT\n"
      header ++= s"static const float elem_t_max = ${limits._2};\n"
      header ++= s"static const float elem_t_min = ${limits._1};\n"
    } else {
      header ++= s"static const elem_t elem_t_max = ${limits._2};\n"
      header ++= s"static const elem_t elem_t_min = ${limits._1};\n"
    }
    header ++= s"typedef ${c_type(accType)} acc_t;\n"
    header ++= s"typedef ${full_c_type(inputType)} full_t;\n\n"

    if (inputType.isInstanceOf[Float]) {
      header ++= "#define ELEM_T_IS_FLOAT\n"
      header ++= s"#define ELEM_T_EXP_BITS ${inputType.asInstanceOf[Float].expWidth}\n"
      header ++= s"#define ELEM_T_SIG_BITS ${inputType.asInstanceOf[Float].sigWidth}\n"
      header ++= s"#define ACC_T_EXP_BITS ${accType.asInstanceOf[Float].expWidth}\n"
      header ++= s"#define ACC_T_SIG_BITS ${accType.asInstanceOf[Float].sigWidth}\n"
      header ++= s"typedef ${c_type(UInt(inputType.getWidth.W))} elem_t_bits;\n"
      header ++= s"typedef ${c_type(UInt(accType.getWidth.W))} acc_t_bits;\n\n"
    }

    if (mvin_scale_args.isDefined) {
      header ++= "#define HAS_MVIN_SCALE\n"
      header ++= s"typedef ${c_type(mvin_scale_args.get.multiplicand_t)} scale_t;\n"
      header ++= s"typedef ${c_type(UInt(mvin_scale_args.get.multiplicand_t.getWidth.W))} scale_t_bits;\n\n"
    } else {
      header ++= s"typedef int32_t scale_t;\n"
      header ++= s"typedef uint32_t scale_t_bits;\n\n"
    }

    if (mvin_scale_acc_args.isDefined) {
      header ++= "#define HAS_MVIN_ACC_SCALE\n"
      header ++= s"typedef ${c_type(mvin_scale_acc_args.get.multiplicand_t)} scale_acc_t;\n"
      header ++= s"typedef ${c_type(UInt(mvin_scale_acc_args.get.multiplicand_t.getWidth.W))} scale_acc_t_bits;\n\n"
    } else {
      header ++= s"typedef int32_t scale_acc_t;\n"
      header ++= s"typedef uint32_t scale_acc_t_bits;\n\n"
    }

    header ++= s"typedef ${c_type(acc_scale_t)} acc_scale_t;\n"
    header ++= s"typedef ${c_type(UInt(acc_scale_t_bits.W))} acc_scale_t_bits;\n\n"

    header ++= s"#define row_align(blocks) __attribute__((aligned(blocks*DIM*sizeof(elem_t))))\n"
    header ++= s"#define row_align_acc(blocks) __attribute__((aligned(blocks*DIM*sizeof(acc_t))))\n\n"

    val mvin_scale_identity = mvin_scale_args match {
      case Some(ScaleArguments(_, _, _, _, identity, _)) => identity
      case None => "0"
    }
    header ++= s"#define MVIN_SCALE_IDENTITY $mvin_scale_identity\n\n"
    header ++= s"#define ACC_SCALE_IDENTITY ${acc_scale_identity}\n\n"

    if (inputType.isInstanceOf[Float]) {
      header ++= """#define ROUNDING_RIGHT_SHIFT(x, shift) \
    ((x) / (1 << (shift)))"""
      header ++= "\n\n"
    } else {
      header ++= """// Rounding right shift equation: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm
#define ROUNDING_RIGHT_SHIFT(x, shift) \
    ((shift) > 0 ? (((x) >> (shift)) + \
        (((shift) == 0 ? 0 : (((x) >> ((shift)-1)) & 1)) & \
             ((((shift) <= 1 ? 0 : ((x) & ((1 << ((shift)-1)) - 1))) != 0) | (((x) >> (shift)) & 1)))) : ((x) << (-(shift))))"""
      header ++= "\n\n"
    }

    header ++= """#ifdef __cplusplus
                 |#define SAME_TYPE(x) decltype(x)
                 |#else
                 |#define SAME_TYPE(x) typeof(x)
                 |#endif
                 |
                 |#define ROUND_NEAR_EVEN(x) \
                 |    ({ const SAME_TYPE(x) x_ = (x); \
                 |         const long long i = x_; \
                 |         const long long next = x_ < 0 ? x_ - 1 : x_ + 1; \
                 |         SAME_TYPE(x) rem = x_ - i; \
                 |         rem = rem < 0 ? -rem : rem; \
                 |         SAME_TYPE(x) result = rem < 0.5 ? i : (rem > 0.5 ? next : ( \
                 |                     i % 2 == 0 ? i : next)); \
                 |         result; })
                 |""".stripMargin
    header ++= "\n"

    header ++= """// Rounding right shift equation: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm
#define ROUNDING_RIGHT_SHIFT_BITS(x, shift) \
((shift) > 0 ? (((x) >> (shift)) + \
    (((shift) == 0 ? 0 : (((x) >> ((shift)-1)) & 1)) & \
         ((((shift) <= 1 ? 0 : ((x) & ((1 << ((shift)-1)) - 1))) != 0) | (((x) >> (shift)) & 1)))) : ((x) << (-(shift))))"""
    header ++= "\n\n"

    header ++= """#define ACC_SCALE(x, scale) \
"""
    header ++= s"    ${acc_scale_c_str}"
    header ++= "\n\n"

    if (mvin_scale_args.isDefined) {
      header ++=
        s"""#define MVIN_SCALE(x, scale) \\
    ${mvin_scale_args.get.c_str}"""
      header ++= "\n\n"
    } else {
      header ++=
        s"""#define MVIN_SCALE(x, scale) (x)"""
      header ++= "\n\n"
    }

    if (mvin_scale_acc_args.isDefined) {
      header ++=
        s"""#define MVIN_SCALE_ACC(x, scale) \\
    ${mvin_scale_acc_args.get.c_str}"""
      header ++= "\n\n"
    } else {
      header ++=
        s"""#define MVIN_SCALE_ACC(x, scale) (x)"""
      header ++= "\n\n"
    }

    if (acc_scale_t.isInstanceOf[Float]) {
      header ++= "#define ACC_SCALE_T_IS_FLOAT\n"
      header ++= s"#define ACC_SCALE_EXP_BITS ${acc_scale_t.asInstanceOf[Float].expWidth}\n"
      header ++= s"#define ACC_SCALE_SIG_BITS ${acc_scale_t.asInstanceOf[Float].sigWidth}\n\n"
    }

    if (acc_read_small_width)
      header ++= s"#define ACC_READ_SMALL_WIDTH\n"
    if (acc_read_full_width)
      header ++= s"#define ACC_READ_FULL_WIDTH\n"
    header ++= s"\n"

    if (has_first_layer_optimizations) {
      header ++= "#define HAS_FIRST_LAYER_OPTIMIZATIONS\n\n"
    }

    header ++= s"#endif // $guard\n"
    header.toString()
  }

  def headerFilePath: String = {
    val chipyard_directory = "./generators/gemmini/software/gemmini-rocc-tests/include"
    val firesim_directory = "../target-design/chipyard/generators/gemmini/software/gemmini-rocc-tests/include"
    val default_directory = "."

    val in_chipyard = {
      val dir = new java.io.File(chipyard_directory)
      dir.exists() && dir.isDirectory
    }

    val in_firesim = {
      val dir = new java.io.File(firesim_directory)
      dir.exists() && dir.isDirectory
    }

    if (in_chipyard) {
      s"$chipyard_directory/$headerFileName"
    } else if (in_firesim) {
      s"$firesim_directory/$headerFileName"
    } else {
      s"$default_directory/$headerFileName"
    }
  }
}

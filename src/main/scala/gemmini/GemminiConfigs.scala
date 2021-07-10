
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
                                                                             opcodes: OpcodeSet,
                                                                             tileRows: Int,
                                                                             tileColumns: Int,
                                                                             meshRows: Int,
                                                                             meshColumns: Int,
                                                                             ld_queue_length: Int,
                                                                             st_queue_length: Int,
                                                                             ex_queue_length: Int,
                                                                             rob_full_entries: Int,
                                                                             rob_partial_entries: Int,
                                                                             sp_banks: Int, // TODO support one-bank designs
                                                                             sp_singleported: Boolean,
                                                                             sp_capacity: GemminiMemCapacity,
                                                                             acc_banks: Int,
                                                                             acc_singleported: Boolean,
                                                                             num_acc_sub_banks: Int,
                                                                             acc_capacity: GemminiMemCapacity,
                                                                             shifter_banks: Int,
                                                                             dataflow: Dataflow.Value,
                                                                             mem_pipeline: Int,
                                                                             dma_maxbytes: Int,
                                                                             dma_buswidth: Int,
                                                                             aligned_to: Int, // TODO we should align to inputType and accType instead
                                                                             inputType: T,
                                                                             outputType: T,
                                                                             accType: T,
                                                                             mvin_scale_args: Option[ScaleArguments[T, U]],
                                                                             mvin_scale_acc_args: Option[ScaleArguments[T, U]],
                                                                             mvin_scale_shared: Boolean,
                                                                             acc_scale_args: ScaleArguments[T, V],
                                                                             hasIm2col: Boolean,
                                                                             pe_latency: Int,
                                                                             acc_read_full_width: Boolean,
                                                                             acc_read_small_width: Boolean,
                                                                             use_dedicated_tl_port: Boolean,
                                                                             // enable_a_transpose: Boolean,
                                                                             // enable_b_transpose: Boolean,

                                                                             tlb_size: Int,
                                                                             use_tlb_register_filter: Boolean,
                                                                             max_in_flight_reqs: Int,

                                                                             ex_read_from_spad: Boolean,
                                                                             ex_read_from_acc: Boolean,
                                                                             ex_write_to_spad: Boolean,
                                                                             ex_write_to_acc: Boolean,

                                                                             hardcode_d_to_garbage_addr: Boolean,

                                                                             mesh_output_delay: Int,

                                                                             ld_ooo: Boolean,
                                                                             ex_ooo: Boolean,
                                                                             st_ooo: Boolean,

                                                                             use_preload_filter: Boolean,

                                                                             prng_seed: Int = 1, // ALON: You can change the PRNG seed here
                                                                             proportion_of_slow_accesses_out_of_128: Int = 10, // ALON: The number of memory accesses (out of 128) that are slow. You can also make this 0
                                                                             stall_delay: Int = 1000, // ALON: How many cycles should we wait for a slow memory access? You can also make this 0
                                                                             delay_lds: Boolean = false, // ALON: Should loads be stalled?
                                                                             delay_sts: Boolean = false, // ALON: Should stores be stalled?

                                                                             ex_total_k_portions: Int = 2, // ALON: You can change this to any number of k-portions that you would like
                                                                             ex_fine_grained_interleaving: Boolean = true, // ALON: If this is true, then we use the newer ("finer") intervleaving strategy

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
  require (!acc_singleported || (num_acc_sub_banks <= 4 && isPow2(num_acc_sub_banks)))

  val local_addr_t = new LocalAddr(sp_banks, sp_bank_entries, acc_banks, acc_bank_entries)

  val mvin_scale_t = mvin_scale_args match {
    case Some(ScaleArguments(_, _, t, _, _, _)) => t
    case None => Bool() // TODO replace this with UInt(0.W)
  }

  val mvin_scale_acc_t = mvin_scale_acc_args match {
    case Some(ScaleArguments(_, _, t, _, _, _)) => t
    case None => Bool() // TODO replace this with UInt(0.W)
  }

  val acc_scale_t = acc_scale_args.multiplicand_t

  val mvin_scale_t_bits = mvin_scale_t.getWidth max mvin_scale_acc_t.getWidth
  val mvin_scale_same = (mvin_scale_args.isEmpty && mvin_scale_acc_args.isEmpty) || mvin_scale_shared

  val acc_scale_t_bits = acc_scale_t.getWidth

  val mvin_cols_bits = log2Up(((dma_maxbytes / (inputType.getWidth / 8)) max (meshColumns * tileColumns)) + 1)
  val mvin_rows_bits = log2Up(meshRows * tileRows + 1)
  val mvout_cols_bits = log2Up(((dma_maxbytes / (inputType.getWidth / 8)) max (meshColumns * tileColumns)) + 1)
  val mvout_rows_bits = log2Up(meshRows * tileRows + 1)

  val load_states = 3
  val block_stride_bits = 16

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
  val rob_entries      = rob_full_entries + rob_partial_entries
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
        case dt: UInt => ("0", BigInt(2).pow(dt.getWidth).-(1).toString)
        case dt: SInt => ("-" + BigInt(2).pow(dt.getWidth - 1).toString, BigInt(2).pow(dt.getWidth - 1).-(1).toString)
        case dt: Float =>
          (dt.expWidth, dt.sigWidth) match {
            case (8, 24) => (scala.Float.MinValue.toString, scala.Float.MaxValue.toString)
            case (11, 53) => (scala.Double.MinValue.toString, scala.Double.MaxValue.toString)
            case _ => (((Range(-1,-(dt.sigWidth),-1).map(-Math.pow(2, _)).foldLeft(-1.0)(_ + _)) * Math.pow(2, Math.pow(2, dt.expWidth - 1) - 1)).toString, ((Range(-1,-(dt.sigWidth),-1).map(Math.pow(2, _)).foldLeft(1.0)(_ + _)) * Math.pow(2, Math.pow(2, dt.expWidth - 1) - 1)).toString)
          }
        case _ => throw new IllegalArgumentException(s"Data type $dataType is unknown")
      }
    }

    def c_type(dataType: Data): String = {
      dataType match {
        case dt: UInt => s"uint${dt.getWidth}_t"
        case dt: SInt => s"int${dt.getWidth}_t"
        case dt: Float =>
          (dt.expWidth, dt.sigWidth) match {
            case (8, 24) => "float"
            case (11, 53) => "double"
            case _ => s"uint" + (Math.pow(2, Math.ceil(Math.log(dt.expWidth + dt.sigWidth)/Math.log(2.0)))).toInt.toString + s"_t"
          }
        case _ => throw new IllegalArgumentException(s"Data type $dataType is unknown")
      }
    }

    def full_c_type(dataType: Data): String = {
      dataType match {
        case dt: UInt => "uint64_t"
        case dt: SInt => "int64_t"
        case dt: Float => "double"
        case _ => throw new IllegalArgumentException(s"Data type $dataType is unknown")
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

    header ++= s"typedef ${c_type(acc_scale_args.multiplicand_t)} acc_scale_t;\n"
    header ++= s"typedef ${c_type(UInt(acc_scale_args.multiplicand_t.getWidth.W))} acc_scale_t_bits;\n\n"

    header ++= s"#define row_align(blocks) __attribute__((aligned(blocks*DIM*sizeof(elem_t))))\n"
    header ++= s"#define row_align_acc(blocks) __attribute__((aligned(blocks*DIM*sizeof(acc_t))))\n\n"

    val mvin_scale_identity = mvin_scale_args match {
      case Some(ScaleArguments(_, _, _, _, identity, _)) => identity
      case None => "0"
    }
    header ++= s"#define MVIN_SCALE_IDENTITY $mvin_scale_identity\n\n"
    header ++= s"#define ACC_SCALE_IDENTITY ${acc_scale_args.identity}\n\n"

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
    header ++= s"    ${acc_scale_args.c_str}"
    header ++= "\n\n"

    if (mvin_scale_args.isDefined) {
      header ++=
        s"""#define MVIN_SCALE(x, scale) \\
    ${mvin_scale_args.get.c_str}"""
      header ++= "\n\n"
    }

    if (mvin_scale_acc_args.isDefined) {
      header ++=
        s"""#define MVIN_SCALE_ACC(x, scale) \\
    ${mvin_scale_acc_args.get.c_str}"""
      header ++= "\n\n"
    }

    if (acc_scale_args.multiplicand_t.isInstanceOf[Float]) {
      header ++= "#define ACC_SCALE_T_IS_FLOAT\n"
      header ++= s"#define ACC_SCALE_EXP_BITS ${acc_scale_args.multiplicand_t.asInstanceOf[Float].expWidth}\n"
      header ++= s"#define ACC_SCALE_SIG_BITS ${acc_scale_args.multiplicand_t.asInstanceOf[Float].sigWidth}\n\n"
    }

    if (acc_read_small_width)
      header ++= s"#define ACC_READ_SMALL_WIDTH\n"
    if (acc_read_full_width)
      header ++= s"#define ACC_READ_FULL_WIDTH\n"
    header ++= s"\n"

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

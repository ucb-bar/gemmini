package gemmini

import chisel3._
import chisel3.util._

sealed abstract trait GemminiMemCapacity
case class CapacityInKilobytes(kilobytes: Int) extends GemminiMemCapacity
case class CapacityInMatrices(matrices: Int) extends GemminiMemCapacity

case class GemminiArrayConfig[T <: Data : Arithmetic](
                                                         tileRows: Int,
                                                         tileColumns: Int,
                                                         meshRows: Int,
                                                         meshColumns: Int,
                                                         ld_queue_length: Int,
                                                         st_queue_length: Int,
                                                         ex_queue_length: Int,
                                                         rob_entries: Int,
                                                         sp_banks: Int, // TODO support one-bank designs
                                                         sp_capacity: GemminiMemCapacity,
                                                         acc_banks: Int,
                                                         acc_capacity: GemminiMemCapacity,
                                                         shifter_banks: Int,
                                                         dataflow: Dataflow.Value,
                                                         mem_pipeline: Int,
                                                         dma_maxbytes: Int,
                                                         dma_buswidth: Int,
                                                         aligned_to: Int, // TODO we should align to inputType and outputType instead
                                                         inputType: T,
                                                         outputType: T,
                                                         accType: T,
                                                         pe_latency: Int,
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

  val local_addr_t = new LocalAddr(sp_banks, sp_bank_entries, acc_banks, acc_bank_entries)

  val max_in_flight_reqs = 16 // TODO calculate this somehow

  val mvin_len_bits = log2Up(((dma_maxbytes / (inputType.getWidth / 8)) max (meshColumns * tileColumns)) + 1)
  val mvin_rows_bits = log2Up(meshRows * tileRows + 1)
  val mvout_len_bits = log2Up(meshColumns * tileColumns + 1)
  val mvout_rows_bits = log2Up(meshRows * tileRows + 1)

  require(isPow2(sp_bank_entries), "each SRAM bank must have a power-of-2 rows, to simplify address calculations") // TODO remove this requirement
  require(sp_bank_entries % (meshRows * tileRows) == 0, "the number of rows in a bank must be a multiple of the dimensions of the systolic array")
  require(meshColumns * tileColumns == meshRows * tileRows, "the systolic array must be square") // TODO remove this requirement
  require(meshColumns * tileColumns >= 2, "the systolic array must have a dimension of at least 2") // TODO remove this requirement
  require(isPow2(meshColumns * tileColumns), "the systolic array's dimensions must be powers of 2") // TODO remove this requirement
  require(acc_bank_entries % (meshRows * tileRows) == 0, "the number of rows in an accumulator bank must be a multiple of the dimensions of the systolic array")

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
            case _ => throw new IllegalArgumentException(s"Only single- and double-precision IEEE754 floating point types are currently supported")
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
            case _ => throw new IllegalArgumentException(s"Only single- and double-precision IEEE754 floating point types are currently supported")
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
    header ++= s"elem_t elem_t_max = ${limits._2};\n"
    header ++= s"elem_t elem_t_min = ${limits._1};\n"
    header ++= s"typedef ${c_type(accType)} acc_t;\n"
    header ++= s"typedef ${full_c_type(inputType)} full_t;\n\n"

    if (inputType.isInstanceOf[Float]) {
      header ++= "#define ELEM_T_IS_FLOAT\n\n"
    }

    header ++= s"#define row_align(blocks) __attribute__((aligned(blocks*DIM*sizeof(elem_t))))\n"
    header ++= s"#define row_align_acc(blocks) __attribute__((aligned(blocks*DIM*sizeof(acc_t))))\n\n"

    header ++= s"#endif // $guard"
    header.toString()
  }

  def headerFilePath: String = {
    val chipyard_directory = "./generators/gemmini/software/gemmini-rocc-tests/include"
    val project_template_directory = "./gemmini-rocc-tests/include" // Old root directory; rendered obsolete by Chipyard
    val default_directory = "."

    val in_chipyard = {
      val dir = new java.io.File(chipyard_directory)
      dir.exists() && dir.isDirectory
    }

    val in_project_template = {
      val dir = new java.io.File(project_template_directory)
      dir.exists() && dir.isDirectory
    }

    if (in_chipyard) {
      s"$chipyard_directory/$headerFileName"
    } else if (in_project_template) {
      s"$project_template_directory/$headerFileName"
    } else {
      s"$default_directory/$headerFileName"
    }
  }
}

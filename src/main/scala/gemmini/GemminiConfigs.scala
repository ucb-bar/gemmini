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
                                                         acc_capacity: GemminiMemCapacity,
                                                         shifter_banks: Int,
                                                         dataflow: Dataflow.Value,
                                                         mem_pipeline: Int,
                                                         dma_maxbytes: Int,
                                                         dma_buswidth: Int,
                                                         aligned_to: Int,
                                                         inputType: T,
                                                         outputType: T,
                                                         accType: T,
                                                         headerFileName: String = "gemmini_params.h"
                                                       ) {
  val sp_width = meshColumns * tileColumns * inputType.getWidth
  val sp_bank_entries = sp_capacity match {
    case CapacityInKilobytes(kb) => kb * 1024 * 8 / (sp_banks * sp_width)
    case CapacityInMatrices(ms) => ms * meshRows * tileRows / sp_banks
  }
  val acc_rows = acc_capacity match {
    case CapacityInKilobytes(kb) => kb * 1024 * 8 / (meshColumns * tileColumns * accType.getWidth)
    case CapacityInMatrices(ms) => ms * meshRows * tileRows
  }

  val local_addr_t = new LocalAddr(sp_banks, sp_bank_entries, acc_rows)

  val max_in_flight_reqs = 16 // TODO calculate this somehow

  require(isPow2(sp_bank_entries), "each SRAM bank must have a power-of-2 rows, to simplify address calculations") // TODO remove this requirement
  require(sp_bank_entries % (meshRows * tileRows) == 0, "the number of rows in a bank must be a multiple of the dimensions of the systolic array")
  require(acc_rows % (meshRows * tileRows) == 0, "the number of rows in the accumulator must be a multiple of the dimensions of the systolic array")

  def generateHeader(guard: String = "SYSTOLIC_PARAMS_H"): String = {
    // Returns the (min,max) values for a dataType
    def limitsOfDataType(dataType: Data): (String, String) = {
      assert(dataType.getWidth <= 32) // Above 32 bits, we need to append UL to the number, which isn't done yet
      if (dataType.isInstanceOf[UInt]) {
        ("0", BigInt(2).pow(dataType.getWidth).-(1).toString)
      } else if (dataType.isInstanceOf[SInt]) {
        ("-" + BigInt(2).pow(dataType.getWidth - 1).toString ,BigInt(2).pow(dataType.getWidth - 1).-(1).toString)
      } else {
        throw new IllegalArgumentException(s"Data type $dataType isn't an integer")
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
    header ++= s"#define BANK_ROWS $sp_bank_entries\n" // TODO: check
    header ++= s"#define ACC_ROWS $acc_rows\n" // TODO: check
    header ++= s"#define MAX_BYTES 64\n"
    header ++= s"#define MAX_BLOCK_LEN (MAX_BYTES/(DIM*${inputType.getWidth/8}))\n"
    header ++= s"#define MAX_BLOCK_LEN_ACC (MAX_BYTES/(DIM*${accType.getWidth/8}))\n\n"

    // Datatype of the systolic array
    val limits = limitsOfDataType(inputType)
    header ++= s"typedef int${inputType.getWidth}_t elem_t;\n"
    header ++= s"elem_t elem_t_max = ${limits._2};\n"
    header ++= s"elem_t elem_t_min = ${limits._1};\n"
    header ++= s"typedef int${accType.getWidth}_t acc_t;\n\n"

    header ++= s"#define row_align(blocks) __attribute__((aligned(blocks*DIM*sizeof(elem_t))))\n"
    header ++= s"#define row_align_acc(blocks) __attribute__((aligned(blocks*DIM*sizeof(acc_t))))\n\n"

    header ++= s"#endif // $guard"
    header.toString()
  }

  def headerFilePath: String = {
    val chipyard_directory = "./generators/gemmini/gemmini-tests-workload/gemmini-rocc-tests/include"
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

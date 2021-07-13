package gemmini

import chisel3._
import chisel3.util._

object GemminiISA {
  // funct values
  val CONFIG_CMD = 0.U
  val LOAD2_CMD = 1.U
  val LOAD_CMD = 2.U
  val STORE_CMD = 3.U
  val COMPUTE_AND_FLIP_CMD = 4.U
  val COMPUTE_AND_STAY_CMD = 5.U
  val PRELOAD_CMD = 6.U
  val FLUSH_CMD = 7.U

  val LOOP_WS = 8.U
  val LOOP_WS_CONFIG_BOUNDS = 9.U
  val LOOP_WS_CONFIG_ADDRS_AB = 10.U
  val LOOP_WS_CONFIG_ADDRS_DC = 11.U
  val LOOP_WS_CONFIG_STRIDES_AB = 12.U
  val LOOP_WS_CONFIG_STRIDES_DC = 13.U

  val LOAD3_CMD = 14.U

  // TODO add orows and ocols to this as well
  val LOOP_CONV_WS = 15.U // no_bias, wrot180, trans_output_1203, trans_weight_1203, trans_input_3120 | no_pool, downsample
  val LOOP_CONV_WS_CONFIG_1 = 16.U // batch_size, in_dim, in_channels, out_channels | out_dim, pool_out_dim, stride, padding
  val LOOP_CONV_WS_CONFIG_2 = 17.U // kernel_dim, pool_size, pool_stride, pool_padding | batches, porows, pocols, pochs
  val LOOP_CONV_WS_CONFIG_3 = 18.U // krows, kcols, kchs, lpad | rpad, upad, dpad, plpad
  val LOOP_CONV_WS_CONFIG_4 = 19.U // prad, pupad, pdpad, orows | ocols, kernel_dilation
  val LOOP_CONV_WS_CONFIG_5 = 20.U // *weights | *output
  val LOOP_CONV_WS_CONFIG_6 = 21.U // *bias, *input

  // rs1[2:0] values
  val CONFIG_EX = 0.U
  val CONFIG_LOAD = 1.U
  val CONFIG_STORE = 2.U
  val CONFIG_IM2COL = 3.U

  //==========================================================================
  // cisc-gemmini opcodes
  //==========================================================================
  // TODO the numbers here overlap with the LOOP_WS commands
  val CISC_CONFIG  = 10.U(7.W) // same as COMPUTE_AND_FLIP
  val ADDR_AB      = 11.U(7.W)
  val ADDR_CD      = 12.U(7.W)
  val SIZE_MN      = 13.U(7.W)
  val SIZE_K       = 14.U(7.W)
  val RPT_BIAS     = 15.U(7.W)
  val RESET        = 16.U(7.W)
  val COMPUTE_CISC = 17.U(7.W)

  //==========================================================================
  // dataflow configuration
  //==========================================================================
  val GARBAGE_ADDR      = "hffffffff".U(32.W)

  private val register_len = 64

  object LoadCmd {
    class Rs1(val coreMaxAddrBits: Int) extends Bundle {
      val garbage = UInt((register_len - coreMaxAddrBits).W)
      val dram_addr = UInt(coreMaxAddrBits.W)
    }

    class Rs2(local_addr_t: LocalAddr) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt(((16 - maxLocalAddrBits) max 0).W)
      val rows = UInt((16 min maxLocalAddrBits).W)
      val garbage2 = UInt(((16 - maxLocalAddrBits) max 0).W)
      val cols = UInt((16 min maxLocalAddrBits).W)
      val spad_addr = local_addr_t.cloneType

      override def cloneType: Rs2.this.type = new Rs2(local_addr_t).asInstanceOf[this.type]
    }
  }

  object StoreCmd {
    class Rs1(val coreMaxAddrBits: Int) extends Bundle {
      val garbage = UInt((register_len - coreMaxAddrBits).W)
      val dram_addr = UInt(coreMaxAddrBits.W)
    }

    class Rs2(local_addr_t: LocalAddr) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt(((16 - maxLocalAddrBits) max 0).W)
      val rows = UInt((16 min maxLocalAddrBits).W)
      val garbage2 = UInt(((16 - maxLocalAddrBits) max 0).W)
      val cols = UInt((16 min maxLocalAddrBits).W)
      val spad_addr = local_addr_t.cloneType

      override def cloneType: Rs2.this.type = new Rs2(local_addr_t).asInstanceOf[this.type]
    }
  }

  object PreloadCmd {
    class Rs1(local_addr_t: LocalAddr, block_size: Int) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt((16 - log2Up(block_size+1)).W)
      val bd_rows = UInt(log2Up(block_size+1).W)
      val garbage2 = UInt((16 - log2Up(block_size+1)).W)
      val bd_cols = UInt(log2Up(block_size+1).W)
      val bd = local_addr_t.cloneType

      override def cloneType: Rs1.this.type = new Rs1(local_addr_t, block_size).asInstanceOf[this.type]
    }

    class Rs2(local_addr_t: LocalAddr, block_size: Int, max_block_len: Int) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt((16 - log2Up(max_block_len*block_size+1)).W)
      val c_rows = UInt(log2Up(max_block_len*block_size+1).W)
      val garbage2 = UInt((16 - log2Up(max_block_len*block_size+1)).W)
      val c_cols = UInt(log2Up(max_block_len*block_size+1).W)
      val c = local_addr_t.cloneType

      override def cloneType: Rs2.this.type = new Rs2(local_addr_t, block_size, max_block_len).asInstanceOf[this.type]
    }
  }

  object ComputeCmd {
    class Rs1(local_addr_t: LocalAddr, block_size: Int, max_block_len: Int) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt((16 - log2Up(max_block_len*block_size+1)).W)
      val a_rows = UInt(log2Up(max_block_len*block_size+1).W)
      val garbage2 = UInt((16 - log2Up(max_block_len*block_size+1)).W)
      val a_cols = UInt(log2Up(max_block_len*block_size+1).W)
      val a = local_addr_t.cloneType

      override def cloneType: Rs1.this.type = new Rs1(local_addr_t, block_size, max_block_len).asInstanceOf[this.type]
    }

    class Rs2(local_addr_t: LocalAddr, block_size: Int) extends Bundle {
      private val maxLocalAddrBits = local_addr_t.maxLocalAddrBits

      val garbage1 = UInt((16 - log2Up(block_size+1)).W)
      val bd_rows = UInt(log2Up(block_size+1).W)
      val garbage2 = UInt((16 - log2Up(block_size+1)).W)
      val bd_cols = UInt(log2Up(block_size+1).W)
      val bd = local_addr_t.cloneType

      override def cloneType: Rs2.this.type = new Rs2(local_addr_t, block_size).asInstanceOf[this.type]
    }
  }
}

// package gemmini

// import chisel3._
// import chisel3.util._
// import chiseltest._
// import org.scalatest.flatspec.AnyFlatSpec
// import chisel3.experimental.BundleLiterals._

// class MxRequantizerSpec extends AnyFlatSpec with ChiselScalatestTester {
  
//   behavior of "MxRequantizer"
  
//   def createConfig(inputLanes: Int, outputLanes: Int) = new GemminiRequantizerConfig(
//     numInputLanes = inputLanes,
//     numOutputLanes = outputLanes,
//     inputBits = 16,
//     baseAddr = 0x80000000L
//   )
  
//   it should "find block max correctly in 16-lane FP8 mode" in {
//     val config = createConfig(inputLanes = 64, outputLanes = 32)
//     test(new MxRequantizer[SInt](
//       sp_data_width = 128,
//       sp_addr_width = 12,
//       scaleMem_data_width = 64,
//       scaleMem_addr_width = 10,
//       scaleSize = 8,
//       scaleMembasewrite = 0x1000,
//       sp_bank_entries = 64,
//       sp_banks = 4,
//       sp_width = 128,
//       sp_width_projected = 64,
//       config = config
//     )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
//       dut.io.fp8_mode.poke(true.B)
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(5)
      
//       val testData1 = Seq(0x3F80, 0x4000, 0x4040, 0x4080, 0x3C00, 0x3E00, 0x4100, 0x4200,
//                           0x3800, 0x3A00, 0x3D00, 0x4300, 0x4400, 0x4500, 0x4600, 0x4700)
      
//       dut.io.requnat_data_in.valid.poke(true.B)
//       dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP8)
//       dut.io.requnat_data_in.bits.address.poke(0x1000.U)
//       for (i <- 0 until 16) {
//         dut.io.requnat_data_in.bits.data(i).poke(testData1(i).U)
//       }
//       dut.clock.step(1)
      
//       val testData2 = Seq(0x4800, 0x4900, 0x4A00, 0x4B00, 0x4C00, 0x4D00, 0x4E00, 0x4F00,
//                           0x5000, 0x5100, 0x5200, 0x5300, 0x5400, 0x5500, 0x5600, 0x5700)
      
//       for (i <- 0 until 16) {
//         dut.io.requnat_data_in.bits.data(i).poke(testData2(i).U)
//       }
//       dut.clock.step(1)
      
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(10)
      
//       println("Test: 16-lane FP8 mode - expected max should be 0x5700")
//     }
//   }
  
//   it should "find block max correctly in 64-lane FP4 mode" in {
//     val config = createConfig(inputLanes = 64, outputLanes = 32)
//     test(new MxRequantizer[SInt](
//       sp_data_width = 128,
//       sp_addr_width = 12,
//       scaleMem_data_width = 64,
//       scaleMem_addr_width = 10,
//       scaleSize = 8,
//       scaleMembasewrite = 0x1000,
//       config = config
//     )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
//       dut.io.fp8_mode.poke(false.B)
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(5)
      
//       val testData = (0 until 64).map(i => (0x3000 + i * 0x10).U)
      
//       dut.io.requnat_data_in.valid.poke(true.B)
//       dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP4)
//       dut.io.requnat_data_in.bits.address.poke(0x2000.U)
//       for (i <- 0 until 64) {
//         dut.io.requnat_data_in.bits.data(i).poke(testData(i))
//       }
//       dut.clock.step(1)
      
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(15)
      
//       println("Test: 64-lane FP4 mode - expected max should be 0x33F0")
//     }
//   }
  
//   it should "handle zero values correctly" in {
//     val config = createConfig(inputLanes = 64, outputLanes = 32)
//     test(new MxRequantizer[SInt](
//       sp_data_width = 128,
//       sp_addr_width = 12,
//       scaleMem_data_width = 64,
//       scaleMem_addr_width = 10,
//       scaleSize = 8,
//       scaleMembasewrite = 0x1000,
//       config = config
//     )).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
//       dut.io.fp8_mode.poke(true.B)
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(5)
      
//       dut.io.requnat_data_in.valid.poke(true.B)
//       dut.io.requnat_data_in.bits.dataType.poke(RequantizerDataType.FP6)
//       dut.io.requnat_data_in.bits.address.poke(0x3000.U)
//       for (i <- 0 until 16) {
//         dut.io.requnat_data_in.bits.data(i).poke(0.U)
//       }
//       dut.clock.step(1)
      
//       for (i <- 0 until 16) {
//         dut.io.requnat_data_in.bits.data(i).poke(0.U)
//       }
//       dut.clock.step(1)
      
//       dut.io.requnat_data_in.valid.poke(false.B)
//       dut.clock.step(10)
      
//       println("Test: All zeros - scale should be 0")
//     }
//   }
// }
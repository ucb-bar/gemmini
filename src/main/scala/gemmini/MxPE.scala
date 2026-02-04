// package gemmini 

// import chisel3._
// import chisel3.util._

// class MxPE(mxparameters: MxParams, lut: Boolean) extends Module {
//   val io = IO(new Bundle {
//     val modeDecoded = Input(new mxMode())
//     val activation_mx_format = Input(UInt(2.W))   // 0=FP8, 1=FP6, 2=FP4
//     val weight_mx_format = Input(UInt(2.W))
//     val in_a = Input(UInt(mxparameters.inPE_act_totalWidth.W))
//     val in_w = Input(UInt(mxparameters.inPE_wei_totalWidth.W))
//     val mask_a = Input(UInt(4.W))
//     val mask_w = Input(UInt(4.W))
//     val enable = Input(Bool())
//     val output = Output(UInt(mxparameters.outPE_width.W))
//   })

//   val flexMults = Seq.fill(2,2){ Module(new MACU(lut)) }
//   val outFM = Wire(Vec(4, UInt(mxparameters.multOutWidth.W)))

//   for (i <- 0 until 2) {
//     for (j <- 0 until 2) {
//       val fm = flexMults(i)(j)
//       val a = Wire(UInt(mxparameters.actflexMulInWidth.W))
//       val a_en = Wire(Bool())
//       val w_en = Wire(Bool())
//       when (io.activation_mx_format === 2.U) {  // FP4
//         when (io.modeDecoded.actInputs === 1.U) {
//           a := io.in_a(2*(i+1) - 1, 2*i)
//           a_en := io.mask_a(0)
//         } .otherwise {
//           a := io.in_a(mxparameters.actflexMulInWidth*(i+1) - 1, mxparameters.actflexMulInWidth*i)
//           a_en := io.mask_a(i*2)
//         }
//       } .elsewhen (io.activation_mx_format === 1.U) {  // FP6
//         a := io.in_a(6*(i+1) - 1, 6*i)
//         a_en := io.mask_a(i)
//       } .otherwise {  // FP8 (default)
//         a := io.in_a(8*(i+1) - 1, 8*i)
//         a_en := io.mask_a(i)
//       }

//       val w = Wire(UInt(mxparameters.weiflexMulInWidth.W))
//       when (io.weight_mx_format === 2.U) {  // FP4
//         when (io.modeDecoded.weiInputs === 1.U) {
//           w := io.in_w(2*(j+1) - 1, 2*j).pad(mxparameters.weiflexMulInWidth)
//           w_en := io.mask_w(0)
//         } .otherwise {
//           w := io.in_w(mxparameters.weiflexMulInWidth*(j+1) - 1, mxparameters.weiflexMulInWidth*j)
//           w_en := io.mask_w(j*2)
//         }
//       } .elsewhen (io.weight_mx_format === 1.U) {  // FP6
//         w := io.in_w(6*(j+1) - 1, 6*j)
//         w_en := io.mask_w(j)
//       } .otherwise {  // FP8 (default)
//         w := io.in_w(8*(j+1) - 1, 8*j)
//         w_en := io.mask_w(j)
//       }

//       fm.io.w := a(mxparameters.actflexMulInWidth - 1, 0)
//       fm.io.act := w(mxparameters.weiflexMulInWidth - 1, 0)
//       fm.io.enable := io.enable && a_en && w_en
      
      
//       fm.io.w_mode := (io.activation_mx_format === 1.U)  // FP6 mode
//       fm.io.act_mode := (io.weight_mx_format === 1.U)

//       if (mxparameters.multOutWidth == 6) {
//         outFM(i*2 + j) := fm.io.output
//       } else if (mxparameters.multOutWidth == 5) {
//         outFM(i*2 + j) := fm.io.output(4,0)
//       } else {
//         outFM(i*2 + j) := fm.io.output(3,0)
//       }
//     }
//   }

//   when (io.activation_mx_format === 2.U || io.weight_mx_format === 2.U) { 
//     val outShift0 = outFM(3) << io.modeDecoded.shift(0)(0)
//     val outShift1 = outFM(2) << io.modeDecoded.shift(0)(1)
//     val outShift2 = outFM(1) << io.modeDecoded.shift(1)(0)
//     val outShift3 = outFM(0) << io.modeDecoded.shift(1)(1)

//     val sum1 = outShift0 + outShift1 + outShift2 + outShift3
//     val sum2 = Mux(io.modeDecoded.actWidth === 4.U, outShift0 + outShift2, outShift0 + outShift1)
//     val sum3 = Mux(io.modeDecoded.actWidth === 4.U, outShift1 + outShift3, outShift2 + outShift3)

//     val out1 = sum1.pad(mxparameters.outPE_width)
//     val out2 = Wire(Vec(2, UInt((mxparameters.outPE_width / 2).W))) 
//     out2(0) := sum3.pad((mxparameters.outPE_width / 2))
//     out2(1) := sum2.pad((mxparameters.outPE_width / 2))

//     io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.asUInt, 
//                    Mux(io.modeDecoded.numOutputs === 2.U, out2.asUInt, 
//                      outFM.asUInt))
    
//   } .otherwise {
//     io.output := outFM.asUInt
//   }
// }


package gemmini 

import chisel3._
import chisel3.util._

class MxPE(mxparameters: MxParams, lut: Boolean) extends Module {
  require(mxparameters.inPE_wei_totalWidth == mxparameters.inPE_act_totalWidth, "inPE_wei_totalWidth must equal inPE_act_totalWidth for this MxPE implementation")
  val io = IO(new Bundle {
    val modeDecoded = Input(new mxMode())
    val in_a = Input(UInt(mxparameters.inPE_act_totalWidth.W))
    val in_w = Input(UInt(mxparameters.inPE_wei_totalWidth.W))
    val mask_a = Input(UInt(2.W))
    val mask_w = Input(UInt(2.W))
    val enable = Input(Bool())
    val output = Output(UInt(mxparameters.outPE_width.W))
  })

  val flexMults = Seq.fill(2,2){ Module(new MACU(lut)) }
  val outFM = Wire(Vec(4, UInt(mxparameters.multOutWidth.W)))

  // printf(p"enable: ${io.enable}, mask_a: ${Binary(io.mask_a)}, mask_w: ${Binary(io.mask_w)}\n")  // --- IGNORE ---
  // printf(p"in_a: ${Binary(io.in_a)}, in_w: ${Binary(io.in_w)}\n")  // --- IGNORE ---

  for (i <- 0 until 2) {
    for (j <- 0 until 2) {
      val fm = flexMults(i)(j)
      val a = Wire(UInt(mxparameters.actflexMulInWidth.W))
      val a_en = Wire(Bool())
      val w_en = Wire(Bool())

      if (mxparameters.inPE_act_width == 4) {
          when (io.modeDecoded.actInputs === 1.U) {
            a := io.in_a(2*(i+1) - 1, 2*i)
            a_en := io.mask_a(0)
          } .otherwise {
            a := io.in_a(mxparameters.actflexMulInWidth*(i+1) - 1, mxparameters.actflexMulInWidth*i)
            a_en := io.mask_a(i)
          }
      } else {
        a := io.in_a(mxparameters.actflexMulInWidth*(i+1) - 1, mxparameters.actflexMulInWidth*i)
        a_en := io.mask_a(i)
      }

      val w = Wire(UInt(mxparameters.weiflexMulInWidth.W))

      if (mxparameters.numWeiInputs < 3) {
        if (mxparameters.inPE_wei_width == 4) {
          when (io.modeDecoded.weiInputs === 1.U) {
            w := io.in_w(2*(j+1) - 1, 2*j).pad(mxparameters.weiflexMulInWidth)
            w_en := io.mask_w(0)
          } .otherwise {
            w := io.in_w(mxparameters.weiflexMulInWidth*(j+1) - 1, mxparameters.weiflexMulInWidth*j)
            w_en := io.mask_w(j)
          }
        } else {
          w := io.in_w(mxparameters.weiflexMulInWidth*(j+1) - 1, mxparameters.weiflexMulInWidth*j)
          w_en := io.mask_w(j)
        }
      } else {
        when (io.modeDecoded.weiInputs === 1.U) {
            w := io.in_w(2*(j+1) - 1, 2*j).pad(mxparameters.weiflexMulInWidth)
            w_en := io.mask_w(0)
          } .elsewhen (io.modeDecoded.weiInputs < 3.U) {
            w := io.in_w(mxparameters.weiflexMulInWidth*(j*2+1) - 1, mxparameters.weiflexMulInWidth*j*2)
            w_en := io.mask_w(j)
          }.otherwise {
            w := io.in_w(mxparameters.weiflexMulInWidth*(i*2+j + 1) - 1, mxparameters.weiflexMulInWidth*((i*2+j)))
            w_en := io.mask_w(j)
          }
      }

      // printf(p"a: ${Binary(a)} w: ${Binary(w)} \n")

      fm.io.w := a(mxparameters.actflexMulInWidth - 1, 0)
      fm.io.act := w(mxparameters.weiflexMulInWidth - 1, 0)
      fm.io.enable := io.enable && a_en && w_en
      fm.io.w_mode := (mxparameters.weiflexMulInWidth == 3 || mxparameters.actflexMulInWidth == 3).B
      fm.io.act_mode := (mxparameters.actflexMulInWidth == 3 || mxparameters.weiflexMulInWidth == 3).B

      if (mxparameters.multOutWidth == 6) {
        outFM(i*2 + j) := fm.io.output
      } else if (mxparameters.multOutWidth == 5) {
        outFM(i*2 + j) := fm.io.output(4,0)
      } else {
        outFM(i*2 + j) := fm.io.output(3,0)
      }
    }
  }

  if (mxparameters.inPE_act_width == 4 || mxparameters.inPE_wei_width == 4) {
    val outShift0 = outFM(3) << io.modeDecoded.shift(0)(0)
    val outShift1 = outFM(2) << io.modeDecoded.shift(0)(1)
    val outShift2 = outFM(1) << io.modeDecoded.shift(1)(0)
    val outShift3 = outFM(0) << io.modeDecoded.shift(1)(1)

    val sum1 = outShift0 + outShift1 + outShift2 + outShift3
    val sum2 = Mux(io.modeDecoded.actWidth === 4.U, outShift0 + outShift2, outShift0 + outShift1)
    val sum3 = Mux(io.modeDecoded.actWidth === 4.U, outShift1 + outShift3, outShift2 + outShift3)
    
    val out1 = sum1.pad(mxparameters.outPE_width)
    val out2 = Wire(Vec(2, UInt((mxparameters.outPE_width / 2).W))) 
    out2(0) := sum3.pad((mxparameters.outPE_width / 2))
    out2(1) := sum2.pad((mxparameters.outPE_width / 2))

    io.output := Mux(io.modeDecoded.numOutputs === 1.U, out1.asUInt, 
                   Mux(io.modeDecoded.numOutputs === 2.U, out2.asUInt, 
                     outFM.asUInt))
    
  } else {
    io.output := outFM.asUInt
  }
}
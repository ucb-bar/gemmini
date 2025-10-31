package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental._
import org.chipsalliance.cde.config.Parameters

import scala.math.{pow}

class QuantLut (wPorts: Int, rPorts: Int, lutaddr: Int, lutdata: Int)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle{
        val rdaddr = Input(Vec(rPorts, UInt(lutaddr.W)))
        val wraddr = Input(Vec(wPorts, UInt(lutaddr.W)))
        val rddata = Output(Vec(rPorts, UInt(lutdata.W)))
        val wrdata = Input(Vec(wPorts, UInt(lutdata.W)))
        val wr = Input(Bool())
    })

    val lut = RegInit(VecInit(Seq.fill(pow(2,lutaddr).toInt) (0.U(lutdata.W))))
    
    for (i <- 0 until rPorts) {
        io.rddata(i) := lut(io.rdaddr(i))
    }

    when(io.wr) {
        for (j <- 0 until wPorts) {
            lut(io.wraddr(j)) := io.wrdata(j)
        }
    }
}
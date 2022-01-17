package gemmini

import chisel3._
import chisel3.util._

class Pipeline[T <: Data] (gen: T, latency: Int)(comb: Seq[T => T] = Seq.fill(latency+1)((x: T) => x)) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
    val busy = Output(Bool())
  })

  require(comb.size == latency+1, "length of combinational is incorrect")

  if (latency == 0) {
    io.in.ready := io.out.ready
    io.out.valid := io.in.valid
    io.out.bits := comb.head(io.in.bits)
    io.busy := io.in.valid
  } else {
    val stages = Reg(Vec(latency, gen))
    val valids = RegInit(VecInit(Seq.fill(latency)(false.B)))
    val stalling = VecInit(Seq.fill(latency)(false.B))
    io.busy := io.in.valid || valids.reduce(_||_)

    // Stall signals
    io.in.ready := !stalling.head
    stalling.last := valids.last && !io.out.ready
    (stalling.init, stalling.tail, valids.init).zipped.foreach { case (s1, s2, v1) =>
      s1 := v1 && s2
    }

    // Valid signals
    // When the pipeline stage ahead of you isn't stalling, then make yourself invalid
    io.out.valid := valids.last
    when(io.out.ready) {
      valids.last := false.B
    }
    (valids.init, stalling.tail).zipped.foreach { case (v1, s2) =>
      when(!s2) {
        v1 := false.B
      }
    }
    // When the pipeline stage behind you is valid then become true
    when(io.in.fire) {
      valids.head := true.B
    }
    (valids.tail, valids.init).zipped.foreach { case (v2, v1) =>
      when(v1) {
        v2 := true.B
      }
    }

    // Stages
    when(io.in.fire) {
      stages.head := comb.head(io.in.bits)
    }
    io.out.bits := comb.last(stages.last)
    ((stages.tail zip stages.init) zip (stalling.tail zip comb.tail.init)).foreach { case ((st2, st1), (s2, c1)) =>
      when(!s2) {
        st2 := c1(st1)
      }
    }
  }
}

object Pipeline {
  def apply[T <: Data](in: ReadyValidIO[T], latency: Int, comb: Seq[T => T]): DecoupledIO[T] = {
    val p = Module(new Pipeline(in.bits.cloneType, latency)(comb))
    p.io.in <> in
    p.io.out
  }

  def apply[T <: Data](in: ReadyValidIO[T], latency: Int): DecoupledIO[T] = {
    val p = Module(new Pipeline(in.bits.cloneType, latency)())
    p.io.in <> in
    p.io.out
  }
}

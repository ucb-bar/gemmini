package gemmini

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class DMACommandTrackerTester(c: DMAReadCommandTracker[UInt]) extends PeekPokeTester(c) {
  case class AllocatedCmd(id: Int, tag: Int, requestsSent: Int)
  
  var max_cycles = 100000000

  var cmdsToAllocate = 100
  val cmdIdsAllocated: ArrayBuffer[AllocatedCmd] = ArrayBuffer() // TODO use a map instead

  while ((cmdsToAllocate > 0 || cmdIdsAllocated.size > 0) && max_cycles > 0) {
    val can_allocate = cmdsToAllocate > 0 && rnd.nextBoolean()
    poke(c.io.alloc.valid, can_allocate)

    val tag = rnd.nextInt(100)
    poke(c.io.alloc.bits.tag, tag)

    val can_read_cmd = rnd.nextBoolean()
    poke(c.io.cmd_completed.ready, can_read_cmd)

    if (rnd.nextBoolean() || cmdIdsAllocated.size == 0) {
      poke(c.io.request_returned.valid, false)
    } else {
      val cmd_buf_id = rnd.nextInt(cmdIdsAllocated.size)
      val id = cmdIdsAllocated(cmd_buf_id).id
      val requestsSent = cmdIdsAllocated(cmd_buf_id).requestsSent

      if (requestsSent < c.nRequests) {
        poke(c.io.request_returned.valid, true)
        poke(c.io.request_returned.bits.cmd_id, id)

        cmdIdsAllocated(cmd_buf_id) = cmdIdsAllocated(cmd_buf_id).copy(requestsSent=requestsSent+1)
      } else {
        poke(c.io.request_returned.valid, false)
      }
    }

    val alloc_fire = can_allocate && peek(c.io.alloc.ready) != 0
    if (alloc_fire) {
      cmdsToAllocate -= 1
      val cmd_id = peek(c.io.alloc.bits.cmd_id).toInt
      assert(!cmdIdsAllocated.exists(_.id == cmd_id), s"$cmd_id already allocated")
      cmdIdsAllocated += AllocatedCmd(cmd_id, tag, 0)
    }

    val cmd_completed_fire = can_read_cmd && peek(c.io.cmd_completed.valid) != 0
    if (cmd_completed_fire) {
      val cmd_id = peek(c.io.cmd_completed.bits.cmd_id).toInt
      val cmd_buf_id = cmdIdsAllocated.zipWithIndex.collectFirst { case (AllocatedCmd(id, _, _), i) if id == cmd_id => i }.get
      val tag = peek(c.io.cmd_completed.bits.tag).toInt

      if (cmdIdsAllocated(cmd_buf_id).tag != tag) {
        println(s"wrong tag, $tag, ${cmdIdsAllocated(cmd_buf_id).tag}")
        max_cycles = 0
      }

      assert(cmdIdsAllocated(cmd_buf_id).tag == tag, "tag is incorrect")
      assert(cmdIdsAllocated(cmd_buf_id).requestsSent == c.nRequests, "returned after wrong number of requests returned")
      cmdIdsAllocated.remove(cmd_buf_id)
    }

    step(1)
    max_cycles -= 1
  }

  assert(max_cycles > 0, "reached max_cycles")
}

class DMACommandTrackerUnitTest extends ChiselFlatSpec {
  val testerArgs = Array(
    "--backend-name", "treadle",
    // "--generate-vcd-output", "on",
    "--target-dir", "test_run_dir/dmacommandtracker"
  )

  behavior of "DMACommandTracker"
  it should "work" in {
    chisel3.iotesters.Driver.execute(testerArgs, () => new DMAReadCommandTracker(2, 16, UInt(29.W))) {
      c => new DMACommandTrackerTester(c)
    } should be (true)
  }
}


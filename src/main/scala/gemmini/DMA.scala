package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.tilelink.TLBundleA
import testchipip.TLHelper
import freechips.rocketchip.rocket.MStatus
import freechips.rocketchip.rocket.constants.MemoryOpConstants

import Util._

class StreamReadRequest[U <: Data](spad_rows: Int, acc_rows: Int, mvin_scale_t_bits: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val spaddr = UInt(log2Up(spad_rows max acc_rows).W) // TODO use LocalAddr in DMA
  val is_acc = Bool()
  val accumulate = Bool()
  val has_acc_bitwidth = Bool()
  val scale = UInt(mvin_scale_t_bits.W)
  val status = new MStatus
  val len = UInt(16.W) // TODO magic number
  val repeats = UInt(16.W) // TODO magic number
  val block_stride = UInt(16.W) // TODO magic number
  val cmd_id = UInt(8.W) // TODO magic number

  // for conflict monitoring
  val monitor_conflict = Bool()
  val monitor_conflict_start = Bool()
  val monitor_conflict_end = Bool()
  val profile_conflict = Bool()
  val profile_conflict_start = Bool()
  val profile_conflict_end = Bool()

  override def cloneType: StreamReadRequest.this.type = new StreamReadRequest(spad_rows, acc_rows, mvin_scale_t_bits).asInstanceOf[this.type]
}

class StreamReadResponse[U <: Data](spadWidth: Int, accWidth: Int, spad_rows: Int, acc_rows: Int, aligned_to: Int, mvin_scale_t_bits: Int)
                        (implicit p: Parameters) extends CoreBundle {
  val data = UInt((spadWidth max accWidth).W)
  val addr = UInt(log2Up(spad_rows max acc_rows).W)
  val mask = Vec((spadWidth max accWidth) / (aligned_to * 8) max 1, Bool())
  val is_acc = Bool()
  val accumulate = Bool()
  val has_acc_bitwidth = Bool()
  val scale = UInt(mvin_scale_t_bits.W)
  val repeats = UInt(16.W) // TODO magic number
  val last = Bool()
  val bytes_read = UInt(8.W) // TODO magic number
  val cmd_id = UInt(8.W) // TODO magic number

  override def cloneType: StreamReadResponse.this.type = new StreamReadResponse(spadWidth, accWidth, spad_rows, acc_rows, aligned_to, mvin_scale_t_bits).asInstanceOf[this.type]
}

class StreamReader[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], nXacts: Int, beatBits: Int, maxBytes: Int, spadWidth: Int, accWidth: Int, aligned_to: Int,
                   spad_rows: Int, acc_rows: Int, meshRows: Int, use_tlb_register_filter: Boolean)
                  (implicit p: Parameters) extends LazyModule {
  val core = LazyModule(new StreamReaderCore(config, nXacts, beatBits, maxBytes, spadWidth, accWidth, aligned_to, spad_rows, acc_rows, meshRows, use_tlb_register_filter))
  val node = core.node

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows, acc_rows, config.mvin_scale_t_bits)))
      val resp = Decoupled(new StreamReadResponse(spadWidth, accWidth, spad_rows, acc_rows, aligned_to, config.mvin_scale_t_bits))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
      val flush = Input(Bool())

      //for monitoring conflicts, latency
      val latency_in = Input(UInt(16.W))
      val alert_cycles_in = Input(UInt(6.W))
      val latency_out = Output(UInt(16.W))
      val alert_cycles_out = Output(UInt(6.W))
      val pause_turn_in = Input(UInt(3.W))
      val pause_turn_out = Output(UInt(3.W))

      //for pausing monitoring
      val pause_out = Output(Bool())
    })
    io.latency_out := io.latency_in
    io.alert_cycles_out := io.alert_cycles_in
    io.pause_turn_out := io.pause_turn_in
    core.module.io.latency := io.latency_out
    core.module.io.alert_cycles := io.alert_cycles_out
    io.pause_out := core.module.io.pause
    core.module.io.pause_turn := io.pause_turn_out

    val nCmds = (nXacts / meshRows) + 1

    val xactTracker = Module(new XactTracker(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes, config.mvin_scale_t_bits, nCmds))

    val beatPacker = Module(new BeatMerger(beatBits, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes, aligned_to, meshRows, config.mvin_scale_t_bits, nCmds))

    core.module.io.req <> io.req
    io.tlb <> core.module.io.tlb
    io.busy := xactTracker.io.busy
    core.module.io.flush := io.flush

    xactTracker.io.alloc <> core.module.io.reserve
    xactTracker.io.peek.xactid := RegEnableThru(core.module.io.beatData.bits.xactid, beatPacker.io.req.fire())
    xactTracker.io.peek.pop := beatPacker.io.in.fire() && core.module.io.beatData.bits.last

    core.module.io.beatData.ready := beatPacker.io.in.ready
    beatPacker.io.req.valid := core.module.io.beatData.valid
    beatPacker.io.req.bits := xactTracker.io.peek.entry
    beatPacker.io.req.bits.lg_len_req := core.module.io.beatData.bits.lg_len_req
    beatPacker.io.in.valid := core.module.io.beatData.valid
    beatPacker.io.in.bits := core.module.io.beatData.bits.data

    beatPacker.io.out.ready := io.resp.ready
    io.resp.valid := beatPacker.io.out.valid
    io.resp.bits.data := beatPacker.io.out.bits.data
    io.resp.bits.addr := beatPacker.io.out.bits.addr
    io.resp.bits.mask := beatPacker.io.out.bits.mask
    io.resp.bits.is_acc := beatPacker.io.out.bits.is_acc
    io.resp.bits.accumulate := beatPacker.io.out.bits.accumulate
    io.resp.bits.has_acc_bitwidth := beatPacker.io.out.bits.has_acc_bitwidth
    io.resp.bits.scale := RegEnable(xactTracker.io.peek.entry.scale, beatPacker.io.req.fire())
    io.resp.bits.repeats := RegEnable(xactTracker.io.peek.entry.repeats, beatPacker.io.req.fire())
    io.resp.bits.cmd_id := RegEnable(xactTracker.io.peek.entry.cmd_id, beatPacker.io.req.fire())
    io.resp.bits.bytes_read := RegEnable(xactTracker.io.peek.entry.bytes_to_read, beatPacker.io.req.fire())
    io.resp.bits.last := beatPacker.io.out.bits.last
  }
}

class StreamReadBeat (val nXacts: Int, val beatBits: Int, val maxReqBytes: Int) extends Bundle {
  val xactid = UInt(log2Up(nXacts).W)
  val data = UInt(beatBits.W)
  val lg_len_req = UInt(log2Up(log2Up(maxReqBytes+1)+1).W)
  val last = Bool()
}

// TODO StreamReaderCore and StreamWriter are actually very alike. Is there some parent class they could both inherit from?
class StreamReaderCore[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], nXacts: Int, beatBits: Int, maxBytes: Int,
                                  spadWidth: Int, accWidth: Int, aligned_to: Int,
                                  spad_rows: Int, acc_rows: Int, meshRows: Int, use_tlb_register_filter: Boolean)
                                 (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-reader", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  // TODO when we request data from multiple rows which are actually contiguous in main memory, we should merge them into fewer requests

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)

    val spadWidthBytes = spadWidth / 8
    val accWidthBytes = accWidth / 8
    val beatBytes = beatBits / 8

    val nCmds = (nXacts / meshRows) + 1

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamReadRequest(spad_rows, acc_rows, config.mvin_scale_t_bits)))
      val reserve = new XactTrackerAllocIO(nXacts, maxBytes, spadWidth, accWidth, spad_rows, acc_rows, maxBytes, config.mvin_scale_t_bits, nCmds)
      val beatData = Decoupled(new StreamReadBeat(nXacts, beatBits, maxBytes))
      val tlb = new FrontendTLBIO
      val flush = Input(Bool())

      //for monitoring conflicts, latency
      val latency = Input(UInt(16.W))
      val alert_cycles = Input(UInt(6.W))
      val pause_turn = Input(UInt(3.W))
      val pause = Output(Bool())
    })

    val s_idle :: s_req_new_block :: Nil = Enum(2)
    val state = RegInit(s_idle)

    val req = Reg(new StreamReadRequest(spad_rows, acc_rows, config.mvin_scale_t_bits))
    val vaddr = req.vaddr

    val bytesRequested = Reg(UInt(log2Ceil(spadWidthBytes max accWidthBytes max maxBytes).W)) // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = Mux(req.has_acc_bitwidth, req.len * (config.accType.getWidth / 8).U, req.len * (config.inputType.getWidth / 8).U) - bytesRequested

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := state_machine_ready_for_req

    // Select the size and mask of the TileLink request
    class Packet extends Bundle {
      val size = UInt(log2Up(maxBytes+1).W)
      val lg_size = UInt(log2Ceil(log2Ceil(maxBytes+1)).W)
      val bytes_read = UInt(log2Up(maxBytes+1).W)
      val shift = UInt(log2Up(maxBytes).W)
      //val paddr = UInt(paddrBits.W)
      val vaddr = UInt(vaddrBits.W)

      //for bank conflict monitoring
      val monitor_conflict = Bool()
      val monitor_conflict_start = Bool()
      val monitor_conflict_end = Bool()
      val profile_conflict = Bool()
      val profile_conflict_start = Bool()
      val profile_conflict_end = Bool()
    }

    // TODO Can we filter out the larger read_sizes here if the systolic array is small, in the same way that we do so
    // for the write_sizes down below?
    val read_sizes = ((aligned_to max beatBytes) to maxBytes by aligned_to).
      filter(s => isPow2(s)).
      filter(s => s % beatBytes == 0)
    val read_packets = read_sizes.map { s =>
      val lg_s = log2Ceil(s)
      val vaddr_aligned_to_size = if (s == 1) vaddr else Cat(vaddr(vaddrBits-1, lg_s), 0.U(lg_s.W))
      val vaddr_offset = if (s > 1) vaddr(lg_s-1, 0) else 0.U

      val packet = Wire(new Packet())
      packet.size := s.U
      packet.lg_size := lg_s.U
      packet.bytes_read := minOf(s.U - vaddr_offset, bytesLeft)
      packet.shift := vaddr_offset
      packet.vaddr := vaddr_aligned_to_size

      //for bank conflict monitoring
      packet.monitor_conflict := req.monitor_conflict
      packet.monitor_conflict_start := req.monitor_conflict_start
      packet.monitor_conflict_end := req.monitor_conflict_end
      packet.profile_conflict := req.profile_conflict
      packet.profile_conflict_end := req.profile_conflict_end
      packet.profile_conflict_start := req.profile_conflict_start

      packet
    }
    val read_packet = read_packets.reduce { (acc, p) =>
      Mux(p.bytes_read > acc.bytes_read, p, acc)
    }
    val read_vaddr = read_packet.vaddr
    val read_lg_size = read_packet.lg_size
    val read_bytes_read = read_packet.bytes_read
    val read_shift = read_packet.shift
    //for bank conflict monitoring
    val read_monitor = read_packet.monitor_conflict
    val read_monitor_start = read_packet.monitor_conflict_start
    val read_monitor_end = read_packet.monitor_conflict_end
    val profile = read_packet.profile_conflict
    val profile_start = read_packet.profile_conflict_start
    val profile_end = read_packet.profile_conflict_end

    // Firing off TileLink read requests and allocating space inside the reservation buffer for them
    val get = edge.Get(
      fromSource = io.reserve.xactid,
      toAddress = 0.U, //read_paddr,
      lgSize = read_lg_size
    )._2

    class TLBundleAWithInfo extends Bundle {
      val tl_a = DataMirror.internal.chiselTypeClone[TLBundleA](tl.a.bits)
      val vaddr = Output(UInt(vaddrBits.W))
      val status = Output(new MStatus)

      //for bank conflict monitoring
      val monitor_conflict = Output(Bool())
      val monitor_conflict_start = Output(Bool())
      val monitor_conflict_end = Output(Bool())

      val profile_conflict = Output(Bool())
      val profile_conflict_start = Output(Bool())
      val profile_conflict_end = Output(Bool())
    }

    val untranslated_a = Wire(Decoupled(new TLBundleAWithInfo))
    untranslated_a.valid := state === s_req_new_block && io.reserve.ready
    untranslated_a.bits.tl_a := get
    untranslated_a.bits.vaddr := read_vaddr
    untranslated_a.bits.status := req.status
    //for bank conflict monitoring
    untranslated_a.bits.monitor_conflict := read_monitor
    untranslated_a.bits.monitor_conflict_start := read_monitor_start
    untranslated_a.bits.monitor_conflict_end := read_monitor_end
    untranslated_a.bits.profile_conflict := profile
    untranslated_a.bits.profile_conflict_end := profile_end
    untranslated_a.bits.profile_conflict_start := profile_start

    // 0 goes to retries, 1 goes to state machine
    val retry_a = Wire(Decoupled(new TLBundleAWithInfo))
    val tlb_arb = Module(new Arbiter(new TLBundleAWithInfo, 2))
    tlb_arb.io.in(0) <> retry_a
    tlb_arb.io.in(1) <> untranslated_a

    val tlb_q = Module(new Queue(new TLBundleAWithInfo, 1, pipe=true))
    tlb_q.io.enq <> tlb_arb.io.out

    io.tlb.req.valid := tlb_q.io.deq.valid
    io.tlb.req.bits.tlb_req.vaddr := tlb_q.io.deq.bits.vaddr
    io.tlb.req.bits.tlb_req.passthrough := false.B
    io.tlb.req.bits.tlb_req.size := 0.U // send_size
    io.tlb.req.bits.tlb_req.cmd := M_XRD
    io.tlb.req.bits.status := tlb_q.io.deq.bits.status

    val translate_q = Module(new Queue(new TLBundleAWithInfo, 1, pipe=true))
    translate_q.io.enq <> tlb_q.io.deq
    translate_q.io.deq.ready := true.B

    //retry_a.valid := translate_q.io.deq.valid && (io.tlb.resp.miss || !tl.a.ready)
    //retry_a.bits := translate_q.io.deq.bits
    //assert(retry_a.ready)

    /////////////////////////////////////////////////////////////////////////////////////////
    val conflict_detected = RegInit(false.B)
    val pause_turn = RegInit(io.pause_turn)
    retry_a.valid := translate_q.io.deq.valid && (io.tlb.resp.miss || !tl.a.ready || conflict_detected)
    retry_a.bits := translate_q.io.deq.bits
    assert(retry_a.ready)
    when(reset.toBool()){
      translate_q.io.enq.bits.profile_conflict := false.B
      translate_q.io.enq.bits.profile_conflict_start := false.B
      translate_q.io.enq.bits.profile_conflict_end := false.B
      translate_q.io.enq.bits.monitor_conflict := false.B
      translate_q.io.enq.bits.monitor_conflict_end := false.B
      translate_q.io.enq.bits.monitor_conflict_start := false.B
      retry_a.bits.profile_conflict := false.B
      retry_a.bits.profile_conflict_start := false.B
      retry_a.bits.profile_conflict_end := false.B
      retry_a.bits.monitor_conflict_end := false.B
      retry_a.bits.monitor_conflict_start := false.B
      retry_a.bits.monitor_conflict := false.B
      untranslated_a.bits.profile_conflict := false.B
      untranslated_a.bits.profile_conflict_start := false.B
      untranslated_a.bits.profile_conflict_end := false.B
      untranslated_a.bits.monitor_conflict_end := false.B
      untranslated_a.bits.monitor_conflict_start := false.B
      untranslated_a.bits.monitor_conflict := false.B
    }

    val tl_miss = tl.a.valid && !tl.a.ready
    val tl_profile = translate_q.io.deq.bits.profile_conflict
    val tl_profile_start = translate_q.io.deq.bits.profile_conflict_start && translate_q.io.deq.valid
    val tl_profile_end = translate_q.io.deq.bits.profile_conflict_end
    val (p_reset :: p_profile_start :: Nil) = Enum(2)
    val profile_miss_counter = RegInit(0.U(7.W))
    val p_state = RegInit(p_reset)
    val profile_number = RegInit(0.U(9.W))
    val profile_total = RegInit(0.U(12.W))
    val profile_max = RegInit(0.U(7.W))
    val profile_average = RegInit(0.U(7.W))
    // either average or max
    val profile_cycle = RegInit(profile_max)
    val profile_detected = RegInit(false.B)
    when(p_state === p_reset){
      when(tl_profile_start){
        p_state := p_profile_start
      }
    }
    when(p_state === p_profile_start){
      when(tl_miss && tl_profile){ // and here?
        when(profile_miss_counter === 5.U){ //only count those that are over 5 cycles (to avoid false detection)
          profile_number := profile_number + 1.U
          profile_detected := true.B
        }
        profile_miss_counter := profile_miss_counter + 1.U //which counter to use?
      }.otherwise{
        when(profile_detected){
          profile_total := profile_total + profile_miss_counter
          profile_max := Mux(profile_max < profile_miss_counter, profile_miss_counter, profile_max)//update to max value
          profile_detected := false.B
          when(profile_number === 64.U){
            profile_average := profile_total / 64.U
          }
        }
        profile_miss_counter := 0.U
      }
      when(tl_profile_end){
        profile_detected := false.B
        p_state := p_reset
        profile_miss_counter := 0.U
        when(profile_number === 64.U){
          profile_average := profile_total / 64.U
        }
        //profile_average := profile_total / profile_number // ToDo: need to change (don't use division)
        profile_cycle := Mux(io.pause_turn === 1.U, profile_average * 2.U, profile_max + 1.U) //parameterize what to select
      }
    }
    dontTouch(profile_miss_counter)
    dontTouch(profile_cycle)
    dontTouch(profile_average)
    dontTouch(profile_max)
    dontTouch(profile_detected)
    dontTouch(profile_total)
    dontTouch(profile_number)

    val tl_counter_trigger = tl_miss && translate_q.io.deq.bits.monitor_conflict
    val tl_miss_counter = RegInit(0.U(6.W))
    val alert_cycles = RegInit(profile_cycle)
    //val latency = RegInit(io.latency)
    val max_block_len = (maxBytes / (meshRows * spadWidth / 8)) max 1
    val expected_tl_req = (spad_rows / (2*2*max_block_len)).asUInt()
    val latency = Mux(translate_q.io.deq.bits.monitor_conflict && !translate_q.io.deq.bits.profile_conflict,
      Mux(io.latency === 0.U, expected_tl_req * profile_average, io.latency), 0.U) //if latency not give, use profiled one

    tl_miss_counter := satAdd(tl_miss_counter, 1.U, alert_cycles + 2.U, tl_counter_trigger)
    when(tl_miss_counter >= alert_cycles){ //reached limit
      conflict_detected := true.B
    }.elsewhen(!tl_counter_trigger){
      tl_miss_counter := 0.U
    }
    // pause monitoring detecting logic
    val (s_reset :: s_monitor_start :: s_conflict_detected  :: Nil) = Enum(3)
    val m_state = RegInit(s_reset)
    val pause_detect = RegInit(false.B)
    val pause_count = RegInit(0.U(2.W)) //Todo: parameterize it?
    //val pause_monitor_start = RegInit(0.U(6.W))
    io.pause := pause_detect
    when(translate_q.io.deq.bits.monitor_conflict && !translate_q.io.deq.bits.monitor_conflict_end){
      when(m_state === s_reset) {
        when(translate_q.io.deq.bits.monitor_conflict_start && translate_q.io.deq.valid){ // to avoid false detection
          m_state := s_monitor_start
          alert_cycles := Mux(io.alert_cycles === 0.U, profile_cycle, io.alert_cycles)
          //latency := io.latency //delared latency above
          pause_turn := io.pause_turn
        }
      }.elsewhen(m_state === s_monitor_start){
        when(tl_miss_counter >= alert_cycles){
          m_state := s_conflict_detected
        }
        //pause_monitor_start := 0.U
      }
    }.elsewhen(translate_q.io.deq.bits.monitor_conflict_end) {
      when(m_state === s_conflict_detected) {
        m_state := s_reset
        pause_count := 0.U
        pause_detect := false.B
      }.elsewhen(m_state === s_monitor_start) { // no detection during time window
        when(pause_count === pause_turn) { // pause monitoring
          pause_detect := true.B // on 3rd time (ToDo: parameterize this?)
          m_state := s_reset
          pause_count := 0.U // reset pause counter
        }.otherwise {
          pause_count := pause_count + 1.U
          m_state := s_reset
        }
      }
      //pause_monitor_start := 0.U
    } //ToDo: how to restart monitoring after pausing

    val tl_miss_timer = RegInit(0.U(16.W))
    tl_miss_timer := floorAdd(tl_miss_timer, 1.U, latency + 1.U, conflict_detected)
    when(tl_miss_timer === latency){ //resolve miss counter temporary
      tl_miss_counter := 0.U //reset miss counter
      conflict_detected := false.B
    }
    tl.a.valid   := translate_q.io.deq.valid && !io.tlb.resp.miss && !conflict_detected
    tl.a.bits   := translate_q.io.deq.bits.tl_a
    tl.a.bits.address := io.tlb.resp.paddr
    val cycles = freechips.rocketchip.util.WideCounter(32)
    when(tl.a.fire()){
      printf("GEMMINI_MEM %x %x %x %x\n", cycles.value, p(freechips.rocketchip.tile.TileKey).hartId.U, tl.a.bits.address, tl.a.bits.size)
      //printf(midas.targetutils.SynthesizePrintf("GEMMINI_MEM: %x %x %x\n", p(freechips.rocketchip.tile.TileKey).hartId.U, tl.a.bits.address, tl.a.bits.size))
    }
    when(tl_miss){
      printf("GEMMINI_BLOCK %x %x\n", cycles.value, p(freechips.rocketchip.tile.TileKey).hartId.U)
      //printf(midas.targetutils.SynthesizePrintf("GEMMINI_BLOCK: %x %x \n", p(freechips.rocketchip.tile.TileKey).hartId.U, tl.a.bits.address))

    }
    /////////////////////////////////////////////////////////////////////////////////////////

    //tl.a.valid := translate_q.io.deq.valid && !io.tlb.resp.miss
    //tl.a.bits := translate_q.io.deq.bits.tl_a
    //tl.a.bits.address := io.tlb.resp.paddr

    io.reserve.valid := state === s_req_new_block && untranslated_a.ready // TODO decouple "reserve.valid" from "tl.a.ready"
    io.reserve.entry.shift := read_shift
    io.reserve.entry.is_acc := req.is_acc
    io.reserve.entry.accumulate := req.accumulate
    io.reserve.entry.has_acc_bitwidth := req.has_acc_bitwidth
    io.reserve.entry.scale := req.scale
    io.reserve.entry.repeats := req.repeats
    io.reserve.entry.block_stride := req.block_stride
    io.reserve.entry.lg_len_req := DontCare // TODO just remove this from the IO completely
    io.reserve.entry.bytes_to_read := read_bytes_read
    io.reserve.entry.cmd_id := req.cmd_id

    io.reserve.entry.addr := req.spaddr + req.block_stride *
      Mux(req.has_acc_bitwidth,
        // We only add "if" statements here to satisfy the Verilator linter. The code would be cleaner without the
        // "if" condition and the "else" clause
        if (bytesRequested.getWidth >= log2Up(accWidthBytes+1)) bytesRequested / accWidthBytes.U else 0.U,
        if (bytesRequested.getWidth >= log2Up(spadWidthBytes+1)) bytesRequested / spadWidthBytes.U else 0.U)
    io.reserve.entry.spad_row_offset := Mux(req.has_acc_bitwidth, bytesRequested % accWidthBytes.U, bytesRequested % spadWidthBytes.U)

    when (untranslated_a.fire()) {
      val next_vaddr = req.vaddr + read_bytes_read // send_size
      val new_page = next_vaddr(pgIdxBits-1, 0) === 0.U
      req.vaddr := next_vaddr

      bytesRequested := bytesRequested + read_bytes_read // send_size

      // when (send_size >= bytesLeft) {
      when (read_bytes_read >= bytesLeft) {
        // We're done with this request at this point
        state_machine_ready_for_req := true.B
        state := s_idle
      }
    }

    // Forward TileLink read responses to the reservation buffer
    tl.d.ready := io.beatData.ready
    io.beatData.valid := tl.d.valid
    io.beatData.bits.xactid := tl.d.bits.source
    io.beatData.bits.data := tl.d.bits.data
    io.beatData.bits.lg_len_req := tl.d.bits.size
    io.beatData.bits.last := edge.last(tl.d)
    // TODO the size data is already returned from TileLink, so there's no need for us to store it in the XactTracker ourselves

    // Accepting requests to kick-start the state machine
    when (io.req.fire()) {
      req := io.req.bits
      bytesRequested := 0.U

      state := s_req_new_block
    }
  }
}

class StreamWriteRequest(val dataWidth: Int, val maxBytes: Int)(implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val data = UInt(dataWidth.W)
  val len = UInt(log2Up((dataWidth/8 max maxBytes)+1).W) // The number of bytes to write
  val block = UInt(8.W) // TODO magic number
  val status = new MStatus

  // Pooling variables
  val pool_en = Bool()
  val store_en = Bool()
}

class StreamWriter[T <: Data: Arithmetic](nXacts: Int, beatBits: Int, maxBytes: Int, dataWidth: Int, aligned_to: Int,
                                          inputType: T, block_cols: Int, use_tlb_register_filter: Boolean)
                  (implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "stream-writer", sourceId = IdRange(0, nXacts))

  require(isPow2(aligned_to))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (tl, edge) = node.out(0)
    val dataBytes = dataWidth / 8
    val beatBytes = beatBits / 8
    val lgBeatBytes = log2Ceil(beatBytes)
    val maxBeatsPerReq = maxBytes / beatBytes
    val inputTypeRowBytes = block_cols * inputType.getWidth / 8
    val maxBlocks = maxBytes / inputTypeRowBytes

    require(beatBytes > 0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new StreamWriteRequest(dataWidth, maxBytes)))
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
      val flush = Input(Bool())
    })

    val (s_idle :: s_writing_new_block :: s_writing_beats :: Nil) = Enum(3)
    val state = RegInit(s_idle)

    val req = Reg(new StreamWriteRequest(dataWidth, maxBytes))

    // TODO use the same register to hold data_blocks and data_single_block, so that this Mux here is not necessary
    val data_blocks = Reg(Vec(maxBlocks, UInt((inputTypeRowBytes * 8).W)))
    val data_single_block = Reg(UInt(dataWidth.W)) // For data that's just one-block-wide
    val data = Mux(req.block === 0.U, data_single_block, data_blocks.asUInt())

    val bytesSent = Reg(UInt(log2Ceil((dataBytes max maxBytes)+1).W))  // TODO this only needs to count up to (dataBytes/aligned_to), right?
    val bytesLeft = req.len - bytesSent

    val xactBusy = RegInit(0.U(nXacts.W))
    val xactOnehot = PriorityEncoderOH(~xactBusy)
    val xactId = OHToUInt(xactOnehot)

    val xactBusy_fire = WireInit(false.B)
    val xactBusy_add = Mux(xactBusy_fire, (1.U << xactId).asUInt(), 0.U)
    val xactBusy_remove = ~Mux(tl.d.fire(), (1.U << tl.d.bits.source).asUInt(), 0.U)
    xactBusy := (xactBusy | xactBusy_add) & xactBusy_remove.asUInt()

    val state_machine_ready_for_req = WireInit(state === s_idle)
    io.req.ready := state_machine_ready_for_req
    io.busy := xactBusy.orR || (state =/= s_idle)

    val vaddr = req.vaddr

    // Select the size and mask of the TileLink request
    class Packet extends Bundle {
      val size = UInt(log2Ceil(maxBytes+1).W)
      val lg_size = UInt(log2Ceil(log2Ceil(maxBytes+1)+1).W)
      val mask = Vec(maxBeatsPerReq, Vec(beatBytes, Bool()))
      val vaddr = UInt(vaddrBits.W)
      val is_full = Bool()

      val bytes_written = UInt(log2Up(maxBytes+1).W)
      val bytes_written_per_beat = Vec(maxBeatsPerReq, UInt(log2Up(beatBytes+1).W))

      def total_beats(dummy: Int = 0) = Mux(size < beatBytes.U, 1.U, size / beatBytes.U)
    }

    val smallest_write_size = aligned_to max beatBytes
    val write_sizes = (smallest_write_size to maxBytes by aligned_to).
      filter(s => isPow2(s)).
      filter(s => s % beatBytes == 0) /*.
      filter(s => s <= dataBytes*2 || s == smallest_write_size)*/
    val write_packets = write_sizes.map { s =>
      val lg_s = log2Ceil(s)
      val vaddr_aligned_to_size = if (s == 1) vaddr else Cat(vaddr(vaddrBits-1, lg_s), 0.U(lg_s.W))
      val vaddr_offset = if (s > 1) vaddr(lg_s - 1, 0) else 0.U

      val mask = (0 until maxBytes).map { i => i.U >= vaddr_offset && i.U < vaddr_offset +& bytesLeft && (i < s).B }

      val bytes_written = {
        Mux(vaddr_offset +& bytesLeft > s.U, s.U - vaddr_offset, bytesLeft)
      }

      val packet = Wire(new Packet())
      packet.size := s.U
      packet.lg_size := lg_s.U
      packet.mask := VecInit(mask.grouped(beatBytes).map(v => VecInit(v)).toSeq)
      packet.vaddr := vaddr_aligned_to_size
      packet.is_full := mask.take(s).reduce(_ && _)

      packet.bytes_written := bytes_written
      packet.bytes_written_per_beat.zipWithIndex.foreach { case (b, i) =>
        val start_of_beat = i * beatBytes
        val end_of_beat = (i+1) * beatBytes

        val left_shift = Mux(vaddr_offset >= start_of_beat.U && vaddr_offset < end_of_beat.U,
          vaddr_offset - start_of_beat.U,
          0.U)

        val right_shift = Mux(vaddr_offset +& bytesLeft >= start_of_beat.U && vaddr_offset +& bytesLeft < end_of_beat.U,
          end_of_beat.U - (vaddr_offset +& bytesLeft),
          0.U)

        val too_early = vaddr_offset >= end_of_beat.U
        val too_late = vaddr_offset +& bytesLeft <= start_of_beat.U

        b := Mux(too_early || too_late, 0.U, beatBytes.U - (left_shift +& right_shift))
      }

      packet
    }
    val best_write_packet = write_packets.reduce { (acc, p) =>
      Mux(p.bytes_written > acc.bytes_written, p, acc)
    }
    val write_packet = RegEnableThru(best_write_packet, state === s_writing_new_block)

    for (wp <- write_packets)
      dontTouch(wp)

    val write_size = write_packet.size
    val lg_write_size = write_packet.lg_size
    val write_beats = write_packet.total_beats()
    val write_vaddr = write_packet.vaddr
    val write_full = write_packet.is_full

    val beatsLeft = Reg(UInt(log2Up(maxBytes/aligned_to).W))
    val beatsSent = Mux(state === s_writing_new_block, 0.U, write_beats - beatsLeft)

    val write_mask = write_packet.mask(beatsSent)
    val write_shift = PriorityEncoder(write_mask)

    val bytes_written_this_beat = write_packet.bytes_written_per_beat(beatsSent)

    // Firing off TileLink write requests
    val putFull = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = 0.U,
      lgSize = lg_write_size,
      data = (data >> (bytesSent * 8.U)).asUInt()
    )._2

    val putPartial = edge.Put(
      fromSource = RegEnableThru(xactId, state === s_writing_new_block),
      toAddress = 0.U,
      lgSize = lg_write_size,
      data = ((data >> (bytesSent * 8.U)) << (write_shift * 8.U)).asUInt(),
      mask = write_mask.asUInt()
    )._2

    class TLBundleAWithInfo extends Bundle {
      val tl_a = DataMirror.internal.chiselTypeClone[TLBundleA](tl.a.bits)
      val vaddr = Output(UInt(vaddrBits.W))
      val status = Output(new MStatus)
    }

    val untranslated_a = Wire(Decoupled(new TLBundleAWithInfo))
    xactBusy_fire := untranslated_a.fire() && state === s_writing_new_block
    untranslated_a.valid := (state === s_writing_new_block || state === s_writing_beats) && !xactBusy.andR()
    untranslated_a.bits.tl_a := Mux(write_full, putFull, putPartial)
    untranslated_a.bits.vaddr := write_vaddr
    untranslated_a.bits.status := req.status

    // 0 goes to retries, 1 goes to state machine
    val retry_a = Wire(Decoupled(new TLBundleAWithInfo))
    val shadow_retry_a = Module(new Queue(new TLBundleAWithInfo, 1))
    shadow_retry_a.io.enq.valid := false.B
    shadow_retry_a.io.enq.bits := DontCare
    val tlb_arb = Module(new Arbiter(new TLBundleAWithInfo, 3))
    tlb_arb.io.in(0) <> retry_a
    tlb_arb.io.in(1) <> shadow_retry_a.io.deq
    tlb_arb.io.in(2) <> untranslated_a

    val tlb_q = Module(new Queue(new TLBundleAWithInfo, 1, pipe=true))
    tlb_q.io.enq <> tlb_arb.io.out

    io.tlb.req.valid := tlb_q.io.deq.fire()
    io.tlb.req.bits.tlb_req.vaddr := tlb_q.io.deq.bits.vaddr
    io.tlb.req.bits.tlb_req.passthrough := false.B
    io.tlb.req.bits.tlb_req.size := 0.U // send_size
    io.tlb.req.bits.tlb_req.cmd := M_XWR
    io.tlb.req.bits.status := tlb_q.io.deq.bits.status

    val translate_q = Module(new Queue(new TLBundleAWithInfo, 1, pipe=true))
    translate_q.io.enq <> tlb_q.io.deq
    when (retry_a.valid) {
      translate_q.io.enq.valid := false.B
      shadow_retry_a.io.enq.valid := tlb_q.io.deq.valid
      shadow_retry_a.io.enq.bits  := tlb_q.io.deq.bits
    }
    translate_q.io.deq.ready := tl.a.ready || io.tlb.resp.miss

    retry_a.valid := translate_q.io.deq.valid && io.tlb.resp.miss
    retry_a.bits := translate_q.io.deq.bits
    assert(!(retry_a.valid && !retry_a.ready))

    tl.a.valid := translate_q.io.deq.valid && !io.tlb.resp.miss
    tl.a.bits := translate_q.io.deq.bits.tl_a
    tl.a.bits.address := RegEnableThru(io.tlb.resp.paddr, RegNext(io.tlb.req.fire()))

    tl.d.ready := xactBusy.orR()

    when (untranslated_a.fire()) {
      when (state === s_writing_new_block) {
        beatsLeft := write_beats - 1.U

        val next_vaddr = req.vaddr + write_packet.bytes_written
        req.vaddr := next_vaddr

        bytesSent := bytesSent + bytes_written_this_beat

        when (write_beats === 1.U) {
          when (bytes_written_this_beat >= bytesLeft) {
            // We're done with this request at this point
            state_machine_ready_for_req := true.B
            state := s_idle
          }
        }.otherwise {
          state := s_writing_beats
        }
      }.elsewhen(state === s_writing_beats) {
        beatsLeft := beatsLeft - 1.U
        bytesSent := bytesSent + bytes_written_this_beat

        assert(beatsLeft > 0.U)

        when (beatsLeft === 1.U) {
          when (bytes_written_this_beat >= bytesLeft) {
            // We're done with this request at this point
            state_machine_ready_for_req := true.B
            state := s_idle
          }.otherwise {
            state := s_writing_new_block
          }
        }
      }
    }

    // Accepting requests to kick-start the state machine
    when (io.req.fire()) {
      val pooled = {
        val cols = dataWidth / inputType.getWidth
        val v1 = io.req.bits.data.asTypeOf(Vec(cols, inputType))
        val v2 = data_single_block.asTypeOf(Vec(cols, inputType))
        val m = v1.zip(v2)
        VecInit(m.zipWithIndex.map{case ((x, y), i) => if (i < block_cols) maxOf(x, y) else y}).asUInt()
      }

      req := io.req.bits
      req.len := io.req.bits.block * inputTypeRowBytes.U + io.req.bits.len

      data_single_block := Mux(io.req.bits.pool_en, pooled, io.req.bits.data)
      data_blocks(io.req.bits.block) := io.req.bits.data

      bytesSent := 0.U

      state := Mux(io.req.bits.store_en, s_writing_new_block, s_idle)

      assert(io.req.bits.len <= (block_cols * inputType.getWidth / 8).U || io.req.bits.block === 0.U, "DMA can't write multiple blocks to main memory when writing full accumulator output")
      assert(!io.req.bits.pool_en || io.req.bits.block === 0.U, "Can't pool with block-mvout")
    }
 }
}

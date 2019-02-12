package systolic

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

class SystolicArray(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new SystolicArrayModule(this)
  override val atlNode = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("SystolicArrayRoCC")))))
}

class SystolicArrayModule(outer: SystolicArray,val width: Int, val tileRows: Int, val tileColumns: Int, val meshRows: Int, val meshColumns: Int,
                          val sramEntries: Int, val queue_length: Int) extends LazyRoCCModule(outer) {
  val cmd = Queue(io.cmd, queue_length)
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2
  // dma controller module
  val dma_ctrl = Module(new DmaController)
  //aliases of cmd
  val rs1 = cmd.bits.rs1
  val rs2 = cmd.bits.rs2
  val rd = cmd.bits.rd
  val funct = cmd.bits.inst.funct
  val opcode = cmd.bits.inst.opcode
  val DoLoad = funct === UInt(2)
  val DoStore = funct === UInt(3)
  val DoComputeAndWrite = funct === UInt(4)
  val DoComputeOnly = funct === UInt(5)


  //regs for data coming
  //val input_data_from_dma = RegInit(0.U((width*tileColumns*meshColumns*2).W))

  // STATE defines
  val idle :: start_load :: Nil = Enum(2)
  val DRAM_to_SRAM_state = RegInit(idle)
  val idle :: start_store :: Nil = Enum(2)
  val SRAM_to_DRAM_state = RegInit(idle)
  val compute_state = RegInit(idle)

  //SRAM scratchpad
  val Sram_width:Int = 32
  val addr = Wire(UInt(Sram_width.W))
  //val dataIn = Wire(UInt(Sram_width.W))
  //val dataOut = Wire(UInt(Sram_width.W))
  val enable = Wire(Bool())
  val mem = SyncReadMem(sramEntries, UInt(Sram_width.W))
  mesh.io.a.valid := false.B
  mesh.io.b.valid := false.B
  mesh.io.d.valid := false.B
  mesh.io.out.ready := false.B

  switch(compute_state){
    is(idle){
      when((DoComputeAndWrite||DoComputeOnly)  && cmd.valid){
        //feed first row of a b
        compute_state := feed_data
      }

    }
    is(feed_data){
      // feed a and b
      when (mesh.io.a.ready &&mesh.io.b.ready &&mesh.io.d.ready){
        mesh.io.a.valid := true.B
        mesh.io.a.bits := data(address) // add adress here
        mesh.io.b.valid := true.B
        mesh.io.b.bits := data(address) // add address here
        mesh.io.d.valid := true.B
        mesh.io.d.bits := 0.U
      }

      when(end){
        when(DoComputeOnly){
          compute_state := idle

        }.elsewhen(DoComputeAndWrite){
          compute_state := write_output
          mesh.io.out.ready := true.B

        }
      }
    }
    is(write_output){
      mesh.io.out.ready := true.B
      when(mesh.io.out.valid){
        mem.write(addr,mesh.io.out.bits)
      }
    }
  }

  cmd.deq.ready := false.B
  dma_ctrl.req.valid := false.B

  switch(DRAM_to_SRAM_state){
    is(idle) {
      when (DoLoad && cmd.valid && dma_ctrl.ready){
        dma_ctrl.req.valid := true.B
        dma_ctrl.req.bits := DmaRequest(
          cmd = DmaRequest.DMA_CMD_PFR,
          source = rs1.U,
          length = (width*tileColumns*meshColumns*2).U)
        DRAM_to_SRAM_state := start_load
      }
    }
    is(start_load){
      when(dma_ctrl.resp.valid){
        //input_data_from_dma := dma_ctrl.resp.bits
        mem.write(addr, dma_ctrl.resp.bits)
        cmd.deq.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }

  switch(SRAM_to_DRAM_state){
    is(idle) {
      when (DoStore && cmd.valid && dma_ctrl.ready){
        dma_ctrl.req.valid := true.B
        dma_ctrl.req.bits := DmaRequest(
          cmd = DmaRequest.DMA_CMD_PFR, //fix to make it read
          source = rs1.U,
          data = mem.read(readAddr),
          length = (width*tileColumns*meshColumns*2).U)
        DRAM_to_SRAM_state := start_store
      }
    }
    is(start_store){
      when(dma_ctrl.resp.valid){
        cmd.deq.ready := true.B
        DRAM_to_SRAM_state := idle
      }
    }
  }

  import chisel3._
  import chisel3.util._
  import freechips.rocketchip.config.{Parameters, Field}
  import freechips.rocketchip.coreplex.CacheBlockBytes
  import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
  import freechips.rocketchip.rocket.PAddrBits
  import freechips.rocketchip.tilelink._
  import freechips.rocketchip.util._

  case class DmaConfig(
                        nTrackers: Int = 1,
                        nDmaXacts: Int = 4,
                        nMemXacts: Int = 4,
                        pipelineDepth: Int = 16)

  case object DmaKey extends Field[DmaConfig]

  trait HasDmaParameters {
    implicit val p: Parameters
    val dmaExternal = p(DmaKey)
    val nDmaTrackers = dmaExternal.nTrackers
    val nDmaXacts = dmaExternal.nDmaXacts
    val nDmaTrackerMemXacts = dmaExternal.nMemXacts
    val dmaXactIdBits = log2Up(nDmaXacts)
    val pipelineDepth = dmaExternal.pipelineDepth
    val pipelineIdxBits = log2Up(pipelineDepth)
    val pipelineCountBits = log2Up(pipelineDepth+1)
    val dmaStatusBits = 3
    val addrBits = p(PAddrBits)
    val blockBytes = p(CacheBlockBytes)
    val blockOffset = log2Ceil(blockBytes)
    val blockAddrBits = addrBits - blockOffset
  }

  trait DmaTrackerUtils extends HasDmaParameters {
    val edge: TLEdge

    lazy val dataBits = edge.bundle.dataBits
    lazy val dataBytes = dataBits / 8
    lazy val dataBeats = blockBytes / dataBytes
    lazy val beatAddrBits = log2Ceil(dataBeats)
    lazy val byteAddrBits = log2Ceil(dataBytes)

    def incWrap(cur: UInt, inc: UInt): UInt = {
      val unwrapped = cur +& inc
      Mux(unwrapped >= pipelineDepth.U, unwrapped - pipelineDepth.U, unwrapped)
    }
  }

  abstract class DmaModule(implicit val p: Parameters) extends Module with HasDmaParameters
  abstract class DmaBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with HasDmaParameters

  class DmaRequest(implicit p: Parameters) extends DmaBundle()(p) {
    val xact_id = UInt(dmaXactIdBits.W)
    val cmd = UInt(DmaRequest.DMA_CMD_SZ.W)
    val source = UInt(addrBits.W)
    val dest = UInt(addrBits.W)
    val length = UInt(addrBits.W)
    val alloc = UInt(2.W)

    def isPrefetch(dummy: Int = 0): Bool =
      cmd === DmaRequest.DMA_CMD_PFR || cmd === DmaRequest.DMA_CMD_PFW
  }

  class DmaResponse(implicit p: Parameters) extends DmaBundle()(p) {
    val xact_id = UInt(dmaXactIdBits.W)
    val status = UInt(dmaStatusBits.W)
  }

  object DmaRequest {
    val DMA_CMD_SZ = 2

    val DMA_CMD_COPY = "b00".U
    val DMA_CMD_PFR  = "b10".U
    val DMA_CMD_PFW  = "b11".U

    def apply(xact_id: UInt = 0.U,
              cmd: UInt,
              source: UInt,
              dest: UInt,
              length: UInt,
              alloc: UInt = "b10".U)(implicit p: Parameters): DmaRequest = {
      val req = Wire(new DmaRequest)
      req.xact_id := xact_id
      req.cmd := cmd
      req.source := source
      req.dest := dest
      req.length := length
      req.alloc := alloc
      req
    }
  }
  import DmaRequest._

  class DmaIO(implicit p: Parameters) extends DmaBundle()(p) {
    val req = Decoupled(new DmaRequest)
    val resp = Flipped(Decoupled(new DmaResponse))
  }

  class PipelinePacket(dataBits: Int)(implicit p: Parameters) extends DmaBundle {
    val data = UInt(dataBits.W)
    val bytes = UInt(log2Up(dataBits/8).W)

    override def cloneType =
      new PipelinePacket(dataBits)(p).asInstanceOf[this.type]
  }

  class ReservationRequest extends Bundle {
    val multibeat = Bool()
  }

  class ReservationResponse(implicit val p: Parameters)
    extends ParameterizedBundle
      with HasDmaParameters {
    val idx = UInt(pipelineIdxBits.W)
  }

  class ReservationData(dataBits: Int)(implicit p: Parameters)
    extends DmaBundle {
    val data = UInt(dataBits.W)
    val bytes = UInt(log2Ceil(dataBits/8).W)
    val idx = UInt(pipelineIdxBits.W)

    override def cloneType =
      new ReservationData(dataBits)(p).asInstanceOf[this.type]
  }

  class ReservationInputIO(dataBits: Int)(implicit p: Parameters)
    extends Bundle {
    val req = Decoupled(new ReservationRequest)
    val resp = Flipped(Decoupled(new ReservationResponse))
    val data = Decoupled(new ReservationData(dataBits))

    override def cloneType =
      new ReservationInputIO(dataBits)(p).asInstanceOf[this.type]
  }

  class ReservationOutputIO(dataBits: Int)(implicit p: Parameters)
    extends DmaBundle {
    val count = Output(UInt(pipelineCountBits.W))
    val data = Decoupled(new PipelinePacket(dataBits))

    override def cloneType =
      new ReservationOutputIO(dataBits)(p).asInstanceOf[this.type]
  }

  class ReservationQueue(val edge: TLEdge)(implicit val p: Parameters)
    extends Module with DmaTrackerUtils {

    val io = IO(new Bundle {
      val in = Flipped(new ReservationInputIO(dataBits))
      val out = new ReservationOutputIO(dataBits)
    })

    val req = Queue(io.in.req, 1)

    val pkt_buffer = Mem(pipelineDepth, new PipelinePacket(dataBits))
    val pkt_valid = Reg(init = 0.U(pipelineDepth.W))

    val head = Reg(init = 0.U(pipelineIdxBits.W))
    val tail = Reg(init = 0.U(pipelineIdxBits.W))
    val count = Reg(init = 0.U(pipelineCountBits.W))

    val req_count = Mux(req.bits.multibeat, dataBeats.U, 1.U)
    count := count + Mux(req.fire(), req_count, 0.U) - io.out.data.fire()

    val full = (count + req_count) > pipelineDepth.U

    req.ready := io.in.resp.ready && !full
    io.in.resp.valid := req.valid && !full
    io.in.resp.bits.idx := tail

    io.in.data.ready := true.B
    io.out.data.valid := (pkt_valid >> head)(0)
    io.out.data.bits := pkt_buffer(head)
    io.out.count := PopCount(pkt_valid)

    when (req.fire()) {
      tail := incWrap(tail, req_count)
    }

    when (io.in.data.fire()) {
      val pkt = Wire(new PipelinePacket(dataBits))
      pkt.data := io.in.data.bits.data
      pkt.bytes := io.in.data.bits.bytes
      pkt_buffer(io.in.data.bits.idx) := pkt
    }

    when (io.out.data.fire()) {
      head := incWrap(head, 1.U)
    }

    pkt_valid := (pkt_valid &
      ~Mux(io.out.data.fire(), UIntToOH(head), 0.U)) |
      Mux(io.in.data.fire(), UIntToOH(io.in.data.bits.idx), 0.U)
  }


  class DmaTrackerReader(id: Int)(implicit p: Parameters)
    extends LazyModule with HasDmaParameters {
    val node = TLClientNode(TLClientParameters(
      name = s"dma-reader${id}",
      sourceId = IdRange(0, nDmaTrackerMemXacts)))

    lazy val module = new DmaTrackerReaderModule(this)
  }

  class DmaTrackerReaderModule(outer: DmaTrackerReader)
    extends LazyModuleImp(outer)
      with DmaTrackerUtils {

    val edge = outer.node.edgesOut(0)
    val io = IO(new Bundle {
      val dma_req = Flipped(Decoupled(new DmaRequest))
      val mem = outer.node.bundleOut
      val res = new ReservationInputIO(dataBits)
    })
    val tl = io.mem(0)

    val src_addr = Reg(UInt(addrBits.W))
    val src_block = src_addr(addrBits - 1, blockOffset)
    val src_beat = src_addr(blockOffset - 1, byteAddrBits)
    val src_byte_off = src_addr(byteAddrBits - 1, 0)
    val bytes_left = Reg(UInt(addrBits.W))

    val s_idle :: s_reserve :: s_mem_req :: Nil = Enum(3)
    val state = Reg(init = s_idle)

    val get_busy = Reg(init = 0.U(nDmaTrackerMemXacts.W))
    val byte_offset = Mem(nDmaTrackerMemXacts, UInt(byteAddrBits.W))
    val bytes_valid = Mem(nDmaTrackerMemXacts, UInt(byteAddrBits.W))
    val get_id_onehot = PriorityEncoderOH(~get_busy)
    val get_id = Reg(UInt(log2Up(nDmaTrackerMemXacts).W))
    val data_index = Reg(Vec(nDmaTrackerMemXacts, UInt(log2Up(pipelineDepth).W)))

    val alloc = Reg(Bool())
    val send_block =
      src_beat === 0.U && src_byte_off === 0.U &&
        bytes_left >= blockBytes.U

    io.dma_req.ready := (state === s_idle)

    when (io.dma_req.fire()) {
      src_addr := io.dma_req.bits.source
      bytes_left := io.dma_req.bits.length
      alloc := io.dma_req.bits.alloc(0)
      state := s_reserve
    }

    when (io.res.req.fire()) {
      get_id := OHToUInt(get_id_onehot)
      state := s_mem_req
    }

    when (io.res.resp.fire()) {
      data_index(get_id) := io.res.resp.bits.idx
    }

    when (tl.a.fire()) {
      val bytes_to_read =
        Mux(send_block, blockBytes.U, dataBytes.U - src_byte_off)

      src_addr := src_addr + bytes_to_read
      byte_offset(get_id) := src_byte_off
      bytes_valid(get_id) := Mux(bytes_to_read > dataBytes.U, 0.U,
        Mux(bytes_to_read < bytes_left, bytes_to_read, bytes_left))

      when (bytes_left > bytes_to_read) {
        bytes_left := bytes_left - bytes_to_read
        state := s_reserve
      } .otherwise {
        bytes_left := 0.U
        state := s_idle
      }
    }

    io.res.req.valid := (state === s_reserve) && !get_busy.andR
    io.res.req.bits.multibeat := send_block

    io.res.resp.ready := (state === s_mem_req) && tl.a.ready
    tl.a.valid := (state === s_mem_req) && io.res.resp.valid
    tl.a.bits := edge.Get(
      fromSource = get_id,
      toAddress = Cat(src_block,
        Mux(send_block, 0.U(beatAddrBits.W), src_beat),
        0.U(byteAddrBits.W)),
      lgSize = Mux(send_block, log2Ceil(blockBytes).U, byteAddrBits.U))._2

    val grant_id = tl.d.bits.source

    get_busy := (get_busy | Mux(io.res.req.fire(), get_id_onehot, 0.U)) &
      ~Mux(tl.d.fire() && edge.last(tl.d),
        UIntToOH(grant_id), 0.U)

    when (io.res.data.fire()) {
      data_index(grant_id) := incWrap(data_index(grant_id), 1.U)
    }

    tl.d.ready := io.res.data.ready
    io.res.data.valid := tl.d.valid
    io.res.data.bits.idx := data_index(grant_id)
    io.res.data.bits.data := tl.d.bits.data >> Cat(byte_offset(grant_id), 0.U(3.W))
    io.res.data.bits.bytes := bytes_valid(grant_id) - 1.U
  }

  class DmaTrackerWriter(id: Int)(implicit p: Parameters)
    extends LazyModule with HasDmaParameters {
    val node = TLClientNode(TLClientParameters(
      name = s"dma-writer${id}",
      sourceId = IdRange(0, nDmaTrackerMemXacts)))

    lazy val module = new DmaTrackerWriterModule(this)
  }

  class DmaTrackerWriterModule(outer: DmaTrackerWriter)
    extends LazyModuleImp(outer)
      with DmaTrackerUtils {

    val edge = outer.node.edgesOut(0)
    val io = IO(new Bundle {
      val dma = Flipped(new DmaIO)
      val mem = outer.node.bundleOut
      val pipe = Flipped(new ReservationOutputIO(dataBits))
    })
    val tl = io.mem(0)

    val dst_addr = Reg(UInt(addrBits.W))
    val dst_block = dst_addr(addrBits - 1, blockOffset)
    val dst_beat = dst_addr(blockOffset - 1, byteAddrBits)
    val dst_byte_off = dst_addr(byteAddrBits - 1, 0)
    val bytes_left = Reg(UInt(addrBits.W))

    val dma_req_id = Reg(io.dma.req.bits.xact_id.cloneType)

    val s_idle :: s_mem_req :: s_resp :: Nil = Enum(3)
    val state = Reg(init = s_idle)

    val last_data = Reg(UInt(dataBits.W))
    val last_bytes_val = Reg(UInt((log2Up(dataBytes) + 1).W))

    val put_busy = Reg(UInt(nDmaTrackerMemXacts.W), init = 0.U)
    val put_id_onehot = PriorityEncoderOH(~put_busy)
    val put_id = OHToUInt(put_id_onehot)
    val put_block_id = RegEnable(put_id, tl.a.fire() && edge.first(tl.a))

    val data = last_data | Mux(io.pipe.data.valid,
      (io.pipe.data.bits.data << Cat(last_bytes_val, 0.U(3.W))), 0.U)
    val bytes_val = Mux(io.pipe.data.valid,
      last_bytes_val + io.pipe.data.bits.bytes + 1.U,
      last_bytes_val)

    val off_size = dataBytes.U - dst_byte_off
    val needs_more = (bytes_val < off_size) && (bytes_val < bytes_left)
    val flush_buffer = (last_bytes_val >= bytes_left)

    val bytes_to_send = Mux(bytes_val < off_size, bytes_val, off_size)
    val shift_data = (data << Cat(dst_byte_off, 0.U(3.W)))(dataBits-1, 0)
    val write_mask = (((1.U << bytes_to_send) - 1.U) << dst_byte_off)(dataBytes-1, 0)

    val send_block = Reg(init = false.B)
    val alloc = Reg(Bool())
    val block_acquire = send_block && (io.pipe.count < (dataBeats.U - dst_beat))
    val acquire_ok = (state === s_mem_req) &&
      (!put_busy.andR || send_block && dst_beat =/= 0.U) &&
      !block_acquire

    tl.a.valid := acquire_ok && !needs_more && (io.pipe.data.valid || flush_buffer)
    tl.a.bits := edge.Put(
      fromSource = Mux(send_block && dst_beat =/= 0.U, put_block_id, put_id),
      toAddress = Cat(
        dst_block,
        Mux(send_block, 0.U(beatAddrBits.W), dst_beat),
        0.U(byteAddrBits.W)),
      lgSize = Mux(send_block, blockOffset.U, byteAddrBits.U),
      data = shift_data,
      mask = write_mask)._2
    tl.d.ready := put_busy.orR

    io.pipe.data.ready := (acquire_ok && tl.a.ready && !flush_buffer) || needs_more

    put_busy := (put_busy |
      Mux(tl.a.fire() && edge.first(tl.a), UIntToOH(put_id), 0.U)) &
      ~Mux(tl.d.fire(), UIntToOH(tl.d.bits.source), 0.U)

    io.dma.req.ready := (state === s_idle)

    io.dma.resp.valid := (state === s_resp) && !put_busy.orR
    io.dma.resp.bits.xact_id := dma_req_id
    io.dma.resp.bits.status := 0.U

    when (io.dma.req.fire()) {
      dma_req_id := io.dma.req.bits.xact_id
      dst_addr := io.dma.req.bits.dest
      bytes_left := io.dma.req.bits.length
      last_data := 0.U
      last_bytes_val := 0.U
      alloc := io.dma.req.bits.alloc(1)
      send_block := io.dma.req.bits.dest(blockOffset - 1, 0) === 0.U &&
        io.dma.req.bits.length >= blockBytes.U
      state := s_mem_req
    }

    when (io.pipe.data.fire() && needs_more) {
      last_data := data
      last_bytes_val := bytes_val
    }

    when (tl.a.fire()) {
      val next_addr = dst_addr + bytes_to_send
      val next_bytes_left = bytes_left - bytes_to_send

      last_bytes_val := bytes_val - bytes_to_send
      last_data := data >> Cat(bytes_to_send, 0.U(3.W))
      bytes_left := next_bytes_left
      dst_addr := next_addr

      when (next_bytes_left === 0.U) {
        state := s_resp
      }

      when (next_addr(blockOffset - 1, 0) === 0.U) {
        send_block := next_bytes_left >= blockBytes.U
      }
    }

    when (io.dma.resp.fire()) { state := s_idle }
  }

  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  /////////////////////// dma frontend /////////////////////////
  trait HasClientDmaParameters extends HasCoreParameters with HasDmaParameters {
    val dmaAddrBits = coreMaxAddrBits
    val dmaSegmentSizeBits = coreMaxAddrBits
    val dmaSegmentBits = 24
    val dmaClientIdBits = 2
  }

  abstract class ClientDmaBundle(implicit val p: Parameters)
    extends ParameterizedBundle()(p) with HasClientDmaParameters
  abstract class ClientDmaModule(implicit val p: Parameters)
    extends Module with HasClientDmaParameters

  class ClientDmaRequest(implicit p: Parameters) extends ClientDmaBundle()(p) {
    val client_id = UInt(dmaClientIdBits.W)
    val cmd = UInt(DMA_CMD_SZ.W)
    val src_start  = UInt(dmaAddrBits.W)
    val dst_start  = UInt(dmaAddrBits.W)
    val src_stride = UInt(dmaSegmentSizeBits.W)
    val dst_stride = UInt(dmaSegmentSizeBits.W)
    val segment_size = UInt(dmaSegmentSizeBits.W)
    val nsegments  = UInt(dmaSegmentBits.W)
    val alloc = UInt(2.W)

    def isPrefetch(dummy: Int = 0): Bool =
      cmd === DmaRequest.DMA_CMD_PFR || cmd === DmaRequest.DMA_CMD_PFW
  }

  object ClientDmaRequest {
    val DMA_CMD_RESUME = "b01".U

    def apply(client_id: UInt,
              cmd: UInt,
              src_start: UInt,
              dst_start: UInt,
              segment_size: UInt,
              nsegments: UInt = 1.U,
              src_stride: UInt = 0.U,
              dst_stride: UInt = 0.U,
              alloc: UInt = "b10".U)
             (implicit p: Parameters) = {
      val req = Wire(new ClientDmaRequest)
      req.client_id := client_id
      req.cmd := cmd
      req.src_start := src_start
      req.dst_start := dst_start
      req.src_stride := src_stride
      req.dst_stride := dst_stride
      req.segment_size := segment_size
      req.nsegments := nsegments
      req.alloc := alloc
      req
    }
  }
  import ClientDmaRequest._

  object ClientDmaResponse {
    val NO_ERROR = "b000".U
    val PAUSED = "b001".U
    val SRC_PAGE_FAULT = "b010".U
    val DST_PAGE_FAULT = "b011".U
    val SRC_INVALID_REGION = "b100".U
    val DST_INVALID_REGION = "b101".U

    def apply(client_id: UInt, status: UInt = 0.U, fault_vpn: UInt = 0.U)
             (implicit p: Parameters) = {
      val resp = Wire(new ClientDmaResponse)
      resp.client_id := client_id
      resp.status := status
      resp.fault_vpn := fault_vpn
      resp
    }
  }
  import ClientDmaResponse._

  class ClientDmaResponse(implicit p: Parameters)
    extends ClientDmaBundle()(p) with HasCoreParameters {
    val client_id = UInt(dmaClientIdBits.W)
    val status = UInt(dmaStatusBits.W)
    val fault_vpn = UInt(vpnBits.W)
  }

  class ClientDmaIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
    val req = Decoupled(new ClientDmaRequest)
    val resp = Flipped(Valid(new ClientDmaResponse))
  }

  class ClientDmaArbiter(n: Int)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val in = Flipped(Vec(n, new ClientDmaIO))
      val out = new ClientDmaIO
    })

    val req_arb = Module(new RRArbiter(new ClientDmaRequest, n))
    req_arb.io.in <> io.in.map(_.req)
    io.out.req <> req_arb.io.out

    io.in.zipWithIndex.foreach { case (in, i) =>
      val me = io.out.resp.bits.client_id === i.U
      in.resp.valid := me && io.out.resp.valid
      in.resp.bits := io.out.resp.bits
    }
  }

  class DecoupledTLB(entries: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
    val lgMaxSize = log2Ceil(coreDataBytes)
    val io = new Bundle {
      val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
      val resp = Decoupled(new TLBResp)
      val ptw = new TLBPTWIO
    }

    val req = Reg(new TLBReq(lgMaxSize))
    val resp = Reg(new TLBResp)
    val tlb = Module(new TLB(lgMaxSize, entries))

    val s_idle :: s_tlb_req :: s_tlb_resp :: s_done :: Nil = Enum(Bits(), 4)
    val state = Reg(init = s_idle)

    when (io.req.fire()) {
      req := io.req.bits
      state := s_tlb_req
    }

    when (tlb.io.req.fire()) {
      state := s_tlb_resp
    }

    when (state === s_tlb_resp) {
      when (tlb.io.resp.miss) {
        state := s_tlb_req
      } .otherwise {
        resp := tlb.io.resp
        state := s_done
      }
    }

    when (io.resp.fire()) { state := s_idle }

    io.req.ready := state === s_idle

    tlb.io.req.valid := state === s_tlb_req
    tlb.io.req.bits := req

    io.resp.valid := state === s_done
    io.resp.bits := resp

    io.ptw <> tlb.io.ptw
  }

  class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
    val lgMaxSize = log2Ceil(coreDataBytes)
    val req = Decoupled(new TLBReq(lgMaxSize))
    val resp = Flipped(Decoupled(new TLBResp))
  }

  class FrontendTLB(nClients: Int)
                   (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {
    val io = IO(new Bundle {
      val clients = Flipped(Vec(nClients, new FrontendTLBIO))
      val ptw = new TLBPTWIO
    })

    val lgMaxSize = log2Ceil(coreDataBytes)
    val tlbArb = Module(new InOrderArbiter(
      new TLBReq(lgMaxSize), new TLBResp, nClients))
    val tlb = Module(new DecoupledTLB(4))
    tlb.io.req <> tlbArb.io.out_req
    tlbArb.io.out_resp <> tlb.io.resp
    io.ptw <> tlb.io.ptw

    tlbArb.io.in_req <> io.clients.map(_.req)
    io.clients.zip(tlbArb.io.in_resp).foreach {
      case (client, arb_resp) => client.resp <> arb_resp
    }
  }

  class DmaFrontend(implicit p: Parameters) extends CoreModule()(p)
    with HasClientDmaParameters {
    val io = IO(new Bundle {
      val cpu = Flipped(new ClientDmaIO)
      val dma = new DmaIO
      val tlb = new FrontendTLBIO
      val busy = Output(Bool())
      val pause = Input(Bool())
    })

    private val pgSize = 1 << pgIdxBits

    val cmd = Reg(UInt(DMA_CMD_SZ.W))
    val adv_ptr = MuxLookup(cmd, "b11".U, Seq(
      DMA_CMD_PFR -> "b10".U,
      DMA_CMD_PFW -> "b10".U))
    val client_id = Reg(UInt(dmaClientIdBits.W))

    val segment_size = Reg(UInt(dmaSegmentSizeBits.W))
    val bytes_left = Reg(UInt(dmaSegmentSizeBits.W))
    val segments_left = Reg(UInt(dmaSegmentBits.W))

    val src_vaddr = Reg(UInt(dmaAddrBits.W))
    val src_vpn = src_vaddr(dmaAddrBits - 1, pgIdxBits)
    val src_idx = src_vaddr(pgIdxBits - 1, 0)
    val src_pglen = pgSize.U - src_idx

    val src_stride = Reg(UInt(dmaSegmentSizeBits.W))

    val src_ppn = Reg(UInt(ppnBits.W))

    val src_paddr = Cat(src_ppn, src_idx)

    val last_src_vpn = Reg(UInt(vpnBits.W))

    val tx_len = src_pglen min bytes_left

    val dma_busy = RegInit(0.U(nDmaXacts.W))
    val dma_xact_id = PriorityEncoder(~dma_busy)

    val alloc = Reg(UInt(2.W))

    val (s_idle :: s_translate_req :: s_translate_resp :: s_dma_req :: s_dma_update ::
      s_prepare :: s_finish :: Nil) = Enum(7)
    val state = Reg(init = s_idle)

    // lower bit is for src, higher bit is for dst
    //  val to_translate = Reg(UInt(2.W), init = 0.U)
    //  val tlb_sent = Reg(UInt(2.W), init = ~0.U(2.W))
    //  val tlb_to_send = to_translate & ~tlb_sent
    val resp_status = Reg(UInt(dmaStatusBits.W))
    val fault_vpn = Reg(UInt(vpnBits.W))
    val ptw_errors = RegInit(0.U(2.W))
    val send_vpn = PriorityMux(tlb_to_send, Seq(src_vpn, dst_vpn))

    io.tlb.req.valid := tlb_to_send.orR
    io.tlb.req.bits.vaddr := Cat(send_vpn, 0.U(pgIdxBits.W))
    io.tlb.req.bits.passthrough := false.B
    io.tlb.req.bits.instruction := false.B
    io.tlb.req.bits.sfence.valid := PriorityMux(tlb_to_send, ptw_errors.toBools)
    io.tlb.req.bits.sfence.bits.rs1 := true.B
    io.tlb.req.bits.sfence.bits.rs2 := false.B
    io.tlb.req.bits.sfence.bits.addr := Cat(send_vpn, 0.U(pgIdxBits.W))
    io.tlb.resp.ready := tlb_sent.orR


    io.cpu.req.ready := state === s_idle
    io.cpu.resp.valid := state === s_finish
    io.cpu.resp.bits := ClientDmaResponse(client_id, resp_status, fault_vpn)

    io.dma.req.valid := (state === s_dma_req) && !dma_busy.andR
    io.dma.req.bits := DmaRequest(
      xact_id = dma_xact_id,
      cmd = cmd,
      source = src_paddr,
      dest = dst_paddr,
      length = tx_len,
      alloc = alloc)
    io.dma.resp.ready := true.B

    when (io.cpu.req.fire()) {
      val req = io.cpu.req.bits
      client_id := req.client_id
      when (req.cmd =/= DMA_CMD_RESUME) {
        cmd := req.cmd
        src_vaddr := req.src_start
        dst_vaddr := req.dst_start
        src_stride := req.src_stride
        dst_stride := req.dst_stride
        segment_size := req.segment_size
        segments_left := req.nsegments - 1.U
        bytes_left := req.segment_size
        to_translate := Mux(req.isPrefetch(), "b10".U, "b11".U)
        alloc := req.alloc
      } .otherwise {
        // On resume, retranslate any pages that had errors
        to_translate := ptw_errors
      }
      when (io.pause) {
        resp_status := PAUSED
        state := s_finish
      } .otherwise {
        tlb_sent := 0.U
        state := s_translate
      }
    }

    when (io.tlb.req.fire()) {
      last_src_vpn := src_vpn
      state := s_translate_resp
    }

    when (io.tlb.resp.fire()) {
      val recv_choice_oh = PriorityEncoderOH(to_translate)
      val recv_choice = OHToUInt(recv_choice_oh)(0)
      val page_fault = Mux(recv_choice,
        io.tlb.resp.bits.pf.st, io.tlb.resp.bits.pf.ld)
      val bad_region = Mux(recv_choice,
        alloc(1) && !io.tlb.resp.bits.cacheable,
        alloc(0) && !io.tlb.resp.bits.cacheable)

      when (page_fault || bad_region) {
        resp_status := Mux(page_fault,
          Mux(recv_choice, DST_PAGE_FAULT, SRC_PAGE_FAULT),
          Mux(recv_choice, DST_INVALID_REGION, SRC_INVALID_REGION))
        fault_vpn := Mux(recv_choice, dst_vpn, src_vpn)
        ptw_errors := ptw_errors | recv_choice_oh
      } .otherwise {
        // getting the src translation
        when (recv_choice) {
          dst_ppn := io.tlb.resp.bits.paddr >> pgIdxBits.U
        } .otherwise {
          src_ppn := io.tlb.resp.bits.paddr >> pgIdxBits.U
        }
        ptw_errors := ptw_errors & ~recv_choice_oh
      }
      to_translate := to_translate & ~recv_choice_oh
    }

    when (state === s_translate && !to_translate.orR) {
      last_src_vpn := src_vpn
      last_dst_vpn := dst_vpn
      state := Mux(ptw_errors.orR, s_finish, s_dma_req)
    }

    def setBusy(set: Bool, xact_id: UInt): UInt =
      Mux(set, UIntToOH(xact_id), 0.U)

    dma_busy := (dma_busy |
      setBusy(io.dma.req.fire(), dma_xact_id)) &
      ~setBusy(io.dma.resp.fire(), io.dma.resp.bits.xact_id)


    when (io.dma.req.fire()) {
      src_vaddr := src_vaddr + Mux(adv_ptr(0), tx_len, 0.U)
      dst_vaddr := dst_vaddr + Mux(adv_ptr(1), tx_len, 0.U)
      bytes_left := bytes_left - tx_len
      state := s_dma_update
    }

    when (state === s_dma_update) {
      when (bytes_left === 0.U) {
        when (segments_left === 0.U) {
          resp_status := NO_ERROR
          state := s_finish
        } .otherwise {
          src_vaddr := src_vaddr + Mux(adv_ptr(0), src_stride, 0.U)
          dst_vaddr := dst_vaddr + Mux(adv_ptr(1), dst_stride, 0.U)
          bytes_left := segment_size
          segments_left := segments_left - 1.U
          state := s_prepare
        }
      } .otherwise { state := s_prepare }
    }

    when (state === s_prepare) {
      to_translate := adv_ptr & Cat(
        dst_vpn =/= last_dst_vpn,
        src_vpn =/= last_src_vpn)
      when (io.pause) {
        resp_status := PAUSED
        state := s_finish
      } .otherwise {
        tlb_sent := 0.U
        state := s_translate
      }
    }

    when (state === s_finish) { state := s_idle }

    io.busy := (state =/= s_idle) || dma_busy.orR
  }


//  //RoCC instruction handling
//  //=======================================
//  val rocc <> Module(new RoCCUnit)
//  val sys_arr_ctrl = Module(new Controller)
//  rocc.io.rocc.cmd  <> io.cmd
//  io.resp <> rocc.io.rocc.resp
//  io.busy <> rocc.io.rocc.busy
//  io.interrupt <> rocc.io.rocc.interrupt
//  rocc.io.rocc.exception <> io.exception
//
//
//  //dma port to system bus
//  //====================================
//  private val blockOffset = blockOffBits
//  private val beatOffset = log2Up(cacheDataBits/8)
//
//  val addr_block = addr(coreMaxAddrBits - 1, blockOffset)
//  val offset = addr(blockOffset - 1, 0)
//  val next_addr = (addr_block + UInt(1)) << UInt(blockOffset)
//
//  val s_idle :: s_acq :: s_gnt :: s_check :: s_resp :: Nil = Enum(Bits(), 5)
//  val state = Reg(init = s_idle)
//
//  val (tl_out, edgesOut) = outer.atlNode.out(0)
//  val gnt = tl_out.d.bits
//  val recv_data = Reg(UInt(width = cacheDataBits))
//  val recv_beat = Reg(UInt(width = log2Up(cacheDataBeats+1)), init = UInt(0))
//
//  tl_out.a.valid := (state === s_acq)
//  tl_out.a.bits := edgesOut.Get(
//    fromSource = UInt(0),
//    toAddress = addr_block << blockOffset,
//    lgSize = UInt(lgCacheBlockBytes))._2
//  tl_out.d.ready := (state === s_gnt)
//
//  when (tl_out.a.fire()) { state := s_gnt }
//
//  when (tl_out.d.fire()) {
//    recv_beat := recv_beat + UInt(1)
//    recv_data := gnt.data
//    state := s_check
//  }
//
//  when (state === s_check) {
//    //Accelerator functionality (finished = when done with this piece of memory)
//    when (recv_beat === UInt(cacheDataBeats)) {
//      addr := next_addr
//      state := Mux(finished, s_resp, s_acq)
//    } .otherwise {
//      state := s_gnt
//    }
//  }
//
//
//  io.interrupt := Bool(false)
//  io.mem.req.valid := Bool(false)
//  // Tie off unused channels
//  tl_out.b.ready := Bool(true)
//  tl_out.c.valid := Bool(false)
//  tl_out.e.valid := Bool(false)
//
//  //===============================================
//
//  //Page Table Translation Request
//  //==============================================
//  val req_addr = Reg(UInt(width = coreMaxAddrBits))
//  val req_rd = Reg(io.resp.bits.rd)
//  val req_offset = req_addr(pgIdxBits - 1, 0)
//  val req_vpn = req_addr(coreMaxAddrBits - 1, pgIdxBits)
//  val pte = Reg(new PTE)
//
//  val s_idle :: s_ptw_req :: s_ptw_resp :: s_resp :: Nil = Enum(Bits(), 4)
//  val state = Reg(init = s_idle)
//
//  private val ptw = io.ptw(0)
//
//  when (ptw.req.fire()) { state := s_ptw_resp }
//
//  when (state === s_ptw_resp && ptw.resp.valid) {
//    pte := ptw.resp.bits.pte
//    state := s_resp
//  }
//
//  ptw.req.valid := (state === s_ptw_req)
//  ptw.req.bits.valid := true.B
//  ptw.req.bits.bits.addr := req_vpn
//
//  io.resp.valid := (state === s_resp)
//  io.resp.bits.rd := req_rd
//  io.resp.bits.data := Mux(pte.leaf(), Cat(pte.ppn, req_offset), SInt(-1, xLen).asUInt)
//
//
//  //=========================================
//
//}
//
//class WithSystolicArray extends Config((site, here, up) => {
//  case RocketTilesKey => up(RocketTilesKey, site).map { r =>
//    r.copy(rocc = Seq(
//      RoCCParams(
//        opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
//        generator = (p: Parameters) => LazyModule(new SystolicArray()(p)))))
//  }
//})


//// See README.md for license details.
//
//package systolic
//
//import chisel3._
//import chisel3.util._
//
///**
//  * A PE implementing a MAC operation. Configured as fully combinational when integrated into a Mesh.
//  * @param width Data width of operands
//  * @param pass_through If false, the PE pipelines in_a, in_b, in_propag, in_s for 1 cycle
//  */
//val address_width = 32 //bits
//
//class Controller(val width: Int, val tileRows: Int, val tileColumns: Int,
//                 val meshRows: Int, val meshColumns: Int, val sramEntries: Int) extends Module {
//  val io = IO(new Bundle {
//    val in_cmd = Input(UInt(2.W))
//    val in_address = Input(UInt(32.W))
//    val in_size = Input(UInt(32.W))
//    val in_num_of_rows = Input(UInt(32.W))
//    val in_buffer = Input(UInt(512.W)) // Cache line
//    val cpu_valid = Input(Bool())
//    val sys_valid = Input(Bool())
//    val busy =  Output(Bool())
//  })
//
//  val meshwithmemory = Module(new MeshWithMemory(width, tileRows, tileColumns, meshRows, meshColumns,sramEntries))
//
//  val INIT1 = 0.U(2.W)
//  val INIT2 = 1.U(2.W)
//  val INIT3 = 2.U(2.W)
//  val virtual_address_a = RegInit(0.U)
//  val virtual_address_b = RegInit(0.U)
//  val virtual_address_out = RegInit(0.U)
//  val physical_address_a = RegInit(0.U)
//  val physical_address_b = RegInit(0.U)
//  val physical_address_out = RegInit(0.U)
//  val jump_size_a = RegInit(0.U)
//  val jump_size_b = RegInit(0.U)
//  val jump_size_out = RegInit(0.U)
//  val num_of_rows = RegInit(0.U)
//  val a_read_in_buffer = RegInit(0.U) // cache line read
//  val b_read_in_buffer = RegInit(0.U)// cache line read
//  val a_read_buffer = RegInit(0.U)
//  val b_read_buffer = RegInit(0.U)
//  val c_write_out_buffer = RegInit(0.U)
//
//  val BLOCK_SIZE_a = meshwithmemory.io.a.cloneType
//  val BLOCK_SIZE_b = meshwithmemory.io.b.cloneType
//  val BLOCK_SIZE_c = meshwithmemory.io.out_c.cloneType
//
//  val idle :: initialize1 :: initialize2 :: initialize3 :: address_translation :: load_data :: compute :: Nil = Enum(7)
//  val state = RegInit(idle)
//
//  switch (state) {
//    is (idle) {
//      io.busy := false.B
//      when (io.cpu_valid && io.in_cmd === INIT1) {
//        virtual_address_a := io.in_address
//        num_of_rows := io.in_num_of_rows
//        jump_size_a := io.in_size
//        state := initialize2
//
//      }
//      meshwithmemory.io.valid := false.B
//
//    }
//    is (initialize2) {
//      when (io.cpu_valid && io.in_cmd === INIT2) {
//        virtual_address_b := io.in_address
//        jump_size_b := io.in_size
//        state := initialize3
//      }
//      meshwithmemory.io.valid := false.B
//
//    }
//    is (initialize3) {
//      when(io.cpu_valid && io.in_cmd === INIT3) {
//        virtual_address_out := io.in_address
//        jump_size_out := io.in_size
//        state := address_translation
//      }
//      meshwithmemory.io.valid := false.B
//      io.busy := true.B
//
//    }
//    is(address_translation) {
//      physical_address_a := get_physical_address(virtual_address_a)
//      physical_address_b := get_physical_address(virtual_address_b)
//      physical_address_out := get_physical_address(virtual_address_out)
//      state := load_data
//      meshwithmemory.io.valid := false.B
//
//    }
//    is(load_data) {
//      a_read_in_buffer := load_data(physical_address_a,BLOCK_SIZE_a)
//      b_read_in_buffer := load_data(physical_address_b,BLOCK_SIZE_b)
//      physical_address_a := physical_address_a + jump_size_a
//      physical_address_b := physical_address_b + jump_size_b
//      physical_address_out := physical_address_out + jump_size_out
//      meshwithmemory.io.valid := false.B
//
//    }
//    is(compute) {
//      num_of_rows := num_of_rows - 1
//      when(num_of_rows > 0.U) {
//        meshwithmemory.io.a := a_read_in_buffer
//        meshwithmemory.io.b := b_read_in_buffer
//        meshwithmemory.io.valid := true.B
//        when(num_of_rows > 1.U) {
//            a_read_in_buffer := load_data(physical_address_a, BLOCK_SIZE_a)
//            b_read_in_buffer := load_data(physical_address_b, BLOCK_SIZE_b)
//            physical_address_a := physical_address_a + jump_size_a
//            physical_address_b := physical_address_b + jump_size_b
//        }
//        // c_write_out_buffer := meshwithmemory.io.out_c
//
//      }.otherwise {
//        meshwithmemory.io.valid := false.B
//        state := idle
//        io.busy := false.B
//      }
//      when(io.sys_valid) {
//        store_data(c_write_out_buffer, physical_address_out, BLOCK_SIZE_c)
//        physical_address_out := physical_address_out + jump_size_out
//
//      }
//
//    }
//
//    }
//
//}

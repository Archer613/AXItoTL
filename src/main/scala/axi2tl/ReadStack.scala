package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, _}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.MaskGen
import xs.utils.mbist.MBISTPipeline
import xs.utils.sram.SRAMTemplate
import xs.utils.perf.HasPerfLogging


class readEntry(implicit p: Parameters) extends AXItoTLBundle {
  val rvalid = Bool()
  val rready = Bool()
  val raddress = UInt(axiAddrBits.W)
  val entryid = UInt(axi2tlParams.rbufIdBits.W)
  val arid = UInt(axiIdBits.W)
  val readStatus = UInt(3.W)
  val respStatus = UInt(4.W)
  val rsize = UInt(axiSizeBits.W)
  val entryFifoId = UInt(axi2tlParams.rbufIdBits.W)
  val BeatFifoId = UInt(axi2tlParams.rbufIdBits.W)
  val RespFifoId = UInt(axi2tlParams.rbufIdBits.W)
  val sourceId = UInt(sourceBits.W)
}

class RSBlock(implicit p: Parameters) extends AXItoTLBundle {
  val ardata = UInt(axiDataBits.W)
}

/* ======== diplomacy ======== */
class ReadStack(entries: Int = 8
               )(implicit p: Parameters) extends AXItoTLModule with HasPerfLogging {
  val io = IO(new Bundle() {
    val in = new Bundle() {
      val ar = Flipped(
        DecoupledIO(
          new AXI4BundleAR(
            edgeIn.bundle
          )
        )
      )
      val r = DecoupledIO(
        new AXI4BundleR(
          edgeIn.bundle
        )
      )
    }
    val out = new Bundle() {
      val a = DecoupledIO(new TLBundleA(edgeOut.bundle))
      val d = Flipped(DecoupledIO(new TLBundleD(
        edgeOut.bundle
      )))
    }
  })

  def mask(address: UInt, lgSize: UInt): UInt = {
    MaskGen(address, lgSize, beatBytes)
  }


  def countBeat(arId: UInt, rs: Vec[readEntry]): UInt = {
    rs.count(e => e.rvalid && e.arid === arId)
  }

  /*
     Data Structure:
       readStack: store  contrl imformation
       readDataStack : store r_data
   */
  val readStack = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new readEntry))))
  val readDataStack = Module(new SRAMTemplate(
    gen = new RSBlock,
    set = entries,
    way = 1,
    singlePort = true,
    hasMbist = p(AXI2TLParamKey).hasMbist,
    hasShareBus = p(AXI2TLParamKey).hasShareBus,
    parentName = "axi2tl_read_"
  ))
  val mbistPipeline = MBISTPipeline.PlaceMbistPipeline(1,
    s"MBIST_AXI2TL_R_", p(AXI2TLParamKey).hasMbist && p(AXI2TLParamKey).hasShareBus)
  val idel :: waitSend :: waitResp :: waitSendResp :: done :: Nil = Enum(5)
  val axireqArb = Module(new Arbiter(new readEntry, entries))
  val axirespArb = Module(new Arbiter(new readEntry, entries))


  /* ======== Receive Ar Req and Alloc======== */
  /*
    when ar is fire and readStack is not full,
    alloc a entry in readStack and readDataStack.
  */
  val empty = Cat(readStack.map(e => !e.rvalid && e.readStatus === idel)).orR
  val alloc = empty && io.in.ar.valid

  val idxInsert = Mux(alloc, PriorityEncoder(readStack.map(e => !e.rvalid && e.readStatus === idel)), 0.U)
  io.in.ar.ready := empty
  when(alloc) {
    val entry = readStack(idxInsert)
    val r_size1 = io.in.ar.bits.bytes1()
    val r_size = OH1ToUInt(r_size1.asUInt)
    entry.rvalid := true.B
    entry.entryid := idxInsert
    entry.raddress := io.in.ar.bits.addr
    entry.arid := io.in.ar.bits.id
    entry.readStatus := 1.U
    entry.rsize := r_size
    entry.sourceId := Cat(1.asUInt, idxInsert, io.in.ar.bits.id)
    assert((r_size <= log2Ceil(tlDataBits / 8).asUInt && alloc) || !alloc, "AXItoTL : rsize is too long")
  }


  /* ======== Issue  Get Req ======== */
  /*
      Send a get request to get the data,
      Send sequence is fifo
  */
  val hasWaitTLReq = Cat(readStack.map(_.readStatus === waitSend)).orR
  axireqArb.io.in zip readStack foreach {
    case (in, e) =>

      in.valid := e.rvalid && (e.readStatus === waitSend) && (e.entryFifoId === 0.U)
      in.bits := e
  }
  val chosen = axireqArb.io.chosen
  io.out.a.valid := axireqArb.io.out.valid
  io.out.a.bits.opcode := TLMessages.Get
  io.out.a.bits.param := 0.U
  io.out.a.bits.size := axireqArb.io.out.bits.rsize
  io.out.a.bits.source := Cat(1.asUInt, axireqArb.io.out.bits.entryid, axireqArb.io.out.bits.arid)
  io.out.a.bits.address := axireqArb.io.out.bits.raddress
  io.out.a.bits.mask := mask(axireqArb.io.out.bits.raddress, axireqArb.io.out.bits.rsize)
  io.out.a.bits.data := 0.U
  io.out.a.bits.corrupt := false.B
  axireqArb.io.out.ready := io.out.a.ready


  /* ======== Receive  D Resp ======== */
  /*
      receive D Resp,store data in Sram
  */
  val canReceive = Cat(readStack.map(e => e.rvalid && e.readStatus === waitResp)).orR
  io.out.d.ready := canReceive
  //status update shouble be delay one cycle for waiting data write in STAM
  val d_hasData = Mux(io.out.d.bits.opcode === TLMessages.AccessAckData || io.out.d.bits.opcode === TLMessages.GrantData, true.B, false.B)
  val d_valid = io.out.d.fire && d_hasData
  //control sram read and write
  val wen = d_valid

  when(d_valid) {
    val respTLId = io.out.d.bits.source
    val respEntryId = respTLId(axiIdBits + axi2tlParams.rbufIdBits - 1, axiIdBits).asUInt
    // val respEntryId = respTLId( axi2tlParams.rbufIdBits - 1, 0).asUInt
    val entryResp = readStack(respEntryId)
    entryResp.respStatus := Mux(io.out.d.bits.denied || io.out.d.bits.corrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
  }
  readDataStack.io.w.apply(wen, io.out.d.bits.data.asTypeOf(new RSBlock), io.out.d.bits.source(axiIdBits + axi2tlParams.rbufIdBits - 1, axiIdBits).asUInt, 1.U)
  // readDataStack.io.w.apply(wen, io.out.d.bits.data.asTypeOf(new RSBlock), io.out.d.bits.source( axi2tlParams.rbufIdBits - 1, 0).asUInt, 1.U)
  val dataWillWrite = RegNext(d_valid, false.B)
  val respIdx = RegNext(io.out.d.bits.source(axiIdBits + axi2tlParams.rbufIdBits - 1, axiIdBits).asUInt, 0.U)
  // val respIdx = RegNext(io.out.d.bits.source( axi2tlParams.rbufIdBits - 1, 0).asUInt,0.U)

  when(dataWillWrite) {
    readStack(respIdx).readStatus := waitSendResp
    readStack(respIdx).rready := true.B
  }

  /* ======== Issue AXIResp======== */
  /*
      chosen a fire entry , return R Resp
      Beat with the same ID cannot be interleaved. Beat with different ids can be interleaved
  */
  val priority = VecInit(readStack.map(e => e.readStatus === waitSendResp && e.rvalid && e.BeatFifoId === 0.U))

  val proVec = priority.zip(readStack).map {
    case (valid, e) =>
      Mux(valid, e.RespFifoId, entries.U)
  }
  val priority_valid = Cat(readStack.map(e => e.readStatus === waitSendResp && e.rvalid && e.BeatFifoId === 0.U)).orR

  val proVec_1 = RegNext(VecInit(proVec))
  val priority_valid_1 = RegNext(priority_valid, false.B)

  val max_priority_fifoid_valid = priority_valid_1
  val max_priority_fifoid = proVec_1.reduceLeft(_ min _)

  val max_priority_fifoid_1_valid = RegNext(max_priority_fifoid_valid)
  val max_priority_fifoid_1 = RegNext(max_priority_fifoid)

  axirespArb.io.in zip readStack foreach {
    case (in, e) =>
      in.valid := (e.readStatus === waitSendResp && e.rvalid && e.BeatFifoId === 0.U && e.RespFifoId === max_priority_fifoid_1 && e.rready) && max_priority_fifoid_1_valid
      in.bits := e
  }

  val bloackReadData = RegInit(false.B)
  val ren = axirespArb.io.out.valid && !wen && !bloackReadData && max_priority_fifoid_1_valid
  val chosenResp = axirespArb.io.chosen
  val chosenResp1 = RegEnable(chosenResp, ren)
  //need delay one cycle to wait reading data
  val willFree = io.in.r.fire

  val resp_entry = readStack(chosenResp1)
  val resp_data = readDataStack.io.r.resp.data(0).ardata
  val can_send_rresp = bloackReadData
  readDataStack.io.r.apply(ren, axirespArb.io.out.bits.entryid)
  // io.in.r.valid := RegNext(ren)
  // io.in.r.bits.data := readDataStack.io.r.resp.data(0).ardata
  // io.in.r.bits.id := RegNext(axirespArb.io.out.bits.arid)
  // io.in.r.bits.resp := RegNext(axirespArb.io.out.bits.respStatus)
  // io.in.r.bits.last := true.B
  io.in.r.valid := can_send_rresp
  io.in.r.bits.data := resp_data
  io.in.r.bits.id := resp_entry.arid
  io.in.r.bits.resp := resp_entry.respStatus
  io.in.r.bits.last := true.B
  axirespArb.io.out.ready := io.in.r.ready

  //block axirespArb waiting for data to be reading 
  when(ren) {
    bloackReadData := true.B
  }.elsewhen(io.in.r.fire) {
    bloackReadData := false.B
  }


  /* ======== Update ReadStatus and Fifoid ======== */

  when(io.in.ar.fire && io.out.a.fire) {
    val entry = readStack(idxInsert)
    // a entry status will be sending
    entry.entryFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.readStatus === waitSend)))) - 1.U
  }.elsewhen(io.in.ar.fire && !io.out.a.fire) {
    val entry = readStack(idxInsert)
    entry.entryFifoId := readStack.count(e => e.rvalid && (e.readStatus === waitSend))
  }

  when(io.out.a.fire) {
    readStack(chosen).readStatus := waitResp
    for (e <- readStack) {
      when(e.readStatus === waitSend && e.rvalid) {
        e.entryFifoId := e.entryFifoId - 1.U
      }
    }
  }

  when(alloc && willFree) {
    val entry = readStack(idxInsert)

    entry.RespFifoId := PopCount(Cat(readStack.map(e => e.rvalid))) - 1.U
  }.elsewhen(alloc && !willFree) {
    val entry = readStack(idxInsert)
    entry.BeatFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.arid === io.in.ar.bits.id))))
    entry.RespFifoId := PopCount(Cat(readStack.map(e => e.rvalid)))
  }
  //update BeatFifoId
  when(alloc && willFree && io.in.ar.bits.id === readStack(chosenResp1).arid) {
    val entry = readStack(idxInsert)
    entry.BeatFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.arid === io.in.ar.bits.id)))) - 1.U
  }.elsewhen(alloc) {
    val entry = readStack(idxInsert)
    entry.BeatFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.arid === io.in.ar.bits.id))))
  }

  when(willFree) {
    readStack(chosenResp1).rvalid := false.B
    readStack(chosenResp1).readStatus := 0.U
    readStack(chosenResp1).RespFifoId := (entries - 1).U
    readStack(chosenResp1).sourceId := 0.U
    // readStack(chosenResp1).rready := false.B
    for (e <- readStack) {
      when(e.rvalid && e.arid === readStack(chosenResp1).arid) {
        e.BeatFifoId := e.BeatFifoId - 1.U
      }
      when(e.rvalid && e.RespFifoId > readStack(chosenResp1).RespFifoId) {
        e.RespFifoId := e.RespFifoId - 1.U
      }
    }
  }

  if (axi2tlParams.enablePerf) {
    XSPerfAccumulate("readStack_empty", empty)
    XSPerfAccumulate("readStack_alloc", alloc)
    XSPerfAccumulate("readStack_recv_ar_req", io.in.ar.fire)
    XSPerfAccumulate("readStack_send_a_req", io.out.a.fire)
    XSPerfAccumulate("readStack_recv_d_resp", io.out.d.fire)
    XSPerfAccumulate("readStack_send_r_resp", io.in.r.fire)
  }
}



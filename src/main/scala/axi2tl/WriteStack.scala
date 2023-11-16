package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.sram.SRAMTemplate
import xs.utils.RegNextN
import xs.utils.mbist.MBISTPipeline
import xs.utils.perf.HasPerfLogging

class writeEntry(implicit p: Parameters) extends AXItoTLBundle {
  val wvalid = Bool()
  val wready = Bool()
  val waddr = UInt(tlAddrBits.W)
  val respStatus = UInt(2.W)
  val wstatus = UInt(3.W)
  val entryid = UInt(axi2tlParams.wbufIdBits.W)
  val awid = UInt(axiIdBits.W)
  val entryFifoid = UInt(axi2tlParams.wbufIdBits.W)
  val waitWFifoId = UInt(axi2tlParams.wbufIdBits.W)
  val wsize = UInt(tlSizeBits.W)
  val count = UInt(axiLenBits.W)
  val d_resp = UInt(2.W)
  val size = UInt(axiSizeBits.W)
  val len = UInt(axiLenBits.W)
  val waitSendBRespFifoId = UInt(axi2tlParams.wbufIdBits.W)
  val real_last = Bool()
}

class WSBlock(implicit p: Parameters) extends AXItoTLBundle {
  val data = UInt(tlDataBits.W)
  val mask = UInt((tlDataBits / 8).W)
}

object WSBlock {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new WSBlock))
    init
  }

  def apply(data: UInt, mask: UInt)(implicit p: Parameters) = {
    val entry = Wire(new WSBlock)
    entry.data := data
    entry.mask := mask
    entry
  }
}

/* ======== diplomacy ======== */
class NewWriteStack(
  entries: Int = 8
)(
  implicit p: Parameters)
    extends AXItoTLModule
    with HasPerfLogging {

    val io = IO(new Bundle() {
        val in = new Bundle() {
          val aw = Flipped(
            DecoupledIO(
              new AXI4BundleAW(
                edgeIn.bundle
              )
            )
          )
          val w = Flipped(
            DecoupledIO(
              new AXI4BundleW(
                edgeIn.bundle
              )
            )
          )
          val b = DecoupledIO(
            new AXI4BundleB(
              edgeIn.bundle
            )
          )
        }
        val out = new Bundle() {
          val a = DecoupledIO(new TLBundleA(edgeOut.bundle))
          val d = Flipped(
            DecoupledIO(
              new TLBundleD(
                edgeOut.bundle
              )
            )
          )
        }
  })

     /*
     Data Structure:
       readStack: store  contrl imformation
       readDataStack : store r_data
   */
    val writeStack = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new writeEntry))))
    val mbistPipeline =
    MBISTPipeline.PlaceMbistPipeline(1, s"MBIST_AXI2TL_W_", p(AXI2TLParamKey).hasMbist && p(AXI2TLParamKey).hasShareBus)
    val idel :: waitW :: sendPut :: waitDResp :: sendB :: Nil = Enum(5)

     /*
    when aw is fire and readStack is not full,
    alloc a entry in readStack and readDataStack.
   */
    val full = Cat(writeStack.map(_.wvalid)).andR
    val hasWaitW = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitW)).orR
    val alloc = !full && !hasWaitW 
    val idxInsert = Mux(alloc, PriorityEncoder(writeStack.map(!_.wvalid)), 0.U)
    io.in.aw.ready := !full && !hasWaitW
    when(alloc && io.in.aw.fire) {
      val entry = writeStack(idxInsert)
      entry.wvalid := true.B
      entry.waddr := io.in.aw.bits.addr
      entry.wstatus := waitW
      entry.wsize := OH1ToUInt(io.in.aw.bits.bytes1().asUInt)
      entry.size := io.in.aw.bits.size
      entry.len := io.in.aw.bits.len
      entry.awid := io.in.aw.bits.id
      entry.entryid := idxInsert
      entry.real_last := io.in.aw.bits.echo(AXI4FragLast)
      entry.count := 0.U
  }
    /* ======== Receive W Req ======== */
  /*
        Since the W channel in AXI4 does not use wid,
        each time an AW task is accepted, it must wait for a w task.
        data and mask will be stored in Sram
   */
  val lastWFire = io.in.w.fire && io.in.w.bits.last
  val canW = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitW && RegNext(!lastWFire,false.B) )).orR
  io.in.w.ready := canW && io.out.a.ready
  val wsbIdx =dontTouch(WireInit(0.U))
  writeStack.foreach { e =>
    when(e.wvalid && e.wstatus === waitW ) {
      wsbIdx := e.entryid
    }
  }
   /* ======== transfer w into a ======== */
   io.out.a.valid := io.in.w.fire
   io.out.a.bits.opcode := TLMessages.PutPartialData
   io.out.a.bits.param := 0.U
   io.out.a.bits.size := writeStack(wsbIdx).wsize
   io.out.a.bits.source :=  Cat(0.asUInt, writeStack(wsbIdx).entryid, writeStack(wsbIdx).awid)
   io.out.a.bits.address := writeStack(wsbIdx).waddr
   io.out.a.bits.mask := io.in.w.bits.strb
   io.out.a.bits.data := io.in.w.bits.data
   io.out.a.bits.corrupt := false.B


   /* ======== when w is last fire , update w status ======== */
  when(io.in.w.fire && io.in.w.bits.last)
      {
        writeStack(wsbIdx).wstatus := waitDResp
      }

   /* ======== Receive d resp and Send b resp ======== */
   val canRecD = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitDResp)).orR
   io.out.d.ready := canRecD && io.in.b.ready
   val sourceD = io.out.d.bits.source
   val respIdx = sourceD(axiIdBits + axi2tlParams.wbufIdBits - 1, axiIdBits).asUInt
   val isLastD = writeStack(respIdx).count === writeStack(respIdx).len
   io.in.b.bits.id := writeStack(respIdx).awid
   io.in.b.bits.resp := Mux(
      io.out.d.bits.denied || io.out.d.bits.corrupt,
      AXI4Parameters.RESP_SLVERR,
      AXI4Parameters.RESP_OKAY
    )
   io.in.b.bits.echo(AXI4FragLast) := writeStack(respIdx).real_last
   io.in.b.valid := io.out.d.fire
 /* ========  update w status ======== */ 
//  when(io.in.b.fire)
//       {
//            writeStack(respIdx).count :=  writeStack(respIdx).count + 1.U
//       }
 when(io.in.b.fire )
      {
        writeStack(respIdx).wstatus := idel
        writeStack(respIdx).wvalid := false.B
      }

}
class WriteStack(
  entries: Int = 8
)(
  implicit p: Parameters)
    extends AXItoTLModule
    with HasPerfLogging {
  val io = IO(new Bundle() {
    val in = new Bundle() {
      val aw = Flipped(
        DecoupledIO(
          new AXI4BundleAW(
            edgeIn.bundle
          )
        )
      )
      val w = Flipped(
        DecoupledIO(
          new AXI4BundleW(
            edgeIn.bundle
          )
        )
      )
      val b = DecoupledIO(
        new AXI4BundleB(
          edgeIn.bundle
        )
      )
    }
    val out = new Bundle() {
      val a = DecoupledIO(new TLBundleA(edgeOut.bundle))
      val d = Flipped(
        DecoupledIO(
          new TLBundleD(
            edgeOut.bundle
          )
        )
      )
    }
  })

  /*
     Data Structure:
       readStack: store  contrl imformation
       readDataStack : store r_data
   */

  val writeStack = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new writeEntry))))
  val writeDataStack = Module(
    new SRAMTemplate(
      gen = new WSBlock,
      set = entries,
      way = 1,
      singlePort = true,
      hasMbist = p(AXI2TLParamKey).hasMbist,
      hasShareBus = p(AXI2TLParamKey).hasShareBus,
      parentName = "axi2tl_write_"
    )
  )
  val mbistPipeline =
    MBISTPipeline.PlaceMbistPipeline(1, s"MBIST_AXI2TL_W_", p(AXI2TLParamKey).hasMbist && p(AXI2TLParamKey).hasShareBus)
  val idel :: waitW :: sendPut :: waitDResp :: sendB :: Nil = Enum(5)
  val wreqArb = Module(new Arbiter(new writeEntry, entries))
  val sendBArb = Module(new Arbiter(new writeEntry, entries))

  /*
    when aw is fire and readStack is not full,
    alloc a entry in readStack and readDataStack.
   */
  val full = Cat(writeStack.map(_.wvalid)).andR
  val hasWaitW = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitW)).orR
  val alloc = !full && !hasWaitW
  val idxInsert = Mux(alloc, PriorityEncoder(writeStack.map(!_.wvalid)), 0.U)
  
  //when write stack is not full,can receive aw request
  io.in.aw.ready := !full && !hasWaitW
  when(alloc && io.in.aw.fire) {
    val entry = writeStack(idxInsert)
    entry.wvalid := true.B
    entry.waddr := io.in.aw.bits.addr
    entry.wstatus := waitW
    entry.wsize := OH1ToUInt(io.in.aw.bits.bytes1().asUInt)
    entry.size := io.in.aw.bits.size
    entry.len := io.in.aw.bits.len
    entry.awid := io.in.aw.bits.id
    entry.entryid := idxInsert
    entry.real_last := io.in.aw.bits.echo(AXI4FragLast)
  }

  /* ======== Receive W Req ======== */
  /*
        Since the W channel in AXI4 does not use wid,
        each time an AW task is accepted, it must wait for a w task.
        data and mask will be stored in Sram
   */
  val wFire = io.in.w.fire
  val canW = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitW && RegNext(!wFire,false.B) && e.waitWFifoId === 0.U)).orR
  val wen = io.in.w.fire
  val wsbIdx =dontTouch(WireInit(0.U))
  io.in.w.ready := canW
  writeStack.foreach { e =>
    when(e.wvalid && e.wstatus === waitW && e.waitWFifoId === 0.U) {
      wsbIdx := e.entryid
    }
  }
  writeDataStack.io.w.apply(wen, WSBlock(io.in.w.bits.data, io.in.w.bits.strb), wsbIdx, 1.U)
  

  /* ======== Send ======== */
  //when a entry is fire,send a put request to L3

  wreqArb.io.in.zip(writeStack).foreach {
    case (in, e) =>
      in.valid := e.wvalid && e.wstatus === sendPut && e.entryFifoid === 0.U && e.wready
      in.bits := e
  }
  val WSBIdx1 = dontTouch(wreqArb.io.chosen)
  val ren = dontTouch(wreqArb.io.out.valid && !wen)
  // val rdDataRaw = RegNextN(array.io.r.resp.data(0), sramLatency - 1) // DSBlock
  val write_data = writeDataStack.io.r.resp.data(0).data
  val write_mask = writeDataStack.io.r.resp.data(0).mask
  val write_size = RegNextN(wreqArb.io.out.bits.wsize, sramLatency - 1)
  val mask_bits = dontTouch(PopCount(write_mask))
  val write_bits = dontTouch(1.U << io.out.a.bits.size)
  // assert((mask_bits <= write_bits && io.out.a.fire) || !io.out.a.fire,"AXItoTL: write bits is too much")
  writeDataStack.io.r.apply(ren, wreqArb.io.chosen)
  io.out.a.valid := RegNextN(ren, sramLatency - 1)
  io.out.a.bits.opcode := RegNextN(TLMessages.PutPartialData, sramLatency - 1)
  io.out.a.bits.param := 0.U
  io.out.a.bits.size := RegNextN(wreqArb.io.out.bits.wsize, sramLatency - 1)
  io.out.a.bits.source := RegNextN(
    Cat(0.asUInt, wreqArb.io.out.bits.entryid, wreqArb.io.out.bits.awid),
    sramLatency - 1
  )
  io.out.a.bits.address := RegNextN(wreqArb.io.out.bits.waddr, sramLatency - 1)
  io.out.a.bits.mask := writeDataStack.io.r.resp.data(0).mask
  io.out.a.bits.data := writeDataStack.io.r.resp.data(0).data
  io.out.a.bits.corrupt := false.B
  wreqArb.io.out.ready := io.out.a.ready

  /* ======== Receive d resp and Send b resp ======== */
  val canRecD = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitDResp)).orR
  val d_valid = io.out.d.fire
  io.out.d.ready := canRecD

  when(d_valid) {
    val sourceD = io.out.d.bits.source
    val wsIdx = sourceD(axiIdBits + axi2tlParams.wbufIdBits - 1, axiIdBits).asUInt
    // val wsIdx = sourceD(axi2tlParams.wbufIdBits - 1,0).asUInt
    writeStack(wsIdx).wstatus := sendB
    writeStack(wsIdx).d_resp := Mux(
      io.out.d.bits.denied || io.out.d.bits.corrupt,
      AXI4Parameters.RESP_SLVERR,
      AXI4Parameters.RESP_OKAY
    )
  }

  when(d_valid && io.in.b.fire) {
    val sourceD = io.out.d.bits.source
    val wsIdx = sourceD(axiIdBits + axi2tlParams.wbufIdBits - 1, axiIdBits).asUInt

    // val wsIdx = sourceD( axi2tlParams.wbufIdBits - 1, 0).asUInt
    writeStack(wsIdx).waitSendBRespFifoId := PopCount(writeStack.map(e => e.wvalid && e.wstatus === sendB)) - 1.U
  }.elsewhen(d_valid && !io.in.b.fire) {
    val sourceD = io.out.d.bits.source
    val wsIdx = sourceD(axiIdBits + axi2tlParams.wbufIdBits - 1, axiIdBits).asUInt
    // val wsIdx = sourceD( axi2tlParams.wbufIdBits - 1, 0).asUInt

    writeStack(wsIdx).waitSendBRespFifoId := PopCount(writeStack.map(e => e.wvalid && e.wstatus === sendB))
  }
  sendBArb.io.in.zip(writeStack).foreach {
    case (in, e) =>
      in.valid := e.wvalid && e.wstatus === sendB && e.waitSendBRespFifoId === 0.U
      in.bits := e
  }

  val chosenBIdx = sendBArb.io.chosen
  sendBArb.io.out.ready := io.in.b.ready
  io.in.b.valid := sendBArb.io.out.valid
  io.in.b.bits.id := writeStack(chosenBIdx).awid
  io.in.b.bits.resp := writeStack(chosenBIdx).d_resp
  io.in.b.bits.echo(AXI4FragLast) := writeStack(chosenBIdx).real_last

  /* ======== Update wstatus and Fifoid ======== */

  val datawillready = RegNext(io.in.w.fire, false.B)
  when(io.in.aw.fire && datawillready) {
    val entry = writeStack(idxInsert)
    entry.waitWFifoId := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW))) - 1.U
  }.elsewhen(io.in.aw.fire && !datawillready) {
    val entry = writeStack(idxInsert)
    entry.waitWFifoId := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW)))
  }

  when(datawillready) {
    writeStack.foreach { e =>
      when(e.wvalid && e.wstatus === waitW && e.waitWFifoId === 0.U) {
        e.wstatus := sendPut
        e.waitWFifoId := e.waitWFifoId - 1.U
        e.wready := true.B
      }.elsewhen(e.wvalid && (e.wstatus === waitW)) {
        e.waitWFifoId := e.waitWFifoId - 1.U
      }
    }
  }

  val willput = RegNextN(wreqArb.io.out.valid && !io.in.w.fire && io.out.a.ready, sramLatency - 1)
  when(wreqArb.io.out.valid && !io.in.w.fire && io.out.a.ready) {
    writeStack(WSBIdx1).wready := false.B
  }
  when(willput) {
    writeStack.foreach { e =>
      when(e.wvalid && e.wstatus === sendPut && e.entryFifoid === 0.U) {
        e.wstatus := waitDResp
      }
      when(e.wvalid) {
        e.entryFifoid := e.entryFifoid - 1.U
      }
    }
  }

  when(io.in.aw.fire && willput) {
    val entry = writeStack(idxInsert)
    entry.entryFifoid := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW || e.wstatus === sendPut))) - 1.U
  }.elsewhen(io.in.aw.fire && !willput) {
    val entry = writeStack(idxInsert)
    entry.entryFifoid := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW || e.wstatus === sendPut)))
  }

  when(io.in.b.fire) {
    writeStack.foreach { e =>
      when(e.wvalid && e.wstatus === sendB && e.waitSendBRespFifoId === 0.U) {
        e.wvalid := false.B
        e.wstatus := idel
        e.waitSendBRespFifoId := (entries - 1).U
        //                  e.wready := false.B
      }
      when(e.wvalid && e.wstatus === sendB) {
        e.waitSendBRespFifoId := e.waitSendBRespFifoId - 1.U
      }
    }
  }
  if (axi2tlParams.enablePerf) {
    XSPerfAccumulate("writeStack_full", full)
    XSPerfAccumulate("writeStack_alloc", alloc)
    XSPerfAccumulate("writeStack_recv_aw_req", io.in.aw.fire)
    XSPerfAccumulate("writeStack_recv_w_req", io.in.w.fire)
    XSPerfAccumulate("writeStack_send_a_req", io.out.a.fire)
    XSPerfAccumulate("writeStack_recv_d_resp", io.out.d.fire)
    XSPerfAccumulate("writeStack_send_b_resp", io.in.b.fire)
  }

}

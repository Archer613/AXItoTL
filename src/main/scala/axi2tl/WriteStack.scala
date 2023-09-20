package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

import scala.collection.immutable.Nil
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4._
//import utility._
import freechips.rocketchip.util._
import freechips.rocketchip.util.MaskGen
import xs.utils.sram.SRAMTemplate


class writeEntry(implicit p:Parameters) extends AXItoTLBundle {
    val wvalid = Bool()
    val wready = Bool()
    val waddr = UInt(tlAddrBits.W)
    val respStatus  = UInt(2.W)
    val wstatus = UInt(2.W)
    val entryid = UInt(axi2tlParams.ridBits.W)
    val awid = UInt(axiIdBits.W)
    val entryFifoid = UInt(axi2tlParams.ridBits.W)
    val waitWFifoId = UInt(axi2tlParams.ridBits.W)
    val wsize = UInt(tlSizeBits.W)
    val count = UInt(axi2tlParams.ridBits.W)
    val d_resp = UInt(2.W)
    val size = UInt(axiSizeBits.W)
    val len = UInt(axiLenBits.W)
    val waitSendBRespFifoId = UInt(axi2tlParams.ridBits.W)
}
class WSBlock(implicit p:Parameters) extends AXItoTLBundle {
  val data = UInt(tlDataBits.W)
  val mask = UInt((tlDataBits/8).W)
}

object WSBlock{
  def apply()(implicit p:Parameters) = {
    val init = WireInit(0.U.asTypeOf(new WSBlock))
    init
  }

  def apply(data : UInt , mask : UInt)(implicit p:Parameters) = {
    val entry = Wire(new WSBlock)
    entry.data := data
    entry.mask := mask
    entry
  }
}


class WriteStack(entries : Int = 8)(implicit p:Parameters) extends AXItoTLModule {
  val io = IO(new Bundle() {
    val in = Flipped(AXI4Bundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
  })

  // TODO: not fully initialized
  io.in.r.bits.resp := DontCare
  io.in.ar.ready := false.B
  io.in.r.bits.last := DontCare
  io.in.r.bits.data := DontCare
  io.in.r.bits.id := DontCare
  io.in.r.valid := false.B
  io.in.b.bits.resp := DontCare
  io.in.b.bits.id := DontCare

  /*
     Data Structure:
       writeStack: 32 entrires
   */
  val writeStack = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new writeEntry))))
  val writeDataStack = Module(new SRAMTemplate(
    gen = new WSBlock,
    set = entries,
    way = 1,
    singlePort = true
  ))
//  UIntToOH()
  val idel::waitW::sending::sendW::waitDResp::sendB::done::Nil = Enum(7)
  val wreqArb = Module(new Arbiter(new writeEntry,entries))
  val awreqArb = Module(new Arbiter(new writeEntry,entries))
  val sendBArb = Module(new Arbiter(new writeEntry,entries))

  // TODO: not fully initialized
  awreqArb.io.in := DontCare
  wreqArb.io.out.ready := false.B
  sendBArb.io.out.ready := false.B

  //flags
  val full = Cat(writeStack.map(_.wvalid)).andR
  val alloc = !full && io.in.aw.valid
//  val watiWReq = false.B
  io.in.aw.ready := !full
  val idxInsert = Mux(alloc,PriorityEncoder(writeStack.map(!_.wvalid)),0.U)

  val wen = io.in.w.fire
  val ren = awreqArb.io.out.valid && !wen && io.out.a.ready
  //when write stack is not full,can receive aw request
  when(alloc){
    val entry = writeStack(idxInsert)
    entry.wvalid  := true.B
    entry.waddr := io.in.aw.bits.addr
    entry.wstatus := waitW
//    entry.validStatus := waitValid
    entry.wsize := OH1ToUInt(io.in.aw.bits.bytes1().asUInt)
    entry.size := io.in.aw.bits.size
    entry.len := io.in.aw.bits.len
    entry.awid := io.in.aw.bits.id
    entry.entryid := idxInsert

  }

  //receive w request
  val canW = Cat(writeStack.map(e => e.wvalid && e.wstatus === waitW && e.waitWFifoId === 0.U)).orR
  io.in.w.ready := canW
  val wsbIdx = RegInit(0.U)
  writeStack.foreach{
    case e =>
      when(e.wvalid && e.wstatus === waitW && e.waitWFifoId === 0.U)
      {
        wsbIdx := e.entryid
      }
  }
  writeDataStack.io.w.apply(wen, WSBlock(io.in.w.bits.data, io.in.w.bits.strb), wsbIdx, 1.U)
  val datawillready = RegNext(io.in.w.fire, false.B)

  //update waitWFifoId
  when(io.in.aw.fire && datawillready)
      {
        val entry = writeStack(idxInsert)
        entry.waitWFifoId := PopCount(writeStack.map(e => e.wvalid&&(e.wstatus === waitW ))) - 1.U
      }.elsewhen(io.in.aw.fire && !datawillready)
      {
        val entry = writeStack(idxInsert)
        entry.waitWFifoId := PopCount(writeStack.map(e => e.wvalid&&(e.wstatus === waitW )))
      }

  when(datawillready)
    {
          writeStack foreach{
            e =>
              when(e.wvalid && e.wstatus === waitW &&e.waitWFifoId === 0.U){
                e.wstatus := sending
                e.waitWFifoId := e.waitWFifoId-1.U
                e.wready := true.B
              }.elsewhen(e.wvalid&&(e.wstatus === waitW))
              {
                e.waitWFifoId := e.waitWFifoId-1.U
              }
          }
    }




  //when a entry is ready,send a put request to L3
  wreqArb.io.in zip writeStack foreach{
        case(in,e) =>
        in.valid := e.wvalid && e.wstatus === sending  && e.entryFifoid === 0.U && e.wready
        in.bits := e
  }
  val WSBIdx = awreqArb.io.chosen
  writeDataStack.io.r.apply(ren, awreqArb.io.chosen)
  io.out.a.valid := RegNext(wreqArb.io.out.valid && !io.in.w.fire)
  io.out.a.bits.opcode := RegNext(TLMessages.PutPartialData)
  io.out.a.bits.param := 0.U
  io.out.a.bits.size := RegNext(wreqArb.io.out.bits.wsize)
  io.out.a.bits.source := RegNext(Cat(wreqArb.io.out.bits.entryid,wreqArb.io.out.bits.awid))
  io.out.a.bits.address := RegNext( wreqArb.io.out.bits.waddr)
  io.out.a.bits.mask := RegNext(writeDataStack.io.r.resp.data(0).mask)
  io.out.a.bits.data := RegNext(writeDataStack.io.r.resp.data(0).data)
  io.out.a.bits.corrupt := false.B
  awreqArb.io.out.ready := io.out.a.ready


  val willput = RegNext(awreqArb.io.out.valid && !io.in.w.fire && io.out.a.ready)
  //avoid will hold two cycle

  when(awreqArb.io.out.valid && !io.in.w.fire && io.out.a.ready) {
    writeStack(WSBIdx).wready := false.B
  }
  when(willput) {
    writeStack foreach {
      e =>
        when(e.wvalid && e.wstatus === sendW  && e.entryFifoid === 0.U) {
          e.wstatus := waitDResp
        }
        when(e.wvalid) {
          e.entryFifoid := e.entryFifoid - 1.U
        }
    }
  }

  //update entry fifoid
  when(io.in.aw.fire && willput) {
    val entry = writeStack(idxInsert)
    entry.entryFifoid := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW || e.wstatus === sending || e.wstatus === sendW))) - 1.U
  }.elsewhen(io.in.aw.fire && !willput) {
    val entry = writeStack(idxInsert)
    entry.entryFifoid := PopCount(writeStack.map(e => e.wvalid && (e.wstatus === waitW || e.wstatus === sending || e.wstatus === sendW)))
  }

  //receive a d resp
  val canRecD = Cat(writeStack.map(e => e.wvalid && e.wready && e.wstatus === waitDResp)).orR
  io.out.d.ready := canRecD
  val d_valid = io.out.d.fire && !edgeOut.hasData(io.out.d.bits)
  when(d_valid)
    {
        val sourceD = io.out.d.bits.source
//        val wsIdx = sourceD(edgeIn.bundle.idBits + axi2tlParams.ridBits,edgeIn.bundle.idBits).asUInt // TODO: parameterization
        val wsIdx = 0.U
        writeStack(wsIdx).wstatus := sendB
        writeStack(wsIdx).d_resp := Mux(io.out.d.bits.denied || io.out.d.bits.corrupt, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY)
    }
  when(d_valid && io.in.b.fire)
    {
      val sourceD = io.out.d.bits.source
//      val wsIdx = sourceD(edgeIn.bundle.idBits + axi2tlParams.ridBits, edgeIn.bundle.idBits).asUInt // TODO: parameterization
      val wsIdx = 0.U
      writeStack(wsIdx).waitSendBRespFifoId := PopCount(writeStack.map(e => e.wvalid && e.wstatus === sendB)) - 1.U
    }.elsewhen(d_valid && !io.in.b.fire)
    {
      val sourceD = io.out.d.bits.source
//      val wsIdx = sourceD(edgeIn.bundle.idBits + axi2tlParams.ridBits, edgeIn.bundle.idBits).asUInt // TODO: parameterization
      val wsIdx = 0.U
      writeStack(wsIdx).waitSendBRespFifoId := PopCount(writeStack.map(e => e.wvalid && e.wstatus === sendB)) - 1.U
    }
  sendBArb.io.in zip writeStack foreach{
      case(in,e) =>
        in.valid := e.wvalid && e.wstatus === sendB && e.waitSendBRespFifoId === 0.U
        in.bits := e
  }
  io.in.b.valid := sendBArb.io.out.valid
  val chosenBIdx = sendBArb.io.chosen
  when(io.in.b.fire)
    {
      io.in.b.bits.id := writeStack(chosenBIdx).awid
      io.in.b.bits.resp := writeStack(chosenBIdx).d_resp
//      io.in.b.bits.user := so
    }
  when(io.in.b.fire)
      {
        writeStack.foreach{
          case  e =>
            when(e.wvalid && e.wstatus === sendB && e.waitSendBRespFifoId === 0.U)
                {
                  e.wvalid := false.B
                  e.wstatus := idel
                  e.wready := false.B
                }
            when(e.wvalid)
              {
                e.waitSendBRespFifoId := e.waitSendBRespFifoId - 1.U
              }
        }
      }
}

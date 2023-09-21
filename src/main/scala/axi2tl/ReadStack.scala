package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._

import scala.collection.immutable.Nil
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, _}
//import utility._
import freechips.rocketchip.util._
import freechips.rocketchip.util.MaskGen
import xs.utils.sram.SRAMTemplate

class readEntry(implicit p:Parameters) extends AXItoTLBundle{
  val rvalid = Bool()
  val rready = Bool()
  val raddress = UInt(axiAddrBits.W)
  val entryid = UInt(axi2tlParams.ridBits.W)
  val arid = UInt(axiIdBits.W)
  val readStatus = UInt(2.W)
  val respStatus = UInt(4.W)
  val rsize = UInt(axiSizeBits.W)
  val entryFifoId = UInt(axi2tlParams.fifoBits.W)
  val BeatFifoId = UInt(axi2tlParams.fifoBits.W)
  val RespFifoId = UInt(axi2tlParams.fifoBits.W)
}

class RSBlock(implicit p:Parameters) extends AXItoTLBundle  {
  val ardata = UInt(axiDataBits.W)
}

class ReadStack(entries : Int = 8)(implicit p:Parameters) extends AXItoTLModule {
  val io = IO(new Bundle(){
    val in = Flipped(AXI4Bundle(edgeIn.bundle))
    val out = TLBundle(edgeOut.bundle)
  })

  // TODO: not fully initialized
  io.in.b.bits.resp := DontCare
  io.in.b.bits.id := DontCare
  io.in.b.valid := false.B
  io.in.w.ready := false.B
  io.in.aw.ready := false.B

  def mask(address: UInt, lgSize: UInt): UInt = {
    MaskGen(address, lgSize, axi2tlParams.beatBytes)
  }
//  def isFisrtAXI(arId: UInt,rs:Vec[readEntry]):Bool = !Cat(rs.map(
//    m =>
//    m.rvalid && m.readStatus === waitSend && m.arid === arId
//  )).orR
//  def LastIdx(arId:UInt,rs:Vec[readEntry]): Unit = {
//    rs.zipWithIndex.collectFirst {
//      case(value,index)
//            if (value.rvalid && value.readStatus === waitSend && value.arid === arId)
//      => index
//    }.getOrElse(-1.U)
//  }
//  def countBeat(arId:UInt,rs:Vec[readEntry]):UInt = (rs.map(e => Mux(e.rvalid && e.arid === arId,true.B,false.B))).reduce(_ +& _)
//

  def countBeat(arId: UInt, rs: Vec[readEntry]): UInt = {
    rs.count(e => e.rvalid && e.arid === arId)
  }

  /*
     Data Structure:
       readStack: 32 entrires
   */
  val readStack = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new readEntry))))
  val readDataStack = Module(new SRAMTemplate(
    gen = new RSBlock,
    set = entries,
    way = 1 ,
    singlePort = true
  ))
  val idel::waitSend::waitResp::waitSendResp::done::Nil = Enum(5)
  val axireqArb = Module(new Arbiter(new readEntry,entries))
  val axirespArb = Module(new Arbiter(new readEntry,entries))

  //flags
  val empty = Cat(readStack.map(e => !e.rvalid && e.readStatus === idel)).orR
  val alloc = empty && io.in.ar.valid
//  val theLastFifoId = RegInit(0.U)
  println("alloc os :%d\n",alloc)
  io.in.ar.ready := empty
  val idxInsert = Mux(alloc,PriorityEncoder(readStack.map(e => !e.rvalid && e.readStatus === idel)),0.U)
//
  //receive AXIReq
  when(alloc)
    {
//      idxInsert = PriorityEncoder(readStack.map(!_.rvalid))
      val entry = readStack(idxInsert)
      val r_size1 = io.in.ar.bits.bytes1()
      val r_size = OH1ToUInt(r_size1.asUInt)
      entry.rvalid := true.B
      entry.entryid := idxInsert
      entry.raddress := io.in.ar.bits.addr
      entry.arid := io.in.ar.bits.id
      entry.readStatus := 1.U
      entry.rsize := r_size1
//      entry.entryFifoId := readStack.count(e => e.rvalid && (e.readStatus === waitSend))

    //  entry.isFirstBeat := isFisrtAXI(io.in.ar.bits.id,readStack)
      print("receive a axi request , entryId:%d arId:%d raddress:%d rsize:%d readStatus:%d entryFifoId:%d beatFifoId:%d\n")
    }


  //issue TLReq
  val hasWaitTLReq = Cat(readStack.map(_.readStatus === waitSend)).orR
  axireqArb.io.in zip readStack foreach{
      case(in,e) =>
      /*
          when entry is valid and is the first beat
       */
      in.valid := e.rvalid && (e.readStatus === waitSend) &&(e.entryFifoId === 0.U)
      in.bits := e
  }
  io.out.a .valid := axireqArb.io.out.valid
  io.out.a .bits.opcode := TLMessages.Get
  io.out.a .bits.param := 0.U
  io.out.a .bits.size := axireqArb.io.out.bits.rsize
  io.out.a .bits.source := Cat(axireqArb.io.out.bits.entryid,axireqArb.io.out.bits.arid)
  io.out.a .bits.address := axireqArb.io.out.bits.raddress
  io.out.a .bits.mask := mask(axireqArb.io.out.bits.raddress,axireqArb.io.out.bits.rsize)
  io.out.a .bits.data := 0.U
  io.out.a .bits.corrupt := false.B
  axireqArb.io.out.ready := io.out.a .ready
  val chosen = axireqArb.io.chosen



  //update
  //when tlReq send , entry status will be waitResp
  //when entry is waitSend,entryFIfoId -1
  //chosen entry readStatue:waitSend => waitResp
  when(io.in.ar.fire && io.out.a .fire)
    {
        val entry = readStack(idxInsert)
        // a entry status will be sending
        entry.entryFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.readStatus === waitSend)))) - 1.U
    }.elsewhen(io.in.ar.fire && !io.out.a .fire)
    {
        val entry = readStack(idxInsert)
        // a entry status will be sending
        entry.entryFifoId := readStack.count(e => e.rvalid && (e.readStatus === waitSend))
    }

  when(io.out.a .fire)
  {
    readStack(chosen).readStatus := waitResp
//    readStack(chosen).rready := true.B
    for (e <- readStack) {
      when(e.readStatus === waitSend && e.rvalid) {
        e.entryFifoId := e.entryFifoId - 1.U
      }
    }
  }

  //receive TLResq
  val canReceive = Cat(readStack.map(e =>e.rvalid && e.readStatus === waitResp)).orR
  io.out.d.ready := canReceive

  //status update shouble be delay one cycle for waiting data write in STAM
  val d_valid = io.out.d.fire&& edgeOut.hasData(io.out.d.bits)
  val wen = d_valid
  val ren = axirespArb.io.out.valid && !wen && io.in.r.ready
  when(d_valid)
  {
    val respTLId = io.out.d.bits.source
//    val respEntryId = respTLId(18, 14).asUInt // TODO: parameterization
    val respEntryId = 0.U
    val entryResp = readStack(respEntryId)

    entryResp.readStatus := waitSendResp
    entryResp.respStatus := !io.out.d.bits.denied //need change
  }
//  readDataStack.io.w.apply(wen, io.out.d.bits.data.asTypeOf(new RSBlock), io.out.d.bits.source(18, 14).asUInt, 1.U) // TODO: parameterization
  readDataStack.io.w.apply(wen, io.out.d.bits.data.asTypeOf(new RSBlock), io.out.d.bits.source.asUInt, 1.U)
  val dataWillWrite = RegNext(d_valid,false.B)
//  val respIdx = RegNext(io.out.d.bits.source(18, 14).asUInt,0.U) // TODO: parameterization
  val respIdx = 0.U
  when(dataWillWrite)
  {
    readStack(respIdx).rready := true.B
  }
  //issue AXIResp
  val priority = VecInit(readStack.map(e => e.readStatus === waitSendResp && e.rvalid && e.BeatFifoId === 0.U && e.rready))
  val proVec = priority.zip(readStack).map {
      case(valid,e) =>
        Mux(valid,e.RespFifoId,entries.U)
  }
  val max_priority_fifoid = proVec.reduceLeft(_ min _)
  axirespArb.io.in zip readStack foreach{
      case(in,e) =>
      //when entry status is waitSendResp and is the first Beat in same AXI ID
      in.valid := (e.readStatus === waitSendResp && e.rvalid && e.BeatFifoId === 0.U && e.RespFifoId === max_priority_fifoid && e.rready)
      in.bits := e
  }

  //need delay one cycle to wait reading data
  readDataStack.io.r.apply(ren,axirespArb.io.out.bits.entryid)
  io.in.r.valid := RegNext(axirespArb.io.out.valid && !wen)
  io.in.r.bits.data := RegNext(readDataStack.io.r.resp.data(0).ardata)
  io.in.r.bits.id := RegNext(axirespArb.io.out.bits.arid)
  io.in.r.bits.resp := RegNext(axirespArb.io.out.bits.respStatus)
  io.in.r.bits.last := true.B
  val chosenResp = axirespArb.io.chosen
  val willFree = RegNext(axirespArb.io.out.valid && !wen && io.in.r.ready,false.B)
  when(axirespArb.io.out.valid && !wen && io.in.r.ready)
  {
    readStack(chosenResp).rready := false.B
  }


  axirespArb.io.out.ready := io.in.r.ready

  //clean the entry
  //readStatue : waitResp => done
  //rvalid => false.B
  //beatFifoId - 1

  when( alloc && willFree)
      {
        val entry = readStack(idxInsert)
        entry.BeatFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.arid === io.in.ar.bits.id)))) - 1.U
        entry.RespFifoId := PopCount(Cat(readStack.map(e => e.rvalid ))) - 1.U
      }.elsewhen( alloc && !willFree)
        {
          val entry = readStack(idxInsert)
          entry.BeatFifoId := PopCount(Cat(readStack.map(e => e.rvalid && (e.arid === io.in.ar.bits.id))))
          entry.RespFifoId := PopCount(Cat(readStack.map(e => e.rvalid )))
        }

  when(willFree){
      readStack(chosenResp).rvalid := false.B
      readStack(chosenResp).readStatus := 0.U
      readStack(chosenResp).RespFifoId := (entries-1).U
        for (e <- readStack) {
          when(e.rvalid && e.arid === readStack(chosenResp).arid) {
            e.BeatFifoId := e.BeatFifoId - 1.U
          }
          when(e.rvalid && e.RespFifoId > readStack(chosenResp).RespFifoId)
          {
              e.RespFifoId := e.RespFifoId - 1.U
          }
        }
  }

}



package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}
import freechips.rocketchip.amba._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import  freechips.rocketchip.amba.axi4._

import chisel3.util._


trait HasAXI2TLParameters {
  val p: Parameters
  val axi2tlParams = p(AXI2TLParamKey)
  val sramLatency = 2
  lazy val edgeIn = p(EdgeInKey)
  lazy val edgeOut = p(EdgeOutKey)
  lazy val beatBytes = edgeIn.bundle.dataBits / 8
  lazy val axiAddrBits = edgeIn.bundle.addrBits
  lazy val axiSizeBits = edgeIn.bundle.sizeBits
  lazy val axiDataBits = edgeIn.bundle.dataBits
  lazy val axiIdBits = edgeIn.bundle.idBits
  lazy val axiLenBits = edgeIn.bundle.lenBits

  lazy val tlAddrBits = edgeOut.bundle.addressBits
  lazy val tlSizeBits = edgeIn.bundle.sizeBits
  lazy val tlDataBits = edgeOut.bundle.dataBits
  lazy val sourceBits = edgeOut.bundle.sourceBits
  lazy val sinkBits = edgeOut.bundle.sinkBits
  lazy val  tlechoFields = edgeOut.bundle.echoFields
  lazy val  tlrequestFields = edgeOut.bundle.requestFields
  lazy val  tlresponseFields = edgeOut.bundle.responseFields
}


case class MyAXI4ToTLNode(wcorrupt: Boolean,wbufSize:Int, rbufSize:Int)(implicit valName: ValName) extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = {  mp => TLMasterPortParameters.v1(
      clients = mp.masters.map{m =>
          TLMasterParameters.v1(
            name        = s"axitotlnode",
            sourceId    = IdRange(0, 1 << (log2Ceil(m.id.end) + Seq(log2Ceil(wbufSize), log2Ceil(rbufSize)).max+1)), // R+W ids are distinct
            nodePath    = m.nodePath,
            requestFifo = true
          )
      }
      ,
      echoFields    = mp.echoFields,
      requestFields = AMBAProtField() +: mp.requestFields,
      responseKeys  = mp.responseKeys
      )
  },
  uFn = { mp => AXI4SlavePortParameters(
    slaves = mp.managers.map { m =>
      val maxXfer = TransferSizes(1, mp.beatBytes * (1 << AXI4Parameters.lenBits))
      AXI4SlaveParameters(
        address       = m.address,
        resources     = m.resources,
        regionType    = m.regionType,
        executable    = m.executable,
        nodePath      = m.nodePath,
        supportsWrite = m.supportsPutPartial.intersect(maxXfer),
        supportsRead  = m.supportsGet.intersect(maxXfer),
        interleavedId = Some(0))}, // TL2 never interleaves D beats
    beatBytes = mp.beatBytes,
    minLatency = mp.minLatency,
    responseFields = mp.responseFields,
    requestKeys    = (if (wcorrupt) Seq(AMBACorrupt) else Seq()) ++ mp.requestKeys.filter(_ != AMBAProt))
  })


class AXItoTL(wbufSize:Int, rbufSize:Int)(implicit p: Parameters) extends LazyModule with HasAXI2TLParameters {
  
  val node = MyAXI4ToTLNode(false,wbufSize,rbufSize)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    require(node.in.length == 1)
    require(node.out.length == 1)
    private val edgeIn = node.in.head._2
    private val edgeOut = node.out.head._2
    private val axiSideParam = node.in.head._2.bundle
    private val tlSideParam = node.out.head._2.bundle
    println("AXI to TL Info:")
    println(s"AXI side:\n\tAddr Width:${axiSideParam.addrBits}\n\tData Width:${axiSideParam.dataBits}\n\tID Width:${axiSideParam.idBits}\n")
    println(s"TL side:\n\tAddr Width:${tlSideParam.addressBits}\n\tData Width:${tlSideParam.dataBits}\n\tSource Width:${tlSideParam.sourceBits}\n")

    private val params = AXI2TLParam( wbufSize = wbufSize, rbufSize = rbufSize)

    private val writeStack = Module(new WriteStack(wbufSize)(p.alterPartial {
      case AXI2TLParamKey => params
      case  EdgeInKey => edgeIn
      case EdgeOutKey => edgeOut
    }))
    private val readStack = Module(new ReadStack(rbufSize)(p.alterPartial {
      case AXI2TLParamKey => params
      case EdgeInKey => edgeIn
      case EdgeOutKey => edgeOut
    }))


    val entries = 4 
    val readStackQ  = Module(new Queue(new TLBundleA(node.out.head._2.bundle), entries, flow = false, pipe = false))
    val writeStackQ = Module(new Queue(new TLBundleA(node.out.head._2.bundle), entries, flow = false, pipe = false))
    val arbiter = Module(new RRArbiter(new TLBundleA(node.out.head._2.bundle), 2))
    // AXI in
    // readStack.io.in <> node.in.head._1
    // writeStack.io.in <> node.in.head._1

    readStack.io.in.ar <> node.in.head._1.ar
    readStack.io.in.r <> node.in.head._1.r
    
    writeStack.io.in.aw <> node.in.head._1.aw
    writeStack.io.in.w <> node.in.head._1.w
    writeStack.io.in.b <> node.in.head._1.b

    // TL out
    // arbiter.io.in(0) <> readStack.io.out.a
    // arbiter.io.in(1) <> writeStack.io.out.a

    readStackQ.io.enq <> readStack.io.out.a
    writeStackQ.io.enq <> writeStack.io.out.a
    arbiter.io.in(0) <> readStackQ.io.deq
    arbiter.io.in(1) <> writeStackQ.io.deq
    node.out.head._1.a <> arbiter.io.out




    val out = node.out.head._1
   

    // val d_hasData = node.out.head._2.hasData(out.d.bits)
    val d_hasData = Mux(out.d.bits.opcode === TLMessages.AccessAckData || out.d.bits.opcode === TLMessages.GrantData  ,true.B,false.B)
    // val d_hasData = false.B

    // out.d.ready := Mux(d_hasData, readStack.io.out.d.ready, writeStack.io.out.d.ready)
    out.d.ready := readStack.io.out.d.ready || writeStack.io.out.d.ready

    readStack.io.out.d.valid := out.d.valid && d_hasData
    writeStack.io.out.d.valid := out.d.valid && !d_hasData

    readStack.io.out.d.bits.source   := out.d.bits.source 
    readStack.io.out.d.bits.data := out.d.bits.data
    readStack.io.out.d.bits.param := out.d.bits.param
    readStack.io.out.d.bits.size := out.d.bits.size
    readStack.io.out.d.bits.opcode := out.d.bits.opcode
    readStack.io.out.d.bits.sink := out.d.bits.sink
    readStack.io.out.d.bits.denied := out.d.bits.denied
    readStack.io.out.d.bits.corrupt := out.d.bits.corrupt

    writeStack.io.out.d.bits.source  := out.d.bits.source 
    writeStack.io.out.d.bits.data := out.d.bits.data
    writeStack.io.out.d.bits.param := out.d.bits.param
    writeStack.io.out.d.bits.size := out.d.bits.size
    writeStack.io.out.d.bits.opcode := out.d.bits.opcode
    writeStack.io.out.d.bits.sink := out.d.bits.sink
    writeStack.io.out.d.bits.denied := out.d.bits.denied
    writeStack.io.out.d.bits.corrupt := out.d.bits.corrupt

  }
}
object AXI2TL
{
  def apply(wbufSize:Int, rbufSize:Int)(implicit p: Parameters): MyAXI4ToTLNode = {
    val axi2tl = LazyModule(new AXItoTL(wbufSize,rbufSize))
    axi2tl.node
  }
}

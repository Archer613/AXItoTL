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
  val p:Parameters
  val axi2tlParams = p(AXI2TLParamKey)
  val edgeIn = axi2tlParams.edgeIn
  val edgeOut = axi2tlParams.edgeOut
  val axiAddrBits = axi2tlParams.axiAddrBits
  val axiSizeBits = axi2tlParams.axiSizeBits
  val axiDataBits = axi2tlParams.axiDataBits
  val axiIdBits = axi2tlParams.axiIdBits
  val axiLenBits = axi2tlParams.axiLenBits

  val tlAddrBits = axi2tlParams.tlAddrBits
  val tlSizeBits = axi2tlParams.tlSizeBits
  val tlDataBits = axi2tlParams.tlDataBits
  val sourceBits = axi2tlParams.sourceBits
  val sinkBits = axi2tlParams.sinkBits
  val tlechoFields = axi2tlParams.tlechoFields
  val tlrequestFields = axi2tlParams.tlrequestFields
  val tlresponseFields = axi2tlParams.tlresponseFields
}

class AXItoTL(wbufSize:Int, rbufSize:Int)(implicit p: Parameters) extends LazyModule {
  
  val node = AXI2TLNode(wcorrupt = false, wbufSize, rbufSize)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    require(node.in.length == 1)
    require(node.out.length == 1)
    private val edgeIn = node.in.head._2
    private val edgeOut = node.out.head._2
    private val axiSideParam = node.in.head._2.bundle
    private val tlSideParam = node.out.head._2.bundle
    println("AXI to TL Info:")
    println(s"AXI side:\n\tAddr Width:${axiSideParam.addrBits}\n\tData Width:${axiSideParam.dataBits}\n\tID Width:${axiSideParam.idBits}\n")
    println(s"TL side:\n\tAddr Width:${tlSideParam.addressBits}\n\tData Width:${tlSideParam.dataBits}\n\tSource Width:${tlSideParam.sourceBits}\n")

    private val params = AXI2TLParam(edgeIn = edgeIn, edgeOut = edgeOut, wbufSize = wbufSize, rbufSize = rbufSize)

    private val writeStack = Module(new WriteStack(wbufSize)(p.alterPartial {
      case AXI2TLParamKey => params
    }))
    private val readStack = Module(new ReadStack(rbufSize)(p.alterPartial {
      case AXI2TLParamKey => params
    }))

    private val arbiter = Module(new Arbiter(new TLBundleA(node.out.head._2.bundle), 2))

    readStack.io.in.ar <> node.in.head._1.ar
    readStack.io.in.r <> node.in.head._1.r
    
    writeStack.io.in.aw <> node.in.head._1.aw
    writeStack.io.in.w <> node.in.head._1.w
    writeStack.io.in.b <> node.in.head._1.b

    // TL out
    arbiter.io.in(0) <> readStack.io.out.a
    arbiter.io.in(1) <> writeStack.io.out.a
    node.out.head._1.a <> arbiter.io.out

    private val out = node.out.head._1
   
    // val writeStack_d  = Wire(writeStack.io.out.d)
    // val readStack_d  = Wire(readStack.io.out.d)

    // val d_hasData = node.out.head._2.hasData(out.d.bits)
    private val d_hasData = out.d.bits.opcode === TLMessages.AccessAckData || out.d.bits.opcode === TLMessages.GrantData || out.d.bits.opcode === TLMessages.Get
    // val d_hasData = false.B

    out.d.ready := Mux(d_hasData, readStack.io.out.d.ready, writeStack.io.out.d.ready)

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
  def apply(wbufSize:Int, rbufSize:Int)(implicit p: Parameters): AXI2TLNode = {
    val axi2tl = LazyModule(new AXItoTL(wbufSize, rbufSize))
    axi2tl.node
  }
}

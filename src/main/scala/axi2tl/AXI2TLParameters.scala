package axi2tl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Field
import freechips.rocketchip.amba.axi4._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy.BufferParams
// General parameter key of AXI2TL
case object AXI2TLParamKey extends Field[AXI2TLParam]

case class AXI2TLParam(
  name:String = "AXI2TL",
//  edgeIn: AXI4EdgeParameters,
//  edgeOut: TLEdgeOut,
  wbufSize:Int =16,
  // Performance analysis
  enablePerf: Boolean = false,
  rbufSize:Int = 16,
  hasMbist:Boolean = false,
  hasShareBus:Boolean = false,
  innerTLBuf: TLBufferParams = TLBufferParams(),
  outerTLBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.none,
    d = BufferParams.default,
    e = BufferParams.default
  ),
  innerAXI4Buf: AXI4BufferParams = AXI4BufferParams(),
  outerAXI4Buf:AXI4BufferParams = AXI4BufferParams(
      aw = BufferParams.default,
      w = BufferParams.default,
      b = BufferParams.none,
      ar = BufferParams.default,
      r = BufferParams.none
  )
  
  ){
  val rbufIdBits:Int = log2Ceil(wbufSize)
  val wbufIdBits:Int = log2Ceil(rbufSize)
 
}

case object EdgeInKey extends Field[AXI4EdgeParameters]
case object EdgeOutKey extends Field[TLEdgeOut]


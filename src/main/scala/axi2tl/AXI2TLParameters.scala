package axi2tl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Field
import freechips.rocketchip.amba.axi4._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.util._

// General parameter key of AXI2TL
case object AXI2TLParamKey extends Field[AXI2TLParam]

case class AXI2TLParam(
  name:String = "AXI2TL",
//  edgeIn: AXI4EdgeParameters,
//  edgeOut: TLEdgeOut,
  wbufSize:Int =16,
  // Performance analysis
  enablePerf: Boolean = false,
  rbufSize:Int = 16){
  val rbufIdBits:Int = log2Ceil(wbufSize)
  val wbufIdBits:Int = log2Ceil(rbufSize)

//  val beatBytes:Int = edgeIn.bundle.dataBits / 8
//  val axiAddrBits:Int = edgeIn.bundle.addrBits
//  val axiSizeBits:Int = edgeIn.bundle.sizeBits
//  val axiDataBits:Int = edgeIn.bundle.dataBits
//  val axiIdBits:Int = edgeIn.bundle.idBits
//  val axiLenBits:Int = edgeIn.bundle.lenBits
//
//  val tlAddrBits:Int = edgeOut.bundle.addressBits
//  val tlSizeBits:Int = edgeIn.bundle.sizeBits
//  val tlDataBits:Int = edgeOut.bundle.dataBits
//  val sourceBits:Int = edgeOut.bundle.sourceBits
//  val sinkBits:Int = edgeOut.bundle.sinkBits
//  val tlechoFields: Seq[BundleFieldBase] = edgeOut.bundle.echoFields
//  val tlrequestFields: Seq[BundleFieldBase] = edgeOut.bundle.requestFields
//  val tlresponseFields: Seq[BundleFieldBase] = edgeOut.bundle.responseFields
}

case object EdgeInKey extends Field[AXI4EdgeParameters]
case object EdgeOutKey extends Field[TLEdgeOut]


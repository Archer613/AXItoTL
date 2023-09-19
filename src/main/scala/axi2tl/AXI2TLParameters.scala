package axi2tl

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import chipsalliance.rocketchip.config.Field
import freechips.rocketchip.amba.axi4._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.util._

// General parameter key of AXI2TL
case object AXI2TLParamKey extends Field[AXI2TLParam](AXI2TLParam())

case class AXI2TLParam(
  name:String = "AXI2TL",
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  // Client (these are set in Configs.scala in XiangShan)
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil,

  ridBits: Int = 5,
  aridBits: Int = 14,
  entrydatabits: Int = 256,
  beatBytes: Int = 8,
  fifoBits: Int = 6,
  axiSizeBits: Int = 3,
  axiLenBits: Int = 8,
  stacksize: Int = 8
)

case object EdgeInKey extends Field[AXI4EdgeParameters]

case object EdgeOutKey extends Field[TLEdgeOut]
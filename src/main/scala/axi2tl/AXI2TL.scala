package axi2tl

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}
import freechips.rocketchip.amba._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import  freechips.rocketchip.amba.axi4._

import chisel3.util._

trait HasAXI2TLParameters {
  val p: Parameters
  val axi2tlParams = p(AXI2TLParamKey)

  lazy val edgeIn = p(EdgeInKey)
  lazy val edgeOut = p(EdgeOutKey)

  lazy val axiAddrBits = edgeIn.bundle.addrBits
  lazy val axiSizeBits = edgeIn.bundle.sizeBits
  lazy val axiDataBits = edgeIn.bundle.dataBits
  lazy val axiIdBits = edgeIn.bundle.idBits
  lazy val axiLenBits = edgeIn.bundle.lenBits

  lazy val tlAddrBits = edgeOut.bundle.addressBits
  lazy val tlSizeBits = edgeIn.bundle.sizeBits
  lazy val tlDataBits = edgeOut.bundle.dataBits
  lazy val sourceBits = edgeOut.bundle.sourceBits
}

class AXItoTL(implicit p: Parameters) extends LazyModule with HasAXI2TLParameters {

  val node = AXI4ToTLNode(wcorrupt = false)

  lazy val module = new LazyModuleImp(this) {

    def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
      if (fs.nonEmpty) {
        println(fs.map { f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)" }.mkString("\n"))
      }
    }
    print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")
//    val readStack = Module(new ReadStack(entries = 8))
//    val writeStack = Module(new WriteStack(entries = 8))

    val readStack = Module(new ReadStack(entries = axi2tlParams.readEntriesSize)(p.alterPartial {
        case EdgeInKey => node.in.head._2
        case EdgeOutKey => node.out.head._2
      }))
    val writeStack = Module(new WriteStack(entries = axi2tlParams.writeEntriesSize)(p.alterPartial {
      case EdgeInKey => node.in.head._2
      case EdgeOutKey => node.out.head._2
    }))

    val arbiter = Module(new Arbiter(new TLBundleA(node.out.head._2.bundle), 2))
    // AXI in
    readStack.io.in <> node.in.head._1
    writeStack.io.in <> node.in.head._1

    // TL out
    arbiter.io.in(0) <> readStack.io.out.a
    arbiter.io.in(1) <> writeStack.io.out.a
    node.out.head._1.a <> arbiter.io.out
    readStack.io.out.d <> node.out.head._1.d
    writeStack.io.out.d <> node.out.head._1.d




  }
}

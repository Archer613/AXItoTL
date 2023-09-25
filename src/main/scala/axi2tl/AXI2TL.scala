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

  // val axiAddrBits = p(axiAddrBitsKey)
  // val axiSizeBits = p(axiSizeBitsKey)
  // val axiDataBits = p(axiDataBitsKey)
  // val axiIdBits = p(axiIdBitsKey)
  // val axiLenBits = p(axiLenBitsKey)

  // val tlAddrBits = p(tlAddrBitsKey)
  // val tlSizeBits = p(tlSizeBitsKey)
  // val tlDataBits = p(tlDataBitsKey)
  // val sourceBits = p(sourceBitsKey)
  // val sinkBits = p(sinkBitsKey)
  // val  tlechoFields = p(tlechoFieldsKey)
  // val  tlrequestFields = p(tlrequestFieldsKey)
  // val  tlresponseFields = p(tlresponseFieldsKey)

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
  lazy val sinkBits = edgeOut.bundle.sinkBits
  lazy val  tlechoFields = edgeOut.bundle.echoFields
  lazy val  tlrequestFields = edgeOut.bundle.requestFields
  lazy val  tlresponseFields = edgeOut.bundle.responseFields
}


case class MyAXI4ToTLNode(wcorrupt: Boolean, ridBits: Int)(implicit valName: ValName) extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = {  mp => TLMasterPortParameters.v1(
      clients = mp.masters.map{m =>
          TLMasterParameters.v1(
            name        = s"axitotlnode",
            sourceId    = IdRange(0, 1 << (log2Ceil(m.id.end) + ridBits+1)), // R+W ids are distinct
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


class AXItoTL(implicit p: Parameters) extends LazyModule with HasAXI2TLParameters {
  
  val node = MyAXI4ToTLNode(wcorrupt = false, axi2tlParams.ridBits)

  lazy val module = new LazyModuleImp(this) {

    def print_bundle_fields(fs: Seq[BundleFieldBase], prefix: String) = {
      if (fs.nonEmpty) {
        println(fs.map { f => s"$prefix/${f.key.name}: (${f.data.getWidth}-bit)" }.mkString("\n"))
      }
    }
    // val ei = node.in._2
    // print_bundle_fields(node.in.head._2.bundle.requestFields, "usr")
    // print_bundle_fields(node.in.head._2.bundle.echoFields, "echo")
    // println(s"axiAddrBits: ${ei.}")
    // println(s"axiIdBits: ${axiIdBits}")

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
    // readStack.io.in <> node.in.head._1
    // writeStack.io.in <> node.in.head._1

    readStack.io.in.ar <> node.in.head._1.ar
    readStack.io.in.r <> node.in.head._1.r
    
    writeStack.io.in.aw <> node.in.head._1.aw
    writeStack.io.in.w <> node.in.head._1.w
    writeStack.io.in.b <> node.in.head._1.b

    // TL out
    arbiter.io.in(0) <> readStack.io.out.a
    arbiter.io.in(1) <> writeStack.io.out.a
    node.out.head._1.a <> arbiter.io.out




    val out = node.out.head._1
   
    // val writeStack_d  = Wire(writeStack.io.out.d)
    // val readStack_d  = Wire(readStack.io.out.d)

    // val d_hasData = node.out.head._2.hasData(out.d.bits)
    val d_hasData = Mux(out.d.bits.opcode === TLMessages.AccessAckData || out.d.bits.opcode === TLMessages.GrantData || out.d.bits.opcode === TLMessages.Get ,true.B,false.B)
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

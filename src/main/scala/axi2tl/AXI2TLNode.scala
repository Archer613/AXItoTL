package axi2tl
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
case class AXI2TLNode(wcorrupt: Boolean, wbufSize:Int, rbufSize:Int)(implicit valName: ValName) extends MixedAdapterNode(AXI4Imp, TLImp)(
  dFn = { mp =>
    require(mp.masters.length == 1)
    val sourceIdWidth = Seq(log2Ceil(wbufSize), log2Ceil(rbufSize)).max + 1
    TLMasterPortParameters.v1(
      clients = mp.masters.flatMap { m =>
        val wParams = Seq.tabulate(wbufSize)(id => TLMasterParameters.v1(
          name = s"write_buffer_${id}",
          sourceId = IdRange(id, id + 1),
          nodePath = m.nodePath,
          requestFifo = true
        ))
        val rParams = Seq.tabulate(rbufSize)(id =>{
          val offset = 1 << (sourceIdWidth - 1)
          TLMasterParameters.v1(
            name = s"write_buffer_${id}",
            sourceId = IdRange(offset + id, offset + id + 1),
            nodePath = m.nodePath,
            requestFifo = true
          )
        })
        wParams ++ rParams
      },
      echoFields    = mp.echoFields,
      requestFields = AMBAProtField() +: mp.requestFields,
      responseKeys  = mp.responseKeys)
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

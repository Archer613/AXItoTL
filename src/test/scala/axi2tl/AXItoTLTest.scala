package axi2tl

import chisel3._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.ChiselStage
import xs.utils.{ChiselDB, FileRegisters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._

class TestDMA(implicit p: Parameters) extends LazyModule {

  /*   DMA
   *    |
   *   MEM
   */
  val idBits = 14
  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits),
      maxFlight = Some(16)
    ))
  )))

  // val xbar = TLXbar()
  private val beatBytes = 32
  private val tlSlaveNode = TLManagerNode(
    portParams = Seq(TLSlavePortParameters.v1(
      managers = Seq(TLSlaveParameters.v1(
        address = Seq(AddressSet(0x00000000L, 0x1fffffffffL)),
        resources = (new MemoryDevice).reg("mem"),
        regionType = RegionType.UNCACHED,
        executable = true,
        supportsGet = TransferSizes(1, beatBytes),
        supportsPutPartial = TransferSizes(1, beatBytes),
        supportsPutFull = TransferSizes(1, beatBytes)
      )),
      beatBytes = beatBytes,
      endSinkId = 0
    ))
  )

  tlSlaveNode := AXI2TL(16, 16) := l3FrontendAXI4Node


  val master_nodes = l3FrontendAXI4Node
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    master_nodes.makeIOs()(ValName(s"master_port_"))
    tlSlaveNode.makeIOs()(ValName(s"l3_port_"))
  }
}

object TestDMA extends App {
  val config = new Config((_,_,_) =>{
    case _ => None
  })
  val top = DisableMonitors(p => LazyModule(new TestDMA()(p)))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))
}
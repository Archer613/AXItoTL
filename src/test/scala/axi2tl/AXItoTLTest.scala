package axi2tl

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import utility.{ChiselDB, FileRegisters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import axi2tl._

class TestDMA()(implicit p: Parameters) extends LazyModule {

  /*   DMA
   *    |
   *   MEM
   */

  val delayFactor = 0.5

  val idBits = 14

  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits)
    ))
  )))

  val xbar = TLXbar()
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xffffL), beatBytes = 32))
  ram.node :=
    TLXbar() :=*
      TLFragmenter(32, 64) :=*
      TLCacheCork() :=*
      TLDelayer(delayFactor) :=*
      xbar

  val axi2tlParams = p(AXI2TLParamKey)
  val AXItoTL = LazyModule(new AXItoTL)

  xbar :=
    TLFIFOFixer() :=
    TLWidthWidget(32) :=
    TLBuffer() :=
    AXItoTL.node :=
    AXI4Buffer() :=
    AXI4UserYanker(Some(16)) :=
    AXI4Fragmenter() :=
    AXI4Buffer() :=
    AXI4Buffer() :=
    AXI4IdIndexer(4) :=
    l3FrontendAXI4Node


  val master_nodes = l3FrontendAXI4Node
  lazy val module = new LazyModuleImp(this){
    master_nodes.makeIOs()(ValName(s"master_port_"))
  }

}

object TestDMA extends App {
  val config = new Config((_, _, _) => {
    case AXI2TLParamKey => AXI2TLParam()
  })
  val top = DisableMonitors(p => LazyModule(new TestDMA()(p)) )(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module)
  ))

  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}
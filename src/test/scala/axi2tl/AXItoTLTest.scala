package axi2tl

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.{DisableMonitors, LazyModule}
import org.scalatest.flatspec.AnyFlatSpec
import chipsalliance.rocketchip.config._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import utility.{ChiselDB, FileRegisters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import axi2tl._
import chisel3.{Bool, Bundle, Flipped, UInt}
import chisel3.util.DecoupledIO
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}


class TestDMA()(implicit p: Parameters) extends LazyModule {

  /*   DMA
   *    |
   *   MEM
   */
 def createManagerNode(name: String, sources: Int) = {
  val xfer = TransferSizes(1, 32)
  val slaveNode = TLManagerNode(Seq(
   TLSlavePortParameters.v1(Seq(
    TLManagerParameters(
     address     = Seq(AddressSet(0, 0xffffffffL)),
     regionType    = RegionType.CACHED,
     executable    = true,
     supportsAcquireT = xfer,
     supportsAcquireB = xfer,
     fifoId      = None
    )),
    beatBytes = 32,
    minLatency = 2,
    responseFields = Nil,
    requestKeys = Nil,
    endSinkId = sources
   ))
  )
  slaveNode
 }
  val delayFactor = 0.5

  val idBits = 14
  val device = new SimpleDevice("my-device", Seq("tutorial,my-device0"))
  val l3FrontendAXI4Node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters(
      name = "dma",
      id = IdRange(0, 1 << idBits),
      maxFlight = Some(16)
    ))
  )))

  // val xbar = TLXbar()
  val tlnode = TLManagerNode(
    Seq(
      TLSlavePortParameters.v1(
        Seq(
          TLManagerParameters(
            address = Seq(
              AddressSet(0x00000000L, 0xfffffffffL)
            ),
            resources = device.reg,
            supportsGet = TransferSizes(1, 32),
            supportsPutFull = TransferSizes(1, 32),
            supportsPutPartial = TransferSizes(1, 32),
            fifoId = Some(0)
          )
        ),
        32
      )
    )
  )


  val axi2tlParams = p(AXI2TLParamKey)
  val AXItoTL = LazyModule(new AXItoTL(16,16))
    // TLFIFOFixer() :=

    tlnode :=
    // TLWidthWidget(32) :=
    AXItoTL.node :=
    l3FrontendAXI4Node
    // AXI4UserYanker(Some(16)) :=
    // AXI4Fragmenter() :=
    // AXI4Buffer() :=
    // AXI4Buffer() :=
    // AXI4IdIndexer(4) :=
    // l3FrontendAXI4Node


  val master_nodes = l3FrontendAXI4Node
  lazy val module = new LazyModuleImp(this){
    master_nodes.makeIOs()(ValName(s"master_port"))
    tlnode.makeIOs()(ValName(s"l3_port"))
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
//
  ChiselDB.init(false)
  ChiselDB.addToFileRegisters
  FileRegisters.write("./build")
}
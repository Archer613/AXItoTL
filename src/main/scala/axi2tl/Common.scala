package axi2tl

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.{Bool, Bundle, Flipped, UInt}
import chisel3.util.DecoupledIO
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}

abstract class AXItoTLModule(implicit val p: Parameters) extends MultiIOModule with HasAXI2TLParameters
abstract class AXItoTLBundle(implicit val p: Parameters) extends Bundle with HasAXI2TLParameters

//class AXIInBundle(implicit p : Parameters) extends AXItoTLBundle {
//  val ar = Flipped(DecoupledIO(
//    new AXI4BundleAR(
//      edgeIn.bundle
//    )
//  ))
//  val aw = Flipped(DecoupledIO(
//    new AXI4BundleAW(
//      edgeIn.bundle
//    )
//  ))
//  val w = Flipped(
//    DecoupledIO(
//      new AXI4BundleW(
//        edgeIn.bundle
//      )
//    ))
//  val d = Flipped(DecoupledIO(new TLBundleD(
//    edgeOut.bundle
//  )))
//
//  val b = Flipped(DecoupledIO(
//    new AXI4BundleB(
//      edgeIn.bundle
//    )
//  )
//  )
//}
//
//class AXIOutBundle(implicit p : Parameters) extends AXItoTLBundle{
//  val b = DecoupledIO(
//    new AXI4BundleB(
//      edgeIn.bundle
//    )
//  )
//  val r = DecoupledIO(
//    new AXI4BundleR(
//      edgeIn.bundle
//    )
//  )
//
//  val aw = DecoupledIO(
//    new AXI4BundleAW(edgeIn.bundle)
//  )
//  val w = DecoupledIO(
//    new AXI4BundleW(edgeIn.bundle)
//  )
//  val a = DecoupledIO(new TLBundleA(edgeOut.bundle))
//
//}
//
//
//class MemAXIReqestBundle (implicit p : Parameters) extends AXItoTLBundle {
//  val aw = DecoupledIO(
//    new AXI4BundleAW(edgeIn.bundle)
//  )
//  val w = DecoupledIO(
//    new AXI4BundleW(edgeIn.bundle)
//  )
//}
//class MemAXIRespBundle (implicit p : Parameters) extends AXItoTLBundle {
//  val b = Flipped(
//    DecoupledIO(
//      new AXI4BundleB(edgeIn.bundle)))
//}
//
//class AXIWriterRequestBundle(implicit p : Parameters) extends AXItoTLBundle {
//  val aw = Flipped(DecoupledIO(
//    new AXI4BundleAW(edgeIn.bundle)
//  ))
//  val w = Flipped(
//    DecoupledIO(
//      new AXI4BundleW(edgeIn.bundle)
//    ))
//}
//class AXIWriteRespBundle(implicit p : Parameters) extends AXItoTLBundle{
//  val b =DecoupledIO(
//    new AXI4BundleB(
//      edgeIn.bundle
//    )
//  )
//}
//
//
//class TLRequestBundel(implicit p : Parameters) extends AXItoTLBundle {
//  val a = DecoupledIO(new TLBundleA(edgeOut.bundle))
//}
//
//class AXIRequestBundel(implicit p : Parameters) extends AXItoTLBundle {
//  val ar = Flipped(DecoupledIO(
//    new AXI4BundleAR(edgeIn.bundle)
//  ))
//
//
//}
//
//class TLRespBundle(implicit p : Parameters) extends AXItoTLBundle {
//  val d = Flipped(DecoupledIO(new TLBundleD(edgeOut.bundle)))
//}
//
//class AXIRespBundle(implicit p : Parameters) extends AXItoTLBundle {
//  val r = DecoupledIO(
//    new AXI4BundleR(
//      edgeIn.bundle
//    )
//  )
//}
package axi2tl

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.{Bool, Bundle, Flipped, UInt}
import chisel3.util.DecoupledIO
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}

abstract class AXItoTLModule(implicit val p: Parameters) extends Module with HasAXI2TLParameters
abstract class AXItoTLBundle(implicit val p: Parameters) extends Bundle with HasAXI2TLParameters


package axi2tl

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.{Bool, Bundle, Flipped, UInt}
import chisel3.util.DecoupledIO
import freechips.rocketchip.amba.axi4.{AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleParameters, AXI4BundleR, AXI4BundleW}
import freechips.rocketchip.tilelink.{TLBundleA, TLBundleD, TLBundleParameters}

abstract class AXItoTLModule(implicit val p: Parameters) extends Module with HasAXI2TLParameters
abstract class AXItoTLBundle(implicit val p: Parameters) extends Bundle with HasAXI2TLParameters


class AXIIn(implicit p:Parameters) extends AXItoTLBundle{
    val ar = Flipped(DecoupledIO(
        new AXI4BundleAR(new AXI4BundleParameters(
          addrBits = axiAddrBits,
          dataBits = axiDataBits,
          idBits = axiIdBits
        ))
      ))
    val aw = Flipped(DecoupledIO(
        new AXI4BundleAW(new AXI4BundleParameters(
          addrBits = axiAddrBits,
          dataBits = axiDataBits,
          idBits = axiIdBits
        ))
    ))
    val w = Flipped(
        DecoupledIO(
          new AXI4BundleW(new AXI4BundleParameters(
            addrBits = axiAddrBits,
            dataBits = axiDataBits,
            idBits = axiIdBits
          ))
    ))

    val r = DecoupledIO(
        new AXI4BundleR(
        new AXI4BundleParameters(
           addrBits = axiAddrBits,
           dataBits = axiDataBits,
           idBits = axiIdBits
        )
        )
    )

    val b = DecoupledIO(
    new AXI4BundleB(
      new AXI4BundleParameters(
        addrBits = axiAddrBits,
        dataBits = axiDataBits,
        idBits = axiIdBits
      )
    )
  )

}


class TLOut(implicit  p: Parameters) extends  AXItoTLBundle{
     val a = DecoupledIO(new TLBundleA(new TLBundleParameters(
            addressBits = tlAddrBits,
            dataBits = tlDataBits, 
            sourceBits = sourceBits, 
            sinkBits = sinkBits, 
            sizeBits = tlSizeBits, 
            echoFields = tlechoFields, 
            requestFields = tlrequestFields, 
            responseFields = tlresponseFields, 
            hasBCE = false
  )))
    val d = Flipped(DecoupledIO(new TLBundleD(new TLBundleParameters(
   addressBits = tlAddrBits,
            dataBits = tlDataBits, 
            sourceBits = sourceBits, 
            sinkBits = sinkBits, 
            sizeBits = tlSizeBits, 
            echoFields = tlechoFields, 
            requestFields = tlrequestFields, 
            responseFields = tlresponseFields, 
            hasBCE = false
  ))))
}

class ReadStackIn(implicit  p:Parameters) extends AXItoTLBundle{
    val ar = Flipped(DecoupledIO(
        new AXI4BundleAR(new AXI4BundleParameters(
          addrBits = axiAddrBits,
          dataBits = axiDataBits,
          idBits = axiIdBits
        ))
      ))
  
    val r = DecoupledIO(
        new AXI4BundleR(
        new AXI4BundleParameters(
           addrBits = axiAddrBits,
           dataBits = axiDataBits,
           idBits = axiIdBits
        )
        )
    )


}

class WriteStackIn(implicit  p:Parameters) extends AXItoTLBundle{
    val aw = Flipped(DecoupledIO(
        new AXI4BundleAW(new AXI4BundleParameters(
          addrBits = axiAddrBits,
          dataBits = axiDataBits,
          idBits = axiIdBits
        ))
    ))
    val w = Flipped(
        DecoupledIO(
          new AXI4BundleW(new AXI4BundleParameters(
            addrBits = axiAddrBits,
            dataBits = axiDataBits,
            idBits = axiIdBits
          ))
    ))

    val b = DecoupledIO(
    new AXI4BundleB(
      new AXI4BundleParameters(
        addrBits = axiAddrBits,
        dataBits = axiDataBits,
        idBits = axiIdBits
      )
    )
  )

}
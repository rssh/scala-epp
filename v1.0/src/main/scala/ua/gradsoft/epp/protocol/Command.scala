package ua.gradsoft.epp.protocol

import ua.gradsoft.xmlbinding.XMLCodec

trait Command


case class EppResult(code:Int, msg:Option[String], lang:Option[String])


case class EppResponse(
               result: EppResult,
               msg: Option[String],
               trId: Option[TrId]
 )

case class TrId(clTRID:String, svTRID:String) derives XMLCodec


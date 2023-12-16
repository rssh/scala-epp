package ua.gradsoft.epp.domain

import java.util.Calendar

case class HostInfo(
                     val name: String,
                     val roid: Option[String] = None,
                     val statuses: Seq[String] = Seq(), // TODO: status will be enum
                     val addrsIp4: Seq[String] = Seq(),
                     val addrsIp6:  Seq[String] = Seq(),
                     val clId: Option[String] = None,
                     val crId: Option[String] = None,
                     val crDate: Option[Calendar] = None,
                     val upId: Option[String] = None,
                     val upDate: Option[Calendar] = None,
                     val trDate: Option[Calendar] = None
                   )


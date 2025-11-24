package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.InfDataTypeType
import ua.gradsoft.epp.util.EppXmlUtil
import java.time.Instant

case class ContactInfo(
  id: String,
  roid: String,
  email: String,
  sponsoringClientID: String,
  creatingClientID: String,
  lastUpdateClientID: Option[String],
  creationDate: Instant,
  lastUpdateDate: Option[Instant],
  lastTransferDate: Option[Instant]
)

object ContactInfo {

  def fromInfDataType(infData: InfDataTypeType): ContactInfo = {
    ContactInfo(
      id = infData.id,
      roid = infData.roid,
      email = infData.email,
      sponsoringClientID = infData.clID,
      creatingClientID = infData.crID,
      lastUpdateClientID = infData.upID,
      creationDate = EppXmlUtil.fromXMLGregorianCalendar(infData.crDate),
      lastUpdateDate = infData.upDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      lastTransferDate = infData.trDate.map(EppXmlUtil.fromXMLGregorianCalendar)
    )
  }

}

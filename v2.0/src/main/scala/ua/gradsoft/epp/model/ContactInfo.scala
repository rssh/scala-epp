package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.InfDataTypeType
import ua.gradsoft.epp.util.EppXmlUtil
import java.time.Instant

/**
 * Phone number with optional extension
 */
case class PhoneNumber(
  number: String,
  extension: Option[String] = None
)

/**
 * Full EPP Contact information
 */
case class ContactInfo(
  id: String,
  roid: String,
  status: Seq[ContactStatus],
  postalInfo: Seq[PostalInfo],
  voice: Option[PhoneNumber],
  fax: Option[PhoneNumber],
  email: String,
  sponsoringClientID: String,
  creatingClientID: String,
  lastUpdateClientID: Option[String],
  creationDate: Instant,
  lastUpdateDate: Option[Instant],
  lastTransferDate: Option[Instant]
):
  /** Get local postal info if available */
  def localPostalInfo: Option[PostalInfo] =
    postalInfo.find(_.infoType == PostalInfoType.Local)

  /** Get international postal info if available */
  def internationalPostalInfo: Option[PostalInfo] =
    postalInfo.find(_.infoType == PostalInfoType.International)

  /** Get preferred postal info (international first, then local) */
  def preferredPostalInfo: Option[PostalInfo] =
    internationalPostalInfo.orElse(localPostalInfo)

object ContactInfo:

  def fromInfDataType(infData: InfDataTypeType): ContactInfo =
    ContactInfo(
      id = infData.id,
      roid = infData.roid,
      status = infData.status.map(s => ContactStatus.fromStatusValueType(s.s)),
      postalInfo = infData.postalInfo.map(PostalInfo.fromPostalInfoType),
      voice = infData.voice.map(v => PhoneNumber(v.value, v.x)),
      fax = infData.fax.map(f => PhoneNumber(f.value, f.x)),
      email = infData.email,
      sponsoringClientID = infData.clID,
      creatingClientID = infData.crID,
      lastUpdateClientID = infData.upID,
      creationDate = EppXmlUtil.fromXMLGregorianCalendar(infData.crDate),
      lastUpdateDate = infData.upDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      lastTransferDate = infData.trDate.map(EppXmlUtil.fromXMLGregorianCalendar)
    )

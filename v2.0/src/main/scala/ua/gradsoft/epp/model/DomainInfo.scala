package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.{InfDataTypeType2, NsType, ContactType => XsdContactType, ContactAttrType => XsdContactAttrType, StatusTypeType2, AuthInfoTypeType, PwAuthInfoType, Admin, Billing, Tech, ClientDeleteProhibitedValue2, ClientHold, ClientRenewProhibited, ClientTransferProhibitedValue, ClientUpdateProhibitedValue2, Inactive, OkValue2, PendingCreateValue2, PendingDeleteValue2, PendingRenew, PendingTransferValue2, PendingUpdateValue2, ServerDeleteProhibitedValue2, ServerHold, ServerRenewProhibited, ServerTransferProhibitedValue, ServerUpdateProhibitedValue2, AutoRenewGracePeriod, RedemptionPeriod}
import ua.gradsoft.epp.util.EppXmlUtil
import java.time.Instant

case class DomainContact(
  id: String,
  contactType: Option[ContactType]
)

case class DomainStatusInfo(
    status: DomainStatus,
    message: Option[String]
)

case class DomainInfo(
  name: String,
  roid: String,
  status: Seq[DomainStatusInfo], 
  registrant: Option[String],
  contacts: Seq[DomainContact],
  nameservers: Seq[String],
  hosts: Seq[String], 
  sponsoringClientID: String,
  creatingClientID: Option[String],
  creationDate: Option[Instant],
  lastUpdateClientID: Option[String],
  lastUpdateDate: Option[Instant],
  expirationDate: Option[Instant],
  lastTransferDate: Option[Instant],
  license: Option[String], 
  authInfo: Option[String] 
)

object DomainInfo {

  def fromInfDataType(infData: InfDataTypeType2): DomainInfo = {
    val nameservers = infData.ns match {
      case Some(ns) => ns.nstypeoption.collect {
        case scalaxb.DataRecord(_, Some("hostObj"), hostObj: String) => hostObj
      }
      case None => Seq.empty
    }

    val contacts = infData.contact.map { c =>
      val contactType = c.typeValue.map {
        case Admin => ContactType.Admin
        case Billing => ContactType.Billing
        case Tech => ContactType.Tech
      }
      DomainContact(
        id = c.value,
        contactType = contactType
      )
    }

    val authInfoPassword = infData.authInfo.flatMap { auth =>
      auth.authinfotypetypeoption.value match {
        case pw: PwAuthInfoType => Some(pw.value)
        case _ => None
      }
    }

    val statuses = infData.status.map{ s =>
        val status = s.s match {
            case ClientDeleteProhibitedValue2 => DomainStatus.ClientDeleteProhibited
            case ClientHold => DomainStatus.ClientHold
            case ClientRenewProhibited => DomainStatus.ClientRenewProhibited
            case ClientTransferProhibitedValue => DomainStatus.ClientTransferProhibited
            case ClientUpdateProhibitedValue2 => DomainStatus.ClientUpdateProhibited
            case Inactive => DomainStatus.Inactive
            case OkValue2 => DomainStatus.Ok
            case PendingCreateValue2 => DomainStatus.PendingCreate
            case PendingDeleteValue2 => DomainStatus.PendingDelete
            case PendingRenew => DomainStatus.PendingRenew
            case PendingTransferValue2 => DomainStatus.PendingTransfer
            case PendingUpdateValue2 => DomainStatus.PendingUpdate
            case ServerDeleteProhibitedValue2 => DomainStatus.ServerDeleteProhibited
            case ServerHold => DomainStatus.ServerHold
            case ServerRenewProhibited => DomainStatus.ServerRenewProhibited
            case ServerTransferProhibitedValue => DomainStatus.ServerTransferProhibited
            case ServerUpdateProhibitedValue2 => DomainStatus.ServerUpdateProhibited
            case AutoRenewGracePeriod => DomainStatus.AutoRenewGracePeriod
            case RedemptionPeriod => DomainStatus.RedemptionPeriod
        }
        DomainStatusInfo(status, Some(s.value))
    }

    DomainInfo(
      name = infData.name,
      roid = infData.roid,
      status = statuses, 
      registrant = infData.registrant,
      contacts = contacts,
      nameservers = nameservers,
      hosts = infData.host, 
      sponsoringClientID = infData.clID,
      creatingClientID = infData.crID,
      creationDate = infData.crDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      lastUpdateClientID = infData.upID,
      lastUpdateDate = infData.upDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      expirationDate = infData.exDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      lastTransferDate = infData.trDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      license = infData.license, 
      authInfo = authInfoPassword 
    )
  }

}

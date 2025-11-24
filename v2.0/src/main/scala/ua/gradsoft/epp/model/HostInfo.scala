package ua.gradsoft.epp.model

import java.time.Instant
import ua.gradsoft.epp.util.EppXmlUtil
import ua.gradsoft.epp.xsdmodel.{InfDataType, AddrType, IpType, StatusType, ClientDeleteProhibited, ClientUpdateProhibited, Linked, Ok, PendingCreate, PendingDelete, PendingTransfer, PendingUpdate, ServerDeleteProhibited, ServerUpdateProhibited}

case class HostStatusInfo(
    status: HostStatus,
    message: Option[String]
)

case class HostInfo(
  name: String,
  roid: String,
  status: Seq[HostStatusInfo],
  addresses: Map[String, IpType],
  sponsoringClientID: String,
  creatingClientID: String,
  creationDate: Instant,
  lastUpdateClientID: Option[String],
  lastUpdateDate: Option[Instant],
  lastTransferDate: Option[Instant]
)

object HostInfo {

  def fromInfDataType(infData: InfDataType): HostInfo = {
    val addresses = infData.addr.map(a => (a.value, a.ip)).toMap
    val statuses = infData.status.map{ s =>
        val status = s.s match {
            case ClientDeleteProhibited => HostStatus.ClientDeleteProhibited
            case ClientUpdateProhibited => HostStatus.ClientUpdateProhibited
            case Linked => HostStatus.Linked
            case Ok => HostStatus.Ok
            case PendingCreate => HostStatus.PendingCreate
            case PendingDelete => HostStatus.PendingDelete
            case PendingTransfer => HostStatus.PendingTransfer
            case PendingUpdate => HostStatus.PendingUpdate
            case ServerDeleteProhibited => HostStatus.ServerDeleteProhibited
            case ServerUpdateProhibited => HostStatus.ServerUpdateProhibited
        }
        HostStatusInfo(status, Some(s.value))
    }
    HostInfo(
      name = infData.name,
      roid = infData.roid,
      status = statuses,
      addresses = addresses,
      sponsoringClientID = infData.clID,
      creatingClientID = infData.crID,
      creationDate = EppXmlUtil.fromXMLGregorianCalendar(infData.crDate),
      lastUpdateClientID = infData.upID,
      lastUpdateDate = infData.upDate.map(EppXmlUtil.fromXMLGregorianCalendar),
      lastTransferDate = infData.trDate.map(EppXmlUtil.fromXMLGregorianCalendar)
    )
  }

}

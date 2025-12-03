package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.TrnDataType
import ua.gradsoft.epp.util.EppXmlUtil
import java.time.Instant

/**
 * Result of a contact transfer operation
 *
 * @param id Contact ID
 * @param transferStatus Current transfer status
 * @param requestingClientId ID of the client that requested the transfer
 * @param requestDate Date when transfer was requested
 * @param actingClientId ID of the client that should act on the transfer
 * @param actionDate Date by which the acting client must respond
 */
case class ContactTransferResult(
  id: String,
  transferStatus: TransferStatus,
  requestingClientId: String,
  requestDate: Instant,
  actingClientId: String,
  actionDate: Instant
)

object ContactTransferResult:
  def fromTrnDataType(data: TrnDataType): ContactTransferResult =
    ContactTransferResult(
      id = data.id,
      transferStatus = TransferStatus.fromTrStatusType(data.trStatus),
      requestingClientId = data.reID,
      requestDate = EppXmlUtil.fromXMLGregorianCalendar(data.reDate),
      actingClientId = data.acID,
      actionDate = EppXmlUtil.fromXMLGregorianCalendar(data.acDate)
    )

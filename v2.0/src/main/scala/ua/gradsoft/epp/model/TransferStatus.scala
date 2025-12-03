package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.*

/**
 * EPP Transfer status values (common for contact and domain)
 */
enum TransferStatus:
  case ClientApproved
  case ClientCancelled
  case ClientRejected
  case Pending
  case ServerApproved
  case ServerCancelled

object TransferStatus:
  def fromTrStatusType(status: TrStatusType): TransferStatus =
    status match
      case ua.gradsoft.epp.xsdmodel.ClientApproved => TransferStatus.ClientApproved
      case ua.gradsoft.epp.xsdmodel.ClientCancelled => TransferStatus.ClientCancelled
      case ua.gradsoft.epp.xsdmodel.ClientRejected => TransferStatus.ClientRejected
      case ua.gradsoft.epp.xsdmodel.Pending => TransferStatus.Pending
      case ua.gradsoft.epp.xsdmodel.ServerApproved => TransferStatus.ServerApproved
      case ua.gradsoft.epp.xsdmodel.ServerCancelled => TransferStatus.ServerCancelled

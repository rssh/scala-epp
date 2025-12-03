package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.StatusValueTypeType

/**
 * EPP Contact status values
 */
enum ContactStatus:
  case ClientDeleteProhibited
  case ClientTransferProhibited
  case ClientUpdateProhibited
  case Linked
  case Ok
  case PendingCreate
  case PendingDelete
  case PendingTransfer
  case PendingUpdate
  case ServerDeleteProhibited
  case ServerTransferProhibited
  case ServerUpdateProhibited

object ContactStatus:
  def fromStatusValueType(status: StatusValueTypeType): ContactStatus =
    import ua.gradsoft.epp.xsdmodel as xsd
    status match
      case xsd.ClientDeleteProhibitedValue => ContactStatus.ClientDeleteProhibited
      case xsd.ClientTransferProhibited => ContactStatus.ClientTransferProhibited
      case xsd.ClientUpdateProhibitedValue => ContactStatus.ClientUpdateProhibited
      case xsd.LinkedValue => ContactStatus.Linked
      case xsd.OkValue => ContactStatus.Ok
      case xsd.PendingCreateValue => ContactStatus.PendingCreate
      case xsd.PendingDeleteValue => ContactStatus.PendingDelete
      case xsd.PendingTransferValue => ContactStatus.PendingTransfer
      case xsd.PendingUpdateValue => ContactStatus.PendingUpdate
      case xsd.ServerDeleteProhibitedValue => ContactStatus.ServerDeleteProhibited
      case xsd.ServerTransferProhibited => ContactStatus.ServerTransferProhibited
      case xsd.ServerUpdateProhibitedValue => ContactStatus.ServerUpdateProhibited

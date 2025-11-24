package ua.gradsoft.epp.model

enum DomainStatus {
  case ClientDeleteProhibited,
       ClientHold,
       ClientRenewProhibited,
       ClientTransferProhibited,
       ClientUpdateProhibited,
       Inactive,
       Ok,
       PendingCreate,
       PendingDelete,
       PendingRenew,
       PendingTransfer,
       PendingUpdate,
       ServerDeleteProhibited,
       ServerHold,
       ServerRenewProhibited,
       ServerTransferProhibited,
       ServerUpdateProhibited,
       AutoRenewGracePeriod,
       RedemptionPeriod
}

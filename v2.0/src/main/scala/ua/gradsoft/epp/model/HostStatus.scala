package ua.gradsoft.epp.model

enum HostStatus {
  case ClientDeleteProhibited,
       ClientUpdateProhibited,
       Linked,
       Ok,
       PendingCreate,
       PendingDelete,
       PendingTransfer,
       PendingUpdate,
       ServerDeleteProhibited,
       ServerUpdateProhibited
}

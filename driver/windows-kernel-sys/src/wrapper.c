#include "wrapper.h"

ULONG wrapper_MmGetMdlByteCount(
  PMDL Mdl
){
  return MmGetMdlByteCount(Mdl);
}

PVOID wrapper_MmGetSystemAddressForMdlSafe(
  PMDL  Mdl,
  ULONG Priority
) {
  return MmGetSystemAddressForMdlSafe(Mdl, Priority);
}

PNET_BUFFER wrapper_NET_BUFFER_LIST_FirstNb(PNET_BUFFER_LIST Nbl) {
  return NET_BUFFER_LIST_FIRST_NB(Nbl);
}

PNET_BUFFER_LIST wrapper_NET_BUFFER_LIST_NextNbl(PNET_BUFFER_LIST Nbl) {
  return NET_BUFFER_LIST_NEXT_NBL(Nbl);
}

VOID wrapper_NdisGetNextMdl(PMDL CurrentMdl, PMDL *NextMdl) {
  NdisGetNextMdl(CurrentMdl, NextMdl);
}

VOID wrapper_NdisQueryMdl(
  PMDL Mdl,
  PVOID *VirtualAddress,
  ULONG *Length,
  ULONG Priority
) {
  NdisQueryMdl(Mdl, VirtualAddress, Length, Priority);
}

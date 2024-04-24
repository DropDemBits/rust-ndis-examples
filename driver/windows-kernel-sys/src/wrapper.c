#include "wrapper.h"

PIO_STACK_LOCATION wrapper_IoGetCurrentIrpStackLocation(
  PIRP Irp
) {
  return IoGetCurrentIrpStackLocation(Irp);
}

ULONG wrapper_MmGetMdlByteCount(
  const MDL* Mdl
){
  return MmGetMdlByteCount(Mdl);
}

ULONG wrapper_MmGetMdlByteOffset(
  const MDL* Mdl
){
  return MmGetMdlByteOffset(Mdl);
}

PVOID wrapper_MmGetSystemAddressForMdlSafe(
  PMDL  Mdl,
  ULONG Priority
) {
  return MmGetSystemAddressForMdlSafe(Mdl, Priority);
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

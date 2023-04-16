// Bindgen doesn't like feeding these in beforehand
#define _WIN64
#define AMD64
#define _AMD64_

#include <ntddk.h>
#include <wdf.h>

// Workaround for https://github.com/rust-lang/rust-bindgen/issues/316
// based on https://github.com/rust-lang/rust-bindgen/issues/753#issuecomment-459851952
#define RENAME_TYPED(ty,c) static const ty __rename_typed_##c = c;

RENAME_TYPED(NTSTATUS,STATUS_SUCCESS);
RENAME_TYPED(NTSTATUS,STATUS_UNSUCCESSFUL);


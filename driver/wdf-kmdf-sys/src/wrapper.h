// Bindgen doesn't like feeding these in beforehand
// #define _WIN64
#define AMD64
#define _AMD64_

#define NTDDI_VERSION (0x0A000008) // Win10 2004
#define _WIN32_WINNT (0x0A00)
#define WINVER (0x0A00)
#define DEPRECATE_DDK_FUNCTIONS 1

// Minimal WDF headers
#include <wdm.h>
#include <ntdef.h>

#define KMDF_VERSION_MINOR 31
#define KMDF_MINIMUM_VERSION_REQUIRED 31
#include <wdf.h>
#include <wdfminiport.h>

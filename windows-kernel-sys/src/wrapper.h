// Bindgen doesn't like feeding these in beforehand
// #define _WIN64
#define AMD64
#define _AMD64_

#define NTDDI_VERSION (0x0A000008) // Win10 2004
#define _WIN32_WINNT (0x0A00)
#define WINVER (0x0A00)
#define DEPRECATE_DDK_FUNCTIONS 1

// NDIS
#define NDIS_WDM
#define NDIS_MINIPORT_DRIVER 1

// Only support NDIS 6.82 right now
#define NDIS682_MINIPORT 1
#define NDIS682 1

//   Headers   //

#include <ntddk.h>
// #include <ndis.h>
// see build.rs file for more details
#include <header_hacks/ndis.h>
#include <wdmsec.h>

// Bindgen doesn't like feeding these in beforehand
// #define _WIN64
#define AMD64
#define _AMD64_

#define NTDDI_VERSION (0x0A000008) // Win10 2004
#define _WIN32_WINNT (0x0A00)
#define WINVER (0x0A00)
#define DEPRECATE_DDK_FUNCTIONS 1
#include <ntddk.h>

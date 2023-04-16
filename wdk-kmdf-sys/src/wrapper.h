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

typedef union _KGDTENTRY64
{
	struct
	{
		unsigned short LimitLow;
		unsigned short BaseLow;
		union
		{
			struct
			{
				unsigned char BaseMiddle;
				unsigned char Flags1;
				unsigned char Flags2;
				unsigned char BaseHigh;
			} Bytes;
			struct
			{
				unsigned long BaseMiddle : 8;
				unsigned long Type : 5;
				unsigned long Dpl : 2;
				unsigned long Present : 1;
				unsigned long LimitHigh : 4;
				unsigned long System : 1;
				unsigned long LongMode : 1;
				unsigned long DefaultBig : 1;
				unsigned long Granularity : 1;
				unsigned long BaseHigh : 8;
			} Bits;
		};
		unsigned long BaseUpper;
		unsigned long MustBeZero;
	};
	unsigned __int64 Alignment;
} KGDTENTRY64, *PKGDTENTRY64;

typedef union _KIDTENTRY64
{
	struct
	{
		unsigned short OffsetLow;
		unsigned short Selector;
		unsigned short IstIndex : 3;
		unsigned short Reserved0 : 5;
		unsigned short Type : 5;
		unsigned short Dpl : 2;
		unsigned short Present : 1;
		unsigned short OffsetMiddle;
		unsigned long OffsetHigh;
		unsigned long Reserved1;
	};
	unsigned __int64 Alignment;
} KIDTENTRY64, *PKIDTENTRY64;

#![no_std]

extern crate alloc;

pub mod allocator {
    //! Global kernel allocators
    //! Modified from <https://github.com/not-matthias/kernel-alloc-rs/blob/19b2b992c0f0dacf60ba60e758929809f85b5790/src/lib.rs>
    use core::alloc::{GlobalAlloc, Layout};

    use windows_kernel_sys::{ExAllocatePool2, ExFreePool, POOL_FLAG_NON_PAGED};

    const POOL_TAG: u32 = u32::from_be_bytes(*b"Rust");

    /// The global kernel allocator structure.
    pub struct KernelAlloc;

    unsafe impl GlobalAlloc for KernelAlloc {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            // Defer error handling to clients
            ExAllocatePool2(POOL_FLAG_NON_PAGED, layout.size() as u64, POOL_TAG).cast()
        }

        unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
            ExFreePool(ptr.cast());
        }
    }
}

pub mod log {
    //! Logging helpers

    // Reexport for convenience
    pub use log::*;

    use windows_kernel_sys::{
        DbgPrintEx, DPFLTR_ERROR_LEVEL, DPFLTR_INFO_LEVEL, DPFLTR_TRACE_LEVEL, DPFLTR_TYPE,
        DPFLTR_WARNING_LEVEL,
    };

    pub const COMPONENT_IHVBUS: u32 = DPFLTR_TYPE::DPFLTR_IHVBUS_ID.0 as u32;
    pub const COMPONENT_IHVAUDIO: u32 = DPFLTR_TYPE::DPFLTR_IHVAUDIO_ID.0 as u32;
    pub const COMPONENT_IHVVIDEO: u32 = DPFLTR_TYPE::DPFLTR_IHVVIDEO_ID.0 as u32;
    pub const COMPONENT_IHVDRIVER: u32 = DPFLTR_TYPE::DPFLTR_IHVDRIVER_ID.0 as u32;
    pub const COMPONENT_IHVNETWORK: u32 = DPFLTR_TYPE::DPFLTR_IHVNETWORK_ID.0 as u32;
    pub const COMPONENT_IHVSTREAMING: u32 = DPFLTR_TYPE::DPFLTR_IHVSTREAMING_ID.0 as u32;

    // Borrowed from the kernel_log crate, made to be more resilient to
    // allocation failures, as well as specifying component id
    pub struct KernelLogger<const COMPONENT_ID: u32>;

    // FIXME: Investigate using IoAllocateErrorLogEntry too
    // on log init failure
    #[doc(hidden)]
    pub fn report_init_fail<const COMPONENT_ID: u32>() {
        unsafe {
            DbgPrintEx(
                COMPONENT_ID,
                DPFLTR_ERROR_LEVEL,
                b"Failed to initialize kernel logger\0".as_ptr().cast(),
            )
        };
    }

    /// Initializes the kernel logging infrastructure, based off of `DbgPrintEx`
    #[macro_export(local_inner_macros)]
    macro_rules! init_kernel_logger {
        ($component_id:expr, $level:expr) => {{
            static LOGGER: $crate::log::KernelLogger<{ $component_id }> = $crate::log::KernelLogger;

            match log::set_logger(&LOGGER) {
                Ok(()) => $crate::log::set_max_level($level),
                Err(_) => $crate::log::report_init_fail::<{ $component_id }>(),
            }
        };};
    }

    impl<const COMPONENT_ID: u32> log::Log for KernelLogger<COMPONENT_ID> {
        fn enabled(&self, _metadata: &Metadata) -> bool {
            true
        }

        fn log(&self, record: &Record) {
            use core::fmt::Write;

            if self.enabled(record.metadata()) {
                // DbgPrint(Ex) buffer is specified to be at least 512 bytes long
                // per invocation
                let mut message_buf = heapless::String::<512>::new();

                let status = write!(
                    &mut message_buf,
                    "{:<5} [{}] {}\n\0",
                    record.level(),
                    record.target(),
                    record.args()
                );

                let real_level = match record.level() {
                    Level::Error => DPFLTR_ERROR_LEVEL,
                    Level::Warn => DPFLTR_WARNING_LEVEL,
                    Level::Info => DPFLTR_INFO_LEVEL,
                    // There isn't a debug level, so these get to share
                    Level::Debug | Level::Trace => DPFLTR_TRACE_LEVEL,
                };

                if status.is_ok() {
                    unsafe { DbgPrintEx(COMPONENT_ID, real_level, message_buf.as_ptr().cast()) };
                } else {
                    unsafe {
                        DbgPrintEx(
                            COMPONENT_ID,
                            DPFLTR_ERROR_LEVEL,
                            b"ERROR [windows_kernel_rs] overflow while formatting message buffer\n\0".as_ptr().cast(),
                        )
                    };

                    // Do a breakpoint so that we can still log some information
                    unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
                }
            }
        }

        fn flush(&self) {}
    }
}

pub use nt_string as string;
use windows_kernel_sys::PLONGLONG;

pub mod ioctl {
    use windows_kernel_sys::{
        FILE_ANY_ACCESS, FILE_READ_ACCESS, FILE_WRITE_ACCESS, METHOD_BUFFERED, METHOD_IN_DIRECT,
        METHOD_NEITHER, METHOD_OUT_DIRECT,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct IoControlCode {
        device_type: DeviceType,
        required_access: RequiredAccess,
        function_code: u16,
        transfer_method: TransferMethod,
    }

    impl IoControlCode {
        // borrowed from https://codentium.com/guides/windows-dev/windows-drivers-in-rust-io-controls/
        const METHOD_BITS: usize = 2;
        const NUM_BITS: usize = 12;
        const ACCESS_BITS: usize = 2;
        const TYPE_BITS: usize = 16;

        const METHOD_SHIFT: usize = 0;
        const NUM_SHIFT: usize = Self::METHOD_SHIFT + Self::METHOD_BITS;
        const ACCESS_SHIFT: usize = Self::NUM_SHIFT + Self::NUM_BITS;
        const TYPE_SHIFT: usize = Self::ACCESS_SHIFT + Self::ACCESS_BITS;

        const METHOD_MASK: u32 = (1 << Self::METHOD_BITS) - 1;
        const NUM_MASK: u32 = (1 << Self::NUM_BITS) - 1;
        const ACCESS_MASK: u32 = (1 << Self::ACCESS_BITS) - 1;
        const TYPE_MASK: u32 = (1 << Self::TYPE_BITS) - 1;

        pub const fn new(
            device_type: DeviceType,
            function_code: u16,
            transfer_method: TransferMethod,
            required_access: RequiredAccess,
        ) -> Self {
            assert!(function_code < 0xFFF);

            Self {
                device_type,
                required_access,
                function_code,
                transfer_method,
            }
        }

        pub fn device_type(self) -> DeviceType {
            self.device_type
        }

        pub fn required_access(self) -> RequiredAccess {
            self.required_access
        }

        pub fn function_code(self) -> u16 {
            self.function_code
        }

        pub fn transfer_method(self) -> TransferMethod {
            self.transfer_method
        }
    }

    impl From<u32> for IoControlCode {
        fn from(value: u32) -> Self {
            let method = (value >> Self::METHOD_SHIFT) & Self::METHOD_MASK;
            let function_code = ((value >> Self::NUM_SHIFT) & Self::NUM_MASK) as u16;
            let access = (value >> Self::ACCESS_SHIFT) & Self::ACCESS_MASK;
            let ty = ((value >> Self::TYPE_SHIFT) & Self::TYPE_MASK) as u16;

            Self {
                device_type: ty.into(),
                required_access: RequiredAccess::from_bits(access as u8)
                    .unwrap_or(RequiredAccess::Read),
                function_code,
                transfer_method: method.try_into().unwrap_or(TransferMethod::Buffered),
            }
        }
    }

    bitflags::bitflags! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct RequiredAccess: u8 {
           const Any = FILE_ANY_ACCESS as u8;
           const Read = FILE_READ_ACCESS as u8;
           const Write = FILE_WRITE_ACCESS as u8;
           const ReadWrite = FILE_READ_ACCESS as u8 | FILE_WRITE_ACCESS as u8;
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(u8)]
    pub enum TransferMethod {
        Buffered = METHOD_BUFFERED as u8,
        InDirect = METHOD_IN_DIRECT as u8,
        OutDirect = METHOD_OUT_DIRECT as u8,
        Neither = METHOD_NEITHER as u8,
    }

    impl TryFrom<u32> for TransferMethod {
        type Error = ();

        fn try_from(value: u32) -> Result<Self, Self::Error> {
            match value {
                METHOD_BUFFERED => Ok(Self::Buffered),
                METHOD_IN_DIRECT => Ok(Self::InDirect),
                METHOD_OUT_DIRECT => Ok(Self::OutDirect),
                METHOD_NEITHER => Ok(Self::Neither),
                _ => Err(()),
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum DeviceType {
        I8042Port,
        Acpi,
        Battery,
        Beep,
        BusExtender,
        CdRom,
        CdRomFileSystem,
        Changer,
        Controller,
        Datalink,
        Dfs,
        DfsFileSystem,
        DfsVolume,
        Disk,
        DiskFileSystem,
        Dvd,
        FileSystem,
        Fips,
        FullscreenVideo,
        InportPort,
        Keyboard,
        Ks,
        Ksec,
        Mailslot,
        MassStorage,
        MidiIn,
        MidiOut,
        Modem,
        Mouse,
        MultiUncProvider,
        NamedPipe,
        Network,
        NetworkBrowser,
        NetworkFileSystem,
        NetworkRedirector,
        Null,
        ParallelPort,
        PhysicalNetcard,
        Printer,
        Scanner,
        Screen,
        Serenum,
        SerialMousePort,
        SerialPort,
        Smartcard,
        Smb,
        Sound,
        Streams,
        Tape,
        TapeFileSystem,
        Termsrv,
        Transport,
        Unknown,
        Vdm,
        Video,
        VirtualDisk,
        WaveIn,
        WaveOut,
        Custom(u16),
    }

    impl From<u16> for DeviceType {
        fn from(value: u16) -> Self {
            match value {
                0x0027 => Self::I8042Port,
                0x0032 => Self::Acpi,
                0x0029 => Self::Battery,
                0x0001 => Self::Beep,
                0x002a => Self::BusExtender,
                0x0002 => Self::CdRom,
                0x0003 => Self::CdRomFileSystem,
                0x0030 => Self::Changer,
                0x0004 => Self::Controller,
                0x0005 => Self::Datalink,
                0x0006 => Self::Dfs,
                0x0035 => Self::DfsFileSystem,
                0x0036 => Self::DfsVolume,
                0x0007 => Self::Disk,
                0x0008 => Self::DiskFileSystem,
                0x0033 => Self::Dvd,
                0x0009 => Self::FileSystem,
                0x003a => Self::Fips,
                0x0034 => Self::FullscreenVideo,
                0x000a => Self::InportPort,
                0x000b => Self::Keyboard,
                0x002f => Self::Ks,
                0x0039 => Self::Ksec,
                0x000c => Self::Mailslot,
                0x002d => Self::MassStorage,
                0x000d => Self::MidiIn,
                0x000e => Self::MidiOut,
                0x002b => Self::Modem,
                0x000f => Self::Mouse,
                0x0010 => Self::MultiUncProvider,
                0x0011 => Self::NamedPipe,
                0x0012 => Self::Network,
                0x0013 => Self::NetworkBrowser,
                0x0014 => Self::NetworkFileSystem,
                0x0028 => Self::NetworkRedirector,
                0x0015 => Self::Null,
                0x0016 => Self::ParallelPort,
                0x0017 => Self::PhysicalNetcard,
                0x0018 => Self::Printer,
                0x0019 => Self::Scanner,
                0x001c => Self::Screen,
                0x0037 => Self::Serenum,
                0x001a => Self::SerialMousePort,
                0x001b => Self::SerialPort,
                0x0031 => Self::Smartcard,
                0x002e => Self::Smb,
                0x001d => Self::Sound,
                0x001e => Self::Streams,
                0x001f => Self::Tape,
                0x0020 => Self::TapeFileSystem,
                0x0038 => Self::Termsrv,
                0x0021 => Self::Transport,
                0x0022 => Self::Unknown,
                0x002c => Self::Vdm,
                0x0023 => Self::Video,
                0x0024 => Self::VirtualDisk,
                0x0025 => Self::WaveIn,
                0x0026 => Self::WaveOut,
                other if other >= 0x8000 => Self::Custom(other),
                _ => Self::Unknown,
            }
        }
    }

    impl DeviceType {
        pub fn to_u16(self) -> u16 {
            match self {
                Self::I8042Port => 0x0027,
                Self::Acpi => 0x0032,
                Self::Battery => 0x0029,
                Self::Beep => 0x0001,
                Self::BusExtender => 0x002a,
                Self::CdRom => 0x0002,
                Self::CdRomFileSystem => 0x0003,
                Self::Changer => 0x0030,
                Self::Controller => 0x0004,
                Self::Datalink => 0x0005,
                Self::Dfs => 0x0006,
                Self::DfsFileSystem => 0x0035,
                Self::DfsVolume => 0x0036,
                Self::Disk => 0x0007,
                Self::DiskFileSystem => 0x0008,
                Self::Dvd => 0x0033,
                Self::FileSystem => 0x0009,
                Self::Fips => 0x003a,
                Self::FullscreenVideo => 0x0034,
                Self::InportPort => 0x000a,
                Self::Keyboard => 0x000b,
                Self::Ks => 0x002f,
                Self::Ksec => 0x0039,
                Self::Mailslot => 0x000c,
                Self::MassStorage => 0x002d,
                Self::MidiIn => 0x000d,
                Self::MidiOut => 0x000e,
                Self::Modem => 0x002b,
                Self::Mouse => 0x000f,
                Self::MultiUncProvider => 0x0010,
                Self::NamedPipe => 0x0011,
                Self::Network => 0x0012,
                Self::NetworkBrowser => 0x0013,
                Self::NetworkFileSystem => 0x0014,
                Self::NetworkRedirector => 0x0028,
                Self::Null => 0x0015,
                Self::ParallelPort => 0x0016,
                Self::PhysicalNetcard => 0x0017,
                Self::Printer => 0x0018,
                Self::Scanner => 0x0019,
                Self::Screen => 0x001c,
                Self::Serenum => 0x0037,
                Self::SerialMousePort => 0x001a,
                Self::SerialPort => 0x001b,
                Self::Smartcard => 0x0031,
                Self::Smb => 0x002e,
                Self::Sound => 0x001d,
                Self::Streams => 0x001e,
                Self::Tape => 0x001f,
                Self::TapeFileSystem => 0x0020,
                Self::Termsrv => 0x0038,
                Self::Transport => 0x0021,
                Self::Unknown => 0x0022,
                Self::Vdm => 0x002c,
                Self::Video => 0x0023,
                Self::VirtualDisk => 0x0024,
                Self::WaveIn => 0x0025,
                Self::WaveOut => 0x0026,
                Self::Custom(value) => value,
            }
        }
    }
}

pub struct DriverObject(windows_kernel_sys::PDRIVER_OBJECT);

impl DriverObject {
    /// Creates a new `DriverObject` wrapper
    ///
    /// ## Safety
    ///
    /// Must only be called from the `DriverEntry` method
    #[must_use]
    pub unsafe fn new(driver_object: windows_kernel_sys::PDRIVER_OBJECT) -> Self {
        Self(driver_object)
    }

    /// Unwraps the driver object, yielding the original raw pointer
    #[must_use]
    pub fn into_raw(self) -> windows_kernel_sys::PDRIVER_OBJECT {
        self.0
    }
}

/// Represents a timeout used by `KeWaitForSingleObject` and `KeWaitForMultipleObjects`.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Timeout(Option<i64>);

impl Timeout {
    /// Wait until reaching an absolute timestamp, relative to
    /// January 1st, 1601, in 100-nanosecond increments.
    ///
    /// Also accounts for changes in the system time.
    pub const fn absolute(timestamp: i64) -> Self {
        assert!(timestamp > 0);
        Self(Some(timestamp))
    }

    /// Waits for an interval (in units of 100-nanoseconds) to pass.
    pub const fn relative(duration: i64) -> Self {
        assert!(duration > 0);
        Self(Some(duration.wrapping_neg()))
    }

    /// Like [`Self::relative`], but in units of milliseconds.
    pub const fn relative_ms(duration: i64) -> Self {
        // 100 ns is basically 0.1 us
        // 1 ms = 1_000 us = 1_000_0 100-ns
        let Some(duration) = duration.checked_mul(1_000_0) else {
            panic!("overflow in ms to 100-ns conversion")
        };

        Self::relative(duration)
    }

    /// Don't wait and return immediately
    pub const fn dont_wait() -> Self {
        Self(Some(0))
    }

    /// Wait indefinitely until the object is set to the signaled state.
    ///
    /// ## IRQL: `..=APC_LEVEL`
    pub const fn forever() -> Self {
        Self(None)
    }

    /// Gets the raw timeout representation to pass to kernel functions.
    pub fn value(&mut self) -> PLONGLONG {
        match &mut self.0 {
            Some(timeout) => timeout as *mut _,
            None => core::ptr::null_mut(),
        }
    }
}

pub fn __handle_panic(info: &core::panic::PanicInfo) -> ! {
    // Closest bugcheck we can get that doesn't have any parameters
    const FATAL_UNHANDLED_HARD_ERROR: u32 = 0x4C;

    // Show panic message
    log::error!("{info}");

    // Try entering the debugger first...
    unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
    // Before causing a bugcheck

    // SAFETY: Just a matching FFI signature
    unsafe { windows_kernel_sys::KeBugCheck(FATAL_UNHANDLED_HARD_ERROR) }
}

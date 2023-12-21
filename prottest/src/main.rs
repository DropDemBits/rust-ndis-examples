//! Test program for ndisprot_kmdf.sys

use std::str::FromStr;

use bytemuck::Zeroable;
use clap::Parser;
use color_eyre::eyre::{Result, WrapErr};
use windows::Win32::{
    Foundation::{CloseHandle, ERROR_NO_MORE_ITEMS, GENERIC_READ, GENERIC_WRITE, HANDLE},
    Storage::FileSystem::{CreateFileW, ReadFile, WriteFile, FILE_ATTRIBUTE_NORMAL, OPEN_EXISTING},
    System::IO::DeviceIoControl,
};

use crate::user_io_interface::{NdisProtIoctl, QueryBinding, QueryBindingHeader, QueryOid};

const DEFAULT_NDISPROT_DEVICE: &str = r"\\.\\NdisProt";

const MAC_ADDR_LEN: usize = 6;

const FAKE_SRC_MAC_ADDR: MACAddr = MACAddr([0; MAC_ADDR_LEN]);

const DEFAULT_PACKET_LENGTH: u32 = 100;

#[derive(
    Default, Debug, Clone, Copy, PartialEq, Eq, bytemuck_derive::Zeroable, bytemuck_derive::Pod,
)]
#[repr(transparent)]
struct MACAddr([u8; MAC_ADDR_LEN]);

impl std::fmt::Display for MACAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}",
            self.0[0], self.0[1], self.0[2], self.0[3], self.0[4], self.0[5]
        ))
    }
}

#[derive(Debug)]
struct InvalidMACAddressError {}

impl std::fmt::Display for InvalidMACAddressError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid mac address")
    }
}

impl std::error::Error for InvalidMACAddressError {}

impl FromStr for MACAddr {
    type Err = InvalidMACAddressError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut mac_addr = MACAddr::default();

        if s.split(':').count() != 6 {
            return Err(InvalidMACAddressError {});
        }

        for (i, part) in s.split(':').enumerate().take(6) {
            mac_addr.0[i] = u8::from_str_radix(part, 16).map_err(|_| InvalidMACAddressError {})?;
        }

        Ok(mac_addr)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, bytemuck_derive::Zeroable, bytemuck_derive::Pod)]
#[repr(C, packed)]
struct EthHeader {
    dst_addr: MACAddr,
    src_addr: MACAddr,
    eth_type: u16,
}

#[derive(clap::Parser)]
struct Options {
    /// mode
    mode: Mode,
    #[arg(help = "device name (usually {DEFAULT_NDISPROT_DEVICE})", default_value_t = DEFAULT_NDISPROT_DEVICE.to_string())]
    device_name: String,
    #[arg(
        help = "length of each packet (default: {DEFAULT_PACKET_LENGTH})",
        short = 'l',
        default_value_t = DEFAULT_PACKET_LENGTH,
    )]
    packet_length: u32,
    /// number of packets (defaults to infinity)
    #[arg(short = 'n')]
    packet_count: Option<u32>,
    /// (defaults to local MAC)
    #[arg(short = 'm')]
    override_mac_address: Option<MACAddr>,
    /// use a fake address to send packets
    #[arg(short = 'f', default_value_t)]
    use_fake_address: bool,
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum Mode {
    Read,
    Write,
    Enumerate,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let opts = Options::try_parse()?;
    let device_handle = open_handle(DEFAULT_NDISPROT_DEVICE)
        .wrap_err_with(|| format!("failed to open {}", opts.device_name))?;

    let mut state = State {
        eth_type: 0x8e88,
        opts,
        src_mac_addr: Default::default(),
        dst_mac_addr: Default::default(),
        device_handle,
    };

    if matches!(state.opts.mode, Mode::Enumerate) {
        return state.enumerate_devices();
    }

    // open the device
    state
        .open_ndis_device()
        .wrap_err("trying to open ndis device")?;

    state
        .get_src_mac()
        .wrap_err("trying to get src MAC address")?;

    println!("got local MAC: {}", state.src_mac_addr);

    if let Some(mac_addr) = state.opts.override_mac_address {
        state.dst_mac_addr = mac_addr;
    }

    match state.opts.mode {
        Mode::Read => {
            state.do_read();
        }
        Mode::Write => {
            state.do_write();
            state.do_read();
        }
        _ => unreachable!(),
    }

    Ok(())
}

struct State {
    src_mac_addr: MACAddr,
    dst_mac_addr: MACAddr,
    opts: Options,
    eth_type: u16,
    device_handle: HANDLE,
}

impl State {
    fn enumerate_devices(&self) -> Result<()> {
        #[repr(align(16))]
        struct RawQueryBindingInfo([u8; 1024]);

        let mut buf = RawQueryBindingInfo([0; 1024]);
        let mut bytes_written = 0u32;
        let mut i = 0u32;

        loop {
            let mut in_binding = QueryBindingHeader {
                binding_index: i,
                ..Default::default()
            };
            i += 1;

            let res = unsafe {
                DeviceIoControl(
                    self.device_handle,
                    NdisProtIoctl::QueryBinding.code().bits(),
                    Some((&mut in_binding as *mut QueryBindingHeader).cast()),
                    std::mem::size_of::<QueryBindingHeader>() as u32,
                    Some((&mut buf.0 as *mut u8).cast()),
                    buf.0.len() as u32,
                    Some(&mut bytes_written),
                    None,
                )
            };

            match res {
                Ok(_) => {
                    let info = QueryBinding::from_bytes(&buf.0)
                        .wrap_err("enumerate_devices: failed to parse binding info")?;
                    println!("{:2}. {}", i, info.device_name);
                    println!("    - {}", info.device_desc);
                    buf.0.fill(0)
                }
                Err(err) if err == ERROR_NO_MORE_ITEMS.into() => break,
                Err(err) => {
                    return Err(err).wrap_err("enumerate_devices: terminated abnormally");
                }
            }
        }

        Ok(())
    }

    fn open_ndis_device(&self) -> Result<()> {
        // Convert to local-agnostic unicode string
        let str = widestring::U16CString::from_str(&self.opts.device_name)
            .wrap_err("failed to convert device name to a wide string")?;
        let str = str.as_slice_with_nul();
        let str_byte_count = (str.len() * 2)
            .try_into()
            .wrap_err("failed to convert device name length")?;

        println!("trying to access NDIS device: {}", self.opts.device_name);

        unsafe {
            DeviceIoControl(
                self.device_handle,
                NdisProtIoctl::OpenDevice.code().bits(),
                Some(str.as_ptr().cast()),
                str_byte_count,
                None,
                0,
                None,
                None,
            )
            .wrap_err("failed to open device")
        }
    }

    fn get_src_mac(&mut self) -> Result<()> {
        println!("trying to get src mac address");

        let oid_query = QueryOid::<{ std::mem::size_of::<MACAddr>() }> {
            oid: windows::Win32::NetworkManagement::Ndis::OID_802_3_CURRENT_ADDRESS,
            port_number: 0,
            data: MACAddr::default().0,
        };
        let oid_query_bytes = bytemuck::bytes_of(&oid_query);

        let mut oid_query_result = QueryOid::<{ std::mem::size_of::<MACAddr>() }>::zeroed();
        let oid_query_result_bytes = bytemuck::bytes_of_mut(&mut oid_query_result);

        let mut bytes_written = 0;

        unsafe {
            DeviceIoControl(
                self.device_handle,
                NdisProtIoctl::QueryOidValue.code().bits(),
                Some(oid_query_bytes.as_ptr().cast()),
                oid_query_bytes.len() as u32,
                Some(oid_query_result_bytes.as_mut_ptr().cast()),
                oid_query_result_bytes.len() as u32,
                Some(&mut bytes_written),
                None,
            )
            .wrap_err("DeviceIoControl failed")?;
        }

        println!("IoControl success, BytesWritten {bytes_written}");
        if bytes_written as usize != std::mem::size_of_val(&oid_query_result) {
            color_eyre::eyre::bail!(
                "returned MAC address buffer was not the right size ({bytes_written} != {})",
                std::mem::size_of_val(&oid_query_result)
            );
        }

        self.src_mac_addr = MACAddr(oid_query_result.data);

        Ok(())
    }

    fn do_read(&self) {
        let mut read_buf = (0..self.opts.packet_length)
            .map(|_| 0u8)
            .collect::<Vec<_>>();

        let mut read_count = 0;
        loop {
            let mut bytes_read = 0;
            let res = unsafe {
                ReadFile(
                    self.device_handle,
                    Some(read_buf.as_mut_slice()),
                    Some(&mut bytes_read),
                    None,
                )
                .wrap_err_with(|| format!("ReadFile failed on Handle {:?}", self.device_handle))
            };

            if let Err(err) = res {
                eprintln!("{err}");
                break;
            }

            read_count += 1;

            if self
                .opts
                .packet_count
                .is_some_and(|count| read_count == count)
            {
                break;
            }
        }

        println!("do_read finished: read {read_count} packets");
    }

    fn do_write(&self) {
        println!("do_write");

        let header = EthHeader {
            src_addr: if self.opts.use_fake_address {
                FAKE_SRC_MAC_ADDR
            } else {
                self.src_mac_addr
            },
            dst_addr: self.dst_mac_addr,
            eth_type: self.eth_type,
        };
        let header = bytemuck::bytes_of(&header);

        let mut write_buf = (0..self.opts.packet_length)
            .map(|idx| idx.wrapping_sub(header.len() as u32) as u8)
            .collect::<Vec<_>>();

        let max_len = header.len().min(self.opts.packet_length as usize);
        write_buf[0..max_len].copy_from_slice(&header[..max_len]);

        let mut write_count = 0;
        let mut bytes_read = 0;

        loop {
            let res = unsafe {
                WriteFile(
                    self.device_handle,
                    Some(write_buf.as_slice()),
                    Some(&mut bytes_read),
                    None,
                )
                .wrap_err_with(|| format!("ReadFile failed on Handle {:?}", self.device_handle))
            };

            if let Err(err) = res {
                eprintln!("{err}");
                break;
            }

            write_count += 1;

            if self
                .opts
                .packet_count
                .is_some_and(|count| write_count == count)
            {
                break;
            }
        }

        println!(
            "do_read finished: sent {write_count} packets of {} bytes each",
            self.opts.packet_length
        );
    }
}

impl Drop for State {
    fn drop(&mut self) {
        let _ = unsafe { CloseHandle(self.device_handle) };
    }
}

fn open_handle(device_name: &str) -> Result<HANDLE> {
    let desired_access = GENERIC_READ | GENERIC_WRITE;
    let share_mode = windows::Win32::Storage::FileSystem::FILE_SHARE_NONE;
    let security_attributes = None;
    let creation_distribution = OPEN_EXISTING;
    let flags_and_attributes = FILE_ATTRIBUTE_NORMAL;

    let device_name = windows::core::HSTRING::from(device_name);
    let device_name = windows::core::PCWSTR::from_raw(device_name.as_ptr());

    let handle = unsafe {
        CreateFileW(
            device_name,
            desired_access.0,
            share_mode,
            security_attributes,
            creation_distribution,
            flags_and_attributes,
            HANDLE(0),
        )
        .wrap_err("creating file failed")?
    };

    // wait for the driver to finish binding
    let res = unsafe {
        DeviceIoControl(
            handle,
            NdisProtIoctl::BindWait.code().bits(),
            None,
            0,
            None,
            0,
            None,
            None,
        )
    };

    if let Err(err) = res.wrap_err("IOCTL_NDISPROT_BIND_WAIT failed") {
        // Don't really care if `CloseHandle` fails
        let _ = unsafe { CloseHandle(handle) };
        return Err(err);
    }

    Ok(handle)
}

mod user_io_interface {
    use byte::ctx::Bytes;
    use nt_string::{unicode_string::NtUnicodeStr, NtStringError};

    use self::ioctl::{
        DeviceType, IoControlCode, RequiredAccess, StandardDeviceType, TransferMethod,
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[allow(unused)] // we don't use everything here
    pub enum NdisProtIoctl {
        OpenDevice,
        QueryOidValue,
        SetOidValue,
        QueryBinding,
        BindWait,
        IndicateStatus,
    }

    impl NdisProtIoctl {
        pub fn code(self) -> IoControlCode {
            const FSCTL_NDISPROT_BASE: DeviceType =
                DeviceType::Standard(StandardDeviceType::Network);

            match self {
                Self::OpenDevice => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x200,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
                Self::QueryOidValue => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x201,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
                Self::SetOidValue => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x205,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
                Self::QueryBinding => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x203,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
                Self::BindWait => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x204,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
                Self::IndicateStatus => IoControlCode::code(
                    FSCTL_NDISPROT_BASE,
                    0x206,
                    TransferMethod::Buffered,
                    RequiredAccess::ReadWrite,
                ),
            }
        }
    }

    #[derive(Default)]
    #[repr(C)]
    pub struct QueryBindingHeader {
        pub binding_index: u32,
        pub device_name_offset: u32,
        pub device_name_length: u32,
        pub device_descr_offset: u32,
        pub device_descr_length: u32,
    }

    pub struct QueryBinding<'a> {
        pub binding_index: u32,
        pub device_name: NtUnicodeStr<'a>,
        pub device_desc: NtUnicodeStr<'a>,
    }

    #[derive(Debug)]
    pub enum QueryBindingError {
        DecodeError(byte::Error),
        NtStringError(nt_string::NtStringError),
        InvalidStrSlice(bytemuck::PodCastError),
    }

    impl From<byte::Error> for QueryBindingError {
        fn from(value: byte::Error) -> Self {
            Self::DecodeError(value)
        }
    }

    impl From<nt_string::NtStringError> for QueryBindingError {
        fn from(value: NtStringError) -> Self {
            Self::NtStringError(value)
        }
    }

    impl From<bytemuck::PodCastError> for QueryBindingError {
        fn from(value: bytemuck::PodCastError) -> Self {
            Self::InvalidStrSlice(value)
        }
    }

    impl std::fmt::Display for QueryBindingError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                QueryBindingError::DecodeError(err) => {
                    f.write_fmt(format_args!("error during decoding: {err:?}"))
                }
                QueryBindingError::NtStringError(err) => {
                    f.write_fmt(format_args!("error during conversion to string: {err}"))
                }
                QueryBindingError::InvalidStrSlice(err) => {
                    f.write_fmt(format_args!("error during conversion to string: {err}"))
                }
            }
        }
    }

    impl std::error::Error for QueryBindingError {}

    impl<'a> QueryBinding<'a> {
        pub fn from_bytes(bytes: &'a [u8]) -> Result<Self, QueryBindingError> {
            use byte::{BytesExt, LE};

            // Parse the header first
            let header = {
                let offset = &mut 0;

                QueryBindingHeader {
                    binding_index: bytes.read_with::<u32>(offset, LE)?,
                    device_name_offset: bytes.read_with::<u32>(offset, LE)?,
                    device_name_length: bytes.read_with::<u32>(offset, LE)?,
                    device_descr_offset: bytes.read_with::<u32>(offset, LE)?,
                    device_descr_length: bytes.read_with::<u32>(offset, LE)?,
                }
            };

            fn read_str(
                bytes: &[u8],
                offset: u32,
                len: u32,
            ) -> Result<NtUnicodeStr<'_>, QueryBindingError> {
                let str =
                    bytes.read_with::<&[u8]>(&mut (offset as usize), Bytes::Len(len as usize))?;
                let str = bytemuck::try_cast_slice(str)?;
                let str = NtUnicodeStr::try_from_u16(str)?;
                Ok(str)
            }

            // Parse out the strings

            Ok(Self {
                binding_index: header.binding_index,
                device_name: read_str(bytes, header.device_name_offset, header.device_name_length)?,
                device_desc: read_str(
                    bytes,
                    header.device_descr_offset,
                    header.device_descr_length,
                )?,
            })
        }
    }

    #[repr(C)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct QueryOid<const BUFFER_SIZE: usize> {
        pub oid: u32,
        pub port_number: u32,
        pub data: [u8; BUFFER_SIZE],
    }

    unsafe impl<const BUFFER_SIZE: usize> bytemuck::Zeroable for QueryOid<BUFFER_SIZE> {
        fn zeroed() -> Self {
            unsafe { core::mem::zeroed() }
        }
    }

    unsafe impl<const BUFFER_SIZE: usize> bytemuck::Pod for QueryOid<BUFFER_SIZE> {}

    pub mod ioctl {
        use mycelium_bitfield::{bitfield, enum_from_bits, FromBits};

        bitfield! {
            #[derive(PartialEq, Eq)]
            pub struct IoControlCode<u32> {
                pub const TRANSFER_METHOD: TransferMethod;
                pub const FUNCTION_CODE = 12;
                pub const REQUIRED_ACCESS: RequiredAccess;
                pub const DEVICE_TYPE: DeviceType;
            }
        }

        impl IoControlCode {
            // can't make this const because there's no const traits yet
            pub fn code(
                device_type: DeviceType,
                function_code: u16,
                transfer_method: TransferMethod,
                required_access: RequiredAccess,
            ) -> Self {
                Self::new()
                    .with(Self::DEVICE_TYPE, device_type)
                    .with(Self::REQUIRED_ACCESS, required_access)
                    .with(Self::FUNCTION_CODE, function_code.into())
                    .with(Self::TRANSFER_METHOD, transfer_method)
            }
        }

        enum_from_bits! {
            #[derive(Debug, PartialEq, Eq)]
            pub enum RequiredAccess<u8> {
                Any = 0b00,
                Read = 0b01,
                Write = 0b10,
                ReadWrite = 0b11,
            }
        }

        enum_from_bits! {
            #[derive(Debug, PartialEq, Eq)]
            pub enum TransferMethod<u8> {
                Buffered = 0b00,
                InDirect = 0b01,
                OutDirect = 0b10,
                Neither = 0b11,
            }
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum DeviceType {
            Standard(StandardDeviceType),
            Vendor(VendorDeviceType),
        }

        impl FromBits<u16> for DeviceType {
            type Error = &'static str;
            const BITS: u32 = 16;

            fn try_from_bits(bits: u16) -> Result<Self, Self::Error> {
                if bits < 0x8000 {
                    Ok(Self::Standard(StandardDeviceType::try_from_bits(bits)?))
                } else {
                    Ok(Self::Vendor(VendorDeviceType::from_bits(bits)))
                }
            }

            fn into_bits(self) -> u16 {
                match self {
                    DeviceType::Standard(standard) => standard.into_bits(),
                    DeviceType::Vendor(vendor) => vendor.bits(),
                }
            }
        }

        impl FromBits<u32> for DeviceType {
            type Error = <Self as FromBits<u16>>::Error;
            const BITS: u32 = <Self as FromBits<u16>>::BITS;

            fn try_from_bits(bits: u32) -> Result<Self, Self::Error> {
                if bits <= u16::MAX as u32 {
                    <Self as FromBits<u16>>::try_from_bits(bits as u16)
                } else {
                    Err("expected a StandardDeviceType (in [0..0x7FFF]) or a VendorDeviceType (in [0x8000..0xFFFF]))")
                }
            }

            fn into_bits(self) -> u32 {
                <Self as FromBits<u16>>::into_bits(self).into()
            }
        }

        enum_from_bits! {
            #[derive(Debug, PartialEq, Eq)]
            pub enum StandardDeviceType<u16> {
                I8042Port = 0x0027,
                Acpi = 0x0032,
                Battery = 0x0029,
                Beep = 0x0001,
                BusExtender = 0x002a,
                CdRom = 0x0002,
                CdRomFileSystem = 0x0003,
                Changer = 0x0030,
                Controller = 0x0004,
                Datalink = 0x0005,
                Dfs = 0x0006,
                DfsFileSystem = 0x0035,
                DfsVolume = 0x0036,
                Disk = 0x0007,
                DiskFileSystem = 0x0008,
                Dvd = 0x0033,
                FileSystem = 0x0009,
                Fips = 0x003a,
                FullscreenVideo = 0x0034,
                InportPort = 0x000a,
                Keyboard = 0x000b,
                Ks = 0x002f,
                Ksec = 0x0039,
                Mailslot = 0x000c,
                MassStorage = 0x002d,
                MidiIn = 0x000d,
                MidiOut = 0x000e,
                Modem = 0x002b,
                Mouse = 0x000f,
                MultiUncProvider = 0x0010,
                NamedPipe = 0x0011,
                Network = 0x0012,
                NetworkBrowser = 0x0013,
                NetworkFileSystem = 0x0014,
                NetworkRedirector = 0x0028,
                Null = 0x0015,
                ParallelPort = 0x0016,
                PhysicalNetcard = 0x0017,
                Printer = 0x0018,
                Scanner = 0x0019,
                Screen = 0x001c,
                Serenum = 0x0037,
                SerialMousePort = 0x001a,
                SerialPort = 0x001b,
                Smartcard = 0x0031,
                Smb = 0x002e,
                Sound = 0x001d,
                Streams = 0x001e,
                Tape = 0x001f,
                TapeFileSystem = 0x0020,
                Termsrv = 0x0038,
                Transport = 0x0021,
                Unknown = 0x0022,
                Vdm = 0x002c,
                Video = 0x0023,
                VirtualDisk = 0x0024,
                WaveIn = 0x0025,
                WaveOut = 0x0026,
            }
        }

        bitfield! {
            #[derive(PartialEq, Eq)]
            pub struct VendorDeviceType<u16> {
                pub const DEVICE_TYPE = 15;
                const _IS_VENDOR = 1;
            }
        }
    }
}

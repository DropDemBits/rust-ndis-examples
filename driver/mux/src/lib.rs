#![no_std]
#![allow(non_upper_case_globals, non_snake_case, dead_code)]

use core::{
    ffi::CStr,
    mem::{ManuallyDrop, MaybeUninit},
    pin::Pin,
    ptr::NonNull,
    sync::atomic::{AtomicI32, AtomicU32, AtomicU64},
};

use crossbeam_utils::atomic::AtomicCell;
use ndis_rs::NetBuffer;
use nt_list::{
    list::{NtList, NtListEntry, NtListHead},
    NtListElement,
};
use wdf_kmdf::{
    miniport::MiniportDevice,
    sync::{WaitMutex, WaitPinMutex},
};
use windows_kernel_rs::{
    log,
    string::unicode_string::{NtUnicodeStr, NtUnicodeString},
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, KeAcquireSpinLockAtDpcLevel, KeAcquireSpinLockRaiseToDpc, KeInitializeMutex,
    KeReleaseMutex, KeReleaseSpinLock, KeReleaseSpinLockFromDpcLevel, KeWaitForSingleObject,
    NdisAcquireReadWriteLock, NdisInitializeReadWriteLock, NdisReleaseReadWriteLock, BOOLEAN,
    KMUTEX, NDIS_BIND_PARAMETERS, NDIS_DEVICE_POWER_STATE, NDIS_EVENT, NDIS_HANDLE,
    NDIS_LINK_STATE, NDIS_MEDIUM, NDIS_OID_REQUEST, NDIS_PACKET_TYPE_ALL_MULTICAST,
    NDIS_PACKET_TYPE_BROADCAST, NDIS_PACKET_TYPE_DIRECTED, NDIS_PACKET_TYPE_MULTICAST,
    NDIS_PACKET_TYPE_PROMISCUOUS, NDIS_PNP_CAPABILITIES, NDIS_RECEIVE_SCALE_CAPABILITIES,
    NDIS_RW_LOCK, NDIS_SPIN_LOCK, NDIS_STATUS, NET_IFINDEX, NPAGED_LOOKASIDE_LIST, NTSTATUS,
    PDRIVER_OBJECT, PLOCK_STATE, PNDIS_OID_REQUEST, PUNICODE_STRING, ULONG, ULONG64,
};

mod miniport;
mod mux;
mod protocol;

const MUX_MAJOR_NDIS_VERSION: u8 = 6;
const MUX_MINOR_NDIS_VERSION: u8 = 0;

const MUX_MAJOR_DRIVER_VERSION: u8 = 6;
const MUX_MINOR_DRIVER_VERSION: u8 = 0;

const MUX_PROT_MAJOR_NDIS_VERSION: u8 = 6;
const MUX_PROT_MINOR_NDIS_VERSION: u8 = 0;

const MUX_TAG: u32 = u32::from_le_bytes(*b"MuxS");
const WAIT_INFINITE: u32 = 0;

/// This OID specifies the current driver version.
/// The high byte is the major version.
/// The low byte is the minor version.
const VELAN_DRIVER_VERSION: u16 =
    u16::from_be_bytes([MUX_MAJOR_DRIVER_VERSION, MUX_MINOR_DRIVER_VERSION]);

/// media type, change if necessary
const VELAN_MEDIA_TYPE: NDIS_MEDIUM = NDIS_MEDIUM::NdisMedium802_3;

/// change to your company name instead of the default
const VELAN_VENDOR_DESC: &CStr = c"TheDefault";

/// Highest byte is the NIC byte plus three vendor bytes, they are normally obtained from the NIC
const VELAN_VENDOR_ID: u32 = 0x00_FF_FF_FF;

const VELAN_MAX_MCAST_LIST: usize = 32;
const VELAN_MAX_SEND_PKTS: usize = 5;

const ETH_MAX_PACKET_SIZE: u16 = 1514;
const ETH_MIN_PACKET_SIZE: u16 = 60;
const ETH_HEADER_SIZE: usize = core::mem::size_of::<EthHeader>();

const VELAN_SUPPORTED_FILTERS: u32 = NDIS_PACKET_TYPE_DIRECTED
    | NDIS_PACKET_TYPE_MULTICAST
    | NDIS_PACKET_TYPE_BROADCAST
    | NDIS_PACKET_TYPE_PROMISCUOUS
    | NDIS_PACKET_TYPE_ALL_MULTICAST;

const MUX_ADAPTER_PACKET_FILTER: u32 = NDIS_PACKET_TYPE_PROMISCUOUS;

const MIN_PACKET_POOL_SIZE: u32 = 255;
const MAX_PACKET_POOL_SIZE: u32 = 4096;

// Default values
const MUX_DEFAULT_LINK_SPEED: u32 = 100_000; // in 100s of bits/sec
const MUX_DEFAULT_LOOKAHEAD_SIZE: u32 = 512;

#[allow(non_camel_case_types)]
type MUX_REQ_COMPLETE_HANDLER = unsafe fn(*mut Adapt, *mut MuxNdisRequest, NDIS_STATUS);

/// Super-struct for NDIS_REQUEST, to allow us to keep context about requests
/// sent down to a lower binding.
#[repr(C)]
struct MuxNdisRequest {
    /// Set iff this is a forwarded request from a VELAN
    pVElan: Option<NonNull<VELan>>,
    /// Completion status
    Status: NDIS_STATUS,
    /// Called on completion of request
    pCallback: Option<MUX_REQ_COMPLETE_HANDLER>,
    /// Request that originated this request
    OrigRequest: PNDIS_OID_REQUEST,
    Request: NDIS_OID_REQUEST,
    Refcount: AtomicU32,
    Cancelled: bool,
}

#[repr(u8)]
enum AdapterBindingState {
    Paused,
    Pausing,
    Running,
}

const MUX_BINDING_ACTIVE: u32 = 0b01;
const MUX_BINDING_CLOSING: u32 = 0b10;

#[derive(NtList)]
enum AdaptList {}

/// The Adapt struct represents a binding to a lower adapter by the protocol
/// edge of this adapter. Based on the configured Upper bindings, zero or more
/// virtual miniport devices (VELANs) are created above this binding.
#[derive(NtListElement)]
#[repr(C)]
struct Adapt {
    /// Chain adapters. Access to this is protected by the global lock.
    Link: NtListEntry<Self, AdaptList>,

    /// References to this adapter
    RefCount: AtomicU32,

    /// Handle to the lower adapter, used in NDIS calls referring to this adapter.
    BindingHandle: NDIS_HANDLE,

    /// List of all the virtual ELANs created on this lower binding
    VElanList: NtListHead<VELan, AdapterVELanList>,
    VElanCount: u32,

    /// String used to access configuration for this binding
    ConfigString: ManuallyDrop<NtUnicodeString>,

    /// Open Status. used by bind/halt for Open/Close adapter status.
    Status: NDIS_STATUS,

    Event: NDIS_EVENT,

    /// Packet filter set to the underlying adapter. This is a union of filter
    /// bits set on all attached VELAN miniports.
    PacketFilter: u32,

    /// NDIS Medium of VELAN taken from the miniport below
    Medium: NDIS_MEDIUM,

    /// BindParameters passed to protocol giving it information on the miniport
    /// below.
    BindParameters: NDIS_BIND_PARAMETERS,
    PowerManagementCaps: NDIS_PNP_CAPABILITIES,
    RcvScaleCapabilities: NDIS_RECEIVE_SCALE_CAPABILITIES,
    LastIndicatedLinkState: NDIS_LINK_STATE,
    BindingState: AdapterBindingState,

    OutstandingSends: AtomicU32,
    PauseEvent: Option<NonNull<NDIS_EVENT>>,
    Lock: NDIS_SPIN_LOCK,

    /// Read/Write lock: allows multiple readers but only a single writer.
    ///
    /// Used to protect the VELAN list and fields (e.g. packet filter) shared
    /// on an Adapt by multiple VELANs.
    /// Code that needs to traverse the VELAN list safely acquires a READ lock.
    /// Code that needs to safely modify the VELAN list or shared fields acquires a WRITE lock (which also excludes readers).
    ///
    /// See `AcquireAdapt_xxx`/`ReleaseAdapt_xxx` below.
    RWLock: NDIS_RW_LOCK,

    OutstandingRequests: AtomicU32,
    CloseEvent: Option<NonNull<NDIS_EVENT>>,
    Flags: u32,
}

#[derive(nt_list::list::NtList)]
enum AdapterVELanList {}

#[derive(nt_list::NtListElement)]
#[repr(C)]
struct VELan {
    // Link into parent adapter's VELAN list.
    Link: NtListEntry<Self, AdapterVELanList>,

    // Link into global VELAN list
    //
    // NOTE: Not really used? Global VELAN list should be derived through
    // `adapters.iter().flat_map(|adapt| adapt.velans.iter())`
    // GlobalLink: NtListEntry<Self, VELanList>,

    // References to this VELAN.
    RefCount: ULONG,

    // Parent ADAPT.
    pAdapt: *mut Adapt,

    // Copy of BindingHandle from ADAPT.
    BindingHandle: NDIS_HANDLE,

    // Adapter handle for NDIS up-calls related to this virtual miniport.
    MiniportAdapterHandle: NDIS_HANDLE,

    // Virtual miniport's power state.
    MPDevicePowerState: NDIS_DEVICE_POWER_STATE,

    // Has our Halt entry point been called?
    MiniportHalting: BOOLEAN,

    // Do we need to indicate receive complete?
    IndicateRcvComplete: BOOLEAN,

    // Do we need to indicate status complete?
    IndicateStatusComplete: BOOLEAN,

    // Synchronization fields
    MiniportInitPending: BOOLEAN,
    MiniportInitEvent: NDIS_EVENT,

    // Uncompleted Sends/Requests to the adapter below.
    OutstandingSends: AtomicU32,

    // Count outstanding indications, including received
    // packets, passed up to protocols on this VELAN.
    OutstandingReceives: AtomicU32,

    // A request block that is used to forward a request presented
    // to the virtual miniport, to the lower binding. Since NDIS
    // serializes requests to a miniport, we only need one of these
    // per VELAN.
    //
    Request: MuxNdisRequest,
    // Have we queued a request because the lower binding is
    // at a low power state?
    QueuedRequest: BOOLEAN,

    // Have we started to deinitialize this VELAN?
    DeInitializing: BOOLEAN,

    // configuration
    PermanentAddress: MACAddr,
    CurrentAddress: MACAddr,

    CfgDeviceName: ManuallyDrop<NtUnicodeString>, // used as the unique ID for the VELAN
    VElanNumber: ULONG,                           // logical Elan number

    //
    //  ----- Buffer Management: Header buffers and Protocol buffers ----
    //

    // Some standard miniport parameters (OID values).
    PacketFilter: ULONG,
    LookAhead: ULONG,
    LinkSpeed: ULONG64,

    MaxBusySends: ULONG,
    MaxBusyRecvs: ULONG,

    // Packet counts
    GoodTransmits: AtomicU64,
    GoodReceives: AtomicU64,
    NumTxSinceLastAdjust: ULONG,

    // Count of transmit errors
    TxAbortExcessCollisions: ULONG,
    TxLateCollisions: ULONG,
    TxDmaUnderrun: ULONG,
    TxLostCRS: ULONG,
    TxOKButDeferred: ULONG,
    OneRetry: ULONG,
    MoreThanOneRetry: ULONG,
    TotalRetries: ULONG,
    TransmitFailuresOther: ULONG,

    // Count of receive errors
    RcvCrcErrors: ULONG,
    RcvAlignmentErrors: ULONG,
    RcvResourceErrors: ULONG,
    RcvDmaOverrunErrors: ULONG,
    RcvCdtFrames: ULONG,
    RcvRuntErrors: ULONG,
    RegNumTcb: ULONG,

    // Multicast list
    McastAddrs: [MACAddr; VELAN_MAX_MCAST_LIST],
    McastAddrCount: ULONG,

    LastIndicatedStatus: NDIS_STATUS,
    LatestUnIndicateStatus: NDIS_STATUS,
    Lock: NDIS_SPIN_LOCK,

    //  Miniport Pause/Restart functionality
    Paused: BOOLEAN,
    PauseLock: NDIS_SPIN_LOCK,
    LatestUnIndicateLinkState: NDIS_LINK_STATE,
    LastIndicatedLinkState: NDIS_LINK_STATE,

    // #[cfg(ieee_vlan_support)]
    VlanSupport: VELanVlan,

    IfIndex: NET_IFINDEX,
}

struct VELanVlan {
    VlanID: ULONG,
    RcvFormatErrors: ULONG,
    RcvVlanIdErrors: ULONG,
    RestoreLookaheadSize: BOOLEAN,
    TagLookaside: NPAGED_LOOKASIDE_LIST,
}

#[inline]
fn acquire_spin_lock(spinlock: *mut NDIS_SPIN_LOCK, dispatch_level: bool) {
    if dispatch_level {
        unsafe { KeAcquireSpinLockAtDpcLevel(core::ptr::addr_of_mut!((*spinlock).SpinLock)) }
    } else {
        unsafe {
            (*spinlock).OldIrql =
                KeAcquireSpinLockRaiseToDpc(core::ptr::addr_of_mut!((*spinlock).SpinLock))
        }
    }
}

#[inline]
fn release_spin_lock(spinlock: *mut NDIS_SPIN_LOCK, dispatch_level: bool) {
    if dispatch_level {
        unsafe { KeReleaseSpinLockFromDpcLevel(core::ptr::addr_of_mut!((*spinlock).SpinLock)) }
    } else {
        unsafe {
            KeReleaseSpinLock(
                core::ptr::addr_of_mut!((*spinlock).SpinLock),
                (*spinlock).OldIrql,
            )
        }
    }
}

mod ieee_vlan_support {
    use core::ptr::NonNull;

    use modular_bitfield::specifiers::{B1, B12, B3};
    use ndis_rs::NetBuffer;
    use windows_kernel_sys::{NDIS_NET_BUFFER_LIST_8021Q_INFO, PMDL};

    use crate::VELan;

    pub(crate) const TPID: u16 = 0x0081;

    #[repr(C)]
    pub(crate) struct VlanTagHeader {
        tag_info: TagInfo,
    }

    #[modular_bitfield::bitfield(bits = 16)]
    pub(crate) struct TagInfo {
        vlan_id: B12,
        canonical_format_id: B1,
        user_priority: B3,
    }

    impl TagInfo {
        pub(crate) fn zeroed() -> Self {
            Self::new()
        }

        pub(crate) fn copy_to_packet_info(
            self,
            packet_8021q_info: &mut NDIS_NET_BUFFER_LIST_8021Q_INFO,
        ) {
            let tag_header = unsafe { &mut packet_8021q_info.__bindgen_anon_1.TagHeader };
            tag_header.set_UserPriority(self.user_priority().into());
            tag_header.set_CanonicalFormatId(self.canonical_format_id().into());
            tag_header.set_VlanId(self.vlan_id().into());
        }
    }

    pub(crate) const VLAN_TAG_HEADER_SIZE: u16 = core::mem::size_of::<VlanTagHeader>() as u16;

    pub(crate) const VLAN_ID_DEFAULT: u32 = 0;
    pub(crate) const VLAN_ID_MIN: u32 = 0;
    pub(crate) const VLAN_ID_MAX: u32 = 0xfff;

    // Flags used by VELAN supports
    pub(crate) const MUX_RETREAT_DATA: u32 = 0x00000001;
    // Flags used by VELAN supports on receive code path
    pub(crate) const MUX_ADVANCE_DATA: u32 = 0x00000001;

    /// Every Nbl that is indicated up to a protocol needs to advance the buffer
    /// in case the VLAN tag is present. It should be restored before returning
    /// the packet to the miniport. This structure is used for that purpose.
    #[repr(C)]
    #[cfg_attr(target_pointer_width = "32", repr(align(8)))]
    #[cfg_attr(target_pointer_width = "64", repr(align(16)))]
    pub(crate) struct RecvNblEntry {
        Flags: u32,
        TagHeader: VlanTagHeader,
    }

    /// This structure is used to save context in the NET_BUFFER on the send
    /// path, if the ethernet header and VLAN tag is allocated by MUX.
    #[repr(C)]
    pub(crate) struct ImSendNblEntry {
        CurrentMdl: PMDL,
        PrevMdl: PMDL,
        CurrentMdlOffset: u32,
        NextNetBuffer: Option<NonNull<NetBuffer>>,
    }

    pub(crate) fn recognized_vlan_id(velan: &VELan, vlan_id: u32) -> bool {
        velan.VlanSupport.VlanID == vlan_id
    }
}

#[repr(C)]
#[cfg_attr(target_pointer_width = "32", repr(align(8)))]
#[cfg_attr(target_pointer_width = "64", repr(align(16)))]
struct ImNblEntry {
    PreviousSourceHanlde: NDIS_HANDLE,
    pVElan: *mut VELan,
    // if ieee_vlan_support
    Flags: ULONG,
    // if ieee_vlan_support
    MdlAllocatedNetBuffers: Option<NonNull<NetBuffer>>,
}

fn is_low_power_state(power_state: NDIS_DEVICE_POWER_STATE) -> bool {
    power_state.0 > NDIS_DEVICE_POWER_STATE::NdisDeviceStateD0.0
}

fn adapt_init_rw_lock(adapt: *mut MaybeUninit<Adapt>) {
    unsafe {
        NdisInitializeReadWriteLock(core::ptr::addr_of_mut!((*(*adapt).as_mut_ptr()).RWLock))
    };
}

#[inline]
fn adapt_acquire_read_lock(adapt: *mut Adapt, lock_state: PLOCK_STATE) {
    unsafe {
        NdisAcquireReadWriteLock(
            core::ptr::addr_of_mut!((*adapt).RWLock),
            false as u8,
            lock_state,
        )
    }
}

#[inline]
fn adapt_release_read_lock(adapt: *mut Adapt, lock_state: PLOCK_STATE) {
    unsafe { NdisReleaseReadWriteLock(core::ptr::addr_of_mut!((*adapt).RWLock), lock_state) }
}

#[inline]
fn adapt_acquire_write_lock(adapt: *mut Adapt, lock_state: PLOCK_STATE) {
    unsafe {
        NdisAcquireReadWriteLock(
            core::ptr::addr_of_mut!((*adapt).RWLock),
            true as u8,
            lock_state,
        )
    }
}

#[inline]
fn adapt_release_write_lock(adapt: *mut Adapt, lock_state: PLOCK_STATE) {
    unsafe { NdisReleaseReadWriteLock(core::ptr::addr_of_mut!((*adapt).RWLock), lock_state) }
}

#[inline]
fn velan_inc_pending_receives(velan: &VELan) {
    velan
        .OutstandingReceives
        .fetch_add(1, core::sync::atomic::Ordering::Release);
}

#[inline]
fn velan_dec_pending_receives(velan: &VELan) {
    velan
        .OutstandingReceives
        .fetch_sub(1, core::sync::atomic::Ordering::Release);
}

#[inline]
fn velan_inc_pending_sends(velan: &VELan) {
    velan
        .OutstandingSends
        .fetch_add(1, core::sync::atomic::Ordering::Release);
}

#[inline]
fn velan_dec_pending_sends(velan: &VELan) {
    velan
        .OutstandingSends
        .fetch_sub(1, core::sync::atomic::Ordering::Release);
}

#[inline]
fn velan_dec_multiple_pending_sends(velan: &VELan, count: u32) {
    velan
        .OutstandingSends
        .fetch_sub(count, core::sync::atomic::Ordering::Release);
}

struct MuxMutex(KMUTEX);

impl MuxMutex {
    fn init(mutex: *mut KMUTEX) {
        unsafe { KeInitializeMutex(mutex, 0xFFFF) }
    }

    fn acquire(&self) {
        unsafe {
            KeWaitForSingleObject(
                core::ptr::addr_of!(self.0).cast_mut().cast(),
                windows_kernel_sys::KWAIT_REASON::Executive,
                windows_kernel_sys::MODE::KernelMode.0 as i8,
                false as u8,
                core::ptr::null_mut(),
            );
        }
    }

    fn release(&self) {
        unsafe { KeReleaseMutex(core::ptr::addr_of!(self.0).cast_mut().cast(), false as u8) };
    }
}

static MediumArray: &'static [NDIS_MEDIUM] = &[NDIS_MEDIUM::NdisMedium802_3];

/// The Mux miniport driver object, as well as the globals for the driver
#[pinned_init::pin_data]
pub(crate) struct Mux {
    // List of all virtual adapters.
    // NOTE: Not really used, and we should use the adapter list instead to get all velans.
    // VElanList: wdf_kmdf::sync::SpinPinMutex<TrustMeList<VELan, VELanList>>,
    /// List of all bound adapters.
    #[pin]
    AdapterList: WaitPinMutex<TrustMeList<Adapt, AdaptList>>,
    /// Used to assign VELAN numbers (which are used to generate MAC addresses).
    NextVElanNumber: AtomicU32,

    // Global NDIS handles

    // From NdisRegisterProtocolDriver
    ProtHandle: NdisHandle,
    // From NdisMRegisterMiniportDriver
    DriverHandle: NdisHandle,

    /// Number of VELAN miniports that exist.
    // Serves as the refcount for CDO?
    MiniportCount: AtomicI32,
    /// Device for IOCTLs.
    ControlDevice: WaitMutex<Option<ControlDeviceObject>>,
}

wdf_kmdf::impl_context_space!(Mux);

/// The control device object
struct ControlDeviceObject {
    cdo: MiniportDevice<()>,
    // From NdisMRegisterDeviceEx
    device_handle: NdisHandle,
}

// === Helper types ===

#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
struct MACAddr([u8; 6]);

impl MACAddr {
    const fn zero() -> Self {
        MACAddr([0; 6])
    }

    fn is_locally_administered(self) -> bool {
        self.0[0] & 0x02 != 0
    }
}

#[repr(C, packed)]
struct EthHeader {
    dst_addr: MACAddr,
    src_addr: MACAddr,
    eth_type: u16,
}

#[repr(transparent)]
struct NdisHandle(AtomicCell<NDIS_HANDLE>);

impl NdisHandle {
    pub(crate) fn empty() -> Self {
        Self(AtomicCell::new(core::ptr::null_mut()))
    }

    pub(crate) fn new(handle: NDIS_HANDLE) -> Self {
        Self(AtomicCell::new(handle))
    }

    pub(crate) fn as_ptr(&self) -> *mut NDIS_HANDLE {
        self.0.as_ptr()
    }

    pub(crate) fn get(&self) -> NDIS_HANDLE {
        self.0.load()
    }

    pub(crate) fn take(&self) -> Option<NDIS_HANDLE> {
        Some(self.0.swap(core::ptr::null_mut())).filter(|handle| !handle.is_null())
    }
}

unsafe impl Send for NdisHandle {}
unsafe impl Sync for NdisHandle {}

/// Since `NtListHead` is !Send
#[pinned_init::pin_data]
struct TrustMeList<Elem: NtListElement<L>, L: nt_list::NtTypedList<T = nt_list::list::NtList>> {
    #[pin]
    inner: NtListHead<Elem, L>,
}

impl<E: NtListElement<L>, L: nt_list::NtTypedList<T = nt_list::list::NtList>> TrustMeList<E, L> {
    pub fn new() -> impl pinned_init::PinInit<Self> {
        use moveit::New;

        let list = unsafe {
            pinned_init::pin_init_from_closure(|place: *mut NtListHead<E, L>| {
                let place = place.cast::<MaybeUninit<NtListHead<E, L>>>();
                let place = Pin::new_unchecked(&mut *place);

                NtListHead::new().new(place);

                Ok::<_, core::convert::Infallible>(())
            })
        };

        pinned_init::pin_init! {
            TrustMeList {
                inner <- list,
            }
        }
    }

    pub fn as_list(self: Pin<&Self>) -> Pin<&NtListHead<E, L>> {
        unsafe { self.map_unchecked(|this| &this.inner) }
    }

    pub fn as_list_mut(self: Pin<&mut Self>) -> Pin<&mut NtListHead<E, L>> {
        unsafe { self.map_unchecked_mut(|this| &mut this.inner) }
    }
}

impl<E, L> core::ops::Deref for TrustMeList<E, L>
where
    E: NtListElement<L>,
    L: nt_list::NtTypedList<T = nt_list::list::NtList>,
{
    type Target = NtListHead<E, L>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<E, L> core::ops::DerefMut for TrustMeList<E, L>
where
    E: NtListElement<L>,
    L: nt_list::NtTypedList<T = nt_list::list::NtList>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

unsafe impl<E, L> Send for TrustMeList<E, L>
where
    E: NtListElement<L>,
    L: nt_list::NtTypedList<T = nt_list::list::NtList>,
{
}

// === Common dispatch functions ===

#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    #[global_allocator]
    static ALLOCATOR: windows_kernel_rs::allocator::KernelAlloc =
        windows_kernel_rs::allocator::KernelAlloc;

    windows_kernel_rs::init_kernel_logger!(log::COMPONENT_IHVDRIVER, log::LevelFilter::Trace);

    // SAFETY: This is the driver entry point
    let driver_object = unsafe { DriverObject::new(driver_object) };
    // SAFETY: Is a copy of the original PCUNICODE_STRING, but never modified,
    // and lifetime is tied to this variable (which is bound to `DriverEntry`)
    let registry_path = unsafe {
        NtUnicodeStr::from_raw_parts(
            (*registry_path).Buffer,
            (*registry_path).Length,
            (*registry_path).MaximumLength,
        )
    };

    match mux::driver_entry(driver_object, registry_path) {
        Ok(()) => STATUS::SUCCESS,
        Err(err) => err.0,
    }
    .to_u32()
}

// unfortunately we need to declare _fltused
//
// `_fltused` is problematic since it implies that we have floating point
// operations somewhere, and on x86 kernel mode drivers should wrap floating
// point operations with state saving.
// (see https://github.com/Trantect/win_driver_example/issues/4)
//
// Since we don't plan to support the x86 arch, this isn't *too* bad,
// but still sorta bad.
#[used]
#[no_mangle]
pub static _fltused: i32 = 0;

#[cfg_attr(not(any(test, feature = "std")), panic_handler)]
#[cfg_attr(any(test, feature = "std"), allow(unused))]
fn panic(info: &core::panic::PanicInfo) -> ! {
    windows_kernel_rs::__handle_panic(info);
}

#[no_mangle]
pub extern "system" fn __CxxFrameHandler3() -> i32 {
    0
}

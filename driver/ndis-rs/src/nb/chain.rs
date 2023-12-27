/// A singly-linked list of [`NetBuffer`]s.
use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER;

use super::NetBuffer;

/// A singly-linked list of [`NetBuffer`]s.
#[derive(Debug)]
pub struct NbChain {
    head: NetBuffer,
}

impl NbChain {
    /// Creates a [`NbChain`] from an existing `NET_BUFFER` chain, or `None` if
    /// the pointer was null
    ///
    /// # Safety
    ///
    /// `head` must either be null, or a valid pointer to a `NET_BUFFER`, and
    /// all of the directly accessible `NET_BUFFER`s and `MDL`s are valid.
    unsafe fn new(head: PNET_BUFFER) -> Option<Self> {
        let head = NonNull::new(head)?;

        // SAFETY: The caller ensures that head is a valid non-null `NET_BUFFER`,
        // and that all of the accessible `NET_BUFFER`s and `MDL`s are valid.
        let head = unsafe { NetBuffer::new(head) };

        Some(Self { head })
    }
}
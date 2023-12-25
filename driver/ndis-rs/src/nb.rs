//! Helpers for working with `NET_BUFFER`s
use core::ptr::NonNull;

use windows_kernel_sys::{NET_BUFFER, PNET_BUFFER};

pub mod chain;

/// Data to send or receive over the network.
#[derive(Debug)]
#[repr(transparent)]
pub struct NetBuffer {
    /// Invariant: `NetBuffer::new` ensures that all of the accessible
    /// `NET_BUFFER`s and `MDL`s are valid.
    nb: NonNull<NET_BUFFER>,
}

impl NetBuffer {
    /// Creates a new wrapper around `nb`.
    ///
    /// # Safety
    ///
    /// `nb` must be a valid pointer to a `NET_BUFFER`, and all of the directly
    /// accessible `NET_BUFFER`s and `MDL`s are valid.
    pub(crate) unsafe fn new(nb: NonNull<NET_BUFFER>) -> Self {
        // Safety Invariant: Caller ensures that `nb` and all of the accessible
        // `NET_BUFFER`s and `MDL`s are valid.
        Self { nb }
    }

    /// Creates a new wrapper around `nb`, or `None` if `nb` was null.
    ///
    /// # Safety
    ///
    /// `nb` must either be null, or a valid pointer to a `NET_BUFFER`, and all
    /// of the directly accessible `NET_BUFFER`s and `MDL`s are valid.
    pub(crate) unsafe fn try_new(nb: PNET_BUFFER) -> Option<Self> {
        let nb = NonNull::new(nb)?;
        // SAFETY: Caller ensures that if `nb` is not null, then `nb` itself and
        // all of the accessible `NET_BUFFER`s and `MDL`s are valid as well.
        unsafe { Some(Self::new(nb)) }
    }
}

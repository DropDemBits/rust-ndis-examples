//! WDF-Based synchronization primatives

use core::{
    cell::UnsafeCell,
    marker::PhantomPinned,
    ops::{Deref, DerefMut},
    pin::Pin,
};

use pinned_init::{pin_data, PinInit};
use wdf_kmdf_sys::WDFSPINLOCK;
use windows_kernel_sys::Error;

use crate::raw;

/// A spin-lock based mutex protecting some data.
///
/// The data is guaranteed to be pinned.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
#[pin_data]
pub struct SpinMutex<T> {
    /// The backing spin lock
    spin_lock: SpinLock,
    /// Data we are protecting
    #[pin]
    data: UnsafeCell<T>,
}

impl<T> SpinMutex<T> {
    /// Creates a new mutex
    ///
    /// ## IRQL: <= Dispatch
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn new<E>(value: impl PinInit<T, E>) -> impl PinInit<Self, Error>
    where
        E: Into<Error>,
    {
        pinned_init::try_pin_init!(SpinMutex {
            spin_lock: SpinLock::new()?,
            data <- {
                let init = move |slot: *mut UnsafeCell<T>| {
                    // SAFETY: by guarantees of `pin_init_from_closure`
                    unsafe { value.__pinned_init(slot.cast()).map_err(|err| err.into()) }
                };

                // SAFETY: `data` is pinned too, and initialization requirements are guaranteed
                // by nature of delegating to `value`'s pinned init
                unsafe { pinned_init::pin_init_from_closure(init)}
            }
        }? Error)
    }

    /// Tries to acquire the mutex, returning a guard if successful
    ///
    /// ## IRQL: <= Dispatch
    pub fn try_lock(&self) -> Option<Pin<SpinMutexGuard<'_, T>>> {
        let guard = self.spin_lock.acquire();

        // SAFETY: Uhhhh
        Some(unsafe {
            Pin::new_unchecked(SpinMutexGuard {
                mutex: self,
                _guard: guard,
                _pin: PhantomPinned,
            })
        })
    }

    /// Locks the mutex, busy-looping until the lock is acquired
    ///
    /// ## IRQL: <= Dispatch
    pub fn lock(&self) -> Pin<SpinMutexGuard<'_, T>> {
        loop {
            let Some(guard) = self.try_lock() else {
                core::hint::spin_loop();
                continue;
            };
            break guard;
        }
    }
}

// Can't send `SpinMutex` to another thread because we've guaranteed that the storage will never move
// impl<T> !Send for SpinMutex<T> {}

// SAFETY: Can send &SpinMutex<T> to other threads as we can only observe `T` changing
// when we hold the lock, and only one thread can hold the lock.
//
// `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
// This is so that `SpinMutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
unsafe impl<T> Sync for SpinMutex<T> where T: Send {}

/// Lock guard for a [`SpinMutex`]
pub struct SpinMutexGuard<'a, T> {
    mutex: &'a SpinMutex<T>,
    _guard: SpinLockGuard<'a>,
    _pin: PhantomPinned,
}

impl<'a, T> Deref for SpinMutexGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &*self.mutex.data.get() }
    }
}

impl<'a, T> DerefMut for SpinMutexGuard<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &mut *self.mutex.data.get() }
    }
}

/// Wrapper around a framework-based spin lock.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the lock is acquired
pub struct SpinLock(WDFSPINLOCK);

impl SpinLock {
    /// Creates a new spin lock
    ///
    /// ## IRQL: <= Dispatch
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn new() -> Result<Self, Error> {
        let mut lock = core::ptr::null_mut();

        // SAFETY:
        // - Caller ensures we're calling this at the right IRQL
        // - We're manually managing the lifetime of the spinlock (easier that way)
        Error::to_err(unsafe { raw::WdfSpinLockCreate(None, &mut lock) })?;

        Ok(Self(lock))
    }

    /// Acquires the spin lock, returning a guard that releases the lock once dropped
    ///
    /// ## IRQL: <= Dispatch
    pub fn acquire(&self) -> SpinLockGuard<'_> {
        // SAFETY:
        // - We created the spin lock ourselves, so it's not part of an interrupt config struct
        // - Caller ensures we're calling this at the right IRQL
        unsafe {
            raw::WdfSpinLockAcquire(self.0);
        }

        SpinLockGuard { lock: self }
    }
}

impl Drop for SpinLock {
    fn drop(&mut self) {
        // SAFETY:
        // - Cleanup callbacks are always called at either `DISPATCH_LEVEL` or `PASSIVE_LEVEL`
        // - We're always deleting a spin lock, which has no special deletion requirements
        unsafe { raw::WdfObjectDelete(self.0.cast()) }
    }
}

// SAFETY: Can manifest SpinLock to another thread (as-if we'd moved a pointer)
unsafe impl Send for SpinLock {}
// SAFETY: Can send &SpinLock to other threads (no interior mutability observable)
unsafe impl Sync for SpinLock {}

/// A guard for a [`SpinLock`]
pub struct SpinLockGuard<'a> {
    lock: &'a SpinLock,
}

impl<'a> Drop for SpinLockGuard<'a> {
    fn drop(&mut self) {
        // SAFETY:
        // Having a guard guarantees we've called `WdfSpinLockAcquire`,
        // and also implies we're at IRQL `DISPATCH_LEVEL`
        unsafe { raw::WdfSpinLockRelease(self.lock.0) }
    }
}

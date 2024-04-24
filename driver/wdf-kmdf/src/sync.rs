//! WDF-Based synchronization primatives

use core::{
    cell::UnsafeCell,
    marker::PhantomPinned,
    ops::{Deref, DerefMut},
    pin::Pin,
};

use pinned_init::{pin_data, Init, PinInit};
use windows_kernel_rs::sync::Timeout;
use windows_kernel_sys::Error;

use crate::{
    handle::{DriverOwned, RawHandle, RawSpinlock, RawWaitlock},
    raw,
};

pub trait LockBackend: Sized {
    type Guard<'a>
    where
        Self: 'a;

    fn new() -> Result<Self, Error>;

    fn try_lock(&self) -> Option<Self::Guard<'_>>;

    fn lock(&self) -> Self::Guard<'_> {
        loop {
            let Some(guard) = self.try_lock() else {
                core::hint::spin_loop();
                continue;
            };
            break guard;
        }
    }
}

/// A spin-lock based mutex protecting some data.
///
/// The data is guaranteed to be pinned.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
pub type SpinPinMutex<T> = PinMutex<T, SpinLock>;

pub type SpinPinMutexGuard<'a, T> = PinMutexGuard<'a, T, SpinLock>;

/// A spin-lock based mutex protecting some data.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
pub type SpinMutex<T> = Mutex<T, SpinLock>;

pub type SpinMutexGuard<'a, T> = MutexGuard<'a, T, SpinLock>;

/// A wait-lock based mutex protecting some data.
///
/// The data is guaranteed to be pinned.
pub type WaitPinMutex<T> = PinMutex<T, WaitLock>;

pub type WaitPinMutexGuard<'a, T> = PinMutexGuard<'a, T, WaitLock>;

/// A spin-lock based mutex protecting some data.
pub type WaitMutex<T> = Mutex<T, WaitLock>;

pub type WaitMutexGuard<'a, T> = MutexGuard<'a, T, WaitLock>;

/// A mutex protecting some data.
///
/// The data is guaranteed to be pinned.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
#[pin_data]
pub struct PinMutex<T, Lock: LockBackend> {
    /// The backing lock
    lock: Lock,
    /// Data we are protecting
    #[pin]
    data: UnsafeCell<T>,
}

impl<T, Lock: LockBackend> PinMutex<T, Lock> {
    /// Creates a new pinned mutex
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
        pinned_init::try_pin_init!(Self {
            lock: Lock::new()?,
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
    pub fn try_lock(&self) -> Option<Pin<PinMutexGuard<'_, T, Lock>>> {
        // SAFETY: `PinMutexGuard` will not outlive the backing `PinMutex` since
        // it is tied to the lifetime of `PinMutex`. `PinMutexGuard` also only
        // gives out pinned references, which ensures that the backing data will
        // not be moved.
        self.lock.try_lock().map(|guard| unsafe {
            Pin::new_unchecked(PinMutexGuard {
                mutex: self,
                _guard: guard,
                _pin: PhantomPinned,
            })
        })
    }

    /// Locks the mutex, busy-looping until the lock is acquired
    ///
    /// ## IRQL: <= Dispatch
    pub fn lock(&self) -> Pin<PinMutexGuard<'_, T, Lock>> {
        let guard = self.lock.lock();

        // SAFETY: `PinMutexGuard` will not outlive the backing `PinMutex` since
        // it is tied to the lifetime of `PinMutex`. `PinMutexGuard` also only
        // gives out pinned references, which ensures that the backing data will
        // not be moved.
        unsafe {
            Pin::new_unchecked(PinMutexGuard {
                mutex: self,
                _guard: guard,
                _pin: PhantomPinned,
            })
        }
    }

    /// Gets access to the inner data
    pub fn get_mut(&mut self) -> Pin<&mut T> {
        // SAFETY: The derived mut ref to the data is not exposed anywhere else,
        // so the data will not be moved.
        unsafe { Pin::new_unchecked(self.data.get_mut()) }
    }
}

// Can't send `PinMutex` to another thread because we've guaranteed that the storage will never move
// impl<T> !Send for PinMutex<T> {}

// SAFETY: Can send &PinMutex<T> to other threads as we can only observe `T` changing
// when we hold the lock, and only one thread can hold the lock.
//
// `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
// This is so that `PinMutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
unsafe impl<T, Lock: LockBackend> Sync for PinMutex<T, Lock> where T: Send {}

/// Lock guard for a [`PinMutex`]
pub struct PinMutexGuard<'a, T, Lock: LockBackend> {
    mutex: &'a PinMutex<T, Lock>,
    _guard: Lock::Guard<'a>,
    _pin: PhantomPinned,
}

impl<'a, T, Lock: LockBackend> Deref for PinMutexGuard<'a, T, Lock> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &*self.mutex.data.get() }
    }
}

impl<'a, T, Lock: LockBackend> DerefMut for PinMutexGuard<'a, T, Lock> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &mut *self.mutex.data.get() }
    }
}

/// A mutex protecting some data.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
pub struct Mutex<T, Lock: LockBackend> {
    /// The backing lock
    lock: Lock,
    /// Data we are protecting
    data: UnsafeCell<T>,
}

impl<T, Lock: LockBackend> Mutex<T, Lock> {
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
    pub fn new<E>(value: impl Init<T, E>) -> impl Init<Self, Error>
    where
        E: Into<Error>,
    {
        pinned_init::try_init!(Self {
            lock: Lock::new()?,
            data <- {
                let init = move |slot: *mut UnsafeCell<T>| {
                    // SAFETY: by guarantees of `pin_init_from_closure`
                    unsafe { value.__init(slot.cast()).map_err(|err| err.into()) }
                };

                // SAFETY: initialization requirements are guaranteed by nature
                // of delegating to `value`'s init
                unsafe { pinned_init::init_from_closure(init)}
            }
        }? Error)
    }

    /// Tries to acquire the mutex, returning a guard if successful
    ///
    /// ## IRQL: <= Dispatch
    pub fn try_lock(&self) -> Option<MutexGuard<'_, T, Lock>> {
        // SAFETY: Uhhhh
        self.lock.try_lock().map(|guard| MutexGuard {
            mutex: self,
            _guard: guard,
            _pin: PhantomPinned,
        })
    }

    /// Locks the mutex, busy-looping until the lock is acquired
    ///
    /// ## IRQL: <= Dispatch
    pub fn lock(&self) -> MutexGuard<'_, T, Lock> {
        let guard = self.lock.lock();
        MutexGuard {
            mutex: self,
            _guard: guard,
            _pin: PhantomPinned,
        }
    }

    /// Gets access to the inner data
    pub fn get_mut(&mut self) -> &mut T {
        self.data.get_mut()
    }
}

// SAFETY: Can send `Mutex<T>` to other threads as we don't guarantee that the storage
// won't be moved.
unsafe impl<T, Lock: LockBackend> Send for Mutex<T, Lock> where T: Send {}

// SAFETY: Can send &Mutex<T> to other threads as we can only observe `T` changing
// when we hold the lock, and only one thread can hold the lock.
//
// `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
// This is so that `Mutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
unsafe impl<T, Lock: LockBackend> Sync for Mutex<T, Lock> where T: Send {}

/// Lock guard for a [`Mutex`]
pub struct MutexGuard<'a, T, Lock: LockBackend> {
    mutex: &'a Mutex<T, Lock>,
    _guard: Lock::Guard<'a>,
    _pin: PhantomPinned,
}

impl<'a, T, Lock: LockBackend> Deref for MutexGuard<'a, T, Lock> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &*self.mutex.data.get() }
    }
}

impl<'a, T, Lock: LockBackend> DerefMut for MutexGuard<'a, T, Lock> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: We have exclusive access to the data
        unsafe { &mut *self.mutex.data.get() }
    }
}

/// Wrapper around a framework-based spin lock.
///
/// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the lock is acquired
pub struct SpinLock(RawHandle<RawSpinlock, DriverOwned>);

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

        // SAFETY: We're manually managing the lifetime of the spinlock, and thus
        // the spinlock is driver-owned.
        Ok(Self(unsafe { RawHandle::create(lock) }))
    }

    /// Acquires the spin lock, returning a guard that releases the lock once dropped
    ///
    /// ## IRQL: <= Dispatch
    pub fn acquire(&self) -> SpinLockGuard<'_> {
        // SAFETY:
        // - We created the spin lock ourselves, so it's not part of an interrupt config struct
        // - Caller ensures we're calling this at the right IRQL
        unsafe {
            raw::WdfSpinLockAcquire(self.0.as_handle());
        }

        SpinLockGuard { lock: self }
    }
}

impl LockBackend for SpinLock {
    type Guard<'a> = SpinLockGuard<'a>;

    fn new() -> Result<Self, Error> {
        SpinLock::new()
    }

    fn try_lock(&self) -> Option<Self::Guard<'_>> {
        Some(self.acquire())
    }
}

/// A guard for a [`SpinLock`]
pub struct SpinLockGuard<'a> {
    lock: &'a SpinLock,
}

impl<'a> Drop for SpinLockGuard<'a> {
    fn drop(&mut self) {
        // SAFETY:
        // Having a guard guarantees we've called `WdfSpinLockAcquire`,
        // and also implies we're at IRQL `DISPATCH_LEVEL`
        unsafe { raw::WdfSpinLockRelease(self.lock.0.as_handle()) }
    }
}

/// Wrapper around a framework-based wait lock.
///
/// This does not adjust the IRQL level.
pub struct WaitLock(RawHandle<RawWaitlock, DriverOwned>);

impl WaitLock {
    /// Creates a new wait lock
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
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
        // - We're manually managing the lifetime of the waitlock (easier that way)
        Error::to_err(unsafe { raw::WdfWaitLockCreate(None, &mut lock) })?;

        // SAFETY: We're manually managing the lifetime of the waitlock, and thus
        // the waitlock is driver-owned.
        Ok(Self(unsafe { RawHandle::create(lock) }))
    }

    /// Acquires the wait lock, returning a guard that releases the lock once dropped.
    ///
    /// ## IRQL: `..=PASSIVE_LEVEL`
    pub fn acquire(&self) -> WaitLockGuard<'_> {
        // Wait forever to acquire the lock.
        let mut timeout = Timeout::forever();

        // SAFETY:
        // - We created the wait lock ourselves, so it's not part of an interrupt config struct
        // - Caller ensures we're calling this at the right IRQL
        unsafe {
            _ = raw::WdfWaitLockAcquire(self.0.as_handle(), timeout.value());
        }

        WaitLockGuard { lock: self }
    }

    /// Acquires the wait lock, returning a guard that releases the lock once dropped.
    ///
    /// ## IRQL: `..=APC_LEVEL`
    pub fn try_acquire(&self) -> Option<WaitLockGuard<'_>> {
        // Try to acquire the lock
        let mut timeout = Timeout::dont_wait();

        // SAFETY:
        // - We created the wait lock ourselves, so it's not part of an interrupt config struct
        // - Caller ensures we're calling this at the right IRQL
        let status = unsafe { raw::WdfWaitLockAcquire(self.0.as_handle(), timeout.value()) };

        match Error::to_err(status) {
            Ok(()) => Some(WaitLockGuard { lock: self }),
            Err(_) => None,
        }
    }
}

impl LockBackend for WaitLock {
    type Guard<'a> = WaitLockGuard<'a>;

    fn new() -> Result<Self, Error> {
        WaitLock::new()
    }

    fn try_lock(&self) -> Option<Self::Guard<'_>> {
        self.try_acquire()
    }

    fn lock(&self) -> Self::Guard<'_> {
        self.acquire()
    }
}

/// A guard for a [`WaitLock`]
pub struct WaitLockGuard<'a> {
    lock: &'a WaitLock,
}

impl<'a> Drop for WaitLockGuard<'a> {
    fn drop(&mut self) {
        // SAFETY:
        // Having a guard guarantees we've called `WdfWaitLockAcquire`,
        unsafe { raw::WdfWaitLockRelease(self.lock.0.as_handle()) }
    }
}

//! A one-time initializable container for an [`Arc`].
//!
//! Only takes up 1 pointer-sized value as opposed to using a `OnceLock<Arc<T>>`.
#![no_std]

// During tests, allow importing std
#[cfg(any(feature = "std", test))]
extern crate std;

// Needed for `Arc`
extern crate alloc;

use core::{
    mem::ManuallyDrop,
    sync::atomic::{AtomicPtr, Ordering},
};

use alloc::sync::Arc;

/// A one-time inizializable container
///
/// Always hands out clones of the `Arc`.
pub struct OnceArc<T> {
    /// Invariant: This either stores a valid `Arc`,
    /// or null representing no `Arc` being there
    inner: AtomicPtr<T>,
}

impl<T: core::fmt::Debug> core::fmt::Debug for OnceArc<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("OnceArc")
            .field("inner", &self.get())
            .finish()
    }
}

impl<T: Default> Default for OnceArc<T> {
    fn default() -> Self {
        Self::new(None)
    }
}

// Is a wrapper around an `Arc`, so we use the same bounds
unsafe impl<T: Send + Sync> Send for OnceArc<T> {}
unsafe impl<T: Send + Sync> Sync for OnceArc<T> {}

impl<T> OnceArc<T> {
    /// Constructs a new `OnceArc<T>`, with an optional initial value
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use once_arc::OnceArc;
    /// use alloc::sync::Arc;
    ///
    /// let once = OnceArc::new(Some(Arc::new(5)));
    /// ```
    pub fn new(value: Option<Arc<T>>) -> Self {
        Self {
            inner: AtomicPtr::new(Self::into_inner_ptr(value)),
        }
    }

    /// Gets the current `Arc<T>`, or `None` if this hasn't been initialized
    /// yet.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use once_arc::OnceArc;
    /// use alloc::sync::Arc;
    ///
    /// let once = OnceArc::new(Some(Arc::new(12)));
    /// let get = once.get();
    ///
    /// assert_eq!(get, Some(Arc::new(12)));
    /// ```
    pub fn get(&self) -> Option<Arc<T>> {
        // We need to use `Acquire` instead of `Relaxed` ordering
        // since we always want the latest (and therefore valid) pointer
        let ptr = self.inner.load(Ordering::Acquire);

        // SAFETY: `inner` is always either from a `Arc::into_raw` or null
        // by the invariant of `inner`
        unsafe { Self::manifest_cloned_arc(ptr) }
    }

    /// Takes the current `Option<Arc<T>>`
    ///
    /// Requires exclusive access as shared access would lead to the scenario
    /// of using `take` to drop the `Arc` before `get` could manifest a cloned
    /// `Arc`.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use once_arc::OnceArc;
    /// use alloc::sync::Arc;
    ///
    /// let mut once = OnceArc::new(Some(Arc::new(12)));
    /// let old = once.take();
    ///
    /// assert_eq!(once.get(), None);
    /// assert_eq!(old, Some(Arc::new(12)));
    /// ```
    pub fn take(&mut self) -> Option<Arc<T>> {
        // Take the old arc, and using `AcqRel` ordering so that
        // - `get()` sees the `null` pointer
        // - we see the latest pointer
        let old_arc = self.inner.swap(core::ptr::null_mut(), Ordering::AcqRel);

        unsafe { Self::manifest_original_arc(old_arc) }
    }

    /// Tries to initialize the `OnceArc` with `new`.
    /// If already initialized, returns `Err(current)`.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use once_arc::OnceArc;
    /// use alloc::sync::Arc;
    ///
    /// let once = OnceArc::new(None);
    ///
    /// // Initialize it for the first time...
    /// assert!(once.try_init(Arc::new(8)).is_ok());
    ///
    /// // Subsequent initialization attempts fail
    /// assert_eq!(once.try_init(Arc::new(7)), Err(Arc::new(8)));
    /// ```
    pub fn try_init(&self, new: Arc<T>) -> Result<(), Arc<T>> {
        let new_arc = Self::into_inner_ptr(Some(new));

        // Use `Release` on success so that other threads can see the latest `Arc` we put in
        // Use `Acquire` on failure since we still want a valid `Arc` to go from
        match self.inner.compare_exchange(
            core::ptr::null_mut(),
            new_arc,
            Ordering::Release,
            Ordering::Acquire,
        ) {
            Ok(old_arc) => {
                // `old_arc` should always be null
                assert!(old_arc.is_null());

                Ok(())
            }
            Err(old_arc) => {
                // Need to drop `new_arc` since it wasn't stored in `inner`
                //
                // SAFETY: Came from `Self::into_ptr`, and wasn't stored in `inner`
                // so we have exclusive ownership of the pointer
                let new_arc = unsafe { Self::manifest_original_arc(new_arc) };
                drop(new_arc);

                // Clone the original `Arc<T>` so that the caller can inspect it
                //
                // SAFETY: `inner` is always either from a `Arc::into_raw`
                // by the invariant of `inner`, and is not null because we
                // failed the compare exchange. The old `Arc` will also
                // still be valid since `take()` requires exclusive access
                // and replaces `inner` with `ptr::null()`.
                let old_arc = unsafe { Self::manifest_cloned_arc(old_arc) };
                Err(old_arc.expect("current `Arc` should not be null"))
            }
        }
    }

    /// Mainfests the original [`Arc`] from `arc_ptr`
    ///
    /// ## Safety
    ///
    /// - `arc_ptr` must be from an [`Arc::into_raw`] or null
    /// - Must have exclusive ownership to the [`Arc`] behind `arc_ptr`
    #[must_use]
    unsafe fn manifest_original_arc(arc_ptr: *mut T) -> Option<Arc<T>> {
        if arc_ptr.is_null() {
            // Holds a None
            return None;
        }

        // Manifest the Arc
        //
        // SAFETY: `arc_ptr` isn't null so it must be an Arc we have exclusive
        // ownership to by the caller's guarantees
        Some(unsafe { Arc::from_raw(arc_ptr) })
    }

    /// Mainfests the original [`Arc`] from `arc_ptr`
    ///
    /// ## Safety
    ///
    /// - `arc_ptr` must be from an [`Arc::into_raw`] or null
    /// - `arc_ptr` must have a ref-count of at least 2
    #[must_use]
    unsafe fn manifest_cloned_arc(arc_ptr: *mut T) -> Option<Arc<T>> {
        if arc_ptr.is_null() {
            None
        } else {
            // SAFETY: `inner` was derived from a real Arc,
            // and we never have to worry about the ref count dropping below
            // 1 as `try_init` is the only place that modifies `inner`,
            // and only replaces `null` with a real `Arc`.
            //
            // We also wrap the newly manifested `Arc` in a `ManuallyDrop`
            // so that in the situation of `clone` panicking, we don't
            // accidentally change the ref count below 1 (since when we drop
            // we expect to have a ref count of 1)
            let arc = ManuallyDrop::new(unsafe { Arc::from_raw(arc_ptr) });

            // Clone the arc and return it!
            Some(ManuallyDrop::into_inner(arc.clone()))
        }
    }

    /// Converts `arc` into a pointer suitable for meeting the invariant of `inner`
    fn into_inner_ptr(arc: Option<Arc<T>>) -> *mut T {
        let Some(arc) = arc else {
            // `None` is represented by a null pointer
            return core::ptr::null_mut();
        };

        // Convert into the raw pointer
        Arc::into_raw(arc).cast_mut()
    }
}

impl<T> Drop for OnceArc<T> {
    fn drop(&mut self) {
        let arc_ptr = *self.inner.get_mut();

        // Manifest the Arc so that it automatically handles dropping it for us
        //
        // SAFETY:
        // - `inner` always stores either `Arc<T>` or `None`
        // - we have a mutable reference to self, so by definition we have
        //   exclusive access
        let _ = unsafe { Self::manifest_original_arc(arc_ptr) };
    }
}

#[cfg(test)]
mod test {
    use std::thread;

    use alloc::vec::Vec;

    use super::*;

    #[test]
    fn assert_send_and_sync() {
        fn is_send_sync<T: Send + Sync>() {}

        is_send_sync::<OnceArc<u32>>();
    }

    #[test]
    fn test_concurrent_get() {
        let once = &OnceArc::<u32>::new(None);

        thread::scope(|s| {
            let _reader_threads = (0..2)
                .map(|_| {
                    s.spawn(|| {
                        for _ in 0..1000 {
                            let v = once.get();

                            if let Some(v) = v {
                                assert!((3..4).contains(&*v));
                            }
                        }
                    })
                })
                .collect::<Vec<_>>();

            s.spawn(|| {
                let _res = once.try_init(Arc::new(3));
            });

            s.spawn(|| {
                let _res = once.try_init(Arc::new(4));
            });
        })
    }
}

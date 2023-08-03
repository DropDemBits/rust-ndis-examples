//! A simple swappable [`Arc`]
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

/// A swappable `Option<Arc<T>>`
pub struct OptionArcSwap<T> {
    /// Invariant: This either stores an arc with a ref count of 2,
    /// or null representing an empty option
    inner: AtomicPtr<T>,
}

// Is a wrapper around an `Arc`, so we use the same bounds
unsafe impl<T: Send + Sync> Send for OptionArcSwap<T> {}
unsafe impl<T: Send + Sync> Sync for OptionArcSwap<T> {}

impl<T> OptionArcSwap<T> {
    /// Constructs a new `OptionSwapArc<T>`
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(Some(Arc::new(5)));
    /// ```
    pub fn new(value: Option<Arc<T>>) -> Self {
        Self {
            inner: AtomicPtr::new(Self::into_inner_ptr(value)),
        }
    }

    /// Loads the current `Option<Arc<T>>`
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(Some(Arc::new(12)));
    /// let load = swap.load();
    ///
    /// assert_eq!(load, Some(Arc::new(12)));
    /// ```
    pub fn load(&self) -> Option<Arc<T>> {
        let ptr = self.inner.load(Ordering::Acquire);

        // SAFETY: `inner` is always either from a `Arc::into_raw` or null
        // by the invariant of `inner`
        unsafe { Self::manifest_cloned_arc(ptr) }
    }

    /// Swaps the current `Option<Arc<T>>` with `new` and returns the original
    /// `Option<Arc<T>>`
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(Some(Arc::new(6)));
    /// let old = swap.swap(Some(Arc::new(12)));
    ///
    /// assert_eq!(swap.load(), Some(Arc::new(12)));
    /// assert_eq!(old, Some(Arc::new(6)));
    /// ```
    pub fn swap(&self, new: Option<Arc<T>>) -> Option<Arc<T>> {
        let new_arc = Self::into_inner_ptr(new);

        // Swap with the old, we have exclusive ownership to the old Arc instance now
        let old_arc = self.inner.swap(new_arc, Ordering::AcqRel);

        // SAFETY: We only ever use `into_inner_ptr` to make pointers satisfying the invariants
        // and swapping gave us the original arc inside `inner`
        let old_arc = unsafe { Self::manifest_original_arc(old_arc) };

        // Give back the `Arc` we had
        old_arc
    }

    /// Replaces the current `Option<Arc<T>>`
    ///
    /// Essentially just a wrapper around `swap` that drops the original value.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(Some(Arc::new(12)));
    /// swap.store(Some(Arc::new(6)));
    ///
    /// assert_eq!(swap.load(), Some(Arc::new(6)));
    /// ```
    pub fn store(&self, new: Option<Arc<T>>) {
        // Just drop the old value
        let _ = self.swap(new);
    }

    /// Takes the current `Option<Arc<T>>` and fills it in with `None`
    ///
    /// Essentially just a wrapper around `swap` that swaps in `None`.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(Some(Arc::new(12)));
    /// let old = swap.take();
    ///
    /// assert_eq!(swap.load(), None);
    /// assert_eq!(old, Arc::new(12));
    /// ```
    pub fn take(&self) -> Option<Arc<T>> {
        self.swap(None)
    }

    /// Tests if the current `Option<Arc<T>>` is `None`, and swaps in `new` if that is the case.
    ///
    /// If unsuccessful, returns `Err(current)`.
    ///
    /// ## Examples
    ///
    /// ```rust
    ///# extern crate alloc;
    ///# use simple_arc_swap::OptionArcSwap;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionArcSwap::new(None);
    ///
    /// assert!(swap.swap_if_none(Some(Arc::new(8))).is_ok());
    /// ```
    pub fn swap_if_none(&self, new: Option<Arc<T>>) -> Result<(), Option<Arc<T>>> {
        let new_arc = Self::into_inner_ptr(new);

        match self.inner.compare_exchange(
            core::ptr::null_mut(),
            new_arc,
            Ordering::Release,
            Ordering::Acquire,
        ) {
            Ok(old_arc) => {
                // Manifest the original arc so that we can drop the ref to it
                //
                // SAFETY: We only ever use `into_inner_ptr` to make pointers satisfying the invariants
                // and swapping gave us the original arc inside `inner`
                let _ = unsafe { Self::manifest_original_arc(old_arc) };

                Ok(())
            }
            Err(old_arc) => {
                // Need to drop `new_arc_ptr` since it wasn't stored in `inner`
                //
                // SAFETY: Came from `Self::into_ptr`, and wasn't stored in `inner`
                // so we have exclusive ownership of the pointer
                let new_arc = unsafe { Self::manifest_original_arc(new_arc) };
                drop(new_arc);

                // Clone the original `Option<Arc<T>>` so that the caller can inspect it
                //
                // SAFETY: `inner` is always either from a `Arc::into_raw` or null
                // by the invariant of `inner`
                let old_arc = unsafe { Self::manifest_cloned_arc(old_arc) };
                Err(old_arc)
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

        // Decrement the ref count so that we can get the real ref count
        //
        // SAFETY: `arc_ptr` isn't null so it must be an Arc, it is the
        // caller's responsibility to ensure exclusive access to the Arc,
        // and our internal invariant is that we have a ref count of 2
        unsafe { Arc::decrement_strong_count(arc_ptr) }

        // Manifest the Arc
        //
        // SAFETY: `arc_ptr` isn't null so it must be an Arc we have exclusive
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
            // and incremented the ref count to 2 when making this
            //
            // We also wrap the newly manifested `Arc` in a `ManuallyDrop`
            // so that in the situation of `clone` panicking, we don't
            // accidentally change the ref count below 2 (since when we drop
            // we expect to have a ref count of 2)
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

        // Ensure that we always have a ref count of 2 so that we can safely
        // make a ref out of thin air without having to worry about the ref
        // count dropping to 0 while we're making it
        //
        // We also do this first so that in the event that `Arc::clone` panics,
        // we're able to drop the original `Arc`
        core::mem::forget(arc.clone());

        // Convert into the raw pointer
        Arc::into_raw(arc).cast_mut()
    }
}

impl<T> Drop for OptionArcSwap<T> {
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
    use super::*;

    #[test]
    fn assert_send_and_sync() {
        fn is_send_sync<T: Send + Sync>() {}

        is_send_sync::<OptionArcSwap<u32>>();
    }
}

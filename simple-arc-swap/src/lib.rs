//! A simple swappable [`Arc`]
#![no_std]

// During tests, allow importing std
#[cfg(any(feature = "std", test))]
#[macro_use]
extern crate std;

// Needed for `Arc`
extern crate alloc;

use core::{
    mem::ManuallyDrop,
    sync::atomic::{AtomicPtr, Ordering},
};

use alloc::sync::Arc;

pub struct OptionArcSwap<T> {
    /// Invariant: This either stores an arc with a ref count of 2,
    /// or null representing an empty option
    inner: AtomicPtr<T>,
}

// Is a wrapper around an `Arc`, so we use the same bounds
unsafe impl<T: Send + Sync> Send for OptionArcSwap<T> {}
unsafe impl<T: Send + Sync> Sync for OptionArcSwap<T> {}

impl<T> OptionArcSwap<T> {
    /// Constructs a new `Arc<T>`
    ///
    /// ## Examples
    ///
    /// ```rust
    ///#extern crate alloc;
    /// use alloc::sync::Arc;
    ///
    /// let swap = OptionSwapArc::new(Some(Arc::new(5)));
    /// ```
    pub fn new(value: Option<Arc<T>>) -> Self {
        let Some(arc) = value else {
            return Self {
                inner: AtomicPtr::new(core::ptr::null_mut()),
            };
        };

        let arc_ptr = Arc::into_raw(arc);

        // Ensure that we always have a ref count of 2 so that we can safely
        // make a ref out of thin air without having to worry about the ref
        // count dropping to 0 while we're making it
        //
        // SAFETY: We just derived `arc_ptr` from the Arc above
        unsafe {
            Arc::increment_strong_count(arc_ptr);
        }

        Self {
            inner: AtomicPtr::new(arc_ptr.cast_mut()),
        }
    }

    pub fn load(&self) -> Option<Arc<T>> {
        let ptr = self.inner.load(Ordering::Relaxed);

        if ptr.is_null() {
            None
        } else {
            // SAFETY: `inner` was derived from a real Arc,
            // and incremented the ref count to 2 when making this
            //
            // We also wrap the newly manifested `Arc` in a `ManuallyDrop`
            // so that in the situation of `clone` panicking, we don't
            // accidentally change the ref count below 2 (since when we drop
            // we expect to have a ref count of 2)
            let arc = ManuallyDrop::new(unsafe { Arc::from_raw(ptr) });

            // Clone the arc and return it!
            Some(ManuallyDrop::into_inner(arc.clone()))
        }
    }
}

impl<T> Drop for OptionArcSwap<T> {
    fn drop(&mut self) {
        let arc_ptr = *self.inner.get_mut();

        if arc_ptr.is_null() {
            // Holds an option, can return
            return;
        }

        // Decrement the ref count so that we can drop the arc
        //
        // SAFETY: `arc_ptr` isn't null so it must be an Arc,
        // we have exclusive access to the Arc, and our internal
        // invariant is that we have a ref count of 2
        unsafe { Arc::decrement_strong_count(arc_ptr) }

        // Manifest and drop the Arc
        //
        // SAFETY: `arc_ptr` isn't null so it must be an Arc,
        // we have exclusive
        let _ = unsafe { Arc::from_raw(arc_ptr) };
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

//! Helper utilities for making it easier to work with `NET_BUFFER_LIST`s and `NET_BUFFER`s
//!
//! Based off of [ndis-driver-library](https://github.com/microsoft/ndis-driver-library).
#![no_std]

// During tests, allow importing std
#[cfg(any(test))]
extern crate std;

pub mod nb;
pub mod nb_list;

pub use nb::{chain::NbChain, NetBuffer};
pub use nb_list::{
    chain::NblChain, counted_queue::NblCountedQueue, queue::NblQueue, NetBufferList,
};

// NblQueue: can iter & iter_mut over individual nbls, can append & prepend
// -
//
// NblChain: can iter & iter_mut over individual nbls, but can't append & prepend
// - XXX: With lifetime attached? would allow for NblQueue -> as &NblChain conversion
// - Is there any scenario where we get an NBL but can't modify the chain?
//   - Protocol drivers in `ProtocolReceiveNetBufferLists` can partition the list
//   - Filter drivers in `FilterSendNetBufferListsComplete` can partition the list
//   - Intermediate drivers in `MiniportSendNetBufferLists` can reorder
//

#[cfg(test)]
pub(crate) mod test {
    use std::boxed::Box;

    use crate::{NetBuffer, NetBufferList};

    /// Allocates a [`NetBufferList`]
    pub(crate) fn alloc_nbl() -> Box<NetBufferList> {
        use std::alloc::Layout;
        let layout = Layout::new::<NetBufferList>();
        let blah = unsafe { std::alloc::alloc_zeroed(layout).cast() };
        unsafe { Box::from_raw(blah) }
    }

    /// Allocates a [`NetBuffer`]
    pub(crate) fn alloc_nb() -> Box<NetBuffer> {
        use std::alloc::Layout;
        let layout = Layout::new::<NetBuffer>();
        let blah = unsafe { std::alloc::alloc_zeroed(layout).cast() };
        unsafe { Box::from_raw(blah) }
    }
}

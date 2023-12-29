//! Helper utilities for making it easier to work with `NET_BUFFER_LIST`s and `NET_BUFFER`s
//!
//! Based off of [ndis-driver-library](https://github.com/microsoft/ndis-driver-library).
#![no_std]
#![deny(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

// During tests, allow importing std
#[cfg(test)]
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

    use crate::{NbChain, NblChain, NetBuffer, NetBufferList};

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

    pub(crate) fn free_nbls(nbls: NblChain) {
        for nbl in nbls.into_iter() {
            let nb_chain = std::mem::take(nbl.nb_chain_mut());
            free_nbs(nb_chain);
            unsafe { drop(Box::from_raw(nbl)) };
        }
    }

    pub(crate) fn free_nbs(nbs: NbChain) {
        for nb in nbs.into_iter() {
            unsafe { drop(Box::from_raw(nb)) };
        }
    }
}

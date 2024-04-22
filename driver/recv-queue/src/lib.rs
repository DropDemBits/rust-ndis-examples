#![no_std]
#![deny(unsafe_op_in_unsafe_fn, clippy::multiple_unsafe_ops_per_block)]

use ndis_rs::NetBufferList;

// During tests, allow importing std
#[cfg(test)]
extern crate std;

#[derive(Debug)]
pub struct RecvQueue<const LEN: usize> {
    // Used in
    // - recv::ServiceReads (read, technically write into_iter)
    // - recv::QueueReceiveNBL (write)
    // - recv::FlushReceiveQueue (write into_iter)
    // - BindAdapter (write)
    queue: [Option<&'static mut NetBufferList>; LEN],

    // read == write
    // write is always ahead or at read
    //
    // ???: wrapping behaviour?
    //
    // replacement behaviour:
    // bump read head
    // take element
    // bump write head

    //
    // |  0  |  0  |  0  |  0  |
    // ^
    // read (0, 0)
    // write (0, 0)
    //
    // |  a  |  0  |  0  |  0  |
    // ^     ^
    // read  |
    //       write (0, 1)
    //
    // |  a  |  b  |  0  |  0  |
    // ^           ^
    // read        |
    //             write (0, 2)
    //
    // |  a  |  b  |  c  |  0  |
    // ^                 ^
    // read              |
    //                   write (0, 3)
    //
    // |  a  |  b  |  c  |  d  |
    // ^                       ^
    // read                    |
    //                         write (1, 0)

    // read head & write head are composed of a
    // - lap portion (head / LEN)
    // - index portion (head % LEN)
    //
    // the queue is empty if they're on the same index & lap
    read_head: usize,
    write_head: usize,
}

impl<const LEN: usize> RecvQueue<LEN> {
    pub fn new() -> Self {
        Self {
            queue: core::array::from_fn(|_| None),
            read_head: 0,
            write_head: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.read_head == self.write_head
    }

    pub fn is_full(&self) -> bool {
        self.len() == (LEN - 1) as u32
    }

    pub fn len(&self) -> u32 {
        (self.write_head.wrapping_sub(self.read_head) % LEN) as u32
    }

    pub fn enqueue(
        &mut self,
        element: &'static mut NetBufferList,
    ) -> Option<&'static mut NetBufferList> {
        let old_element = if self.is_full() { self.dequeue() } else { None };
        let _ = self.enqueue_inner(element);
        old_element
    }

    fn enqueue_inner(
        &mut self,
        element: &'static mut NetBufferList,
    ) -> Result<(), &'static mut NetBufferList> {
        if self.is_full() {
            return Err(element);
        }

        let slot = self.write_head;
        let old_slot = self.queue[slot].replace(element);
        debug_assert!(
            old_slot.is_none(),
            "writing to existing slot {slot:x?} {:x?} -> {old_slot:x?} ({:#x?} @ r {:x?} w {:x?})",
            self.queue[slot],
            self.queue,
            self.read_head,
            self.write_head,
        );
        self.write_head = self.write_head.wrapping_add(1) % LEN;
        Ok(())
    }

    pub fn dequeue(&mut self) -> Option<&'static mut NetBufferList> {
        if self.read_head == self.write_head {
            // queue is empty
            return None;
        }

        let element = self.queue[self.read_head].take();
        self.read_head = self.read_head.wrapping_add(1) % LEN;
        element
    }

    pub fn front(&self) -> Option<&NetBufferList> {
        if self.is_empty() {
            return None;
        }

        self.queue[self.read_head].as_ref().map(|it| &**it)
    }
}

#[cfg(test)]
mod test {
    use std::boxed::Box;

    use ndis_rs::{NbChain, NblChain, NetBufferList};

    use crate::RecvQueue;

    /// Allocates a [`NetBufferList`]
    fn alloc_nbl() -> Box<NetBufferList> {
        use std::alloc::Layout;
        let layout = Layout::new::<NetBufferList>();
        let blah = unsafe { std::alloc::alloc_zeroed(layout).cast() };
        unsafe { Box::from_raw(blah) }
    }

    fn free_nbl(nbl: &mut NetBufferList) {
        unsafe { drop(Box::from_raw(nbl)) };
    }

    #[test]
    fn fill_recv_queue() {
        let mut queue = RecvQueue::<4>::new();

        for read_count in 0..3 {
            for i in 0..(4 * 5 * read_count) {
                let read = i % 5 == 0;
                if read {
                    if let Some(old) = queue.dequeue() {
                        free_nbl(old)
                    }
                    if let Some(old) = queue.dequeue() {
                        free_nbl(old)
                    }
                }

                if let Some(old) = queue.enqueue(Box::leak(alloc_nbl())) {
                    free_nbl(old)
                }

                std::eprintln!("{i}: {queue:x?} ({read})");
            }
        }

        // free any remaining bits
        while let Some(nbl) = queue.dequeue() {
            free_nbl(nbl)
        }

        assert!(
            queue.queue.iter().all(|slot| slot.is_none()),
            "queue: {queue:#x?}"
        );
    }
}

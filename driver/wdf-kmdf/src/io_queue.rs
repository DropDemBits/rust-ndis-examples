use core::marker::PhantomData;

use wdf_kmdf_sys::{WDFDEVICE, WDFQUEUE, WDF_IO_QUEUE_CONFIG};
use windows_kernel_sys::{result::STATUS, Error};

use crate::{
    handle::{DriverOwned, HandleWrapper, RawHandle},
    raw,
    request::Request,
};

pub struct IoQueue<R> {
    handle: RawHandle<WDFQUEUE, DriverOwned>,
    _request: PhantomData<R>,
}

impl<R> IoQueue<R> {
    /// Creates an IO queue for the given device
    ///
    /// A device can have multiple IO queues.
    ///
    /// ## IRQL: <= `DISPATCH_LEVEL`
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn create(device: WDFDEVICE, mut config: WDF_IO_QUEUE_CONFIG) -> Result<Self, Error> {
        let handle = {
            let mut handle = core::ptr::null_mut();

            // SAFETY: uhhhhhh
            Error::to_err(unsafe {
                raw::WdfIoQueueCreate(device, &mut config, None, Some(&mut handle))
            })?;

            // SAFETY: uhhhhhh
            unsafe { RawHandle::create(handle) }
        };

        Ok(Self {
            handle,
            _request: PhantomData,
        })
    }

    /// Gets the request handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFQUEUE {
        self.handle.as_handle()
    }

    /// Informs the framework to stop queuing IO requests to the queue
    /// and cancel any unprocessed and driver-owned cancellable requests
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn purge_synchronously(&self) {
        // SAFETY: uhhhhhh
        unsafe { raw::WdfIoQueuePurgeSynchronously(self.handle.as_handle()) }
    }

    /// Enables the IO queue to start delivering IO requests
    ///
    /// If IO requests are present when [`IoQueue::start`] is called, the
    /// queue's request handlers can be called before the end of the call to
    /// [`IoQueue::start`]. Therefore, any locks that the request handlers may
    /// acquire must not be held before calling [`IoQueue::start`], otherwise a
    /// deadlock will happen.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn start(&self) {
        // SAFETY: uhhhhhh
        unsafe { raw::WdfIoQueueStart(self.handle.as_handle()) }
    }
}

impl<R> IoQueue<R>
where
    R: Request,
{
    /// Requeues the IO request to another driver-owned IO queue
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn forward_to_queue(&self, request: R) -> Result<(), Error> {
        // SAFETY: uhhhhhh
        Error::to_err(unsafe {
            raw::WdfRequestForwardToIoQueue(
                request.as_object_handle().cast(),
                self.handle.as_handle(),
            )
        })
    }

    /// Retrieves the next available IO request from the queue
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    // ???: May want to transform into `process_next_request` taking in a closure,
    // which ensures that a request is always processed
    pub fn retrive_next_request(&self) -> Option<Result<R, Error>> {
        let mut request = core::ptr::null_mut();
        // SAFETY: uhhhhhh
        let status = Error::to_err(unsafe {
            raw::WdfIoQueueRetrieveNextRequest(self.handle.as_handle(), &mut request)
        });

        if status == Err(STATUS::NO_MORE_ENTRIES.into()) {
            None
        } else {
            // SAFETY: uhhhhhh
            Some(status.map(|_| unsafe { R::wrap_raw(request.cast()) }))
        }
    }
}

impl<R> HandleWrapper for IoQueue<R> {
    type Handle = WDFQUEUE;

    unsafe fn wrap_raw(raw: wdf_kmdf_sys::WDFOBJECT) -> Self {
        Self {
            // SAFETY: Caller ensures that the handle is valid
            handle: unsafe { RawHandle::wrap_raw(raw) },
            _request: PhantomData,
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<R> core::fmt::Debug for IoQueue<R> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Queue").field(&self.handle).finish()
    }
}

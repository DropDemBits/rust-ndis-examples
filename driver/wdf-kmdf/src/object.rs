use pinned_init::PinInit;
use wdf_kmdf_sys::{
    WDFOBJECT, WDF_EXECUTION_LEVEL, WDF_OBJECT_ATTRIBUTES, WDF_SYNCHRONIZATION_SCOPE,
};
use windows_kernel_sys::Error;

use crate::{
    context_space::{self, IntoContextSpace},
    handle::{
        DriverOwned, HandleWrapper, HasContext, RawHandleWithContext, RawObject, Ref, Unique,
        Wrapped,
    },
    impl_clone_ref, raw,
};

/// The maximum IRQL the object's event callback functions will be called at
///
/// Execution levels can only be specified for:
///
///
/// - Driver objects
/// - Device objects
/// - File objects
/// - General objects
///
/// And since WDF 1.9 and later:
///
/// - Queue objects
/// - Timer objects
#[derive(Copy, Clone, PartialEq, Eq, Default)]
#[repr(i32)]
pub enum ExecutionLevel {
    /// Use the execution level from the parent device object.
    ///
    /// This is also used as the default execution level for [`Driver`](crate::driver::Driver),
    /// where:
    /// - In KMDF, the default execution level is [`ExecutionLevel::Dispatch`]
    /// - In UMDF, the default execution level is [`ExecutionLevel::Passive`]
    #[default]
    InheritFromParent = WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent.0,
    /// Always execute event callbacks at IRQL == PASSIVE_LEVEL
    Passive = WDF_EXECUTION_LEVEL::WdfExecutionLevelPassive.0,
    /// Execute event callbacks at IRQL <= DISPATCH_LEVEL (KMDF only)
    Dispatch = WDF_EXECUTION_LEVEL::WdfExecutionLevelDispatch.0,
}

/// What scope to sequentialize event callbacks at
///
/// Synchronization scopes can only be specified for
///
/// - Driver objects
/// - Device objects
/// - Queue objects
#[derive(Copy, Clone, PartialEq, Eq, Default)]
#[repr(i32)]
pub enum SynchronizationScope {
    /// Use the synchronization scope from the parent device object.
    ///
    /// This is also used as the default synchronization level for [`Driver`](crate::driver::Driver),
    /// where it is [`SynchronizationScope::None`].
    #[default]
    InheritFromParent = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent.0,
    /// Event callbacks for all of the device's descendant objects will be executed sequentially (i.e.
    /// before executing an event callback, the device's synchronization lock is acquired).
    ///
    /// This applies to:
    ///
    /// - Queue objects
    /// - File objects
    ///
    /// If the `AutomaticSerialization` flag is true in the object's config struct,
    /// this also applies to the event callbacks of:
    ///
    /// - Interrupt objects
    /// - DPC objects
    /// - Work Item objects
    /// - Timer objects
    ///
    /// Note that this is sequentialization does not happen between device object hierarchies,
    /// which are free to independently execute event callbacks concurrently.
    Device = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeDevice.0,
    /// Event callbacks for a queue will be executed sequentially (i.e. before executing a queue
    /// event callback, the queue's synchronization lock is acquired).
    ///
    /// If the `AutomaticSerialization` flag is true in the object's config struct,
    /// this also applies to the event callbacks of:
    ///
    /// - Interrupt objects
    /// - DPC objects
    /// - Work Item objects
    /// - Timer objects
    ///
    /// Note that this is sequentialization does not happen between queue object hierarchies,
    /// which are free to independently execute event callbacks concurrently.
    ///
    /// Since WDF 1.9, `SynchronizationScope::Queue` should be specified on the queue objects
    /// themselves.
    /// For older WDF versions, the queue object should specify `SynchronizationScope::InheritFromParent`,
    /// and the parent object should specify `SynchronizationScope::Queue`
    Queue = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeQueue.0,
    /// No synchronization is performed, so event callbacks may execute concurrently
    None = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeNone.0,
}

pub struct GeneralObject<T: IntoContextSpace> {
    handle: RawHandleWithContext<RawObject, T, DriverOwned>,
}

impl<T> GeneralObject<T>
where
    T: IntoContextSpace,
{
    /// Creates a new WDF General Object
    ///
    /// ## IRQL: <= `DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn create<I>(init_context: impl FnOnce(&Self) -> Result<I, Error>) -> Result<Self, Error>
    where
        I: PinInit<T, Error>,
    {
        Self::_create(
            context_space::default_object_attributes::<T>(),
            init_context,
            |handle| {
                // SAFETY: We own this handle as we take it from `WdfObjectCreate`.
                let handle = unsafe { RawHandleWithContext::create(handle) };
                Self { handle }
            },
        )
    }

    /// Creates a new WDF General Object attached to an object
    ///
    /// ## IRQL: <= `DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn with_parent<I>(
        parent: &impl HandleWrapper,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
    ) -> Result<Ref<Self>, Error>
    where
        I: PinInit<T, Error>,
    {
        let mut object_attrs = context_space::default_object_attributes::<T>();
        object_attrs.ParentObject = parent.as_object_handle();

        Self::_create(object_attrs, init_context, |handle| {
            // SAFETY: We own this handle as we take it from `WdfObjectCreate`.
            let handle = unsafe { RawHandleWithContext::create_parented(handle) };
            let handle = Self { handle };
            // SAFETY: `handle` comes from the `create_parented` call above
            unsafe { Ref::into_parented(handle) }
        })
    }

    /// Creates a new WDF General Object with exclusive access to the context space
    ///
    /// ## IRQL: <= `DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn create_unique<I>(
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
    ) -> Result<Unique<Self, T>, Error>
    where
        I: PinInit<T, Error>,
    {
        Self::_create_unique(
            context_space::default_object_attributes::<T>(),
            init_context,
            |handle| {
                // SAFETY: We own this handle as we take it from `WdfObjectCreate`.
                let handle = unsafe { RawHandleWithContext::create(handle) };
                Self { handle }
            },
        )
    }

    /// Creates a new WDF General Object attached to an object, with exclusive access to the context space
    ///
    /// ## IRQL: <= `DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn with_parent_unique<I>(
        parent: &impl HandleWrapper,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
    ) -> Result<Unique<Ref<Self>, T>, Error>
    where
        I: PinInit<T, Error>,
    {
        let mut object_attrs = context_space::default_object_attributes::<T>();
        object_attrs.ParentObject = parent.as_object_handle();

        Self::_create_unique(object_attrs, init_context, |handle| {
            // SAFETY: We own this handle as we take it from `WdfObjectCreate`.
            let handle = unsafe { RawHandleWithContext::create_parented(handle) };
            let handle = Self { handle };
            // SAFETY: `handle` comes from the `create_parented` call above
            unsafe { Ref::into_parented(handle) }
        })
    }

    fn _create<Handle, I>(
        mut object_attrs: WDF_OBJECT_ATTRIBUTES,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
        create_handle: impl FnOnce(WDFOBJECT) -> Handle,
    ) -> Result<Handle, Error>
    where
        Handle: HandleWrapper + AsRef<Self>,
        I: PinInit<T, Error>,
    {
        let handle = {
            let mut handle = core::ptr::null_mut();
            // SAFETY:
            // - Caller ensures that we're at the right IRQL
            unsafe { Error::to_err(raw::WdfObjectCreate(Some(&mut object_attrs), &mut handle)) }?;
            // Wrap it in the right raw handle
            create_handle(handle)
        };

        // SAFETY:
        // - It's WDF's responsibility to insert the context area, since we create
        //   the default object attributes with T's context area
        // - The object was just created, and the context space has not been initialized yet
        unsafe { context_space::context_pin_init(handle.as_ref(), init_context)? };

        Ok(handle)
    }

    fn _create_unique<Handle, I>(
        mut object_attrs: WDF_OBJECT_ATTRIBUTES,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
        create_handle: impl FnOnce(WDFOBJECT) -> Handle,
    ) -> Result<Unique<Handle, T>, Error>
    where
        Handle: HandleWrapper + AsRef<Self> + HasContext<T>,
        I: PinInit<T, Error>,
    {
        let handle = {
            let mut handle = core::ptr::null_mut();
            // SAFETY:
            // - Caller ensures that we're at the right IRQL
            unsafe { Error::to_err(raw::WdfObjectCreate(Some(&mut object_attrs), &mut handle)) }?;
            // Wrap it in the right raw handle
            create_handle(handle)
        };

        // SAFETY:
        // - It's WDF's responsibility to insert the context area, since we create
        //   the default object attributes with T's context area
        // - The object was just created, and the context space has not been initialized yet
        unsafe { context_space::context_pin_init_mut(handle.as_ref(), init_context)? };

        let handle = unsafe { Unique::into_unique(handle) };

        Ok(handle)
    }

    /// Wraps the raw handle in a general object wrapper
    ///
    /// ## Safety
    ///
    /// Respect aliasing rules, since this can be used to
    /// generate aliasing mutable references to the context space.
    pub unsafe fn wrap(handle: WDFOBJECT) -> Wrapped<Self> {
        // SAFETY: It's the caller's responsibility to ensure that this doesn't
        // generate aliasing references on drop.
        unsafe { Wrapped::wrap_raw(handle) }
    }

    /// Gets the object handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFOBJECT {
        self.handle.as_handle()
    }
}

impl<T: IntoContextSpace> HandleWrapper for GeneralObject<T> {
    type Handle = RawObject;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { RawHandleWithContext::wrap_raw(raw) },
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl_clone_ref!(GeneralObject<T: IntoContextSpace>);

impl<T: IntoContextSpace> HasContext<T> for GeneralObject<T> {}

impl<T: IntoContextSpace> AsRef<GeneralObject<T>> for GeneralObject<T> {
    fn as_ref(&self) -> &GeneralObject<T> {
        self
    }
}

impl<T: IntoContextSpace> core::fmt::Debug for GeneralObject<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("GeneralObject")
            .field("handle", &self.handle)
            .finish()
    }
}

impl<T: IntoContextSpace> PartialEq for GeneralObject<T> {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<T: IntoContextSpace> Eq for GeneralObject<T> {}

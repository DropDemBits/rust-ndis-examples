# Note taking for implementation and other reasons

Topic groups are separated by h2 level headers, and can be modified at any time

## Accessibility of driver globals

A device needs to know what driver it's a part of, and we can get a device
from an IoQueue, FileObject, ChildList, IoTarget, DmaTransaction, Interrupt,
WmiInstance and WmiProvider (we only care about the first 2 for ndisprot).

Note that that accessing a driver from a device means that (driver) context access
needs to be fallible (see below)

## Keep Calm and Don't Panic

Probably want to avoid any sort of panicing behaviour (e.g. `Option::unwrap`)
so that we can recover from anything. For example, if we somehow get the context
space of a driver during context space init, it should be a faillible operation
since by that point the context space isn't initialized yet.

## Unloading a driver

For a non-miniport driver, `FxStubDriverUnload` calls [`FxDriver::Unload`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/core/fxdriver.cpp#L167) , which first calls the `DriverUnload` callback specified during driver creation.

In `FxDriver::Unload` there's a call to `DeleteObject`, which calls `EvtCleanup` and `EvtDestroy` in the usual way. This potentially means that we can use `EvtDestroy` as the place to destroy the driver context, since by the definition of `EvtDestroy` callback we have exclusive access to the space. Note that also the entire driver framework object hierarchy gets destroyed inside of the WDM `Unload` callback, which is relevant in the case of [`NdisDeregisterProtocolDriver`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ndis/nf-ndis-ndisderegisterprotocoldriver) since it's typically called from the `Unload` callback (but doesn't necessarily mean that it needs to be called inside of WDF's `DriverUnload` callback). There's also [`NdisUnbindAdapter`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ndis/nf-ndis-ndisunbindadapter) which can be used to guarantee that all of the bound adapters are unbound before we get to deregistering the protocol driver (e.g. if we represent bound adapters using WDF objects which are parented to the protocol driver).

## Can't Use `lld-link`

`lld-link` lets through the `.cfg00` and `.retplne` sections. `.retplne` is particularly problematic because it has characteristics 0, resulting in an inaccessible page and causing a bug-check (MEMORY_MANAGEMENT (1a) \[ 0x3000, \<VA\>, \<PTE\>, \<NTSTATUS\> \]).

Even once that's worked around (e.g. by merging `.retplne` into `.rdata`), all of the non-pageable sections (i.e. any section not `PAGE`) are missing the not-pageable flag, so they could be paged out at any time causing spurious crashes.

Technically we could use this <https://learn.microsoft.com/en-us/cpp/build/reference/section-specify-section-attributes?view=msvc-170>, but it's easier to just stick with msvc link.exe for now.

## NDIS Callback Ordering & Interleaving

Since the NdisProt sample waits in `NdisprotBindAdapter` using `NdisWaitEvent` (which uses `KeWaitForSingleObject`) and `NdisprotOpenAdapterComplete` does get called at some point, we can assume that NDIS uses separate threads when calling `ProtocolBindAdapterEx` and `ProtocolOpenAdapterCompleteEx`. This can also be assumed for the following callbacks for the same reason:
- `ProtocolUnbindAdapterEx`
- `ProtocolCloseAdapterCompleteEx`
- `ProtocolOidRequestComplete`

Through observation, the following callbacks are known to interleave with itself:
- `ProtocolNetPnPEvent`
and these are likely to interleave with itself:
- `ProtocolReceiveNetBufferLists`
- `ProtocolSendNetBufferListsComplete`
- `ProtocolStatusEx`
- `ProtocolDirectOidRequestComplete`

Unknown when this is called in relation to the other callbacks:
- `ProtocolUninstall`

Q's:
### What callbacks can be called before `NdisRegisterProtocolDriver` returns?

Per [NdisRegisterProtocolDriver#Remarks](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ndis/nf-ndis-ndisregisterprotocoldriver#remarks):
  > Drivers that call **NdisRegisterProtocolDriver** must be prepared for an immediate call to any of their _ProtocolXxx_ functions.

There's also an implicit happens-before relation where the calls to the *ProtocolXxx* callbacks happens-before the protocol driver handle is written to the destination place.
### How can we model ownership of the Protocol Binding Context?

The only things we need the protocol binding context for is:
- Passing data to (and from without async) `NdisprotBindAdapter` and `NdisprotOpenAdapterComplete`
- Passing data to (and from without async) `NdisprotUnbindAdapter` and `NdisprotCloseAdapterComplete`
- Storing user-defined context data

An issue we have right now is that if we store the binding handle in the binding context, then we can't access it for `ndisprotDoRequest`.

We could just \*makes ur wait into a queue\*, though that's new code™. Doing it that way also means that we can decouple the WDF side from the NDIS side. Unfortunately this has some problems:
- For requests originating from the NDIS side, we'd have to make a WDF request, which requires access to an IO target, which seems to only be used for inter-device communication
- WDF requests are completion-based, which would need to be adapted to the readiness-based API of async Rust (this is something we'd need to do anyway).

There's also the option of making our own OID request messages and relate them to WDF requests somehow, though that does mean we'd have to manually handle cancellation.
## Using Critical Regions

Critical regions should be used while acquiring a lock so that the operations inside the critical region aren't interrupted by a normal kernel APC (but still get interrupted by special Kernel APCs). There are executive resources which automatically wrap acquire & release inside a critical region (including spinlocks, mutexes), but otherwise acquiring a resource must also manually be enclosed by `KeEnterCriticalRegion` and `KeLeaveCriticalRegion`.

[OSR Article](https://www.osr.com/nt-insider/2015-issue3/critical-regions/) on using Critical Regions.

## Safely creating threads & interactions with `DropLock`

[`PsCreateSystemThread`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-pscreatesystemthread) creates a separate thread of execution to do work that's not on the main thread. However, one issue with it is that the lifetime of the thread is not bounded by the lifetime of the driver, which the kernel detects and subsequently results in a bugcheck (stop code 0xCE / `DRIVER_UNLOADED_WITHOUT_CANCELLING_PENDING_OPERATIONS`). [`IoCreateSystemThread`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-iocreatesystemthread) instead acquires a reference to the driver or device object passed in, and releases it on worker thread exit. See [this article](https://www.codemachine.com/articles/safe_kernel_thread_creation_api.html) for more info.

Since the primary reason for making `DropLock` was because it was unknown whether the main driver thread would always outlive worker threads made with the WDM APIs, we can leverage relying on maintaining the ref-count properly and the safe thread creation API to ensure exclusivity when dropping the context space.
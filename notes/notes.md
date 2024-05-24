# Note taking for implementation and other reasons

Topic groups are separated by h2 level headers, and can be modified at any time

## Accessibility of driver globals

A device needs to know what driver it's a part of, and we can get a device
from an IoQueue, FileObject, ChildList, IoTarget, DmaTransaction, Interrupt,
WmiInstance and WmiProvider (we only care about the first 2 for ndisprot).

Note that that accessing a driver from a device means that (driver) context access
needs to be fallible (see below).

A protocol driver in some situations needs to access the driver globals but does not have a binding context present to get the driver handle from, so we'd need to use `WdfGetDriver` (with the safety invariant that the global driver object outlives the protocol driver) to get access to the driver globals.

## Keep Calm and Don't Panic

Probably want to avoid any sort of panicing behaviour (e.g. `Option::unwrap`)
so that we can recover from anything. For example, if we somehow get the context
space of a driver during context space init, it should be a faillible operation
since by that point the context space isn't initialized yet.

## Unloading a driver

For a non-miniport driver, `FxStubDriverUnload` calls [`FxDriver::Unload`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/core/fxdriver.cpp#L167) , which first calls the `DriverUnload` callback specified during driver creation.

In `FxDriver::Unload` there's a call to `DeleteObject`, which calls `EvtCleanup` and `EvtDestroy` in the usual way. This potentially means that we can use `EvtDestroy` as the place to destroy the driver context, since by the definition of `EvtDestroy` callback we have exclusive access to the space. Note that also the entire driver framework object hierarchy gets destroyed inside of the WDM `Unload` callback, which is relevant in the case of [`NdisDeregisterProtocolDriver`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ndis/nf-ndis-ndisderegisterprotocoldriver) since it's typically called from the `Unload` callback (but doesn't necessarily mean that it needs to be called inside of WDF's `DriverUnload` callback). There's also [`NdisUnbindAdapter`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/ndis/nf-ndis-ndisunbindadapter) which can be used to guarantee that all of the bound adapters are unbound before we get to deregistering the protocol driver (e.g. if we represent bound adapters using WDF objects which are parented to the protocol driver).

## Can't Use `lld-link` out of the box

`lld-link` lets through the `.cfg00` and `.retplne` sections. `.retplne` is particularly problematic because it has characteristics 0, resulting in an inaccessible page and causing a bug-check (MEMORY_MANAGEMENT (1a) \[ 0x3000, \<VA\>, \<PTE\>, \<NTSTATUS\> \]).
- Apparently `.cfg00` is indeed dropped sometime between LLVM 16 and LLVM 18, but `.retplne` still isn't currently.

Even once that's worked around (e.g. by merging `.retplne` into `.rdata`), all of the non-pageable sections (i.e. any section not `PAGE`) are missing the not-pageable flag, so they could be paged out at any time causing spurious crashes.

Technically we could use this <https://learn.microsoft.com/en-us/cpp/build/reference/section-specify-section-attributes?view=msvc-170>, but it's easier to just stick with msvc link.exe for now.

This also means we miss out on LTO, so our wrapper functions don't get inlined automatically.

---

After a bit of investigating, it is possible to use `lld-link`, though it does require manually specifying the attributes of sections. `lld-link` and getting rid of the `.retplne` section (though it could also just have been specified as readable). `lld-link` also doesn't accept the correct way to specify a non-paged section (`/SECTION:.text,!P`), which is something to keep in mind.

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

# Does `ProtocolBindAdapterEx` have exclusive access to the binding context?

Per the remarks on `ProtocolBindAdapterEx`, these callbacks cannot happen during `ProtocolBindAdapterEx` until specific points:
- `ProtocolOidRequestComplete` and `ProtocolDirectOidRequestComplete` since OID requests cannot be sent until the open operation is complete.
- `ProtocolReceiveNetBufferLists` since the packet filter must be set up, which requires an OID request.
- `ProtocolSendNetBufferListsComplete` since after the operation is complete, the binding is in the paused state.
- `ProtocolUnbindAdapterEx` can only be called after the binding state transitions to "Paused", which can only happen after a successful `ProtocolBindAdapterEx` callback.
where the completion of the open operation refers to when `NdisOpenAdapterEx` returns `STATUS_SUCCESS`, or `ProtocolOpenAdapterCompleteEx` is called if `NdisOpenAdapterEx` returned `STATUS_PENDING`.

Callbacks that have/have not been verified to be called during the execution of `ProtocolBindAdapterEx`:

- [x] `ProtocolBindAdapterEx`
- [ ] `ProtocolOpenAdapterCompleteEx`
- [x] `ProtocolUnbindAdapterEx`
- [ ] `ProtocolCloseAdapterCompleteEx`
- [x] `ProtocolOidRequestComplete`
- [x] `ProtocolDirectOidRequestComplete`
- [x] `ProtocolReceiveNetBufferLists`
- [x] `ProtocolSendNetBufferListsComplete`
- [ ] `ProtocolStatusEx`
- [x] `ProtocolUninstall`
- [ ] `ProtocolNetPnPEvent`


## Using Critical Regions

Critical regions should be used while acquiring a lock so that the operations inside the critical region aren't interrupted by a normal kernel APC (but still get interrupted by special Kernel APCs). There are executive resources which automatically wrap acquire & release inside a critical region (including spinlocks, mutexes), but otherwise acquiring a resource must also manually be enclosed by `KeEnterCriticalRegion` and `KeLeaveCriticalRegion`.

[OSR Article](https://www.osr.com/nt-insider/2015-issue3/critical-regions/) on using Critical Regions.

## Safely creating threads & interactions with `DropLock`

[`PsCreateSystemThread`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-pscreatesystemthread) creates a separate thread of execution to do work that's not on the main thread. However, one issue with it is that the lifetime of the thread is not bounded by the lifetime of the driver, which the kernel detects and subsequently results in a bugcheck (stop code 0xCE / `DRIVER_UNLOADED_WITHOUT_CANCELLING_PENDING_OPERATIONS`). [`IoCreateSystemThread`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-iocreatesystemthread) instead acquires a reference to the driver or device object passed in, and releases it on worker thread exit. See [this article](https://www.codemachine.com/articles/safe_kernel_thread_creation_api.html) for more info.

Since the primary reason for making `DropLock` was because it was unknown whether the main driver thread would always outlive worker threads made with the WDM APIs, we can leverage relying on maintaining the ref-count properly and the safe thread creation API to ensure exclusivity when dropping the context space.

## Removing `ContextGuard`

Ideally would want to remove `ContextGuard` as it's essentially a refcount on a refcounted object, so would be nice to just encode the object context area liveness guarantee to that.

Protecting against improper parenting:
```
A <- B <- C
|         ^
+---------+ uses the handle from creating the object (has no refcount adjustment)
```
Problem is that WDF deletes from the bottom up (to ensure that the parent is present), whereas `Drop` goes top-down. Unfortunately, trying to delete an already fully deleted object results in a UAF :(. Luckily we can turn this from UB into a deadlock via bumping the refcount on C.

Protecting against bad refcounts:
- `WdfObjectReference` & `WdfObjectDereference` can't delete an object by itself, needs `WdfObjectDelete`.
- Refcount adjustment shouldn't be exposed to users of the library, and should be considered a bug in the library.
- Wrapping raw handles don't adjust refcount since raw handles are derived from existing handles
	- Should only be used for WDF <-> bindings communication as we lose the ability to track the handle type (owned vs ref vs wrapped)
	- Q: Should we independently store another refcount so that dropping WDF objects have uniform behavior?

Protecting against other threads created by `PsCreateSystemThread`:
- Just another case of protecting against bad refcounts
- WdfDriver is really the only special case for threads since `WdfGetDriver` gets a driver handle via global data, whereas everything else would need to be passed through the context parameter.
## Dropping the driver context area

Need exclusive access to the driver context area to soundly drop it.
`EvtDestroy` guarantees exclusive access to the context area (no other live references exist at that point).
Unfortunately by that point WDF has already destroyed any object owned by the driver and has a refcount of 1, so trying to manually delete the objects results in an invalid memory access.

How fix:
- Bump refcount and delete
	- (See "Unloading a driver") normal `DeleteObject` gets called, subject to all the normal refcounting rules (i.e. gets deleted when the refcount gets to one?)
	- Deleting beforehand means that we can't add children to an object (see [here](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobject.cpp#L867-L874))
		- Yields a status code of `STATUS_DELETE_PENDING`
- Just bump refcount & unbump on drop?
	- Like previous option, but allows adding children to an object
	- Requires doing delete (if necessary) before unbumping refcount
	- Also kinda allows inverting the deletion direction (mostly top-down removal instead of always bottom-up removal)
	- Disadvantage of dropping the originating owning handle results in preventing additional child objects
- Push drop requirement to unload
	- Unload callback happens before we have guaranteed exclusive access
		- Current solution uses a separate guard to track live context area references, could weaken it to just live references so that we can drop the guard? Unload would then be required to wait until all live references are dead so could block unload thread if `Unload` is called at `PASSIVE_LEVEL` (could be done by using a `KEVENT`).
- drop-like trait to allow calling `EvtCallback`?
	-  Needs to be derived manually and for every possible type that could be in an object context area
	- Alternatively would be easier with some sort of *introspection* API to automatically do the cleanup callback processing without requiring derives
	- Auto traits would work well for this too, but it's on track to be a perma-unstable feature (see the [`auto_traits` tracking issue](https://github.com/rust-lang/rust/issues/13231))
	- Specialization would work too, but is stuck on unsoundness of specializing over lifetimes

Q: Possible for:
- hold &handle
- access context area
- handle gets dropped
implies that handle got dropped (requires exclusive access), so would be UB from the place where the handle was dropped.

hmmm, driver context area having a handle to an object which requires access to driver context area on drop?
> means that dropping the driver context area drops the last reference to the handle, calling the corresponding `EvtDestroy` for that object context area, and depending on how the driver context area is referenced
> - by a handle stored in the area: never possible, since there's a refcount loop
> - by `WdfGetDriver`: driver context area gets marked as being dropped, so trying to access it again causes a panic

Q: When exactly is `Unload` called?
A: When the last refcount to `DRIVER_OBJECT` is dropped.

From vague thinking, it doesn't seem like the refcount of `WDFDRIVER` isn't tied to the refcount of the real `DRIVER_OBJECT`, which is adjusted when [`IoCreateSystemThread`](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-iocreatesystemthread) is called, so we'd need to make an additional thread creation wrapper API that ties the `WDFDRIVER` refcount to the `DRIVER_OBJECT`.

There's also the little hiccup of NDIS and `NdisDeregisterProtooclDriver`, where NDIS worker threads can still end up accessing `WDFDRIVER` while we're still in the unload callback, which is unavoidable as `NdisDeregisterProtooclDriver` is meant to be called in the unload callback.
## Unreferencing & Deleting Objects

As of WDF X.33:
`WdfObjectDereferenceActual` -> [`FxObject::_DereferenceActual`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/inc/private/common/fxobject.hpp#L710) -> [`FxObject::ReleaseOverride`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/inc/private/common/fxobject.hpp#L905) -> [`FxObject::Release`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/inc/private/common/fxobject.hpp#L866) -> [`FxObject::FinalRelease`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobject.cpp#L207) if refcount reached 0 after decrement

`WdfObjectDelete` -> [`FxObject::DeleteObject`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L45) and then to either
- (Not disposed) [`FxObject::DeleteWorkerAndUnlock`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L931) and then to either
	- (Defer disposal) `QueueDeferredDisposedLocked`
	- (Not deferred immediately) [`FxObject::DisposeChildrenWorker`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L1033) -> [`FxObject::DeletedAndDisposedWorkerLocked`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L1230) if not deferred
- (Disposed and delete not pending) [`FxObject::DeletedAndDisposedWorkerLocked`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L1230) -> [`FxObject::DestroyChildren`](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/inc/private/common/fxobject.hpp#L477) 

Note: Deletion can break the child - parent relation (see [here](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobjectstatemachine.cpp#L113-L116))

## Be wary of making handle wrappers pointer-sized

Bottom 3 bits are guaranteed to be used for flags (see [here](https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/inc/private/common/fxobject.hpp#L55-L63)), so if we care about future proofing (which we do), we can't safely use the bottom 3 bits for packing in custom data. If there's a platform which allows using the top n bits for addressing, we can't use it there either (unless the Windows kernel has a guarantee of where it allocates heap objects in terms of the top n bits).

Could represent refcounted version of a handle by a `WdfRef<T>` wrapper type? Depends on if default behaviour is to sometimes `WdfObjectDelete` or always to `WdfObjectDereferece`.

Interestingly on x64 there's a free bit that can be used due to allocations being aligned to 16 byte boundaries.

## Debugging Tips

- Pool leakage? Look at `!wdfkd.wdfpoolusage` to find more details on what was leaked.
- [NDIS Debug Tracing](https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/enabling-ndis-debug-tracing)

## `ndis-driver-library` MDL subset

`MDL_CHAIN` == `MdlChain<'a>(LinkedList<'a, Mdl>)`.
`MDL_POINTER` == `MdlOffset<'a> { at_mdl: &'a Mdl, offset: usize }` (a `MdlOffset` is always bound to a specific `Mdl` when normalized, otherwise is relative to an `Mdl`).
`MDL_SPAN` == `MdlSpan<'a'> { offset: MdlOffset<'a'>, length: Option<usize> }`

Q: Do all  `MdlChain-AtOffset` require `MdlPointer` + length?

Does, could probably use `MdlOffset` -> `MdlSpan`:
- `MdlChainZeroBuffersAtOffset`
- `MdlChainZeroBuffersAtOffsetNonTemporal`
- `MdlChainZeroBuffersAtOffsetSecure`
- `MdlChainFillBuffersAtOffset`
- `MdlChainFillBuffersAtOffsetNonTemporal`

Does, but shared:
- `MdlCopyFlatBufferToMdlChainAtOffset` (shared with `SourceBuffer`)
- `MdlCopyFlatBufferToMdlChainAtOffsetNonTemporal` (shared with `SourceBuffer`)
- `MdlCopyMdlChainAtOffsetToFlatBuffer` (shared with `DestinationBuffer`)
- `MdlCopyMdlChainAtOffsetToFlatBufferNonTemporal` (shared with `DestinationBuffer`)
- `MdlCopyMdlChainToMdlChainAtOffset` (shared between `DestinationMdlChain` and `SourceMdlChain`)
- `MdlCopyMdlChainToMdlChainAtOffsetNonTemporal` (shared between `DestinationMdlChain` and `SourceMdlChain`)
- `MdlEqualBufferContentsAtOffset` (shared between `MdlChain1` and `MdlChain2`)

Want an api that can do `MdlOffset<'a>` -> `Spans<'a'>` so that we can inspect the ethernet header by picking the first buffer. I guess we want a `MdlSpan::map_view`? Although we'd probably want to guarantee that it's within a particular `MDL`, so maybe a separate `MdlBuffer::map_buffer`?

## To Make A Notify Object

For mux drivers, required to selectively control what the mux protocol edge can bind to, as well as install/uninstall the virtual miniport devices.

Obstacles:
 - [ ] Declaring the library & coclass
	 - Do we even need a typelib? Is that read at load time or compile time?
		 - Seems to be at compile time? So we may not need to do so (just share the GUID where needed)
		 - To do CoClass, looks like we can just use the co class uuid?
 - [ ] Embedding with Resource Files
	 - [x] RGS
		 - ATL only, so not applicable for us.
	 - [ ] DIALOG?
	 - [ ] Manifest
		 - [ ] Privilege Request
 - [x] `mux.def`???
	 - `#[no_mangle]` already adds us to the exports
 - [ ] Managing the property pages and property page input
	 - MUX sample essentially has 2 buttons (add & remove) and a "reducer" to apply the action.
	 - What would a more complex property page involve?
 - [x] COM Server?
	 - How does it find the CLSID of the notify object anyway?
		 - Via `Ndi/ClsId` (see <https://learn.microsoft.com/en-us/windows-hardware/drivers/network/adding-registry-values-for-a-notify-object>)
	 - Apparently it's relatively simple to make a class factory, since `windows-rs` handles most of the grunt work.

## Uncluttering `wdf-kmdf`'s handle representation

Currently? It's a bit of a mess.
Would probably want to have a closed set of raw handle types, since custom objects build off of the base types anyways.
- Forgot about e.g. using this as the base for other Cx wrappers.
Should also investigate in yoinking the context space code into its own module, since it's separate from actual `WdfObject`s.

Status: Done

## DVRT & `.retplne`

Overview of Spectre v2 Mitigation: <https://techcommunity.microsoft.com/t5/windows-kernel-internals-blog/mitigating-spectre-variant-2-with-retpoline-on-windows/ba-p/295618>
Info on the Retpoline components of the DVRT: <https://denuvosoftwaresolutions.github.io/DVRT/dvrt.html>

### `.retplne` contents
Note that the section is marked `IMAGE_SCN_LNK_INFO` (0x200)

```rust
#[repr(C)]
struct RetpolineData {
	magic: [u8; 12], // b"RetpolineV1\0",
	// There may be zero or more reloc grooups
	relocs: [RetpolineReloc; 0..],
}

#[repr(C)]
struct RetpolineReloc {
	// Size of the entire reloc entry
	size: u32,
	// Which section are these relocs targeting?
	section_idx: u32,
	entries: [RetpolineEntry; (size - 8) / 8],
}

#[repr(C)]
struct RetpolineEntry {
	// What kind of entry this is?
	kind: u32,
	// Byte offset in the section
	offset: u32,
}
```

where kind can be one of:
```
0x00: Reserved?
0x01: Reserved?
0x02: IAT jmp
0x03: IAT call
0x04: Indirect jmp
0x05: Indirect call
0x06: Indirect REX.w jmp?
0x07: Indirect REX.w call?
0x08: Indirect CFG jmp
0x09: Indirect CFG call
0x0A: Indirect CFG REX.w jmp?
0x0B: Indirect CFG REX.w call?
0x0C: Reserved?
0x0D: Reserved?
0x0E: Reserved?
0x0F: Reserved?
0x10: SwitchTable rax?
0x11: SwitchTable rcx
0x12: SwitchTable rdx?
0x13: SwitchTable rbx?
0x14: SwitchTable rsp?
0x15: SwitchTable rbp?
0x16: SwitchTable rsi?
0x17: SwitchTable rdi?
0x18: SwitchTable r8?
0x19: SwitchTable r9?
0x1A: SwitchTable r10?
0x1B: SwitchTable r11?
0x1C: SwitchTable r12?
0x1D: SwitchTable r13?
0x1E: SwitchTable r14?
0x1F: SwitchTable r15?
```
Note: not tested against the real fixup process, and everything is speculatively named based on the final Retpoline section generation.
## When are IRPs sent to a device after calling `WdfControlFinishInitializing`/ clearing `DO_DEVICE_INITIALIZING`?

### For Control devices
Control devices tend to be created in `DriverEntry` ([CtlDeviceFinishInitDeviceAdd]) or sometimes even `AddDevice` ([CtlDeviceFinishInitDeviceAdd]).

Regarding the former case, it seems like the driver doesn't get to do any IO processing if it returns an error status. From <https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/driverentry-return-values>:

> If a **DriverEntry** routine returns an NTSTATUS value that is not a success or informational value, such as STATUS_SUCCESS, the driver for that **DriverEntry** routine is not loaded.

This also sorta implies that we'll never get IO requests forwarded to us while we're still in `DriverEntry`, since driver initialization is an atomic operation (i.e. it either passes and successfully loads, or fails and gets unloaded).

An interesting note is that for any device object created in `DriverEntry`, `DO_DEVICE_INITIALIZING` automatically gets cleared? From <https://www.codeproject.com/articles/9504/driver-development-part-1-introduction-to-drivers>:

> The “`DO_DEVICE_INITIALIZING`” tells the I/O Manager that the device is being initialized and not to send any I/O requests to the driver. For devices created in the context of the “`DriverEntry`”, this is not needed since the I/O Manager will clear this flag once the “`DriverEntry`” is done. However, if you create a device in any function outside of the `DriverEntry`, you need to manually clear this flag for any device you create with `IoCreateDevice`. This flag is actually set by the `IoCreateDevice` function. We cleared it here just for fun even though we weren’t required to.

While it doesn't state it explicitly, there is mentions that `DO_DEVICE_INITIALIZING` must be cleared in function & filter drivers, see <https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/initializing-a-device-object>. It also explains why the `ndisprot` non-KMDF example doesn't clear `DO_DEVICE_INITIALIZING`, since it's not a function or filter device object, and thus doesn't require clearing `DO_DEVICE_INITIALIZING`.

Generally it seems like a bad idea to create control devices outside of `AddDevice` or `DriverEntry`, as they're owned by the framework and thus will only be deleted upon driver unload. If it's created during an IOCTL, that can cause a memory leak, which is bad™. Thus, we should never be in a state where we mark the device as finishing initialization and mark the queues as ready to start up \[citation needed\], but we haven't marked the context area as being uninitialized.
### For `AddDevice` devices

According to <https://learn.microsoft.com/en-ca/windows-hardware/drivers/wdf/porting-adddevice-to-evtdriverdeviceadd>:

> A framework-based driver should set up the I/O queues and create the interrupt object in the [_EvtDriverDeviceAdd_](https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add) callback, immediately after creating the device object. The framework connects the interrupt object and starts the queues at the appropriate time later, during start-device processing.

So queues only start accepting requests after [EvtDriverDeviceAdd] is finished calling, and thus after the context space is initialized.

## For other devices

WDF clears `DO_DEVICE_INITIALIZING` for PDOs, but not FDOs, CDOs, or filter DOs? For FDOs & filter DOs, they can either be cleared inside of [EvtDriverDeviceAdd], or after returning, cleared by WDF.

[CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
[EvtDriverDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
[CtlDeviceFinishInitDrEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDrEntry
[DriverEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
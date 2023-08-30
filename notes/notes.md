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
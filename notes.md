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
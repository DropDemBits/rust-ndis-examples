use crate::object::AsObjectHandle;

// FIXME: Temporary, replace with a proper handle wrapper
impl AsObjectHandle for wdf_kmdf_sys::WDFREQUEST {
    fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.cast()
    }

    fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
        self.cast()
    }
}

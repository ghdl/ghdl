use std::slice;
use crate::sintern::{Interner, StrId};

// Create an interner
#[no_mangle]
pub extern fn sintern_new_interner(cap: u32) -> Box<Interner> {
    let res = Interner::with_capacity(cap as usize);
    Box::new(res)
}

// Delete the interner
#[no_mangle]
pub extern fn sintern_delete_interner(_: Box<Interner>) {
    //  Will be droped
}

// Return a unique id
#[no_mangle]
pub extern fn sintern_get_identifier_with_len(inst: &mut Interner, name: *const u8, len: u32) -> StrId {
    unsafe { inst.intern(std::str::from_utf8_unchecked(slice::from_raw_parts(name, len as usize))) }
}

// Return a unique id if it already exists.
#[no_mangle]
pub extern fn sintern_get_identifier_no_create_with_len(inst: &mut Interner, name: *const u8, len: u32) -> StrId {
    unsafe { inst.intern_no_create(std::str::from_utf8_unchecked(slice::from_raw_parts(name, len as usize))) }
}

// Return the unique id without copying the string [name].
#[no_mangle]
pub extern fn sintern_get_identifier_static_with_len(inst: &mut Interner, name: *const u8, len: u32) -> StrId {
    unsafe { inst.intern_static(std::str::from_utf8_unchecked(slice::from_raw_parts(name, len as usize))) }
}

// Return the unique id when it is known not to already exist.
#[no_mangle]
pub extern fn sintern_get_identifier_extra_with_len(inst: &mut Interner, name: *const u8, len: u32) -> StrId {
    unsafe { inst.intern_extra(std::str::from_utf8_unchecked(slice::from_raw_parts(name, len as usize))) }
}

// Get the C string for the [id].  It is NULL terminated.
#[no_mangle]
pub extern fn sintern_get_address(inst: &Interner, id: StrId) -> *const u8 {
    inst.lookup(id).as_ptr()
}

// Get the length of the identifier (in bytes).
#[no_mangle]
pub extern fn sintern_get_length(inst: &Interner, id: StrId) -> u32 {
    inst.lookup(id).len() as u32
}

// Get the last known identifier.
#[no_mangle]
pub extern fn sintern_get_last(inst: &Interner) -> StrId {
    inst.get_last()
}

#[no_mangle]
pub extern fn sintern_get_info(inst: &mut Interner, id: StrId) -> u32 {
    inst.get_info(id)
}

#[no_mangle]
pub extern fn sintern_set_info(inst: &mut Interner, id: StrId, info: u32) {
    inst.set_info(id, info)
}

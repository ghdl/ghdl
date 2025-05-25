#![allow(dead_code)]

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq)]
pub struct String8Id(u32);

extern "C" {
    #[link_name = "str_table__create_string8"]
    fn create_string8() -> String8Id;

    #[link_name = "str_table__append_string8"]
    fn append_string8_u8(c: u8);
}

impl String8Id {
    pub const NULL : String8Id = String8Id(0);

    pub fn new() -> String8Id {
        unsafe { create_string8() }
    }

    pub fn append(self: Self, c: u8) {
        unsafe { append_string8_u8(c) }
    }
}

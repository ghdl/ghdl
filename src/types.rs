pub type SourcePtr = u32;
pub type FileChecksumId = u32;
pub type TimeStampId = u32;
pub type DateType = u32;

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
pub struct Logic32 {
    pub val: u32,
    pub zx: u32
}
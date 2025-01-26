#![allow(dead_code)]
use crate::NameId;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq)]
pub struct SourceFileEntry(u32);

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq)]
pub struct Location(u32);

extern "C" {
    #[link_name = "files_map__location_to_file"]
    fn location_to_file(loc: Location) -> SourceFileEntry;

    #[link_name = "files_map__get_file_name"]
    fn get_file_name(sfe: SourceFileEntry) -> NameId;

    #[link_name = "files_map__get_directory_name"]
    fn get_directory_name(sfe: SourceFileEntry) -> NameId;
}

impl SourceFileEntry {
    pub const NULL : SourceFileEntry = SourceFileEntry(0);

    pub fn file_name(self: Self) -> NameId {
        unsafe { get_file_name(self) }
    }

    pub fn directory_name(self: Self) -> NameId {
        unsafe { get_directory_name(self) }
    }
}

impl Location {
    pub fn to_file(self: Self) -> SourceFileEntry {
        unsafe { location_to_file(self) }
    }
}
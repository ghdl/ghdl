use crate::types::Logic32;

pub type LitId = u32;
pub type ProcId = u32;
pub type ScopeId = u32;
pub type ObjId = u32;
pub type Width = u32;
pub type Tsize = u32;

extern "C" {
    #[link_name = "verilog__bn_tables__get_logic_32"]
    fn get_logic_32(idx: u32) -> Logic32;
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq)]
pub struct BnIndex(u32);

impl BnIndex {
    pub fn logic32(self: Self, idx: u32) -> Logic32 {
        unsafe { get_logic_32(self.0 + idx) }
    }
}
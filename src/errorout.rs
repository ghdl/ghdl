use std::ops::{Index, IndexMut};

pub type Msgid = u8;
pub type Warnid = crate::errorout_def::Warnid;
pub const MSGID_FIRST_WARNID: Msgid = crate::errorout_def::MSGID_FIRST_WARNID;
pub const WARNID_USIZE: usize = crate::errorout_def::WARNID_USIZE;

impl<T> Index<Warnid> for [T; WARNID_USIZE] {
    type Output = T;
    fn index(&self, idx: Warnid) -> &Self::Output {
        &self[idx as usize]
    }
}
impl<T> IndexMut<Warnid> for [T; WARNID_USIZE] {
    fn index_mut(&mut self, idx: Warnid) -> &mut Self::Output {
        &mut self[idx as usize]
    }
}
use crate::StrInterner;
use lazy_static::lazy_static;
use parking_lot::Mutex;

lazy_static! {
    pub static ref STR_INTERNER: Mutex<StrInterner> = StrInterner::default().into();
}

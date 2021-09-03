use crate::Diagnostic;
use lazy_static::lazy_static;
use parking_lot::Mutex;

lazy_static! {
    pub static ref DIAGNOSTICS: Mutex<Vec<Diagnostic>> = Vec::default().into();
}

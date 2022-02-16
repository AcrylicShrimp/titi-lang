macro_rules! new_ty {
    ($ty:expr) => {{
        let ty = $ty;
        Ty(crate::ty::TY_INTERNER.lock().intern(ty))
    }};
}

mod expr;
mod lookup;
mod ty_interner;

pub use expr::*;
pub use lookup::*;
pub use ty_interner::*;

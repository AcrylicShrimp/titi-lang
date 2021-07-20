use str_interner::StrIdx;

pub mod str {
    pub const EMPTY: &str = "";
    pub const BOOL: &str = "bool";
    pub const BYTE: &str = "byte";
    pub const CHAR: &str = "char";
    pub const I64: &str = "i64";
    pub const U64: &str = "u64";
    pub const ISIZE: &str = "isize";
    pub const USIZE: &str = "usize";
    pub const F64: &str = "f64";
    pub const STR: &str = "str";
    pub const CPTR: &str = "cptr";
    pub const MPTR: &str = "mptr";
    pub const IF: &str = "if";
    pub const ELSE: &str = "else";
    pub const FOR: &str = "for";
    pub const IN: &str = "in";
    pub const AS: &str = "as";
}

pub const EMPTY: StrIdx = StrIdx::new_const(0);
pub const BOOL: StrIdx = StrIdx::new_const(1);
pub const BYTE: StrIdx = StrIdx::new_const(2);
pub const CHAR: StrIdx = StrIdx::new_const(3);
pub const I64: StrIdx = StrIdx::new_const(4);
pub const U64: StrIdx = StrIdx::new_const(5);
pub const ISIZE: StrIdx = StrIdx::new_const(6);
pub const USIZE: StrIdx = StrIdx::new_const(7);
pub const CPTR: StrIdx = StrIdx::new_const(8);
pub const MPTR: StrIdx = StrIdx::new_const(9);
pub const IF: StrIdx = StrIdx::new_const(10);
pub const ELSE: StrIdx = StrIdx::new_const(11);
pub const FOR: StrIdx = StrIdx::new_const(12);
pub const IN: StrIdx = StrIdx::new_const(13);
pub const AS: StrIdx = StrIdx::new_const(14);

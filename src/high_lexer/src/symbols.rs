use lazy_static::lazy_static;
use str_interner::StrIdx;

lazy_static! {
    pub static ref EMPTY: StrIdx = StrIdx::intern("");
    pub static ref BOOL: StrIdx = StrIdx::intern("bool");
    pub static ref BYTE: StrIdx = StrIdx::intern("byte");
    pub static ref CHAR: StrIdx = StrIdx::intern("char");
    pub static ref I64: StrIdx = StrIdx::intern("i64");
    pub static ref U64: StrIdx = StrIdx::intern("u64");
    pub static ref ISIZE: StrIdx = StrIdx::intern("isize");
    pub static ref USIZE: StrIdx = StrIdx::intern("usize");
    pub static ref F64: StrIdx = StrIdx::intern("f64");
    pub static ref STR: StrIdx = StrIdx::intern("str");
    pub static ref CPTR: StrIdx = StrIdx::intern("cptr");
    pub static ref MPTR: StrIdx = StrIdx::intern("mptr");
    pub static ref IF: StrIdx = StrIdx::intern("if");
    pub static ref ELSE: StrIdx = StrIdx::intern("else");
    pub static ref FOR: StrIdx = StrIdx::intern("for");
    pub static ref IN: StrIdx = StrIdx::intern("in");
    pub static ref AS: StrIdx = StrIdx::intern("as");
    pub static ref STRUCT: StrIdx = StrIdx::intern("struct");
    pub static ref PUB: StrIdx = StrIdx::intern("pub");
    pub static ref MOD: StrIdx = StrIdx::intern("mod");
}

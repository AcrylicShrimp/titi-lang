use str_interner::StrIdx;

pub enum TopLevel {
    Fn(Fn),
}

pub struct Fn {
    pub name: StrIdx,
}

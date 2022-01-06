mod function;
mod in_fn;
mod r#struct;

pub use in_fn::*;

use crate::{ResolvedContext, ResolvedModule};
use ast::*;
use function::*;
use r#struct::*;
use span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InnerStructDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionDef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionHeaderDef(pub usize);

#[derive(Debug, Clone, Hash)]
pub struct TyRef {
    pub scope: ScopeRef,
    pub ty: Ty,
}

#[derive(Debug, Clone, Hash)]
pub struct TyRefUserDef {
    pub scope: ScopeRef,
    pub ty: TyUserDef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeRef {
    pub module: ModuleDef,
    pub scope: Option<InFnScopeDef>,
}

#[derive(Default, Debug)]
pub struct GlobalContextWithoutModule {
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<GlobalFnHeader>,
}

impl GlobalContextWithoutModule {
    pub fn with_module(self, modules: Vec<ResolvedModule>) -> GlobalContext {
        GlobalContext {
            modules,
            structs: self.structs,
            inner_structs: self.inner_structs,
            fns: self.fns,
            fn_headers: self.fn_headers,
        }
    }
}

#[derive(Debug)]
pub struct GlobalContext {
    pub modules: Vec<ResolvedModule>,
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<GlobalFnHeader>,
}

#[derive(Debug)]
pub struct GlobalStruct {
    pub name: SymbolWithSpan,
    pub fields: Vec<GlobalStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalStructField {
    pub vis: Option<Vis>,
    pub name: SymbolWithSpan,
    pub kind: GlobalStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum GlobalStructFieldKind {
    Plain(TyRef),
    Struct(InnerStructDef),
}

#[derive(Debug)]
pub struct GlobalInnerStruct {
    pub fields: Vec<GlobalInnerStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalInnerStructField {
    pub name: SymbolWithSpan,
    pub kind: GlobalStructFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFn {
    pub header: FunctionHeaderDef,
    pub ctx: InFnContext,
    pub scope: InFnScopeDef,
    pub body: InFnBlockDef,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnHeader {
    pub name: SymbolWithSpan,
    pub params: Vec<GlobalFnParam>,
    pub return_ty: Option<TyRef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GlobalFnParam {
    pub name: SymbolWithSpan,
    pub ty: TyRef,
    pub span: Span,
}

pub fn make_global(mut resolved: ResolvedContext) -> GlobalContext {
    let mut structs = resolved.structs.into_iter().map(Some).collect::<Vec<_>>();
    let mut fns = resolved.fns.into_iter().map(Some).collect::<Vec<_>>();
    let mut fn_headers = resolved
        .fn_headers
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();

    let mut context = GlobalContextWithoutModule::default();

    for module in &mut resolved.modules {
        for r#struct in &mut module.structs {
            r#struct.def = make_global_struct(
                &mut context,
                ScopeRef {
                    module: module.def,
                    scope: None,
                },
                structs[r#struct.def.0].take().unwrap().1,
            );
        }

        for r#fn in &mut module.fns {
            r#fn.def = make_global_fn(
                &mut context,
                ScopeRef {
                    module: module.def,
                    scope: None,
                },
                fns[r#fn.def.0].take().unwrap().1,
            );
        }

        for header in &mut module.fn_headers {
            header.def = make_global_fn_header(
                &mut context,
                ScopeRef {
                    module: module.def,
                    scope: None,
                },
                fn_headers[header.def.0].take().unwrap().1,
            );
        }
    }

    context.with_module(resolved.modules)
}

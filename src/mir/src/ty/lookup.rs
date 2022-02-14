use crate::ty::{deduce_expr, Ty};
use crate::{
    MirContext, MirFunctionContext, MirFunctionDef, MirFunctionHeaderDef, MirLetDef, MirLetKind,
    MirStructDef, MirTy, MirTyFn, MirTyKind,
};
use ast::{ExternKind, SubTy, TopLevelItemPrefixKind, TyKind, TyUserDef, VisKind};
use high_lexer::Symbol;
use hir::{
    GlobalContext, InFnContext, InFnExprDef, InFnScopeDef, InFnScopeKind, ModuleDef, ScopeRef,
    TyRef,
};

use super::BinaryOpTyMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lookup {
    Primitive(Ty),
    Struct(MirStructDef),
    Fn(MirFunctionDef),
    FnHeader(MirFunctionHeaderDef),
    Local(MirLetDef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupError {
    NoModuleInUse,
    NotFound,
    NotPub,
}

pub type LookupResult = Result<Lookup, LookupError>;
pub type ResolveResult = Result<Ty, LookupError>;

pub fn lookup_let(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    id: Symbol,
) -> LookupResult {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Let(def) if ctx.lets[def.0].name.symbol == id => {
                return Ok(Lookup::Local(MirLetDef(def.0)));
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    Err(LookupError::NotFound)
}

pub fn lookup_local(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    id: Symbol,
    struct_only: bool,
) -> LookupResult {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Struct(def) if global_ctx.structs[def.0].name.symbol == id => {
                return Ok(Lookup::Struct(MirStructDef(def.0)));
            }
            InFnScopeKind::Fn(def) => {
                if struct_only
                    && global_ctx.fn_headers[global_ctx.fns[def.0].header.0]
                        .name
                        .symbol
                        == id
                {
                    return Ok(Lookup::Fn(MirFunctionDef(def.0)));
                }
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    Err(LookupError::NotFound)
}

pub fn lookup_module(
    global_ctx: &GlobalContext,
    module: ModuleDef,
    id: Symbol,
    is_local: bool,
    struct_only: bool,
) -> LookupResult {
    if let Some(index) = global_ctx.modules[module.0]
        .structs
        .iter()
        .position(|r#struct| r#struct.name.symbol == id)
    {
        let r#struct = &global_ctx.modules[module.0].structs[index];

        if !is_local {
            if let Some(prefix) = &r#struct.prefix {
                match prefix.kind {
                    TopLevelItemPrefixKind::Extern(r#extern) => match r#extern.kind {
                        ExternKind::Extern => {}
                        _ => return Err(LookupError::NotPub),
                    },
                    TopLevelItemPrefixKind::Vis(vis) => match vis.kind {
                        VisKind::Pub => {}
                        _ => return Err(LookupError::NotPub),
                    },
                }
            }
        }

        Ok(Lookup::Struct(MirStructDef(r#struct.def.0)))
    } else if struct_only {
        if let Some(index) = global_ctx.modules[module.0]
            .fns
            .iter()
            .position(|r#fn| r#fn.name.symbol == id)
        {
            let r#fn = &global_ctx.modules[module.0].fns[index];

            if !is_local {
                if let Some(prefix) = &r#fn.prefix {
                    match prefix.kind {
                        TopLevelItemPrefixKind::Extern(r#extern) => match r#extern.kind {
                            ExternKind::Extern => {}
                            _ => return Err(LookupError::NotPub),
                        },
                        TopLevelItemPrefixKind::Vis(vis) => match vis.kind {
                            VisKind::Pub => {}
                            _ => return Err(LookupError::NotPub),
                        },
                    }
                }
            }

            Ok(Lookup::Fn(MirFunctionDef(r#fn.def.0)))
        } else if let Some(index) = global_ctx.modules[module.0]
            .fn_headers
            .iter()
            .position(|ty| ty.name.symbol == id)
        {
            let fn_header = &global_ctx.modules[module.0].fn_headers[index];

            // We can skip the visibility check here because the fn headers are always public currently.

            Ok(Lookup::FnHeader(MirFunctionHeaderDef(r#fn_header.def.0)))
        } else {
            Err(LookupError::NotFound)
        }
    } else {
        Err(LookupError::NotFound)
    }
}

pub fn lookup_full(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    user_def: &TyUserDef,
    struct_only: bool,
) -> LookupResult {
    if let Some(external_module) = user_def.module {
        return if let Some(use_index) = global_ctx.modules[scope.module.0]
            .uses
            .iter()
            .position(|r#use| r#use.name.symbol == external_module.symbol)
        {
            lookup_module(
                global_ctx,
                global_ctx.modules[scope.module.0].uses[use_index].def,
                user_def.id.symbol,
                false,
                struct_only,
            )
        } else {
            Err(LookupError::NoModuleInUse)
        };
    }

    if let Some(scope_def) = scope.scope {
        if let Some(ctx) = ctx {
            let local = lookup_local(global_ctx, ctx, scope_def, user_def.id.symbol, struct_only);

            if local.is_ok() {
                return local;
            }
        }
    }

    lookup_module(
        global_ctx,
        scope.module,
        user_def.id.symbol,
        true,
        struct_only,
    )
}

pub fn resolve_sub_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    mir_ctx: &MirContext,
    mir_function_ctx: &MirFunctionContext,
    binary_op_ty_map: &BinaryOpTyMap,
    scope: ScopeRef,
    ty: &SubTy,
    struct_only: bool,
) -> ResolveResult {
    Ok(match &ty.kind {
        TyKind::Bool => new_ty! {
            MirTy {
                kind: MirTyKind::Bool,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Byte => new_ty! {
            MirTy {
                kind: MirTyKind::Byte,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Char => new_ty! {
            MirTy {
                kind: MirTyKind::Char,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::I64 => new_ty! {
            MirTy {
                kind: MirTyKind::I64,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::U64 => new_ty! {
            MirTy {
                kind: MirTyKind::U64,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Isize => new_ty! {
            MirTy {
                kind: MirTyKind::Isize,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Usize => new_ty! {
            MirTy {
                kind: MirTyKind::Usize,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::F64 => new_ty! {
            MirTy {
                kind: MirTyKind::F64,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Str => new_ty! {
            MirTy {
                kind: MirTyKind::Str,
                source_kind: None,
                is_ref: false,
            }
        },
        TyKind::Ptr(inner_ty) => {
            let ty = resolve_sub_ty(
                global_ctx,
                ctx,
                mir_ctx,
                mir_function_ctx,
                binary_op_ty_map,
                scope,
                inner_ty,
                struct_only,
            )?;
            new_ty! {
                MirTy {
                    kind: MirTyKind::Ptr(ty),
                    source_kind: None,
                    is_ref: false,
                }
            }
        }
        TyKind::UserDef(user_def) => {
            let lookup = lookup_full(global_ctx, ctx, scope, user_def, struct_only)?;

            match lookup {
                Lookup::Primitive(ty) => ty,
                Lookup::Struct(def) => new_ty! {
                    MirTy {
                        kind: MirTyKind::Struct(def),
                        source_kind: None,
                        is_ref: false,
                    }
                },
                Lookup::Fn(def) => {
                    let header = &mir_ctx.fn_headers[mir_ctx.fns[def.0].header.0];
                    new_ty! {
                        MirTy {
                            kind: MirTyKind::Fn(MirTyFn {
                                params: header.params.iter().map(|param| param.ty.clone()).collect(),
                                return_ty: header.return_ty.clone(),
                            }),
                            source_kind: None,
                            is_ref: false,
                        }
                    }
                }
                Lookup::FnHeader(def) => {
                    let header = &mir_ctx.fn_headers[def.0];
                    new_ty! {
                        MirTy {
                            kind: MirTyKind::Fn(MirTyFn {
                                params: header.params.iter().map(|param| param.ty.clone()).collect(),
                                return_ty: header.return_ty.clone(),
                            }),
                            source_kind: None,
                            is_ref: false,
                        }
                    }
                }
                Lookup::Local(_) => unreachable!(),
            }
        }
    })
}

pub fn resolve_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    mir_ctx: &MirContext,
    mir_function_ctx: &MirFunctionContext,
    binary_op_ty_map: &BinaryOpTyMap,
    ty: &TyRef,
    struct_only: bool,
) -> ResolveResult {
    let mir_ty = resolve_sub_ty(
        global_ctx,
        ctx,
        mir_ctx,
        mir_function_ctx,
        binary_op_ty_map,
        ty.scope,
        &ty.ty.sub_ty,
        struct_only,
    )?
    .as_ty();

    Ok(new_ty!(MirTy {
        kind: mir_ty.kind.clone(),
        source_kind: mir_ty.source_kind,
        is_ref: ty.ty.is_ref
    }))
}

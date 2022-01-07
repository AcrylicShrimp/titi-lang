use crate::{
    MirContext, MirFunctionDef, MirFunctionHeaderDef, MirStructDef, MirTy, MirTyDef, MirTyFn,
    MirTyKind, MirTyRefKind,
};
use ast::{ExternKind, SubTy, TopLevelItemPrefixKind, TyKind, TyRefKind, TyUserDef, VisKind};
use high_lexer::Symbol;
use hir::{
    GlobalContext, InFnContext, InFnLetKind, InFnScopeDef, InFnScopeKind, ModuleDef, ScopeRef,
    TyRef,
};

/// Lookup failure reasons.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupError {
    /// The lookup failed because the referring module was not mentioned in the use list.
    NoModuleInUse,
    /// The lookup failed because the symbol was not found.
    TyNotFound,
    /// The lookup failed because the visibility of the symbol is not pub.
    NotPub,
}

pub fn resolve_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    mir_ctx: &mut MirContext,
    ty: &TyRef,
) -> Result<MirTyDef, LookupError> {
    Ok(match &ty.ty.sub_ty.kind {
        TyKind::Bool => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Bool,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Byte => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Byte,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Char => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Char,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::I64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::I64,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::U64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::U64,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Isize => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Isize,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Usize => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Usize,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::F64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::F64,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Str => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Str,
            ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                TyRefKind::Cref => MirTyRefKind::Cref,
                TyRefKind::Mref => MirTyRefKind::Mref,
            }),
        }),
        TyKind::Cptr(inner_ty) => {
            let def = resolve_sub_ty(global_ctx, ctx, scope, mir_ctx, inner_ty)?;
            mir_ctx.push_ty(MirTy {
                temporary: false,
                kind: MirTyKind::Cptr(def),
                ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                    TyRefKind::Cref => MirTyRefKind::Cref,
                    TyRefKind::Mref => MirTyRefKind::Mref,
                }),
            })
        }
        TyKind::Mptr(inner_ty) => {
            let def = resolve_sub_ty(global_ctx, ctx, scope, mir_ctx, inner_ty)?;
            mir_ctx.push_ty(MirTy {
                temporary: false,
                kind: MirTyKind::Cptr(def),
                ref_kind: ty.ty.ref_kind.as_ref().map(|ref_kind| match ref_kind {
                    TyRefKind::Cref => MirTyRefKind::Cref,
                    TyRefKind::Mref => MirTyRefKind::Mref,
                }),
            })
        }
        TyKind::UserDef(user_def) => lookup_full(global_ctx, ctx, scope, mir_ctx, user_def)?,
    })
}

pub fn resolve_sub_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    mir_ctx: &mut MirContext,
    ty: &SubTy,
) -> Result<MirTyDef, LookupError> {
    Ok(match &ty.kind {
        TyKind::Bool => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Bool,
            ref_kind: None,
        }),
        TyKind::Byte => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Byte,
            ref_kind: None,
        }),
        TyKind::Char => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Char,
            ref_kind: None,
        }),
        TyKind::I64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::I64,
            ref_kind: None,
        }),
        TyKind::U64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::U64,
            ref_kind: None,
        }),
        TyKind::Isize => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Isize,
            ref_kind: None,
        }),
        TyKind::Usize => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Usize,
            ref_kind: None,
        }),
        TyKind::F64 => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::F64,
            ref_kind: None,
        }),
        TyKind::Str => mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Str,
            ref_kind: None,
        }),
        TyKind::Cptr(inner_ty) => {
            let def = resolve_sub_ty(global_ctx, ctx, scope, mir_ctx, inner_ty)?;
            mir_ctx.push_ty(MirTy {
                temporary: false,
                kind: MirTyKind::Cptr(def),
                ref_kind: None,
            })
        }
        TyKind::Mptr(inner_ty) => {
            let def = resolve_sub_ty(global_ctx, ctx, scope, mir_ctx, inner_ty)?;
            mir_ctx.push_ty(MirTy {
                temporary: false,
                kind: MirTyKind::Mptr(def),
                ref_kind: None,
            })
        }
        TyKind::UserDef(user_def) => lookup_full(global_ctx, ctx, scope, mir_ctx, user_def)?,
    })
}

pub fn lookup_full(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    mir_ctx: &mut MirContext,
    user_def: &TyUserDef,
) -> Result<MirTyDef, LookupError> {
    if let Some(external_module) = user_def.module {
        if let Some(use_index) = global_ctx.modules[scope.module.0]
            .uses
            .iter()
            .position(|r#use| r#use.name.symbol == external_module.symbol)
        {
            lookup_module(
                global_ctx,
                global_ctx.modules[scope.module.0].uses[use_index].def,
                mir_ctx,
                user_def.id.symbol,
                false,
            )
        } else {
            Err(LookupError::NoModuleInUse)
        }
    } else {
        if let Some(def) = scope.scope {
            if let Some(ctx) = ctx {
                lookup_local(global_ctx, ctx, def, mir_ctx, user_def.id.symbol)
            } else {
                None
            }
        } else {
            None
        }
        .map_or_else(
            || lookup_module(global_ctx, scope.module, mir_ctx, user_def.id.symbol, true),
            |def| Ok(def),
        )
    }
}

pub fn lookup_id(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    mir_ctx: &mut MirContext,
    id: Symbol,
) -> Option<MirTyDef> {
    if let Some(def) = scope.scope {
        if let Some(ctx) = ctx {
            return lookup_local(global_ctx, ctx, def, mir_ctx, id)
                .or_else(|| lookup_module(global_ctx, scope.module, mir_ctx, id, true).ok());
        }
    }

    lookup_module(global_ctx, scope.module, mir_ctx, id, true).ok()
}

pub fn lookup_module(
    global_ctx: &GlobalContext,
    module: ModuleDef,
    mir_ctx: &mut MirContext,
    id: Symbol,
    is_local: bool,
) -> Result<MirTyDef, LookupError> {
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

        Ok(mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Struct(MirStructDef(r#struct.def.0)),
            ref_kind: None,
        }))
    } else if let Some(index) = global_ctx.modules[module.0]
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

        Ok(mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Fn({
                let header = &global_ctx.fn_headers[global_ctx.fns[r#fn.def.0].header.0];
                MirTyFn {
                    params: todo!(),
                    return_ty: match header.return_ty {
                        Some(ty) => todo!(),
                        None => todo!(),
                    },
                }
            }),
            ref_kind: None,
        }))
    } else if let Some(index) = global_ctx.modules[module.0]
        .fn_headers
        .iter()
        .position(|ty| ty.name.symbol == id)
    {
        let fn_header = &global_ctx.modules[module.0].fn_headers[index];

        // We can skip the visibility check here because the fn headers are always public currently.

        Ok(mir_ctx.push_ty(MirTy {
            temporary: false,
            kind: MirTyKind::Fn({
                let header = &global_ctx.fn_headers[fn_header.def.0];
                MirTyFn {
                    params: todo!(),
                    return_ty: match header.return_ty {
                        Some(ty) => todo!(),
                        None => todo!(),
                    },
                }
            }),
            ref_kind: None,
        }))
    } else {
        Err(LookupError::TyNotFound)
    }
}

pub fn lookup_local(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    mir_ctx: &mut MirContext,
    id: Symbol,
) -> Option<MirTyDef> {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Struct(r#struct) if global_ctx.structs[r#struct.0].name.symbol == id => {
                return Some(mir_ctx.push_ty(MirTy {
                    temporary: false,
                    kind: MirTyKind::Struct(MirStructDef(r#struct.0)),
                    ref_kind: None,
                }));
            }
            InFnScopeKind::Fn(r#fn) => {
                if global_ctx.fn_headers[global_ctx.fns[r#fn.0].header.0]
                    .name
                    .symbol
                    == id
                {
                    return Some(mir_ctx.push_ty(MirTy {
                        temporary: false,
                        kind: MirTyKind::Fn({
                            let header = &global_ctx.fn_headers[global_ctx.fns[r#fn.0].header.0];
                            MirTyFn {
                                params: todo!(),
                                return_ty: match header.return_ty {
                                    Some(ty) => todo!(),
                                    None => todo!(),
                                },
                            }
                        }),
                        ref_kind: None,
                    }));
                }
            }
            InFnScopeKind::Let(r#let) if ctx.lets[r#let.0].name.symbol == id => {
                match &ctx.lets[r#let.0].kind {
                    InFnLetKind::Ty(ty) => {
                        return resolve_ty(
                            global_ctx,
                            Some(ctx),
                            ScopeRef {
                                module: ctx.module,
                                scope,
                            },
                            mir_ctx,
                            ty,
                        )
                        .ok();
                    }
                    InFnLetKind::Expr(_) => todo!(),
                    InFnLetKind::TyExpr(ty, _) => {
                        let ty = resolve_ty(
                            global_ctx,
                            Some(ctx),
                            ScopeRef {
                                module: ctx.module,
                                scope,
                            },
                            mir_ctx,
                            ty,
                        )
                        .ok();

                        todo!();
                    }
                }

                // return Some(mir_ctx.push_ty(MirTy {
                //     kind: MirTyKind::,
                // 	ref_kind: MirTyRefKind::Mref,
                // }));
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    None
}

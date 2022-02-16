use crate::ty::{BinaryOpTyMap, Ty};
use crate::{MirTy, MirTyFn, MirTyKind};
use ast::{ExternKind, SubTy, TopLevelItemPrefixKind, TyKind, TyUserDef, VisKind};
use high_lexer::Symbol;
use hir::{
    FunctionDef, FunctionHeaderDef, GlobalContext, InFnContext, InFnLetDef, InFnScopeDef,
    InFnScopeKind, ModuleDef, ScopeRef, StructDef, TyRef,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lookup {
    Primitive(Ty),
    Struct(StructDef),
    Fn(FunctionDef),
    FnHeader(FunctionHeaderDef),
    Local(InFnLetDef),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupError {
    NoModuleInUse,
    NotFound,
    NotPub,
}

pub type LookupResult = Result<Lookup, LookupError>;
pub type ResolveResult = Result<Ty, LookupError>;

pub fn lookup_local_id(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    id: Symbol,
) -> LookupResult {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Fn(def) => {
                if global_ctx.fn_headers[global_ctx.fns[def.0].header.0]
                    .name
                    .symbol
                    == id
                {
                    return Ok(Lookup::Fn(def));
                }
            }
            InFnScopeKind::Let(def) if ctx.lets[def.0].name.symbol == id => {
                return Ok(Lookup::Local(def));
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    Err(LookupError::NotFound)
}

pub fn lookup_module_id(
    global_ctx: &GlobalContext,
    module: ModuleDef,
    id: Symbol,
    is_local: bool,
) -> LookupResult {
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

        Ok(Lookup::Fn(r#fn.def))
    } else if let Some(index) = global_ctx.modules[module.0]
        .fn_headers
        .iter()
        .position(|ty| ty.name.symbol == id)
    {
        let fn_header = &global_ctx.modules[module.0].fn_headers[index];

        // We can skip the visibility check here because the fn headers are always public currently.

        Ok(Lookup::FnHeader(r#fn_header.def))
    } else {
        Err(LookupError::NotFound)
    }
}

pub fn lookup_full_id(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    user_def: &TyUserDef,
) -> LookupResult {
    if let Some(external_module) = user_def.module {
        return if let Some(use_index) = global_ctx.modules[scope.module.0]
            .uses
            .iter()
            .position(|r#use| r#use.name.symbol == external_module.symbol)
        {
            lookup_module_id(
                global_ctx,
                global_ctx.modules[scope.module.0].uses[use_index].def,
                user_def.id.symbol,
                false,
            )
        } else {
            Err(LookupError::NoModuleInUse)
        };
    }

    if let Some(scope_def) = scope.scope {
        if let Some(ctx) = ctx {
            let local = lookup_local_id(global_ctx, ctx, scope_def, user_def.id.symbol);

            if local.is_ok() {
                return local;
            }
        }
    }

    lookup_module_id(global_ctx, scope.module, user_def.id.symbol, true)
}

pub fn lookup_local_id_ty(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    id: Symbol,
) -> LookupResult {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Struct(def) if global_ctx.structs[def.0].name.symbol == id => {
                return Ok(Lookup::Struct(def));
            }
            InFnScopeKind::Fn(def) => {
                if global_ctx.fn_headers[global_ctx.fns[def.0].header.0]
                    .name
                    .symbol
                    == id
                {
                    return Ok(Lookup::Fn(def));
                }
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    Err(LookupError::NotFound)
}

pub fn lookup_module_id_ty(
    global_ctx: &GlobalContext,
    module: ModuleDef,
    id: Symbol,
    is_local: bool,
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

        Ok(Lookup::Struct(r#struct.def))
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

        Ok(Lookup::Fn(r#fn.def))
    } else if let Some(index) = global_ctx.modules[module.0]
        .fn_headers
        .iter()
        .position(|ty| ty.name.symbol == id)
    {
        let fn_header = &global_ctx.modules[module.0].fn_headers[index];

        // We can skip the visibility check here because the fn headers are always public currently.

        Ok(Lookup::FnHeader(r#fn_header.def))
    } else {
        Err(LookupError::NotFound)
    }
}

pub fn lookup_full_id_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    user_def: &TyUserDef,
) -> LookupResult {
    if let Some(external_module) = user_def.module {
        return if let Some(use_index) = global_ctx.modules[scope.module.0]
            .uses
            .iter()
            .position(|r#use| r#use.name.symbol == external_module.symbol)
        {
            lookup_module_id_ty(
                global_ctx,
                global_ctx.modules[scope.module.0].uses[use_index].def,
                user_def.id.symbol,
                false,
            )
        } else {
            Err(LookupError::NoModuleInUse)
        };
    }

    if let Some(scope_def) = scope.scope {
        if let Some(ctx) = ctx {
            let local = lookup_local_id_ty(global_ctx, ctx, scope_def, user_def.id.symbol);

            if local.is_ok() {
                return local;
            }
        }
    }

    lookup_module_id_ty(global_ctx, scope.module, user_def.id.symbol, true)
}

pub fn lookup_local_ty(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    scope: InFnScopeDef,
    id: Symbol,
) -> LookupResult {
    let mut scope = Some(scope);

    while let Some(def) = scope {
        match ctx.scopes[def.0].kind {
            InFnScopeKind::Struct(def) if global_ctx.structs[def.0].name.symbol == id => {
                return Ok(Lookup::Struct(def));
            }
            _ => {
                scope = ctx.scopes[def.0].parent;
            }
        }
    }

    Err(LookupError::NotFound)
}

pub fn lookup_module_ty(
    global_ctx: &GlobalContext,
    module: ModuleDef,
    id: Symbol,
    is_local: bool,
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

        Ok(Lookup::Struct(r#struct.def))
    } else {
        Err(LookupError::NotFound)
    }
}

pub fn lookup_full_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    scope: ScopeRef,
    user_def: &TyUserDef,
) -> LookupResult {
    if let Some(external_module) = user_def.module {
        return if let Some(use_index) = global_ctx.modules[scope.module.0]
            .uses
            .iter()
            .position(|r#use| r#use.name.symbol == external_module.symbol)
        {
            lookup_module_ty(
                global_ctx,
                global_ctx.modules[scope.module.0].uses[use_index].def,
                user_def.id.symbol,
                false,
            )
        } else {
            Err(LookupError::NoModuleInUse)
        };
    }

    if let Some(scope_def) = scope.scope {
        if let Some(ctx) = ctx {
            let local = lookup_local_ty(global_ctx, ctx, scope_def, user_def.id.symbol);

            if local.is_ok() {
                return local;
            }
        }
    }

    lookup_module_ty(global_ctx, scope.module, user_def.id.symbol, true)
}

pub fn resolve_sub_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    binary_op_ty_map: &BinaryOpTyMap,
    scope: ScopeRef,
    ty: &SubTy,
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
            let ty = resolve_sub_ty(global_ctx, ctx, binary_op_ty_map, scope, inner_ty)?;

            new_ty! {
                MirTy {
                    kind: MirTyKind::Ptr(ty),
                    source_kind: None,
                    is_ref: false,
                }
            }
        }
        TyKind::UserDef(user_def) => {
            let lookup = lookup_full_ty(global_ctx, ctx, scope, user_def)?;

            match lookup {
                Lookup::Primitive(ty) => ty,
                Lookup::Struct(def) => new_ty! {
                    MirTy {
                        kind: MirTyKind::Struct(def),
                        source_kind: None,
                        is_ref: false,
                    }
                },
                // Lookup::Fn(def) => {
                //     let header = &global_ctx.fn_headers[global_ctx.fns[def.0].header.0];
                //     new_ty! {
                //         MirTy {
                //             kind: MirTyKind::Fn(MirTyFn {
                //                 params: header.params.iter().map(|param| resolve_ty(global_ctx, ctx, binary_op_ty_map, &param.ty, false).unwrap()).collect(),
                //                 return_ty: header.return_ty.as_ref().map(|ty| resolve_ty(global_ctx, ctx, binary_op_ty_map, &ty, false).unwrap()),
                //             }),
                //             source_kind: None,
                //             is_ref: false,
                //         }
                //     }
                // }
                // Lookup::FnHeader(def) => {
                //     let header = &global_ctx.fn_headers[global_ctx.fns[def.0].header.0];
                //     new_ty! {
                //         MirTy {
                //             kind: MirTyKind::Fn(MirTyFn {
                //                 params: header.params.iter().map(|param| resolve_ty(global_ctx, ctx, binary_op_ty_map, &param.ty, false).unwrap()).collect(),
                //                 return_ty: header.return_ty.as_ref().map(|ty| resolve_ty(global_ctx, ctx, binary_op_ty_map, &ty, false).unwrap()),
                //             }),
                //             source_kind: None,
                //             is_ref: false,
                //         }
                //     }
                // }
                _ => unreachable!(),
            }
        }
    })
}

pub fn resolve_ty(
    global_ctx: &GlobalContext,
    ctx: Option<&InFnContext>,
    binary_op_ty_map: &BinaryOpTyMap,
    ty: &TyRef,
) -> ResolveResult {
    let mir_ty =
        resolve_sub_ty(global_ctx, ctx, binary_op_ty_map, ty.scope, &ty.ty.sub_ty)?.as_ty();

    Ok(new_ty!(MirTy {
        kind: mir_ty.kind.clone(),
        source_kind: mir_ty.source_kind,
        is_ref: ty.ty.is_ref
    }))
}

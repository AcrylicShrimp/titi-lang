use crate::*;
use high_lexer::Symbol;

/// Lookup failure reasons.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LookupError {
    /// The lookup failed because the refering module was not mentioned in the use list.
    NoModuleInUse,
    /// The lookup failed because the symbol was not found.
    TyNotFound,
    /// The lookup failed because the visibility of the symbol is not pub.
    NotPub,
}

/// Resolves a given type. If the type is type of `UserDef`, it calls `lookup_full` to resolve it. The type is just forwarded otherwise.
///
/// # Arguments
///
/// * `modules` - The global modules.
/// * `scopes` - The global scopes.
/// * `item` - The `TyRef` to be resolved.
pub fn resolve_ty(
    modules: &[ResolvedModule],
    scopes: &[GlobalScope],
    item: TyRef,
) -> Result<ResolvedType, LookupError> {
    Ok(match item.ty.kind {
        TyKind::Bool => ResolvedType {
            kind: ResolvedTypeKind::Bool,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Byte => ResolvedType {
            kind: ResolvedTypeKind::Byte,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Char => ResolvedType {
            kind: ResolvedTypeKind::Char,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::I64 => ResolvedType {
            kind: ResolvedTypeKind::I64,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::U64 => ResolvedType {
            kind: ResolvedTypeKind::U64,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Isize => ResolvedType {
            kind: ResolvedTypeKind::Isize,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Usize => ResolvedType {
            kind: ResolvedTypeKind::Usize,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::F64 => ResolvedType {
            kind: ResolvedTypeKind::F64,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Str => ResolvedType {
            kind: ResolvedTypeKind::Str,
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Cptr(inner) => ResolvedType {
            kind: ResolvedTypeKind::Cptr(
                resolve_ty(
                    modules,
                    scopes,
                    TyRef {
                        scope: item.scope,
                        ty: *inner,
                    },
                )?
                .into(),
            ),
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::Mptr(inner) => ResolvedType {
            kind: ResolvedTypeKind::Mptr(
                resolve_ty(
                    modules,
                    scopes,
                    TyRef {
                        scope: item.scope,
                        ty: *inner,
                    },
                )?
                .into(),
            ),
            addressable: item.ty.ref_kind.is_some(),
            assignable: item
                .ty
                .ref_kind
                .as_ref()
                .map_or(false, |ref_kind| match ref_kind {
                    TyRefKind::Cref => false,
                    TyRefKind::Mref => true,
                }),
            ref_kind: item.ty.ref_kind,
            span: item.ty.span,
        },
        TyKind::UserDef(user_def) => lookup_full(modules, scopes, item.scope, user_def)?,
    })
}

/// Lookup a symbol in the given module.
///
/// # Arguments
///
/// * `modules` - The global modules.
/// * `scopes` - The global scopes.
/// * `scope` - The current scope as `ScopeDef`.
/// * `user_def` - The `TyUserDef` to look up.
///
/// # Return value
///
/// The type of the `user_def` as `ResolvedType`.
///
/// # Errors
///
/// - `NoModuleInUse`: The module was not mentioned in the use list.
/// - `TyNotFound`: The symbol was not found.
/// - `NotPub`: The visibility of the symbol is not `pub`.
pub fn lookup_full(
    modules: &[ResolvedModule],
    scopes: &[GlobalScope],
    scope: ScopeRef,
    user_def: TyUserDef,
) -> Result<ResolvedType, LookupError> {
    let module = &modules[match scope {
        ScopeRef::Module(def) => def,
        ScopeRef::Scope(def) => scopes[def].module,
    }];

    if let Some(external_module) = user_def.module {
        if let Some(use_index) = module
            .uses
            .iter()
            .position(|item| item.name.symbol == external_module.symbol)
        {
            lookup_module(&modules[use_index], user_def.id.symbol, false).map(|kind| ResolvedType {
                kind,
                addressable: false,
                assignable: false,
                ref_kind: None,
                span: external_module.span.to(user_def.id.span),
            })
        } else {
            Err(LookupError::NoModuleInUse)
        }
    } else {
        if let ScopeRef::Scope(def) = scope {
            lookup_local(scopes, def, user_def.id.symbol)
        } else {
            None
        }
        .map_or_else(
            || {
                lookup_module(module, user_def.id.symbol, true).map(|kind| ResolvedType {
                    kind,
                    addressable: false,
                    assignable: false,
                    ref_kind: None,
                    span: user_def.id.span,
                })
            },
            |kind| {
                let assignable = if let ResolvedTypeKind::Let(..) = &kind {
                    true
                } else {
                    false
                };

                Ok(ResolvedType {
                    kind,
                    addressable: assignable,
                    assignable,
                    ref_kind: None,
                    span: user_def.id.span,
                })
            },
        )
    }
}

/// Lookup a symbol in the given module. It searches the local scope first (if any) and then the global of the module.
///
/// # Arguments
///
/// * `modules` - The global modules.
/// * `scopes` - The global scopes.
/// * `scope` - The current scope as `ScopeDef`.
/// * `id` - The symbol to look up.
///
/// # Return value
/// The type of the symbol as `ResolvedType`.
pub fn lookup_id(
    modules: &[ResolvedModule],
    scopes: &[GlobalScope],
    scope: ScopeRef,
    id: Symbol,
) -> Option<ResolvedTypeKind> {
    match scope {
        ScopeRef::Module(def) => lookup_module(&modules[def], id, true).ok(),
        ScopeRef::Scope(def) => lookup_local(scopes, def, id)
            .or_else(|| lookup_module(&modules[scopes[def].module], id, true).ok()),
    }
}

/// Lookup a global in the given module.
///
/// # Arguments
///
/// * `module` - The module to look in.
/// * `id` - The symbol of the global to lookup.
/// * `is_local` - Whether the module is the local module. If this is `true`, visibility checks are skipped.
///
/// # Return value
///
/// The type of the global as `ResolvedTypeKind`.
///
/// # Errors
///
/// * If the global is not found, `LookupError::TyNotFound` is returned.
/// * If the `is_local` is `false` and the visibility of the global is not `pub`, `LookupError::NotPub` is returned.
pub fn lookup_module(
    module: &ResolvedModule,
    id: Symbol,
    is_local: bool,
) -> Result<ResolvedTypeKind, LookupError> {
    if let Some(index) = module
        .structs
        .iter()
        .position(|item| item.name.symbol == id)
    {
        let item = &module.structs[index];

        if !is_local && item.prefix.is_none() {
            Err(LookupError::NotPub)
        } else {
            Ok(ResolvedTypeKind::Struct(item.def))
        }
    } else if let Some(index) = module.fns.iter().position(|item| item.name.symbol == id) {
        let item = &module.fns[index];

        if !is_local && item.prefix.is_none() {
            Err(LookupError::NotPub)
        } else {
            Ok(ResolvedTypeKind::Fn(item.def))
        }
    } else if let Some(index) = module
        .fn_headers
        .iter()
        .position(|item| item.name.symbol == id)
    {
        let item = &module.fn_headers[index];

        // We can skip the visibility check here because the fn headers are always public currently.

        Ok(ResolvedTypeKind::FnHeader(item.def))
    } else {
        Err(LookupError::TyNotFound)
    }
}

/// Lookup a local in the current scope.
///
/// # Arguments
///
/// * `scopes` - The global scopes.
/// * `scope` - The current scope.
/// * `id` - The symbol of the local to lookup.
///
/// # Return Value
///
/// The type of the local as `ResolvedTypeKind` if it exists, otherwise `None`.
pub fn lookup_local(scopes: &[GlobalScope], scope: usize, id: Symbol) -> Option<ResolvedTypeKind> {
    let mut ty = None;
    let mut scope = Some(scope);

    while let Some(def) = scope {
        if let Some(kind) = &scopes[def].kind {
            match kind {
                ScopeKind::Struct(item) => {
                    if item.name.symbol == id {
                        ty = Some(ResolvedTypeKind::Struct(item.def));
                        break;
                    }
                }
                ScopeKind::Fn(item) => {
                    if item.name.symbol == id {
                        ty = Some(ResolvedTypeKind::Fn(item.def));
                        break;
                    }
                }
                ScopeKind::Let(item) => {
                    if item.name.symbol == id {
                        ty = Some(ResolvedTypeKind::Let(item.def));
                        break;
                    }
                }
                ScopeKind::For(..) => {}
            }
        }

        scope = scopes[def].parent;
    }

    ty
}

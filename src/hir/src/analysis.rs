use crate::*;
use diagnostic::{Diagnostic, Level, MultiSpan};
use high_lexer::Symbol;
use parser::parse;
use span::SourcePath;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

pub fn analyze_module<P: AsRef<Path>>(source: P) -> SymbolTable {
    let mut source_map = SourceMap::new();

    let source_path = source
        .as_ref()
        .canonicalize()
        .expect("unable to resolve entry module path");
    let content = read_to_string(&source_path).expect("unable to read entry module");
    let source = source_map.add_source(
        source_path
            .to_str()
            .expect("path should be utf8")
            .to_owned(),
        SourcePath::Real(source_path.clone()),
        content,
    );

    let mut structs = vec![];
    let mut fns = vec![];
    let mut fn_headers = vec![];

    let mut modules_map = HashMap::new();
    modules_map.insert(source_path, ModuleDef(0));

    let mut modules = vec![parse(source).unwrap()];
    let mut next_modules = vec![];

    let mut resolved_modules: Vec<ResolvedModule> = vec![];

    loop {
        let module_len = modules.len();

        for module in modules {
            let mut resolved = ResolvedModule {
                def: ModuleDef( resolved_modules.len()),
                source: module.source.clone(),
                uses: vec![],
                structs: vec![],
                fns: vec![],
                fn_headers: vec![],
            };

            for top_level in module.top_levels {
                match top_level.kind {
                    TopLevelKind::Use(item) => {
                        let path = use_to_path(&module.source, &item)
                            .expect("unable to resolve used module path");

                        for previous_item in &resolved.uses {
                            if let SourcePath::Real(previous_path) =
                                resolved_modules[previous_item.def.0].source.path()
                            {
                                if path.eq(previous_path) {
                                    Diagnostic::push_new(Diagnostic::new(
                                        Level::Warning,
                                        format!("the module has already been mentioned by a previous one"),
                                        MultiSpan::with_spans(vec![
                                            (
                                                format!("this use statement is mentioning the module"),
                                                Some(previous_item.span),
                                            ),
                                            (format!("...so it will be ignored"), Some(item.span)),
                                        ]),
                                    ));
                                }
                            }
                        }

                        if !modules_map.contains_key(&path) {
                            // This module is new one. Load it and add to unresolved_modules.
                            resolved.uses.push(ResolvedModuleUse {
                                name: item.segments.last().cloned().unwrap(),
                                def: ModuleDef (module_len + next_modules.len()),
                                span: item.span,
                            });
                            modules_map.insert(path.clone(), ModuleDef (module_len + next_modules.len()));
                            next_modules.push({
                                let content = read_to_string(&path).expect(&format!(
                                    "unable to read module at: {}",
                                    path.to_string_lossy()
                                ));
                                parse(source_map.add_source(
                                    path.to_str().expect("path should be utf8").to_owned(),
                                    SourcePath::Real(path),
                                    content,
                                ))
                                .unwrap()
                            });
                        }
                    }
                    TopLevelKind::Struct(item) => {
                        for previous_item in &resolved.structs {
                            if item.item.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this struct has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this struct"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fns {
                            if item.item.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this struct"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fn_headers {
                            if item.item.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function prototype has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this struct"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        resolved.structs.push(ResolvedModuleStruct {
                            name: item.item.name,
                            prefix: item.prefix,
                            def: StructDef( structs.len()),
                            span: item.span,
                        });
                        structs.push((resolved.def, item.item));
                    }
                    TopLevelKind::Fn(item) => {
                        for previous_item in &resolved.structs {
                            if item.item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this struct has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fns {
                            if item.item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fn_headers {
                            if item.item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function prototype has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        resolved.fns.push(ResolvedModuleFn {
                            name: item.item.header.name,
                            prefix: item.prefix,
                            def: FunctionDef( fns.len()),
                            span: item.span,
                        });
                        fns.push((resolved.def, item.item));
                    }
                    TopLevelKind::FnHeader(item) => {
                        for previous_item in &resolved.structs {
                            if item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this struct has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function prototype"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fns {
                            if item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function prototype"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        for previous_item in &resolved.fn_headers {
                            if item.header.name.symbol == previous_item.name.symbol {
                                Diagnostic::push_new(Diagnostic::new(
                                    Level::Error,
                                    format!("duplicate global name"),
                                    MultiSpan::with_spans(vec![
                                        (
                                            format!("this function prototype has the same name"),
                                            Some(previous_item.span),
                                        ),
                                        (format!("...as this function prototype"), Some(item.span)),
                                    ]),
                                ));
                            }
                        }

                        resolved.fn_headers.push(ResolvedModuleFnHeader {
                            name: item.header.name,
                            def: FunctionHeaderDef( fn_headers.len()),
                            span: item.span,
                        });
                        fn_headers.push((resolved.def, item.header));
                    }
                }
            }

            resolved_modules.push(resolved);
        }

        if next_modules.is_empty() {
            break;
        }

        modules = next_modules;
        next_modules = vec![];
    }

    let mut global_types = vec![];
    let mut global_exprs = vec![];
    let mut global_lets = vec![];
    let mut global_scopes = vec![];
    let mut global_inner_structs = vec![];
    let mut global_structs = structs
        .into_iter()
        .map(|(module, item)| {
            to_scope_struct(
                ScopeRef::Module(module),
                &mut global_types,
                &mut global_inner_structs,
                item,
            )
        })
        .collect::<Vec<_>>();
    let mut global_fns = vec![];
    let global_fn_headers = fn_headers
        .into_iter()
        .map(|(module, item)| to_scope_fn_header(ScopeRef::Module(module), &mut global_types, item))
        .collect::<Vec<_>>();

    for (module, item) in fns {
        to_scope_fn(
            ScopeRef::Module(module),
            &mut global_types,
            &mut global_exprs,
            &mut global_lets,
            &mut global_scopes,
            &mut global_structs,
            &mut global_inner_structs,
            &mut global_fns,
            item,
        );
    }

    let resolved_types = global_types
        .into_iter()
        .map(|item| {
            resolve_ty(
                &resolved_modules,
                &global_scopes,
                item,
            ).unwrap()
        })
        .collect::<Vec<_>>();

    println!("resolved types: {:#?}", resolved_types);
    println!("global exprs: {:#?}", global_exprs);
    println!("global lets: {:#?}", global_lets);
    println!("global scopes: {:#?}", global_scopes);
    println!("global structs: {:#?}", global_structs);
    println!("global fns: {:#?}", global_fns);
    println!("global fn headers: {:#?}", global_fn_headers);

    todo!()
}

fn use_to_path(source: &Source, item: &Use) -> Option<PathBuf> {
    if let SourcePath::Real(path) = &source.path() {
        path.parent()
            .map(|path| {
                path.join(
                    item.segments
                        .iter()
                        .map(|segment| segment.symbol.as_str())
                        .collect::<PathBuf>()
                        .with_extension("tt"),
                )
            })
            .and_then(|path| path.canonicalize().ok())
    } else {
        None
    }
}

fn to_global_expr(
    scope: ScopeDef,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    item: Expr,
) -> ExprDef {
    match item.kind {
        ExprKind::Assign(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Assign(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)
        }
        ExprKind::AssignAdd(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignAdd(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)
        }
        ExprKind::AssignSub(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignSub(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)
        }
        ExprKind::AssignMul(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignMul(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)
        }
        ExprKind::AssignDiv(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignDiv(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)
        }
        ExprKind::AssignMod(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignMod(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignShl(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignShl(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignShr(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignShr(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignBitOr(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignBitOr(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignBitAnd(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignBitAnd(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignBitXor(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignBitXor(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::AssignBitNot(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::AssignBitNot(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Rng(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Rng(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::RngInclusive(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::RngInclusive(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Eq(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Eq(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Ne(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Ne(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Lt(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Lt(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Gt(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Gt(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Le(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Le(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Ge(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Ge(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Neg(lhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Neg(lhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Add(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Add(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Sub(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Sub(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Mul(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Mul(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Div(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Div(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Mod(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Mod(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Shl(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Shl(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Shr(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Shr(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::BitOr(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::BitOr(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::BitAnd(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::BitAnd(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::BitXor(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::BitXor(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::BitNot(lhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::BitNot(lhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::LogOr(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::LogOr(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::LogAnd(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::LogAnd(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::LogNot(lhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::BitNot(lhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Cast(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = {
                let def = types.len();
                types.push(TyRef {
                    scope: ScopeRef::Scope(scope),
                    ty: rhs,
                });
                TyDef( def)
            };
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Cast(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Object(lhs) => {
            let lhs = to_global_object(scope, types, exprs, lhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Object(lhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Call(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = rhs
                .into_iter()
                .map(|expr| to_global_expr(scope, types, exprs, expr))
                .collect();
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Call(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Index(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let rhs = to_global_expr(scope, types, exprs, *rhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Index(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Member(lhs, rhs) => {
            let lhs = to_global_expr(scope, types, exprs, *lhs);
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Member(lhs, rhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Deref(_) => todo!(),
        ExprKind::Id(lhs) => {
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Id(lhs),
                span: item.span,
            });
            ExprDef(def)       }
        ExprKind::Literal(lhs) => {
            let def = exprs.len();
            exprs.push(GlobalExpr {
                scope,
                kind: GlobalExprKind::Literal(lhs),
                span: item.span,
            });
            ExprDef(def)       }
    }
}

fn to_global_object(
    scope: ScopeDef,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    item: Object,
) -> GlobalObject {
    GlobalObject {
        ty: {
            let def = types.len();
            types.push(TyRef {
                scope: ScopeRef::Scope(scope),
                ty: Ty {
                    span: item.span,
                    kind: TyKind::UserDef(item.ty),
                    ref_kind: None,
                },
            });
            def
        },
        fields: item
            .fields
            .into_iter()
            .map(|field| GlobalObjectField {
                name: field.name,
                kind: match field.kind {
                    ObjectFieldKind::Expr(expr) => {
                        GlobalObjectFieldKind::Expr(to_global_expr(scope, types, exprs, expr))
                    }
                    ObjectFieldKind::InnerObject(inner) => GlobalObjectFieldKind::InnerObject(
                        to_global_inner_object(scope, types, exprs, inner),
                    ),
                },
                span: field.span,
            })
            .collect(),
        span: item.span,
    }
}

fn to_global_inner_object(
    scope: usize,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    item: InnerObject,
) -> GlobalInnerObject {
    GlobalInnerObject {
        fields: item
            .fields
            .into_iter()
            .map(|field| GlobalObjectField {
                name: field.name,
                kind: match field.kind {
                    ObjectFieldKind::Expr(expr) => {
                        GlobalObjectFieldKind::Expr(to_global_expr(scope, types, exprs, expr))
                    }
                    ObjectFieldKind::InnerObject(inner) => GlobalObjectFieldKind::InnerObject(
                        to_global_inner_object(scope, types, exprs, inner),
                    ),
                },
                span: field.span,
            })
            .collect(),
        span: item.span,
    }
}

fn to_scope_struct(
    scope: ScopeRef,
    types: &mut Vec<TyRef>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    item: Struct,
) -> GlobalStruct {
    GlobalStruct {
        name: item.name,
        fields: item
            .fields
            .into_iter()
            .map(|field| 
                // TODO: Emit a diagnostic if the field name is already used
                GlobalStructField {
                vis: field.vis,
                name: field.name,
                kind: match field.kind {
                    StructFieldKind::Plain(ty) => {
                        let index = types.len();
                        types.push(TyRef { scope, ty });
                        GlobalStructFieldKind::Plain(index)
                    }
                    StructFieldKind::Struct(item) => GlobalStructFieldKind::Struct(
                        to_scope_inner_struct(scope, types, inner_structs, item),
                    ),
                },
                span: field.span,
                })
            .collect(),
        span: item.span,
    }
}

fn to_scope_inner_struct(
    scope: ScopeRef,
    types: &mut Vec<TyRef>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    item: InnerStruct,
) -> usize {
    let item = GlobalInnerStruct {
        fields: item
            .fields
            .into_iter()
            .map(|field| GlobalInnerStructField {
                name: field.name,
                kind: match field.kind {
                    StructFieldKind::Plain(ty) => {
                        let index = types.len();
                        types.push(TyRef { scope, ty });
                        GlobalStructFieldKind::Plain(index)
                    }
                    StructFieldKind::Struct(item) => GlobalStructFieldKind::Struct(
                        to_scope_inner_struct(scope, types, inner_structs, item),
                    ),
                },
                span: field.span,
            })
            .collect(),
        span: item.span,
    };
    let index = inner_structs.len();
    inner_structs.push(item);
    index
}

fn to_scope_fn(
    scope: ScopeRef,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    lets: &mut Vec<GlobalLet>,
    scopes: &mut Vec<GlobalScope>,
    structs: &mut Vec<GlobalStruct>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    fns: &mut Vec<GlobalFn>,
    item: Fn,
) -> usize {
    let inner_scope = scopes.len();
    scopes.push(match scope {
        ScopeRef::Module(def) => GlobalScope {
            module: def,
            kind: None,
            parent: None,
        },
        ScopeRef::Scope(def) => GlobalScope {
            module: scopes[def].module,
            kind: None,
            parent: Some(def),
        },
    });

    let item = GlobalFn {
        header: to_scope_fn_header(scope, types, item.header),
        body: to_scope_block(
            inner_scope,
            types,
            exprs,
            lets,
            scopes,
            structs,
            inner_structs,
            fns,
            item.body,
        ),
        span: item.span,
    };

    let def = fns.len();
    scopes[inner_scope].kind = Some(ScopeKind::Fn(ScopeFn {
        name: item.header.name,
        def,
    }));
    fns.push(item);

    def
}

fn to_scope_fn_header(scope: ScopeRef, types: &mut Vec<TyRef>, item: FnHeader) -> GlobalFnHeader {
    GlobalFnHeader {
        name: item.name,
        params: item
            .params
            .into_iter()
            .map(|param| {
                let index = types.len();
                types.push(TyRef {
                    scope,
                    ty: param.ty,
                });
                GlobalFnParam {
                    name: param.name,
                    ty: index,
                    span: param.span,
                }
            })
            .collect(),
        return_ty: if let Some(ty) = item.return_ty {
            let index = types.len();
            types.push(TyRef { scope, ty });
            Some(index)
        } else {
            None
        },
        span: item.span,
    }
}

fn to_scope_block(
    scope: usize,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    lets: &mut Vec<GlobalLet>,
    scopes: &mut Vec<GlobalScope>,
    structs: &mut Vec<GlobalStruct>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    fns: &mut Vec<GlobalFn>,
    item: Block,
) -> ScopeBlock {
    let mut scope = scope;
    let module = scopes[scope].module;

    ScopeBlock {
        stmts: item
            .stmts
            .into_iter()
            .map(|stmt| ScopeStmt {
                kind: match stmt.kind {
                    StmtKind::Struct(item) => {
                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            module,
                            kind: Some(ScopeKind::Struct(ScopeStruct {
                                name: item.name,
                                def: structs.len(),
                            })),
                            parent: Some(scope),
                        });
                        structs.push(to_scope_struct(
                            ScopeRef::Scope(new_scope),
                            types,
                            inner_structs,
                            item,
                        ));

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Fn(item) => {
                        let name = item.header.name;
                        let def = to_scope_fn(
                            ScopeRef::Scope(scope),
                            types,
                            exprs,
                            lets,
                            scopes,
                            structs,
                            inner_structs,
                            fns,
                            item,
                        );

                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            module,
                            kind: Some(ScopeKind::Fn(ScopeFn { name, def })),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Let(item) => {
                        let def = lets.len();
                        lets.push(GlobalLet {
                            name: item.name,
                            kind: match item.kind {
                                LetKind::Ty(ty) => {
                                    let def = types.len();
                                    types.push(TyRef {
                                        scope: ScopeRef::Scope(scope),
                                        ty,
                                    });
                                    GlobalLetKind::Ty(def)
                                }
                                LetKind::Expr(expr) => {
                                    GlobalLetKind::Expr(to_global_expr(scope, types, exprs, expr))
                                }
                                LetKind::TyExpr(ty, expr) => {
                                    let def = types.len();
                                    types.push(TyRef {
                                        scope: ScopeRef::Scope(scope),
                                        ty,
                                    });
                                    GlobalLetKind::TyExpr(
                                        def,
                                        to_global_expr(scope, types, exprs, expr),
                                    )
                                }
                            },
                            span: item.span,
                        });

                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            module,
                            kind: Some(ScopeKind::Let(ScopeLet {
                                name: item.name,
                                def,
                            })),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::If(item) => ScopeStmtKind::If(to_scope_if(
                        scope,
                        types,
                        exprs,
                        lets,
                        scopes,
                        structs,
                        inner_structs,
                        fns,
                        item,
                    )),
                    StmtKind::For(item) => {
                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            module,
                            kind: None,
                            parent: Some(scope),
                        });

                        let item = ScopeFor {
                            kind: match item.kind {
                                ForKind::Loop => ScopeForKind::Loop,
                                ForKind::While(expr) => {
                                    ScopeForKind::While(to_global_expr(scope, types, exprs, expr))
                                }
                                ForKind::ForIn(id, expr) => ScopeForKind::ForIn(
                                    id,
                                    to_global_expr(scope, types, exprs, expr),
                                ),
                            },
                            body: to_scope_block(
                                new_scope,
                                types,
                                exprs,
                                lets,
                                scopes,
                                structs,
                                inner_structs,
                                fns,
                                item.body,
                            ),
                            span: item.span,
                        };
                        scopes[new_scope].kind = Some(ScopeKind::For(item));

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Block(item) => ScopeStmtKind::Block(to_scope_block(
                        scope,
                        types,
                        exprs,
                        lets,
                        scopes,
                        structs,
                        inner_structs,
                        fns,
                        item,
                    )),
                    StmtKind::Break(item) => ScopeStmtKind::Break(item),
                    StmtKind::Continue(item) => ScopeStmtKind::Continue(item),
                    StmtKind::Return(item) => ScopeStmtKind::Return(item),
                    StmtKind::Expr(item) => {
                        ScopeStmtKind::Expr(to_global_expr(scope, types, exprs, item))
                    }
                },
                span: stmt.span,
            })
            .collect(),
        span: item.span,
    }
}

fn to_scope_if(
    scope: usize,
    types: &mut Vec<TyRef>,
    exprs: &mut Vec<GlobalExpr>,
    lets: &mut Vec<GlobalLet>,
    scopes: &mut Vec<GlobalScope>,
    structs: &mut Vec<GlobalStruct>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    fns: &mut Vec<GlobalFn>,
    item: If,
) -> ScopeIf {
    ScopeIf {
        cond: item.cond,
        then_body: to_scope_block(
            scope,
            types,
            exprs,
            lets,
            scopes,
            structs,
            inner_structs,
            fns,
            item.then_body,
        ),
        else_kind: item.else_kind.map(|item| {
            Box::new(match *item {
                ElseKind::Else(item) => ScopeElseKind::Else(to_scope_block(
                    scope,
                    types,
                    exprs,
                    lets,
                    scopes,
                    structs,
                    inner_structs,
                    fns,
                    item,
                )),
                ElseKind::ElseIf(item) => ScopeElseKind::ElseIf(to_scope_if(
                    scope,
                    types,
                    exprs,
                    lets,
                    scopes,
                    structs,
                    inner_structs,
                    fns,
                    item,
                )),
            })
        }),
        span: item.span,
    }
}

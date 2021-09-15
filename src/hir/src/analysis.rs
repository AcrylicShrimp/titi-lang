use crate::*;
use diagnostic::{Diagnostic, Level, MultiSpan};
use parser::parse;
use span::SourcePath;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

struct TyRef {
    scope: Option<usize>,
    ty: Ty,
}

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

    // TODO: Embed the module index.
    let mut structs = vec![];
    let mut fns = vec![];
    let mut fn_headers = vec![];

    let mut modules_map = HashMap::new();
    modules_map.insert(source_path, 0);

    let mut modules = vec![parse(source).unwrap()];
    let mut next_modules = vec![];

    let mut resolved_modules: Vec<ResolvedModule> = vec![];

    loop {
        let module_len = modules.len();

        for module in modules {
            let mut resolved = ResolvedModule {
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
                                resolved_modules[previous_item.def].source.path()
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
                                def: module_len + next_modules.len(),
                                span: item.span,
                            });
                            modules_map.insert(path.clone(), module_len + next_modules.len());
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
                            def: structs.len(),
                            span: item.span,
                        });
                        structs.push(item.item);
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
                            def: fns.len(),
                            span: item.span,
                        });
                        fns.push(item.item);
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
                            def: fn_headers.len(),
                            span: item.span,
                        });
                        fn_headers.push(item.header);
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
    let mut global_scopes = vec![];
    let mut global_inner_structs = vec![];
    let mut global_structs = structs
        .into_iter()
        .map(|item| to_scope_struct(None, &mut global_types, &mut global_inner_structs, item))
        .collect::<Vec<_>>();
    let mut global_fns = vec![];
    let mut global_fn_headers = fn_headers
        .into_iter()
        .map(|item| to_scope_fn_header(&mut global_types, item))
        .collect::<Vec<_>>();

    for item in fns {
        let item = to_scope_fn(
            None,
            &mut global_types,
            &mut global_scopes,
            &mut global_structs,
            &mut global_inner_structs,
            &mut global_fns,
            item,
        );
        // TODO: Add the index to the module.
    }

    let mut resolved_types = global_types
        .into_iter()
        .map(|item| {
            resolve_ty(
                &global_scopes,
                &global_structs,
                &global_inner_structs,
                &global_fns,
                &global_fn_headers,
                item,
            )
        })
        .collect::<Vec<_>>();

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
                        .collect::<PathBuf>(),
                )
            })
            .and_then(|path| path.canonicalize().ok())
    } else {
        None
    }
}

fn resolve_ty(
    scopes: &[GlobalScope],
    structs: &[GlobalStruct],
    inner_structs: &[GlobalInnerStruct],
    fns: &[GlobalFn],
    fn_headers: &[GlobalFnHeader],
    item: TyRef,
) -> ResolvedType {
    ResolvedType {
        kind: match item.ty.kind {
            TyKind::Bool => ResolvedTypeKind::Bool,
            TyKind::Byte => ResolvedTypeKind::Byte,
            TyKind::Char => ResolvedTypeKind::Char,
            TyKind::I64 => ResolvedTypeKind::I64,
            TyKind::U64 => ResolvedTypeKind::U64,
            TyKind::Isize => ResolvedTypeKind::Isize,
            TyKind::Usize => ResolvedTypeKind::Usize,
            TyKind::F64 => ResolvedTypeKind::F64,
            TyKind::Str => ResolvedTypeKind::Str,
            TyKind::Cptr(inner) => ResolvedTypeKind::Cptr(Box::new(resolve_ty(
                scopes,
                structs,
                inner_structs,
                fns,
                fn_headers,
                TyRef {
                    scope: item.scope,
                    ty: *inner,
                },
            ))),
            TyKind::Mptr(inner) => ResolvedTypeKind::Cptr(Box::new(resolve_ty(
                scopes,
                structs,
                inner_structs,
                fns,
                fn_headers,
                TyRef {
                    scope: item.scope,
                    ty: *inner,
                },
            ))),
            TyKind::External(inner) => {
                if let Some(module) = inner.module {
                    // TODO: Resolve the external module's pub type.
                    todo!()
                } else {
                    let mut ty = None;
                    let mut scope = item.scope;

                    while let Some(index) = scope {
                        if let Some(kind) = &scopes[index].kind {
                            match kind {
                                ScopeKind::Struct(item) => {
                                    if item.name.symbol == inner.item.symbol {
                                        ty = Some(ResolvedTypeKind::Struct(item.def));
                                        break;
                                    }
                                }
                                ScopeKind::Fn(item) => {
                                    if item.name.symbol == inner.item.symbol {
                                        ty = Some(ResolvedTypeKind::Fn(item.def));
                                        break;
                                    }
                                }
                                ScopeKind::Let(item) => {
                                    // TODO: Resolve the type of the let statement.
                                    todo!()
                                }
                                ScopeKind::For(..) => {}
                            }
                        }

                        scope = scopes[index].parent;
                    }

                    if let Some(ty) = ty {
                        ty
                    } else {
                        // TODO: Search for the type in the global of the module.
                        todo!()
                    }
                }
            }
        },
        span: item.ty.span,
    }
}

fn to_scope_struct(
    scope: Option<usize>,
    types: &mut Vec<TyRef>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    item: Struct,
) -> GlobalStruct {
    GlobalStruct {
        name: item.name,
        fields: item
            .fields
            .into_iter()
            .map(|field| GlobalStructField {
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
    scope: Option<usize>,
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
    scope: Option<usize>,
    types: &mut Vec<TyRef>,
    scopes: &mut Vec<GlobalScope>,
    structs: &mut Vec<GlobalStruct>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    fns: &mut Vec<GlobalFn>,
    item: Fn,
) -> usize {
    let inner_scope = scopes.len();
    scopes.push(GlobalScope {
        kind: None,
        parent: scope,
    });

    let item = GlobalFn {
        header: to_scope_fn_header(types, item.header),
        body: to_scope_block(
            inner_scope,
            types,
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

fn to_scope_fn_header(types: &mut Vec<TyRef>, item: FnHeader) -> GlobalFnHeader {
    GlobalFnHeader {
        name: item.name,
        params: item
            .params
            .into_iter()
            .map(|param| {
                let index = types.len();
                types.push(TyRef {
                    scope: None,
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
            types.push(TyRef { scope: None, ty });
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
    scopes: &mut Vec<GlobalScope>,
    structs: &mut Vec<GlobalStruct>,
    inner_structs: &mut Vec<GlobalInnerStruct>,
    fns: &mut Vec<GlobalFn>,
    item: Block,
) -> ScopeBlock {
    let mut scope = scope;

    ScopeBlock {
        stmts: item
            .stmts
            .into_iter()
            .map(|stmt| ScopeStmt {
                kind: match stmt.kind {
                    StmtKind::Struct(item) => {
                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            kind: Some(ScopeKind::Struct(ScopeStruct {
                                name: item.name,
                                def: structs.len(),
                            })),
                            parent: Some(scope),
                        });
                        structs.push(to_scope_struct(Some(scope), types, inner_structs, item));

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Fn(item) => {
                        let name = item.header.name;
                        let def = to_scope_fn(
                            Some(scope),
                            types,
                            scopes,
                            structs,
                            inner_structs,
                            fns,
                            item,
                        );

                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            kind: Some(ScopeKind::Fn(ScopeFn { name, def })),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Let(item) => {
                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            kind: Some(ScopeKind::Let(ScopeLet {
                                name: item.name,
                                kind: match item.kind {
                                    LetKind::Ty(ty) => {
                                        let index = types.len();
                                        types.push(TyRef {
                                            scope: Some(scope),
                                            ty,
                                        });
                                        ScopeLetKind::Ty(index)
                                    }
                                    LetKind::Expr(expr) => ScopeLetKind::Expr(expr),
                                    LetKind::TyExpr(ty, expr) => {
                                        let index = types.len();
                                        types.push(TyRef {
                                            scope: Some(scope),
                                            ty,
                                        });
                                        ScopeLetKind::TyExpr(index, expr)
                                    }
                                },
                                span: item.span,
                            })),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::If(item) => ScopeStmtKind::If(to_scope_if(
                        scope,
                        types,
                        scopes,
                        structs,
                        inner_structs,
                        fns,
                        item,
                    )),
                    StmtKind::For(item) => {
                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            kind: None,
                            parent: Some(scope),
                        });

                        let item = ScopeFor {
                            kind: match item.kind {
                                ForKind::Loop => ScopeForKind::Loop,
                                ForKind::While(expr) => ScopeForKind::While(expr),
                                ForKind::ForIn(id, expr) => ScopeForKind::ForIn(id, expr),
                            },
                            body: to_scope_block(
                                new_scope,
                                types,
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
                        scopes,
                        structs,
                        inner_structs,
                        fns,
                        item,
                    )),
                    StmtKind::Break(item) => ScopeStmtKind::Break(item),
                    StmtKind::Continue(item) => ScopeStmtKind::Continue(item),
                    StmtKind::Return(item) => ScopeStmtKind::Return(item),
                    StmtKind::Expr(item) => ScopeStmtKind::Expr(item),
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
                    scopes,
                    structs,
                    inner_structs,
                    fns,
                    item,
                )),
                ElseKind::ElseIf(item) => ScopeElseKind::ElseIf(to_scope_if(
                    scope,
                    types,
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

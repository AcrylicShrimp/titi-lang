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

pub fn analyze_module<P: AsRef<Path>>(symbol_table: &mut SymbolTable, source: P) {
    let source_path = source
        .as_ref()
        .canonicalize()
        .expect("unable to resolve entry module path");
    let content = read_to_string(&source_path).expect("unable to read entry module");
    let source = symbol_table.source_map.add_source(
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
                                parse(symbol_table.source_map.add_source(
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

    let mut types = vec![];
    let mut inner_structs = vec![];
    let mut structs = structs
        .into_iter()
        .map(|item| to_scope_struct(None, &mut types, &mut inner_structs, item))
        .collect::<Vec<_>>();
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
        kind: ScopeKind::Block,
        parent: scope,
    });

    let new_scope = scopes.len();
    scopes.push(GlobalScope {
        kind: ScopeKind::Fn(ScopeFn {
            name: item.header.name,
            def: 0,
        }),
        parent: scope,
    });

    let item = GlobalFn {
        header: item.header,
        body: to_scope_block(new_scope, types, scopes, structs, inner_structs, fns, item),
        span: item.span,
    };
    let def = fns.len();
    fns.push(item);

    scopes[new_scope].kind = ScopeKind::Fn(ScopeFn {
        name: item.header.name,
        def,
    });

    def
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
                        let def = structs.len();
                        structs.push(to_scope_struct(Some(scope), types, inner_structs, item));

                        let new_scope = scopes.len();
                        scopes.push(GlobalScope {
                            kind: ScopeKind::Struct(ScopeStruct {
                                name: item.name,
                                def,
                            }),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Fn(item) => {
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
                            kind: ScopeKind::Fn(ScopeFn {
                                name: item.header.name,
                                def,
                            }),
                            parent: Some(scope),
                        });

                        scope = new_scope;
                        ScopeStmtKind::Scope(new_scope)
                    }
                    StmtKind::Let(_) => todo!(),
                    StmtKind::If(_) => todo!(),
                    StmtKind::For(_) => todo!(),
                    StmtKind::Block(_) => todo!(),
                    StmtKind::Break(_) => todo!(),
                    StmtKind::Continue(_) => todo!(),
                    StmtKind::Return(_) => todo!(),
                    StmtKind::Expr(_) => todo!(),
                },
                span: stmt.span,
            })
            .collect(),
        span: item.span,
    }
}

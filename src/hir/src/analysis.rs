use crate::*;
use diagnostic::{Diagnostic, Level, MultiSpan};
use parser::parse;
use span::SourcePath;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

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

struct Context {
    pub scopes: Vec<GlobalScope>,
    pub structs: Vec<GlobalStruct>,
    pub inner_structs: Vec<GlobalInnerStruct>,
    pub fns: Vec<GlobalFn>,
    pub fn_headers: Vec<FnHeader>,
}

fn to_scope_struct(types: &mut Vec<Ty>, item: Struct) -> GlobalStruct {
    
}

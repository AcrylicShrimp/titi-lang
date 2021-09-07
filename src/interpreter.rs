use ast::{Fn, Struct, TopLevelKind};
use high_lexer::{Symbol, MAIN};
use parser::parse;
use span::{Source, SourceMap, SourcePath};
use std::collections::HashMap;
use std::fs::{canonicalize, read_to_string};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Debug)]
pub struct ParsedModule {
    pub source: Arc<Source>,
    pub structs: HashMap<Symbol, Struct>,
    pub functions: HashMap<Symbol, Fn>,
}

impl ParsedModule {
    pub fn parse(source: Arc<Source>) -> (Self, Vec<PathBuf>) {
        let module = match parse(source.clone()) {
            Ok(module) => module,
            Err(err) => {
                let line_col = source.find_line_col(err.1.low());
                panic!(
                    "{:?} [where: {:?}] [source: {}]",
                    err.0,
                    line_col,
                    source.slice_line(line_col.line())
                );
            }
        };

        let mut uses = vec![];
        let mut structs = HashMap::new();
        let mut functions = HashMap::new();

        for top_level in module.top_levels {
            match top_level.kind {
                TopLevelKind::Use(u) => {
                    uses.push(
                        u.segments
                            .iter()
                            .map(|segment| segment.symbol.as_str())
                            .collect::<PathBuf>(),
                    );
                }
                TopLevelKind::Struct(s) => {
                    structs.insert(s.name.symbol, s);
                }
                TopLevelKind::Fn(f) => {
                    functions.insert(f.name.symbol, f);
                }
            }
        }

        (
            Self {
                source,
                structs,
                functions,
            },
            uses,
        )
    }
}

#[derive(Debug, Default)]
pub struct Context {
    source_map: SourceMap,
    modules: HashMap<PathBuf, ParsedModule>,
}

impl Context {
    pub fn module<P: AsRef<Path>>(&self, path: P) -> Option<&ParsedModule> {
        self.modules.get(path.as_ref())
    }

    pub fn load_module(&mut self, path: PathBuf, module_path: PathBuf) {
        if self.modules.contains_key(&path) {
            return;
        }

        let path = canonicalize(path).unwrap();
        let name = path.file_name().unwrap().to_string_lossy().into_owned();
        let content = read_to_string(&path).unwrap();
        let source = self
            .source_map
            .add_source(name, SourcePath::Real(path.clone()), content);
        let (module, uses) = ParsedModule::parse(source);

        self.modules.insert(module_path, module);

        for u in uses {
            self.load_module(path.join(u.clone()), path.join(u));
        }
    }
}

#[derive(Debug)]
pub struct Runtime<'ctx> {
    stacks: Vec<CallStack<'ctx>>,
}

impl<'ctx> Runtime<'ctx> {
    pub fn new() -> Self {
        Self { stacks: vec![] }
    }

    pub fn run(&mut self, ctx: &'ctx Context) {
        let module = ctx.module("").unwrap();
        self.exec_fn(ctx, module.functions.get(&MAIN).unwrap());
    }

    fn exec_fn(&mut self, ctx: &'ctx Context, f: &Fn) {
        println!("{:?}", f);
    }
}

#[derive(Debug)]
pub struct CallStack<'ctx> {
    pub f: &'ctx Fn,
    pub args: Vec<(Symbol, Value)>,
    pub locals: Vec<(Symbol, Value)>,
    pub stmt_index: usize,
}

impl<'ctx> CallStack<'ctx> {
    pub fn from_fn(f: &'ctx Fn, args: Vec<(Symbol, Value)>) -> Self {
        Self {
            f,
            args,
            locals: vec![],
            stmt_index: 0,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Char(char),
    I64(i64),
    U64(u64),
    Isize(isize),
    Usize(usize),
    F64(f64),
    Str(String),
    Cptr(*const Value),
    Mptr(*mut Value),
    Object(Symbol, Object),
}

#[derive(Debug)]
pub struct Object {
    pub fields: HashMap<Symbol, Value>,
}

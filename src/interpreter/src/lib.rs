mod parsed;

use parsed::*;
use std::path::{Path, PathBuf};

pub fn init(entry: PathBuf) -> Context {
    let mut ctx = Context::default();
    let source = ctx.register_source(entry.clone());
    let module = ParsedModule::parse(&mut ctx, source);

    ctx.register_module(PathBuf::from(""), module);
    resolve_module(&mut ctx, "");

    ctx
}

fn resolve_module<P: AsRef<Path>>(ctx: &mut Context, path: P) {
    for item in ctx.module(path).unwrap().uses.clone() {
        if ctx.has_module(&item) {
            continue;
        }

        let source = ctx.register_source(item.clone());
        let module = ParsedModule::parse(ctx, source);

        ctx.register_module(item.clone(), module);
        resolve_module(ctx, item);
    }
}

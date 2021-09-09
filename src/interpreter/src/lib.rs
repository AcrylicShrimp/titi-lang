mod check;
mod exec;
mod parsed;

pub use exec::*;

use parsed::*;
use std::path::{Path, PathBuf};

pub fn init(entry: PathBuf) -> Context {
    let mut ctx = Context::default();
    let source = ctx.register_source(entry.clone());
    let module = ParsedModule::parse(&mut ctx, source);

    ctx.register_module(PathBuf::from(""), module);
    resolve_module(
        &mut ctx,
        "",
        entry.canonicalize().unwrap().parent().unwrap(),
    );

    ctx
}

fn resolve_module<P0: AsRef<Path>, P1: AsRef<Path>>(
    ctx: &mut Context,
    module_path: P0,
    base_path: P1,
) {
    for item in ctx.module(module_path).unwrap().uses.clone() {
        if ctx.has_module(&item) {
            continue;
        }

        let path = base_path.as_ref().join(&item).with_extension("tt");
        let source = ctx.register_source(path.clone());
        let module = ParsedModule::parse(ctx, source);

        ctx.register_module(item.clone(), module);
        resolve_module(ctx, item, path.parent().unwrap());
    }
}

use clap::{App, Arg};
use hir::{make_global, resolve, InFnExprDef};
use mir::{deduce_expr, BinaryOpTyMap};
use std::path::PathBuf;

fn main() {
    let matches = App::new("ttc")
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about("titi-lang compiler")
        .arg(
            Arg::with_name("entry")
                .help("input file to be used as entry point")
                .index(1)
                .required(true),
        )
        .get_matches();
    let entry = matches.value_of("entry").unwrap();

    let resolved = resolve(PathBuf::from(entry));

    // println!("{:?}", resolved);

    let global = make_global(resolved);

    // println!("{:?}", global);

    let mut binary_op_map = BinaryOpTyMap::new();
    // binary_op_map.insert(BinaryOp {
    //     kind: BinaryOpKind::Add,
    //     lhs: todo!(),
    //     rhs: todo!(),
    //     ty: todo!(),
    // })

    for function in &global.fns {
        println!(
            "=========== {} ===========",
            global.fn_headers[function.header.0].name.symbol,
        );
        for (index, expr) in function.ctx.exprs.iter().enumerate() {
            println!("expr({}): {:?}", index, expr);
            println!(
                "expr ty({}): {:?}",
                index,
                deduce_expr(
                    &global,
                    &function.ctx,
                    expr.scope,
                    &binary_op_map,
                    InFnExprDef(index)
                )
            );
        }
    }

    // let ctx = init(PathBuf::from(entry));
    // let module = ctx.module("").unwrap();
    // let function = module
    //     .top_level_functions
    //     .iter()
    //     .rev()
    //     .find(|f| f.name.symbol == MAIN)
    //     .expect("no main function found");
    // let function = ctx.function(function.def);

    // let runtime = Runtime::new(&ctx);
    // runtime.call(module, function, vec![]);
}

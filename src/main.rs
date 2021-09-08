mod interpreter;

use clap::{App, Arg};
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

    let mut context = interpreter::Context::default();
    context.load_module(PathBuf::from(entry), PathBuf::from(""));

    // let mut runtime = interpreter::Runtime::new();
    // runtime.run(&context);
}

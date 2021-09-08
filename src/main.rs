use clap::{App, Arg};
use interpreter::init;
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
    let mut ctx = init(PathBuf::from(entry));

    print!("{:?}", ctx);
}

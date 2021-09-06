use diagnostic::Diagnostic;
use high_lexer::token_iter;
use parser::{parse_toplevel, Cursor, Parser};
use span::{Pos, Source, SourcePath, Span};

fn main() {
    let source = r#"
pub fn test1(a cptr str, b isize) f64 {
    break; cotinue;
    +++++a;
    {
        return 10;
    }
    123;
    ta;
    ta[321];
    call(a, b, expr[12241]);
    (a as f64 as f64 as f64)(10, 342);
    !~~~!~!~!~~!~!!!~!+--+-++----++((aaaaa))(1, 2, 3, 5);
    a | b as isize & 0x1000isize ^ 0x1110isize;
    0x1000isize ^ 0x1110isize;
    1000f64;
    a == b[0012];
    b != c;
    123 + 10;
    a = a + b / 10 % call(a, b, c[123 + 10 % 1231isize]) && a == 10isize;
}

fn test2(a str) cptr str {
    return "hello";
}
    "#;
    let source = Source::new(
        Span::new(Pos::new(0), Pos::new(source.len() as _)),
        "main.tt".to_owned(),
        SourcePath::Virtual("main.tt".to_owned()),
        source.to_owned(),
    );
    let mut parser = Parser::new(Cursor::new(token_iter(&source)));

    while parser.exists() {
        println!(
            "{:?}",
            match parse_toplevel(&mut parser) {
                Ok(t) => t,
                Err(e) => {
                    let line_col = source.find_line_col(e.1.low());

                    panic!(
                        "{:?} [where: {:?}] [source: {}]",
                        e.0,
                        line_col,
                        source.slice_line(line_col.line())
                    );
                }
            }
        );
    }

    // for token in iter {
    //     println!("{:?}", token);
    // }

    for diagnostic in Diagnostic::diagnostics().iter() {
        println!("{:?}", diagnostic);
    }
}

use diagnostic::Diagnostic;
use high_lexer::token_iter;
use span::{Pos, Source, SourcePath, Span};

fn main() {
    let source = r#"
    if a == 10 {
        let test = 0.0usize;
        test <<= 10;
        print("%s", a + 2usize);
    }
    "#;
    let source = Source::new(
        Span::new(Pos::new(0), Pos::new(source.len() as _)),
        "main.tt".to_owned(),
        SourcePath::Virtual("main.tt".to_owned()),
        source.to_owned(),
    );
    let iter = token_iter(&source);

    for token in iter {
        println!("{:?}", token);
    }

    for diagnostic in Diagnostic::diagnostics().iter() {
        println!("{:?}", diagnostic);
    }
}

use high_lexer::token_iter;
use span::{Pos, Source, SourcePath, Span};

fn main() {
    let source = r#"
    if a == 10 {
        let test = 0;
        test <<= 10;
        print('%s', a + 2);
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

    // let mut lexer = compiler::Lexer::new(
    //     r#"

    // if a == 10 {
    //     let test = 0;
    //     test <<= 10;
    //     print('%s', a + 2);
    // }

    // "#,
    // );

    // loop {
    //     let token = lexer.next();

    //     if *token.ty() == TokenType::Eof {
    //         break;
    //     }

    //     println!("{:#?}", token);
    // }

    // let ast = compiler::parse(compiler::Lexer::new(
    //     r#"

    // if a == 10 {
    //     print('%s', a + 2);
    // }

    // "#,
    // ));
    // println!("{:#?}", ast);
}

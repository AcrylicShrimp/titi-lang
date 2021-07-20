use compiler::TokenType;
use high_lexer::*;
use low_lexer::*;

mod compiler;

fn main() {
    let mut lexer = compiler::Lexer::new(
        r#"
    
    if a == 10 {
        let test = 0;
        test <<= 10;
        print('%s', a + 2);
    }
    
    "#,
    );

    loop {
        let token = lexer.next();

        if *token.ty() == TokenType::Eof {
            break;
        }

        println!("{:#?}", token);
    }

    // let ast = compiler::parse(compiler::Lexer::new(
    //     r#"

    // if a == 10 {
    //     print('%s', a + 2);
    // }

    // "#,
    // ));
    // println!("{:#?}", ast);
}

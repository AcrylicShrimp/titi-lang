mod compiler;

fn main() {
    let mut lexer = compiler::Lexer::new(
        r#"
    
    # This is a comment!

    ' ';
    '      ';
    '';
    '
    ';
    'w';
    '\\';
    '\'';
    '\n';
    '\0';

    "#,
    );
    let mut tokens = Vec::new();

    loop {
        let token = lexer.parse();
        let token_ty = token.ty();
        tokens.push(token);

        if token_ty == compiler::TokenType::Eof {
            break;
        }
    }

    println!("{:#?}", tokens);
}

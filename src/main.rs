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

    "this is a str \n literal!";

    @"
    This
    is
    a
    multi-
    line
    str
    literal!
    "@;

    "#,
    );
    let mut tokens = Vec::new();

    loop {
        let token = lexer.parse();
        tokens.push(token);

        if *tokens.last().unwrap().ty() == compiler::TokenType::Eof {
            break;
        }
    }

    println!("{:#?}", tokens);
}

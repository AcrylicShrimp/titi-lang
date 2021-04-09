mod compiler;

fn main() {
    let ast = compiler::parse(compiler::Lexer::new(
        r#"
    
    if a == 10 {
        print('%s', a + 2);
    }
    
    "#
        .to_owned(),
    ));
    println!("{:#?}", ast);
}

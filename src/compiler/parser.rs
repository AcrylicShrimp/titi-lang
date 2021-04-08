use super::lexer::*;
use super::token::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub type AST = (Box<ASTExpr>,);

pub enum ASTExpr {
    Expr(String, Option<(Vec<(Box<ASTOp>, String)>,)>),
}

pub enum ASTOp {
    Add(String),
    Sub(String),
    Mul(String),
    Div(String),
    Mod(String),
}

#[derive(Debug)]
pub enum RawAST {
    __Token(String),
    _Expr1(RawAST_Expr1),
    _Root(RawAST_Root),
    Expr(RawASTExpr),
    Op(RawASTOp),
}

#[derive(Debug)]
pub enum RawAST_Expr1 {
    Expr1ExprReduce0(Box<RawASTOp>, String), // _expr-1-expr [Repeater(2)]
    Expr1ExprReduce1(Box<RawASTOp>, String, Box<RawAST_Expr1>), // _expr-1-expr [RepeaterContinue(3)]
}

#[derive(Debug)]
pub enum RawAST_Root {
    RootReduce0(Box<RawASTExpr>), // root [NonTerminal("expr")]
}

#[derive(Debug)]
pub enum RawASTExpr {
    ExprReduce0(String),                    // expr [Terminal, OptionNone]
    ExprReduce1(String, Box<RawAST_Expr1>), // expr [Terminal, OptionSome([Repeat("_expr-1")])]
}

#[derive(Debug)]
pub enum RawASTOp {
    AddReduce0(String), // add [Terminal]
    DivReduce0(String), // div [Terminal]
    ModReduce0(String), // mod [Terminal]
    MulReduce0(String), // mul [Terminal]
    SubReduce0(String), // sub [Terminal]
}

fn build_ast_terminal(token: Token) -> RawAST {
    RawAST::__Token(token.content().clone())
}

fn build_ast_nonterminal(name: &str, child_asts: Vec<RawAST>) -> RawAST {
    match name {
        "_expr-1_expr-1-expr-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(), it.next().unwrap());
            RawAST::_Expr1(RawAST_Expr1::Expr1ExprReduce0(
                match ast.0 {
                    RawAST::Op(ast) => Box::new(ast),
                    _ => unreachable!(),
                },
                match ast.1 {
                    RawAST::__Token(token) => token,
                    _ => unreachable!(),
                },
            ))
        }
        "_expr-1_expr-1-expr-reduce-1" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(), it.next().unwrap(), it.next().unwrap());
            RawAST::_Expr1(RawAST_Expr1::Expr1ExprReduce1(
                match ast.0 {
                    RawAST::Op(ast) => Box::new(ast),
                    _ => unreachable!(),
                },
                match ast.1 {
                    RawAST::__Token(token) => token,
                    _ => unreachable!(),
                },
                match ast.2 {
                    RawAST::_Expr1(ast) => Box::new(ast),
                    _ => unreachable!(),
                },
            ))
        }
        "__rootroot-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::_Root(RawAST_Root::RootReduce0(match ast.0 {
                RawAST::Expr(ast) => Box::new(ast),
                _ => unreachable!(),
            }))
        }
        "exprexpr-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Expr(RawASTExpr::ExprReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        "exprexpr-reduce-1" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(), it.next().unwrap());
            RawAST::Expr(RawASTExpr::ExprReduce1(
                match ast.0 {
                    RawAST::__Token(token) => token,
                    _ => unreachable!(),
                },
                match ast.1 {
                    RawAST::_Expr1(ast) => Box::new(ast),
                    _ => unreachable!(),
                },
            ))
        }
        "opadd-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Op(RawASTOp::AddReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        "opdiv-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Op(RawASTOp::DivReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        "opmod-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Op(RawASTOp::ModReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        "opmul-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Op(RawASTOp::MulReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        "opsub-reduce-0" => {
            let mut it = child_asts.into_iter();
            let ast = (it.next().unwrap(),);
            RawAST::Op(RawASTOp::SubReduce0(match ast.0 {
                RawAST::__Token(token) => token,
                _ => unreachable!(),
            }))
        }
        _ => unreachable!(),
    }
}

fn raw_ast_to_ast(ast: RawAST) -> AST {
    let root_ast = match ast {
        RawAST::_Root(root_ast) => root_ast,
        _ => unreachable!(),
    };

    match root_ast {
        RawAST_Root::RootReduce0(ast) => (Box::new(raw_ast_to_ast_Expr(*ast)),),
    }
}

fn raw_ast_to_ast__Expr1(ast: RawAST_Expr1) -> Vec<(Box<RawASTOp>, String)> {
    let mut result = Vec::new();
    let mut ast = ast;

    loop {
        match ast {
            RawAST_Expr1::Expr1ExprReduce0(ast_0, ast_1) => {
                result.push((ast_0, ast_1));
                return result;
            }
            RawAST_Expr1::Expr1ExprReduce1(ast_0, ast_1, ast_2) => {
                result.push((ast_0, ast_1));
                ast = *ast_2;
            }
        }
    }
}

fn raw_ast_to_ast_Expr(ast: RawASTExpr) -> ASTExpr {
    match ast {
        RawASTExpr::ExprReduce0(ast_0) => ASTExpr::Expr(ast_0, None),
        RawASTExpr::ExprReduce1(ast_0, ast_1) => ASTExpr::Expr(
            ast_0,
            Some(({
                raw_ast_to_ast__Expr1(*ast_1)
                    .into_iter()
                    .map(|item| (Box::new(raw_ast_to_ast_Op(*item.0)), item.1))
                    .collect::<_>()
            },)),
        ),
    }
}

fn raw_ast_to_ast_Op(ast: RawASTOp) -> ASTOp {
    match ast {
        RawASTOp::AddReduce0(ast_0) => ASTOp::Add(ast_0),
        RawASTOp::DivReduce0(ast_0) => ASTOp::Div(ast_0),
        RawASTOp::ModReduce0(ast_0) => ASTOp::Mod(ast_0),
        RawASTOp::MulReduce0(ast_0) => ASTOp::Mul(ast_0),
        RawASTOp::SubReduce0(ast_0) => ASTOp::Sub(ast_0),
    }
}

enum StackItem {
    AST(RawAST),
    State(usize),
}

enum Action {
    Accept,
    Goto(usize),
    Shift(usize),
    Reduce(usize, String, String),
}

lazy_static! {
    static ref action_table: [(
        HashMap<TokenType, Action>,
        HashMap<String, Action>,
        Vec<String>
    ); 12] = [
        (
            vec![(TokenType::LiteralInteger, Action::Shift(2))]
                .into_iter()
                .collect(),
            vec![("expr".to_owned(), Action::Goto(1))]
                .into_iter()
                .collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(TokenType::Eof, Action::Accept)].into_iter().collect(),
            vec![].into_iter().collect(),
            vec![]
        ),
        (
            vec![
                (TokenType::OpMul, Action::Shift(8)),
                (TokenType::OpDiv, Action::Shift(6)),
                (TokenType::OpSub, Action::Shift(9)),
                (
                    TokenType::Eof,
                    Action::Reduce(1, "expr".to_owned(), "exprexpr-reduce-0".to_owned())
                ),
                (TokenType::OpAdd, Action::Shift(5)),
                (TokenType::OpMod, Action::Shift(7))
            ]
            .into_iter()
            .collect(),
            vec![
                ("op".to_owned(), Action::Goto(3)),
                ("_expr-1".to_owned(), Action::Goto(4))
            ]
            .into_iter()
            .collect(),
            vec![
                "add".to_owned(),
                "div".to_owned(),
                "mod".to_owned(),
                "mul".to_owned(),
                "sub".to_owned()
            ]
        ),
        (
            vec![(TokenType::LiteralInteger, Action::Shift(10))]
                .into_iter()
                .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(
                TokenType::Eof,
                Action::Reduce(2, "expr".to_owned(), "exprexpr-reduce-1".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec![]
        ),
        (
            vec![(
                TokenType::LiteralInteger,
                Action::Reduce(1, "op".to_owned(), "opadd-reduce-0".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(
                TokenType::LiteralInteger,
                Action::Reduce(1, "op".to_owned(), "opdiv-reduce-0".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(
                TokenType::LiteralInteger,
                Action::Reduce(1, "op".to_owned(), "opmod-reduce-0".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(
                TokenType::LiteralInteger,
                Action::Reduce(1, "op".to_owned(), "opmul-reduce-0".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![(
                TokenType::LiteralInteger,
                Action::Reduce(1, "op".to_owned(), "opsub-reduce-0".to_owned())
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec!["number".to_owned()]
        ),
        (
            vec![
                (
                    TokenType::Eof,
                    Action::Reduce(
                        2,
                        "_expr-1".to_owned(),
                        "_expr-1_expr-1-expr-reduce-0".to_owned()
                    )
                ),
                (TokenType::OpDiv, Action::Shift(6)),
                (TokenType::OpSub, Action::Shift(9)),
                (TokenType::OpAdd, Action::Shift(5)),
                (TokenType::OpMod, Action::Shift(7)),
                (TokenType::OpMul, Action::Shift(8))
            ]
            .into_iter()
            .collect(),
            vec![
                ("op".to_owned(), Action::Goto(3)),
                ("_expr-1".to_owned(), Action::Goto(11))
            ]
            .into_iter()
            .collect(),
            vec![
                "add".to_owned(),
                "div".to_owned(),
                "mod".to_owned(),
                "mul".to_owned(),
                "sub".to_owned()
            ]
        ),
        (
            vec![(
                TokenType::Eof,
                Action::Reduce(
                    3,
                    "_expr-1".to_owned(),
                    "_expr-1_expr-1-expr-reduce-1".to_owned()
                )
            )]
            .into_iter()
            .collect(),
            vec![].into_iter().collect(),
            vec![]
        )
    ];
}

pub fn parse(mut lexer: Lexer) -> Result<AST, String> {
    let mut state = 0;
    let mut stack: Vec<StackItem> = vec![StackItem::State(state)];

    let mut next_token = || loop {
        let token = lexer.next();

        if token.ty() != &TokenType::Comment {
            return token;
        }
    };

    let mut token = next_token();

    loop {
        state = match stack.last().unwrap() {
            &StackItem::State(state) => state,
            _ => unreachable!(),
        };

        let action = match action_table[state].0.get(token.ty()) {
            Some(action) => action,
            None => {
                return Err(format!(
                    "unexpected token; got {:#?}, expected one of {:#?}",
                    token, action_table[state].2
                ));
            }
        };

        match action {
            Action::Accept => {
                stack.pop();
                return Ok(match stack.pop().unwrap() {
                    StackItem::AST(ast) => raw_ast_to_ast(ast),
                    _ => unreachable!(),
                });
            }
            Action::Goto(..) => unreachable!(),
            &Action::Shift(next) => {
                stack.push(StackItem::AST(build_ast_terminal(token)));
                stack.push(StackItem::State(next));
                token = next_token();
            }
            Action::Reduce(count, name, reduce_name) => {
                let child_asts = stack
                    .split_off(stack.len() - count * 2)
                    .into_iter()
                    .filter(|item| match item {
                        StackItem::AST(..) => true,
                        _ => false,
                    })
                    .map(|item| match item {
                        StackItem::AST(ast) => ast,
                        _ => unreachable!(),
                    })
                    .collect::<Vec<_>>();
                let ast = build_ast_nonterminal(reduce_name, child_asts);

                state = match stack.last().unwrap() {
                    &StackItem::State(state) => state,
                    _ => unreachable!(),
                };

                stack.push(StackItem::AST(ast));
                stack.push(StackItem::State(
                    match action_table[state].1.get(name).unwrap() {
                        &Action::Goto(next) => next,
                        _ => unreachable!(),
                    },
                ));
            }
        }
    }
}

mod cursor;
mod parser;

pub use cursor::*;
pub use parser::*;

use ast::*;
use high_lexer::*;
use span::Span;

// TODO: Emit diagnostics to report syntax errors.
// TODO: Provide a way to report errors more easily.
// TODO: Fix the expectation of the parser to be correct.

pub fn parse_toplevel(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<TopLevel, (String, Span)> {
    if parser.expect_keyword(PUB) {
        let vis = Vis {
            kind: VisKind::Pub,
            span: parser.span(),
        };

        parser.expect_begin();
        if parser.expect_keyword(FN) {
            parser.expect_begin();
            let f = parse_fn(Some(vis), parser)?;
            Ok(TopLevel {
                span: vis.span.to(f.span),
                kind: TopLevelKind::Fn(f),
            })
        } else {
            Err(parser.expect_else())
        }
    } else if parser.expect_keyword(FN) {
        parser.expect_begin();
        let f = parse_fn(None, parser)?;
        Ok(TopLevel {
            span: f.span,
            kind: TopLevelKind::Fn(f),
        })
    } else {
        Err(parser.expect_else())
    }
}

fn parse_ty(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Ty, (String, Span)> {
    Ok(if parser.expect_keyword(BOOL) {
        Ty {
            kind: TyKind::Bool,
            span: parser.span(),
        }
    } else if parser.expect_keyword(CHAR) {
        Ty {
            kind: TyKind::Char,
            span: parser.span(),
        }
    } else if parser.expect_keyword(I64) {
        Ty {
            kind: TyKind::I64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(U64) {
        Ty {
            kind: TyKind::U64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(ISIZE) {
        Ty {
            kind: TyKind::Isize,
            span: parser.span(),
        }
    } else if parser.expect_keyword(USIZE) {
        Ty {
            kind: TyKind::Usize,
            span: parser.span(),
        }
    } else if parser.expect_keyword(F64) {
        Ty {
            kind: TyKind::F64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(STR) {
        Ty {
            kind: TyKind::Str,
            span: parser.span(),
        }
    } else if parser.expect_keyword(CPTR) {
        let span = parser.span();

        parser.expect_begin();
        return parse_ty(parser).map(|ty| Ty {
            span: span.to(ty.span),
            kind: TyKind::Cptr(Box::new(ty)),
        });
    } else if parser.expect_keyword(MPTR) {
        let span = parser.span();

        parser.expect_begin();
        return parse_ty(parser).map(|ty| Ty {
            span: span.to(ty.span),
            kind: TyKind::Mptr(Box::new(ty)),
        });
    } else {
        return Err(parser.expect_else());
    })
}

fn parse_fn(
    vis: Option<Vis>,
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Fn, (String, Span)> {
    let span = parser.span();
    let name = if let Some(id) = parser.expect_id() {
        SymbolWithSpan {
            symbol: id,
            span: parser.span(),
        }
    } else {
        return Err(parser.expect_else());
    };

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenParen) {
        return Err(parser.expect_else());
    }

    let mut params = vec![];

    while !parser.expect_kind(TokenKind::CloseParen) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        let name = if let Some(id) = parser.expect_id() {
            SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            }
        } else {
            return Err(parser.expect_else());
        };

        parser.expect_begin();
        let ty = parse_ty(parser)?;

        params.push(FnParam {
            span: name.span.to(ty.span),
            name,
            ty,
        });

        parser.expect_begin();
        parser.expect_kind(TokenKind::Comma);
    }

    let mut return_ty = None;

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenBrace) {
        return_ty = Some(parse_ty(parser)?);

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::OpenBrace) {
            return Err(parser.expect_else());
        }
    }

    parser.expect_begin();
    let body = parse_block(parser)?;

    return Ok(Fn {
        span: span.to(body.span),
        vis,
        name,
        params,
        return_ty,
        body,
    });
}

fn parse_if(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<If, (String, Span)> {
    let span = parser.span();
    let cond = parse_expr(parser)?;

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenBrace) {
        return Err(parser.expect_else());
    }

    let then_body = parse_block(parser)?;

    parser.expect_begin();
    Ok(if parser.expect_keyword(ELSE) {
        parser.expect_begin();
        if parser.expect_keyword(IF) {
            let else_body = parse_if(parser)?;

            If {
                span: span.to(else_body.span),
                cond,
                then_body,
                else_kind: Some(Box::new(ElseKind::ElseIf(else_body))),
            }
        } else if parser.expect_kind(TokenKind::OpenBrace) {
            let else_body = parse_block(parser)?;

            If {
                span: span.to(else_body.span),
                cond,
                then_body,
                else_kind: Some(Box::new(ElseKind::Else(else_body))),
            }
        } else {
            return Err(parser.expect_else());
        }
    } else {
        If {
            span: span.to(then_body.span),
            cond,
            then_body,
            else_kind: None,
        }
    })
}

fn parse_block(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Block, (String, Span)> {
    let span = parser.span();
    let mut stmts = vec![];

    while !parser.expect_kind(TokenKind::CloseBrace) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        stmts.push(parse_stmt(parser)?);
    }

    Ok(Block {
        stmts,
        span: span.to(parser.span()),
    })
}

fn parse_stmt(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Stmt, (String, Span)> {
    if parser.expect_keyword(FN) {
        parser.expect_begin();
        let f = parse_fn(None, parser)?;

        Ok(Stmt {
            span: f.span,
            kind: StmtKind::Fn(f),
        })
    } else if parser.expect_keyword(IF) {
        parser.expect_begin();
        let r#if = parse_if(parser)?;

        Ok(Stmt {
            span: r#if.span,
            kind: StmtKind::If(r#if),
        })
    } else if parser.expect_kind(TokenKind::OpenBrace) {
        parser.expect_begin();
        let block = parse_block(parser)?;

        Ok(Stmt {
            span: block.span,
            kind: StmtKind::Block(block),
        })
    } else if parser.expect_keyword(BREAK) {
        let span = parser.span();

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Semicolon) {
            return Err(parser.expect_else());
        }

        Ok(Stmt {
            kind: StmtKind::Break(Break { span }),
            span: span.to(parser.span()),
        })
    } else if parser.expect_keyword(CONTINUE) {
        let span = parser.span();

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Semicolon) {
            return Err(parser.expect_else());
        }

        Ok(Stmt {
            kind: StmtKind::Continue(Continue { span }),
            span: span.to(parser.span()),
        })
    } else if parser.expect_keyword(RETURN) {
        let mut expr = None;
        let mut span = parser.span();

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Semicolon) {
            expr = Some({
                let expr = parse_expr(parser)?;
                span = span.to(expr.span);
                expr
            });

            parser.expect_begin();
            if !parser.expect_kind(TokenKind::Semicolon) {
                return Err(parser.expect_else());
            }
        }

        Ok(Stmt {
            kind: StmtKind::Return(Return { expr, span }),
            span: span.to(parser.span()),
        })
    } else {
        let expr = parse_expr(parser)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Semicolon) {
            return Err(parser.expect_else());
        }

        Ok(Stmt {
            span: expr.span.to(parser.span()),
            kind: StmtKind::Expr(expr),
        })
    }
}

fn parse_expr(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, (String, Span)> {
    parse_assign(parser)
}

fn parse_assign(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, (String, Span)> {
    let item = parse_expr_binary_log_or(parser)?;

    parser.expect_begin();
    if parser.expect_kind(TokenKind::Assign) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::Assign(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignAdd) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignAdd(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignSub) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignSub(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignMul) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignMul(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignDiv) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignDiv(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignMod) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignMod(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignShl) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignShl(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignShr) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignShr(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitOr) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitOr(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitAnd) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitAnd(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitXor) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitXor(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitNot) {
        let rhs = parse_expr_binary_log_or(parser)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitNot(Box::new(item), Box::new(rhs)),
        })
    } else {
        Ok(item)
    }
}

fn parse_expr_binary_log_or(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_log_and(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogOr) {
            let rhs = parse_expr_binary_log_and(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::LogOr(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_log_and(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_cmp(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogAnd) {
            let rhs = parse_expr_binary_cmp(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::LogAnd(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_cmp(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_rng(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Eq) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Eq(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Ne) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Ne(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Lt) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Lt(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Gt) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Gt(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Le) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Le(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Ge) {
            let rhs = parse_expr_binary_rng(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Ge(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_rng(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_shift(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Rng) {
            let rhs = parse_expr_binary_shift(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Rng(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::RngInclusive) {
            let rhs = parse_expr_binary_shift(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::RngInclusive(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_shift(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_addsub(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Shl) {
            let rhs = parse_expr_binary_addsub(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Shl(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Shr) {
            let rhs = parse_expr_binary_addsub(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Shr(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_addsub(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_muldivmod(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Add) {
            let rhs = parse_expr_binary_muldivmod(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Add(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Sub) {
            let rhs = parse_expr_binary_muldivmod(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Sub(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_muldivmod(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_or(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Mul) {
            let rhs = parse_expr_binary_bit_or(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Mul(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Div) {
            let rhs = parse_expr_binary_bit_or(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Div(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Mod) {
            let rhs = parse_expr_binary_bit_or(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Mod(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_or(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_and(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitOr) {
            let rhs = parse_expr_binary_bit_and(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::BitOr(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_and(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_xor(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitAnd) {
            let rhs = parse_expr_binary_bit_xor(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::BitAnd(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_xor(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_as(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitXor) {
            let rhs = parse_expr_as(parser)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::BitXor(Box::new(item), Box::new(rhs)),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_as(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_unary(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_keyword(AS) {
            let ty = parse_ty(parser)?;
            item = Expr {
                span: item.span.to(ty.span),
                kind: ExprKind::Cast(Box::new(item), ty),
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_unary(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    if parser.expect_kind(TokenKind::LogNot) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::LogNot(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::Add) {
        parser.expect_begin();
        parse_expr_unary(parser)
    } else if parser.expect_kind(TokenKind::Sub) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::Neg(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::BitNot) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::BitNot(Box::new(expr)),
        })
    } else {
        let mut item = parse_expr_member(parser)?;

        while parser.exists() {
            parser.expect_begin();
            if parser.expect_kind(TokenKind::OpenParen) {
                let mut args = vec![];

                while !parser.expect_kind(TokenKind::CloseParen) {
                    if !parser.exists() {
                        return Err(parser.expect_else());
                    }

                    args.push(parse_expr(parser)?);

                    parser.expect_begin();
                    parser.expect_kind(TokenKind::Comma);
                }

                item = Expr {
                    span: item.span.to(parser.span()),
                    kind: ExprKind::Call(Box::new(item), args),
                }
            } else if parser.expect_kind(TokenKind::OpenBracket) {
                let expr = parse_expr(parser)?;

                parser.expect_begin();
                if !parser.expect_kind(TokenKind::CloseBracket) {
                    return Err(parser.expect_else());
                }

                item = Expr {
                    span: item.span.to(parser.span()),
                    kind: ExprKind::Index(Box::new(item), Box::new(expr)),
                }
            } else {
                break;
            }
        }

        Ok(item)
    }
}

fn parse_expr_member(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_item(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Dot) {
            parser.expect_begin();
            if let Some(id) = parser.expect_id() {
                item = Expr {
                    span: item.span.to(parser.span()),
                    kind: ExprKind::Member(
                        Box::new(item),
                        SymbolWithSpan {
                            symbol: id,
                            span: parser.span(),
                        },
                    ),
                }
            } else {
                return Err(parser.expect_else());
            }
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_item(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    if parser.expect_kind(TokenKind::OpenParen) {
        let expr = parse_expr(parser)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::CloseParen) {
            return Err(parser.expect_else());
        }

        Ok(expr)
    } else if let Some(id) = parser.expect_id() {
        Ok(Expr {
            kind: ExprKind::Id(SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            }),
            span: parser.span(),
        })
    } else if let Some(literal) = parser.expect_literal() {
        Ok(Expr {
            kind: ExprKind::Literal(Literal {
                lit: literal,
                span: parser.span(),
            }),
            span: parser.span(),
        })
    } else {
        Err(parser.expect_else())
    }
}

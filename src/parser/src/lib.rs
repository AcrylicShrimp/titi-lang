mod cursor;
mod parser;

pub use cursor::*;
pub use parser::*;

use ast::*;
use high_lexer::*;

// TODO: Emit diagnostics to report syntax errors.
// TODO: Provide a way to report errors more easily.

pub fn parse_toplevel(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<TopLevel, String> {
    parser.expect_begin();
    if parser.expect_keyword(PUB) {
        if parser.expect_keyword(FN) {
            parser.expect_begin();
            return Ok(TopLevel::Fn(parse_fn(Vis::Pub, parser)?));
        } else {
            return Err(parser.expect_else());
        }
    } else if parser.expect_keyword(FN) {
        parser.expect_begin();
        return Ok(TopLevel::Fn(parse_fn(Vis::None, parser)?));
    } else {
        return Err(parser.expect_else());
    }
}

fn parse_ty(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Ty, String> {
    Ok(if parser.expect_keyword(BOOL) {
        Ty::Bool
    } else if parser.expect_keyword(CHAR) {
        Ty::Char
    } else if parser.expect_keyword(I64) {
        Ty::I64
    } else if parser.expect_keyword(U64) {
        Ty::U64
    } else if parser.expect_keyword(ISIZE) {
        Ty::Isize
    } else if parser.expect_keyword(USIZE) {
        Ty::Isize
    } else if parser.expect_keyword(F64) {
        Ty::F64
    } else if parser.expect_keyword(STR) {
        Ty::Str
    } else if parser.expect_keyword(CPTR) {
        parser.expect_begin();
        return parse_ty(parser).map(|ty| Ty::Cptr(Box::new(ty)));
    } else if parser.expect_keyword(MPTR) {
        parser.expect_begin();
        return parse_ty(parser).map(|ty| Ty::Mptr(Box::new(ty)));
    } else {
        return Err(parser.expect_else());
    })
}

fn parse_fn(vis: Vis, parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Fn, String> {
    let name = if let Some(id) = parser.expect_id() {
        id
    } else {
        return Err(parser.expect_else());
    };

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenParen) {
        return Err(parser.expect_else());
    }

    let mut params = vec![];

    while parser.exists() && !parser.expect_kind(TokenKind::CloseParen) {
        let name = if let Some(id) = parser.expect_id() {
            id
        } else {
            return Err(parser.expect_else());
        };

        parser.expect_begin();
        let ty = parse_ty(parser)?;

        params.push(FnParam { name, ty });

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
        vis,
        name,
        params,
        return_ty,
        body,
    });
}

fn parse_block(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Block, String> {
    let mut stmts = vec![];

    while parser.exists() && !parser.expect_kind(TokenKind::CloseBrace) {
        stmts.push(parse_stmt(parser)?);
    }

    parser.expect_begin();
    Ok(Block { stmts })
}

fn parse_stmt(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Stmt, String> {
    let expr = parse_expr(parser)?;

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::Semicolon) {
        return Err(parser.expect_else());
    }

    Ok(Stmt::Expr(expr))
}

fn parse_expr(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    parse_assign(parser)
}

fn parse_assign(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    let item = parse_expr_binary_log_or(parser)?;

    parser.expect_begin();
    if parser.expect_kind(TokenKind::Assign) {
        Ok(Expr::Assign(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignAdd) {
        Ok(Expr::AssignAdd(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignSub) {
        Ok(Expr::AssignSub(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignMul) {
        Ok(Expr::AssignMul(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignDiv) {
        Ok(Expr::AssignDiv(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignMod) {
        Ok(Expr::AssignMod(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignShl) {
        Ok(Expr::AssignShl(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignShr) {
        Ok(Expr::AssignShr(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignBitOr) {
        Ok(Expr::AssignBitOr(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignBitAnd) {
        Ok(Expr::AssignBitAnd(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignBitXor) {
        Ok(Expr::AssignBitXor(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else if parser.expect_kind(TokenKind::AssignBitNot) {
        Ok(Expr::AssignBitNot(
            Box::new(item),
            Box::new(parse_expr_binary_log_or(parser)?),
        ))
    } else {
        Ok(item)
    }
}

fn parse_expr_binary_log_or(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_log_and(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogOr) {
            item = Expr::LogOr(Box::new(item), Box::new(parse_expr_binary_log_and(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_log_and(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_cmp(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogAnd) {
            item = Expr::LogAnd(Box::new(item), Box::new(parse_expr_binary_cmp(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_cmp(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    let mut item = parse_expr_binary_shift(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Eq) {
            item = Expr::Eq(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else if parser.expect_kind(TokenKind::Ne) {
            item = Expr::Ne(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else if parser.expect_kind(TokenKind::Lt) {
            item = Expr::Lt(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else if parser.expect_kind(TokenKind::Gt) {
            item = Expr::Gt(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else if parser.expect_kind(TokenKind::Le) {
            item = Expr::Le(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else if parser.expect_kind(TokenKind::Ge) {
            item = Expr::Ge(Box::new(item), Box::new(parse_expr_binary_shift(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_shift(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_addsub(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Shl) {
            item = Expr::Shl(Box::new(item), Box::new(parse_expr_binary_addsub(parser)?));
        } else if parser.expect_kind(TokenKind::Shr) {
            item = Expr::Shr(Box::new(item), Box::new(parse_expr_binary_addsub(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_addsub(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_muldivmod(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Add) {
            item = Expr::Add(
                Box::new(item),
                Box::new(parse_expr_binary_muldivmod(parser)?),
            );
        } else if parser.expect_kind(TokenKind::Sub) {
            item = Expr::Sub(
                Box::new(item),
                Box::new(parse_expr_binary_muldivmod(parser)?),
            );
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_muldivmod(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_bit_or(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Mul) {
            item = Expr::Mul(Box::new(item), Box::new(parse_expr_binary_bit_or(parser)?));
        } else if parser.expect_kind(TokenKind::Div) {
            item = Expr::Div(Box::new(item), Box::new(parse_expr_binary_bit_or(parser)?));
        } else if parser.expect_kind(TokenKind::Mod) {
            item = Expr::Mod(Box::new(item), Box::new(parse_expr_binary_bit_or(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_or(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_bit_and(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitOr) {
            item = Expr::BitOr(Box::new(item), Box::new(parse_expr_binary_bit_and(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_and(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_binary_bit_xor(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitAnd) {
            item = Expr::BitAnd(Box::new(item), Box::new(parse_expr_binary_bit_xor(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_binary_bit_xor(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, String> {
    let mut item = parse_expr_as(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitXor) {
            item = Expr::BitXor(Box::new(item), Box::new(parse_expr_as(parser)?));
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_as(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    let mut item = parse_expr_unary(parser)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_keyword(AS) {
            item = Expr::Cast(Box::new(item), parse_ty(parser)?);
        } else {
            break;
        }
    }

    Ok(item)
}

fn parse_expr_unary(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    if parser.expect_kind(TokenKind::LogNot) {
        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr::LogNot(Box::new(expr)))
    } else if parser.expect_kind(TokenKind::Add) {
        parser.expect_begin();
        parse_expr_unary(parser)
    } else if parser.expect_kind(TokenKind::Sub) {
        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr::Neg(Box::new(expr)))
    } else if parser.expect_kind(TokenKind::BitNot) {
        parser.expect_begin();
        parse_expr_unary(parser).map(|expr| Expr::BitNot(Box::new(expr)))
    } else {
        let mut item = parse_expr_item(parser)?;

        while parser.exists() {
            parser.expect_begin();
            if parser.expect_kind(TokenKind::OpenParen) {
                let mut args = vec![];

                while parser.exists() && !parser.expect_kind(TokenKind::CloseParen) {
                    args.push(parse_expr(parser)?);

                    parser.expect_begin();
                    parser.expect_kind(TokenKind::Comma);
                }

                item = Expr::Call(Box::new(item), args);
            } else if parser.expect_kind(TokenKind::OpenBracket) {
                let expr = parse_expr(parser)?;

                parser.expect_begin();
                if !parser.expect_kind(TokenKind::CloseBracket) {
                    return Err(parser.expect_else());
                }

                item = Expr::Index(Box::new(item), Box::new(expr));
            } else {
                break;
            }
        }

        Ok(item)
    }
}

fn parse_expr_item(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Expr, String> {
    if parser.expect_kind(TokenKind::OpenParen) {
        let expr = parse_expr(parser)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::CloseParen) {
            return Err(parser.expect_else());
        }

        Ok(expr)
    } else if let Some(id) = parser.expect_id() {
        Ok(Expr::Id(id))
    } else if let Some(literal) = parser.expect_literal() {
        Ok(Expr::Literal(literal))
    } else {
        Err(parser.expect_else())
    }
}

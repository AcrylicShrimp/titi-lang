mod cursor;
mod parser;

use crate::parser::*;
use ast::*;
use cursor::*;
use high_lexer::*;
use span::{Source, Span};
use std::sync::Arc;

// TODO: Emit diagnostics to report syntax errors.
// TODO: Provide a way to report errors more easily.
// TODO: Fix the expectation of the parser to be correct.

pub fn parse(source: Arc<Source>) -> Result<Module, (String, Span)> {
    let mut top_levels = vec![];

    {
        let mut parser = Parser::new(Cursor::new(token_iter(&source)));

        while parser.exists() {
            top_levels.push(parse_top_level(&mut parser)?);
        }
    }

    Ok(Module { source, top_levels })
}

fn parse_top_level(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<TopLevel, (String, Span)> {
    if parser.expect_keyword(USE) {
        parser.expect_begin();
        let item = parse_use(parser)?;
        Ok(TopLevel {
            span: item.span.to(parser.span()),
            kind: TopLevelKind::Use(item),
        })
    } else if parser.expect_keyword(EXTERN) {
        let ext = Extern {
            kind: ExternKind::Extern,
            span: parser.span(),
        };

        parser.expect_begin();
        if parser.expect_keyword(STRUCT) {
            parser.expect_begin();
            let item = parse_struct(parser)?;

            Ok(TopLevel {
                span: ext.span.to(item.span),
                kind: TopLevelKind::Struct(TopLevelItem {
                    span: ext.span.to(item.span),
                    prefix: Some(TopLevelItemPrefix {
                        span: ext.span,
                        kind: TopLevelItemPrefixKind::Extern(ext),
                    }),
                    item,
                }),
            })
        } else if parser.expect_keyword(FN) {
            parser.expect_begin();
            let header = parse_fn_header(parser)?;

            parser.expect_begin();
            if parser.expect_kind(TokenKind::Semicolon) {
                Ok(TopLevel {
                    span: ext.span.to(parser.span()),
                    kind: TopLevelKind::FnHeader(TopLevelFnHeader {
                        span: ext.span.to(header.span),
                        ext,
                        header,
                    }),
                })
            } else if parser.expect_kind(TokenKind::OpenBrace) {
                parser.expect_begin();
                let body = parse_block(parser)?;

                Ok(TopLevel {
                    span: ext.span.to(body.span),
                    kind: TopLevelKind::Fn(TopLevelItem {
                        span: ext.span.to(body.span),
                        prefix: Some(TopLevelItemPrefix {
                            span: ext.span,
                            kind: TopLevelItemPrefixKind::Extern(ext),
                        }),
                        item: Fn {
                            span: header.span.to(body.span),
                            header,
                            body,
                        },
                    }),
                })
            } else {
                Err(parser.expect_else())
            }
        } else {
            Err(parser.expect_else())
        }
    } else if parser.expect_keyword(PUB) {
        let prefix = TopLevelItemPrefix {
            kind: TopLevelItemPrefixKind::Vis(Vis {
                kind: VisKind::Pub,
                span: parser.span(),
            }),
            span: parser.span(),
        };

        parser.expect_begin();
        if parser.expect_keyword(STRUCT) {
            parser.expect_begin();
            let item = parse_struct(parser)?;
            Ok(TopLevel {
                span: prefix.span.to(item.span),
                kind: TopLevelKind::Struct(TopLevelItem {
                    span: prefix.span.to(item.span),
                    prefix: Some(prefix),
                    item,
                }),
            })
        } else if parser.expect_keyword(FN) {
            parser.expect_begin();
            let item = parse_fn(parser)?;
            Ok(TopLevel {
                span: prefix.span.to(item.span),
                kind: TopLevelKind::Fn(TopLevelItem {
                    span: prefix.span.to(item.span),
                    prefix: Some(prefix),
                    item,
                }),
            })
        } else {
            Err(parser.expect_else())
        }
    } else if parser.expect_keyword(STRUCT) {
        parser.expect_begin();
        let item = parse_struct(parser)?;
        Ok(TopLevel {
            span: item.span,
            kind: TopLevelKind::Struct(TopLevelItem {
                span: item.span,
                prefix: None,
                item,
            }),
        })
    } else if parser.expect_keyword(FN) {
        parser.expect_begin();
        let item = parse_fn(parser)?;
        Ok(TopLevel {
            span: item.span,
            kind: TopLevelKind::Fn(TopLevelItem {
                span: item.span,
                prefix: None,
                item,
            }),
        })
    } else {
        Err(parser.expect_else())
    }
}

fn parse_ty(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Ty, (String, Span)> {
    let ref_kind = if parser.expect_keyword(CREF) {
        Some(TyRefKind::Cref)
    } else if parser.expect_keyword(MREF) {
        Some(TyRefKind::Mref)
    } else {
        None
    };
    let span = if ref_kind.is_some() {
        Some(parser.span())
    } else {
        None
    };
    let sub_ty = parse_sub_ty(parser)?;

    Ok(Ty {
        ref_kind,
        span: if let Some(span) = span {
            span.to(sub_ty.span)
        } else {
            sub_ty.span
        },
        sub_ty,
    })
}

fn parse_sub_ty(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<SubTy, (String, Span)> {
    Ok(if parser.expect_keyword(BOOL) {
        SubTy {
            kind: TyKind::Bool,
            span: parser.span(),
        }
    } else if parser.expect_keyword(CHAR) {
        SubTy {
            kind: TyKind::Char,
            span: parser.span(),
        }
    } else if parser.expect_keyword(I64) {
        SubTy {
            kind: TyKind::I64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(U64) {
        SubTy {
            kind: TyKind::U64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(ISIZE) {
        SubTy {
            kind: TyKind::Isize,
            span: parser.span(),
        }
    } else if parser.expect_keyword(USIZE) {
        SubTy {
            kind: TyKind::Usize,
            span: parser.span(),
        }
    } else if parser.expect_keyword(F64) {
        SubTy {
            kind: TyKind::F64,
            span: parser.span(),
        }
    } else if parser.expect_keyword(STR) {
        SubTy {
            kind: TyKind::Str,
            span: parser.span(),
        }
    } else if parser.expect_keyword(CPTR) {
        let span = parser.span();

        parser.expect_begin();
        SubTy {
            kind: TyKind::Cptr(Box::new(parse_sub_ty(parser)?)),
            span: span.to(parser.span()),
        }
    } else if parser.expect_keyword(MPTR) {
        let span = parser.span();

        parser.expect_begin();
        SubTy {
            kind: TyKind::Mptr(Box::new(parse_sub_ty(parser)?)),
            span: span.to(parser.span()),
        }
    } else if let Some(id) = parser.expect_id() {
        let id = SymbolWithSpan {
            symbol: id,
            span: parser.span(),
        };

        if parser.expect_kind(TokenKind::Dot) {
            parser.expect_begin();
            if let Some(item) = parser.expect_id() {
                let span = id.span.to(parser.span());

                SubTy {
                    kind: TyKind::UserDef(TyUserDef {
                        module: Some(id),
                        id: SymbolWithSpan {
                            symbol: item,
                            span: parser.span(),
                        },
                        span,
                    }),
                    span,
                }
            } else {
                return Err(parser.expect_else());
            }
        } else {
            SubTy {
                kind: TyKind::UserDef(TyUserDef {
                    module: None,
                    id,
                    span: id.span,
                }),
                span: id.span,
            }
        }
    } else {
        return Err(parser.expect_else());
    })
}

fn parse_use(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Use, (String, Span)> {
    let span = parser.span();

    parser.expect_begin();
    let mut segment_span;
    let mut segments = vec![if let Some(id) = parser.expect_id() {
        segment_span = parser.span();
        SymbolWithSpan {
            symbol: id,
            span: parser.span(),
        }
    } else {
        return Err(parser.expect_else());
    }];

    while !parser.expect_kind(TokenKind::Semicolon) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Dot) {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        if let Some(id) = parser.expect_id() {
            segment_span = parser.span();
            segments.push(SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            });
        } else {
            return Err(parser.expect_else());
        }
    }

    Ok(Use {
        segments,
        span: span.to(segment_span),
    })
}

fn parse_struct(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Struct, (String, Span)> {
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
    if !parser.expect_kind(TokenKind::OpenBrace) {
        return Err(parser.expect_else());
    }

    let mut fields = vec![];

    while !parser.expect_kind(TokenKind::CloseBrace) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        let vis = if parser.expect_keyword(PUB) {
            Some(Vis {
                kind: VisKind::Pub,
                span: parser.span(),
            })
        } else {
            None
        };

        parser.expect_begin();
        let name = if let Some(id) = parser.expect_id() {
            SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            }
        } else {
            return Err(parser.expect_else());
        };

        let kind;
        let kind_span;

        parser.expect_begin();
        if parser.expect_keyword(STRUCT) {
            let inner_struct = parse_inner_struct(parser)?;
            kind_span = inner_struct.span;
            kind = StructFieldKind::Struct(inner_struct);
        } else {
            let ty = parse_ty(parser)?;
            kind_span = ty.span;
            kind = StructFieldKind::Plain(ty);
        }

        fields.push(StructField {
            span: vis.as_ref().map_or(name.span, |vis| vis.span).to(kind_span),
            vis,
            name,
            kind,
        });

        parser.expect_begin();
        parser.expect_kind(TokenKind::Comma);
    }

    Ok(Struct {
        name,
        fields,
        span: span.to(parser.span()),
    })
}

fn parse_inner_struct(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<InnerStruct, (String, Span)> {
    let span = parser.span();

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenBrace) {
        return Err(parser.expect_else());
    }

    let mut fields = vec![];

    while !parser.expect_kind(TokenKind::CloseBrace) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        let name = if let Some(id) = parser.expect_id() {
            SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            }
        } else {
            return Err(parser.expect_else());
        };

        let kind;
        let kind_span;

        parser.expect_begin();
        if parser.expect_keyword(STRUCT) {
            let inner_struct = parse_inner_struct(parser)?;
            kind_span = inner_struct.span;
            kind = StructFieldKind::Struct(inner_struct);
        } else {
            let ty = parse_ty(parser)?;
            kind_span = ty.span;
            kind = StructFieldKind::Plain(ty);
        }

        fields.push(InnerStructField {
            span: name.span.to(kind_span),
            name,
            kind,
        });

        parser.expect_begin();
        parser.expect_kind(TokenKind::Comma);
    }

    Ok(InnerStruct {
        fields,
        span: span.to(parser.span()),
    })
}

fn parse_fn(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Fn, (String, Span)> {
    parser.expect_begin();
    let header = parse_fn_header(parser)?;

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::OpenBrace) {
        return Err(parser.expect_else());
    }

    parser.expect_begin();
    let body = parse_block(parser)?;

    Ok(Fn {
        span: header.span.to(body.span),
        header,
        body,
    })
}

fn parse_fn_header(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<FnHeader, (String, Span)> {
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

    if !parser.cursor().is_kind(TokenKind::Semicolon)
        && !parser.cursor().is_kind(TokenKind::OpenBrace)
    {
        parser.expect_begin();
        return_ty = Some(parse_ty(parser)?);
    }

    Ok(FnHeader {
        name,
        params,
        return_ty,
        span: span.to(parser.span()),
    })
}

fn parse_let(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<Let, (String, Span)> {
    let span = parser.span();
    let name = if let Some(id) = parser.expect_id() {
        SymbolWithSpan {
            symbol: id,
            span: parser.span(),
        }
    } else {
        return Err(parser.expect_else());
    };

    let kind;
    let kind_span;

    parser.expect_begin();
    if parser.expect_kind(TokenKind::Assign) {
        let expr = parse_expr(parser, true)?;

        kind_span = expr.span;
        kind = LetKind::Expr(expr);
    } else {
        let ty = parse_ty(parser)?;

        parser.expect_begin();
        if parser.expect_kind(TokenKind::Assign) {
            let expr = parse_expr(parser, true)?;

            kind_span = expr.span;
            kind = LetKind::TyExpr(ty, expr);
        } else {
            kind_span = ty.span;
            kind = LetKind::Ty(ty);
        }
    }

    parser.expect_begin();
    if !parser.expect_kind(TokenKind::Semicolon) {
        return Err(parser.expect_else());
    }

    Ok(Let {
        span: span.to(kind_span),
        name,
        kind,
    })
}

fn parse_if(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<If, (String, Span)> {
    let span = parser.span();
    let cond = parse_expr(parser, false)?;

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

fn parse_for(parser: &mut Parser<impl Iterator<Item = Token>>) -> Result<For, (String, Span)> {
    let span = parser.span();

    if let Some(first) = parser.cursor().first() {
        if let TokenKind::Id(id) = first.kind() {
            if let Some(second) = parser.cursor().second() {
                if let TokenKind::Id(keyword) = second.kind() {
                    if *keyword == IN {
                        let name = SymbolWithSpan {
                            symbol: *id,
                            span: first.span(),
                        };

                        parser.consume();
                        parser.consume();

                        let expr = parse_expr(parser, false)?;

                        parser.expect_begin();
                        if !parser.expect_kind(TokenKind::OpenBrace) {
                            return Err(parser.expect_else());
                        }

                        let body = parse_block(parser)?;

                        return Ok(For {
                            span: span.to(body.span),
                            kind: ForKind::ForIn(name, expr),
                            body,
                        });
                    }
                }
            }
        }
    }

    parser.expect_begin();
    if parser.expect_kind(TokenKind::OpenBrace) {
        let body = parse_block(parser)?;

        Ok(For {
            span: span.to(body.span),
            kind: ForKind::Loop,
            body,
        })
    } else {
        let expr = parse_expr(parser, false)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::OpenBrace) {
            return Err(parser.expect_else());
        }

        let body = parse_block(parser)?;

        Ok(For {
            span: span.to(body.span),
            kind: ForKind::While(expr),
            body,
        })
    }
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
    if parser.expect_keyword(STRUCT) {
        parser.expect_begin();
        let s = parse_struct(parser)?;

        Ok(Stmt {
            span: s.span,
            kind: StmtKind::Struct(s),
        })
    } else if parser.expect_keyword(FN) {
        parser.expect_begin();
        let f = parse_fn(parser)?;

        Ok(Stmt {
            span: f.span,
            kind: StmtKind::Fn(f),
        })
    } else if parser.expect_keyword(LET) {
        parser.expect_begin();
        let l = parse_let(parser)?;

        Ok(Stmt {
            span: l.span.to(parser.span()),
            kind: StmtKind::Let(l),
        })
    } else if parser.expect_keyword(IF) {
        parser.expect_begin();
        let r#if = parse_if(parser)?;

        Ok(Stmt {
            span: r#if.span,
            kind: StmtKind::If(r#if),
        })
    } else if parser.expect_keyword(FOR) {
        parser.expect_begin();
        let r#for = parse_for(parser)?;

        Ok(Stmt {
            span: r#for.span,
            kind: StmtKind::For(r#for),
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
                let expr = parse_expr(parser, true)?;
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
        let expr = parse_expr(parser, true)?;

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

fn parse_expr(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    parse_assign(parser, allow_object_literal)
}

fn parse_assign(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let item = parse_expr_binary_log_or(parser, allow_object_literal)?;

    parser.expect_begin();
    if parser.expect_kind(TokenKind::Assign) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::Assign(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignAdd) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignAdd(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignSub) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignSub(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignMul) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignMul(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignDiv) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignDiv(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignMod) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignMod(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignShl) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignShl(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignShr) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignShr(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitOr) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitOr(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitAnd) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitAnd(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitXor) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
        Ok(Expr {
            span: item.span.to(rhs.span),
            kind: ExprKind::AssignBitXor(Box::new(item), Box::new(rhs)),
        })
    } else if parser.expect_kind(TokenKind::AssignBitNot) {
        let rhs = parse_expr_binary_log_or(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_log_and(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogOr) {
            let rhs = parse_expr_binary_log_and(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_cmp(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::LogAnd) {
            let rhs = parse_expr_binary_cmp(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_rng(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Eq) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Eq(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Ne) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Ne(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Lt) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Lt(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Gt) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Gt(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Le) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Le(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Ge) {
            let rhs = parse_expr_binary_rng(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_shift(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Rng) {
            let rhs = parse_expr_binary_shift(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Rng(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::RngInclusive) {
            let rhs = parse_expr_binary_shift(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_addsub(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Shl) {
            let rhs = parse_expr_binary_addsub(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Shl(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Shr) {
            let rhs = parse_expr_binary_addsub(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_muldivmod(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Add) {
            let rhs = parse_expr_binary_muldivmod(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Add(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Sub) {
            let rhs = parse_expr_binary_muldivmod(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_or(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::Mul) {
            let rhs = parse_expr_binary_bit_or(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Mul(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Div) {
            let rhs = parse_expr_binary_bit_or(parser, allow_object_literal)?;
            item = Expr {
                span: item.span.to(rhs.span),
                kind: ExprKind::Div(Box::new(item), Box::new(rhs)),
            }
        } else if parser.expect_kind(TokenKind::Mod) {
            let rhs = parse_expr_binary_bit_or(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_and(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitOr) {
            let rhs = parse_expr_binary_bit_and(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_binary_bit_xor(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitAnd) {
            let rhs = parse_expr_binary_bit_xor(parser, allow_object_literal)?;
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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_as(parser, allow_object_literal)?;

    while parser.exists() {
        parser.expect_begin();
        if parser.expect_kind(TokenKind::BitXor) {
            let rhs = parse_expr_as(parser, allow_object_literal)?;
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

fn parse_expr_as(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    let mut item = parse_expr_unary(parser, allow_object_literal)?;

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
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    if parser.expect_kind(TokenKind::LogNot) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser, allow_object_literal).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::LogNot(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::Add) {
        parser.expect_begin();
        parse_expr_unary(parser, allow_object_literal)
    } else if parser.expect_kind(TokenKind::Sub) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser, allow_object_literal).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::Neg(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::BitNot) {
        let span = parser.span();

        parser.expect_begin();
        parse_expr_unary(parser, allow_object_literal).map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::BitNot(Box::new(expr)),
        })
    } else {
        parse_expr_single_and_member(parser, allow_object_literal)
    }
}

fn parse_expr_single_and_member(
    parser: &mut Parser<impl Iterator<Item = Token>>,
    allow_object_literal: bool,
) -> Result<Expr, (String, Span)> {
    if parser.expect_kind(TokenKind::LogNot) {
        let span = parser.span();

        parser.expect_begin();
        if allow_object_literal {
            parse_expr_object(parser)
        } else {
            parse_expr_item(parser)
        }
        .map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::LogNot(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::Add) {
        parser.expect_begin();
        if allow_object_literal {
            parse_expr_object(parser)
        } else {
            parse_expr_item(parser)
        }
    } else if parser.expect_kind(TokenKind::Sub) {
        let span = parser.span();

        parser.expect_begin();
        if allow_object_literal {
            parse_expr_object(parser)
        } else {
            parse_expr_item(parser)
        }
        .map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::Neg(Box::new(expr)),
        })
    } else if parser.expect_kind(TokenKind::BitNot) {
        let span = parser.span();

        parser.expect_begin();
        if allow_object_literal {
            parse_expr_object(parser)
        } else {
            parse_expr_item(parser)
        }
        .map(|expr| Expr {
            span: span.to(expr.span),
            kind: ExprKind::BitNot(Box::new(expr)),
        })
    } else {
        let mut item = if allow_object_literal {
            parse_expr_object(parser)
        } else {
            parse_expr_item(parser)
        }?;

        while parser.exists() {
            parser.expect_begin();
            if parser.expect_kind(TokenKind::OpenParen) {
                let mut args = vec![];

                while !parser.expect_kind(TokenKind::CloseParen) {
                    if !parser.exists() {
                        return Err(parser.expect_else());
                    }

                    args.push(parse_expr(parser, true)?);

                    parser.expect_begin();
                    parser.expect_kind(TokenKind::Comma);
                }

                item = Expr {
                    span: item.span.to(parser.span()),
                    kind: ExprKind::Call(Box::new(item), args),
                }
            } else if parser.expect_kind(TokenKind::OpenBracket) {
                let expr = parse_expr(parser, true)?;

                parser.expect_begin();
                if !parser.expect_kind(TokenKind::CloseBracket) {
                    return Err(parser.expect_else());
                }

                item = Expr {
                    span: item.span.to(parser.span()),
                    kind: ExprKind::Index(Box::new(item), Box::new(expr)),
                }
            } else if parser.expect_kind(TokenKind::Dot) {
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
}

fn parse_expr_object(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    if let Some(id) = parser.expect_id() {
        let name = SymbolWithSpan {
            symbol: id,
            span: parser.span(),
        };

        parser.expect_begin();
        if parser.expect_kind(TokenKind::OpenBrace) {
            let inner_object = parse_expr_inner_object(parser)?;

            Ok(Expr {
                span: name.span.to(inner_object.span),
                kind: ExprKind::Object(Object {
                    span: name.span.to(inner_object.span),
                    ty: TyUserDef {
                        span: name.span,
                        module: None,
                        id: name,
                    },
                    fields: inner_object.fields,
                }),
            })
        } else if parser.expect_kind(TokenKind::Dot) {
            parser.expect_begin();
            if let Some(next_id) = parser.expect_id() {
                let next_name = SymbolWithSpan {
                    symbol: next_id,
                    span: parser.span(),
                };

                parser.expect_begin();
                if parser.expect_kind(TokenKind::OpenBrace) {
                    let inner_object = parse_expr_inner_object(parser)?;

                    Ok(Expr {
                        span: name.span.to(inner_object.span),
                        kind: ExprKind::Object(Object {
                            span: name.span.to(inner_object.span),
                            ty: TyUserDef {
                                span: name.span.to(next_name.span),
                                module: Some(name),
                                id: next_name,
                            },
                            fields: inner_object.fields,
                        }),
                    })
                } else {
                    Ok(Expr {
                        span: name.span.to(next_name.span),
                        kind: ExprKind::Member(
                            Box::new(Expr {
                                span: name.span,
                                kind: ExprKind::Id(name),
                            }),
                            next_name,
                        ),
                    })
                }
            } else {
                return Err(parser.expect_else());
            }
        } else {
            Ok(Expr {
                span: name.span,
                kind: ExprKind::Id(name),
            })
        }
    } else {
        parse_expr_item(parser)
    }
}

fn parse_expr_inner_object(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<InnerObject, (String, Span)> {
    let span = parser.span();
    let mut fields = vec![];

    while !parser.expect_kind(TokenKind::CloseBrace) {
        if !parser.exists() {
            return Err(parser.expect_else());
        }

        parser.expect_begin();
        let name = if let Some(id) = parser.expect_id() {
            SymbolWithSpan {
                symbol: id,
                span: parser.span(),
            }
        } else {
            return Err(parser.expect_else());
        };

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::Colon) {
            return Err(parser.expect_else());
        }

        let kind;
        let kind_span;

        parser.expect_begin();
        if parser.expect_kind(TokenKind::OpenBrace) {
            let inner_object = parse_expr_inner_object(parser)?;

            kind_span = inner_object.span;
            kind = ObjectFieldKind::InnerObject(inner_object);
        } else {
            let expr = parse_expr(parser, true)?;

            kind_span = expr.span;
            kind = ObjectFieldKind::Expr(expr);
        }

        fields.push(ObjectField {
            name,
            kind,
            span: name.span.to(kind_span),
        });

        parser.expect_begin();
        parser.expect_kind(TokenKind::Comma);
    }

    Ok(InnerObject {
        fields,
        span: span.to(parser.span()),
    })
}

fn parse_expr_item(
    parser: &mut Parser<impl Iterator<Item = Token>>,
) -> Result<Expr, (String, Span)> {
    if parser.expect_kind(TokenKind::OpenParen) {
        let expr = parse_expr(parser, true)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::CloseParen) {
            return Err(parser.expect_else());
        }

        Ok(expr)
    } else if parser.expect_kind(TokenKind::OpenBracket) {
        let span = parser.span();
        let expr = parse_expr(parser, true)?;

        parser.expect_begin();
        if !parser.expect_kind(TokenKind::CloseBracket) {
            return Err(parser.expect_else());
        }

        Ok(Expr {
            kind: ExprKind::Deref(Box::new(expr)),
            span: span.to(parser.span()),
        })
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

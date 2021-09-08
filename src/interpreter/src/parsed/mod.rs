use ast::{
    Block, ElseKind, Expr, ExprKind, Fn, ForKind, If, InnerStruct, LetKind, Stmt, StmtKind, Struct,
    StructFieldKind, SymbolWithSpan, TopLevelKind, Ty, TyKind, Vis, VisKind,
};
use high_lexer::TokenLiteral;
use parser::parse;
use span::{Source, SourceMap, Span};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Context {
    source_map: SourceMap,
    modules: HashMap<PathBuf, ParsedModule>,
    scopes: Vec<ParsedScope>,
    structs: Vec<ParsedStruct>,
    functions: Vec<ParsedFunction>,
}

impl Context {
    pub fn scope(&self, index: usize) -> &ParsedScope {
        &self.scopes[index]
    }

    pub fn r#struct(&self, index: usize) -> &ParsedStruct {
        &self.structs[index]
    }

    pub fn function(&self, index: usize) -> &ParsedFunction {
        &self.functions[index]
    }

    pub fn update_function_body(&mut self, index: usize, body: ParsedStmtBlock) {
        self.functions[index].body = body;
    }

    pub fn register_scope(&mut self, parent: Option<usize>, kind: ParsedScopeKind) -> usize {
        self.scopes.push(ParsedScope { parent, kind });
        self.scopes.len() - 1
    }

    pub fn register_struct(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        scope: Option<usize>,
        item: Struct,
    ) -> (SymbolWithSpan, usize) {
        let name = item.name;
        let parsed = ParsedStruct {
            name: ParsedStructNameKind::Named(item.name),
            fields: item
                .fields
                .into_iter()
                .map(|field| ParsedStructField {
                    name: field.name,
                    ty: match field.kind {
                        StructFieldKind::Plain(ty) => self.parse_ty(top_level_structs, scope, ty),
                        StructFieldKind::Struct(item) => ParsedTy {
                            span: item.span,
                            kind: ParsedTyKind::Struct(self.register_inner_struct(
                                top_level_structs,
                                scope,
                                item,
                            )),
                        },
                    },
                    span: field.span,
                })
                .collect(),
            span: item.span,
        };

        self.structs.push(parsed);
        (name, self.structs.len() - 1)
    }

    fn register_inner_struct(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        scope: Option<usize>,
        item: InnerStruct,
    ) -> usize {
        let parsed = ParsedStruct {
            name: ParsedStructNameKind::Inner,
            fields: item
                .fields
                .into_iter()
                .map(|field| ParsedStructField {
                    name: field.name,
                    ty: match field.kind {
                        StructFieldKind::Plain(ty) => self.parse_ty(top_level_structs, scope, ty),
                        StructFieldKind::Struct(item) => ParsedTy {
                            span: item.span,
                            kind: ParsedTyKind::Struct(self.register_inner_struct(
                                top_level_structs,
                                scope,
                                item,
                            )),
                        },
                    },
                    span: field.span,
                })
                .collect(),
            span: item.span,
        };

        self.structs.push(parsed);
        self.structs.len() - 1
    }

    /// Parse a type or lookup a previously parsed struct.
    fn parse_ty(
        &self,
        top_level_structs: &[ParsedTopLevelStruct],
        scope: Option<usize>,
        ty: Ty,
    ) -> ParsedTy {
        ParsedTy {
            kind: match ty.kind {
                TyKind::Bool => ParsedTyKind::Bool,
                TyKind::Byte => ParsedTyKind::Byte,
                TyKind::Char => ParsedTyKind::Char,
                TyKind::I64 => ParsedTyKind::I64,
                TyKind::U64 => ParsedTyKind::U64,
                TyKind::Isize => ParsedTyKind::Isize,
                TyKind::Usize => ParsedTyKind::Usize,
                TyKind::F64 => ParsedTyKind::F64,
                TyKind::Str => ParsedTyKind::Str,
                TyKind::Cptr(ty) => {
                    ParsedTyKind::Cptr(Box::new(self.parse_ty(top_level_structs, scope, *ty)))
                }
                TyKind::Mptr(ty) => {
                    ParsedTyKind::Mptr(Box::new(self.parse_ty(top_level_structs, scope, *ty)))
                }
                TyKind::Struct(item) => {
                    if let Some(scope) = scope {
                        let mut scope = self.scope(scope);

                        loop {
                            if let ParsedScopeKind::Struct(target) = &scope.kind {
                                if target.name.symbol == item {
                                    return ParsedTy {
                                        kind: ParsedTyKind::Struct(target.def),
                                        span: ty.span,
                                    };
                                }
                            }

                            scope = if let Some(parent) = scope.parent {
                                self.scope(parent)
                            } else {
                                break;
                            };
                        }
                    }

                    if let Some(target) = top_level_structs
                        .iter()
                        .rev()
                        .find(|&target| target.name.symbol == item)
                    {
                        return ParsedTy {
                            kind: ParsedTyKind::Struct(target.def),
                            span: ty.span,
                        };
                    }

                    panic!("no struct '{}' found", item);
                }
            },
            span: ty.span,
        }
    }

    // NOTE: This function will not fill the stmt field of the parsed function. Caller must fill it.
    pub fn register_function(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        scope: Option<usize>,
        item: Fn,
    ) -> (SymbolWithSpan, usize, Block) {
        let name = item.name;
        let def = self.functions.len();

        self.functions.push(ParsedFunction {
            name: item.name,
            params: item
                .params
                .into_iter()
                .map(|param| ParsedFunctionParam {
                    name: param.name,
                    ty: self.parse_ty(top_level_structs, scope, param.ty),
                    span: param.span,
                })
                .collect(),
            return_ty: item
                .return_ty
                .map(|ty| self.parse_ty(top_level_structs, scope, ty)),
            body: ParsedStmtBlock {
                stmts: vec![],
                span: item.body.span,
            },
            span: item.span,
        });

        (name, def, item.body)
    }

    pub fn parse_stmt(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        top_level_functions: &[ParsedTopLevelFunction],
        mut scope: usize,
        stmt: Stmt,
    ) -> (ParsedStmt, usize) {
        (
            ParsedStmt {
                kind: match stmt.kind {
                    StmtKind::Struct(item) => {
                        let (name, def) =
                            self.register_struct(top_level_structs, Some(scope), item);

                        scope = self.register_scope(
                            Some(scope),
                            ParsedScopeKind::Struct(ParsedScopeStruct { name, def }),
                        );

                        ParsedStmtKind::Struct(ParsedStmtStruct { name, def })
                    }
                    StmtKind::Fn(item) => {
                        let (name, def, body) =
                            self.register_function(top_level_structs, Some(scope), item);

                        scope = self.register_scope(
                            Some(scope),
                            ParsedScopeKind::Function(ParsedScopeFunction { name, def }),
                        );

                        let body =
                            self.parse_block(top_level_structs, top_level_functions, scope, body);
                        self.update_function_body(def, body);

                        ParsedStmtKind::Fn(ParsedStmtFn { name, def })
                    }
                    StmtKind::Let(item) => ParsedStmtKind::Let(ParsedStmtLet {
                        name: item.name,
                        kind: match item.kind {
                            LetKind::Ty(ty) => ParsedStmtLetKind::Ty(self.parse_ty(
                                top_level_structs,
                                Some(scope),
                                ty,
                            )),
                            LetKind::Expr(expr) => ParsedStmtLetKind::Expr(self.parse_expr(
                                top_level_structs,
                                top_level_functions,
                                scope,
                                expr,
                            )),
                            LetKind::TyExpr(ty, expr) => ParsedStmtLetKind::TyExpr(
                                self.parse_ty(top_level_structs, Some(scope), ty),
                                self.parse_expr(
                                    top_level_structs,
                                    top_level_functions,
                                    scope,
                                    expr,
                                ),
                            ),
                        },
                        span: item.span,
                    }),
                    StmtKind::If(item) => ParsedStmtKind::If(self.parse_if(
                        top_level_structs,
                        top_level_functions,
                        scope,
                        item,
                    )),
                    StmtKind::For(item) => ParsedStmtKind::For(ParsedStmtFor {
                        kind: match item.kind {
                            ForKind::Loop => ParsedStmtForKind::Loop,
                            ForKind::While(cond) => ParsedStmtForKind::While(self.parse_expr(
                                top_level_structs,
                                top_level_functions,
                                scope,
                                cond,
                            )),
                            ForKind::ForIn(local, expr) => {
                                ParsedStmtForKind::ForIn(ParsedStmtForIn {
                                    span: local.span.to(expr.span),
                                    local,
                                    expr: self.parse_expr(
                                        top_level_structs,
                                        top_level_functions,
                                        scope,
                                        expr,
                                    ),
                                })
                            }
                        },
                        body: self.parse_block(
                            top_level_structs,
                            top_level_functions,
                            scope,
                            item.body,
                        ),
                        span: item.span,
                    }),
                    StmtKind::Block(item) => ParsedStmtKind::Block(self.parse_block(
                        top_level_structs,
                        top_level_functions,
                        scope,
                        item,
                    )),
                    StmtKind::Break(item) => {
                        ParsedStmtKind::Break(ParsedStmtBreak { span: item.span })
                    }
                    StmtKind::Continue(item) => {
                        ParsedStmtKind::Continue(ParsedStmtContinue { span: item.span })
                    }
                    StmtKind::Return(item) => ParsedStmtKind::Return(ParsedStmtReturn {
                        expr: item.expr.map(|expr| {
                            self.parse_expr(top_level_structs, top_level_functions, scope, expr)
                        }),
                        span: item.span,
                    }),
                    StmtKind::Expr(item) => ParsedStmtKind::Expr(self.parse_expr(
                        top_level_structs,
                        top_level_functions,
                        scope,
                        item,
                    )),
                },
                span: stmt.span,
            },
            scope,
        )
    }

    pub fn parse_if(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        top_level_functions: &[ParsedTopLevelFunction],
        scope: usize,
        item: If,
    ) -> ParsedStmtIf {
        let cond_scope = self.register_scope(Some(scope), ParsedScopeKind::Block);

        ParsedStmtIf {
            cond: self.parse_expr(
                top_level_structs,
                top_level_functions,
                cond_scope,
                item.cond,
            ),
            then_body: self.parse_block(
                top_level_structs,
                top_level_functions,
                scope,
                item.then_body,
            ),
            else_kind: item.else_kind.map(|else_kind| match *else_kind {
                ElseKind::Else(item) => ParsedStmtElseKind::Else(self.parse_block(
                    top_level_structs,
                    top_level_functions,
                    scope,
                    item,
                )),
                ElseKind::ElseIf(item) => ParsedStmtElseKind::ElseIf(Box::new(self.parse_if(
                    top_level_structs,
                    top_level_functions,
                    scope,
                    item,
                ))),
            }),
            span: item.span,
        }
    }

    pub fn parse_block(
        &mut self,
        top_level_structs: &[ParsedTopLevelStruct],
        top_level_functions: &[ParsedTopLevelFunction],
        mut scope: usize,
        item: Block,
    ) -> ParsedStmtBlock {
        scope = self.register_scope(Some(scope), ParsedScopeKind::Block);

        ParsedStmtBlock {
            stmts: item
                .stmts
                .into_iter()
                .map(|stmt| {
                    let (stmt, new_scope) =
                        self.parse_stmt(top_level_structs, top_level_functions, scope, stmt);
                    scope = new_scope;
                    stmt
                })
                .collect(),
            span: item.span,
        }
    }

    pub fn parse_expr(
        &self,
        top_level_structs: &[ParsedTopLevelStruct],
        top_level_functions: &[ParsedTopLevelFunction],
        scope: usize,
        item: Expr,
    ) -> ParsedExpr {
        ParsedExpr {
            kind: match item.kind {
                ExprKind::Assign(lhs, rhs) => todo!(),
                ExprKind::AssignAdd(lhs, rhs) => todo!(),
                ExprKind::AssignSub(lhs, rhs) => todo!(),
                ExprKind::AssignMul(lhs, rhs) => todo!(),
                ExprKind::AssignDiv(lhs, rhs) => todo!(),
                ExprKind::AssignMod(lhs, rhs) => todo!(),
                ExprKind::AssignShl(lhs, rhs) => todo!(),
                ExprKind::AssignShr(lhs, rhs) => todo!(),
                ExprKind::AssignBitOr(lhs, rhs) => todo!(),
                ExprKind::AssignBitAnd(lhs, rhs) => todo!(),
                ExprKind::AssignBitXor(lhs, rhs) => todo!(),
                ExprKind::AssignBitNot(lhs, rhs) => todo!(),
                ExprKind::Rng(lhs, rhs) => todo!(),
                ExprKind::RngInclusive(lhs, rhs) => todo!(),
                ExprKind::Eq(lhs, rhs) => todo!(),
                ExprKind::Ne(lhs, rhs) => todo!(),
                ExprKind::Lt(lhs, rhs) => todo!(),
                ExprKind::Gt(lhs, rhs) => todo!(),
                ExprKind::Le(lhs, rhs) => todo!(),
                ExprKind::Ge(lhs, rhs) => todo!(),
                ExprKind::Neg(lhs) => todo!(),
                ExprKind::Add(lhs, rhs) => todo!(),
                ExprKind::Sub(lhs, rhs) => todo!(),
                ExprKind::Mul(lhs, rhs) => todo!(),
                ExprKind::Div(lhs, rhs) => todo!(),
                ExprKind::Mod(lhs, rhs) => todo!(),
                ExprKind::Shl(lhs, rhs) => todo!(),
                ExprKind::Shr(lhs, rhs) => todo!(),
                ExprKind::BitOr(lhs, rhs) => todo!(),
                ExprKind::BitAnd(lhs, rhs) => todo!(),
                ExprKind::BitXor(lhs, rhs) => todo!(),
                ExprKind::BitNot(lhs) => todo!(),
                ExprKind::LogOr(lhs, rhs) => todo!(),
                ExprKind::LogAnd(lhs, rhs) => todo!(),
                ExprKind::LogNot(lhs) => todo!(),
                ExprKind::Cast(lhs, rhs) => todo!(),
                ExprKind::Object(lhs) => todo!(),
                ExprKind::Call(lhs, rhs) => todo!(),
                ExprKind::Index(lhs, rhs) => todo!(),
                ExprKind::Member(lhs, rhs) => todo!(),
                ExprKind::Id(lhs) => todo!(),
                ExprKind::Literal(lhs) => todo!(),
            },
            span: item.span,
        }
    }
}

#[derive(Debug)]
pub struct ParsedModule {
    pub source: Arc<Source>,
    pub uses: Vec<PathBuf>,
    pub top_level_structs: Vec<ParsedTopLevelStruct>,
    pub top_level_functions: Vec<ParsedTopLevelFunction>,
}

impl ParsedModule {
    pub fn parse(ctx: &mut Context, source: Arc<Source>) -> Self {
        let module = match parse(source.clone()) {
            Ok(module) => module,
            Err((msg, span)) => {
                let line_col = source.find_line_col(span.low());
                panic!(
                    "{}: {:?}\n{}",
                    msg,
                    span,
                    source.slice_line(line_col.line())
                );
            }
        };

        let mut uses = vec![];
        let mut structs = Vec::new();
        let mut functions = Vec::new();

        for top_level in module.top_levels {
            match top_level.kind {
                TopLevelKind::Use(item) => {
                    uses.push(
                        item.segments
                            .iter()
                            .map(|segment| segment.symbol.as_str())
                            .collect::<PathBuf>(),
                    );
                }
                TopLevelKind::Struct(item) => {
                    let (name, def) = ctx.register_struct(&structs, None, item.item);
                    structs.push(ParsedTopLevelStruct {
                        name,
                        vis: item.vis.map(|vis| vis.into()),
                        def,
                    });
                }
                TopLevelKind::Fn(item) => {
                    let (name, def, body) = ctx.register_function(&structs, None, item.item);
                    functions.push(ParsedTopLevelFunction {
                        name,
                        vis: item.vis.map(|vis| vis.into()),
                        def,
                    });

                    let scope = ctx.register_scope(
                        None,
                        ParsedScopeKind::Function(ParsedScopeFunction { name, def }),
                    );
                    let body = ctx.parse_block(&structs, &functions, scope, body);
                    ctx.update_function_body(def, body);
                }
            }
        }

        Self {
            source,
            uses,
            top_level_structs: structs,
            top_level_functions: functions,
        }
    }
}

#[derive(Debug)]
pub struct ParsedVis {
    pub kind: ParsedVisKind,
    pub span: Span,
}

impl From<Vis> for ParsedVis {
    fn from(vis: Vis) -> Self {
        Self {
            kind: ParsedVisKind::from(vis.kind),
            span: vis.span,
        }
    }
}

#[derive(Debug)]
pub enum ParsedVisKind {
    Pub,
}

impl From<VisKind> for ParsedVisKind {
    fn from(kind: VisKind) -> Self {
        match kind {
            VisKind::Pub => Self::Pub,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTy {
    pub kind: ParsedTyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ParsedTyKind {
    Bool,
    Byte,
    Char,
    I64,
    U64,
    Isize,
    Usize,
    F64,
    Str,
    Cptr(Box<ParsedTy>),
    Mptr(Box<ParsedTy>),
    Struct(usize),
}

#[derive(Debug, Clone)]
pub struct ParsedScope {
    pub parent: Option<usize>,
    pub kind: ParsedScopeKind,
}

#[derive(Debug, Clone)]
pub enum ParsedScopeKind {
    Block,
    Loop,
    Struct(ParsedScopeStruct),
    Function(ParsedScopeFunction),
}

#[derive(Debug, Clone)]
pub struct ParsedScopeStruct {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug, Clone)]
pub struct ParsedScopeFunction {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStruct {
    pub name: ParsedStructNameKind,
    pub fields: Vec<ParsedStructField>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStructNameKind {
    Inner,
    Named(SymbolWithSpan),
}

#[derive(Debug)]
pub struct ParsedStructField {
    pub name: SymbolWithSpan,
    pub ty: ParsedTy,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedFunction {
    pub name: SymbolWithSpan,
    pub params: Vec<ParsedFunctionParam>,
    pub return_ty: Option<ParsedTy>,
    pub body: ParsedStmtBlock,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedFunctionParam {
    pub name: SymbolWithSpan,
    pub ty: ParsedTy,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedTopLevelStruct {
    pub name: SymbolWithSpan,
    pub vis: Option<ParsedVis>,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedTopLevelFunction {
    pub name: SymbolWithSpan,
    pub vis: Option<ParsedVis>,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStmt {
    pub kind: ParsedStmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtKind {
    Struct(ParsedStmtStruct),
    Fn(ParsedStmtFn),
    Let(ParsedStmtLet),
    If(ParsedStmtIf),
    For(ParsedStmtFor),
    Block(ParsedStmtBlock),
    Break(ParsedStmtBreak),
    Continue(ParsedStmtContinue),
    Return(ParsedStmtReturn),
    Expr(ParsedExpr),
}

#[derive(Debug)]
pub struct ParsedStmtStruct {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStmtFn {
    pub name: SymbolWithSpan,
    pub def: usize,
}

#[derive(Debug)]
pub struct ParsedStmtLet {
    pub name: SymbolWithSpan,
    pub kind: ParsedStmtLetKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtLetKind {
    Ty(ParsedTy),
    Expr(ParsedExpr),
    TyExpr(ParsedTy, ParsedExpr),
}

#[derive(Debug)]
pub struct ParsedStmtIf {
    pub cond: ParsedExpr,
    pub then_body: ParsedStmtBlock,
    pub else_kind: Option<ParsedStmtElseKind>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtElseKind {
    Else(ParsedStmtBlock),
    ElseIf(Box<ParsedStmtIf>),
}

#[derive(Debug)]
pub struct ParsedStmtFor {
    pub kind: ParsedStmtForKind,
    pub body: ParsedStmtBlock,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedStmtForKind {
    Loop,
    While(ParsedExpr),
    ForIn(ParsedStmtForIn),
}

#[derive(Debug)]
pub struct ParsedStmtForIn {
    pub local: SymbolWithSpan,
    pub expr: ParsedExpr,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtBlock {
    pub stmts: Vec<ParsedStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtBreak {
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtContinue {
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedStmtReturn {
    pub expr: Option<ParsedExpr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExpr {
    pub kind: ParsedExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedExprKind {
    Assign(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignAdd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignSub(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignMul(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignDiv(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignMod(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignShl(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignShr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitXor(Box<ParsedExprKind>, Box<ParsedExprKind>),
    AssignBitNot(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Rng(Box<ParsedExprKind>, Box<ParsedExprKind>),
    RngInclusive(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Eq(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Ne(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Lt(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Gt(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Le(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Ge(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Neg(Box<ParsedExprKind>),
    Add(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Sub(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Mul(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Div(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Mod(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Shl(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Shr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitXor(Box<ParsedExprKind>, Box<ParsedExprKind>),
    BitNot(Box<ParsedExprKind>),
    LogOr(Box<ParsedExprKind>, Box<ParsedExprKind>),
    LogAnd(Box<ParsedExprKind>, Box<ParsedExprKind>),
    LogNot(Box<ParsedExprKind>),
    Cast(Box<ParsedExprKind>, ParsedTy),
    Call(Box<ParsedExprKind>, Vec<ParsedExprKind>),
    Index(Box<ParsedExprKind>, Box<ParsedExprKind>),
    Member(Box<ParsedExprKind>, SymbolWithSpan),
    Object(ParsedExprObject),
    Id(SymbolWithSpan),
    Literal(ParsedExprLiteral),
}

#[derive(Debug)]
pub struct ParsedExprObject {
    pub name: SymbolWithSpan,
    pub fields: Vec<ParsedExprObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExprObjectField {
    pub name: SymbolWithSpan,
    pub kind: ParsedExprObjectFieldKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ParsedExprObjectFieldKind {
    Expr(ParsedExpr),
    InnerObject(ParsedExprObjectFieldInnerObject),
}

#[derive(Debug)]
pub struct ParsedExprObjectFieldInnerObject {
    pub fields: Vec<ParsedExprObjectField>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsedExprLiteral {
    pub literal: TokenLiteral,
    pub span: Span,
}

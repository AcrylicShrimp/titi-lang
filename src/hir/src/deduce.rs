use crate::*;
use high_lexer::*;

// TODO: Define a function to determine what function to call based on its name and type of arguments.
// There are two types of name of functions:
// 1. Id
// 2. Operator
// If we have an Id, we can look it up in the symbol table. Since no overloading is allowed for functions that have Id names, we can just look up the function in the symbol table.
// If we have an Operator, we need to look up the function by its name and type of arguments. The types are strict and no implicit conversions are allowed.

pub fn deduce_expr_ty(
    module: usize,
    modules: &[ResolvedModule],
    exprs: &[GlobalExpr],
    scopes: &[GlobalScope],
    structs: &[GlobalStruct],
    inner_structs: &[GlobalInnerStruct],
    fns: &[GlobalFn],
    fn_headers: &[GlobalFnHeader],
    types: &mut Vec<ResolvedType>,
    expr_def: usize,
) -> usize {
    let expr = &exprs[expr_def];

    // TODO: Generalize the types so that the types are not known at this moment.

    match &expr.kind {
        GlobalExprKind::Assign(..)
        | GlobalExprKind::AssignAdd(..)
        | GlobalExprKind::AssignSub(..)
        | GlobalExprKind::AssignMul(..)
        | GlobalExprKind::AssignDiv(..)
        | GlobalExprKind::AssignMod(..)
        | GlobalExprKind::AssignShl(..)
        | GlobalExprKind::AssignShr(..)
        | GlobalExprKind::AssignBitOr(..)
        | GlobalExprKind::AssignBitAnd(..)
        | GlobalExprKind::AssignBitXor(..)
        | GlobalExprKind::AssignBitNot(..) => {
            // There's no type for assignments.
            let def = types.len();
            types.push(ResolvedType {
                kind: ResolvedTypeKind::None,
                addressable: false,
                assignable: false,
                ref_kind: None,
                span: expr.span,
            });
            def
        }
        GlobalExprKind::Rng(..) => {
            let def = types.len();
            types.push(ResolvedType {
                kind: ResolvedTypeKind::Range(expr_def),
                addressable: false,
                assignable: false,
                ref_kind: None,
                span: expr.span,
            });
            def
        }
        GlobalExprKind::RngInclusive(..) => {
            let def = types.len();
            types.push(ResolvedType {
                kind: ResolvedTypeKind::RangeInclusive(expr_def),
                addressable: false,
                assignable: false,
                ref_kind: None,
                span: expr.span,
            });
            def
        }
        GlobalExprKind::Eq(..)
        | GlobalExprKind::Ne(..)
        | GlobalExprKind::Lt(..)
        | GlobalExprKind::Gt(..)
        | GlobalExprKind::Le(..)
        | GlobalExprKind::Ge(..) => {
            let def = types.len();
            types.push(ResolvedType {
                kind: ResolvedTypeKind::Bool,
                addressable: false,
                assignable: false,
                ref_kind: None,
                span: expr.span,
            });
            def
        }
        GlobalExprKind::Neg(_) => todo!(),
        GlobalExprKind::Add(_, _) => todo!(),
        GlobalExprKind::Sub(_, _) => todo!(),
        GlobalExprKind::Mul(_, _) => todo!(),
        GlobalExprKind::Div(_, _) => todo!(),
        GlobalExprKind::Mod(_, _) => todo!(),
        GlobalExprKind::Shl(_, _) => todo!(),
        GlobalExprKind::Shr(_, _) => todo!(),
        GlobalExprKind::BitOr(_, _) => todo!(),
        GlobalExprKind::BitAnd(_, _) => todo!(),
        GlobalExprKind::BitXor(_, _) => todo!(),
        GlobalExprKind::BitNot(_) => todo!(),
        GlobalExprKind::LogOr(_, _) => todo!(),
        GlobalExprKind::LogAnd(_, _) => todo!(),
        GlobalExprKind::LogNot(_) => todo!(),
        &GlobalExprKind::Cast(_, def) => def,
        GlobalExprKind::Object(_) => todo!(),
        GlobalExprKind::Call(lhs, ..) => {
            let lhs = match &types[*lhs].kind {
                ResolvedTypeKind::None => todo!(),
                ResolvedTypeKind::Bool => todo!(),
                ResolvedTypeKind::Byte => todo!(),
                ResolvedTypeKind::Char => todo!(),
                ResolvedTypeKind::I64 => todo!(),
                ResolvedTypeKind::U64 => todo!(),
                ResolvedTypeKind::Isize => todo!(),
                ResolvedTypeKind::Usize => todo!(),
                ResolvedTypeKind::F64 => todo!(),
                ResolvedTypeKind::Str => todo!(),
                ResolvedTypeKind::Cptr(_) => todo!(),
                ResolvedTypeKind::Mptr(_) => todo!(),
                ResolvedTypeKind::Range(_) => todo!(),
                ResolvedTypeKind::RangeInclusive(_) => todo!(),
                ResolvedTypeKind::Struct(_) => todo!(),
                ResolvedTypeKind::InnerStruct(_) => todo!(),
                ResolvedTypeKind::Fn(_) => todo!(),
                ResolvedTypeKind::FnHeader(_) => todo!(),
                ResolvedTypeKind::Let(_) => todo!(),
                ResolvedTypeKind::Literal(_) => todo!(),
            };
        }
        GlobalExprKind::Index(..) => todo!(), // We don't have types to deduce for indexing right now.
        &GlobalExprKind::Member(lhs, rhs) => {
            deduce_expr_member_ty(structs, inner_structs, types, lhs, rhs)
        }
        &GlobalExprKind::Id(lhs) => {
            let def = types.len();
            types.push(ResolvedType {
                // TODO: Emit a diagnostic if the id is not found instead of panicking.
                kind: lookup_id(modules, scopes, ScopeRef::Scope(expr.scope), lhs.symbol)
                    .expect("undefined id used"),
                span: lhs.span,
            });
            def
        }
        GlobalExprKind::Literal(lhs) => deduce_expr_literal(types, lhs),
    }
}

fn deduce_expr_member_ty(
    structs: &[GlobalStruct],
    inner_structs: &[GlobalInnerStruct],
    types: &mut Vec<ResolvedType>,
    lhs_def: usize,
    rhs: SymbolWithSpan,
) -> usize {
    match types[lhs_def].kind {
        ResolvedTypeKind::None
        | ResolvedTypeKind::Bool
        | ResolvedTypeKind::Byte
        | ResolvedTypeKind::Char
        | ResolvedTypeKind::I64
        | ResolvedTypeKind::U64
        | ResolvedTypeKind::Isize
        | ResolvedTypeKind::Usize
        | ResolvedTypeKind::F64
        | ResolvedTypeKind::Str
        | ResolvedTypeKind::Range(..)
        | ResolvedTypeKind::RangeInclusive(..)
        | ResolvedTypeKind::Fn(..)
        | ResolvedTypeKind::FnHeader(..)
        | ResolvedTypeKind::Let(..)
        | ResolvedTypeKind::Literal(..) => panic!("that type has no member"),
        ResolvedTypeKind::Cptr(lhs) => {
            deduce_expr_member_ty(structs, inner_structs, types, lhs, rhs)
        }
        ResolvedTypeKind::Mptr(lhs) => {
            deduce_expr_member_ty(structs, inner_structs, types, lhs, rhs)
        }
        ResolvedTypeKind::Struct(lhs) => {
            let lhs = &structs[lhs];
            if let Some(field) = lhs
                .fields
                .iter()
                .rev()
                .find(|&field| field.name.symbol == rhs.symbol)
            {
                let def = types.len();
                types.push(match field.kind {
                    GlobalStructFieldKind::Plain(def) => {
                        let ty = types[def].clone();
                        let ref_kind = types[lhs_def].ref_kind.clone();

                        ResolvedType {
                            kind: ty.kind.clone(),
                            addressable: true,
                            assignable: true,
                            ref_kind,
                            span: ty.span,
                        }
                    }
                    GlobalStructFieldKind::Struct(def) => ResolvedType {
                        kind: ResolvedTypeKind::InnerStruct(def),
                        addressable: true,
                        assignable: true,
                        ref_kind: None,
                        span: inner_structs[def].span,
                    },
                });
                def
            } else {
                panic!("that type has no member like that");
            }
        }
        ResolvedTypeKind::InnerStruct(lhs) => {
            let lhs = &inner_structs[lhs];
            if let Some(field) = lhs
                .fields
                .iter()
                .rev()
                .find(|&field| field.name.symbol == rhs.symbol)
            {
                let def = types.len();
                types.push(match field.kind {
                    GlobalStructFieldKind::Plain(def) => types[def].clone(),
                    GlobalStructFieldKind::Struct(def) => ResolvedType {
                        kind: ResolvedTypeKind::InnerStruct(def),
                        addressable: true,
                        assignable: true,
                        ref_kind: None,
                        span: inner_structs[def].span,
                    },
                });
                def
            } else {
                panic!("that type has no member like that");
            }
        }
    }
}

fn deduce_expr_literal(types: &mut Vec<ResolvedType>, lhs: &Literal) -> usize {
    // TODO: Emit a diagnostic if the suffix is not valid instead of panicking.
    let def = types.len();
    types.push(ResolvedType {
        kind: match lhs.lit.kind() {
            TokenLiteralKind::Bool => ResolvedTypeKind::Bool,
            TokenLiteralKind::IntegerBinary
            | TokenLiteralKind::IntegerOctal
            | TokenLiteralKind::IntegerHexadecimal => {
                if let Some(suffix) = lhs.lit.suffix() {
                    match suffix {
                        BYTE => ResolvedTypeKind::Byte,
                        CHAR => ResolvedTypeKind::Char,
                        I64 => ResolvedTypeKind::I64,
                        U64 => ResolvedTypeKind::U64,
                        ISIZE => ResolvedTypeKind::Isize,
                        USIZE => ResolvedTypeKind::Usize,
                        _ => panic!("the suffix is not allowed"),
                    }
                } else {
                    ResolvedTypeKind::I64
                }
            }
            TokenLiteralKind::IntegerDecimal => {
                if let Some(suffix) = lhs.lit.suffix() {
                    match suffix {
                        BYTE => ResolvedTypeKind::Byte,
                        CHAR => ResolvedTypeKind::Char,
                        I64 => ResolvedTypeKind::I64,
                        U64 => ResolvedTypeKind::U64,
                        ISIZE => ResolvedTypeKind::Isize,
                        USIZE => ResolvedTypeKind::Usize,
                        F64 => ResolvedTypeKind::F64,
                        _ => panic!("the suffix is not allowed"),
                    }
                } else {
                    ResolvedTypeKind::I64
                }
            }
            TokenLiteralKind::Float => {
                if let Some(suffix) = lhs.lit.suffix() {
                    match suffix {
                        F64 => ResolvedTypeKind::F64,
                        _ => panic!("the suffix is not allowed"),
                    }
                } else {
                    ResolvedTypeKind::F64
                }
            }
            TokenLiteralKind::SingleQuotedStr | TokenLiteralKind::DoubleQuotedStr => {
                if lhs.lit.suffix().is_some() {
                    panic!("the suffix is not allowed");
                }

                let def = types.len();
                types.push(ResolvedType {
                    kind: ResolvedTypeKind::Str,
                    addressable: false,
                    assignable: false,
                    ref_kind: None,
                    span: lhs.span,
                });
                ResolvedTypeKind::Cptr(def)
            }
        },
        addressable: true,
        assignable: false,
        ref_kind: None,
        span: lhs.span,
    });
    def
}

use crate::make_global::{
    make_global_expr, GlobalExpr, GlobalInnerObject, GlobalObject, GlobalObjectField,
    GlobalObjectFieldKind,
};
use crate::{ScopeRef, TyRefUserDef};
use ast::{InnerObject, Literal, Object, ObjectField, ObjectFieldKind};

pub fn make_global_object(
    global_exprs: &mut Vec<GlobalExpr>,
    global_lits: &mut Vec<Literal>,
    scope: ScopeRef,
    object: Object,
) -> GlobalObject {
    GlobalObject {
        ty: TyRefUserDef {
            scope,
            ty: object.ty,
        },
        fields: object
            .fields
            .into_iter()
            .map(|field| make_global_object_field(global_exprs, global_lits, scope, field))
            .collect(),
        span: object.span,
    }
}

fn make_global_object_field(
    global_exprs: &mut Vec<GlobalExpr>,
    global_lits: &mut Vec<Literal>,
    scope: ScopeRef,
    field: ObjectField,
) -> GlobalObjectField {
    GlobalObjectField {
        name: field.name,
        kind: match field.kind {
            ObjectFieldKind::Expr(expr) => GlobalObjectFieldKind::Expr(make_global_expr(
                global_exprs,
                global_lits,
                scope,
                expr,
            )),
            ObjectFieldKind::InnerObject(inner) => GlobalObjectFieldKind::InnerObject(
                make_global_inner_object(global_exprs, global_lits, scope, inner),
            ),
        },
        span: field.span,
    }
}

fn make_global_inner_object(
    global_exprs: &mut Vec<GlobalExpr>,
    global_lits: &mut Vec<Literal>,
    scope: ScopeRef,
    inner: InnerObject,
) -> GlobalInnerObject {
    GlobalInnerObject {
        fields: inner
            .fields
            .into_iter()
            .map(|field| make_global_object_field(global_exprs, global_lits, scope, field))
            .collect(),
        span: inner.span,
    }
}

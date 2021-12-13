use crate::make_global::{
    make_global_expr, GlobalExpr, GlobalInnerObject, GlobalObject, GlobalObjectField,
    GlobalObjectFieldKind,
};
use crate::{ScopeDef, ScopeRef, TyRefUserDef};
use ast::{InnerObject, Object, ObjectField, ObjectFieldKind};

pub fn make_global_object(
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    object: Object,
) -> GlobalObject {
    GlobalObject {
        ty: TyRefUserDef {
            scope: ScopeRef::Scope(scope),
            ty: object.ty,
        },
        fields: object
            .fields
            .into_iter()
            .map(|field| make_global_object_field(global_exprs, scope, field))
            .collect(),
        span: object.span,
    }
}

fn make_global_object_field(
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    field: ObjectField,
) -> GlobalObjectField {
    GlobalObjectField {
        name: field.name,
        kind: match field.kind {
            ObjectFieldKind::Expr(expr) => {
                GlobalObjectFieldKind::Expr(make_global_expr(global_exprs, scope, expr))
            }
            ObjectFieldKind::InnerObject(inner) => GlobalObjectFieldKind::InnerObject(
                make_global_inner_object(global_exprs, scope, inner),
            ),
        },
        span: field.span,
    }
}

fn make_global_inner_object(
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeDef,
    inner: InnerObject,
) -> GlobalInnerObject {
    GlobalInnerObject {
        fields: inner
            .fields
            .into_iter()
            .map(|field| make_global_object_field(global_exprs, scope, field))
            .collect(),
        span: inner.span,
    }
}

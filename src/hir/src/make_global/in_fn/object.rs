use super::{
    expr::make_in_fn_expr, InFnContext, InFnInnerObject, InFnObject, InFnObjectField,
    InFnObjectFieldKind, InFnScopeDef,
};
use crate::{ScopeRef, TyRefUserDef};
use ast::{InnerObject, Object, ObjectField, ObjectFieldKind};

pub fn make_in_fn_object(ctx: &mut InFnContext, scope: InFnScopeDef, object: Object) -> InFnObject {
    InFnObject {
        ty: TyRefUserDef {
            scope: ScopeRef {
                module: ctx.module,
                function: Some(ctx.function),
                scope: Some(scope),
            },
            ty: object.ty,
        },
        fields: object
            .fields
            .into_iter()
            .map(|field| make_in_fn_object_field(ctx, scope, field))
            .collect(),
        span: object.span,
    }
}

fn make_in_fn_object_field(
    ctx: &mut InFnContext,
    scope: InFnScopeDef,
    field: ObjectField,
) -> InFnObjectField {
    InFnObjectField {
        name: field.name,
        kind: match field.kind {
            ObjectFieldKind::Expr(expr) => {
                InFnObjectFieldKind::Expr(make_in_fn_expr(ctx, scope, expr))
            }
            ObjectFieldKind::InnerObject(inner) => {
                InFnObjectFieldKind::InnerObject(make_in_fn_inner_object(ctx, scope, inner))
            }
        },
        span: field.span,
    }
}

fn make_in_fn_inner_object(
    ctx: &mut InFnContext,
    scope: InFnScopeDef,
    inner: InnerObject,
) -> InFnInnerObject {
    InFnInnerObject {
        fields: inner
            .fields
            .into_iter()
            .map(|field| make_in_fn_object_field(ctx, scope, field))
            .collect(),
        span: inner.span,
    }
}

use super::in_fn::{make_in_fn_block, InFnContext, InFnScope, InFnScopeKind};
use crate::make_global::{GlobalFn, GlobalFnHeader, GlobalFnParam};
use crate::{FunctionDef, FunctionHeaderDef, GlobalContextWithoutModule, ScopeRef, TyRef};
use ast::{Fn, FnHeader, FnParam};

pub fn make_global_fn(
    global_ctx: &mut GlobalContextWithoutModule,
    scope: ScopeRef,
    function: Fn,
) -> FunctionDef {
    let mut ctx = InFnContext::new(scope.module);
    let root_scope = ctx.push_scope(InFnScope {
        module: scope.module,
        parent: None,
        kind: InFnScopeKind::Block,
    });
    let block = make_in_fn_block(global_ctx, &mut ctx, root_scope, function.body);
    let global_fn = GlobalFn {
        header: make_global_fn_header(global_ctx, scope, function.header),
        ctx,
        scope: root_scope,
        body: block,
        span: function.span,
    };
    let def = global_ctx.fns.len();
    global_ctx.fns.push(global_fn);
    FunctionDef(def)
}

pub fn make_global_fn_header(
    global_ctx: &mut GlobalContextWithoutModule,
    scope: ScopeRef,
    header: FnHeader,
) -> FunctionHeaderDef {
    let global_fn_header = GlobalFnHeader {
        name: header.name,
        params: header
            .params
            .into_iter()
            .map(|param| make_global_fn_param(scope, param))
            .collect(),
        return_ty: header.return_ty.map(|ty| TyRef { scope, ty }),
        span: header.span,
    };
    let def = global_ctx.fn_headers.len();
    global_ctx.fn_headers.push(global_fn_header);
    FunctionHeaderDef(def)
}

fn make_global_fn_param(scope: ScopeRef, param: FnParam) -> GlobalFnParam {
    GlobalFnParam {
        name: param.name,
        ty: TyRef {
            scope,
            ty: param.ty,
        },
        span: param.span,
    }
}

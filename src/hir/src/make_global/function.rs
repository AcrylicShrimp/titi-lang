use crate::make_global::{GlobalFn, GlobalFnHeader};
use crate::{FunctionDef, FunctionHeaderDef, ScopeRef, TyRef};
use ast::{Fn, FnHeader, FnParam};

use super::GlobalFnParam;

pub fn make_global_fn(
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    scope: ScopeRef,
    function: Fn,
) -> FunctionDef {
    let global_fn = GlobalFn {
        header: make_global_fn_header(global_fn_headers, scope, function.header),
        body: todo!(),
        span: function.span,
    };
    let def = global_fns.len();
    global_fns.push(global_fn);
    FunctionDef(def)
}

pub fn make_global_fn_header(
    global_fn_headers: &mut Vec<GlobalFnHeader>,
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
    let def = global_fn_headers.len();
    global_fn_headers.push(global_fn_header);
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

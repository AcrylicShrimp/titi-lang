use crate::make_global::{
    GlobalExpr, GlobalFn, GlobalFnHeader, GlobalFnParam, GlobalInnerStruct, GlobalScope,
    GlobalStmt, GlobalStruct,
};
use crate::{FunctionDef, FunctionHeaderDef, ScopeRef, TyRef};
use ast::{Fn, FnHeader, FnParam};

use super::make_global_stmt_block;

pub fn make_global_fn(
    global_scopes: &mut Vec<GlobalScope>,
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    global_fns: &mut Vec<GlobalFn>,
    global_fn_headers: &mut Vec<GlobalFnHeader>,
    global_stmts: &mut Vec<GlobalStmt>,
    global_exprs: &mut Vec<GlobalExpr>,
    scope: ScopeRef,
    function: Fn,
) -> FunctionDef {
    let global_fn = GlobalFn {
        header: make_global_fn_header(global_fn_headers, scope, function.header),
        body: make_global_stmt_block(
            global_scopes,
            global_structs,
            global_inner_structs,
            global_fns,
            global_fn_headers,
            global_stmts,
            global_exprs,
            scope,
            function.body,
        ),
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

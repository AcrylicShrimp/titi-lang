use crate::{
    resolve_ty, BinaryOpTyMap, MirContext, MirFn, MirFnHeader, MirFunctionDef,
    MirFunctionHeaderDef, MirInnerStruct, MirInnerStructDef, MirInnerStructField, MirScope,
    MirScopeDef, MirStmt, MirStruct, MirStructDef, MirStructField, MirStructFieldKind,
};
use hir::{
    GlobalContext, GlobalFn, GlobalFnHeader, GlobalInnerStruct, GlobalStruct,
    GlobalStructFieldKind, InFnContext, InFnStmt, InFnStmtKind,
};

pub fn transform(hir: GlobalContext) -> MirContext {
    let mut mir = MirContext::default();
    let mut binary_op_map = BinaryOpTyMap::new();

    for item in &hir.structs {
        transform_struct(&hir, &binary_op_map, item, &mut mir);
    }

    for item in &hir.fns {
        transform_fn(&hir, &binary_op_map, item, &mut mir);
    }

    for item in &hir.fn_headers {
        transform_fn_header(&hir, &binary_op_map, item, &mut mir);
    }

    mir
}

fn transform_struct(
    global_ctx: &GlobalContext,
    binary_op_map: &BinaryOpTyMap,
    item: &GlobalStruct,
    mir_ctx: &mut MirContext,
) -> MirStructDef {
    let mir_item = MirStruct {
        name: item.name.clone(),
        fields: item
            .fields
            .iter()
            .map(|field| MirStructField {
                name: field.name.clone(),
                kind: match &field.kind {
                    GlobalStructFieldKind::Plain(ty) => MirStructFieldKind::Plain(
                        resolve_ty(
                            global_ctx,
                            ty.scope.function.map(|def| &global_ctx.fns[def.0].ctx),
                            binary_op_map,
                            ty,
                        )
                        .unwrap(),
                    ),
                    &GlobalStructFieldKind::Struct(def) => {
                        MirStructFieldKind::Struct(transform_inner_struct(
                            global_ctx,
                            binary_op_map,
                            &global_ctx.inner_structs[def.0],
                            mir_ctx,
                        ))
                    }
                },
                span: field.span,
            })
            .collect(),
        span: item.span,
    };
    mir_ctx.push_struct(mir_item)
}

fn transform_inner_struct(
    global_ctx: &GlobalContext,
    binary_op_map: &BinaryOpTyMap,
    item: &GlobalInnerStruct,
    mir_ctx: &mut MirContext,
) -> MirInnerStructDef {
    let mir_item = MirInnerStruct {
        fields: item
            .fields
            .iter()
            .map(|field| MirInnerStructField {
                name: field.name.clone(),
                kind: match &field.kind {
                    GlobalStructFieldKind::Plain(ty) => MirStructFieldKind::Plain(
                        resolve_ty(
                            global_ctx,
                            ty.scope.function.map(|def| &global_ctx.fns[def.0].ctx),
                            binary_op_map,
                            ty,
                        )
                        .unwrap(),
                    ),
                    &GlobalStructFieldKind::Struct(def) => {
                        MirStructFieldKind::Struct(transform_inner_struct(
                            global_ctx,
                            binary_op_map,
                            &global_ctx.inner_structs[def.0],
                            mir_ctx,
                        ))
                    }
                },
                span: field.span,
            })
            .collect(),
        span: item.span,
    };
    mir_ctx.push_inner_struct(mir_item)
}

fn transform_fn(
    global_ctx: &GlobalContext,
    binary_op_map: &BinaryOpTyMap,
    item: &GlobalFn,
    mir_ctx: &mut MirContext,
) -> MirFunctionDef {
    let item = MirFn {
        header: transform_fn_header(
            global_ctx,
            binary_op_map,
            &global_ctx.fn_headers[item.header.0],
            mir_ctx,
        ),
        body: item.ctx.blocks[item.body.0]
            .stmts
            .iter()
            .map(|stmt| {
                transform_stmt(
                    global_ctx,
                    &item.ctx,
                    binary_op_map,
                    &item.ctx.stmts[stmt.0],
                )
            })
            .collect(),
        span: item.span,
    };
    mir_ctx.push_fn(item)
}

fn transform_fn_header(
    global_ctx: &GlobalContext,
    binary_op_map: &BinaryOpTyMap,
    item: &GlobalFnHeader,
    mir_ctx: &mut MirContext,
) -> MirFunctionHeaderDef {
    let item = MirFnHeader {
        name: item.name.clone(),
        params: item
            .params
            .iter()
            .map(|param| crate::MirFnParam {
                name: param.name.clone(),
                ty: resolve_ty(
                    global_ctx,
                    param
                        .ty
                        .scope
                        .function
                        .map(|def| &global_ctx.fns[def.0].ctx),
                    binary_op_map,
                    &param.ty,
                )
                .unwrap(),
                span: param.span,
            })
            .collect(),
        return_ty: item.return_ty.as_ref().map(|ty| {
            resolve_ty(
                global_ctx,
                ty.scope.function.map(|def| &global_ctx.fns[def.0].ctx),
                binary_op_map,
                ty,
            )
            .unwrap()
        }),
        span: item.span,
    };
    mir_ctx.push_fn_header(item)
}

fn transform_stmt(
    global_ctx: &GlobalContext,
    ctx: &InFnContext,
    binary_op_map: &BinaryOpTyMap,
    item: &InFnStmt,
) -> MirStmt {
    match &item.kind {
        &InFnStmtKind::ScopeBegin(def) => MirStmt {
            scope: (),
            kind: todo!(),
            span: todo!(),
        },
        &InFnStmtKind::ScopeEnd(def) => todo!(),
        &InFnStmtKind::Let(def) => todo!(),
        &InFnStmtKind::Block(def) => todo!(),
        &InFnStmtKind::If(def) => todo!(),
        &InFnStmtKind::For(def) => todo!(),
        InFnStmtKind::Break(item) => todo!(),
        InFnStmtKind::Continue(item) => todo!(),
        InFnStmtKind::Return(item) => todo!(),
        InFnStmtKind::Assign(item) => todo!(),
        &InFnStmtKind::Expr(def) => todo!(),
    }
}

fn transform_stmt_scope() -> MirScope {}

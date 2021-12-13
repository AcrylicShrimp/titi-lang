use crate::make_global::{
    GlobalInnerStruct, GlobalInnerStructField, GlobalStruct, GlobalStructField,
    GlobalStructFieldKind,
};
use crate::{InnerStructDef, ScopeRef, StructDef, TyRef};
use ast::{InnerStruct, InnerStructField, Struct, StructField, StructFieldKind};

pub fn make_global_struct(
    global_structs: &mut Vec<GlobalStruct>,
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    scope: ScopeRef,
    r#struct: Struct,
) -> StructDef {
    let global_struct = GlobalStruct {
        name: r#struct.name,
        fields: r#struct
            .fields
            .into_iter()
            .map(|field| make_global_struct_field(global_inner_structs, scope, field))
            .collect(),
        span: r#struct.span,
    };
    let def = global_structs.len();
    global_structs.push(global_struct);
    StructDef(def)
}

fn make_global_struct_field(
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    scope: ScopeRef,
    field: StructField,
) -> GlobalStructField {
    GlobalStructField {
        vis: field.vis,
        name: field.name,
        kind: match field.kind {
            StructFieldKind::Plain(ty) => GlobalStructFieldKind::Plain(TyRef { scope, ty }),
            StructFieldKind::Struct(inner) => GlobalStructFieldKind::Struct(
                make_global_inner_struct(global_inner_structs, scope, inner),
            ),
        },
        span: field.span,
    }
}

pub fn make_global_inner_struct(
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    scope: ScopeRef,
    inner: InnerStruct,
) -> InnerStructDef {
    let global_inner_struct = GlobalInnerStruct {
        fields: inner
            .fields
            .into_iter()
            .map(|field| make_global_inner_struct_field(global_inner_structs, scope, field))
            .collect(),
        span: inner.span,
    };
    let def = global_inner_structs.len();
    global_inner_structs.push(global_inner_struct);
    InnerStructDef(def)
}

fn make_global_inner_struct_field(
    global_inner_structs: &mut Vec<GlobalInnerStruct>,
    scope: ScopeRef,
    field: InnerStructField,
) -> GlobalInnerStructField {
    GlobalInnerStructField {
        name: field.name,
        kind: match field.kind {
            StructFieldKind::Plain(ty) => GlobalStructFieldKind::Plain(TyRef { scope, ty }),
            StructFieldKind::Struct(inner) => GlobalStructFieldKind::Struct(
                make_global_inner_struct(global_inner_structs, scope, inner),
            ),
        },
        span: field.span,
    }
}

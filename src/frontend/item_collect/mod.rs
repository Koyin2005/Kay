use indexmap::IndexMap;

use crate::{
    context::{GlobalContext, NodeInfo},
    errors::DiagnosticReporter,
    frontend::hir::{Hir, ItemKind, TypeDefKind},
};

pub struct ItemCollect<'a> {
    diag: &'a DiagnosticReporter,
}
impl<'a> ItemCollect<'a> {
    pub fn new(diag: &'a DiagnosticReporter) -> Self {
        Self { diag }
    }
    pub fn collect(self, hir: &'a Hir) -> GlobalContext<'a> {
        let mut nodes = IndexMap::new();
        for (&id, item) in hir.items.iter() {
            match &item.kind {
                ItemKind::TypeDef(type_def)=>{
                    for generic_param in type_def.generics.params.iter(){
                        nodes.insert(generic_param.def_id, NodeInfo::GenericParam(generic_param));
                    }
                    match &type_def.kind {
                        TypeDefKind::Struct(struct_def) => {
                            for field in struct_def.fields.iter() {
                                nodes.insert(field.id, NodeInfo::Field(field));
                            }
                        }
                        TypeDefKind::Variant(variant_def) => {
                            for case in variant_def.cases.iter() {
                                nodes.insert(case.id, NodeInfo::VariantCase(case));
                                for field in case.fields.iter().flatten() {
                                    nodes.insert(field.id, NodeInfo::VariantField(field));
                                }
                            }
                        }
                    }
                },
                ItemKind::Function(function) => {
                    for generic_param in function.generics.params.iter(){
                        nodes.insert(generic_param.def_id, NodeInfo::GenericParam(generic_param));
                    }
                },
                ItemKind::Module(_) => ()
            }
            nodes.insert(id, NodeInfo::Item(item));
        }
        GlobalContext::new(hir, self.diag, nodes)
    }
}

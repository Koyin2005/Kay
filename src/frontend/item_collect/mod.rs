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
            if let ItemKind::TypeDef(type_def) = &item.kind {
                match &type_def.kind {
                    TypeDefKind::Struct(struct_def) => {
                        for field in struct_def.fields.iter() {
                            nodes.insert(field.id, NodeInfo::Field(field));
                        }
                    }
                    TypeDefKind::Variant(variant_def) => {
                        for case in variant_def.cases.iter() {
                            for field in case.fields.iter() {
                                nodes.insert(field.id, NodeInfo::VariantField(field));
                            }
                        }
                    }
                }
            }
            nodes.insert(id, NodeInfo::Item(item));
        }
        GlobalContext::new(hir, self.diag, nodes)
    }
}

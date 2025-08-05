use crate::{
    Resolver,
    frontend::{
        ast::{Ast, Item, ItemKind, NodeId, TypeDefKind},
        ast_visit::{Visitor, walk_item},
        hir::DefId,
    },
};

pub struct DefCollector<'rsv, 'source> {
    resolver: &'rsv mut Resolver<'source>,
    next_id: DefId,
}

impl<'a, 'b> DefCollector<'a, 'b> {
    pub fn new(resolver: &'a mut Resolver<'b>, start_id: DefId) -> Self {
        Self {
            next_id: start_id,
            resolver,
        }
    }
    fn create_id(&mut self, id: NodeId) {
        let next_id = self.next_id;
        self.next_id = self.next_id.add(1);
        self.resolver
            .node_ids_to_defs
            .insert(id, next_id)
            .ok_or(())
            .expect_err("There should be only 1 def-id for each stmt.");
    }
    pub fn collect(mut self, ast: &Ast) {
        self.visit_ast(ast);
    }
}

impl Visitor for DefCollector<'_, '_> {
    fn visit_item(&mut self, item: &Item) {
        match item.kind {
            ItemKind::Function(ref function_def) => {
                self.create_id(function_def.id);
            }
            ItemKind::Type(ref type_def) => {
                self.create_id(type_def.id);
                match type_def.kind {
                    TypeDefKind::Struct(ref struct_def) => {
                        for field in struct_def.fields.iter() {
                            self.create_id(field.id);
                        }
                    }
                    TypeDefKind::Variant(ref variant_def) => {
                        for variant_case in variant_def.cases.iter() {
                            self.create_id(variant_case.id);
                            for field in variant_case.fields.iter() {
                                self.create_id(field.id);
                            }
                        }
                    }
                }
            }
        }

        walk_item(self, item);
    }
}

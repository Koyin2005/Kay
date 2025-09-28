use crate::{
    Resolver,
    frontend::{
        ast::{Ast, GenericParam, GenericParamKind, Item, ItemKind, NodeId, TypeDefKind},
        ast_visit::{Visitor, walk_item, walk_module},
        hir::{DefId, DefInfo, DefKind},
    },
};

pub struct DefCollector<'rsv, 'source> {
    resolver: &'rsv mut Resolver<'source>,
    current_module: Option<DefId>,
}

impl<'a, 'b> DefCollector<'a, 'b> {
    pub fn new(resolver: &'a mut Resolver<'b>) -> Self {
        Self {
            resolver,
            current_module: None,
        }
    }
    fn create_id(&mut self, id: NodeId, kind: DefKind, parent: Option<DefId>) -> DefId {
        let next_id = self.resolver.info.push(DefInfo { parent, kind });
        self.resolver
            .node_ids_to_defs
            .insert(id, next_id)
            .ok_or(())
            .expect_err("There should be only 1 def-id for each definition.");
        next_id
    }
    fn visit_generic_params(&mut self, params: &[GenericParam], parent: DefId) {
        for generic_param in params {
            self.create_id(
                generic_param.id,
                match generic_param.kind {
                    GenericParamKind::Origin => DefKind::OriginParam,
                    GenericParamKind::Type => DefKind::TypeParam,
                },
                Some(parent),
            );
        }
    }
    pub fn collect(mut self, ast: &Ast) {
        self.visit_ast(ast);
    }
}

impl Visitor for DefCollector<'_, '_> {
    fn visit_module(&mut self, module: &crate::frontend::ast::Module) {
        self.current_module = Some(self.create_id(module.id, DefKind::Module, None));
        walk_module(self, module);
        self.current_module = None;
    }
    fn visit_item(&mut self, item: &Item) {
        match item.kind {
            ItemKind::Function(ref function_def) => {
                let func_id =
                    self.create_id(function_def.id, DefKind::Function, self.current_module);
                self.visit_generic_params(
                    function_def
                        .generics
                        .as_ref()
                        .map_or(&[] as &[_], |generics| generics.params.as_slice()),
                    func_id,
                );
            }
            ItemKind::Type(ref type_def) => {
                let type_id = self.create_id(
                    type_def.id,
                    match type_def.kind {
                        TypeDefKind::Struct(..) => DefKind::Struct,
                        TypeDefKind::Variant(..) => DefKind::Variant,
                    },
                    self.current_module,
                );
                self.visit_generic_params(
                    type_def
                        .generics
                        .as_ref()
                        .map_or(&[] as &[_], |generics| generics.params.as_slice()),
                    type_id,
                );
                match type_def.kind {
                    TypeDefKind::Struct(ref struct_def) => {
                        for field in struct_def.fields.iter() {
                            self.create_id(field.id, DefKind::Field, Some(type_id));
                        }
                    }
                    TypeDefKind::Variant(ref variant_def) => {
                        for variant_case in variant_def.cases.iter() {
                            let case_id = self.create_id(
                                variant_case.id,
                                DefKind::VariantCase,
                                Some(type_id),
                            );
                            for field in variant_case.fields.iter().flatten() {
                                self.create_id(field.id, DefKind::Field, Some(case_id));
                            }
                        }
                    }
                }
            }
            ItemKind::Import(_) => (),
        }
        let old_module = self.current_module.take();
        walk_item(self, item);
        self.current_module = old_module;
    }
}

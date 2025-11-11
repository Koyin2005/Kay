use crate::{
    Ast, Resolver,
    builtins::builtin_from_name,
    frontend::{
        ast::{ItemKind, TypeDefKind},
        ast_visit::{Visitor, walk_item, walk_module},
        hir::DefId,
    },
    span::{
        Span,
        symbol::{Symbol, symbols},
    },
};

pub struct BuiltinCollector<'r, 'src> {
    resolver: &'r mut Resolver<'src>,
    in_builtin: bool,
}
impl<'r, 'src> BuiltinCollector<'r, 'src> {
    pub fn new(resolver: &'r mut Resolver<'src>) -> Self {
        Self {
            resolver,
            in_builtin: false,
        }
    }
    fn check_builtin(&mut self, id: DefId, name: Symbol, span: Span) -> bool {
        if self.in_builtin
            && let Some(builtin) = builtin_from_name(name)
        {
            if self.resolver.builtins.get_item(builtin).is_some() {
                self.resolver
                    .error(format!("Duplicate builtin '{}'.", name.as_str()), span);
            } else {
                self.resolver.builtins.set_item(builtin, id);
            }
            true
        } else {
            false
        }
    }
    pub fn collect(mut self, ast: &Ast) {
        self.visit_ast(ast);
    }
}
impl<'a, 'b> Visitor for BuiltinCollector<'a, 'b> {
    fn visit_item(&mut self, item: &crate::frontend::ast::Item) {
        match item.kind {
            ItemKind::Function(ref function) => {
                let id = self.resolver.node_ids_to_defs[&function.id];
                if !self.check_builtin(id, function.name.symbol, function.name.span)
                    && function.body.is_none()
                {
                    self.resolver.error(
                        format!(
                            "function '{}' must have body.",
                            function.name.symbol.as_str()
                        ),
                        function.span,
                    );
                }
            }
            ItemKind::Import(_) => (),
            ItemKind::Type(ref type_def) => {
                let id = self.resolver.node_ids_to_defs[&type_def.id];
                self.check_builtin(id, type_def.name.symbol, type_def.name.span);
                match &type_def.kind {
                    TypeDefKind::Struct(_) => (),
                    TypeDefKind::Variant(variant_def) => {
                        for case in variant_def.cases.iter() {
                            let id = self.resolver.node_ids_to_defs[&case.id];
                            self.check_builtin(id, case.name.symbol, case.name.span);
                        }
                    }
                }
            }
        }
        walk_item(self, item);
    }
    fn visit_module(&mut self, module: &crate::frontend::ast::Module) {
        self.in_builtin = module.name == symbols::BUILTINS;
        walk_module(self, module);
        self.in_builtin = false;
    }
}

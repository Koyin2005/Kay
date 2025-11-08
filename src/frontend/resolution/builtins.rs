use crate::{
    Ast, Resolver,
    builtins::builtin_from_name,
    frontend::{
        ast::ItemKind,
        ast_visit::{Visitor, walk_item, walk_module},
    },
    span::symbol::symbols,
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
    pub fn collect(mut self, ast: &Ast) {
        self.visit_ast(ast);
    }
}
impl<'a, 'b> Visitor for BuiltinCollector<'a, 'b> {
    fn visit_item(&mut self, item: &crate::frontend::ast::Item) {
        match item.kind {
            ItemKind::Function(ref function) => {
                let id = self.resolver.node_ids_to_defs[&function.id];
                if self.in_builtin
                    && let Some(builtin) = builtin_from_name(function.name.symbol)
                {
                    if function.body.is_some() {
                        self.resolver.error(
                            format!(
                                "builtin function '{}' can't have body.",
                                function.name.symbol.as_str()
                            ),
                            function.span,
                        );
                    }
                    if self.resolver.builtins.get_item(builtin).is_some() {
                        self.resolver.error(
                            format!(
                                "Duplicate builtin function '{}'.",
                                function.name.symbol.as_str()
                            ),
                            function.span,
                        );
                    } else {
                        self.resolver.builtins.set_item(builtin, id);
                    }
                } else if !self.in_builtin && function.body.is_none() {
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
            ItemKind::Type(_) => (),
        }
        walk_item(self, item);
    }
    fn visit_module(&mut self, module: &crate::frontend::ast::Module) {
        self.in_builtin = module.name == symbols::BUILTINS;
        walk_module(self, module);
        self.in_builtin = false;
    }
}

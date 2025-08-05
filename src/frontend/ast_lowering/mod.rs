use indexmap::IndexMap;

use crate::frontend::{
    ast,
    hir::{self, DefId, Hir},
    resolution::resolve::ResolveResults,
};

pub struct AstLower {
    resolution_results: ResolveResults,
}
impl AstLower {
    pub fn new(resolution_results: ResolveResults) -> Self {
        Self { resolution_results }
    }
    fn lower_item(&mut self, item: &ast::Item) -> (DefId, hir::Item) {
        todo!("LOWER ITEMS")
    }
    pub fn lower_ast(mut self, ast: &ast::Ast) -> Hir {
        let items = ast.items.iter().map(|item| self.lower_item(item)).collect();
        Hir {
            items,
            bodies: IndexMap::new(),
        }
    }
}

use fxhash::FxHashMap;

use crate::{
    errors::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{Ast, NodeId},
        ast_visit::Visitor,
        hir::{DefId, Resolution},
        resolution::{def_collect::DefCollector, name_res::NameRes},
    },
    span::Span,
};
pub struct ResolveResults {
    resolutions: FxHashMap<NodeId, Resolution<NodeId>>,
    node_ids_to_defs: FxHashMap<NodeId, DefId>,
}
impl ResolveResults {
    pub fn expect_def_id(&self, id: NodeId) -> DefId {
        self.node_ids_to_defs
            .get(&id)
            .copied()
            .expect("Expected a def-id when there was none.")
    }
    pub fn get_resolution(&self, id: NodeId) -> Option<Resolution<NodeId>> {
        self.resolutions.get(&id).copied()
    }
}
pub struct Resolver<'source> {
    pub(super) node_ids_to_defs: FxHashMap<NodeId, DefId>,
    pub(super) resolutions: FxHashMap<NodeId, Resolution<NodeId>>,
    diag: &'source DiagnosticReporter<'source>,
}

impl<'source> Resolver<'source> {
    pub fn new(diag: &'source DiagnosticReporter<'source>) -> Self {
        Self {
            diag,
            resolutions: FxHashMap::default(),
            node_ids_to_defs: FxHashMap::default(),
        }
    }
    pub fn error(&self, msg: impl IntoDiagnosticMessage, span: Span) {
        self.diag.emit_diag(msg, span);
    }
    pub fn resolve(mut self, ast: &Ast) -> ResolveResults {
        DefCollector::new(&mut self, DefId::new(0)).collect(ast);
        NameRes::new(&mut self).visit_ast(ast);
        self.diag.emit();
        ResolveResults {
            node_ids_to_defs: self.node_ids_to_defs,
            resolutions: self.resolutions,
        }
    }
}

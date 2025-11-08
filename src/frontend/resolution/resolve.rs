use fxhash::FxHashMap;

use crate::{
    builtins::Builtins,
    diagnostics::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{Ast, NodeId},
        ast_visit::Visitor,
        hir::{DefId, DefInfo, Resolution},
        resolution::{builtins::BuiltinCollector, def_collect::DefCollector, name_res::NameRes},
    },
    indexvec::IndexVec,
    span::Span,
};
#[derive(Debug)]
pub struct ResolveResults {
    resolutions: FxHashMap<NodeId, Resolution<NodeId>>,
    info: IndexVec<DefId, DefInfo>,
    node_ids_to_defs: FxHashMap<NodeId, DefId>,
    builtins: Builtins,
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
    pub fn get_def_info_and_builtins(self) -> (IndexVec<DefId, DefInfo>, Builtins) {
        (self.info, self.builtins)
    }
}
pub struct Resolver<'source> {
    pub(super) node_ids_to_defs: FxHashMap<NodeId, DefId>,
    pub(super) info: IndexVec<DefId, DefInfo>,
    pub(super) resolutions: FxHashMap<NodeId, Resolution<NodeId>>,
    pub(super) builtins: Builtins,
    diag: &'source DiagnosticReporter,
}

impl<'source> Resolver<'source> {
    pub fn new(diag: &'source DiagnosticReporter) -> Self {
        Self {
            diag,
            resolutions: FxHashMap::default(),
            node_ids_to_defs: FxHashMap::default(),
            info: IndexVec::new(),
            builtins: Builtins::new(),
        }
    }
    pub fn error(&self, msg: impl IntoDiagnosticMessage, span: Span) {
        self.diag.emit_diag(msg, span);
    }
    pub fn resolve(mut self, ast: &Ast) -> ResolveResults {
        DefCollector::new(&mut self).collect(ast);
        BuiltinCollector::new(&mut self).collect(ast);
        NameRes::new(&mut self).visit_ast(ast);
        self.diag.emit();
        ResolveResults {
            node_ids_to_defs: self.node_ids_to_defs,
            resolutions: self.resolutions,
            info: self.info,
            builtins: self.builtins,
        }
    }
}

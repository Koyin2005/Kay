use fxhash::FxHashMap;

use crate::{
    errors::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{Ast, NodeId},
        ast_visit::Visitor,
        hir::DefId,
        resolution::{def_collect::DefCollector, name_res::NameRes},
    },
    span::Span,
};
#[derive(Clone, Copy, Debug)]
pub enum Resolution {
    Builtin,
    Type(NodeId),
    Field,
    Function,
    VariantCase,
    Variable,
}
impl Resolution {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Field => "field",
            Self::Builtin => "builtin",
            Self::Function => "function",
            Self::Type(_) => "type",
            Self::Variable => "variable",
            Self::VariantCase => "case",
        }
    }
}
pub struct ResolveResults {
    resolutions: FxHashMap<NodeId, Resolution>,
    node_ids_to_defs: FxHashMap<NodeId, DefId>,
}
impl ResolveResults {
    pub fn get_def_id(&self, id: NodeId) -> Option<DefId> {
        self.node_ids_to_defs.get(&id).copied()
    }
    pub fn get_resolution(&self, id: NodeId) -> Option<Resolution> {
        self.resolutions.get(&id).copied()
    }
}
pub struct Resolver<'source> {
    pub(super) node_ids_to_defs: FxHashMap<NodeId, DefId>,
    pub(super) resolutions: FxHashMap<NodeId, Resolution>,
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
        NameRes::new(&mut self).visit_ast(ast);
        DefCollector::new(&mut self, DefId::new(0)).collect(ast);
        self.diag.emit();
        ResolveResults {
            node_ids_to_defs: self.node_ids_to_defs,
            resolutions: self.resolutions,
        }
    }
}

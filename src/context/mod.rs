use crate::{
    errors::DiagnosticReporter,
    frontend::{
        hir::{self, DefId, Hir},
        ty_lower::TypeLower,
    },
    types::Type,
};

pub type CtxtRef<'ctxt> = &'ctxt GlobalContext<'ctxt>;
pub struct GlobalContext<'hir> {
    hir: &'hir Hir,
    diag: &'hir DiagnosticReporter<'hir>,
}
impl<'hir> GlobalContext<'hir> {
    pub(crate) fn new(hir: &'hir Hir, diag: &'hir DiagnosticReporter<'hir>) -> Self {
        Self { hir, diag }
    }
    pub fn diag(&self) -> &'hir DiagnosticReporter<'hir> {
        self.diag
    }
    pub fn expect_item(&self, id: DefId) -> &hir::Item {
        &self.hir.items[&id]
    }
    pub fn signature_of(&'hir self, id: DefId) -> (Vec<Type>, Type) {
        match &self.expect_item(id).kind {
            hir::ItemKind::Function(function) => {
                let lower = TypeLower::new(self, true);
                let (params, return_ty) = lower.lower_function_sig(&function.sig);
                (params.collect(), return_ty)
            }
            hir::ItemKind::TypeDef(_) => (Vec::new(), Type::new_unit()),
        }
    }
    pub fn type_of(&'hir self, id: DefId) -> Type {
        match &self.expect_item(id).kind {
            hir::ItemKind::Function(function) => {
                let lower = TypeLower::new(self, true);
                let (params, return_ty) = lower.lower_function_sig(&function.sig);
                Type::new_function(params, return_ty)
            }
            hir::ItemKind::TypeDef(type_def) => Type::new_nominal(type_def.id),
        }
    }
    pub fn expect_body_for(&self, id: DefId) -> &hir::Body {
        self.get_body_for(id)
            .expect("There should be a body for this item")
    }
    pub fn get_body_for(&self, id: DefId) -> Option<&hir::Body> {
        match self.expect_item(id).kind {
            hir::ItemKind::Function(ref function) => self.hir.bodies.get(&function.body_id),
            hir::ItemKind::TypeDef(_) => None,
        }
    }
}

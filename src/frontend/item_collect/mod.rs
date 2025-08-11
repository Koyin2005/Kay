use crate::{context::GlobalContext, errors::DiagnosticReporter, frontend::hir::Hir};

pub struct ItemCollect<'a> {
    diag: &'a DiagnosticReporter,
}
impl<'a> ItemCollect<'a> {
    pub fn new(diag: &'a DiagnosticReporter) -> Self {
        Self { diag }
    }
    pub fn collect(self, hir: &'a Hir) -> GlobalContext<'a> {
        GlobalContext::new(hir, self.diag)
    }
}

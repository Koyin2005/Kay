use crate::{
    context::CtxtRef,
    frontend::hir::{Body, DefId, Expr, Pattern},
    types::Type,
};

pub struct TypeCheck<'ctxt> {
    context: CtxtRef<'ctxt>,
    id: DefId,
    body: &'ctxt Body,
}
impl<'ctxt> TypeCheck<'ctxt> {
    pub fn new(context: CtxtRef<'ctxt>, id: DefId) -> Option<Self> {
        let body = context.get_body_for(id)?;
        Some(Self { context, id, body })
    }

    fn check_expr(&self, _expr: &Expr, _expected_ty: Option<Type>) {}
    fn check_pattern(&self, _pat: &Pattern, _expected_ty: Type) {}
    pub fn check(self) {
        let (params, return_ty) = self.context.signature_of(self.id);
        for (param, ty) in self.body.params.iter().zip(params) {
            self.check_pattern(&param.pat, ty);
        }
        self.check_expr(&self.body.value, Some(return_ty));
    }
}

use crate::{
    TypeCheck,
    frontend::hir::{self, HirId, PrimitiveType},
    span::Span,
    types::Type,
};

pub struct Coerce {
    target_type: Option<Type>,
    exprs_with_tys: Vec<(HirId, Span, Type)>,
    final_ty: Option<Type>,
}

impl Coerce {
    pub fn new(base_ty: Option<Type>) -> Self {
        Self {
            exprs_with_tys: Vec::new(),
            final_ty: base_ty.clone(),
            target_type: base_ty,
        }
    }
    pub fn add_expr_and_ty<'b>(&'b mut self, expr: &hir::Expr, ty: Type, ctxt: &TypeCheck) {
        self.merge(&ty, ctxt);
        self.exprs_with_tys.push((expr.id, expr.span, ty));
    }
    pub fn target_type(&self) -> Option<Type> {
        self.target_type.clone()
    }
    fn merge(&mut self, new_ty: &Type, ctxt: &TypeCheck) {
        let Some(ref a) = self.final_ty else {
            self.final_ty = Some(new_ty.clone());
            return;
        };
        let b = new_ty;
        let next_ty = (|| match (a, b) {
            (Type::Primitive(PrimitiveType::Never), ty)
            | (ty, Type::Primitive(PrimitiveType::Never)) => Some(ty.clone()),
            (a, b) => ctxt.infer_ctxt().unify(a, b).ok(),
        })();
        if let Some(next_ty) = next_ty {
            self.final_ty = Some(next_ty);
        }
    }

    pub fn complete(self) -> (Option<Type>, impl Iterator<Item = (HirId, Span, Type)>) {
        (self.final_ty, self.exprs_with_tys.into_iter())
    }
}

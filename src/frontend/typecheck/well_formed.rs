use fxhash::FxHashSet;

use crate::{
    context::CtxtRef,
    frontend::hir::HirId,
    span::Span,
    types::{Type, TypeVisitor, walk_ty},
};

pub(super) struct WellFormed<'a> {
    param_locals: FxHashSet<HirId>,
    sig_types: Vec<(Span, Type)>,
    ctxt: CtxtRef<'a>,
}
impl<'a> WellFormed<'a> {
    pub fn new(ctxt: CtxtRef<'a>) -> Self {
        Self {
            param_locals: FxHashSet::default(),
            sig_types: Vec::new(),
            ctxt,
        }
    }
    pub fn add_type_from_sig(&mut self, source: Span, ty: Type) {
        self.sig_types.push((source, ty));
    }
    pub fn add_param_local(&mut self, id: HirId) {
        self.param_locals.insert(id);
    }
    pub fn check(self) {
        for (src, ty) in self.sig_types {
            struct ValidOrigins<'a> {
                param_locals: &'a FxHashSet<HirId>,
                ctxt: CtxtRef<'a>,
                source: Span,
            }
            impl<'a> TypeVisitor for ValidOrigins<'a> {
                fn visit_ty(&mut self, ty: &Type) {
                    if let &Type::Ref(_, Some(origin), _) = ty {
                        if self.param_locals.contains(&origin.1) {
                            self.ctxt.diag().emit_diag(
                                format!("Cannot use '{}' as 'origin'.", origin.0.as_str()),
                                self.source,
                            );
                        }
                    }
                    walk_ty(self, ty);
                }
            }
            let mut valid_origign = ValidOrigins {
                param_locals: &self.param_locals,
                ctxt: self.ctxt,
                source: src,
            };
            valid_origign.visit_ty(&ty);
        }
    }
}

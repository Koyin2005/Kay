use crate::{
    context::CtxtRef,
    frontend::{
        pattern_analysis::PatternContext,
        pattern_check::{error::MissingPatternError, lower::lower_pattern},
        thir::{
            self, Body,
            visit::{Visitor, walk_expr, walk_stmt},
        },
    },
};
mod error;
mod lower;
pub struct PatCheck<'a> {
    ctxt: CtxtRef<'a>,
    body: &'a Body,
}

impl<'a> PatCheck<'a> {
    pub fn new(body: &'a Body, ctxt: CtxtRef<'a>) -> Self {
        Self { ctxt, body }
    }
    pub fn check(mut self) {
        for thir::Param { pattern } in self.body.info.params.iter() {
            let usefulness = PatternContext::new(self.ctxt)
                .check(pattern.ty.clone(), std::iter::once(lower_pattern(&pattern)));
            if !usefulness.missing_patterns.is_empty() {
                self.ctxt
                    .diag()
                    .emit_diag("Refutable pattern in function parameters.", pattern.span);
            }
        }
        self.visit_expr(self.body.expr(self.body.value));
    }
}

impl<'a> Visitor<'a> for PatCheck<'a> {
    fn body(&self) -> &'a super::thir::Body {
        self.body
    }
    fn visit_stmt(&mut self, stmt: &thir::Stmt) {
        if let thir::StmtKind::Let(pattern, _) = &stmt.kind {
            let usefulness = PatternContext::new(self.ctxt)
                .check(pattern.ty.clone(), std::iter::once(lower_pattern(&pattern)));
            if !usefulness.missing_patterns.is_empty() {
                self.ctxt
                    .diag()
                    .emit_diag("Refutable pattern in let bindings.", pattern.span);
            }
        }
        walk_stmt(self, stmt);
    }
    fn visit_expr(&mut self, expr: &super::thir::Expr) {
        if let thir::ExprKind::Match(scrut, ref arms) = expr.kind {
            let scrut = self.body.expr(scrut);
            let ty = scrut.ty.clone();
            let arms = arms.iter().copied().map(|arm| self.body().arm(arm));
            let patterns = arms.map(|arm| &arm.pattern);
            let usefulness = PatternContext::new(self.ctxt)
                .check(ty, patterns.map(|pattern| lower_pattern(pattern)));
            if !usefulness.missing_patterns.is_empty() {
                self.ctxt.diag().emit_diag_from(MissingPatternError {
                    missing_patterns: usefulness.missing_patterns,
                    span: scrut.span,
                    ctxt: self.ctxt,
                });
            }
        } else if let thir::ExprKind::For(_, ref pattern, ..) = expr.kind {
            let usefulness = PatternContext::new(self.ctxt)
                .check(pattern.ty.clone(), std::iter::once(lower_pattern(&pattern)));
            if !usefulness.missing_patterns.is_empty() {
                self.ctxt
                    .diag()
                    .emit_diag("Refutable pattern in let bindings.", pattern.span);
            }
        }
        walk_expr(self, expr);
    }
}

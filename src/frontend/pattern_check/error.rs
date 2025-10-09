use crate::{
    context::CtxtRef,
    diagnostics::{Diagnostic, IntoDiagnostic, Note},
    frontend::pattern_analysis::Pattern,
    span::Span,
};

pub(super) struct MissingPatternError<'a> {
    pub(super) span: Span,
    pub(super) ctxt: CtxtRef<'a>,
    pub(super) missing_patterns: Vec<Pattern>,
}

impl IntoDiagnostic for MissingPatternError<'_> {
    fn into(self) -> crate::diagnostics::Diagnostic {
        Diagnostic::new(
            "Inexhaustive patterns.",
            self.span,
            Some(Note::new(
                "Missing patterns are:",
                self.missing_patterns
                    .into_iter()
                    .map(|pattern| pattern.format(self.ctxt).into())
                    .collect(),
            )),
        )
    }
}

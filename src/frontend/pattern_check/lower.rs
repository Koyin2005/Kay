use crate::frontend::{
    ast::LiteralKind,
    pattern_analysis::{Pattern, constructors::Constructor},
    thir,
};

pub fn lower_pattern(pattern: &thir::Pattern) -> Pattern {
    match pattern.kind {
        thir::PatternKind::Binding(..) | thir::PatternKind::Wilcard => Pattern {
            ty: pattern.ty.clone(),
            constructor: Constructor::Wildcard,
            fields: vec![],
        },
        thir::PatternKind::Case(id, _, ref fields) => Pattern {
            ty: pattern.ty.clone(),
            constructor: Constructor::Case(id),
            fields: fields.iter().map(lower_pattern).collect(),
        },
        thir::PatternKind::Tuple(ref fields) => Pattern {
            constructor: Constructor::Tuple,
            ty: pattern.ty.clone(),
            fields: fields.iter().map(lower_pattern).collect(),
        },
        thir::PatternKind::Lit(lit_kind) => Pattern {
            ty: pattern.ty.clone(),
            constructor: match lit_kind {
                LiteralKind::Bool(value) => Constructor::Bool(value),
                LiteralKind::Int(value) => Constructor::Int(value),
                LiteralKind::IntErr => Constructor::Unknown,
                LiteralKind::String(_) => Constructor::Unknown,
            },
            fields: vec![],
        },
    }
}

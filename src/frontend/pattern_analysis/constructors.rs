use crate::{
    context::TypeDefKind,
    frontend::{
        hir::{Definition, PrimitiveType},
        pattern_analysis::PatternContext,
    },
    types::{Type, VariantCaseIndex},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constructor {
    Bool(bool),
    Int(i64),
    Tuple,
    Ref,
    Unknown,
    NonExhaustive,
    Case(VariantCaseIndex),
    Wildcard,
}
impl Constructor {
    pub fn is_covered_by(self, other: Constructor) -> bool {
        match (self, other) {
            (Constructor::Bool(value), Constructor::Bool(other)) => value == other,
            (Constructor::Int(value), Constructor::Int(other)) => value == other,
            (Constructor::Tuple, Constructor::Tuple) => true,
            (Constructor::Ref, Constructor::Ref) => true,
            (_, Constructor::Wildcard) => true,
            (Constructor::Case(name), Constructor::Case(other)) => name == other,
            (Constructor::Unknown, _) | (_, Constructor::Unknown) => false,
            _ => false,
        }
    }
}

impl PatternContext<'_> {
    pub fn constructors<'b>(&self, ty: &Type) -> Vec<(Constructor, Vec<Type>)> {
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Bool => vec![
                    (Constructor::Bool(true), Vec::new()),
                    (Constructor::Bool(false), Vec::new()),
                ],
                PrimitiveType::Int(_) | PrimitiveType::String => {
                    vec![(Constructor::NonExhaustive, Vec::new())]
                }
                PrimitiveType::Never => vec![],
            },
            Type::Ref(ty, _, _) => {
                if ty.is_str() {
                    vec![(Constructor::Unknown, Vec::new())]
                } else {
                    vec![(Constructor::Ref, vec![*ty.clone()])]
                }
            }
            Type::Tuple(fields) => vec![(Constructor::Tuple, fields.clone())],
            &Type::Nominal(def, ref args) => match &self.ctxt.type_def(def).kind {
                TypeDefKind::Struct(case) => {
                    if case.fields.iter().any(|field| {
                        !self
                            .ctxt
                            .is_inhabited(&self.ctxt.type_of(field.id).instantiate(args.clone()))
                    }) {
                        return Vec::new();
                    }
                    vec![(
                        Constructor::Tuple,
                        case.fields
                            .iter()
                            .map(|field| self.ctxt.type_of(field.id).instantiate(args.clone()))
                            .collect(),
                    )]
                }
                TypeDefKind::Variant(cases) => cases
                    .iter()
                    .enumerate()
                    .filter_map(|(i, (Definition::Def(_), case))| {
                        if case.fields.iter().any(|field| {
                            !self.ctxt.is_inhabited(
                                &self.ctxt.type_of(field.id).instantiate(args.clone()),
                            )
                        }) {
                            return None;
                        }
                        Some((
                            Constructor::Case(VariantCaseIndex::new(i)),
                            case.fields
                                .iter()
                                .map(|field| self.ctxt.type_of(field.id).instantiate(args.clone()))
                                .collect(),
                        ))
                    })
                    .collect(),
            },
            Type::Generic(_, _) | Type::Err | Type::Function(..) | Type::Array(_) => {
                vec![(Constructor::NonExhaustive, Vec::new())]
            }
            Type::Infer(_) => unreachable!("Cannot have inference variables here"),
        }
    }
}

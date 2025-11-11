use std::vec;

use fxhash::FxHashSet;

use crate::{
    context::CtxtRef,
    frontend::pattern_analysis::constructors::Constructor,
    types::{Type, format::TypeFormat},
};
pub mod constructors;
#[derive(Debug, Clone)]
pub struct Pattern {
    pub ty: Type,
    pub constructor: Constructor,
    pub fields: Vec<Pattern>,
}

impl Pattern {
    pub fn format(&self, ctxt: CtxtRef) -> String {
        let mut start_or_comma = {
            let mut first = true;
            move || {
                if first {
                    first = false;
                    ""
                } else {
                    ","
                }
            }
        };
        match self.constructor {
            Constructor::Wildcard | Constructor::Unknown | Constructor::NonExhaustive => {
                "_".to_string()
            }
            Constructor::Bool(value) => value.to_string(),
            Constructor::Int(value) => value.to_string(),
            Constructor::Ref => format!("ref {}", self.fields[0].format(ctxt)),
            Constructor::Case(case) => {
                let &Type::Nominal(id, _) = &self.ty else {
                    unreachable!("Cannot use this with non-adt")
                };
                let case_id = ctxt.type_def(id).cases()[case].0.into();
                let mut txt = format!(
                    "{}.{}",
                    ctxt.symbol(id).as_str(),
                    ctxt.symbol(case_id).as_str()
                );
                if ctxt
                    .expect_variant_case_node(case_id.into())
                    .fields
                    .is_some()
                {
                    txt.push('(');
                    for field in self.fields.iter() {
                        txt.push_str(&format!("{}{}", start_or_comma(), field.format(ctxt)));
                    }
                    txt.push(')');
                }
                txt
            }
            Constructor::Tuple => {
                let mut txt = match &self.ty {
                    Type::Nominal(id, _) => {
                        let name = ctxt.symbol(*id);
                        name.as_str().to_string()
                    }
                    Type::Tuple(_) => String::with_capacity(2),
                    ty => unreachable!(
                        "Cannot have tuple constructor with non-product type {}",
                        TypeFormat::ty_to_string(ctxt, ty)
                    ),
                };
                txt.push('(');
                for field in self.fields.iter() {
                    txt.push_str(start_or_comma());
                    txt.push_str(&field.format(ctxt));
                }
                txt.push(')');
                txt
            }
        }
    }
}
fn specialize_tuple<'a>(tuple: &'a [Pattern], constructor: Constructor) -> Option<Vec<Pattern>> {
    let (head, tail) = match tuple.split_first() {
        Some((head, tail)) => (head, tail),
        None => return Some(Vec::new()),
    };
    if constructor.is_covered_by(head.constructor) {
        Some(head.fields.iter().chain(tail).cloned().collect())
    } else {
        None
    }
}
struct Matrix {
    rows: Vec<Vec<Pattern>>,
    columns: Vec<(Type, bool)>,
}
impl Matrix {
    fn new(
        columns: impl IntoIterator<Item = Type>,
        rows: impl IntoIterator<Item = Vec<Pattern>>,
    ) -> Self {
        Self {
            rows: rows.into_iter().collect(),
            columns: columns.into_iter().map(|ty| (ty, true)).collect(),
        }
    }
    fn columns(&self) -> &[(Type, bool)] {
        self.columns.as_slice()
    }
    fn row_count<'b>(&'b self) -> usize {
        self.rows.len()
    }
    fn rows(&self) -> impl Iterator<Item = &[Pattern]> {
        self.rows.iter().map(|row| row.as_slice())
    }
    fn specialize(&self, constructor: Constructor, fields: &[Type]) -> Matrix {
        let columns = fields
            .iter()
            .map(|ty| (ty.clone(), false))
            .chain(self.columns[1..].iter().cloned());
        let rows = self
            .rows
            .iter()
            .filter_map(|row| specialize_tuple(row, constructor));
        Self {
            rows: rows.collect::<Vec<_>>(),
            columns: columns.collect(),
        }
    }
}

pub struct Usefulness {
    pub missing_patterns: Vec<Pattern>,
}
pub struct PatternContext<'a> {
    pub(super) ctxt: CtxtRef<'a>,
}
impl<'a> PatternContext<'a> {
    pub fn new(ctxt: CtxtRef<'a>) -> Self {
        Self { ctxt }
    }
    pub fn check(mut self, ty: Type, patterns: impl IntoIterator<Item = Pattern>) -> Usefulness {
        let missing = self.witnesses(Matrix::new(
            std::iter::once(ty),
            patterns.into_iter().map(|pattern| vec![pattern]),
        ));
        let missing = if missing.is_empty() {
            Vec::new()
        } else {
            missing.into_iter().map(|mut row| row.remove(0)).collect()
        };
        Usefulness {
            missing_patterns: missing,
        }
    }

    fn witnesses<'b: 'a>(&mut self, matrix: Matrix) -> Vec<Vec<Pattern>> {
        if matrix.columns().len() == 0 {
            return if matrix.row_count() == 0 {
                vec![vec![]]
            } else {
                vec![]
            };
        }
        let (ty, top_level) = matrix.columns()[0].clone();
        let mut witnesses = Vec::new();
        let mut constructors = self.constructors(&ty);
        let seen_constructors = matrix
            .rows()
            .filter_map(|row| row.get(0).map(|pat| pat.constructor))
            .collect::<FxHashSet<_>>();

        if seen_constructors.is_empty() && !top_level {
            constructors = vec![(Constructor::Wildcard, Vec::new())];
        }
        for (constructor, fields) in constructors {
            let matrix = matrix.specialize(constructor, &fields);
            let new_witnesses = self.witnesses(matrix);
            for witneess in new_witnesses {
                let (fields, rest) = witneess.split_at(fields.len());
                let new_witness = std::iter::once(Pattern {
                    ty: ty.clone(),
                    constructor,
                    fields: fields.to_vec(),
                })
                .chain(rest.iter().cloned())
                .collect::<Vec<_>>();
                witnesses.push(new_witness);
            }
        }
        witnesses
    }
}

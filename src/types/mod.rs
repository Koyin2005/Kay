use crate::{define_id, frontend::hir, indexvec::IndexVec, span::symbol::Symbol};

pub mod format;
define_id!(
    #[derive(Debug)]
    pub struct FieldIndex {}
);
define_id!(
    #[derive(Debug)]
    pub struct VariantIndex {}
);
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct FieldType {
    pub name: Symbol,
    pub ty: Type,
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GenericArg(pub Type);
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct VariantFields {
    pub name: Symbol,
    pub fields: Vec<Type>,
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
}
impl GenericArgs {
    pub const fn empty() -> Self {
        Self { args: Vec::new() }
    }
    pub const fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Primitive(hir::PrimitiveType),
    Nominal(hir::DefId, GenericArgs),
    Struct(IndexVec<FieldIndex, FieldType>),
    Variant(IndexVec<VariantIndex, VariantFields>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Ref(Box<Type>),
    Err,
}
impl Type {
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Err)
    }
    pub fn new_ref_str() -> Self {
        Self::new_ref(Self::new_primative(hir::PrimitiveType::String))
    }
    pub fn new_ref(ty: Type) -> Self {
        Self::Ref(Box::new(ty))
    }
    pub const fn new_str() -> Self {
        Self::Tuple(Vec::new())
    }
    pub const fn new_int(signed: hir::IntType) -> Self {
        Self::new_primative(hir::PrimitiveType::Int(signed))
    }
    pub const fn new_bool() -> Self {
        Self::new_primative(hir::PrimitiveType::Bool)
    }
    pub const fn new_unit() -> Self {
        Self::Tuple(Vec::new())
    }
    pub fn new_function(params: impl Iterator<Item = Type>, return_type: Type) -> Self {
        Self::Function(params.collect(), Box::new(return_type))
    }
    pub const fn new_primative(primative: hir::PrimitiveType) -> Self {
        Self::Primitive(primative)
    }
    pub fn new_nominal(id: hir::DefId) -> Self {
        Self::Nominal(id, GenericArgs::empty())
    }
    pub fn new_nominal_with_args(id: hir::DefId, args: impl Iterator<Item = GenericArg>) -> Self {
        Self::Nominal(
            id,
            GenericArgs {
                args: args.collect(),
            },
        )
    }
    pub fn new_struct(fields: impl Iterator<Item = (Symbol, Type)>) -> Self {
        Self::Struct(IndexVec::from_iter(
            fields.map(|(name, ty)| FieldType { name, ty }),
        ))
    }
    pub fn new_variants(cases: impl Iterator<Item = (Symbol, Vec<Type>)>) -> Self {
        Self::Variant(IndexVec::from_iter(
            cases.map(|(name, fields)| VariantFields { name, fields }),
        ))
    }
    pub fn new_tuple_from_iter(iter: impl Iterator<Item = Type>) -> Self {
        Self::Tuple(iter.collect())
    }

    pub fn is_sized(&self) -> bool {
        match self {
            Self::Primitive(prim) => match prim {
                hir::PrimitiveType::Bool
                | hir::PrimitiveType::Never
                | hir::PrimitiveType::Int(..) => true,
                hir::PrimitiveType::String => false,
            },
            Self::Function(..) => true,
            Self::Ref(_) => true,
            Self::Nominal(_, _) => todo!("HANDLE NOMINAL TYPE SIZENESS"),
            Self::Err => true,
            Self::Tuple(elements) => elements.iter().all(|ty| ty.is_sized()),
            Self::Variant(variants) => variants
                .iter()
                .all(|variant| variant.fields.iter().all(|field| field.is_sized())),
            Self::Struct(fields) => fields.iter().all(|field| field.ty.is_sized()),
        }
    }
}

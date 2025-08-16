use crate::{
    context::CtxtRef,
    define_id,
    frontend::{ast, hir},
    indexvec::IndexVec,
    span::symbol::{self, Symbol},
    types::format::TypeFormat,
};

pub mod format;
define_id!(
    #[derive(Debug)]
    pub struct FieldIndex {}
);
define_id!(
    #[derive(Debug)]
    pub struct VariantCaseIndex {}
);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IsMutable {
    Yes,
    No,
}
impl From<ast::Mutable> for IsMutable {
    fn from(value: ast::Mutable) -> Self {
        match value {
            ast::Mutable::Yes(_) => IsMutable::Yes,
            ast::Mutable::No => IsMutable::No,
        }
    }
}
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
    Nominal(hir::Definition, GenericArgs),
    Struct(IndexVec<FieldIndex, FieldType>),
    Variant(IndexVec<VariantCaseIndex, VariantFields>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Ref(Box<Type>, IsMutable),
    Generic(symbol::Symbol, u32),
    Infer(u32),
    Err,
}
impl Type {
    pub fn format(&self, ctxt: CtxtRef) -> String {
        TypeFormat::new(ctxt).format_type(self)
    }
    pub fn has_error(&self) -> bool {
        match self {
            Self::Generic(_, _) => false,
            Self::Infer(_) => false,
            Self::Err => true,
            Self::Function(params, return_ty) => {
                return_ty.has_error() && params.iter().any(|ty| ty.has_error())
            }
            Self::Primitive(..) => false,
            Self::Ref(ty, _) => ty.has_error(),
            Self::Tuple(fields) => fields.iter().any(|ty| ty.has_error()),
            Self::Variant(variants) => variants
                .iter()
                .any(|variant| variant.fields.iter().any(|ty| ty.has_error())),
            Self::Struct(fields) => fields.iter().any(|field| field.ty.has_error()),
            Self::Nominal(_, args) => args.args.iter().any(|arg| arg.0.has_error()),
        }
    }
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Err)
    }
    pub fn new_ref_str() -> Self {
        Self::new_ref_immutable(Self::new_primative(hir::PrimitiveType::String))
    }
    pub fn new_ref(ty: Type, mutable: IsMutable) -> Self {
        Self::Ref(Box::new(ty), mutable)
    }
    pub fn new_ref_immutable(ty: Type) -> Self {
        Self::Ref(Box::new(ty), IsMutable::No)
    }
    pub fn new_ref_mut(ty: Type) -> Self {
        Self::Ref(Box::new(ty), IsMutable::Yes)
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
    pub fn new_function(params: impl IntoIterator<Item = Type>, return_type: Type) -> Self {
        Self::Function(params.into_iter().collect(), Box::new(return_type))
    }
    pub const fn new_never() -> Self {
        Self::Primitive(hir::PrimitiveType::Never)
    }
    pub const fn new_primative(primative: hir::PrimitiveType) -> Self {
        Self::Primitive(primative)
    }
    pub fn new_nominal(id: hir::Definition) -> Self {
        Self::Nominal(id, GenericArgs::empty())
    }
    pub fn new_nominal_with_args(
        id: hir::Definition,
        args: impl IntoIterator<Item = GenericArg>,
    ) -> Self {
        Self::Nominal(
            id,
            GenericArgs {
                args: args.into_iter().collect(),
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
    pub fn new_tuple_from_iter(iter: impl IntoIterator<Item = Type>) -> Self {
        Self::Tuple(iter.into_iter().collect())
    }
}

use std::{collections::BTreeSet, fmt::Debug, hash::Hash};

use crate::{
    context::CtxtRef,
    define_id,
    frontend::{
        ast,
        hir::{self, HirId},
        ty_infer::InferVar,
    },
    span::symbol::Symbol,
    types::format::TypeFormat,
};
use fxhash::FxHashSet;
use indexmap::IndexSet;

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
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum GenericArg {
    Type(Type),
    Origin(Origin),
}
impl GenericArg {
    pub const fn new_ty(ty: Type) -> Self {
        Self::Type(ty)
    }
    pub const fn new_origin(origin: Origin) -> Self {
        Self::Origin(origin)
    }
    pub fn as_ty(&self) -> Option<&Type> {
        let GenericArg::Type(ty) = self else {
            return None;
        };
        Some(ty)
    }
    pub fn as_origin(&self) -> Option<&Origin> {
        let GenericArg::Origin(origin) = self else {
            return None;
        };
        Some(origin)
    }
    pub fn expect_type(&self) -> &Type {
        let GenericArg::Type(ty) = self else {
            panic!("Expected a type got '{:?}'.", self)
        };
        ty
    }
    pub fn expect_origin(&self) -> &Origin {
        let GenericArg::Origin(origin) = self else {
            panic!("Expected an origin got '{:?}'.", self)
        };
        origin
    }
}
impl From<Origin> for GenericArg {
    fn from(value: Origin) -> Self {
        Self::Origin(value)
    }
}
impl From<Type> for GenericArg {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericArgs {
    args: Vec<GenericArg>,
}
impl GenericArgs {
    pub fn new(args: Vec<GenericArg>) -> Self {
        Self { args }
    }
    pub const fn empty() -> Self {
        Self { args: Vec::new() }
    }
    pub const fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
    pub const fn len(&self) -> usize {
        self.args.len()
    }
    pub fn iter(&self) -> impl Iterator<Item = &GenericArg> {
        self.args.iter()
    }
}
impl IntoIterator for GenericArgs {
    type IntoIter = std::vec::IntoIter<GenericArg>;
    type Item = GenericArg;
    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}
impl<'a> IntoIterator for &'a GenericArgs {
    type IntoIter = std::slice::Iter<'a, GenericArg>;
    type Item = &'a GenericArg;
    fn into_iter(self) -> Self::IntoIter {
        self.args.iter()
    }
}
impl FromIterator<GenericArg> for GenericArgs {
    fn from_iter<T: IntoIterator<Item = GenericArg>>(iter: T) -> Self {
        Self {
            args: Vec::from_iter(iter),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq, Debug, Copy, PartialOrd, Ord)]
pub enum Place {
    Var(Symbol, HirId),
    Generic(Symbol, u32),
    Err,
}
#[derive(Clone, Debug, Eq)]
pub struct Origin(Vec<Place>, BTreeSet<Place>);

impl Origin {
    pub const STATIC: Self = Self(Vec::new(), BTreeSet::new());
    pub const fn is_static(&self) -> bool {
        self.0.is_empty()
    }
    pub fn is_subset_of(&self, other: &Self) -> bool {
        self.1.is_subset(&other.1)
    }
    pub fn superset_of(&self, other: &Self) -> Self {
        let self_set = self.0.iter().collect::<IndexSet<_>>();
        let other_set = other.0.iter().collect::<IndexSet<_>>();
        self_set.union(&other_set).copied().copied().collect()
    }
    pub fn places(&self) -> impl Iterator<Item = &Place> {
        self.0.iter()
    }
}
impl FromIterator<Place> for Origin {
    fn from_iter<T: IntoIterator<Item = Place>>(iter: T) -> Self {
        let places = iter.into_iter().collect::<Vec<_>>();
        let place_set = places.iter().copied().collect();
        Self(places, place_set)
    }
}
impl From<Place> for Origin {
    fn from(value: Place) -> Self {
        Origin(vec![value], [value].into_iter().collect())
    }
}
impl PartialEq for Origin {
    fn eq(&self, other: &Self) -> bool {
        self.0.iter().collect::<FxHashSet<_>>() == other.0.iter().collect()
    }
}
impl Hash for Origin {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0.len());
        for elem in self.0.iter().collect::<FxHashSet<_>>() {
            elem.hash(state);
        }
    }
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Primitive(hir::PrimitiveType),
    Nominal(hir::Definition, GenericArgs),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Ref(Box<Type>, Origin, IsMutable),
    Generic(Symbol, u32),
    Array(Box<Type>),
    Infer(InferVar),
    Err,
}
impl Type {
    pub fn is_str(&self) -> bool {
        matches!(self, Type::Primitive(hir::PrimitiveType::String))
    }
    pub fn is_unit(&self) -> bool {
        matches!(self,Type::Tuple(elements) if elements.is_empty())
    }
    pub fn is_infer(&self) -> bool{
        matches!(self, Type::Infer(_))
    }
    pub fn is_never(&self) -> bool {
        matches!(self, Type::Primitive(hir::PrimitiveType::Never))
    }
    pub fn new_array(element: Self) -> Self {
        Self::Array(Box::new(element))
    }
    pub fn format(&self, ctxt: CtxtRef) -> String {
        TypeFormat::new(ctxt).format_type(self)
    }
    pub fn infer_vars(&self) -> FxHashSet<InferVar> {
        struct InferVarCollect {
            vars: FxHashSet<InferVar>,
        }
        impl TypeVisitor for InferVarCollect {
            fn visit_ty(&mut self, ty: &Type) {
                if let &Type::Infer(var) = ty {
                    self.vars.insert(var);
                }
                walk_ty(self, ty);
            }
        }
        let mut vars_collect = InferVarCollect {
            vars: FxHashSet::default(),
        };
        vars_collect.visit_ty(self);
        vars_collect.vars
    }
    pub fn has_infer(&self) -> bool {
        struct HasInfer {
            found: bool,
        }
        impl TypeVisitor for HasInfer {
            fn visit_ty(&mut self, ty: &Type) {
                if self.found {
                    return;
                }
                if let &Type::Infer(_) = ty {
                    self.found = true;
                } else {
                    walk_ty(self, ty);
                }
            }
        }
        let mut has_infer = HasInfer { found: false };
        has_infer.visit_ty(self);
        has_infer.found
    }
    pub fn has_error(&self) -> bool {
        struct HasError {
            found: bool,
        }
        impl TypeVisitor for HasError {
            fn visit_ty(&mut self, ty: &Type) {
                if self.found {
                    return;
                }
                if let Type::Err = ty {
                    self.found = true;
                }
                walk_ty(self, ty);
            }
        }
        let mut has_error = HasError { found: false };
        has_error.visit_ty(self);
        has_error.found
    }
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Err)
    }
    pub fn new_ref_str() -> Self {
        Self::new_ref(
            Self::new_primative(hir::PrimitiveType::String),
            Origin::STATIC,
            IsMutable::No,
        )
    }
    pub fn new_ref(ty: Type, origin: impl Into<Origin>, mutable: IsMutable) -> Self {
        Self::Ref(Box::new(ty), origin.into(), mutable)
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
    pub fn new_tuple_from_iter(iter: impl IntoIterator<Item = Type>) -> Self {
        Self::Tuple(iter.into_iter().collect())
    }
}

pub trait TypeVisitor {
    fn visit_ty(&mut self, ty: &Type);
}
pub fn walk_ty(v: &mut impl TypeVisitor, ty: &Type) {
    match ty {
        Type::Array(ty) => v.visit_ty(ty),
        Type::Tuple(fields) => fields.iter().for_each(|ty| v.visit_ty(ty)),
        Type::Function(params, return_ty) => params
            .iter()
            .chain(std::iter::once(&**return_ty))
            .for_each(|ty| v.visit_ty(ty)),
        Type::Nominal(_, args) => args
            .iter()
            .filter_map(|arg| arg.as_ty())
            .for_each(|ty| v.visit_ty(ty)),
        Type::Ref(ty, _, _) => v.visit_ty(ty),
        Type::Generic(..) => (),
        Type::Infer(..) => (),
        Type::Err | Type::Primitive(_) => (),
    }
}

pub fn super_map_ty<M: TypeMapper>(mapper: &M, ty: &Type) -> Result<Type, M::Error> {
    Ok(match ty {
        &Type::Infer(var) => Type::Infer(var),
        Type::Array(ty) => Type::new_array(mapper.map_ty(ty)?),
        &Type::Ref(ref ty, ref origin, mutable) => {
            Type::new_ref(mapper.map_ty(ty)?, mapper.map_origin(origin)?, mutable)
        }
        Type::Function(params, return_ty) => Type::new_function(
            params
                .iter()
                .map(|ty| mapper.map_ty(ty))
                .collect::<Result<Vec<_>, _>>()?,
            mapper.map_ty(return_ty)?,
        ),
        Type::Primitive(prim) => Type::new_primative(*prim),
        Type::Tuple(fields) => Type::new_tuple_from_iter(
            fields
                .iter()
                .map(|ty| mapper.map_ty(ty))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Type::Generic(name, index) => Type::Generic(*name, *index),
        Type::Nominal(def, args) => Type::Nominal(*def, mapper.map_generic_args(args)?),
        Type::Err => Type::Err,
    })
}
pub trait TypeMapper: Sized {
    type Error;
    fn map_origin(&self, origin: &Origin) -> Result<Origin, Self::Error>;
    fn map_generic_arg(&self, arg: &GenericArg) -> Result<GenericArg, Self::Error> {
        match arg {
            GenericArg::Origin(origin) => self.map_origin(&origin).map(GenericArg::Origin),
            GenericArg::Type(ty) => self.map_ty(ty).map(GenericArg::Type),
        }
    }
    fn map_generic_args(&self, args: &GenericArgs) -> Result<GenericArgs, Self::Error> {
        args.iter()
            .map(|arg| self.map_generic_arg(arg))
            .collect::<Result<_, _>>()
    }
    fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error>;
}
impl<T: TypeMapper> TypeMapper for &T {
    type Error = T::Error;
    fn map_origin(&self, origin: &Origin) -> Result<Origin, Self::Error> {
        (*self).map_origin(origin)
    }
    fn map_ty(&self, ty: &Type) -> Result<Type, <T as TypeMapper>::Error> {
        (*self).map_ty(ty)
    }
}
pub enum InfallibleMap {}
pub struct TypeScheme {
    ty: Type,
    arg_count: u32,
}
impl TypeScheme {
    pub fn arg_count(&self) -> u32 {
        self.arg_count
    }
    pub fn new(ty: Type, param_count: u32) -> Self {
        Self {
            ty,
            arg_count: param_count,
        }
    }
    pub fn skip_instantiate(self) -> Type {
        self.ty
    }
    ///Replaces all generic parameters using generic arguments
    pub fn instantiate(self, args: GenericArgs) -> Type {
        if args.is_empty() {
            return self.ty;
        }
        struct InstanceArgs {
            args: Vec<GenericArg>,
        }

        impl TypeMapper for InstanceArgs {
            type Error = InfallibleMap;
            fn map_origin(&self, origin: &Origin) -> Result<Origin, Self::Error> {
                let places = &origin.0;
                Ok(places
                    .iter()
                    .map(|place| match place {
                        place @ (Place::Err | Place::Var(..)) => vec![place],
                        Place::Generic(_, index) => self
                            .args
                            .get(*index as usize)
                            .and_then(|arg| arg.as_origin())
                            .map(|origin| origin.0.iter().collect())
                            .unwrap_or(Vec::new()),
                    })
                    .flatten()
                    .cloned()
                    .collect())
            }
            fn map_ty(&self, ty: &Type) -> Result<Type, InfallibleMap> {
                if let &Type::Generic(_, index) = ty {
                    Ok(self
                        .args
                        .get(index as usize)
                        .and_then(|arg| arg.as_ty())
                        .cloned()
                        .unwrap_or(Type::Err))
                } else {
                    super_map_ty(self, ty)
                }
            }
        }
        let Ok(result) = InstanceArgs {
            args: args.into_iter().collect(),
        }
        .map_ty(&self.ty);
        result
    }
}
impl From<Type> for TypeScheme {
    fn from(value: Type) -> Self {
        Self {
            ty: value,
            arg_count: 0,
        }
    }
}

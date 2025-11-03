use std::{fmt::Debug, hash::Hash};

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
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum GenericArg {
    Type(Type),
    Region(Region),
}
impl GenericArg {
    pub const fn new_ty(ty: Type) -> Self {
        Self::Type(ty)
    }
    pub const fn new_region(region: Region) -> Self {
        Self::Region(region)
    }
    pub fn as_ty(&self) -> Option<&Type> {
        let GenericArg::Type(ty) = self else {
            return None;
        };
        Some(ty)
    }
    pub fn as_region(&self) -> Option<&Region> {
        let GenericArg::Region(region) = self else {
            return None;
        };
        Some(region)
    }
    pub fn expect_type(&self) -> &Type {
        let GenericArg::Type(ty) = self else {
            panic!("Expected a type got '{:?}'.", self)
        };
        ty
    }
    pub fn expect_region(&self) -> &Region {
        let GenericArg::Region(region) = self else {
            panic!("Expected an region got '{:?}'.", self)
        };
        region
    }
}
impl From<Region> for GenericArg {
    fn from(value: Region) -> Self {
        Self::Region(value)
    }
}
impl From<Type> for GenericArg {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
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
#[derive(Clone, Debug, Eq, PartialEq, Copy, Hash)]
pub enum Region {
    Infer(InferVar),
    Local(Symbol, HirId),
    Static,
    Generic(Symbol, u32),
    Err,
}
impl Region {
    pub const fn is_static(&self) -> bool {
        matches!(self, Region::Static)
    }
}
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Type {
    Primitive(hir::PrimitiveType),
    Nominal(hir::Definition, GenericArgs),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Ref(Box<Type>, Region, IsMutable),
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
    pub fn is_infer(&self) -> bool {
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
    pub fn region_vars(&self) -> FxHashSet<InferVar> {
        struct InferVarCollect {
            vars: FxHashSet<InferVar>,
        }
        impl TypeVisitor for InferVarCollect {
            fn visit_region(&mut self, region: &Region) {
                if let Region::Infer(var) = region {
                    self.vars.insert(*var);
                }
            }
            fn visit_ty(&mut self, ty: &Type) {
                walk_ty(self, ty);
            }
        }
        let mut vars_collect = InferVarCollect {
            vars: FxHashSet::default(),
        };
        vars_collect.visit_ty(self);
        vars_collect.vars
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
            Region::Static,
            IsMutable::No,
        )
    }
    pub fn new_ref(ty: Type, region: impl Into<Region>, mutable: IsMutable) -> Self {
        Self::Ref(Box::new(ty), region.into(), mutable)
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
    fn visit_region(&mut self, _region: &Region) {}
}
pub fn walk_ty(v: &mut impl TypeVisitor, ty: &Type) {
    match ty {
        Type::Array(ty) => v.visit_ty(ty),
        Type::Tuple(fields) => fields.iter().for_each(|ty| v.visit_ty(ty)),
        Type::Function(params, return_ty) => params
            .iter()
            .chain(std::iter::once(&**return_ty))
            .for_each(|ty| v.visit_ty(ty)),
        Type::Nominal(_, args) => args.iter().for_each(|arg| match arg {
            GenericArg::Region(region) => v.visit_region(region),
            GenericArg::Type(ty) => v.visit_ty(ty),
        }),
        Type::Ref(ty, region, _) => {
            v.visit_ty(ty);
            v.visit_region(region);
        }
        Type::Generic(..) => (),
        Type::Infer(..) => (),
        Type::Err | Type::Primitive(_) => (),
    }
}

pub fn super_map_ty<M: TypeMapper>(mapper: &M, ty: &Type) -> Result<Type, M::Error> {
    Ok(match ty {
        &Type::Infer(var) => Type::Infer(var),
        Type::Array(ty) => Type::new_array(mapper.map_ty(ty)?),
        &Type::Ref(ref ty, ref region, mutable) => {
            Type::new_ref(mapper.map_ty(ty)?, mapper.map_region(region)?, mutable)
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
    fn map_region(&self, region: &Region) -> Result<Region, Self::Error>;
    fn map_generic_arg(&self, arg: &GenericArg) -> Result<GenericArg, Self::Error> {
        match arg {
            GenericArg::Region(region) => self.map_region(&region).map(GenericArg::Region),
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
    fn map_region(&self, region: &Region) -> Result<Region, Self::Error> {
        (*self).map_region(region)
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
            fn map_region(&self, region: &Region) -> Result<Region, Self::Error> {
                Ok(match region {
                    Region::Local(name, id) => Region::Local(*name, *id),
                    Region::Err => Region::Err,
                    Region::Static => Region::Static,
                    Region::Infer(infer) => Region::Infer(*infer),
                    Region::Generic(_, index) => self
                        .args
                        .get(*index as usize)
                        .and_then(|arg| arg.as_region())
                        .cloned()
                        .unwrap_or(Region::Static),
                })
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

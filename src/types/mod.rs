use fxhash::FxHashSet;

use crate::{
    context::CtxtRef,
    define_id,
    frontend::{ast, hir, ty_infer::InferVar},
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
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FieldType {
    pub name: Symbol,
    pub ty: Type,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct GenericArg(pub Type);
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct VariantCase {
    pub name: Symbol,
    pub fields: Vec<Type>,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
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
impl FromIterator<GenericArg> for GenericArgs {
    fn from_iter<T: IntoIterator<Item = GenericArg>>(iter: T) -> Self {
        Self {
            args: Vec::from_iter(iter),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Type {
    Primitive(hir::PrimitiveType),
    Nominal(hir::Definition, GenericArgs),
    Struct(IndexVec<FieldIndex, FieldType>),
    Variant(IndexVec<VariantCaseIndex, VariantCase>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Ref(Box<Type>, IsMutable),
    Generic(symbol::Symbol, u32),
    Array(Box<Type>),
    Infer(InferVar),
    Err,
}
impl Type {
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
                }
                else {
                    walk_ty(self, ty);
                }
            }
        }
        let mut has_infer = HasInfer {
            found: false,
        };
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
    pub fn new_struct(fields: impl IntoIterator<Item = (Symbol, Type)>) -> Self {
        Self::Struct(IndexVec::from_iter(
            fields.into_iter().map(|(name, ty)| FieldType { name, ty }),
        ))
    }
    pub fn new_variants(cases: impl IntoIterator<Item = (Symbol, Vec<Type>)>) -> Self {
        Self::Variant(IndexVec::from_iter(
            cases
                .into_iter()
                .map(|(name, fields)| VariantCase { name, fields }),
        ))
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
        Type::Struct(fields) => fields.iter().for_each(|field| v.visit_ty(&field.ty)),
        Type::Variant(cases) => cases
            .iter()
            .for_each(|case| case.fields.iter().for_each(|field| v.visit_ty(field))),
        Type::Function(params, return_ty) => params
            .iter()
            .chain(std::iter::once(&**return_ty))
            .for_each(|ty| v.visit_ty(ty)),
        Type::Nominal(_, args) => args.iter().for_each(|GenericArg(ty)| v.visit_ty(ty)),
        Type::Ref(ty, _) => v.visit_ty(ty),
        Type::Generic(..) => (),
        Type::Infer(..) => (),
        Type::Err | Type::Primitive(_) => (),
    }
}

pub fn super_map_ty<M: TypeMapper>(mapper: &M, ty: &Type) -> Result<Type, M::Error> {
    Ok(match ty {
        &Type::Infer(var) => Type::Infer(var),
        Type::Array(ty) => Type::new_array(mapper.map_ty(ty)?),
        &Type::Ref(ref ty, mutable) => Type::new_ref(mapper.map_ty(ty)?, mutable),
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
        Type::Variant(cases) => Type::new_variants(
            cases
                .iter()
                .map(|case| {
                    Ok((
                        case.name,
                        case.fields
                            .iter()
                            .map(|field| mapper.map_ty(field))
                            .collect::<Result<Vec<_>, _>>()?,
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Type::Struct(fields) => Type::new_struct(
            fields
                .iter()
                .map(|field| Ok((field.name, mapper.map_ty(&field.ty)?)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        Type::Nominal(def, args) => Type::Nominal(*def, mapper.map_generic_args(args)?),
        Type::Err => Type::Err,
    })
}
pub trait TypeMapper: Sized {
    type Error;
    fn map_generic_args(&self, args: &GenericArgs) -> Result<GenericArgs, Self::Error> {
        args.iter()
            .map(|arg| Ok(GenericArg(self.map_ty(&arg.0)?)))
            .collect::<Result<_, _>>()
    }
    fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error>;
}
impl<T: TypeMapper> TypeMapper for &T {
    type Error = T::Error;
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
            args: Vec<Type>,
        }

        impl TypeMapper for InstanceArgs {
            type Error = InfallibleMap;
            fn map_ty(&self, ty: &Type) -> Result<Type, InfallibleMap> {
                if let &Type::Generic(_, index) = ty {
                    Ok(self.args.get(index as usize).cloned().unwrap_or(Type::Err))
                } else {
                    super_map_ty(self, ty)
                }
            }
        }
        let Ok(result) = InstanceArgs {
            args: args.args.into_iter().map(|GenericArg(ty)| ty).collect(),
        }
        .map_ty(&self.ty);
        result
    }
}

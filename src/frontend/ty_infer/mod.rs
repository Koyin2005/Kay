use std::cell::RefCell;

use fxhash::FxHashSet;

use crate::{
    span::Span,
    types::{super_map_ty, walk_ty, GenericArg, Type, TypeMapper, TypeVisitor},
};
#[derive(Debug)]
struct TypeVar{
    current_ty : Option<Type>,
    source : Span
}

pub type InferResult<T> = Result<T, InferError>;
#[derive(Debug)]
pub enum InferError {
    UnifyFailed,
    InfiniteType,
}
#[derive(Debug)]
pub struct TypeInfer {
    vars: RefCell<Vec<TypeVar>>,
}

impl TypeInfer {
    pub fn new() -> Self{
        Self { vars: RefCell::new(Vec::new()) }
    }
    pub fn fresh_var(&self, span: Span) -> u32 {
        self.vars.borrow_mut().push(TypeVar { current_ty: None, source: span });
        (self.vars.borrow().len() - 1)
            .try_into()
            .expect("SHould never have too many type variables")
    }
    pub fn span(&self, var: u32) -> Span{
        self.vars.borrow()[var as usize].source
    }
    pub fn var_count(&self) -> u32{
        self.vars.borrow().len().try_into().expect("Shouldn't have more than u32::MAX type vars")
    }
    pub fn completed(self) -> bool {
        self.vars.into_inner().iter().all(|ty_var| ty_var.current_ty.is_some())
    }
    pub fn normalize(&self, ty: &Type) -> Type {
        struct Normalizer<'infer> {
            infer: &'infer TypeInfer,
        }
        impl TypeMapper for Normalizer<'_> {
            type Error = std::convert::Infallible;
            fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
                match ty {
                    &Type::Infer(index) => Ok(self
                        .infer
                        .vars
                        .borrow()
                        .get(index as usize)
                        .and_then(|ty| ty.current_ty.as_ref())
                        .map(|ty| self.infer.normalize(ty))
                        .unwrap_or(ty.clone())),
                    _ => super_map_ty(self, ty),
                }
            }
        }
        let Ok(ty) = Normalizer { infer: self }.map_ty(ty);
        ty
    }
    ///Try to unify a ty with an expected ty
    pub fn unify(&self, ty: &Type, expected: &Type) -> InferResult<Type> {
        let norm_ty = self.normalize(ty);
        let norm_exp = self.normalize(expected);
        match (norm_ty, norm_exp) {
            (ty, expected) if ty == expected => Ok(ty.clone()),
            (ty, Type::Infer(index)) | (Type::Infer(index), ty) => {
                struct OccursCheck {
                    var: u32,
                    found: bool,
                }
                impl TypeVisitor for OccursCheck {
                    fn visit_ty(&mut self, ty: &Type) {
                        match ty {
                            Type::Infer(var) => {
                                if *var == self.var {
                                    self.found = true;
                                }
                            }
                            _ => walk_ty(self, ty),
                        }
                    }
                }
                let mut occurs = OccursCheck {
                    var: index,
                    found: false,
                };
                occurs.visit_ty(&ty);
                let var = &mut self.vars.borrow_mut()[index as usize];
                if occurs.found {
                    var.current_ty = Some(Type::Err);
                    Err(InferError::InfiniteType)
                } else if let Some(curr_ty) = &var.current_ty {
                    self.unify(&curr_ty, &ty)
                } else {
                    var.current_ty = Some(ty.clone());
                    Ok(ty)
                }
            }
            (Type::Err, _) | (_, Type::Err) => Ok(Type::Err),
            (Type::Array(element), Type::Array(other_element)) => {
                Ok(Type::new_array(self.unify(&*element, &*other_element)?))
            }
            (Type::Tuple(elements), Type::Tuple(other_elements))
                if elements.len() == other_elements.len() =>
            {
                Ok(Type::new_tuple_from_iter(
                    elements
                        .iter()
                        .zip(other_elements)
                        .map(|(ty, other)| self.unify(ty, &other))
                        .collect::<InferResult<Vec<_>>>()?,
                ))
            }
            (Type::Function(params, return_ty), Type::Function(other_params, other_return_ty))
                if params.len() == other_params.len() =>
            {
                Ok(Type::new_function(
                    params
                        .iter()
                        .zip(other_params)
                        .map(|(param, other_param)| self.unify(param, &other_param))
                        .collect::<InferResult<Vec<_>>>()?,
                    self.unify(&return_ty, &other_return_ty)?,
                ))
            }
            (Type::Ref(ty, mutable), Type::Ref(other_ty, other_mutable))
                if mutable == other_mutable =>
            {
                Ok(Type::new_ref(self.unify(&*ty, &*other_ty)?, mutable))
            }
            (Type::Struct(fields), Type::Struct(other_fields))
                if fields.len() == other_fields.len() && {
                    let all_fields = FxHashSet::from_iter(fields.iter().map(|field| field.name));
                    other_fields
                        .iter()
                        .all(|field| all_fields.contains(&field.name))
                } =>
            {
                Ok(Type::new_struct(
                    fields
                        .iter()
                        .zip(other_fields)
                        .map(|(field, other_field)| {
                            self.unify(&field.ty, &other_field.ty)
                                .map(|ty| (field.name, ty))
                        })
                        .collect::<InferResult<Vec<_>>>()?,
                ))
            }
            (Type::Variant(cases), Type::Variant(other_cases))
                if cases.len() == other_cases.len()
                    && cases
                        .iter()
                        .zip(other_cases.iter())
                        .all(|(case, other_case)| case.fields.len() == other_case.fields.len())
                    && {
                        let all_cases = FxHashSet::from_iter(cases.iter().map(|case| case.name));
                        other_cases
                            .iter()
                            .all(|other_case| all_cases.contains(&other_case.name))
                    } =>
            {
                Ok(Type::new_variants(
                    cases
                        .iter()
                        .zip(other_cases)
                        .map(|(case, other_case)| {
                            Ok((
                                case.name,
                                case.fields
                                    .iter()
                                    .zip(other_case.fields.iter())
                                    .map(|(field, other_field)| self.unify(field, other_field))
                                    .collect::<InferResult<Vec<_>>>()?,
                            ))
                        })
                        .collect::<InferResult<Vec<_>>>()?,
                ))
            }
            (Type::Nominal(def, generic_args), Type::Nominal(other_def, other_generic_args))
                if def == other_def && generic_args.len() == other_generic_args.len() =>
            {
                Ok(Type::Nominal(
                    def,
                    generic_args
                        .iter()
                        .zip(other_generic_args.iter())
                        .map(|(arg, other_arg)| self.unify(&arg.0, &other_arg.0).map(GenericArg))
                        .collect::<InferResult<_>>()?,
                ))
            }
            _ => Err(InferError::UnifyFailed),
        }
    }
}

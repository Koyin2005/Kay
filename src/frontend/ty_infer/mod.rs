use std::cell::RefCell;

use fxhash::FxHashSet;

use crate::types::{GenericArg, Type, TypeMapper, super_map_ty};

pub type InferResult<T> = Result<T, InferError>;
#[derive(Debug)]
pub enum InferError {
    UnifyFailed,
}
#[derive(Debug)]
pub struct TypeInfer {
    vars: RefCell<Vec<Option<Type>>>,
}

impl TypeInfer {
    pub fn new_with_count(count: u32) -> Self {
        Self {
            vars: RefCell::new((0..count).map(|_| (None)).collect()),
        }
    }
    pub fn completed(self) -> bool {
        self.vars.into_inner().iter().all(Option::is_some)
    }
    pub fn normalize(&self, ty: &Type) -> Option<Type> {
        struct Normalizer<'infer> {
            infer: &'infer TypeInfer,
        }
        impl TypeMapper for Normalizer<'_> {
            type Error = ();
            fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
                match ty {
                    &Type::Generic(_, index) => Ok(self
                        .infer
                        .vars
                        .borrow()
                        .get(index as usize)
                        .and_then(|ty| ty.clone())
                        .ok_or(())?),
                    _ => super_map_ty(self, ty),
                }
            }
        }
        Normalizer { infer: self }.map_ty(ty).ok()
    }
    ///Try to unify a ty with an expected ty
    pub fn unify(&self, ty: &Type, expected: &Type) -> InferResult<Type> {
        match (ty, expected) {
            (ty, Type::Generic(_, index)) => {
                let curr_ty = &mut self.vars.borrow_mut()[*index as usize];
                if let Some(curr_ty) = curr_ty
                    && curr_ty == ty
                {
                    Ok(curr_ty.clone())
                } else if curr_ty.is_some() {
                    Err(InferError::UnifyFailed)
                } else {
                    *curr_ty = Some(ty.clone());
                    Ok(ty.clone())
                }
            }
            (ty, expected) if ty == expected => Ok(ty.clone()),
            (Type::Err, _) | (_, Type::Err) => Ok(Type::Err),
            (Type::Array(element), Type::Array(other_element)) => {
                Ok(Type::new_array(self.unify(element, other_element)?))
            }
            (Type::Tuple(elements), Type::Tuple(other_elements))
                if elements.len() == other_elements.len() =>
            {
                Ok(Type::new_tuple_from_iter(
                    elements
                        .iter()
                        .zip(other_elements)
                        .map(|(ty, other)| self.unify(ty, other))
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
                        .map(|(param, other_param)| self.unify(param, other_param))
                        .collect::<InferResult<Vec<_>>>()?,
                    self.unify(return_ty, other_return_ty)?,
                ))
            }
            (Type::Ref(ty, mutable), Type::Ref(other_ty, other_mutable))
                if mutable == other_mutable =>
            {
                Ok(Type::new_ref(self.unify(ty, other_ty)?, *mutable))
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
                        .zip(other_cases)
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
                    *def,
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

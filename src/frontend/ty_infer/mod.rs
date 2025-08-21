use std::cell::RefCell;

use fxhash::FxHashSet;

use crate::{
    span::Span,
    types::{GenericArg, Type, TypeMapper, TypeVisitor, super_map_ty},
};

pub type InferResult<T> = Result<T, InferError>;
pub enum InferError {
    InfiniteType,
    UnifyFailed,
}
pub struct TypeInfer {
    vars: RefCell<Vec<(Option<Type>, Span,bool)>>,
}

impl TypeInfer {
    pub fn new() -> Self {
        Self {
            vars: RefCell::new(Vec::new()),
        }
    }
    pub fn new_var(&self, span: Span) -> u32 {
        self.vars.borrow_mut().push((None, span,false));
        self.vars
            .borrow()
            .len()
            .wrapping_sub(1)
            .try_into()
            .expect("Should be less than u32::MAX type vars.")
    }
    pub fn var_span(&self, var: u32) -> Span {
        self.vars.borrow()[var as usize].1
    }
    pub fn var_count(&self) -> u32 {
        self.vars
            .borrow()
            .len()
            .try_into()
            .expect("Can't have more than u32::MAX vars")
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
                        .and_then(|(ty, _,_)| ty.as_ref())
                        .map(|ty| self.infer.normalize(ty))
                        .unwrap_or(Type::Infer(index))),
                    _ => super_map_ty(self, ty),
                }
            }
        }
        let Ok(ty) = Normalizer { infer: self }.map_ty(ty);
        ty
    }
    pub fn unify(&self, ty: &Type, other: &Type) -> InferResult<Type> {
        match (ty, other) {
            (Type::Generic(_, _), Type::Generic(_, _))
            | (Type::Infer(_), Type::Infer(_))
            | (Type::Primitive(_), Type::Primitive(_))
                if ty == other =>
            {
                Ok(ty.clone())
            }
            (&Type::Infer(var), &Type::Infer(other)) => {
                let min_var = var.min(other);
                let max_var = var.max(other);
                let ty = self.vars.borrow_mut()[min_var as usize].0.clone();
                let other_ty = self.vars.borrow_mut()[max_var as usize].0.clone();
                match (ty, other_ty) {
                    (Some(ty), Some(other_ty)) => self.unify(&ty, &other_ty),
                    (Some(ty), None) => self.unify(&Type::Infer(max_var), &ty),
                    (None, Some(ty)) => self.unify(&ty, &Type::Infer(min_var)),
                    (None, None) => {
                        self.vars.borrow_mut()[min_var as usize].0 = Some(Type::Infer(max_var));
                        Ok(Type::Infer(max_var))
                    }
                }
            }
            (Type::Infer(index), ty) | (ty, Type::Infer(index)) => {
                struct OccursCheck {
                    var: u32,
                    found_var: bool,
                }
                impl TypeVisitor for OccursCheck {
                    fn visit_ty(&mut self, ty: &Type) {
                        use crate::types::walk_ty;
                        match ty {
                            &Type::Infer(other_var) => {
                                if self.var == other_var {
                                    self.found_var = true;
                                }
                            }
                            _ => walk_ty(self, ty),
                        }
                    }
                }

                let curr_ty = &mut self.vars.borrow_mut()[*index as usize].0;
                if let Some(curr_ty) = curr_ty
                    && curr_ty == ty
                {
                    Ok(curr_ty.clone())
                } else if curr_ty.is_some() {
                    Err(InferError::UnifyFailed)
                } else {
                    let mut occurs_check = OccursCheck {
                        var: *index,
                        found_var: false,
                    };
                    occurs_check.visit_ty(ty);
                    if occurs_check.found_var {
                        *curr_ty = Some(Type::Err);
                        return Err(InferError::InfiniteType);
                    }
                    *curr_ty = Some(ty.clone());
                    Ok(ty.clone())
                }
            }
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

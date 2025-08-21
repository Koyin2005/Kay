use std::cell::RefCell;

use fxhash::FxHashSet;

use crate::{
    span::Span,
    types::{GenericArg, Type, TypeMapper, super_map_ty},
};

pub struct TypeInfer {
    vars: RefCell<Vec<(Option<Type>, Span)>>,
}

impl TypeInfer {
    pub fn new() -> Self {
        Self {
            vars: RefCell::new(Vec::new()),
        }
    }
    pub fn new_var(&self, span: Span) -> u32 {
        self.vars.borrow_mut().push((None, span));
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

    pub fn normalize(&self, ty: &Type) -> Option<Type> {
        struct Normalizer<'infer> {
            infer: &'infer TypeInfer,
        }
        impl TypeMapper for Normalizer<'_> {
            type Error = ();
            fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
                match ty {
                    &Type::Infer(index) => self
                        .infer
                        .vars
                        .borrow()
                        .get(index as usize)
                        .and_then(|(ty, _)| ty.as_ref())
                        .cloned()
                        .ok_or(()),
                    _ => super_map_ty(self, ty),
                }
            }
        }
        Normalizer { infer: self }.map_ty(ty).ok()
    }
    pub fn unify(&self, ty: &Type, other: &Type) -> Option<Type> {
        match (ty, other) {
            (Type::Generic(_, _), Type::Generic(_, _))
            | (Type::Infer(_), Type::Infer(_))
            | (Type::Primitive(_), Type::Primitive(_))
                if ty == other =>
            {
                Some(ty.clone())
            }
            (Type::Infer(index), ty) | (ty, Type::Infer(index)) => {
                let curr_ty = &mut self.vars.borrow_mut()[*index as usize].0;
                if let Some(curr_ty) = curr_ty
                    && curr_ty == ty
                {
                    Some(curr_ty.clone())
                } else if curr_ty.is_some() {
                    None
                } else {
                    *curr_ty = Some(ty.clone());
                    Some(ty.clone())
                }
            }
            (Type::Err, _) | (_, Type::Err) => Some(Type::Err),
            (Type::Array(element), Type::Array(other_element)) => {
                Some(Type::new_array(self.unify(element, other_element)?))
            }
            (Type::Tuple(elements), Type::Tuple(other_elements))
                if elements.len() == other_elements.len() =>
            {
                Some(Type::new_tuple_from_iter(
                    elements
                        .iter()
                        .zip(other_elements)
                        .map(|(ty, other)| self.unify(ty, other))
                        .collect::<Option<Vec<_>>>()?,
                ))
            }
            (Type::Function(params, return_ty), Type::Function(other_params, other_return_ty))
                if params.len() == other_params.len() =>
            {
                Some(Type::new_function(
                    params
                        .iter()
                        .zip(other_params)
                        .map(|(param, other_param)| self.unify(param, other_param))
                        .collect::<Option<Vec<_>>>()?,
                    self.unify(return_ty, other_return_ty)?,
                ))
            }
            (Type::Ref(ty, mutable), Type::Ref(other_ty, other_mutable))
                if mutable == other_mutable =>
            {
                Some(Type::new_ref(self.unify(ty, other_ty)?, *mutable))
            }
            (Type::Struct(fields), Type::Struct(other_fields))
                if fields.len() == other_fields.len() && {
                    let all_fields = FxHashSet::from_iter(fields.iter().map(|field| field.name));
                    other_fields
                        .iter()
                        .all(|field| all_fields.contains(&field.name))
                } =>
            {
                Some(Type::new_struct(
                    fields
                        .iter()
                        .zip(other_fields)
                        .map(|(field, other_field)| {
                            self.unify(&field.ty, &other_field.ty)
                                .map(|ty| (field.name, ty))
                        })
                        .collect::<Option<Vec<_>>>()?,
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
                Some(Type::new_variants(
                    cases
                        .iter()
                        .zip(other_cases)
                        .map(|(case, other_case)| {
                            Some((
                                case.name,
                                case.fields
                                    .iter()
                                    .zip(other_case.fields.iter())
                                    .map(|(field, other_field)| self.unify(field, other_field))
                                    .collect::<Option<Vec<_>>>()?,
                            ))
                        })
                        .collect::<Option<Vec<_>>>()?,
                ))
            }
            (Type::Nominal(def, generic_args), Type::Nominal(other_def, other_generic_args))
                if def == other_def =>
            {
                Some(Type::Nominal(
                    *def,
                    generic_args
                        .iter()
                        .zip(other_generic_args.iter())
                        .map(|(arg, other_arg)| self.unify(&arg.0, &other_arg.0).map(GenericArg))
                        .collect::<Option<_>>()?,
                ))
            }
            _ => None,
        }
    }
}

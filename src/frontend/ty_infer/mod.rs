use fxhash::FxHashSet;

use crate::types::{GenericArg, Type, TypeMapper, super_map_ty};

pub struct TypeInfer {
    vars: Box<[Option<Type>]>,
}

impl TypeInfer {
    pub fn new(vars: u32) -> Self {
        Self {
            vars: Box::from_iter((0..vars).map(|_| None)),
        }
    }
    pub fn completed(self) -> bool {
        self.vars.iter().all(Option::is_some)
    }
    pub fn normalize(&self, ty: &Type) -> Option<Type> {
        struct Normalizer<'infer> {
            infer: &'infer TypeInfer,
        }
        impl TypeMapper for Normalizer<'_> {
            type Error = ();
            fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
                match ty {
                    &Type::Generic(_, index) => self
                        .infer
                        .vars
                        .get(index as usize)
                        .and_then(|ty| ty.as_ref())
                        .cloned()
                        .ok_or(()),
                    _ => super_map_ty(self, ty),
                }
            }
        }
        Normalizer { infer: self }.map_ty(ty).ok()
    }
    pub fn unify(&mut self, ty: &Type, other: &Type) -> Option<Type> {
        match (ty, other) {
            (Type::Generic(_, _), Type::Generic(_, _)) => None,
            (Type::Generic(_, index), ty) | (ty, Type::Generic(_, index)) => {
                self.vars[*index as usize] = Some(ty.clone());
                Some(ty.clone())
            }
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
            (Type::Primitive(primative), Type::Primitive(other_primative))
                if primative == other_primative =>
            {
                Some(Type::new_primative(*primative))
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

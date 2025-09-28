use std::cell::RefCell;

use crate::{
    span::Span,
    types::{GenericArg, Type, TypeMapper, TypeVisitor, super_map_ty, walk_ty},
};
#[derive(Debug)]
struct TypeVar {
    current_ty: Option<Type>,
    source: Span,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferVar {
    index: u32,
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
    pub fn new() -> Self {
        Self {
            vars: RefCell::new(Vec::new()),
        }
    }
    pub fn fresh_var(&self, span: Span) -> InferVar {
        self.vars.borrow_mut().push(TypeVar {
            current_ty: None,
            source: span,
        });
        let index = (self.vars.borrow().len() - 1)
            .try_into()
            .expect("SHould never have too many type variables");
        InferVar { index }
    }
    pub fn span(&self, var: InferVar) -> Span {
        self.vars.borrow()[var.index as usize].source
    }
    pub fn vars(&self) -> impl ExactSizeIterator<Item = InferVar> {
        (0u32..self
            .vars
            .borrow()
            .len()
            .try_into()
            .expect("Shouldn't have more than u32::MAX type vars"))
            .map(|i| InferVar { index: i })
    }
    pub fn normalize(&self, ty: &Type) -> Type {
        struct Normalizer<'infer> {
            infer: &'infer TypeInfer,
        }
        impl TypeMapper for Normalizer<'_> {
            type Error = std::convert::Infallible;
            fn map_origin(
                &self,
                origin: &crate::types::Origin,
            ) -> Result<crate::types::Origin, Self::Error> {
                Ok(origin.clone())
            }
            fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
                match ty {
                    &Type::Infer(var) => Ok(self
                        .infer
                        .vars
                        .borrow()
                        .get(var.index as usize)
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
    fn unfiy_generic_arg(
        &self,
        arg: &GenericArg,
        expected: &GenericArg,
    ) -> InferResult<GenericArg> {
        match (arg, expected) {
            (GenericArg::Type(ty), GenericArg::Type(expected)) => {
                self.unify(ty, expected).map(GenericArg::Type)
            }
            (GenericArg::Origin(origin), GenericArg::Origin(expected)) if origin == expected => {
                Ok(GenericArg::Origin(origin.clone()))
            }
            _ => Err(InferError::UnifyFailed),
        }
    }
    ///Try to unify a ty with an expected ty
    pub fn unify(&self, ty: &Type, expected: &Type) -> InferResult<Type> {
        let norm_ty = self.normalize(ty);
        let norm_exp = self.normalize(expected);
        match (norm_ty, norm_exp) {
            (ty, expected) if ty == expected => Ok(ty.clone()),
            (ty, Type::Infer(var)) | (Type::Infer(var), ty) => {
                struct OccursCheck {
                    var: InferVar,
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
                let mut occurs = OccursCheck { var, found: false };
                occurs.visit_ty(&ty);
                let var_info = &mut self.vars.borrow_mut()[var.index as usize];
                if occurs.found {
                    var_info.current_ty = Some(Type::Err);
                    Err(InferError::InfiniteType)
                } else if let Some(curr_ty) = &var_info.current_ty {
                    self.unify(curr_ty, &ty)
                } else {
                    var_info.current_ty = Some(ty.clone());
                    Ok(ty)
                }
            }
            (Type::Err, _) | (_, Type::Err) => Ok(Type::Err),
            (Type::Array(element), Type::Array(other_element)) => {
                Ok(Type::new_array(self.unify(&element, &other_element)?))
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
            (Type::Ref(ty, origin, mutable), Type::Ref(other_ty, other_origin, other_mutable))
                if mutable == other_mutable && origin == other_origin =>
            {
                Ok(Type::new_ref(self.unify(&ty, &other_ty)?, origin, mutable))
            }
            (Type::Nominal(def, generic_args), Type::Nominal(other_def, other_generic_args))
                if def == other_def && generic_args.len() == other_generic_args.len() =>
            {
                Ok(Type::Nominal(
                    def,
                    generic_args
                        .iter()
                        .zip(other_generic_args.iter())
                        .map(|(arg, other_arg)| self.unfiy_generic_arg(arg, other_arg))
                        .collect::<InferResult<_>>()?,
                ))
            }
            _ => Err(InferError::UnifyFailed),
        }
    }
}

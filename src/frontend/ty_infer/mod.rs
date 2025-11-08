use std::cell::RefCell;

use crate::{
    span::Span,
    types::{GenericArg, Region, Type, TypeMapper, TypeVisitor, super_map_ty, walk_ty},
};
#[derive(Debug)]
struct TypeVar {
    current_ty: Option<Type>,
    source: Span,
}
#[derive(Debug)]
struct RegionVar {
    current_region: Option<Region>,
    source: Span,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InferVar {
    index: u32,
    from_region: bool,
}
pub type InferResult<T> = Result<T, InferError>;
#[derive(Debug)]
pub enum InferError {
    UnifyFailed,
    InfiniteType,
}
#[derive(Debug)]
pub struct TypeInfer {
    ty_vars: RefCell<Vec<TypeVar>>,
    region_vars: RefCell<Vec<RegionVar>>,
}

impl TypeInfer {
    pub fn new() -> Self {
        Self {
            ty_vars: RefCell::new(Vec::new()),
            region_vars: RefCell::new(Vec::new()),
        }
    }
    pub fn fresh_var(&self, span: Span) -> InferVar {
        self.ty_vars.borrow_mut().push(TypeVar {
            current_ty: None,
            source: span,
        });
        let index = (self.ty_vars.borrow().len() - 1)
            .try_into()
            .expect("SHould never have too many type variables");
        InferVar {
            index,
            from_region: false,
        }
    }
    pub fn fresh_region_var(&self, span: Span) -> InferVar {
        self.region_vars.borrow_mut().push(RegionVar {
            current_region: None,
            source: span,
        });
        let index = (self.region_vars.borrow().len() - 1)
            .try_into()
            .expect("SHould never have too many type variables");
        InferVar {
            index,
            from_region: true,
        }
    }
    pub fn span(&self, var: InferVar) -> Span {
        if var.from_region {
            self.region_vars.borrow()[var.index as usize].source
        } else {
            self.ty_vars.borrow()[var.index as usize].source
        }
    }
    pub fn ty_vars(&self) -> impl ExactSizeIterator<Item = InferVar> {
        (0u32..self
            .ty_vars
            .borrow()
            .len()
            .try_into()
            .expect("Shouldn't have more than u32::MAX type vars"))
            .map(|i| InferVar {
                index: i,
                from_region: false,
            })
    }
    pub fn region_vars(&self) -> impl ExactSizeIterator<Item = InferVar> {
        (0u32..self
            .region_vars
            .borrow()
            .len()
            .try_into()
            .expect("Shouldn't have more than u32::MAX region vars"))
            .map(|i| InferVar {
                index: i,
                from_region: true,
            })
    }
    pub fn normalize_region(&self, region: &Region) -> Region {
        let Ok(region) = Normalizer { infer: self }.map_region(region);
        region
    }
    pub fn normalize(&self, ty: &Type) -> Type {
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
            (GenericArg::Region(region), GenericArg::Region(expected)) => {
                self.unify_region(region, expected).map(GenericArg::Region)
            }
            _ => Err(InferError::UnifyFailed),
        }
    }
    pub fn unify_region(&self, region: &Region, other: &Region) -> InferResult<Region> {
        match (region, other) {
            (Region::Err, _) | (_, Region::Err) => Ok(Region::Err),
            (Region::Static, Region::Static) => Ok(Region::Static),
            (Region::Generic(name, index), Region::Generic(other_name, other_index))
                if name == other_name && index == other_index =>
            {
                Ok(Region::Generic(*name, *index))
            }

            (Region::Local(index), Region::Local(other_index))
                if index == other_index =>
            {
                Ok(Region::Local(*index))
            }
            (Region::Infer(var), region) | (region, Region::Infer(var)) => {
                let mut occurs_check = OccursCheck {
                    var: *var,
                    found: false,
                };
                occurs_check.visit_region(region);
                let var_info = &mut self.region_vars.borrow_mut()[var.index as usize];
                if occurs_check.found {
                    var_info.current_region = Some(Region::Err);
                    Err(InferError::InfiniteType)
                } else if let Some(curr_ty) = &var_info.current_region {
                    self.unify_region(curr_ty, &region)
                } else {
                    var_info.current_region = Some(region.clone());
                    Ok(region.clone())
                }
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
                let mut occurs = OccursCheck { var, found: false };
                occurs.visit_ty(&ty);
                let var_info = &mut self.ty_vars.borrow_mut()[var.index as usize];
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
            (Type::Ref(ty, region, mutable), Type::Ref(other_ty, other_region, other_mutable))
                if mutable == other_mutable =>
            {
                Ok(Type::new_ref(
                    self.unify(&ty, &other_ty)?,
                    self.unify_region(&region, &other_region)?,
                    mutable,
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
                        .map(|(arg, other_arg)| self.unfiy_generic_arg(arg, other_arg))
                        .collect::<InferResult<_>>()?,
                ))
            }
            _ => Err(InferError::UnifyFailed),
        }
    }
}

struct OccursCheck {
    var: InferVar,
    found: bool,
}
impl TypeVisitor for OccursCheck {
    fn visit_region(&mut self, region: &Region) {
        if let Region::Infer(var) = region {
            if *var == self.var {
                self.found = true;
            }
        }
    }
    fn visit_ty(&mut self, ty: &Type) {
        if self.found {
            return;
        }
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

struct Normalizer<'infer> {
    infer: &'infer TypeInfer,
}
impl TypeMapper for Normalizer<'_> {
    type Error = std::convert::Infallible;
    fn map_region(
        &self,
        region: &crate::types::Region,
    ) -> Result<crate::types::Region, Self::Error> {
        match region {
            Region::Infer(var) => Ok(self
                .infer
                .region_vars
                .borrow()
                .get(var.index as usize)
                .and_then(|ty| ty.current_region.as_ref())
                .map(|region| self.infer.normalize_region(region))
                .unwrap_or(region.clone())),
            _ => Ok(region.clone()),
        }
    }
    fn map_ty(&self, ty: &Type) -> Result<Type, Self::Error> {
        match ty {
            &Type::Infer(var) => Ok(self
                .infer
                .ty_vars
                .borrow()
                .get(var.index as usize)
                .and_then(|ty| ty.current_ty.as_ref())
                .map(|ty| self.infer.normalize(ty))
                .unwrap_or(ty.clone())),
            _ => super_map_ty(self, ty),
        }
    }
}

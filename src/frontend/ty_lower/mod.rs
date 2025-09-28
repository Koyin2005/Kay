use crate::{
    context::CtxtRef,
    frontend::{
        hir::{self, DefId},
        ty_infer::TypeInfer,
    },
    span::Span,
    types::{GenericArg, GenericArgs, Origin, Place, Type},
};
pub struct TypeLower<'ctxt> {
    ctxt: CtxtRef<'ctxt>,
    infer: Option<&'ctxt TypeInfer>,
}
impl<'a> TypeLower<'a> {
    pub fn new(ctxt: CtxtRef<'a>, infer: Option<&'a TypeInfer>) -> Self {
        Self { ctxt, infer }
    }
    pub fn lower_function_sig(&self, sig: &hir::FunctionSig) -> (impl Iterator<Item = Type>, Type) {
        (
            sig.inputs.iter().map(|param| self.lower(param)),
            sig.output
                .as_ref()
                .map(|ty| self.lower(ty))
                .unwrap_or(Type::new_unit()),
        )
    }
    pub fn lower_ty_path(
        &self,
        path: &hir::Path,
        generic_args: Option<&hir::GenericArgs>,
        span: Span,
    ) -> Type {
        let def = match path.res {
            hir::Resolution::Def(
                id,
                hir::DefKind::Struct | hir::DefKind::Variant | hir::DefKind::TypeParam,
            ) => id,
            hir::Resolution::Err => return Type::Err,
            _ => {
                self.ctxt.diag().emit_diag(
                    format!("Cannot use '{}' as a type.", path.res.as_str()),
                    span,
                );
                return Type::Err;
            }
        };
        let generic_args = self.lower_generic_args_for(def, generic_args, span);
        self.ctxt.type_of(def.into()).instantiate(generic_args)
    }
    fn lower_origin(&self, origin: &hir::Origin) -> Origin {
        Origin::from_iter(origin.places.iter().map(|place| {
            match place {
                &hir::Place::Err => Place::Err,
                &hir::Place::Param(name, id) => Place::Generic(
                    name.symbol,
                    self.ctxt
                        .generics_for(self.ctxt.expect_parent_of_def(id.into()))
                        .expect_index(name.symbol),
                ),
                &hir::Place::Var(name, id) => Place::Var(name.symbol, id),
            }
        }))
    }
    fn lower_generic_arg(&self, generic_arg: &hir::GenericArg) -> GenericArg {
        match generic_arg {
            hir::GenericArg::Origin(origin) => GenericArg::Origin(self.lower_origin(origin)),
            hir::GenericArg::Type(ty) => GenericArg::Type(self.lower(ty)),
        }
    }
    pub fn lower_generic_args_for(
        &self,
        id: DefId,
        generic_args: Option<&hir::GenericArgs>,
        span: Span,
    ) -> GenericArgs {
        let generics = self.ctxt.generics_for(id.into());
        match (generic_args, generics.count()) {
            (None, 0) => GenericArgs::empty(),
            (None, _) => {
                if let Some(infer) = self.infer {
                    generics
                        .params
                        .iter()
                        .map(|param| match param.kind {
                            hir::GenericParamKind::Type => {
                                GenericArg::Type(Type::Infer(infer.fresh_var(span)))
                            }
                            hir::GenericParamKind::Origin => {
                                self.ctxt
                                    .diag()
                                    .emit_diag("Cannot infer 'origin' here.", span);
                                GenericArg::Origin(Origin::from(Place::Err))
                            }
                        })
                        .collect()
                } else {
                    self.ctxt
                        .diag()
                        .emit_diag("Cannot infer 'generic' parameters.", span);
                    generics
                        .params
                        .iter()
                        .map(|param| match param.kind {
                            hir::GenericParamKind::Type => GenericArg::Type(Type::Err),
                            hir::GenericParamKind::Origin => {
                                GenericArg::Origin(Origin::from(Place::Err))
                            }
                        })
                        .collect()
                }
            }
            (Some(args), _) => {
                let mut params = generics.params().peekable();
                let mut passed_args = args.args.iter().peekable();
                let mut args = Vec::new();
                loop {
                    let arg = match (passed_args.peek(), params.peek()) {
                        (Some(arg), Some(param)) => match (arg, param.kind) {
                            (hir::GenericArg::Type(ty), hir::GenericParamKind::Type) => {
                                GenericArg::Type(self.lower(ty))
                            }
                            (hir::GenericArg::Origin(origin), hir::GenericParamKind::Origin) => {
                                GenericArg::Origin(self.lower_origin(origin))
                            }

                            (arg, param) => {
                                self.ctxt.diag().emit_diag(
                                    format!(
                                        "Expected '{}' parameter got '{}'.",
                                        param.kind(),
                                        arg.kind()
                                    ),
                                    span,
                                );
                                match param {
                                    hir::GenericParamKind::Origin => {
                                        GenericArg::Origin(Origin::from(Place::Err))
                                    }
                                    hir::GenericParamKind::Type => GenericArg::Type(Type::Err),
                                }
                            }
                        },
                        (Some(_), None) | (None, Some(_)) | (None, None) => {
                            break;
                        }
                    };
                    args.push(arg);
                    passed_args.next();
                    params.next();
                }
                if passed_args.peek().is_some() {
                    self.ctxt
                        .diag()
                        .emit_diag("Too many generic arguments.", span);
                    for arg in passed_args {
                        let _ = self.lower_generic_arg(arg);
                    }
                }

                if params.peek().is_some() {
                    self.ctxt
                        .diag()
                        .emit_diag("Not enough generic arguments.", span);
                }

                GenericArgs::new(args)
            }
        }
    }
    pub fn lower(&self, ty: &hir::Type) -> Type {
        match &ty.kind {
            hir::TypeKind::Infer => {
                if let Some(infer) = self.infer {
                    Type::Infer(infer.fresh_var(ty.span))
                } else {
                    self.ctxt.diag().emit_diag("Cannot use '_' here.", ty.span);
                    Type::Err
                }
            }
            hir::TypeKind::Array(element_ty) => Type::new_array(self.lower(element_ty)),
            &hir::TypeKind::Ref(mutable, ref origin, ref ty) => Type::new_ref(
                self.lower(ty),
                origin
                    .as_ref()
                    .map(|origin| self.lower_origin(&origin))
                    .unwrap_or(Origin::STATIC),
                mutable.into(),
            ),
            hir::TypeKind::Tuple(elements) => {
                Type::new_tuple_from_iter(elements.iter().map(|ty| self.lower(ty)))
            }
            &hir::TypeKind::Primitive(primative) => Type::new_primative(primative),
            hir::TypeKind::Path(path, args) => self.lower_ty_path(path, args.as_ref(), ty.span),
            hir::TypeKind::Fun(params, return_ty) => Type::new_function(
                params.iter().map(|ty| self.lower(ty)),
                return_ty
                    .as_ref()
                    .map(|ty| self.lower(ty))
                    .unwrap_or(Type::new_unit()),
            ),
        }
    }
}

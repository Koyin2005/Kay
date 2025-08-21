use crate::{
    context::CtxtRef,
    frontend::{hir, ty_infer::TypeInfer},
    span::Span,
    types::{GenericArg, Type},
};

pub struct NotAType;
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
    pub fn lower_ty_path(&self, path: &hir::Path, span: Span) -> Result<Type, NotAType> {
        Ok(match path.res {
            hir::Resolution::Builtin(builtin) => {
                Type::new_nominal(hir::Definition::Builtin(builtin))
            }
            hir::Resolution::Def(
                id,
                hir::DefKind::Struct | hir::DefKind::Variant | hir::DefKind::GenericParam,
            ) => {
                let scheme = self.ctxt.type_of(hir::Definition::Def(id));
                let arg_count = scheme.arg_count();
                if let Some(infer) = self.infer {
                    scheme.instantiate(
                        (0..arg_count)
                            .map(|_| GenericArg(Type::Infer(infer.new_var(span))))
                            .collect(),
                    )
                } else if arg_count == 0 {
                    scheme.skip_instantiate()
                } else {
                    self.ctxt
                        .diag()
                        .emit_diag("Cannot infer generic parameters.", span);
                    scheme.instantiate((0..arg_count).map(|_| GenericArg(Type::Err)).collect())
                }
            }
            hir::Resolution::Err => Type::Err,
            _ => return Err(NotAType),
        })
    }
    pub fn lower(&self, ty: &hir::Type) -> Type {
        match &ty.kind {
            hir::TypeKind::Array(element_ty) => Type::new_array(self.lower(element_ty)),
            &hir::TypeKind::Ref(mutable, ref ty) => Type::new_ref(self.lower(ty), mutable.into()),
            hir::TypeKind::Tuple(elements) => {
                Type::new_tuple_from_iter(elements.iter().map(|ty| self.lower(ty)))
            }
            hir::TypeKind::Struct(fields) => Type::new_struct(
                fields
                    .iter()
                    .map(|field| (field.name.symbol, self.lower(&field.ty))),
            ),
            hir::TypeKind::Variant(cases) => Type::new_variants(cases.iter().map(|case| {
                (
                    case.name.symbol,
                    case.fields
                        .iter()
                        .flatten()
                        .map(|ty| self.lower(ty))
                        .collect(),
                )
            })),
            &hir::TypeKind::Primitive(primative) => Type::new_primative(primative),
            hir::TypeKind::Path(path) => self.lower_ty_path(path, ty.span).unwrap_or_else(|_| {
                self.ctxt.diag().emit_diag(
                    format!("Cannot use '{}' as a type.", path.res.as_str()),
                    ty.span,
                );
                Type::Err
            }),
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

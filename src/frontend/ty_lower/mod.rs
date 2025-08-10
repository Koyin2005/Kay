use crate::{context::CtxtRef, frontend::hir, types::Type};

pub struct TypeLower<'ctxt> {
    ctxt: CtxtRef<'ctxt>,
    in_sig: bool,
}
impl<'a> TypeLower<'a> {
    pub fn new(ctxt: CtxtRef<'a>, in_sig: bool) -> Self {
        Self { ctxt, in_sig }
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
    pub fn lower(&self, ty: &hir::Type) -> Type {
        match &ty.kind {
            hir::TypeKind::Infer => {
                if self.in_sig {
                    self.ctxt
                        .diag()
                        .emit_diag("Cannot use '_' inside a signature.", ty.span);
                    Type::Err
                } else {
                    todo!("LOWER INFER TYPE")
                }
            }
            hir::TypeKind::Array(_ty) => todo!("LOWER ARRAY TYPE"),
            hir::TypeKind::Ref(_mut, _ty) => todo!("LOWER REF TYPE"),
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
                    case.fields.iter().map(|ty| self.lower(ty)).collect(),
                )
            })),
            &hir::TypeKind::Primitive(primative) => Type::new_primative(primative),
            hir::TypeKind::Path(..) => todo!("LOWER PATH TYPE"),
        }
    }
}

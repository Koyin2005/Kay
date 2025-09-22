use crate::{
    context::CtxtRef,
    frontend::hir::{IntType, PrimitiveType},
    types::{IsMutable, Type},
};

pub struct TypeFormat<'a> {
    context: CtxtRef<'a>,
}
impl<'a> TypeFormat<'a> {
    pub fn new(ctxt: CtxtRef<'a>) -> Self {
        Self { context: ctxt }
    }
    fn format_multiple<'b, T: 'b>(
        &self,
        tys: impl IntoIterator<Item = &'b T>,
        f: &impl Fn(&'b T) -> String,
    ) -> String {
        let mut first = true;
        let mut output = String::new();
        for elem in tys {
            if !first {
                output.push(',');
            }
            output.push_str(&f(elem));
            first = false;
        }
        output
    }
    fn format_types<'b>(&self, tys: impl IntoIterator<Item = &'b Type>) -> String {
        self.format_multiple(tys, &|ty| self.format_type(ty))
    }
    pub fn format_type(&self, ty: &Type) -> String {
        match ty {
            Type::Infer(_) => "_".to_string(),
            Type::Generic(name, _) => name.as_str().to_string(),
            Type::Array(element) => format!("[{}]", self.format_type(element)),
            Type::Err => "{unknown}".to_string(),
            Type::Ref(ty,origin, is_mutable) => format!(
                "ref{} [{}] {}",
                if let IsMutable::Yes = is_mutable {
                    " mut"
                } else {
                    ""
                },
                if let Some(origin) = origin{
                    origin.0.as_str()
                }
                else{
                    "static"
                },
                self.format_type(ty)
            ),
            Type::Function(params, return_type) => format!(
                "fun ({}) -> {}",
                self.format_types(params),
                self.format_type(return_type)
            ),
            Type::Tuple(args) => format!("({})", self.format_types(args)),
            &Type::Nominal(def, ref args) => format!(
                "{}{}",
                self.context.symbol(def).as_str(),
                if args.is_empty() {
                    ""
                } else {
                    &format!(
                        "[{}]",
                        self.format_types(args.args.iter().map(|arg| &arg.0))
                    )
                }
            ),
            Type::Primitive(ty) => match ty {
                PrimitiveType::Bool => "bool",
                PrimitiveType::Int(signed) => match signed {
                    IntType::Signed => "int",
                    IntType::Unsigned => "uint",
                },
                PrimitiveType::Never => "never",
                PrimitiveType::String => "string",
            }
            .to_string(),
        }
    }
}

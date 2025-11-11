use crate::{
    context::CtxtRef,
    frontend::hir::{IntType, PrimitiveType},
    types::{GenericArg, IsMutable, Region, Type},
};

pub struct TypeFormat<'a> {
    context: CtxtRef<'a>,
    output: String,
}
impl<'a> TypeFormat<'a> {
    pub const fn take(&mut self) -> String {
        std::mem::replace(&mut self.output, String::new())
    }
    pub const fn new(ctxt: CtxtRef<'a>) -> Self {
        Self {
            context: ctxt,
            output: String::new(),
        }
    }
    fn format_multiple<'b, T: 'b>(
        &mut self,
        elements: impl IntoIterator<Item = &'b T>,
        f: &impl Fn(&mut Self, &'b T),
    ) {
        let mut first = true;
        for elem in elements {
            if !first {
                self.write(",");
            }
            f(self, elem);
            first = false;
        }
    }
    fn write(&mut self, string: &str) {
        self.output.push_str(string);
    }
    pub fn finish(self) -> String {
        self.output
    }
    pub fn format_region(&mut self, region: &Region) {
        match region {
            Region::Local(name, _) => {
                self.write("local ");
                self.write(name.as_str());
            }
            Region::Static => self.write("static"),
            Region::Infer(_) => self.write("_"),
            Region::Err => self.write("{unknown}"),
            Region::Generic(name, _) => {
                self.write("generic ");
                self.write(name.as_str());
            }
        }
    }
    pub fn format_generic_args<'b>(&mut self, args: impl IntoIterator<Item = &'b GenericArg>) {
        self.format_multiple(args, &|this, arg| match arg {
            GenericArg::Type(ty) => this.format_type(ty),
            GenericArg::Region(region) => this.format_region(region),
        })
    }
    fn format_types<'b>(&mut self, tys: impl IntoIterator<Item = &'b Type>) {
        self.format_multiple(tys, &|this, ty| this.format_type(ty))
    }
    pub fn format_type(&mut self, ty: &Type) {
        match ty {
            Type::Infer(_) => self.write("_"),
            Type::Generic(name, _) => self.write(name.as_str()),
            Type::Array(element) => {
                self.write("[");
                self.format_type(&element);
                self.write("]");
            }
            Type::Err => self.write("{unknown}"),
            Type::Ref(ty, region, is_mutable) => {
                self.write(if let IsMutable::Yes = is_mutable {
                    "ref mut "
                } else {
                    "ref "
                });
                self.format_region(region);
                self.write(" ");
                self.format_type(ty);
            }
            Type::Function(params, return_type) => {
                self.write("fun (");
                self.format_types(params);
                self.write(") -> ");
                self.format_type(return_type);
            }
            Type::Tuple(args) => {
                self.write("(");
                self.format_types(args);
                self.write(")");
            }
            &Type::Nominal(def, ref args) => {
                self.write(self.context.symbol(def).as_str());
                if !args.is_empty() {
                    self.format_generic_args(args);
                }
            }
            Type::Primitive(ty) => self.write(match ty {
                PrimitiveType::Bool => "bool",
                PrimitiveType::Int(signed) => match signed {
                    IntType::Signed => "int",
                    IntType::Unsigned => "uint",
                },
                PrimitiveType::Never => "never",
                PrimitiveType::String => "string",
            }),
        }
    }
    pub fn apply_format(ctxt: CtxtRef<'a>, f: impl Fn(&mut Self)) -> String {
        let mut format = TypeFormat::new(ctxt);
        f(&mut format);
        format.finish()
    }
    pub fn region_to_string(ctxt: CtxtRef<'a>, region: &Region) -> String {
        Self::apply_format(ctxt, |this| {
            this.format_region(region);
        })
    }
    pub fn ty_to_string(ctxt: CtxtRef<'a>, ty: &Type) -> String {
        Self::apply_format(ctxt, |this| {
            this.format_type(ty);
        })
    }
}

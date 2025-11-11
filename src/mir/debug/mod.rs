use crate::{
    context::{CtxtRef, NodeInfo, TypeDefKind},
    frontend::hir::Definition,
    indexvec::Idx,
    mir::{
        AggregateKind, AssertMessage, BasicBlock, Body, BorrowKind, Constant, Local, LocalKind,
        Place, PlaceProjection, Rvalue, Stmt, StmtKind, Terminator, TerminatorKind, UnaryOp, Value,
    },
    types::{Type, format::TypeFormat},
};

pub struct DebugMir<'body> {
    output: String,
    body: &'body Body,
    ctxt: CtxtRef<'body>,
}

impl<'body> DebugMir<'body> {
    pub fn new(body: &'body Body, ctxt: CtxtRef<'body>) -> Self {
        Self {
            output: String::new(),
            body,
            ctxt,
        }
    }
    fn write_char(&mut self, c: char) {
        self.output.push(c);
    }
    fn write_space(&mut self) {
        self.output.push(' ');
    }
    fn write_coma(&mut self) {
        self.output.push(',');
    }
    fn write_newline(&mut self) {
        self.output.push('\n');
    }
    fn write(&mut self, txt: &str) {
        self.output.push_str(txt);
    }
    fn format_header(&mut self) {
        self.write("fun ");
        self.write(&self.ctxt.debug_name(self.body.info.id));

        self.write_char('(');
        let (params, return_ty) = self.ctxt.signature_of(self.body.info.id);
        let mut formatter = TypeFormat::new(self.ctxt);
        let mut first = true;
        for (i, param) in params.into_iter().enumerate() {
            let i = i + 1;
            if !first {
                self.write_coma();
            }
            self.write(&format!("{} : ", Local::new(i)));
            formatter.format_type(&param);
            self.write(&formatter.take());
            first = false;
        }
        self.write_char(')');
        self.write_space();
        self.write("-> ");
        formatter.format_type(&return_ty);
        self.write(&formatter.finish());
        self.write_space();
        self.write_char('{');
        self.write_newline();
    }
    fn format_place(&self, place: &Place) -> String {
        let mut string = format!("{}", place.local);
        let mut ty = self.body.locals[place.local].ty.clone();
        for projection in &place.projections {
            match projection {
                PlaceProjection::Deref => {
                    string = format!("{}^", string);
                }
                PlaceProjection::Field(field, case) => {
                    match ty {
                        Type::Tuple(_) => {
                            string.push('.');
                            string.push_str(&field.into_index().to_string());
                        }
                        Type::Nominal(def, _) => match &self.ctxt.type_def(def).kind {
                            TypeDefKind::Struct(struct_def) => {
                                string.push('.');
                                string.push_str(struct_def.fields[*field].name.as_str());
                            }
                            TypeDefKind::Variant(variant_cases) => {
                                let case = case.expect("There should be a case for field");
                                let (id, _) = &variant_cases[case];
                                let case_ident = self.ctxt.ident((*id).into());
                                let case_name = case_ident.symbol.as_str();
                                string =
                                    format!("({} as {}).{}", string, case_name, field.into_index());
                            }
                        },
                        _ => unreachable!("Can't get field for this ty {:?}", ty),
                    };
                }
                PlaceProjection::Index(index) => {
                    string.push('[');
                    string.push_str(&format!("{}", index));
                    string.push(']');
                }
            }
            ty = projection.apply_to(&ty, self.ctxt);
        }
        string
    }
    fn format_constant(&mut self, value: &Constant) {
        match value {
            Constant::Bool(value) => self.write(if *value { "true" } else { "false" }),
            Constant::Int(value) => self.write(&format!("{value}")),
            Constant::Named(id, args) => {
                if let NodeInfo::VariantCase(_) = self.ctxt.expect_node(*id) {
                    self.write(
                        self.ctxt
                            .ident(self.ctxt.expect_parent(*id))
                            .symbol
                            .as_str(),
                    );
                    self.write_char('.');
                }

                self.write(self.ctxt.ident(*id).symbol.as_str());
                if !args.is_empty() {
                    self.write_char('[');
                    let mut format = TypeFormat::new(self.ctxt);
                    format.format_generic_args(args);
                    self.write(&format.finish());
                    self.write_char(']');
                }
            }
            Constant::String(string) => {
                self.write_char('\"');
                self.write(string.as_str());
                self.write_char('\"');
            }
            Constant::ZeroSized(ty) => self.write(&TypeFormat::ty_to_string(self.ctxt, ty)),
        }
    }
    fn format_value(&mut self, value: &Value) {
        match value {
            Value::Constant(value) => self.format_constant(value),
            Value::Load(place) => {
                self.write(&self.format_place(place));
            }
        }
    }
    fn format_stmt(&mut self, stmt: &Stmt) {
        self.write_space();
        self.write_space();
        match &stmt.kind {
            StmtKind::Assign(place, rvalue) => {
                self.write(&self.format_place(place));
                self.write(" = ");
                match rvalue {
                    Rvalue::Use(value) => {
                        self.format_value(value);
                    }
                    Rvalue::Call(callee, args) => {
                        self.format_value(callee);
                        self.write_char('(');
                        let mut first = true;
                        for arg in args {
                            if !first {
                                self.write_char(',');
                            }
                            self.format_value(arg);
                            first = false;
                        }
                        self.write_char(')');
                    }
                    Rvalue::Binary(op, left_and_right) => {
                        let (left, right) = left_and_right.as_ref();
                        self.write(op.name());
                        self.write_char('(');
                        self.format_value(left);
                        self.write_coma();
                        self.format_value(right);
                        self.write_char(')');
                    }
                    Rvalue::Unary(op, value) => {
                        self.write(match op {
                            UnaryOp::Negate => "-",
                        });
                        self.format_value(value);
                    }
                    Rvalue::Discrimant(value) => {
                        self.write("discriminant(");
                        self.write(&self.format_place(value));
                        self.write_char(')');
                    }
                    Rvalue::Len(value) => {
                        self.write("len(");
                        self.write(&self.format_place(value));
                        self.write_char(')');
                    }
                    Rvalue::Ref(borrow_kind, region, place) => {
                        self.write("ref ");
                        match borrow_kind {
                            BorrowKind::Read => (),
                            BorrowKind::ReadWrite => {
                                self.write("mut ");
                            }
                        }
                        self.write_char('{');
                        self.write(&TypeFormat::region_to_string(self.ctxt, region));
                        self.write("} ");
                        self.write(&self.format_place(place));
                    }
                    Rvalue::Aggregate(kind, fields) => {
                        let format_fields = |this: &mut DebugMir, ty: &Type| match ty {
                            Type::Nominal(def, _) => {
                                if let Some(struct_def) = self.ctxt.type_def(*def).as_struct() {
                                    let field_defs = &struct_def.fields;
                                    let field_defs_with_field_vals = field_defs
                                        .iter()
                                        .map(|def| {
                                            let Definition::Def(id) = def.id;
                                            id
                                        })
                                        .zip(fields.iter());
                                    let mut first = true;
                                    for (field, value) in field_defs_with_field_vals {
                                        if !first {
                                            this.write_char(',');
                                        }
                                        this.write(self.ctxt.ident(field).symbol.as_str());
                                        this.write_char('=');
                                        this.format_value(value);
                                        first = false;
                                    }
                                } else {
                                    let mut first = true;
                                    for value in fields.iter() {
                                        if !first {
                                            this.write_char(',');
                                        }
                                        this.format_value(value);
                                        first = false;
                                    }
                                }
                            }
                            _ => {
                                let mut first = true;
                                for value in fields.iter() {
                                    if !first {
                                        this.write_char(',');
                                    }
                                    this.format_value(value);
                                    first = false;
                                }
                            }
                        };
                        let ty = place.type_of(&self.body.locals, self.ctxt);
                        match kind.as_ref() {
                            AggregateKind::Array => {
                                self.write_char('[');
                                format_fields(self, &ty);
                                self.write_char(']');
                            }
                            AggregateKind::Tuple => {
                                self.write_char('(');
                                format_fields(self, &ty);
                                self.write_char(')');
                            }
                            AggregateKind::Struct(id, args) => {
                                self.write(self.ctxt.ident(*id).symbol.as_str());
                                if !args.is_empty() {
                                    self.write_char('[');
                                    let mut format = TypeFormat::new(self.ctxt);
                                    format.format_generic_args(args);
                                    self.write(&format.finish());
                                    self.write_char(']');
                                }
                                self.write_space();
                                self.write_char('{');
                                format_fields(self, &ty);
                                self.write_char('}');
                            }
                            AggregateKind::Variant {
                                type_id,
                                args,
                                case_index,
                            } => {
                                self.write(self.ctxt.ident(*type_id).symbol.as_str());
                                self.write_char('.');
                                self.write(
                                    self.ctxt
                                        .ident(
                                            self.ctxt.type_def((*type_id).into()).cases()
                                                [*case_index]
                                                .0
                                                .into(),
                                        )
                                        .symbol
                                        .as_str(),
                                );
                                if !args.is_empty() {
                                    self.write_char('[');
                                    let mut format = TypeFormat::new(self.ctxt);
                                    format.format_generic_args(args);
                                    self.write(&format.take());
                                    self.write_char(']');
                                }
                                self.write_char('(');
                                format_fields(self, &ty);
                                self.write_char(')');
                            }
                        }
                    }
                }
                self.write_char(';');
            }
            StmtKind::Noop => {
                self.write("noop;");
            }
        }
        self.write_newline();
    }
    fn format_terminator(&mut self, terminator: &Terminator) {
        self.write_space();
        self.write_space();
        match terminator.kind {
            TerminatorKind::Goto(target) => {
                self.write("goto -> bb");
                self.write(&target.0.to_string());
                self.write_char(';');
            }
            TerminatorKind::Return => {
                self.write("return;");
            }
            TerminatorKind::Unreachable => self.write("unreachable;"),
            TerminatorKind::Switch(ref value, ref targets, otherwise) => {
                self.write("switch(");
                self.format_value(value);
                self.write_char(')');

                self.write(" -> ");
                self.write_char('[');
                let mut first = true;
                for (constant, target) in targets {
                    if !first {
                        self.write(", ");
                    }
                    self.format_constant(constant);
                    self.write(" : ");
                    self.write(&format!("bb{}", target.0));
                    first = false;
                }
                if !targets.is_empty() {
                    self.write(", ");
                }
                self.write("otherwise : ");
                self.write(&format!("bb{}", otherwise.0));
                self.write_char(']');
            }
            TerminatorKind::Assert(ref condition, is_true, ref message, target) => {
                self.write("assert(");
                if !is_true {
                    self.write_char('!');
                }
                self.format_value(condition);
                self.write_char(',');
                match message {
                    AssertMessage::BoundsCheck { index, len } => {
                        self.format_value(index);
                        self.write_coma();
                        self.format_value(len);
                        self.write_coma();
                        self.write("\"index {} out of bounds : the length is {} but index is {}\"");
                    }
                    AssertMessage::NoOverflow(op, left, right) => {
                        self.format_value(left);
                        self.write_coma();
                        self.format_value(right);
                        self.write_coma();
                        self.write("\"cannot compute {} '");
                        self.write(op.as_str());
                        self.write("' {}\"");
                    }
                    AssertMessage::NegOverflow(operand) => {
                        self.format_value(operand);
                        self.write_coma();
                        self.write("\"cannot compute -{}\"");
                    }
                    AssertMessage::DivisionByZero(left) => {
                        self.format_value(left);
                        self.write_coma();
                        self.write("\"attempted division of {} by 0\"");
                    }
                }
                self.write_char(')');
                self.write(" -> ");
                self.write(&format!("bb{}", target.0));
                self.write_char(';');
            }
            TerminatorKind::FalseEdge {
                real_target,
                false_target,
            } => {
                self.write("false_edge real -> ");
                self.write(&format!("bb{}, ", real_target.0));
                self.write("imaginary -> ");
                self.write(&format!("bb{}", false_target.0));
                self.write_char(';');
            }
        }
        self.write_newline();
    }
    fn format_block(&mut self, block: BasicBlock) {
        self.write_space();
        self.write(&format!("bb{}:", block.into_index()));
        self.write_newline();
        for stmt in &self.body.blocks[block].stmts {
            self.format_stmt(stmt);
        }
        if let Some(ref terminator) = self.body.blocks[block].terminator {
            self.format_terminator(terminator);
        }
    }
    fn format_body(&mut self) {
        for (local, info) in self.body.locals.iter_enumerated() {
            self.write_space();
            self.write(&format!(
                "{} : {}",
                local,
                TypeFormat::ty_to_string(self.ctxt, &info.ty)
            ));
            self.write_space();
            self.write_char('(');
            match info.kind {
                LocalKind::Param(name) => {
                    self.write("param");
                    if let Some(name) = name {
                        self.write_space();
                        self.write(name.as_str());
                    }
                }
                LocalKind::Return => {
                    self.write("return");
                }
                LocalKind::Temp => {
                    self.write("temp");
                }
                LocalKind::UserDefined(name) => {
                    self.write("var ");
                    self.write(name.as_str());
                }
            }
            self.write_char(')');
            self.write_newline();
        }
        for block in self.body.blocks.indices() {
            self.format_block(block);
        }
        self.write_char('}');
        self.write_newline();
    }
    pub fn output(mut self) -> String {
        self.format_header();
        self.format_body();
        self.output
    }
}

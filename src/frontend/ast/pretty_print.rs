use crate::frontend::ast::{
    Block, ByRef, Expr, ExprKind, ItemKind, IteratorExprKind, LiteralKind, Mutable, Pattern,
    PatternKind, Stmt, StmtKind, Type, TypeKind,
};

pub struct PrettyPrint<W> {
    writer: W,
    depth: usize,
}
impl<W: std::fmt::Write> PrettyPrint<W> {
    pub fn new(w: W) -> Self {
        Self {
            writer: w,
            depth: 0,
        }
    }
    fn print_depth(&mut self) -> std::fmt::Result {
        for _ in 0..self.depth {
            self.writer.write_char(' ')?;
        }
        Ok(())
    }
    fn increase_depth(&mut self) {
        self.depth += 1;
    }
    fn decrease_depth(&mut self) {
        self.depth -= 1;
    }
    fn print(&mut self, text: &str) -> std::fmt::Result {
        self.writer.write_str(text)
    }
    fn print_newline(&mut self) -> std::fmt::Result {
        self.writer.write_char('\n')
    }

    fn print_item(&mut self, item: &ItemKind) -> std::fmt::Result {
        match item {
            ItemKind::Function(function) => {
                self.print("function def\n")?;
                self.increase_depth();
                if !function.params.is_empty() {
                    self.print_depth()?;
                    self.print("function params\n")?;
                    self.increase_depth();
                    for param in function.params.iter() {
                        self.print_depth()?;
                        self.print_pattern(&param.pattern)?;
                        self.print_newline()?;
                        self.print_depth()?;
                        self.print_ty(&param.ty)?;
                        self.print_newline()?;
                    }
                    self.decrease_depth();
                }
                if let Some(return_type) = function.return_type.as_ref() {
                    self.print_depth()?;
                    self.print_ty(return_type)?;
                    self.print_newline()?;
                }
                self.print_depth()?;
                self.print_block(&function.body)?;
                self.decrease_depth();

                Ok(())
            }
        }
    }

    fn print_block(&mut self, block: &Block) -> std::fmt::Result {
        self.print("block\n")?;
        self.increase_depth();
        for (i, stmt) in block.stmts.iter().enumerate() {
            self.pretty_print_stmt(stmt, i + 1 < block.stmts.len())?;
        }
        self.decrease_depth();
        Ok(())
    }

    fn print_exprs(&mut self, elements: &[Expr]) -> std::fmt::Result {
        self.increase_depth();
        for (i, element) in elements.iter().enumerate() {
            self.print_depth()?;
            self.pretty_print_expr(element)?;
            if i + 1 < elements.len() {
                self.print_newline()?;
            }
        }
        self.decrease_depth();
        Ok(())
    }
    fn print_literal(&mut self, literal: &LiteralKind) -> std::fmt::Result {
        match literal {
            LiteralKind::Int(value) => self.print(&value.to_string()),
            &LiteralKind::Bool(value) => self.print(if value { "true" } else { "false" }),
            LiteralKind::String(value) => self.print(value.as_str()),
        }
    }
    fn pretty_print_expr(&mut self, expr: &Expr) -> std::fmt::Result {
        match &expr.kind {
            ExprKind::Literal(literal) => self.print_literal(literal),
            ExprKind::Ident(name) => self.print(name.as_str()),
            ExprKind::Assign(lhs, rhs, _) => {
                self.print("assign\n")?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(lhs)?;
                self.print_newline()?;

                self.print_depth()?;
                self.pretty_print_expr(rhs)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Binary(op, left, right) => {
                self.print(op.node.as_str())?;
                self.print_newline()?;

                self.increase_depth();

                self.print_depth()?;
                self.pretty_print_expr(left)?;
                self.print_newline()?;

                self.print_depth()?;
                self.pretty_print_expr(right)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Unary(op, operand) => {
                self.print(op.node.as_str())?;
                self.print_newline()?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(operand)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Deref(_, operand) => {
                self.print("deref")?;
                self.print_newline()?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(operand)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Call(callee, args) => {
                self.print("call\n")?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(callee)?;
                self.print_newline()?;

                self.print_exprs(args)?;
                Ok(())
            }
            ExprKind::Tuple(elements) => {
                self.print("tuple\n")?;
                self.print_exprs(elements)
            }
            ExprKind::Array(elements) => {
                self.print("array\n")?;
                self.print_exprs(elements)
            }
            ExprKind::Field(base, field) => {
                self.print("field\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(base)?;
                self.print_newline()?;

                self.print_depth()?;
                self.print(field.symbol.as_str())?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Grouped(expr) => {
                self.print("grouped\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(expr)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Block(block) => {
                self.print_block(block)?;
                Ok(())
            }
            ExprKind::While(condition, body) => {
                self.print("while\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(condition)?;
                self.print_newline()?;
                self.print_depth()?;
                self.print_block(body)?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::For(pat, iterator, body) => {
                self.print("for\n")?;
                self.increase_depth();

                self.print_depth()?;
                self.print("pat\n")?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(pat)?;
                self.print_newline()?;
                self.decrease_depth();

                self.print_depth()?;
                self.print("iterator\n")?;
                self.increase_depth();

                self.print_depth()?;

                match &iterator.kind {
                    IteratorExprKind::Range(start, end) => {
                        self.pretty_print_expr(start)?;
                        self.print_newline()?;

                        self.print_depth()?;
                        self.pretty_print_expr(end)?;
                    }
                    IteratorExprKind::Expr(expr) => self.pretty_print_expr(expr)?,
                }
                self.print_newline()?;
                self.decrease_depth();

                self.print_depth()?;
                self.print_block(body)?;
                self.print_newline()?;
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Break(expr) => {
                if let Some(expr) = expr {
                    self.print("break\n")?;

                    self.print_depth()?;
                    self.pretty_print_expr(expr)
                } else {
                    self.print("break")
                }
            }
            ExprKind::If(cond, then, else_branch) => {
                self.print("if")?;
                self.print_newline()?;

                self.increase_depth();

                self.print_depth()?;
                self.pretty_print_expr(cond)?;
                self.print_newline()?;

                self.print_depth()?;
                self.print_block(then)?;

                if let Some(else_branch) = else_branch {
                    self.print_newline()?;

                    self.print_depth()?;
                    self.print("else\n")?;

                    self.increase_depth();
                    self.print_depth()?;
                    self.pretty_print_expr(else_branch)?;
                    self.decrease_depth();
                }
                self.decrease_depth();
                Ok(())
            }
            ExprKind::Return(expr) => {
                self.print("return")?;
                if let Some(expr) = expr {
                    self.print_newline()?;
                    self.increase_depth();
                    self.print_depth()?;
                    self.pretty_print_expr(expr)?;
                }
                Ok(())
            }
            ExprKind::As(expr, ty) => {
                self.print("as\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(expr)?;
                self.print_newline()?;

                self.print_depth()?;
                self.print_ty(ty)?;
                self.decrease_depth();

                Ok(())
            }
        }
    }
    fn print_ty(&mut self, ty: &Type) -> std::fmt::Result {
        match ty.kind {
            TypeKind::Int => self.print("int"),
            TypeKind::Bool => self.print("bool"),
            TypeKind::Uint => self.print("uint"),
            TypeKind::Never => self.print("never"),
            TypeKind::Underscore => self.print("_"),
            TypeKind::Tuple(ref elements) => {
                self.print("tuple type\n")?;
                self.increase_depth();
                for (i, ty) in elements.iter().enumerate() {
                    self.print_depth()?;
                    self.print_ty(ty)?;
                    if i + 1 < elements.len() {
                        self.print_newline()?;
                    }
                }
                self.decrease_depth();
                Ok(())
            }
            TypeKind::Ref(ref ty) => {
                self.print("ref type\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.print_ty(ty)?;
                self.decrease_depth();
                Ok(())
            }
            TypeKind::Grouped(ref ty) => {
                self.print("grouped\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.print_ty(ty)?;
                self.decrease_depth();
                Ok(())
            }
        }
    }
    fn print_pattern(&mut self, pattern: &Pattern) -> std::fmt::Result {
        match &pattern.kind {
            PatternKind::Ident(name, mutable, by_ref) => {
                if let ByRef::Yes(_) = by_ref {
                    self.print("ref ")?;
                }
                if let Mutable::Yes(_) = mutable {
                    self.print("mut ")?;
                }
                self.print(name.as_str())
            }
            PatternKind::Deref(pattern) => {
                self.print("deref pat\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.print_pattern(pattern)
            }
            PatternKind::Wildcard => self.print("_"),
            PatternKind::Grouped(pattern) => {
                self.print("grouped pat\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.print_pattern(pattern)
            }
            PatternKind::Tuple(patterns) => {
                self.print("tuple pat\n")?;
                self.increase_depth();
                for (i, pattern) in patterns.iter().enumerate() {
                    self.print_depth()?;
                    self.print_pattern(pattern)?;
                    if i + 1 < patterns.len() {
                        self.print_newline()?;
                    }
                }
                self.decrease_depth();
                Ok(())
            }
            PatternKind::Literal(literal) => {
                self.print("literal pat\n")?;
                self.increase_depth();
                self.print_depth()?;
                self.print_literal(literal)?;
                self.decrease_depth();
                Ok(())
            }
        }
    }
    pub fn pretty_print_stmt(&mut self, stmt: &Stmt, newline: bool) -> std::fmt::Result {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.print_depth()?;
                self.print("expr")?;
                self.print_newline()?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(expr)?;
                self.decrease_depth();
            }
            StmtKind::ExprWithSemi(expr) => {
                self.print_depth()?;
                self.print("expr with semi")?;
                self.print_newline()?;

                self.increase_depth();
                self.print_depth()?;
                self.pretty_print_expr(expr)?;
                self.decrease_depth();
            }
            StmtKind::Let(pattern, ty, assigned) => {
                self.print_depth()?;
                self.print("let\n")?;

                self.increase_depth();
                self.print_depth()?;
                self.print_pattern(pattern)?;
                self.print_newline()?;

                if let Some(ty) = ty {
                    self.print_depth()?;
                    self.print_ty(ty)?;
                    self.print_newline()?;
                }

                self.print_depth()?;
                self.pretty_print_expr(assigned)?;
                self.decrease_depth();
            }
            StmtKind::Item(item) => {
                self.print_depth()?;
                self.print("item\n")?;

                self.increase_depth();
                self.print_depth()?;

                self.print_item(item)?;
                self.decrease_depth();
            }
        }
        //Only prints newline if requested
        if newline {
            self.print_newline()
        } else {
            Ok(())
        }
    }
    pub fn finish(self) -> W {
        self.writer
    }
}

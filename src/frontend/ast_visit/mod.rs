use crate::frontend::ast::{
    Ast, Block, Expr, ExprKind, Item, ItemKind, IteratorExpr, IteratorExprKind, Module, Pattern,
    PatternKind, Stmt, StmtKind, Type, TypeDefKind, TypeKind,
};

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }
    fn visit_ty(&mut self, ty: &Type) {
        walk_type(self, ty);
    }
    fn visit_item(&mut self, item: &Item) {
        walk_item(self, item);
    }
    fn visit_pat(&mut self, pat: &Pattern) {
        walk_pat(self, pat);
    }
    fn visit_block(&mut self, block: &Block) {
        walk_block(self, block);
    }
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => self.visit_expr(expr),
            StmtKind::ExprWithSemi(expr) => self.visit_expr(expr),
            StmtKind::Item(item) => self.visit_item(item),
            StmtKind::Let(pat, ty, expr) => {
                self.visit_let_stmt(pat, ty.as_ref().map(|ty| &**ty), expr);
            }
        }
    }
    fn visit_let_stmt(&mut self, pat: &Pattern, ty: Option<&Type>, expr: &Expr) {
        self.visit_pat(pat);
        if let Some(ty) = ty {
            self.visit_ty(ty);
        }
        self.visit_expr(expr);
    }
    fn visit_module(&mut self, module: &Module) {
        walk_module(self, module);
    }
    fn visit_ast(&mut self, ast: &Ast) {
        walk_ast(self, ast);
    }
}
pub fn walk_module(visitor: &mut impl Visitor, module: &Module) {
    for item in module.items.iter() {
        visitor.visit_item(item);
    }
}
pub fn walk_ast(visitor: &mut impl Visitor, ast: &Ast) {
    for module in ast.modules.iter() {
        visitor.visit_module(module);
    }
}
pub fn walk_item(visitor: &mut impl Visitor, item: &Item) {
    match &item.kind {
        ItemKind::Function(function) => {
            for param in function.params.iter() {
                visitor.visit_pat(&param.pattern);
            }
            for param in function.params.iter() {
                visitor.visit_ty(&param.ty);
            }
            if let Some(return_ty) = function.return_type.as_ref() {
                visitor.visit_ty(return_ty);
            }
            visitor.visit_expr(&function.body);
        }
        ItemKind::Type(ty) => match &ty.kind {
            TypeDefKind::Struct(struct_def) => {
                for field in struct_def.fields.iter() {
                    visitor.visit_ty(&field.ty);
                }
            }
            TypeDefKind::Variant(variant_def) => {
                for case in variant_def.cases.iter() {
                    for field in case.fields.iter().flatten() {
                        visitor.visit_ty(&field.ty);
                    }
                }
            }
        },
        ItemKind::Import(_) => (),
    }
}
pub fn walk_pat(visitor: &mut impl Visitor, pat: &Pattern) {
    match &pat.kind {
        PatternKind::Grouped(pat) | PatternKind::Deref(pat) => visitor.visit_pat(pat),
        PatternKind::Tuple(elements) | PatternKind::Case(_, elements) => elements
            .iter()
            .for_each(|element| visitor.visit_pat(element)),
        PatternKind::Ident(..) | PatternKind::Literal(..) | PatternKind::Wildcard => (),
    }
}
pub fn walk_block(visitor: &mut impl Visitor, block: &Block) {
    for stmt in block.stmts.iter() {
        visitor.visit_stmt(stmt);
    }
}
pub fn walk_type(visitor: &mut impl Visitor, ty: &Type) {
    match &ty.kind {
        TypeKind::Underscore => (),
        TypeKind::Array(element) => visitor.visit_ty(element),
        TypeKind::Fun(params, return_ty) => params
            .iter()
            .chain(return_ty.as_ref().map(|ty| &**ty))
            .for_each(|ty| {
                visitor.visit_ty(ty);
            }),
        TypeKind::Tuple(elements) => {
            for element in elements {
                visitor.visit_ty(element);
            }
        }
        TypeKind::Grouped(ty) | TypeKind::Ref(_, ty) => visitor.visit_ty(ty),
        TypeKind::Variant(variant) => {
            for case in variant.cases.iter() {
                for field in case.fields.iter().flatten() {
                    visitor.visit_ty(&field.ty);
                }
            }
        }
        TypeKind::Struct(struct_def) => {
            for field in struct_def.fields.iter() {
                visitor.visit_ty(&field.ty);
            }
        }
        TypeKind::Int
        | TypeKind::Uint
        | TypeKind::Bool
        | TypeKind::String
        | TypeKind::Never
        | TypeKind::Named(_) => (),
    }
}
pub fn walk_iterator(visitor: &mut impl Visitor, iterator: &IteratorExpr) {
    match &iterator.kind {
        IteratorExprKind::Range(start, end) => {
            visitor.visit_expr(start);
            visitor.visit_expr(end);
        }
        IteratorExprKind::Expr(expr) => visitor.visit_expr(expr),
    }
}
pub fn walk_expr(visitor: &mut impl Visitor, expr: &Expr) {
    match &expr.kind {
        ExprKind::Underscore => (),
        ExprKind::As(expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        ExprKind::For(pat, iter, body) => {
            visitor.visit_pat(pat);
            walk_iterator(visitor, iter);
            visitor.visit_block(body);
        }
        ExprKind::Grouped(expr)
        | ExprKind::Field(expr, _)
        | ExprKind::Unary(_, expr)
        | ExprKind::Deref(_, expr) => {
            visitor.visit_expr(expr);
        }
        ExprKind::While(condition, block) => {
            visitor.visit_expr(condition);
            visitor.visit_block(block);
        }
        ExprKind::Binary(_, first, last) | ExprKind::Assign(first, last, _) => {
            visitor.visit_expr(first);
            visitor.visit_expr(last);
        }
        ExprKind::Array(elements) | ExprKind::Tuple(elements) => {
            for element in elements {
                visitor.visit_expr(element);
            }
        }
        ExprKind::Block(block) => {
            visitor.visit_block(block);
        }
        ExprKind::If(condition, body, else_) => {
            visitor.visit_expr(condition);
            visitor.visit_block(body);
            if let Some(else_) = else_ {
                visitor.visit_expr(else_);
            }
        }
        ExprKind::Path(..) | ExprKind::Ident(..) | ExprKind::Literal(..) => (),
        ExprKind::Match(scrutinee, arms) => {
            visitor.visit_expr(scrutinee);
            for arm in arms {
                visitor.visit_pat(&arm.pat);
                visitor.visit_expr(&arm.body);
            }
        }
        ExprKind::Init(_, fields) => {
            for field in fields {
                visitor.visit_expr(&field.expr);
            }
        }
        ExprKind::Call(callee, args) => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
        }
        ExprKind::Break(expr) | ExprKind::Return(expr) => {
            if let Some(expr) = expr {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Loop(body) => visitor.visit_block(body),
    }
}

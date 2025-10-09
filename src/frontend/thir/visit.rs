use crate::frontend::thir::{self, Body, ExprKind};

pub trait Visitor<'a>: Sized {
    fn body(&self) -> &'a Body;
    fn visit_expr(&mut self, expr: &thir::Expr) {
        walk_expr(self, expr);
    }
    fn visit_block(&mut self, block: &thir::Block) {
        walk_block(self, block);
    }
    fn visit_arm(&mut self, arm: &thir::Arm) {
        walk_arm(self, arm);
    }
    fn visit_stmt(&mut self, stmt: &thir::Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_pattern(&mut self, pat: &thir::Pattern) {
        walk_pattern(self, pat);
    }
}

pub fn walk_expr<'thir, 'a>(v: &mut impl Visitor<'a>, expr: &'thir thir::Expr) {
    match &expr.kind {
        &ExprKind::Return(expr) => {
            if let Some(expr) = expr {
                v.visit_expr(v.body().expr(expr));
            }
        }
        ExprKind::Struct {
            id: _,
            generic_args: _,
            fields,
        } => {
            for field in fields {
                v.visit_expr(v.body().expr(field.expr));
            }
        }
        ExprKind::Literal(_) | ExprKind::Var(_) | ExprKind::Constant(_, _) => (),
        &ExprKind::NeverToAny(expr)
        | &ExprKind::Field {
            receiver: expr,
            field: _,
            field_span: _,
        }
        | &ExprKind::Unary(_, expr)
        | &ExprKind::Loop(expr) => v.visit_expr(v.body().expr(expr)),
        &ExprKind::Assign(first, second)
        | &ExprKind::Index(first, second)
        | &ExprKind::Binary(_, first, second) => {
            v.visit_expr(v.body().expr(first));
            v.visit_expr(v.body().expr(second));
        }
        ExprKind::Call(callee, args) => {
            v.visit_expr(v.body().expr(*callee));
            for &arg in args {
                v.visit_expr(v.body().expr(arg));
            }
        }
        ExprKind::Array(elements) | ExprKind::Tuple(elements) => {
            for &element in elements {
                v.visit_expr(v.body().expr(element));
            }
        }
        &ExprKind::If(condition, then_branch, else_branch) => {
            v.visit_expr(v.body().expr(condition));
            v.visit_expr(v.body().expr(then_branch));
            if let Some(else_branch) = else_branch {
                v.visit_expr(v.body().expr(else_branch));
            }
        }
        &ExprKind::Block(block) => v.visit_block(v.body().block(block)),
        ExprKind::Match(scrutinee, arms) => {
            v.visit_expr(v.body().expr(*scrutinee));
            for &arm in arms {
                v.visit_arm(v.body().arm(arm));
            }
        }
        ExprKind::VariantCase { fields, .. } => {
            for &field in fields {
                v.visit_expr(v.body().expr(field));
            }
        }
    }
}
pub fn walk_block<'thir, 'a>(v: &mut impl Visitor<'a>, block: &'thir thir::Block) {
    for &stmt in &block.stmts {
        v.visit_stmt(v.body().stmt(stmt));
    }
    if let Some(expr) = block.expr {
        v.visit_expr(v.body().expr(expr));
    }
}

pub fn walk_pattern<'thir, 'a>(v: &mut impl Visitor<'a>, pat: &'thir thir::Pattern) {
    match &pat.kind {
        thir::PatternKind::Binding(..) | thir::PatternKind::Wilcard | thir::PatternKind::Lit(_) => {
            ()
        }
        thir::PatternKind::Case(.., fields) | thir::PatternKind::Tuple(fields) => {
            fields.iter().for_each(|field| {
                v.visit_pattern(field);
            })
        }
    }
}
pub fn walk_arm<'thir, 'a>(v: &mut impl Visitor<'a>, arm: &'thir thir::Arm) {
    v.visit_pattern(&arm.pattern);
    v.visit_expr(v.body().expr(arm.body));
}
pub fn walk_stmt<'thir, 'a>(v: &mut impl Visitor<'a>, stmt: &'thir thir::Stmt) {
    match &stmt.kind {
        &thir::StmtKind::Expr(expr) => v.visit_expr(v.body().expr(expr)),
        thir::StmtKind::Let(pat, expr) => {
            v.visit_pattern(pat);
            v.visit_expr(v.body().expr(*expr));
        }
    }
}

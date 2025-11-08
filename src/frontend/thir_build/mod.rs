use crate::{
    context::CtxtRef,
    frontend::{
        ast::{BinaryOpKind, UnaryOpKind},
        hir::{self, DefId, DefKind, Definition, HirId, Resolution},
        thir::{
            Arm, Block, Body, BodyInfo, Expr, ExprField, ExprId, ExprKind, LocalVar, LogicalOp,
            LoopLabel, Param, Pattern, PatternKind, Stmt, StmtId, StmtKind, Thir, UnaryOp,
        },
        typecheck::{Coercion, results::TypeCheckResults},
    },
    indexvec::IndexVec,
    mir,
    types::Type,
};

pub struct ThirBuild<'ctxt> {
    ctxt: CtxtRef<'ctxt>,
    bodies: Vec<Body>,
}
impl<'ctxt> ThirBuild<'ctxt> {
    pub fn new(ctxt: CtxtRef<'ctxt>) -> Self {
        Self {
            ctxt,
            bodies: Vec::new(),
        }
    }

    pub fn build(&mut self, owner: hir::DefId, body: &hir::Body, results: TypeCheckResults) {
        let Some(body) = ThirBuilder::new(self.ctxt, owner, results).build(body) else {
            return;
        };
        self.bodies.push(body);
    }
    pub fn finish(self) -> Thir {
        Thir {
            bodies: self.bodies.into_boxed_slice(),
        }
    }
}
struct ThirBuilder<'ctxt> {
    ctxt: CtxtRef<'ctxt>,
    body: BodyInfo,
    results: TypeCheckResults,
}
impl<'ctxt> ThirBuilder<'ctxt> {
    pub fn new(ctxt: CtxtRef<'ctxt>, owner: DefId, results: TypeCheckResults) -> Self {
        Self {
            ctxt,
            body: BodyInfo {
                owner,
                params: Vec::new(),
                arms: IndexVec::new(),
                exprs: IndexVec::new(),
                stmts: IndexVec::new(),
                blocks: IndexVec::new(),
            },
            results,
        }
    }
    fn lower_expr(&mut self, expr: &hir::Expr) -> ExprId {
        let id = expr.id;
        let mut expr = self.make_expr(expr);
        if let Some(coercion) = self.results.get_coercion(id) {
            let span = expr.span;
            match coercion {
                Coercion::NeverToAny(ty) => {
                    if !ty.is_never() {
                        let expr_id = self.body.exprs.push(expr);
                        expr = Expr {
                            ty: ty.clone(),
                            span,
                            kind: ExprKind::NeverToAny(expr_id),
                        };
                    }
                }
                Coercion::RefCoercion(region) => {
                    let Type::Ref(target_ty, _, is_mut) = &expr.ty else {
                        unreachable!("Can only ref coerce references")
                    };
                    expr.ty = Type::Ref(target_ty.clone(), (**region).clone(), *is_mut);
                }
            }
        }
        self.body.exprs.push(expr)
    }
    fn get_res(&self, id: HirId) -> Option<Resolution> {
        self.results.get_res(id)
    }
    fn lower_exprs<'a>(&mut self, exprs: impl IntoIterator<Item = &'a hir::Expr>) -> Box<[ExprId]> {
        exprs
            .into_iter()
            .map(|expr| self.lower_expr(expr))
            .collect()
    }
    fn lower_stmt(&mut self, stmt: &hir::Stmt) -> Option<StmtId> {
        let stmt = match &stmt.kind {
            hir::StmtKind::Expr(expr) | hir::StmtKind::ExprWithSemi(expr) => Stmt {
                span: stmt.span,
                kind: StmtKind::Expr(self.lower_expr(expr)),
            },
            hir::StmtKind::Let(pattern, _, expr) => Stmt {
                span: stmt.span,
                kind: StmtKind::Let(Box::new(self.lower_pattern(pattern)), self.lower_expr(expr)),
            },
            hir::StmtKind::Item(_) => return None,
        };
        Some(self.body.stmts.push(stmt))
    }
    fn lower_block(&mut self, block: &hir::Block) -> ExprKind {
        let stmts = block
            .stmts
            .iter()
            .filter_map(|stmt| self.lower_stmt(stmt))
            .collect();
        let block = Block {
            stmts,
            expr: block.result.as_ref().map(|expr| self.lower_expr(expr)),
        };
        ExprKind::Block(self.body.blocks.push(block))
    }

    fn make_expr(&mut self, expr: &hir::Expr) -> Expr {
        let kind = match &expr.kind {
            hir::ExprKind::Err => unreachable!("Can't use an ExprKind::Err in thir"),
            hir::ExprKind::Ascribe(expr, _) => return self.make_expr(expr),
            hir::ExprKind::Literal(literal) => ExprKind::Literal(*literal),
            hir::ExprKind::Tuple(elements) => ExprKind::Tuple(self.lower_exprs(elements)),
            hir::ExprKind::Array(elements) => ExprKind::Array(self.lower_exprs(elements)),
            hir::ExprKind::Binary(op, left, right) => {
                let left = self.lower_expr(left);
                let right = self.lower_expr(right);
                match op.node {
                    BinaryOpKind::Add => ExprKind::Binary(mir::BinaryOp::Add, left, right),
                    BinaryOpKind::Subtract => {
                        ExprKind::Binary(mir::BinaryOp::Subtract, left, right)
                    }
                    BinaryOpKind::Multiply => {
                        ExprKind::Binary(mir::BinaryOp::Multiply, left, right)
                    }
                    BinaryOpKind::Divide => ExprKind::Binary(mir::BinaryOp::Divide, left, right),
                    BinaryOpKind::Equals => ExprKind::Binary(mir::BinaryOp::Equals, left, right),
                    BinaryOpKind::NotEquals => {
                        ExprKind::Binary(mir::BinaryOp::NotEquals, left, right)
                    }
                    BinaryOpKind::LesserEquals => {
                        ExprKind::Binary(mir::BinaryOp::LesserEquals, left, right)
                    }
                    BinaryOpKind::LesserThan => {
                        ExprKind::Binary(mir::BinaryOp::LesserThan, left, right)
                    }
                    BinaryOpKind::GreaterEquals => {
                        ExprKind::Binary(mir::BinaryOp::GreaterEquals, left, right)
                    }
                    BinaryOpKind::GreaterThan => {
                        ExprKind::Binary(mir::BinaryOp::GreaterThan, left, right)
                    }
                    BinaryOpKind::And => ExprKind::Logical(LogicalOp::And, left, right),
                    BinaryOpKind::Or => ExprKind::Logical(LogicalOp::Or, left, right),
                }
            }
            hir::ExprKind::Unary(op, operand) => match op.node {
                UnaryOpKind::Deref => ExprKind::Deref(self.lower_expr(operand)),
                UnaryOpKind::Negate => ExprKind::Unary(UnaryOp::Negate, self.lower_expr(operand)),
                UnaryOpKind::Ref(mutable) => ExprKind::Ref(mutable, self.lower_expr(operand)),
            },
            hir::ExprKind::Index(receiver, index) => {
                let reciever = self.lower_expr(receiver);
                let index = self.lower_expr(index);
                ExprKind::Index(reciever, index)
            }
            hir::ExprKind::Return(expr) => {
                ExprKind::Return(expr.as_ref().map(|expr| self.lower_expr(expr)))
            }
            hir::ExprKind::Init(_, fields) => {
                let Resolution::Def(id, DefKind::Struct) = self
                    .results
                    .get_res(expr.id)
                    .expect("Should have a resolution")
                else {
                    panic!("Had non-struct definition")
                };
                let generic_args = self
                    .results
                    .get_generic_args(expr.id)
                    .unwrap_or_else(|| panic!("Cannot find generic args"))
                    .clone();
                ExprKind::Struct {
                    id,
                    generic_args,
                    fields: fields
                        .iter()
                        .map(|field| {
                            let field_index = self.results.expect_field(field.id);
                            let field_expr = self.lower_expr(&field.expr);
                            ExprField {
                                field: field_index,
                                expr: field_expr,
                            }
                        })
                        .collect(),
                }
            }
            hir::ExprKind::Break(label, expr) => {
                let label = label.expect("Should have a valid loop label now");
                ExprKind::Break(
                    LoopLabel(label),
                    expr.as_ref().map(|expr| self.lower_expr(expr)),
                )
            }
            hir::ExprKind::For(..) => {
                todo!("For loop lowering")
            }

            hir::ExprKind::Match(scrutinee, arms) => ExprKind::Match(
                self.lower_expr(scrutinee),
                arms.iter()
                    .map(|arm| {
                        let arm = Arm {
                            pattern: self.lower_pattern(&arm.pat),
                            body: self.lower_expr(&arm.body),
                        };
                        self.body.arms.push(arm)
                    })
                    .collect(),
            ),
            hir::ExprKind::Call(callee, args) => {
                let variant_case = match &callee.kind {
                    hir::ExprKind::Path(path, _) => {
                        let Some(res) = self.get_res(path.id) else {
                            panic!("Resolution not finished")
                        };
                        match res {
                            Resolution::Def(id, DefKind::VariantCase) => {
                                Some((id, self.results.get_generic_args_or_empty(callee.id)))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };
                if let Some((id, generic_args)) = variant_case {
                    let parent_id = self.ctxt.expect_parent(id);
                    let case_index = self
                        .ctxt
                        .type_def(parent_id.into())
                        .index_of_case_with_id(id.into());
                    ExprKind::VariantCase {
                        type_id: parent_id,
                        case_index,
                        generic_args: generic_args.clone(),
                        fields: self.lower_exprs(args),
                    }
                } else {
                    ExprKind::Call(self.lower_expr(callee), self.lower_exprs(args))
                }
            }
            hir::ExprKind::Field(recevier, field) => {
                let field_index = self.results.expect_field(expr.id);
                ExprKind::Field {
                    receiver: self.lower_expr(recevier),
                    field: field_index,
                    field_span: field.span,
                }
            }
            hir::ExprKind::Assign(_, lhs, rhs) => {
                ExprKind::Assign(self.lower_expr(lhs), self.lower_expr(rhs))
            }
            hir::ExprKind::If(condition, then_branch, else_branch) => {
                let condition = self.lower_expr(condition);
                let then_branch = self.lower_expr(then_branch);
                let else_branch = else_branch
                    .as_ref()
                    .map(|else_branch| self.lower_expr(else_branch));
                ExprKind::If(condition, then_branch, else_branch)
            }
            hir::ExprKind::Path(path, _) => 'a: {
                let def = match self.get_res(path.id).unwrap_or_else(|| {
                    eprintln!("{:?}", self.results);
                    panic!(
                        "There should be a resolution for {:?} at {:?}.",
                        path,
                        self.ctxt.diag().span_info(expr.span)
                    )
                }) {
                    Resolution::Variable(var) => break 'a ExprKind::Var(LocalVar(var)),
                    Resolution::Def(id, _) => Definition::Def(id),
                    Resolution::Err => unreachable!("Can't have err resolutions for path"),
                };
                let generic_args = self.results.get_generic_args_or_empty(path.id).clone();
                ExprKind::Constant(def, generic_args)
            }
            hir::ExprKind::Loop(block, _) => {
                let label = LoopLabel(expr.id);
                let block_ty = self.results.type_of(block.id);
                let expr = Expr {
                    ty: block_ty,
                    span: block.span,
                    kind: self.lower_block(&block),
                };
                ExprKind::Loop(label, self.body.exprs.push(expr))
            }
            hir::ExprKind::Block(block) => self.lower_block(&block),
        };
        Expr {
            ty: self.results.type_of(expr.id),
            span: expr.span,
            kind,
        }
    }
    fn lower_pattern(&mut self, pattern: &hir::Pattern) -> Pattern {
        let ty = self.results.type_of(pattern.id);
        Pattern {
            ty,
            span: pattern.span,
            kind: match pattern.kind {
                hir::PatternKind::Binding(id, name, is_mut, by_ref) => PatternKind::Binding(
                    id,
                    name,
                    by_ref,
                    is_mut,
                    self.results.get_local_type(id).clone(),
                ),
                hir::PatternKind::Case(res, _, ref fields) => {
                    let id = match self.results.get_res(pattern.id).unwrap_or_else(|| {
                        panic!(
                            "There should be a resolution for this pattern {} at {:?}.",
                            res.as_str(),
                            self.ctxt.diag().span_info(pattern.span)
                        )
                    }) {
                        Resolution::Def(id, DefKind::VariantCase) => id,
                        Resolution::Def(_, _) | Resolution::Err | Resolution::Variable(..) => {
                            unreachable!("This can only be a variant case")
                        }
                    };
                    let case = self
                        .ctxt
                        .type_def(self.ctxt.expect_parent(id).into())
                        .index_of_case_with_id(id.into());
                    let generic_args = self.results.get_generic_args_or_empty(pattern.id).clone();
                    PatternKind::Case(
                        id,
                        case,
                        generic_args,
                        fields
                            .iter()
                            .map(|field| self.lower_pattern(field))
                            .collect(),
                    )
                }
                hir::PatternKind::Wildcard => PatternKind::Wilcard,
                hir::PatternKind::Literal(literal) => PatternKind::Lit(literal),
                hir::PatternKind::Tuple(ref fields) => PatternKind::Tuple(
                    fields
                        .iter()
                        .map(|field| self.lower_pattern(field))
                        .collect(),
                ),
            },
        }
    }
    pub fn build(mut self, hir: &hir::Body) -> Option<Body> {
        if self.results.had_error() {
            return None;
        }
        for param in hir.params.iter() {
            let param = Param {
                pattern: self.lower_pattern(&param.pat),
            };
            self.body.params.push(param);
        }
        let value = self.lower_expr(&hir.value);
        Some(Body {
            info: self.body,
            value,
        })
    }
}

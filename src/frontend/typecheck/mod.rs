use std::cell::RefCell;

use fxhash::FxHashMap;

use crate::{
    context::CtxtRef,
    errors::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{BinaryOp, BinaryOpKind, LiteralKind, Mutable, UnaryOp, UnaryOpKind},
        hir::{
            self, Block, Body, Builtin, DefId, DefKind, Expr, ExprKind, HirId, IntType, LoopSource,
            MatchArm, Path, Pattern, PatternKind, PrimitiveType, Resolution, Stmt, StmtKind,
        },
        ty_lower::TypeLower,
    },
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
    types::{IsMutable, Type, format::TypeFormat},
};
#[derive(Clone, Copy)]
enum Expected<'a> {
    Type(&'a Type),
    None,
}
impl<'a> Expected<'a> {
    fn and_then_ty(self, f: impl FnOnce(&'a Type) -> Self) -> Self {
        match self {
            Self::Type(ty) => f(ty),
            _ => self,
        }
    }
}
#[derive(Clone, PartialEq, Eq)]
pub struct LocalInfo {
    pub ty: Type,
    pub name: Symbol,
    pub is_mutable: IsMutable,
}
enum BuiltinFunction {
    Println,
}
enum Callee {
    Normal(Type),
    Builtin(BuiltinFunction),
}
pub struct TypeCheck<'ctxt> {
    context: CtxtRef<'ctxt>,
    id: DefId,
    body: &'ctxt Body,
    return_type: Option<Type>,
    loop_expectation: RefCell<Option<Type>>,
    locals: RefCell<FxHashMap<HirId, LocalInfo>>,
    types: RefCell<FxHashMap<HirId, Type>>,
}
impl<'ctxt> TypeCheck<'ctxt> {
    pub fn new(context: CtxtRef<'ctxt>, id: DefId) -> Option<Self> {
        let body = context.get_body_for(id)?;
        Some(Self {
            context,
            id,
            body,
            return_type: None,
            loop_expectation: RefCell::new(None),
            locals: RefCell::new(FxHashMap::default()),
            types: RefCell::new(FxHashMap::default()),
        })
    }
    fn write_type(&self, id: HirId, ty: Type) {
        self.types.borrow_mut().insert(id, ty);
    }
    fn err(&self, msg: impl IntoDiagnosticMessage, span: Span) -> Type {
        self.diag().emit_diag(msg, span);
        Type::Err
    }
    fn invalid_negate_operand_err(&self, operand: Type, op_span: Span) -> Type {
        self.err(
            format!(
                "Invalid operand '{}' for '-'.",
                TypeFormat::new(self.context).format_type(&operand)
            ),
            op_span,
        );
        match operand {
            ty @ Type::Primitive(PrimitiveType::Int(IntType::Unsigned)) => ty,
            _ => Type::Err,
        }
    }
    fn diag<'a>(&'a self) -> &'a DiagnosticReporter
    where
        'a: 'ctxt,
    {
        self.context.diag()
    }
    fn check_lit(&self, literal: LiteralKind, expected_ty: Expected) -> Type {
        match literal {
            LiteralKind::Bool(_) => Type::new_bool(),
            LiteralKind::Int(_) => Type::new_int(
                if matches!(
                    expected_ty,
                    Expected::Type(Type::Primitive(PrimitiveType::Int(IntType::Unsigned)))
                ) {
                    IntType::Unsigned
                } else {
                    IntType::Signed
                },
            ),
            LiteralKind::String(_) => Type::new_ref_str(),
        }
    }
    fn check_field(&self, receiver: &Expr, field: Ident) -> Type {
        let receiver_ty = self.check_expr(receiver, Expected::None);
        let Some(field_ty) = (match &receiver_ty {
            Type::Struct(fields) => fields.iter().find_map(|receiver_field| {
                (receiver_field.name == field.symbol).then(|| receiver_field.ty.clone())
            }),
            Type::Tuple(fields) => {
                if let Ok(index) = field.symbol.as_str().parse::<usize>() {
                    fields.get(index).cloned()
                } else {
                    None
                }
            }
            Type::Nominal(..) => todo!("HANDLE NOMINAL TYPES"),
            _ => None,
        }) else {
            return self.err(
                format!(
                    "type '{}' has no field '{}'.",
                    TypeFormat::new(self.context).format_type(&receiver_ty),
                    field.symbol.as_str()
                ),
                field.span,
            );
        };
        field_ty
    }
    fn check_tuple(&self, fields: &[Expr], expected_ty: Expected) -> Type {
        let field_tys = if let Expected::Type(Type::Tuple(fields)) = expected_ty {
            fields
        } else {
            &Vec::new()
        };
        let field_tys = fields.iter().enumerate().map(|(i, field)| {
            let expected_ty = field_tys
                .get(i)
                .map(Expected::Type)
                .unwrap_or_else(|| Expected::None);
            self.check_expr(field, expected_ty)
        });
        Type::new_tuple_from_iter(field_tys)
    }
    fn check_stmt(&self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.check_expr(expr, Expected::Type(&Type::new_unit()));
            }
            StmtKind::ExprWithSemi(expr) => {
                self.check_expr(expr, Expected::None);
            }
            StmtKind::Item(_) => {
                //Don't check items here
            }
            StmtKind::Let(pat, ty, expr) => {
                let ty = ty
                    .as_ref()
                    .map(|ty| TypeLower::new(self.context, false).lower(ty));
                let ty = self.check_expr(
                    expr,
                    match ty {
                        Some(ref ty) => Expected::Type(ty),
                        None => Expected::None,
                    },
                );
                self.check_pattern(pat, Some(&ty));
            }
        }
    }
    fn check_block(&self, block: &Block, expected_ty: Expected) -> Type {
        for stmt in block.stmts.iter() {
            self.check_stmt(stmt);
        }
        if let Some(ref expr) = block.result {
            self.check_expr(expr, expected_ty)
        } else {
            Type::new_unit()
        }
    }
    fn check_unary(&self, op: UnaryOp, operand: &Expr, expected_ty: Expected) -> Type {
        match op.node {
            UnaryOpKind::Negate => {
                let expected_ty = expected_ty.and_then_ty(|ty| match ty {
                    Type::Primitive(PrimitiveType::Int(..)) => Expected::Type(ty),
                    _ => Expected::None,
                });
                let operand = self.check_expr(operand, expected_ty);
                match operand {
                    Type::Primitive(PrimitiveType::Int(IntType::Signed)) => operand,
                    operand_ty => self.invalid_negate_operand_err(operand_ty, op.span),
                }
            }
            UnaryOpKind::Ref(mutable) => {
                let expected_ty = if let Expected::Type(Type::Ref(ty, ..)) = expected_ty {
                    Expected::Type(ty)
                } else {
                    Expected::None
                };
                let ty = if let Mutable::Yes(_) = mutable {
                    self.check_expr_is_mutable(operand, expected_ty)
                } else {
                    self.check_expr(operand, expected_ty)
                };
                Type::new_ref(ty, mutable.into())
            }
        }
    }
    fn check_binary(&self, op: BinaryOp, left: &Expr, right: &Expr, expected_ty: Expected) -> Type {
        let (left_expect, right_expect) = match (op.node, expected_ty) {
            (BinaryOpKind::And | BinaryOpKind::Or, _) => (
                Expected::Type(&Type::new_bool()),
                Expected::Type(&Type::new_bool()),
            ),
            (
                BinaryOpKind::Add
                | BinaryOpKind::Subtract
                | BinaryOpKind::Multiply
                | BinaryOpKind::Divide
                | BinaryOpKind::LesserThan
                | BinaryOpKind::GreaterThan
                | BinaryOpKind::LesserEquals
                | BinaryOpKind::GreaterEquals,
                Expected::Type(Type::Primitive(PrimitiveType::Int(_))),
            ) => (expected_ty, expected_ty),
            (_, _) => (Expected::None, Expected::None),
        };
        let left = self.check_expr(left, left_expect);
        let right = self.check_expr(right, right_expect);
        let ty = if left == right {
            match (op.node, &left) {
                (
                    BinaryOpKind::Add
                    | BinaryOpKind::Subtract
                    | BinaryOpKind::Divide
                    | BinaryOpKind::Multiply,
                    ty @ Type::Primitive(PrimitiveType::Int(_)),
                ) => Some(ty.clone()),
                (
                    BinaryOpKind::LesserThan
                    | BinaryOpKind::GreaterThan
                    | BinaryOpKind::LesserEquals
                    | BinaryOpKind::GreaterEquals,
                    Type::Primitive(PrimitiveType::Int(_)),
                ) => Some(Type::new_bool()),
                (BinaryOpKind::NotEquals | BinaryOpKind::Equals, _) => Some(Type::new_bool()),
                (BinaryOpKind::Or | BinaryOpKind::And, Type::Primitive(PrimitiveType::Bool)) => {
                    Some(Type::new_bool())
                }
                _ => None,
            }
        } else {
            None
        };
        if let Some(ty) = ty {
            ty
        } else if left.has_error() || right.has_error() {
            Type::Err
        } else {
            self.err(
                format!(
                    "Invalid operands '{}' and '{}' for '{}'.",
                    left.format(self.context),
                    right.format(self.context),
                    op.node
                ),
                op.span,
            )
        }
    }
    fn check_array(&self, elements: &[Expr], expected_ty: Expected) -> Type {
        todo!("CHECK ARRAY")
    }
    fn check_if(
        &self,
        span: Span,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
        expected_ty: Expected,
    ) -> Type {
        let _ = self.check_expr(condition, Expected::Type(&Type::new_bool()));
        let then_branch = self.check_expr(then_branch, expected_ty);
        if let Some(else_branch) = else_branch {
            let else_branch = self.check_expr(else_branch, expected_ty);
            if else_branch != then_branch {
                self.err(
                    format!(
                        "Incompatible types for 'if' '{}' and '{}'.",
                        then_branch.format(self.context),
                        else_branch.format(self.context)
                    ),
                    span,
                )
            } else {
                then_branch
            }
        } else if then_branch != Type::new_never() && then_branch != Type::new_unit() {
            self.err(
                format!(
                    "'if' of type '{}' missing else branch.",
                    then_branch.format(self.context)
                ),
                span,
            )
        } else {
            then_branch
        }
    }
    fn check_path(&self, path: &hir::Path, _expected_ty: Expected, span: Span) -> Type {
        match path.res {
            hir::Resolution::Builtin(builtin) => match builtin {
                Builtin::Println => self.err(
                    format!("Cannot use '{}' without parameters.", builtin.as_str()),
                    span,
                ),
                Builtin::Option => {
                    self.err(format!("Cannot use '{}' as value.", builtin.as_str()), span)
                }
                _ => self.context.type_of_builtin(builtin),
            },
            hir::Resolution::Variable(id) => self.locals.borrow()[&id].ty.clone(),
            hir::Resolution::Err => Type::Err,
            hir::Resolution::Def(id, _) => self.context.type_of(id),
        }
    }
    fn check_loop(&self, body: &Block, expected_ty: Expected, loop_source: LoopSource) -> Type {
        let old_ty = std::mem::replace(
            &mut *self.loop_expectation.borrow_mut(),
            if loop_source != LoopSource::Explicit {
                Some(Type::new_unit())
            } else if let Expected::Type(ty) = expected_ty {
                Some(ty.clone())
            } else {
                None
            },
        );
        let _ = self.check_block(body, Expected::None);
        *self.loop_expectation.borrow_mut() = old_ty;
        if let LoopSource::Explicit = loop_source {
            Type::new_never()
        } else {
            Type::new_unit()
        }
    }
    fn check_deref(&self, operand: &Expr) -> Type {
        let operand_ty = self.check_expr(operand, Expected::None);
        if let Type::Ref(pointee, _) = operand_ty {
            *pointee
        } else if operand_ty.has_error() {
            Type::Err
        } else {
            self.err(
                format!(
                    "Expected a reference got '{}'.",
                    operand_ty.format(self.context)
                ),
                operand.span,
            )
        }
    }
    fn is_valid_assign_target(&self, lhs: &Expr) -> bool {
        match lhs.kind {
            ExprKind::Err
            | ExprKind::Path(Path {
                res: Resolution::Variable(_) | Resolution::Err,
                ..
            })
            | ExprKind::Field(..)
            | ExprKind::Deref(..) => true,
            ExprKind::Return(..)
            | ExprKind::As(..)
            | ExprKind::Literal(..)
            | ExprKind::Loop(..)
            | ExprKind::Tuple(..)
            | ExprKind::Break(..)
            | ExprKind::Match(..)
            | ExprKind::Init(..)
            | ExprKind::If(..)
            | ExprKind::Path(Path {
                id: _,
                res: Resolution::Builtin(..) | Resolution::Def(..),
            })
            | ExprKind::Array(..)
            | ExprKind::Assign(..)
            | ExprKind::Unary(..)
            | ExprKind::Binary(..)
            | ExprKind::Call(..)
            | ExprKind::Block(..)
            | ExprKind::For(..) => false,
        }
    }
    fn check_assign(&self, lhs: &Expr, rhs: &Expr) -> Type {
        let lhs_ty = self.check_expr_is_mutable(lhs, Expected::None);
        if !self.is_valid_assign_target(lhs) {
            self.err("Invalid assingment target.", lhs.span);
        }
        let _ = self.check_expr(rhs, Expected::Type(&lhs_ty));
        Type::new_unit()
    }
    fn check_init(&self) -> Type {
        todo!("CHECK INIT")
    }
    fn check_callee(&self, callee: &Expr) -> Callee {
        match &callee.kind {
            ExprKind::Path(hir::Path {
                id: _,
                res: Resolution::Builtin(Builtin::Println),
            }) => Callee::Builtin(BuiltinFunction::Println),
            _ => Callee::Normal(self.check_expr(callee, Expected::None)),
        }
    }
    fn check_call(&self, span: Span, callee: &Expr, args: &[Expr], expected_ty: Expected) -> Type {
        let callee_kind = self.check_callee(callee);
        let (param_types, expected_param_count, return_ty) = match callee_kind {
            Callee::Normal(ref ty) => match ty {
                Type::Function(params, return_ty) => {
                    (params, Some(params.len()), Some(return_ty.as_ref()))
                }
                ty => {
                    self.err(
                        format!("Expected callable got '{}'.", ty.format(self.context)),
                        callee.span,
                    );
                    (
                        &Vec::new(),
                        None,
                        match expected_ty {
                            Expected::Type(ty) => Some(ty),
                            _ => None,
                        },
                    )
                }
            },
            Callee::Builtin(BuiltinFunction::Println) => (&vec![], None, Some(&Type::new_unit())),
        };
        if let Some(expected_params) = expected_param_count
            && expected_params != args.len()
        {
            self.err(
                format!("Expected '{}' args got '{}'.", expected_params, args.len()),
                span,
            );
        }
        for (arg, expected) in args.iter().enumerate().map(|(i, arg)| {
            (
                arg,
                param_types.get(i).map_or(Expected::None, Expected::Type),
            )
        }) {
            self.check_expr(arg, expected);
        }

        return_ty.cloned().unwrap_or(Type::Err)
    }
    fn check_return(&self, returned_expr: Option<&Expr>) -> Type {
        let return_ty = self
            .return_type
            .as_ref()
            .expect("There should be a return type when we type check");
        if let Some(expr) = returned_expr {
            self.check_expr(expr, Expected::Type(return_ty));
        }
        Type::new_never()
    }
    fn check_for(&self, pat: &Pattern, iterator: &hir::Iterator, body: &Block) -> Type {
        let item_ty = match iterator {
            hir::Iterator::Expr(expr) => {
                let _ = self.check_expr(expr, Expected::None);
                None
            }
            hir::Iterator::Ranged(span, start, end) => {
                let start = self.check_expr(start, Expected::None);
                let end = self.check_expr(end, Expected::None);
                let int_ty = if start != end {
                    None
                } else {
                    match start {
                        Type::Primitive(PrimitiveType::Int(int_ty)) => Some(int_ty),
                        _ => None,
                    }
                };
                if let Some(int_ty) = int_ty {
                    Some(Type::Primitive(PrimitiveType::Int(int_ty)))
                } else {
                    self.err(
                        format!(
                            "Invalid types for range '{}' and '{}'.",
                            start.format(self.context),
                            end.format(self.context)
                        ),
                        *span,
                    );
                    None
                }
            }
        };
        let _ = self.check_pattern(pat, Some(item_ty.as_ref().unwrap_or(&Type::Err)));
        let block_ty = self.check_block(body, Expected::Type(&Type::new_unit()));
        if !block_ty.has_error() && block_ty != Type::new_unit() {
            self.err(
                format!(
                    "Expected '()' for 'for' body got '{}'.",
                    block_ty.format(self.context)
                ),
                body.span,
            );
        }
        Type::new_unit()
    }
    fn check_match(&self, scrutinee: &Expr, arms: &[MatchArm], expected_ty: Expected) -> Type {
        let mut combined_ty = None;
        let scrut_ty = self.check_expr(scrutinee, Expected::None);
        for &MatchArm {
            id: _,
            span,
            ref pat,
            ref body,
        } in arms
        {
            let _ = self.check_pattern(pat, Some(&scrut_ty));
            let body_ty = self.check_expr(body, expected_ty);
            if combined_ty.is_none() {
                combined_ty = Some(body_ty);
            } else if let Some(ref ty) = combined_ty
                && !ty.has_error()
                && !body_ty.has_error()
                && *ty != body_ty
            {
                self.err(
                    format!(
                        "Expected '{}' got '{}'.",
                        ty.format(self.context),
                        body_ty.format(self.context)
                    ),
                    span,
                );
            }
        }
        combined_ty.unwrap_or(Type::new_never())
    }
    fn check_break(&self, operand: Option<&Expr>) -> Type {
        let expected = self.loop_expectation.borrow().as_ref().map(|ty| ty.clone());

        let expected = match expected {
            Some(ref ty) => Expected::Type(ty),
            None => Expected::None,
        };
        if let Some(operand) = operand {
            self.check_expr(operand, expected);
        }
        Type::new_never()
    }
    fn check_expr_is_mutable(&self, expr: &Expr, expected_ty: Expected) -> Type {
        let ty = self.check_expr(expr, expected_ty);
        if ty.has_error() {
            return ty;
        }
        check_mutable(self, expr);
        fn check_mutable(this: &TypeCheck, expr: &Expr) {
            match &expr.kind {
                ExprKind::Path(path) => match path.res {
                    hir::Resolution::Variable(id) => {
                        let local_info = &this.locals.borrow()[&id];
                        let (name, is_mutable) = (local_info.name, local_info.is_mutable);
                        if let IsMutable::No = is_mutable {
                            this.err(
                                format!("Cannot mutate immutable binding '{}'.", name.as_str()),
                                expr.span,
                            );
                        }
                    }
                    hir::Resolution::Builtin(..) => (),
                    hir::Resolution::Def(..) => (),
                    hir::Resolution::Err => (),
                },
                ExprKind::Field(receiver, _) => {
                    check_mutable(this, receiver);
                }
                ExprKind::Deref(expr) => {
                    let ty = &this.types.borrow()[&expr.id];
                    if let Type::Ref(_, IsMutable::No) = ty {
                        this.err("Cannot mutate through immutable reference.", expr.span);
                    }
                }
                ExprKind::Block(..)
                | ExprKind::Return(..)
                | ExprKind::Err
                | ExprKind::Array(..)
                | ExprKind::As(..)
                | ExprKind::For(..)
                | ExprKind::Init(..)
                | ExprKind::Unary(..)
                | ExprKind::Call(..)
                | ExprKind::Assign(..)
                | ExprKind::Loop(..)
                | ExprKind::Break(..)
                | ExprKind::Binary(..)
                | ExprKind::Literal(..)
                | ExprKind::Tuple(..)
                | ExprKind::If(..)
                | ExprKind::Match(..) => (),
            };
        }
        ty
    }
    fn check_expr(&self, expr: &Expr, expected_ty: Expected) -> Type {
        let Expr { kind, .. } = expr;
        let ty = match kind {
            &ExprKind::Literal(literal) => self.check_lit(literal, expected_ty),
            &ExprKind::Field(ref reciever, field) => self.check_field(reciever, field),
            ExprKind::Tuple(elements) => self.check_tuple(elements, expected_ty),
            ExprKind::As(expr, ty) => self.check_expr(
                expr,
                Expected::Type(&TypeLower::new(self.context, false).lower(ty)),
            ),
            ExprKind::Block(block) => self.check_block(block, expected_ty),
            ExprKind::Unary(op, val) => self.check_unary(*op, val, expected_ty),
            ExprKind::Binary(op, left, right) => self.check_binary(*op, left, right, expected_ty),
            ExprKind::Array(elements) => self.check_array(elements, expected_ty),
            ExprKind::If(condition, then_branch, else_branch) => self.check_if(
                expr.span,
                condition,
                then_branch,
                else_branch.as_ref().map(|expr| &**expr),
                expected_ty,
            ),
            ExprKind::Path(path) => self.check_path(path, expected_ty, expr.span),
            ExprKind::Assign(_, lhs, rhs) => self.check_assign(lhs, rhs),
            &ExprKind::Loop(ref block, source) => self.check_loop(block, expected_ty, source),
            ExprKind::Return(expr) => self.check_return(expr.as_deref()),
            ExprKind::Init(..) => self.check_init(),
            ExprKind::Deref(expr) => self.check_deref(expr),
            ExprKind::Call(callee, args) => self.check_call(expr.span, callee, args, expected_ty),
            ExprKind::Break(_loop_target, expr) => self.check_break(expr.as_deref()),
            ExprKind::Match(scrutinee, arms) => self.check_match(scrutinee, arms, expected_ty),
            ExprKind::For(pat, iter, body) => self.check_for(pat, iter, body),
            ExprKind::Err => Type::Err,
        };
        self.write_type(expr.id, ty.clone());
        if !ty.has_error()
            && let Expected::Type(expect_ty) = expected_ty
            && !expect_ty.has_error()
            && &ty != expect_ty
        {
            let format = TypeFormat::new(self.context);
            self.err(
                format!(
                    "Expected '{}' got '{}'.",
                    format.format_type(expect_ty),
                    format.format_type(&ty)
                ),
                expr.span,
            )
        } else {
            ty
        }
    }
    fn check_pattern(&self, pat: &Pattern, expected_ty: Option<&Type>) -> Type {
        let ty = match &pat.kind {
            &PatternKind::Binding(id, name, mutable, _) => {
                let ty = if let Some(ty) = expected_ty {
                    ty.clone()
                } else {
                    self.err(
                        format!("Cannot infer type of binding {}.", name.as_str()),
                        pat.span,
                    )
                };
                self.locals.borrow_mut().insert(
                    id,
                    LocalInfo {
                        ty: ty.clone(),
                        name,
                        is_mutable: mutable.into(),
                    },
                );
                ty
            }
            PatternKind::Wildcard => expected_ty
                .cloned()
                .unwrap_or_else(|| self.err("Cannot infer type of wildcard.", pat.span)),
            PatternKind::Tuple(fields) => {
                let field_tys = if let Some(Type::Tuple(field_tys)) = expected_ty {
                    if field_tys.len() != fields.len() {
                        self.err(
                            format!(
                                "Expected {} fields in tuple, got {}.",
                                field_tys.len(),
                                fields.len()
                            ),
                            pat.span,
                        );
                    }
                    field_tys.as_slice()
                } else {
                    &[]
                };
                Type::new_tuple_from_iter(
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, field_pat)| self.check_pattern(field_pat, field_tys.get(i))),
                )
            }
            PatternKind::Literal(lit) => match lit {
                LiteralKind::Int(_) => match expected_ty {
                    Some(ty @ Type::Primitive(PrimitiveType::Int(..))) => ty.clone(),
                    _ => Type::new_int(IntType::Signed),
                },
                LiteralKind::Bool(_) => Type::new_bool(),
                LiteralKind::String(..) => Type::new_ref_str(),
            },
            PatternKind::Case(res, fields) => {
                let case_and_variant_ty = match res {
                    Resolution::Def(id, DefKind::VariantCase) => {
                        let parent = self.context.expect_parent(*id);
                        self.context
                            .type_def(parent)
                            .case_with_id(*id)
                            .map(|case| (case, self.context.type_of(parent)))
                    }
                    _ => {
                        self.err(
                            format!("Expected a variant case got '{}'.", res.as_str()),
                            pat.span,
                        );
                        None
                    }
                };
                let field_defs = case_and_variant_ty
                    .as_ref()
                    .map_or(&[] as &[_], |(case, _)| case.fields.as_slice());
                if fields.len() != field_defs.len() {
                    self.err(
                        format!(
                            "Expected '{}' fields got '{}'.",
                            fields.len(),
                            field_defs.len()
                        ),
                        pat.span,
                    );
                }
                for (i, field) in fields.iter().enumerate() {
                    self.check_pattern(
                        field,
                        field_defs
                            .get(i)
                            .map(|field| self.context.type_of(field.id))
                            .as_ref(),
                    );
                }
                case_and_variant_ty.map(|(_, ty)| ty).unwrap_or(Type::Err)
            }
            PatternKind::Deref(..) => todo!("DEREF PATTERNS"),
            PatternKind::Ref(..) => todo!("REF PATTERNS"),
        };
        self.write_type(pat.id, ty.clone());
        if !ty.has_error()
            && let Some(expect) = expected_ty
            && expect != &ty
        {
            self.err(
                format!(
                    "Expected {} got {}.",
                    expect.format(self.context),
                    ty.format(self.context)
                ),
                pat.span,
            )
        } else {
            ty
        }
    }
    pub fn check(mut self) {
        let (params, return_ty) = self.context.signature_of(self.id);
        self.return_type = Some(return_ty.clone());
        for (param, ty) in self.body.params.iter().zip(params) {
            self.check_pattern(&param.pat, Some(&ty));
        }
        self.check_expr(&self.body.value, Expected::Type(&return_ty));
    }
}

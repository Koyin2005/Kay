use std::cell::RefCell;

use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;

use crate::{
    context::CtxtRef,
    diagnostics::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{BinaryOp, BinaryOpKind, ByRef, LiteralKind, Mutable, UnaryOp, UnaryOpKind},
        hir::{
            self, Block, Body, DefId, DefKind, Definition, Expr, ExprField, ExprKind, HirId,
            IntType, LoopSource, MatchArm, OutsideLoop, Path, Pattern, PatternKind, PrimitiveType,
            Resolution, Stmt, StmtKind,
        },
        ty_infer::{InferError, TypeInfer},
        ty_lower::TypeLower,
        typecheck::{Coercion, LocalSource, coercion::Coerce, results::TypeCheckResults},
    },
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
    types::{FieldIndex, GenericArg, GenericArgs, IsMutable, Region, Type},
};
#[derive(Clone, PartialEq, Eq)]
pub struct LocalInfo {
    pub ty: Type,
    pub name: Symbol,
    pub is_mutable: IsMutable,
    source: LocalSource,
}
pub struct TypeCheck<'ctxt> {
    context: CtxtRef<'ctxt>,
    body: &'ctxt Body,
    param_types: Vec<Type>,
    return_type: Type,
    infer_ctxt: TypeInfer,
    loop_coerce: RefCell<Option<Coerce>>,
    locals: RefCell<FxHashMap<HirId, LocalInfo>>,
    results: RefCell<TypeCheckResults>,
}
impl<'ctxt> TypeCheck<'ctxt> {
    pub fn new(context: CtxtRef<'ctxt>, id: DefId) -> Option<Self> {
        let body = context.get_body_for(id)?;
        let (params, return_ty) = context.signature_of(id);
        Some(Self {
            context,
            body,
            infer_ctxt: TypeInfer::new(),
            param_types: params,
            return_type: return_ty,
            loop_coerce: RefCell::new(None),
            locals: RefCell::new(FxHashMap::default()),
            results: RefCell::new(TypeCheckResults {
                owner: id,
                types: FxHashMap::default(),
                coercions: FxHashMap::default(),
                resolutions: FxHashMap::default(),
                generic_args: FxHashMap::default(),
                fields: FxHashMap::default(),
                local_types: FxHashMap::default(),
                had_error: false,
            }),
        })
    }
    pub fn infer_ctxt(&self) -> &TypeInfer {
        &self.infer_ctxt
    }
    fn write_type(&self, id: HirId, ty: Type) {
        self.results.borrow_mut().types.insert(id, ty);
    }
    fn local_ty(&self, id: HirId) -> Type {
        self.locals.borrow()[&id].ty.clone()
    }
    fn err(&self, msg: impl IntoDiagnosticMessage, span: Span) -> Type {
        self.diag().emit_diag(msg, span);
        self.results.borrow_mut().had_error = true;
        Type::Err
    }
    fn invalid_negate_operand_err(&self, operand: Type, op_span: Span) -> Type {
        if !operand.has_error() {
            self.err(
                format!("Invalid operand '{}' for '-'.", self.format_ty(&operand)),
                op_span,
            );
        }
        match operand {
            ty @ Type::Primitive(PrimitiveType::Int(_)) => ty,
            _ => Type::Err,
        }
    }
    fn format_ty(&self, ty: &Type) -> String {
        self.infer_ctxt.normalize(ty).format(self.context)
    }
    fn diag<'a>(&'a self) -> &'a DiagnosticReporter
    where
        'a: 'ctxt,
    {
        self.context.diag()
    }
    fn check_index(&self, base: &Expr, index: &Expr, span: Span) -> Type {
        let base = self.check_expr(base, None);
        let index = self.check_expr(index, Some(&Type::new_int(IntType::Unsigned)));
        match (base, index) {
            (Type::Array(element_ty), Type::Primitive(PrimitiveType::Int(IntType::Unsigned))) => {
                *element_ty
            }
            (base, index) => {
                if base.has_error() || index.has_error() {
                    Type::Err
                } else {
                    self.err(
                        format!(
                            "Cannot index into '{}' with '{}'.",
                            self.format_ty(&base),
                            self.format_ty(&index)
                        ),
                        span,
                    )
                }
            }
        }
    }
    fn check_lit(&self, literal: LiteralKind, expected_ty: Option<&Type>) -> Type {
        match literal {
            LiteralKind::Bool(_) => Type::new_bool(),
            LiteralKind::Int(_) | LiteralKind::IntErr => Type::new_int(match expected_ty {
                Some(Type::Primitive(PrimitiveType::Int(sign))) => *sign,
                _ => IntType::Signed,
            }),
            LiteralKind::String(_) => Type::new_ref_str(),
        }
    }
    fn check_field(&self, expr: &Expr, receiver: &Expr, field: Ident) -> Type {
        let receiver_ty = self.check_expr(receiver, None);
        let Some(field_ty) = (match &receiver_ty {
            Type::Tuple(fields) => {
                if let Ok(index) = field.symbol.as_str().parse::<usize>() {
                    fields.get(index).cloned().map(|field_ty| {
                        self.results
                            .borrow_mut()
                            .fields
                            .insert(expr.id, FieldIndex::new(index));
                        field_ty
                    })
                } else {
                    None
                }
            }
            &Type::Nominal(def, ref args) => {
                self.context.type_def(def).as_struct().and_then(|case| {
                    case.fields
                        .iter_enumerated()
                        .find_map(|(field_index, field_def)| {
                            (field_def.name == field.symbol).then(|| {
                                self.results
                                    .borrow_mut()
                                    .fields
                                    .insert(expr.id, field_index);
                                self.context.type_of(field_def.id).instantiate(args.clone())
                            })
                        })
                })
            }
            _ => None,
        }) else {
            return if receiver_ty.has_error() {
                Type::Err
            } else {
                self.err(
                    format!(
                        "type '{}' has no field '{}'.",
                        self.format_ty(&receiver_ty),
                        field.symbol.as_str()
                    ),
                    field.span,
                )
            };
        };
        field_ty
    }
    fn check_tuple(&self, fields: &[Expr], expected_ty: Option<&Type>) -> Type {
        let field_tys = if let Some(Type::Tuple(fields)) = expected_ty {
            fields
        } else {
            &Vec::new()
        };
        let field_tys = fields
            .iter()
            .enumerate()
            .map(|(i, field)| self.check_expr(field, field_tys.get(i)));
        Type::new_tuple_from_iter(field_tys)
    }
    fn check_stmt(&self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.check_expr_coerces_to(expr, &Type::new_unit());
            }
            StmtKind::ExprWithSemi(expr) => {
                self.check_expr(expr, None);
            }
            StmtKind::Item(_) => {
                //Don't check items here
            }
            StmtKind::Let(pat, ty, expr) => {
                let ty = ty
                    .as_ref()
                    .map(|ty| TypeLower::new(self.context, Some(&self.infer_ctxt)).lower(ty));
                let ty = if let Some(ty) = ty.as_ref() {
                    self.check_expr_coerces_to(expr, ty)
                } else {
                    self.check_expr(expr, None)
                };
                let region = self.get_region(&expr);
                self.check_pattern(pat, Some(&ty), region, false);
            }
        }
    }
    fn check_block(&self, block: &Block, expected_ty: Option<&Type>) -> Type {
        for stmt in block.stmts.iter() {
            self.check_stmt(stmt);
        }
        if let Some(ref expr) = block.result {
            if let Some(ty) = expected_ty {
                self.check_expr_coerces_to(expr, ty)
            } else {
                self.check_expr(expr, None)
            }
        } else {
            Type::new_unit()
        }
    }
    fn get_region(&self, pointee: &Expr) -> Option<Region> {
        match &pointee.kind {
            &ExprKind::Path(
                Path {
                    id: _,
                    res: Resolution::Variable(id),
                },
                _,
            ) => Some(Region::Local(self.locals.borrow()[&id].name, id)),
            ExprKind::Field(expr, _) | ExprKind::Index(expr, _) | ExprKind::Ascribe(expr, _) => {
                self.get_region(expr)
            }
            ExprKind::Unary(op, expr) if op.node == UnaryOpKind::Deref => {
                let ty = self.results.borrow().type_of(expr.id);
                if let Type::Ref(_, region, _) = ty {
                    Some(region)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
    fn check_unary(&self, op: UnaryOp, operand: &Expr, expected_ty: Option<&Type>) -> Type {
        match op.node {
            UnaryOpKind::Negate => {
                let operand =
                    self.check_expr_with_hint(operand, Some(&Type::new_int(IntType::Signed)));
                match operand {
                    Type::Primitive(PrimitiveType::Int(IntType::Signed)) => operand,
                    operand_ty => self.invalid_negate_operand_err(operand_ty, op.span),
                }
            }
            UnaryOpKind::Ref(mutable) => {
                let expected_ty = expected_ty.and_then(|ty| match ty {
                    Type::Ref(ty, ..) => Some(&**ty),
                    _ => None,
                });
                let ty = if let Mutable::Yes(_) = mutable {
                    self.check_expr_is_mutable(operand, expected_ty)
                } else {
                    self.check_expr(operand, expected_ty)
                };
                let region = self.get_region(operand).unwrap_or_else(|| {
                    self.err("Cannot take reference.", operand.span);
                    Region::Err
                });
                Type::new_ref(ty, region, mutable.into())
            }
            UnaryOpKind::Deref => {
                let operand_ty = self.check_expr(operand, None);
                if let Type::Ref(pointee, ..) = operand_ty {
                    *pointee
                } else if operand_ty.has_error() {
                    Type::Err
                } else {
                    self.err(
                        format!(
                            "Expected a reference got '{}'.",
                            self.format_ty(&operand_ty)
                        ),
                        operand.span,
                    )
                }
            }
        }
    }
    fn check_binary(
        &self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        expected_ty: Option<&Type>,
    ) -> Type {
        let (left_expect, right_expect) = match (op.node, expected_ty) {
            (BinaryOpKind::And | BinaryOpKind::Or, _) => {
                (Some(&Type::new_bool()), Some(&Type::new_bool()))
            }
            (
                BinaryOpKind::Add
                | BinaryOpKind::Subtract
                | BinaryOpKind::Multiply
                | BinaryOpKind::Divide
                | BinaryOpKind::LesserThan
                | BinaryOpKind::GreaterThan
                | BinaryOpKind::LesserEquals
                | BinaryOpKind::GreaterEquals,
                Some(Type::Primitive(PrimitiveType::Int(_))),
            ) => (expected_ty, expected_ty),
            (_, _) => (None, None),
        };
        let left = self.check_expr(left, left_expect);
        let right = self.check_expr(right, right_expect);
        let ty = if let Ok(left) = self.infer_ctxt.unify(&left, &right) {
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
                    self.format_ty(&left),
                    self.format_ty(&right),
                    op.node
                ),
                op.span,
            )
        }
    }
    fn check_array(&self, span: Span, elements: &[Expr], expected_ty: Option<&Type>) -> Type {
        let expected_element_ty = expected_ty.and_then(|ty| {
            if let Type::Array(ty) = ty {
                Some(&**ty)
            } else {
                None
            }
        });
        let mut coerce = Coerce::new(expected_element_ty.cloned());
        for element in elements {
            let element_ty = self.check_expr_with_hint(element, expected_element_ty);
            coerce.add_expr_and_ty(element, element_ty, self);
        }
        let element_ty = self.complete_coercion(coerce, |_, _, _| false);
        Type::new_array(element_ty.unwrap_or_else(|| self.err("Cannot infer type of array.", span)))
    }
    fn check_if(
        &self,
        span: Span,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
        expected_ty: Option<&Type>,
    ) -> Type {
        self.check_expr_coerces_to(condition, &Type::new_bool());
        let then_branch_ty = self.check_expr_with_hint(then_branch, expected_ty);
        if let Some(else_branch) = else_branch {
            let mut coerce = Coerce::new(expected_ty.cloned());
            coerce.add_expr_and_ty(then_branch, then_branch_ty.clone(), self);
            let else_branch_ty = self.check_expr_with_hint(else_branch, expected_ty);
            coerce.add_expr_and_ty(else_branch, else_branch_ty.clone(), self);
            if let Some(ty) = self.complete_coercion(coerce, |_, _, _| {
                self.err(
                    format!(
                        "Incompatible types for 'if' '{}' and '{}'.",
                        self.format_ty(&then_branch_ty),
                        self.format_ty(&else_branch_ty)
                    ),
                    span,
                );
                true
            }) {
                ty
            } else {
                Type::Err
            }
        } else {
            match self.try_coerce(then_branch.id, &then_branch_ty, &Type::new_unit()) {
                Ok(ty) => ty,
                Err(_) => {
                    if !then_branch_ty.has_error() {
                        self.err(
                            format!(
                                "'if' of type '{}' missing else branch.",
                                self.format_ty(&then_branch_ty)
                            ),
                            span,
                        );
                    }
                    then_branch_ty
                }
            }
        }
    }
    fn instantiate_generic_def(
        &self,
        id: HirId,
        generic_args: Option<&hir::GenericArgs>,
        definition: Definition,
        expected_ty: Option<&Type>,
        span: Span,
    ) -> Type {
        let def_id = match definition {
            Definition::Def(id) => id,
        };
        let scheme = self.context.type_of(definition);
        if scheme.arg_count() == 0 {
            return scheme.skip_instantiate();
        }
        let generic_args = TypeLower::new(self.context, Some(&self.infer_ctxt))
            .lower_generic_args_for(def_id, generic_args, span);
        self.results
            .borrow_mut()
            .generic_args
            .insert(id, generic_args.clone());
        let ty = scheme.instantiate(generic_args);
        if let Some(expected) = expected_ty {
            //We use the type of the definition as the 'base'
            self.infer_ctxt.unify(expected, &ty).ok().unwrap_or(ty)
        } else {
            ty
        }
    }
    fn check_path(
        &self,
        path: &hir::Path,
        args: Option<&hir::GenericArgs>,
        span: Span,
        expected_ty: Option<&Type>,
    ) -> Type {
        let (def, res) = match path.res {
            hir::Resolution::Err => {
                self.results.borrow_mut().had_error = true;
                return Type::Err;
            }
            hir::Resolution::Def(
                id,
                kind @ (DefKind::Field
                | DefKind::Module
                | DefKind::Struct
                | DefKind::Variant
                | DefKind::TypeParam
                | DefKind::RegionParam),
            ) => {
                return self.err(
                    format!(
                        "Cannot use {} '{}' as value.",
                        kind.as_str(),
                        self.context.ident(id).symbol.as_str()
                    ),
                    span,
                );
            }
            hir::Resolution::Variable(var) => {
                if args.is_some() {
                    self.diag()
                        .emit_diag("Cannot apply generic arguments to variable.", span);
                }
                self.results
                    .borrow_mut()
                    .resolutions
                    .insert(path.id, Resolution::Variable(var));
                return self.local_ty(var);
            }
            hir::Resolution::Def(id, kind @ (DefKind::VariantCase | DefKind::Function)) => {
                (hir::Definition::Def(id), Resolution::Def(id, kind))
            }
        };
        self.results.borrow_mut().resolutions.insert(path.id, res);
        self.instantiate_generic_def(path.id, args, def, expected_ty, span)
    }

    fn check_break(
        &self,
        span: Span,
        loop_target: Result<HirId, OutsideLoop>,
        operand: Option<&Expr>,
    ) -> Type {
        let expected = self
            .loop_coerce
            .borrow()
            .as_ref()
            .and_then(|coerce| coerce.target_type());
        if let Some(operand) = operand {
            let operand_ty = self.check_expr(operand, expected.as_ref());
            if let Ok(_) = loop_target
                && let Some(coerce) = self.loop_coerce.borrow_mut().as_mut()
            {
                coerce.add_expr_and_ty(operand, operand_ty, self);
            }
        }
        if loop_target.is_err() {
            self.err("Cannot use break outside of a loop.", span);
        }
        Type::new_never()
    }

    fn check_loop(
        &self,
        body: &Block,
        expected_ty: Option<&Type>,
        loop_source: LoopSource,
    ) -> Type {
        let old_coerce = std::mem::replace(
            &mut *self.loop_coerce.borrow_mut(),
            Some(Coerce::new(if loop_source != LoopSource::Explicit {
                Some(Type::new_unit())
            } else {
                expected_ty.cloned()
            })),
        );
        let block_ty = self.check_block(body, Some(&Type::new_unit()));
        self.write_type(body.id, block_ty);

        let coerce = std::mem::replace(&mut *self.loop_coerce.borrow_mut(), old_coerce);
        if let LoopSource::Explicit = loop_source {
            coerce
                .and_then(|coerce| self.complete_coercion(coerce, |_, _, _| false))
                .unwrap_or_else(Type::new_never)
        } else {
            Type::new_unit()
        }
    }
    fn is_valid_assign_target(lhs: &Expr) -> bool {
        match lhs.kind {
            ExprKind::Err
            | ExprKind::Path(
                Path {
                    res: Resolution::Variable(_) | Resolution::Err,
                    ..
                },
                _,
            )
            | ExprKind::Field(..)
            | ExprKind::Index(..) => true,
            ExprKind::Unary(op, ..) if op.node == UnaryOpKind::Deref => true,
            ExprKind::Ascribe(ref expr, _) => Self::is_valid_assign_target(expr),
            ExprKind::Return(..)
            | ExprKind::Literal(..)
            | ExprKind::Loop(..)
            | ExprKind::Tuple(..)
            | ExprKind::Break(..)
            | ExprKind::Match(..)
            | ExprKind::Init(..)
            | ExprKind::If(..)
            | ExprKind::Path(
                Path {
                    id: _,
                    res: Resolution::Def(..),
                },
                _,
            )
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
        let lhs_ty = self.check_expr_is_mutable(lhs, None);
        if !Self::is_valid_assign_target(lhs) {
            self.err("Invalid assingment target.", lhs.span);
        }
        self.check_expr_coerces_to(rhs, &lhs_ty);
        Type::new_unit()
    }
    fn check_init(
        &self,
        span: Span,
        id: HirId,
        path: Option<&Path>,
        fields: &[ExprField],
        expected_ty: Option<&Type>,
    ) -> Type {
        let struct_def = if let Some(path) = path {
            let ty_lower = TypeLower::new(self.context, Some(&self.infer_ctxt));
            match path.res {
                Resolution::Def(_, DefKind::Struct) => {
                    let ty = ty_lower.lower_ty_path(path, None, span);
                    if let Type::Nominal(def, args) = ty {
                        Ok(self
                            .context
                            .type_def(def)
                            .as_struct()
                            .map(|type_def| (type_def, def, args)))
                    } else if ty.has_error() {
                        Err(())
                    } else {
                        Ok(None)
                    }
                }
                Resolution::Err => Err(()),
                _ => Ok(None),
            }
        } else if let Some(Type::Nominal(def, args)) =
            expected_ty.map(|ty| self.infer_ctxt.normalize(ty))
        {
            Ok(self
                .context
                .type_def(def)
                .as_struct()
                .map(|case_def| (case_def, def, args)))
        } else {
            Ok(None)
        };
        let (struct_def, def, args) = match struct_def {
            Ok(Some((struct_def, def, args))) => (struct_def, def, args),
            err @ (Ok(None) | Err(_)) => {
                if err.is_ok() {
                    self.err("Cannot initialize.", span);
                }
                for field in fields {
                    self.check_expr(&field.expr, None);
                }
                return Type::Err;
            }
        };
        self.results
            .borrow_mut()
            .generic_args
            .insert(id, args.clone());
        self.results
            .borrow_mut()
            .resolutions
            .insert(id, Resolution::Def(def.into(), DefKind::Struct));
        let mut seen_fields = FxHashSet::default();
        for field in fields {
            if !seen_fields.insert(field.name.symbol) {
                self.err(
                    format!("Repeated field '{}'.", field.name.symbol.as_str()),
                    field.name.span,
                );
            }

            let field_def = struct_def
                .fields
                .iter()
                .enumerate()
                .find_map(|(i, field_def)| {
                    (field_def.name == field.name.symbol)
                        .then_some((FieldIndex::new(i), field_def.id))
                });
            let expected_field_ty = field_def.map(|(field_index, field_def)| {
                self.results
                    .borrow_mut()
                    .fields
                    .insert(field.id, field_index);
                self.context.type_of(field_def).instantiate(args.clone())
            });
            match expected_field_ty {
                Some(ref ty) => {
                    self.check_expr_coerces_to(&field.expr, ty);
                }
                None => {
                    self.check_expr(&field.expr, None);
                }
            }
        }
        for field in struct_def
            .fields
            .iter()
            .filter_map(|field| (!seen_fields.contains(&field.name)).then_some(field.name))
        {
            self.err(format!("Missing field '{}'.", field.as_str()), span);
        }
        Type::new_nominal_with_args(def, args)
    }
    fn check_call(
        &self,
        span: Span,
        callee: &Expr,
        args: &[Expr],
        expected_ty: Option<&Type>,
    ) -> Type {
        let callee_ty = self.check_expr(callee, None);
        let (param_types, expected_param_count, return_ty) = match &callee_ty {
            Type::Function(params, return_ty) => {
                (params, Some(params.len()), Some(return_ty.as_ref()))
            }
            ty => {
                if !ty.has_error() {
                    self.err(
                        format!("Expected callable got '{}'.", self.format_ty(ty)),
                        callee.span,
                    );
                }
                (&Vec::new(), None, expected_ty)
            }
        };
        if let Some(expected_params) = expected_param_count
            && expected_params != args.len()
        {
            self.err(
                format!("Expected '{}' args got '{}'.", expected_params, args.len()),
                span,
            );
        }
        for (i, arg) in args.iter().enumerate() {
            let param_ty = param_types.get(i);
            match param_ty.map(|ty| self.infer_ctxt.normalize(ty)) {
                Some(ref ty) => self.check_expr_coerces_to(arg, ty),
                None => self.check_expr(arg, None),
            };
        }
        return_ty.cloned().unwrap_or(Type::Err)
    }
    fn check_return(&self, returned_expr: Option<&Expr>) -> Type {
        if let Some(expr) = returned_expr {
            self.check_expr_coerces_to(expr, &self.return_type);
        }
        Type::new_never()
    }
    fn check_for(&self, pat: &Pattern, iterator: &hir::Iterator, body: &Block) -> Type {
        let item_ty = match iterator {
            hir::Iterator::Expr(expr) => match self.check_expr(expr, None) {
                Type::Array(elem) => Some(*elem),
                ty => {
                    self.err(
                        format!("Cannot iterate with {}.", self.format_ty(&ty)),
                        expr.span,
                    );
                    None
                }
            },
            hir::Iterator::Ranged(span, start, end) => {
                let start = self.check_expr(start, None);
                let end = self.check_expr(end, None);
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
                            self.format_ty(&start),
                            self.format_ty(&end)
                        ),
                        *span,
                    );
                    None
                }
            }
        };
        let region = match iterator {
            hir::Iterator::Expr(expr) => self.get_region(&expr),
            _ => None,
        };
        self.check_pattern(
            pat,
            Some(item_ty.as_ref().unwrap_or(&Type::Err)),
            region,
            false,
        );
        self.check_block(body, Some(&Type::new_unit()));
        Type::new_unit()
    }
    fn complete_coercion(
        &self,
        coerce: Coerce,
        handle_error: impl Fn(&InferError, &Type, &Type) -> bool,
    ) -> Option<Type> {
        let (final_ty, exprs_with_types) = coerce.complete();
        for (expr, span, ty) in exprs_with_types {
            if let Some(target) = final_ty.as_ref() {
                if let Err(err) = self.try_coerce(expr, &ty, target) {
                    if !handle_error(&err, &ty, target) {
                        self.expected_ty_error(err, target, &ty, span);
                    }
                }
            }
        }
        final_ty
    }
    fn try_coerce(&self, expr: HirId, ty: &Type, target: &Type) -> Result<Type, InferError> {
        let ty = self.infer_ctxt.normalize(ty);
        let target = self.infer_ctxt.normalize(target);
        match (&ty, &target) {
            (Type::Primitive(PrimitiveType::Never), target) => {
                self.results
                    .borrow_mut()
                    .coercions
                    .insert(expr, Coercion::NeverToAny(target.clone()));
                Ok(target.clone())
            }
            (ty, target) => self.infer_ctxt.unify(ty, target),
        }
    }
    fn expected_ty_error(
        &self,
        infer_error: InferError,
        expected_ty: &Type,
        ty: &Type,
        span: Span,
    ) -> Type {
        match infer_error {
            InferError::InfiniteType => self.err("Infinitely recursive type.", span),
            InferError::UnifyFailed => self.err(
                format!(
                    "Expected '{}' got '{}'.",
                    self.format_ty(expected_ty),
                    self.format_ty(ty)
                ),
                span,
            ),
        }
    }
    fn expect_ty(&self, ty: &Type, expected_ty: &Type, span: Span) -> Type {
        match self.infer_ctxt.unify(ty, expected_ty) {
            Ok(ty) => ty,
            Err(infer_error) => {
                if ty.has_error() || expected_ty.has_error() {
                    ty.clone()
                } else {
                    self.expected_ty_error(infer_error, expected_ty, ty, span)
                }
            }
        }
    }

    fn check_match(&self, scrutinee: &Expr, arms: &[MatchArm], expected_ty: Option<&Type>) -> Type {
        let scrut_ty = self.check_expr(scrutinee, None);
        let region = self.get_region(scrutinee);
        let mut coerce = Coerce::new(expected_ty.cloned());
        for MatchArm {
            id: _,
            span: _,
            pat,
            body,
        } in arms
        {
            self.check_pattern(pat, Some(&scrut_ty), region, false);
            let body_ty = self.check_expr_with_hint(body, expected_ty);
            coerce.add_expr_and_ty(body, body_ty, self);
        }
        self.complete_coercion(coerce, |_, _, _| false)
            .unwrap_or(Type::new_never())
    }
    fn check_expr_is_mutable(&self, expr: &Expr, expected_ty: Option<&Type>) -> Type {
        let ty = self.check_expr(expr, expected_ty);
        if ty.has_error() {
            return ty;
        }
        check_mutable(self, expr);
        fn check_mutable(this: &TypeCheck, expr: &Expr) {
            match &expr.kind {
                ExprKind::Path(path, _) => match path.res {
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
                    hir::Resolution::Def(..) => (),
                    hir::Resolution::Err => (),
                },
                ExprKind::Index(base, _) => {
                    check_mutable(this, base);
                }
                ExprKind::Field(receiver, _) => {
                    check_mutable(this, receiver);
                }
                ExprKind::Unary(op, expr) if op.node == UnaryOpKind::Deref => {
                    let is_mut = if let Type::Ref(_, _, IsMutable::No) =
                        this.results.borrow().types[&expr.id]
                    {
                        IsMutable::No
                    } else {
                        IsMutable::Yes
                    };
                    if let IsMutable::No = is_mut {
                        this.err("Cannot mutate through immutable reference.", expr.span);
                    }
                }
                ExprKind::Ascribe(expr, _) => {
                    check_mutable(this, expr);
                }
                ExprKind::Block(..)
                | ExprKind::Return(..)
                | ExprKind::Err
                | ExprKind::Array(..)
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
    fn check_ascribe_expr(&self, expr: &Expr, ty: &hir::Type) -> Type {
        let target_ty = TypeLower::new(self.context, Some(&self.infer_ctxt)).lower(ty);
        let _ = self.check_expr(expr, Some(&target_ty));
        target_ty
    }
    fn check_expr_kind(&self, expr: &Expr, expected_ty: Option<&Type>) -> Type {
        let Expr { kind, .. } = expr;
        let ty = match kind {
            ExprKind::Index(base, index) => self.check_index(base, index, expr.span),
            &ExprKind::Literal(literal) => self.check_lit(literal, expected_ty),
            &ExprKind::Field(ref reciever, field) => self.check_field(expr, reciever, field),
            ExprKind::Tuple(elements) => self.check_tuple(elements, expected_ty),
            ExprKind::Ascribe(expr, ty) => self.check_ascribe_expr(expr, ty),
            ExprKind::Block(block) => self.check_block(block, expected_ty),
            ExprKind::Unary(op, val) => self.check_unary(*op, val, expected_ty),
            ExprKind::Binary(op, left, right) => self.check_binary(*op, left, right, expected_ty),
            ExprKind::Array(elements) => self.check_array(expr.span, elements, expected_ty),
            ExprKind::If(condition, then_branch, else_branch) => self.check_if(
                expr.span,
                condition,
                then_branch,
                else_branch.as_ref().map(|expr| &**expr),
                expected_ty,
            ),
            ExprKind::Path(path, args) => {
                self.check_path(path, args.as_ref(), expr.span, expected_ty)
            }
            ExprKind::Assign(_, lhs, rhs) => self.check_assign(lhs, rhs),
            &ExprKind::Loop(ref block, source) => self.check_loop(block, expected_ty, source),
            ExprKind::Return(expr) => self.check_return(expr.as_deref()),
            ExprKind::Init(path, fields) => {
                self.check_init(expr.span, expr.id, path.as_ref(), fields, expected_ty)
            }
            ExprKind::Call(callee, args) => self.check_call(expr.span, callee, args, expected_ty),
            ExprKind::Break(loop_target, operand) => {
                self.check_break(expr.span, *loop_target, operand.as_deref())
            }
            ExprKind::Match(scrutinee, arms) => self.check_match(scrutinee, arms, expected_ty),
            ExprKind::For(pat, iter, body) => self.check_for(pat, iter, body),
            ExprKind::Err => Type::Err,
        };
        self.write_type(expr.id, ty.clone());
        ty
    }
    fn check_expr_coerces_to(&self, expr: &Expr, expected_ty: &Type) -> Type {
        let ty = self.check_expr_with_hint(expr, Some(expected_ty));
        match self.try_coerce(expr.id, &ty, expected_ty) {
            Ok(ty) => ty,
            Err(err) => {
                self.expected_ty_error(err, &expected_ty, &ty, expr.span);
                expected_ty.clone()
            }
        }
    }
    fn check_expr_with_hint(&self, expr: &Expr, ty: Option<&Type>) -> Type {
        self.check_expr_kind(expr, ty)
    }
    fn check_expr(&self, expr: &Expr, expected_ty: Option<&Type>) -> Type {
        let ty = self.check_expr_kind(expr, expected_ty);
        match expected_ty {
            None => ty,
            Some(expected_ty) => self.expect_ty(&ty, expected_ty, expr.span),
        }
    }
    fn check_pattern(
        &self,
        pat: &Pattern,
        expected_ty: Option<&Type>,
        region: Option<Region>,
        from_param: bool,
    ) -> Type {
        let ty = match &pat.kind {
            &PatternKind::Binding(id, name, mutable, by_ref) => {
                let mutable = IsMutable::from(mutable);
                let ty = if let Some(ty) = expected_ty {
                    ty.clone()
                } else {
                    self.err(
                        format!("Cannot infer type of '{}'.", name.as_str()),
                        pat.span,
                    )
                };
                let get_region = || {
                    region.unwrap_or_else(|| {
                        self.err("Cannot take reference.", pat.span);
                        Region::Err
                    })
                };
                let (local_ty, mutable) = match (by_ref, mutable) {
                    (ByRef::Yes(_), IsMutable::Yes) => (
                        Type::new_ref(ty.clone(), get_region(), IsMutable::Yes),
                        IsMutable::Yes,
                    ),
                    (ByRef::Yes(_), IsMutable::No) => (
                        Type::new_ref(ty.clone(), get_region(), IsMutable::No),
                        IsMutable::No,
                    ),
                    (ByRef::No, mutable) => (ty.clone(), mutable),
                };
                self.locals.borrow_mut().insert(
                    id,
                    LocalInfo {
                        ty: local_ty,
                        name,
                        is_mutable: mutable,
                        source: if from_param {
                            LocalSource::Param
                        } else {
                            LocalSource::Let
                        },
                    },
                );
                ty
            }
            PatternKind::Wildcard => expected_ty
                .cloned()
                .unwrap_or_else(|| self.err("Cannot infer type of '_'.", pat.span)),
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
                Type::new_tuple_from_iter(fields.iter().enumerate().map(|(i, field_pat)| {
                    self.check_pattern(field_pat, field_tys.get(i), region, from_param)
                }))
            }
            PatternKind::Literal(lit) => match lit {
                LiteralKind::Int(_) | LiteralKind::IntErr => match expected_ty {
                    Some(ty @ Type::Primitive(PrimitiveType::Int(..))) => ty.clone(),
                    _ => Type::new_int(IntType::Signed),
                },
                LiteralKind::Bool(_) => Type::new_bool(),
                LiteralKind::String(..) => Type::new_ref_str(),
            },
            PatternKind::Case(res, args, fields) => {
                let args = args.as_ref();
                let case_def = match res {
                    Resolution::Def(id, DefKind::VariantCase) => {
                        self.results
                            .borrow_mut()
                            .resolutions
                            .insert(pat.id, Resolution::Def(*id, DefKind::VariantCase));
                        Some(Definition::Def(*id))
                    }
                    _ => None,
                };
                let case_and_variant_ty = case_def.and_then(|case_def| {
                    let parent = self.context.expect_parent_of_def(case_def);
                    self.context
                        .type_def(parent)
                        .case_with_def(case_def)
                        .map(|case| {
                            (
                                case,
                                self.instantiate_generic_def(
                                    pat.id,
                                    args,
                                    parent,
                                    expected_ty,
                                    pat.span,
                                ),
                            )
                        })
                });
                if case_and_variant_ty.is_none() {
                    self.err(
                        format!("Expected a variant case got '{}'.", res.as_str()),
                        pat.span,
                    );
                }
                let field_defs = case_and_variant_ty
                    .as_ref()
                    .map(|(case, _)| case.fields.as_slice());
                if let Some(field_defs) = field_defs
                    && fields.len() != field_defs.len()
                {
                    self.err(
                        format!(
                            "Expected '{}' fields got '{}'.",
                            fields.len(),
                            field_defs.len()
                        ),
                        pat.span,
                    );
                }
                for (i, field_pat) in fields.iter().enumerate() {
                    self.check_pattern(
                        field_pat,
                        field_defs
                            .and_then(|field_defs| field_defs.get(i))
                            .map(|field| {
                                let generic_args = case_and_variant_ty
                                    .as_ref()
                                    .and_then(|(_, ty)| {
                                        if let Type::Nominal(_, args) = ty {
                                            Some(args.clone())
                                        } else {
                                            None
                                        }
                                    })
                                    .unwrap_or_else(GenericArgs::empty);
                                self.context.type_of(field.id).instantiate(generic_args)
                            })
                            .as_ref(),
                        region,
                        from_param,
                    );
                }
                case_and_variant_ty.map(|(_, ty)| ty).unwrap_or(Type::Err)
            }
            PatternKind::Deref(ref_pat) => {
                let (expected_ty, region, is_mutable) = match expected_ty.and_then(|ty| {
                    if let Type::Ref(ty, region, mutable) = ty {
                        Some((&**ty, region, mutable))
                    } else {
                        None
                    }
                }) {
                    Some((ty, region, mutable)) => (Some(ty), Some(region), Some(mutable)),
                    _ => (None, None, None),
                };

                let ty = self.check_pattern(ref_pat, expected_ty, region.cloned(), from_param);
                Type::new_ref(
                    ty,
                    region.cloned().unwrap_or(Region::Err),
                    is_mutable.copied().unwrap_or(IsMutable::No),
                )
            }
        };
        self.write_type(pat.id, ty.clone());
        if !ty.has_error()
            && let Some(expected_ty) = expected_ty
        {
            self.expect_ty(&ty, expected_ty, pat.span)
        } else {
            ty
        }
    }
    pub fn check(self) -> TypeCheckResults {
        for (param, ty) in self.body.params.iter().zip(self.param_types.iter()) {
            self.check_pattern(&param.pat, Some(ty), None, true);
        }
        self.check_expr_coerces_to(&self.body.value, &self.return_type);
        let mut incomplete_vars = FxHashSet::default();
        for var in self.infer_ctxt.ty_vars() {
            incomplete_vars.extend(self.infer_ctxt.normalize(&Type::Infer(var)).infer_vars());
        }
        for var in self.infer_ctxt.region_vars() {
            if let Region::Infer(var) = self.infer_ctxt.normalize_region(&Region::Infer(var)) {
                incomplete_vars.insert(var);
            }
        }
        let incomplete_vars = {
            let mut vars = incomplete_vars.into_iter().collect::<Vec<_>>();
            vars.sort();
            vars
        };
        if !incomplete_vars.is_empty() {
            let spans = incomplete_vars
                .into_iter()
                .map(|var| self.infer_ctxt.span(var))
                .collect::<IndexSet<_>>();
            for span in spans {
                self.err("Type annotations needed.", span);
            }
            return self.results.into_inner();
        }
        let mut results = self.results.into_inner();
        for ty in results.types.values_mut() {
            let norm_ty = self.infer_ctxt.normalize(&ty);
            *ty = norm_ty;
        }
        for args in results.generic_args.values_mut() {
            *args = args
                .iter()
                .map(|arg| match arg {
                    GenericArg::Type(ty) => GenericArg::Type(self.infer_ctxt.normalize(ty)),
                    GenericArg::Region(region) => GenericArg::Region(region.clone()),
                })
                .collect();
        }
        for coercion in results.coercions.values_mut() {
            let ty = match coercion {
                Coercion::NeverToAny(ty) => ty,
                Coercion::RefCoercion(_) => continue,
            };
            let norm_ty = self.infer_ctxt.normalize(ty);
            *ty = norm_ty;
        }
        for local in self.locals.into_inner() {
            results
                .local_types
                .insert(local.0, self.infer_ctxt.normalize(&local.1.ty));
        }
        results
    }
}

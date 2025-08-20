use std::cell::RefCell;

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    context::CtxtRef,
    errors::{DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{BinaryOp, BinaryOpKind, ByRef, LiteralKind, Mutable, UnaryOp, UnaryOpKind},
        hir::{
            self, Block, Body, Builtin, DefId, DefKind, Definition, Expr, ExprField, ExprKind,
            HirId, IntType, LoopSource, MatchArm, OutsideLoop, Path, Pattern, PatternKind,
            PrimitiveType, Resolution, Stmt, StmtKind,
        },
        ty_infer::TypeInfer,
        ty_lower::TypeLower,
    },
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
    types::{GenericArgs, IsMutable, Type},
};
struct CoerceError;
#[derive(Clone, Copy)]
enum Expected<'a> {
    HasType(&'a Type),
    CoercesTo(&'a Type),
    None,
}
impl<'a> Expected<'a> {
    pub fn as_option_ty(self) -> Option<&'a Type> {
        match self {
            Self::HasType(ty) => Some(ty),
            Self::CoercesTo(ty) => Some(ty),
            _ => None,
        }
    }
    fn and_then_ty(self, f: impl FnOnce(&'a Type) -> Self) -> Self {
        match self {
            Self::HasType(ty) => f(ty),
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
    Normal(Type, Option<u32>),
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
    spans: RefCell<FxHashMap<HirId, Span>>,
}
impl<'ctxt> TypeCheck<'ctxt> {
    pub fn new(context: CtxtRef<'ctxt>, id: DefId) -> Option<Self> {
        let body = context.get_body_for(id)?;
        Some(Self {
            context,
            spans: RefCell::new(FxHashMap::default()),
            id,
            body,
            return_type: None,
            loop_expectation: RefCell::new(None),
            locals: RefCell::new(FxHashMap::default()),
            types: RefCell::new(FxHashMap::default()),
        })
    }
    fn write_type(&self, id: HirId, ty: Type, span: Span) {
        self.types.borrow_mut().insert(id, ty);
        self.spans.borrow_mut().insert(id, span);
    }
    fn local_ty(&self, id: HirId) -> Type {
        self.locals.borrow()[&id].ty.clone()
    }
    fn err(&self, msg: impl IntoDiagnosticMessage, span: Span) -> Type {
        self.diag().emit_diag(msg, span);
        Type::Err
    }
    fn invalid_negate_operand_err(&self, operand: Type, op_span: Span) -> Type {
        self.err(
            format!("Invalid operand '{}' for '-'.", self.format_ty(&operand)),
            op_span,
        );
        match operand {
            ty @ Type::Primitive(PrimitiveType::Int(IntType::Unsigned)) => ty,
            _ => Type::Err,
        }
    }
    fn format_ty(&self, ty: &Type) -> String {
        ty.format(self.context)
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
                if let Some(Type::Primitive(PrimitiveType::Int(IntType::Unsigned))) =
                    expected_ty.as_option_ty()
                {
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
            &Type::Nominal(def, ref args) => {
                self.context.type_def(def).as_struct().and_then(|case| {
                    case.fields.iter().find_map(|field_def| {
                        (field_def.name == field.symbol)
                            .then(|| self.context.type_of(field_def.id).instantiate(args.clone()))
                    })
                })
            }
            _ => None,
        }) else {
            return self.err(
                format!(
                    "type '{}' has no field '{}'.",
                    self.format_ty(&receiver_ty),
                    field.symbol.as_str()
                ),
                field.span,
            );
        };
        field_ty
    }
    fn check_tuple(&self, fields: &[Expr], expected_ty: Expected) -> Type {
        let field_tys = if let Expected::HasType(Type::Tuple(fields)) = expected_ty {
            fields
        } else {
            &Vec::new()
        };
        let field_tys = fields.iter().enumerate().map(|(i, field)| {
            let expected_ty = field_tys
                .get(i)
                .map(Expected::CoercesTo)
                .unwrap_or_else(|| Expected::None);
            self.check_expr(field, expected_ty)
        });
        Type::new_tuple_from_iter(field_tys)
    }
    fn check_stmt(&self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                self.check_expr(expr, Expected::CoercesTo(&Type::new_unit()));
            }
            StmtKind::ExprWithSemi(expr) => {
                self.check_expr(expr, Expected::None);
            }
            StmtKind::Item(_) => {
                //Don't check items here
            }
            StmtKind::Let(pat, ty, expr) => {
                let ty = ty.as_ref().map(|ty| TypeLower::new(self.context).lower(ty));
                let ty = self.check_expr(
                    expr,
                    match ty {
                        Some(ref ty) => Expected::CoercesTo(ty),
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
                    Type::Primitive(PrimitiveType::Int(..)) => Expected::HasType(ty),
                    _ => Expected::None,
                });
                let operand = self.check_expr(operand, expected_ty);
                match operand {
                    Type::Primitive(PrimitiveType::Int(IntType::Signed)) => operand,
                    operand_ty => self.invalid_negate_operand_err(operand_ty, op.span),
                }
            }
            UnaryOpKind::Ref(mutable) => {
                let expected_ty = if let Expected::HasType(Type::Ref(ty, ..)) = expected_ty {
                    Expected::HasType(ty)
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
                Expected::HasType(&Type::new_bool()),
                Expected::HasType(&Type::new_bool()),
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
                Expected::HasType(Type::Primitive(PrimitiveType::Int(_))),
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
    fn check_array(&self, span: Span, elements: &[Expr], expected_ty: Expected) -> Type {
        let expected_element_ty = match expected_ty {
            Expected::HasType(Type::Array(ty)) => Expected::CoercesTo(ty),
            _ => Expected::None,
        };
        let mut element_ty = expected_element_ty.as_option_ty().cloned();
        for element in elements {
            let ty = self.check_expr(element, expected_element_ty);
            if let Some(ref mut element_ty) = element_ty{
                if let Some(common_ty) = self.coercion_lub(&ty, element_ty){
                    *element_ty = common_ty;
                }
                else{
                    self.err(format!("Expected '{}' got '{}'.",self.format_ty(element_ty),self.format_ty(&ty)), element.span);
                }
            }
            else{
                element_ty = Some(ty);
            }
        }

        Type::new_array(element_ty.unwrap_or_else(|| self.err("Cannot infer type of array", span)))
    }
    fn check_if(
        &self,
        span: Span,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
        expected_ty: Expected,
    ) -> Type {
        let _ = self.check_expr(condition, Expected::HasType(&Type::new_bool()));
        let then_branch = self.check_expr(then_branch, expected_ty);
        if let Some(else_branch) = else_branch {
            let else_branch = self.check_expr(else_branch, expected_ty);
            if let Some(ty) = self.coercion_lub(&then_branch, &else_branch){
                ty
            }
            else{
                self.err(
                    format!(
                        "Incompatible types for 'if' '{}' and '{}'.",
                        then_branch.format(self.context),
                        else_branch.format(self.context)
                    ),
                    span,
                )
            }
        } else if self.try_coerce(&then_branch, &Type::new_unit()).is_err() {
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
    fn check_generic_def(
        &self,
        generic_arg_count: u32,
        definition: Definition,
        span: Span,
        expected_ty: Option<&Type>,
    ) -> Type {
        let def_ty = self.context.type_of(definition).skip_instantiate();
        let mut infer_ctxt = TypeInfer::new(generic_arg_count);
        let ty = expected_ty
            .and_then(|ty| infer_ctxt.unify(ty, &def_ty))
            .or_else(|| infer_ctxt.normalize(&def_ty));
        if let Some(ty) = ty {
            ty
        } else {
            self.err("Cannot infer type of generic value.", span)
        }
    }
    fn check_path(&self, path: &hir::Path, expected_ty: Expected, span: Span) -> Type {
        match path.res {
            hir::Resolution::Builtin(builtin) => match builtin {
                Builtin::Println => self.err(
                    format!("Cannot use '{}' without parameters.", builtin.as_str()),
                    span,
                ),
                Builtin::Option => {
                    self.err(format!("Cannot use '{}' as value.", builtin.as_str()), span)
                }
                _ => self.check_generic_def(
                    1,
                    Definition::Builtin(builtin),
                    span,
                    expected_ty.as_option_ty(),
                ),
            },
            hir::Resolution::Variable(id) => self.local_ty(id),
            hir::Resolution::Err => Type::Err,
            hir::Resolution::Def(id, _) => {
                self.check_generic_def(1, Definition::Def(id), span, expected_ty.as_option_ty())
            }
        }
    }
    fn check_loop(&self, body: &Block, expected_ty: Expected, loop_source: LoopSource) -> Type {
        let old_ty = std::mem::replace(
            &mut *self.loop_expectation.borrow_mut(),
            if loop_source != LoopSource::Explicit {
                Some(Type::new_unit())
            } else if let Some(ty) = expected_ty.as_option_ty() {
                Some(ty.clone())
            } else {
                None
            },
        );
        let _ = self.check_block(body, Expected::None);
        let current_ty = std::mem::replace(&mut *self.loop_expectation.borrow_mut(), old_ty);
        if let LoopSource::Explicit = loop_source {
            current_ty.unwrap_or_else(Type::new_never)
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
        let _ = self.check_expr(rhs, Expected::CoercesTo(&lhs_ty));
        Type::new_unit()
    }
    fn check_init(
        &self,
        span: Span,
        path: Option<&Path>,
        fields: &[ExprField],
        expected_ty: Expected,
    ) -> Type {
        let struct_def_args = if let Some(path) = path {
            let type_def = TypeLower::new(self.context)
                .lower_ty_path(path)
                .ok()
                .and_then(|ty| {
                    if let Type::Nominal(def, args) = ty {
                        Some((def, args))
                    } else {
                        None
                    }
                });
            type_def.and_then(|(def, args)| {
                self.context
                    .type_def(def)
                    .as_struct()
                    .map(|case_def| (case_def, def, args))
            })
        } else if let Expected::HasType(&Type::Nominal(def, ref args)) = expected_ty {
            self.context
                .type_def(def)
                .as_struct()
                .map(|case_def| (case_def, def, args.clone()))
        } else {
            None
        };
        let Some((struct_def, def, args)) = struct_def_args else {
            self.err("Cannot initialize.", span);
            for field in fields {
                self.check_expr(&field.expr, Expected::None);
            }
            return Type::Err;
        };
        let mut seen_fields = FxHashSet::default();
        for field in fields {
            if !seen_fields.insert(field.name.symbol) {
                self.err(
                    format!("Repeated field '{}'.", field.name.symbol.as_str()),
                    field.name.span,
                );
            }
            let field_def = struct_def.fields.iter().find_map(|field_def| {
                (field_def.name == field.name.symbol).then_some(field_def.id)
            });
            self.check_expr(
                &field.expr,
                field_def
                    .map(|field_def| self.context.type_of(field_def).instantiate(args.clone()))
                    .as_ref()
                    .map_or(Expected::None, Expected::HasType),
            );
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
    fn check_callee(&self, callee: &Expr) -> Callee {
        match &callee.kind {
            &ExprKind::Path(hir::Path { id: _, res }) => match res {
                Resolution::Builtin(Builtin::Println) => Callee::Builtin(BuiltinFunction::Println),
                Resolution::Builtin(Builtin::OptionSome) => Callee::Normal(
                    self.context
                        .type_of(Builtin::OptionSome.into())
                        .skip_instantiate(),
                    Some(1),
                ),
                _ => Callee::Normal(self.check_expr(callee, Expected::None), None),
            },
            _ => Callee::Normal(self.check_expr(callee, Expected::None), None),
        }
    }
    fn check_call(&self, span: Span, callee: &Expr, args: &[Expr], expected_ty: Expected) -> Type {
        let callee_kind = self.check_callee(callee);
        let (param_types, expected_param_count, return_ty, generic_params) = match callee_kind {
            Callee::Normal(ref ty, generic_params) => match ty {
                Type::Function(params, return_ty) => (
                    params,
                    Some(params.len()),
                    Some(return_ty.as_ref()),
                    generic_params,
                ),
                ty => {
                    if !ty.has_error() {
                        self.err(
                            format!("Expected callable got '{}'.", ty.format(self.context)),
                            callee.span,
                        );
                    }
                    (
                        &Vec::new(),
                        None,
                        match expected_ty {
                            Expected::HasType(ty) => Some(ty),
                            _ => None,
                        },
                        None,
                    )
                }
            },
            Callee::Builtin(BuiltinFunction::Println) => {
                (&vec![], None, Some(&Type::new_unit()), None)
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
        let mut infer_ctxt = generic_params.map(|param_count| {
            let mut infer_ctxt = TypeInfer::new(param_count);
            if let Some(return_ty) = return_ty
                && let Expected::HasType(other) = expected_ty
            {
                infer_ctxt.unify(return_ty, other);
            }
            infer_ctxt
        });
        for (i, arg) in args.iter().enumerate() {
            let param_ty = param_types.get(i);
            let normalized_param = if let Some(ty) = param_ty {
                infer_ctxt
                    .as_ref()
                    .and_then(|infer_ctxt| infer_ctxt.normalize(ty))
            } else {
                None
            };
            let expected = match normalized_param {
                Some(ref ty) => Expected::HasType(ty),
                None => Expected::None,
            };
            let arg_ty = self.check_expr(arg, expected);
            if let Some(infer_ctxt) = infer_ctxt.as_mut()
                && let Some(param_ty) = param_ty
                && normalized_param.is_none()
            {
                infer_ctxt.unify(&arg_ty, param_ty);
            }
        }

        let return_ty = return_ty
            .cloned()
            .and_then(|ty| infer_ctxt.as_mut().and_then(|infer| infer.normalize(&ty)))
            .unwrap_or(Type::Err);
        if let Some(infer_ctxt) = infer_ctxt
            && !infer_ctxt.completed()
        {
            self.err("Cannot infer args of generic function.", span);
        }
        return_ty
    }
    fn check_return(&self, returned_expr: Option<&Expr>) -> Type {
        let return_ty = self
            .return_type
            .as_ref()
            .expect("There should be a return type when we type check");
        if let Some(expr) = returned_expr {
            self.check_expr(expr, Expected::CoercesTo(return_ty));
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
        let block_ty = self.check_block(body, Expected::CoercesTo(&Type::new_unit()));
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
    ///Computes a type that both a and b can be coerced to.
    fn coercion_lub(&self, a: &Type, b: &Type) -> Option<Type>{
        if a == b{ return Some(a.clone());}
        match (a,b) {
            (Type::Primitive(PrimitiveType::Never),ty) | (ty,Type::Primitive(PrimitiveType::Never)) => {
                Some(ty.clone())
            },
            _ => None
        }
    }
    fn try_coerce(&self, ty: &Type, target: &Type) -> Result<Type, CoerceError> {
        Ok(match (ty, target) {
            (ty, target) if ty == target => ty.clone(),
            (Type::Primitive(PrimitiveType::Never), target) => target.clone(),
            (ty, target) if !ty.has_error() && !target.has_error() => return Err(CoerceError),
            _ => Type::Err,
        })
    }
    fn coerce(&self, ty: &Type, target: &Type, span: Span) -> Type {
        match self.try_coerce(ty, target) {
            Ok(ty) => ty,
            Err(_) => self.err(
                format!(
                    "Expected '{}' got '{}'.",
                    self.format_ty(target),
                    self.format_ty(ty)
                ),
                span,
            ),
        }
    }
    fn expect_ty(&self, ty: &Type, expected_ty: &Type, span: Span) -> Type {
        if ty == expected_ty {
            ty.clone()
        } else if !ty.has_error() && !expected_ty.has_error() {
            self.err(
                format!(
                    "Expected '{}' got '{}'.",
                    self.format_ty(expected_ty),
                    self.format_ty(ty)
                ),
                span,
            )
        } else {
            Type::Err
        }
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
    fn check_break(&self, span: Span, loop_target: Result<HirId, OutsideLoop>, operand: Option<&Expr>) -> Type {
        let expected = self.loop_expectation.borrow().as_ref().map(|ty| ty.clone());
        let expected = match expected {
            Some(ref ty) => Expected::CoercesTo(ty),
            None => Expected::None,
        };
        if let Some(operand) = operand {
            let operand_ty = self.check_expr(operand, expected);
            if let Ok(_) = loop_target
                && self.loop_expectation.borrow().is_none()
            {
                *self.loop_expectation.borrow_mut() = Some(operand_ty);
            }
        }
        if loop_target.is_err(){
            self.err(format!("Cannot use break outside of a loop."),span);
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
    fn check_as_expr(&self, expr: &Expr, ty: &hir::Type) -> Type {
        let target_ty = TypeLower::new(self.context).lower(ty);
        self.check_expr(expr, Expected::CoercesTo(&target_ty))
    }
    fn check_expr(&self, expr: &Expr, expected_ty: Expected) -> Type {
        let Expr { kind, .. } = expr;
        let ty = match kind {
            &ExprKind::Literal(literal) => self.check_lit(literal, expected_ty),
            &ExprKind::Field(ref reciever, field) => self.check_field(reciever, field),
            ExprKind::Tuple(elements) => self.check_tuple(elements, expected_ty),
            ExprKind::As(expr, ty) => self.check_as_expr(expr, ty),
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
            ExprKind::Path(path) => self.check_path(path, expected_ty, expr.span),
            ExprKind::Assign(_, lhs, rhs) => self.check_assign(lhs, rhs),
            &ExprKind::Loop(ref block, source) => self.check_loop(block, expected_ty, source),
            ExprKind::Return(expr) => self.check_return(expr.as_deref()),
            ExprKind::Init(path, fields) => {
                self.check_init(expr.span, path.as_ref(), fields, expected_ty)
            }
            ExprKind::Deref(expr) => self.check_deref(expr),
            ExprKind::Call(callee, args) => self.check_call(expr.span, callee, args, expected_ty),
            ExprKind::Break(loop_target, operand) => self.check_break(expr.span,*loop_target, operand.as_deref()),
            ExprKind::Match(scrutinee, arms) => self.check_match(scrutinee, arms, expected_ty),
            ExprKind::For(pat, iter, body) => self.check_for(pat, iter, body),
            ExprKind::Err => Type::Err,
        };
        self.write_type(expr.id, ty.clone(), expr.span);
        match expected_ty {
            Expected::None => ty,
            Expected::CoercesTo(target) => self.coerce(&ty, target, expr.span),
            Expected::HasType(expected) => self.expect_ty(&ty, expected, expr.span),
        }
    }
    fn check_pattern(&self, pat: &Pattern, expected_ty: Option<&Type>) -> Type {
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
                let (local_ty, mutable) = match (by_ref, mutable) {
                    (ByRef::Yes(_), IsMutable::Yes) => {
                        (Type::new_ref_mut(ty.clone()), IsMutable::No)
                    }
                    (ByRef::Yes(_), IsMutable::No) => {
                        (Type::new_ref_immutable(ty.clone()), IsMutable::No)
                    }
                    (ByRef::No, mutable) => (ty.clone(), mutable),
                };
                self.locals.borrow_mut().insert(
                    id,
                    LocalInfo {
                        ty: local_ty,
                        name,
                        is_mutable: mutable,
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
                let case_def = match res {
                    Resolution::Def(id, DefKind::VariantCase) => Some(Definition::Def(*id)),
                    Resolution::Builtin(builtin) if builtin.is_variant() => {
                        Some(Definition::Builtin(*builtin))
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
                                self.check_generic_def(
                                    self.context.generic_arg_count(parent),
                                    parent,
                                    pat.span,
                                    expected_ty,
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
                for (i, field_pat) in fields.iter().enumerate() {
                    self.check_pattern(
                        field_pat,
                        field_defs
                            .get(i)
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
                    );
                }
                case_and_variant_ty.map(|(_, ty)| ty).unwrap_or(Type::Err)
            }
            PatternKind::Deref(ref_pat) => {
                let (expected_ty, is_mutable) = expected_ty
                    .and_then(|ty| {
                        if let Type::Ref(ty, mutable) = ty {
                            Some((&**ty, mutable))
                        } else {
                            None
                        }
                    })
                    .unzip();
                let ty = self.check_pattern(ref_pat, expected_ty);
                Type::new_ref(ty, is_mutable.copied().unwrap_or(IsMutable::No))
            }
        };
        self.write_type(pat.id, ty.clone(), pat.span);
        if !ty.has_error()
            && let Some(expected_ty) = expected_ty
        {
            self.expect_ty(&ty, expected_ty, pat.span)
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
        self.check_expr(&self.body.value, Expected::CoercesTo(&return_ty));
    }
}

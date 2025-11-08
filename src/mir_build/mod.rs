use fxhash::FxHashMap;
use indexmap::IndexMap;

use crate::{
    context::CtxtRef,
    frontend::{
        ast::{ByRef, LiteralKind, Mutable},
        hir::{self, HirId, IntType, PrimitiveType},
        thir::{self, Block, Expr, ExprId, LocalVar, LoopLabel, Pattern, PatternKind},
    },
    indexvec::IndexVec,
    mir::{
        self, AssertMessage, BasicBlock, BasicBlockInfo, Constant, Local, LocalInfo, LocalKind,
        Place, PlaceProjection, Rvalue, Stmt, StmtIndex, Terminator, UnaryOp, Value,
    },
    span::{Span, symbol::Symbol},
    types::{FieldIndex, Type, VariantCaseIndex},
};
#[derive(Debug)]
struct BlockInfo {
    stmts: IndexVec<StmtIndex, Stmt>,
    terminator: Option<Terminator>,
}
#[derive(Clone)]
pub struct PlaceBuilder {
    base: Local,
    projections: Vec<PlaceProjection>,
}
impl PlaceBuilder {
    fn new(local: Local) -> Self {
        Self {
            base: local,
            projections: Vec::new(),
        }
    }
    fn to_place(self) -> Place {
        Place {
            local: self.base,
            projections: self.projections.into_boxed_slice(),
        }
    }
    fn deref(mut self) -> Self {
        self.projections.push(PlaceProjection::Deref);
        self
    }
    fn field(mut self, field: FieldIndex) -> Self {
        self.projections.push(PlaceProjection::Field(field, None));
        self
    }
    fn index(mut self, local: Local) -> Self {
        self.projections.push(PlaceProjection::Index(local));
        self
    }
    fn field_with_variant(mut self, field: FieldIndex, case: VariantCaseIndex) -> Self {
        self.projections
            .push(PlaceProjection::Field(field, Some(case)));
        Self {
            base: self.base,
            projections: self.projections,
        }
    }
}
struct Break {
    block: BasicBlock,
    span: Span,
}
struct LoopScope {
    label: LoopLabel,
    breaks: Vec<Break>,
    result_destination: Place,
}
pub struct MirBuilder<'body> {
    thir: &'body thir::Body,
    context: CtxtRef<'body>,
    body: mir::Body,
    loop_scopes: Vec<LoopScope>,

    vars: FxHashMap<LocalVar, Local>,
    basic_blocks: IndexVec<BasicBlock, BlockInfo>,
    current_block: BasicBlock,
}

impl<'a> MirBuilder<'a> {
    pub fn new(body: &'a thir::Body, ctxt: CtxtRef<'a>) -> Self {
        let mut basic_blocks = IndexVec::new();
        let entry_block = basic_blocks.push(BlockInfo {
            stmts: IndexVec::new(),
            terminator: None,
        });
        Self {
            loop_scopes: Vec::new(),
            thir: body,
            context: ctxt,
            body: mir::Body {
                info: mir::BodyInfo {
                    id: body.info.owner,
                },
                locals: IndexVec::new(),
                blocks: IndexVec::new(),
            },
            vars: FxHashMap::default(),
            basic_blocks: basic_blocks,
            current_block: entry_block,
        }
    }
    fn switch_to_new_block(&mut self) -> BasicBlock {
        let new_block = self.new_block();
        self.switch_to_block(new_block);
        new_block
    }
    fn switch_to_block(&mut self, block: BasicBlock) {
        self.current_block = block;
    }
    fn new_block(&mut self) -> BasicBlock {
        self.basic_blocks.push(BlockInfo {
            stmts: IndexVec::new(),
            terminator: None,
        })
    }
    fn goto(&mut self, span: Span, target: BasicBlock) {
        self.terminate(Terminator {
            span,
            kind: mir::TerminatorKind::Goto(target),
        });
    }
    fn build_return(&mut self, span: Span) {
        self.terminate(Terminator {
            span,
            kind: mir::TerminatorKind::Return,
        });
    }
    fn assert(
        &mut self,
        span: Span,
        msg: AssertMessage,
        condition: Value,
        is_true: bool,
        target: BasicBlock,
    ) {
        self.terminate(Terminator {
            span,
            kind: mir::TerminatorKind::Assert(condition, is_true, msg, target),
        });
    }
    fn terminate(&mut self, terminator: Terminator) {
        self.basic_blocks[self.current_block].terminator = Some(terminator);
    }
    fn if_then_else(
        &mut self,
        span: Span,
        condition: Value,
        then_block: BasicBlock,
        else_block: BasicBlock,
    ) {
        self.terminate(Terminator {
            span,
            kind: mir::TerminatorKind::Switch(
                condition,
                Box::new([(Constant::Bool(false), else_block)]),
                then_block,
            ),
        });
    }
    fn new_local(&mut self, ty: Type, kind: LocalKind) -> Local {
        self.body.locals.push(LocalInfo { ty, kind })
    }
    fn insert_var(&mut self, var: LocalVar, local: Local) {
        self.vars.insert(var, local);
    }
    fn new_var(&mut self, var: LocalVar, name: Symbol, ty: Type) -> Local {
        let local = self.new_local(ty, LocalKind::UserDefined(name));
        self.insert_var(var, local);
        local
    }
    fn new_param(&mut self, ty: Type, name: Option<Symbol>) -> Local {
        self.new_local(ty, LocalKind::Param(name))
    }
    fn new_temp(&mut self, ty: Type) -> Local {
        self.new_local(ty, LocalKind::Temp)
    }
    fn finish(mut self) -> mir::Body {
        for block in self.basic_blocks {
            self.body.blocks.push(BasicBlockInfo {
                stmts: block.stmts,
                terminator: block
                    .terminator
                    .expect("All blocks should have terminators"),
            });
        }
        self.body
    }
    fn push_unit_assign(&mut self, place: Place, span: Span) {
        self.push_constant_assign(place, Constant::ZeroSized(Type::new_unit()), span);
    }
    fn push_constant_assign(&mut self, place: Place, constant: Constant, span: Span) {
        self.push_value_assign(place, Value::Constant(constant), span);
    }
    fn push_value_assign(&mut self, place: Place, value: Value, span: Span) {
        self.push_assign(place, Rvalue::Use(value), span);
    }
    fn push_assign(&mut self, place: Place, value: Rvalue, span: Span) {
        self.push_stmt(Stmt {
            span,
            kind: mir::StmtKind::Assign(place, value),
        });
    }
    fn push_stmt(&mut self, stmt: Stmt) {
        self.basic_blocks[self.current_block].stmts.push(stmt);
    }
    fn build_binary_op(
        &mut self,
        span: Span,
        ty: Type,
        op: mir::BinaryOp,
        left: Value,
        right: Value,
    ) -> Rvalue {
        match op {
            mir::BinaryOp::Add | mir::BinaryOp::Subtract | mir::BinaryOp::Multiply => {
                todo!("Overflowing ops")
            }
            mir::BinaryOp::Divide => {
                //Check for right = 0
                let zero = Value::Constant(Constant::Int(0));
                let is_zero = self.new_temp(Type::new_bool());
                self.push_assign(
                    is_zero.into(),
                    Rvalue::Binary(
                        mir::BinaryOp::Equals,
                        Box::new((right.clone(), zero.clone())),
                    ),
                    span,
                );

                let not_zero_block = self.new_block();
                self.assert(
                    span,
                    AssertMessage::DivisionByZero(left.clone()),
                    Value::Load(is_zero.into()),
                    false,
                    not_zero_block,
                );
                self.switch_to_block(not_zero_block);

                //Check for left = MIN and right = -1
                if let Type::Primitive(PrimitiveType::Int(IntType::Signed)) = ty {
                    let is_left_min = self.new_temp(Type::new_bool());
                    let is_right_negative_1 = self.new_temp(Type::new_bool());

                    let min_val = Value::Constant(Constant::Int(i64::MIN));
                    let negative_1 = Value::Constant(Constant::Int(-1));
                    self.push_assign(
                        is_left_min.into(),
                        Rvalue::Binary(mir::BinaryOp::Equals, Box::new((left.clone(), min_val))),
                        span,
                    );
                    self.push_assign(
                        is_right_negative_1.into(),
                        Rvalue::Binary(
                            mir::BinaryOp::Equals,
                            Box::new((right.clone(), negative_1)),
                        ),
                        span,
                    );

                    let is_overflow = self.new_temp(Type::new_bool());
                    self.push_assign(
                        is_overflow.into(),
                        Rvalue::Binary(
                            mir::BinaryOp::And,
                            Box::new((
                                Value::Load(is_left_min.into()),
                                Value::Load(is_right_negative_1.into()),
                            )),
                        ),
                        span,
                    );
                    let not_overflow_block = self.new_block();
                    self.assert(
                        span,
                        AssertMessage::NoOverflow(
                            mir::BinaryOp::Divide,
                            left.clone(),
                            right.clone(),
                        ),
                        Value::Load(is_overflow.into()),
                        true,
                        not_overflow_block,
                    );
                    self.switch_to_block(not_overflow_block);
                }
                Rvalue::Binary(op, Box::new((left, right)))
            }
            mir::BinaryOp::And
            | mir::BinaryOp::Or
            | mir::BinaryOp::Equals
            | mir::BinaryOp::NotEquals
            | mir::BinaryOp::GreaterEquals
            | mir::BinaryOp::LesserEquals
            | mir::BinaryOp::LesserThan
            | mir::BinaryOp::GreaterThan => Rvalue::Binary(op, Box::new((left, right))),
        }
    }
    fn as_value(&mut self, expr: ExprId) -> Value {
        match self.thir.expr(expr).kind {
            thir::ExprKind::Constant(def, ref args) => {
                Value::Constant(mir::Constant::Function(def.into(), args.clone()))
            }
            thir::ExprKind::Literal(literal) => Value::Constant(match literal {
                LiteralKind::Bool(value) => mir::Constant::Bool(value),
                LiteralKind::Int(value) => mir::Constant::Int(value),
                LiteralKind::String(value) => mir::Constant::String(value),
                LiteralKind::IntErr => unreachable!("Can't have 'LiteralKind::IntErr' this here"),
            }),
            _ => Value::Load(self.lower_expr_as_place(expr).to_place()),
        }
    }
    fn expr_into_temp(&mut self, expr: ExprId) -> Local {
        let ty = self.thir.expr(expr).ty.clone();
        let temp = self.new_temp(ty);
        self.expr_into_place(expr, temp.into());
        temp
    }
    fn expr_into_rvalue(&mut self, expr_id: ExprId) -> Rvalue {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &thir::ExprKind::Binary(op, left, right) => {
                let left = self.as_value(left);
                let right = self.as_value(right);
                self.build_binary_op(expr.span, expr.ty.clone(), op, left, right)
            }
            &thir::ExprKind::Unary(op, operand) => {
                let operand_value = self.as_value(operand);
                match op {
                    thir::UnaryOp::Negate => {
                        // Check for int.MIN
                        let is_min = self.new_temp(Type::new_bool());
                        let min_const = Constant::Int(i64::MIN);
                        let min_value = Value::Constant(min_const);
                        self.push_assign(
                            is_min.into(),
                            Rvalue::Binary(
                                mir::BinaryOp::Equals,
                                Box::new((operand_value.clone(), min_value.clone())),
                            ),
                            expr.span,
                        );
                        let target = self.new_block();
                        self.assert(
                            expr.span,
                            AssertMessage::NegOverflow(operand_value.clone()),
                            Value::Load(is_min.into()),
                            false,
                            target,
                        );
                        self.switch_to_block(target);
                        Rvalue::Unary(mir::UnaryOp::Negate, operand_value)
                    }
                }
            }
            thir::ExprKind::Call(callee, args) => {
                let args = args
                    .iter()
                    .map(|arg| self.as_value(*arg))
                    .collect::<Box<[_]>>();
                let callee = self.as_value(*callee);
                Rvalue::Call(callee, args)
            }
            thir::ExprKind::Field { .. }
            | thir::ExprKind::Var(..)
            | thir::ExprKind::Index(..)
            | thir::ExprKind::Deref(..)
            | thir::ExprKind::Constant(..)
            | thir::ExprKind::Literal(..)
            | thir::ExprKind::If(..)
            | thir::ExprKind::Logical(..)
            | thir::ExprKind::Assign(..)
            | thir::ExprKind::Match(..)
            | thir::ExprKind::Loop(..)
            | thir::ExprKind::Break(..)
            | thir::ExprKind::NeverToAny(..)
            | thir::ExprKind::Block(..)
            | thir::ExprKind::Return(..) => Rvalue::Use(self.as_value(expr_id)),

            thir::ExprKind::Ref(..) => {
                Rvalue::Use(Value::Load(self.expr_into_temp(expr_id).into()))
            }
            thir::ExprKind::Tuple(fields) => Rvalue::Aggregate(
                Box::new(mir::AggregateKind::Tuple),
                fields.iter().map(|&field| self.as_value(field)).collect(),
            ),
            thir::ExprKind::Array(fields) => Rvalue::Aggregate(
                Box::new(mir::AggregateKind::Array),
                fields.iter().map(|&field| self.as_value(field)).collect(),
            ),
            thir::ExprKind::Struct {
                id,
                generic_args,
                fields,
            } => {
                let field_map = fields
                    .iter()
                    .map(|field_expr| (field_expr.field, self.as_value(field_expr.expr)))
                    .collect::<IndexMap<_, _>>();
                Rvalue::Aggregate(
                    Box::new(mir::AggregateKind::Struct(*id, generic_args.clone())),
                    (0..fields.len())
                        .map(|i| field_map[&FieldIndex::new(i)].clone())
                        .collect(),
                )
            }
            thir::ExprKind::VariantCase {
                case_id,
                generic_args,
                fields,
            } => todo!("Variant case"),
        }
    }
    fn lower_expr_as_place(&mut self, expr_id: ExprId) -> PlaceBuilder {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            thir::ExprKind::Var(var) => PlaceBuilder::new(
                self.vars
                    .get(var)
                    .copied()
                    .expect("Local variable should be here"),
            ),
            &thir::ExprKind::Field {
                receiver, field, ..
            } => self.lower_expr_as_place(receiver).field(field),
            &thir::ExprKind::Deref(base) => self.lower_expr_as_place(base).deref(),
            &thir::ExprKind::Index(base, index) => {
                let place = self.lower_expr_as_place(base);
                let index_temp = self.new_temp(Type::new_int(IntType::Unsigned));
                self.expr_into_place(index, index_temp.into());

                let len_temp = self.new_temp(Type::new_int(IntType::Unsigned));
                let in_bounds_tmp = self.new_temp(Type::new_bool());
                let span = self.thir.expr(index).span;
                self.push_assign(len_temp.into(), Rvalue::Len(place.clone().to_place()), span);

                let index = Value::Load(index_temp.into());
                let len = Value::Load(len_temp.into());
                self.push_assign(
                    in_bounds_tmp.into(),
                    Rvalue::Binary(
                        mir::BinaryOp::LesserThan,
                        Box::new((index.clone(), len.clone())),
                    ),
                    span,
                );

                let new_block = self.new_block();
                self.assert(
                    span,
                    mir::AssertMessage::BoundsCheck { index, len },
                    Value::Load(Place::new_from_local(in_bounds_tmp)),
                    true,
                    new_block,
                );
                self.switch_to_block(new_block);

                place.index(index_temp)
            }
            _ => PlaceBuilder::new(self.expr_into_temp(expr_id)),
        }
    }
    fn lower_expr_as_stmt(&mut self, expr_id: ExprId) {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &thir::ExprKind::Assign(place, value) => {
                let place = self.lower_expr_as_place(place).to_place();
                self.expr_into_place(value, place);
            }
            thir::ExprKind::Return(value) => {
                if let Some(value) = value {
                    self.expr_into_place(*value, Place::return_place());
                } else {
                    self.push_unit_assign(Place::return_place(), expr.span);
                }
                self.build_return(expr.span);
                self.switch_to_new_block();
            }
            thir::ExprKind::Break(loop_label, value) => {
                let scope = self
                    .loop_scopes
                    .iter_mut()
                    .rev()
                    .find(|scope| scope.label == *loop_label)
                    .expect("All breaks should have matching loop labels");
                scope.breaks.push(Break {
                    block: self.current_block,
                    span: expr.span,
                });
                let destination = scope.result_destination.clone();
                if let Some(value) = value {
                    self.expr_into_place(*value, destination);
                } else {
                    self.push_unit_assign(destination, expr.span);
                }
                self.switch_to_new_block();
            }
            _ => {
                self.expr_into_temp(expr_id);
            }
        }
    }
    fn lower_stmt(&mut self, stmt: &thir::Stmt) {
        match &stmt.kind {
            &thir::StmtKind::Expr(expr) => {
                self.lower_expr_as_stmt(expr);
            }
            &thir::StmtKind::Let(ref pat, expr) => {
                self.expr_into_pattern(expr, pat);
            }
        }
    }
    fn lower_block(&mut self, place: Place, block: &Block, span: Span) {
        for &stmt in &block.stmts {
            self.lower_stmt(self.thir.stmt(stmt));
        }
        if let Some(expr) = block.expr {
            self.expr_into_place(expr, place);
        } else {
            self.push_unit_assign(place, span.end());
        }
    }
    fn in_loop(
        &mut self,
        label: LoopLabel,
        destination: Place,
        mut f: impl FnMut(&mut Self) -> BasicBlock,
    ) {
        self.loop_scopes.push(LoopScope {
            label,
            result_destination: destination,
            breaks: Vec::new(),
        });
        let continue_block = f(self);
        if let Some(scope) = self.loop_scopes.pop() {
            let current_block = self.current_block;
            for Break { block, span } in scope.breaks {
                self.switch_to_block(block);
                self.goto(span, continue_block);
            }
            self.switch_to_block(current_block);
        }
    }
    fn expr_into_place(&mut self, expr_id: ExprId, place: Place) {
        let expr = self.thir.expr(expr_id);
        match &expr.kind {
            &thir::ExprKind::If(condition, then_expr, else_expr) => {
                let cond_value = self.as_value(condition);
                let cond_block_end = self.current_block;
                let then_span = self.thir.expr(then_expr).span;
                let then_block = self.switch_to_new_block();
                self.expr_into_place(then_expr, place.clone());
                let then_block_end = self.current_block;

                let else_block = self.switch_to_new_block();
                let else_span = if let Some(else_expr) = else_expr {
                    self.expr_into_place(else_expr, place);
                    self.thir.expr(else_expr).span
                } else {
                    let end_span = expr.span.end();
                    self.push_unit_assign(place, end_span);
                    end_span
                };
                let else_block_end = self.current_block;

                self.switch_to_block(cond_block_end);
                self.if_then_else(
                    self.thir.expr(condition).span,
                    cond_value,
                    then_block,
                    else_block,
                );

                let merge_block = self.new_block();
                self.switch_to_block(then_block_end);
                self.goto(then_span, merge_block);

                self.switch_to_block(else_block_end);
                self.goto(else_span, merge_block);

                self.switch_to_block(merge_block);
            }
            thir::ExprKind::Block(block) => {
                self.lower_block(place, self.thir.block(*block), expr.span)
            }
            &thir::ExprKind::Ref(mutable, operand) => {
                let operand_place = self.lower_expr_as_place(operand).to_place();
                self.push_assign(
                    place,
                    Rvalue::Ref(
                        match mutable {
                            Mutable::No => mir::BorrowKind::Read,
                            Mutable::Yes(_) => mir::BorrowKind::ReadWrite,
                        },
                        operand_place,
                    ),
                    expr.span,
                );
            }
            thir::ExprKind::Match(..) => todo!("Pattern matching"),
            thir::ExprKind::Loop(label, body) => {
                self.in_loop(*label, place, |this| {
                    let old_block = this.current_block;
                    let loop_start = this.switch_to_new_block();
                    this.switch_to_block(old_block);
                    this.goto(expr.span, loop_start);
                    this.switch_to_block(loop_start);

                    this.lower_expr_as_stmt(*body);
                    this.goto(expr.span, loop_start);
                    this.switch_to_new_block()
                });
            }
            thir::ExprKind::NeverToAny(never_expr) => {
                self.expr_into_temp(*never_expr);
                self.terminate(Terminator {
                    span: expr.span,
                    kind: mir::TerminatorKind::Unreachable,
                });
                self.switch_to_new_block();
            }
            thir::ExprKind::Binary(..)
            | thir::ExprKind::Var(_)
            | thir::ExprKind::Literal(_)
            | thir::ExprKind::Constant(..)
            | thir::ExprKind::Call(..)
            | thir::ExprKind::Array(..)
            | thir::ExprKind::Field { .. }
            | thir::ExprKind::Tuple(..)
            | thir::ExprKind::VariantCase { .. }
            | thir::ExprKind::Index(..)
            | thir::ExprKind::Deref(..)
            | thir::ExprKind::Unary(..)
            | thir::ExprKind::Struct { .. } => {
                let rvalue = self.expr_into_rvalue(expr_id);
                self.push_assign(place, rvalue, expr.span);
            }
            &thir::ExprKind::Logical(op, left, right) => {
                let left_span = self.thir.expr(left).span;
                let right_span = self.thir.expr(right).span;
                let left_value = self.as_value(left);
                let cond_block = self.current_block;

                let then_block_start = self.switch_to_new_block();
                let (then_block_end, else_block_start) = match op {
                    thir::LogicalOp::And => {
                        self.expr_into_place(right, place.clone());
                        let then_block_end = self.current_block;

                        let else_block_start = self.switch_to_new_block();
                        self.push_constant_assign(place.clone(), Constant::Bool(false), left_span);
                        (then_block_end, else_block_start)
                    }
                    thir::LogicalOp::Or => {
                        self.push_constant_assign(place.clone(), Constant::Bool(true), left_span);
                        let then_block_end = self.current_block;

                        let else_block_start = self.switch_to_new_block();
                        self.expr_into_place(right, place);
                        (then_block_end, else_block_start)
                    }
                };
                let else_block_end = self.current_block;
                let merge_block = self.new_block();

                self.current_block = cond_block;
                self.if_then_else(left_span, left_value, then_block_start, else_block_start);

                self.switch_to_block(then_block_end);
                self.goto(left_span, merge_block);

                self.switch_to_block(else_block_end);
                self.goto(right_span, merge_block);

                self.switch_to_block(merge_block);
            }
            thir::ExprKind::Assign(..) => {
                self.lower_expr_as_stmt(expr_id);
                self.push_unit_assign(place, expr.span);
            }
            thir::ExprKind::Return(..) | thir::ExprKind::Break(..) => {
                self.lower_expr_as_stmt(expr_id);
            }
        }
    }
    fn place_into_pattern(&mut self, place: PlaceBuilder, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Lit(_) | PatternKind::Wilcard => (),
            PatternKind::Tuple(fields) => {
                for (i, field) in fields.iter().enumerate() {
                    self.place_into_pattern(place.clone().field(FieldIndex::new(i)), field);
                }
            }
            PatternKind::Case(_, case, _, fields) => {
                for (i, field) in fields.iter().enumerate() {
                    self.place_into_pattern(
                        place.clone().field_with_variant(FieldIndex::new(i), *case),
                        field,
                    );
                }
            }
            PatternKind::Binding(id, name, by_ref, mutable, ty) => {
                let local = self.new_var(LocalVar(*id), *name, ty.clone());
                if let ByRef::No = *by_ref {
                    self.push_assign(
                        local.into(),
                        Rvalue::Use(Value::Load(place.to_place())),
                        pattern.span,
                    );
                } else {
                    let kind = match mutable {
                        Mutable::Yes(_) => mir::BorrowKind::ReadWrite,
                        Mutable::No => mir::BorrowKind::Read,
                    };
                    self.push_assign(
                        local.into(),
                        Rvalue::Ref(kind, place.to_place()),
                        pattern.span,
                    );
                }
            }
        }
    }
    fn expr_into_pattern(&mut self, expr: ExprId, pattern: &Pattern) {
        match &pattern.kind {
            &PatternKind::Binding(id, name, ByRef::No, _, ref ty) => {
                let var_local = self.new_var(LocalVar(id), name, ty.clone());
                let place = Place::new_from_local(var_local);
                self.expr_into_place(expr, place);
            }
            _ => {
                let place = self.lower_expr_as_place(expr);
                self.place_into_pattern(place, pattern);
            }
        }
    }
    pub fn build(mut self) -> mir::Body {
        self.new_local(
            self.thir.expr(self.thir.value).ty.clone(),
            LocalKind::Return,
        );
        for param in &self.thir.info.params {
            let name = match param.pattern.kind {
                PatternKind::Binding(id, name, ByRef::No, _, _) => Some((id, name)),
                _ => None,
            };
            let local = self.new_param(param.pattern.ty.clone(), name.unzip().1);
            if let Some((id, _)) = name {
                self.insert_var(LocalVar(id), local);
            } else {
                self.place_into_pattern(PlaceBuilder::new(local), &param.pattern);
            }
        }
        let has_body = if let hir::ItemKind::Function(ref function_def) =
            self.context.expect_item(self.thir.info.owner).kind
        {
            function_def.has_body
        } else {
            true
        };
        if has_body {
            self.expr_into_place(self.thir.value, Place::return_place());
            self.build_return(self.thir.expr(self.thir.value).span);
        } else {
            self.terminate(Terminator {
                span: self.thir.expr(self.thir.value).span,
                kind: mir::TerminatorKind::Unreachable,
            });
        }
        self.finish()
    }
}

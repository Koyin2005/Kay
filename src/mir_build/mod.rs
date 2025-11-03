use fxhash::FxHashMap;

use crate::{
    context::CtxtRef,
    frontend::{
        ast::{BinaryOpKind, ByRef, LiteralKind, Mutable, Spanned},
        thir::{self, Block, Expr, LocalVar, Pattern, PatternKind},
    },
    indexvec::IndexVec,
    mir::{
        self, BasicBlock, BasicBlockInfo, Constant, Local, Place, PlaceBase, PlaceProjection,
        Rvalue, Stmt, StmtIndex, Terminator, Value,
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
            base: PlaceBase::Local(self.base),
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
pub struct MirBuilder<'body> {
    thir: &'body thir::Body,
    context: CtxtRef<'body>,
    body: mir::Body,

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
            thir: body,
            context: ctxt,
            body: mir::Body {
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
    fn terminate(&mut self, terminator: Terminator) {
        self.basic_blocks[self.current_block].terminator = Some(terminator);
    }
    fn new_var(&mut self, var: LocalVar, name: Symbol) -> Local {
        let local = self.body.locals.push(mir::LocalInfo::UserDefined(name));
        self.vars.insert(var, local);
        local
    }
    fn new_param(&mut self) -> Local {
        self.body.locals.push(mir::LocalInfo::Param)
    }
    fn new_temp(&mut self) -> Local {
        self.body.locals.push(mir::LocalInfo::Temp)
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
        self.push_stmt(Stmt {
            span,
            kind: mir::StmtKind::Assign(
                place,
                Rvalue::Use(Value::Constant(mir::Constant::ZeroSized(Type::new_unit()))),
            ),
        });
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
    fn build_binary_op(&mut self, op: mir::BinaryOp, left: Value, right: Value) -> Rvalue {
        match op {
            mir::BinaryOp::Add | mir::BinaryOp::Subtract | mir::BinaryOp::Multiply => {
                todo!("Overflowing ops")
            }
            mir::BinaryOp::Divide => {
                todo!("Overflowign / no zero")
            }
            mir::BinaryOp::Equals
            | mir::BinaryOp::NotEquals
            | mir::BinaryOp::GreaterEquals
            | mir::BinaryOp::LesserEquals
            | mir::BinaryOp::LesserThan
            | mir::BinaryOp::GreaterThan => Rvalue::Binary(op, Box::new((left, right))),
        }
    }
    fn as_value(&mut self, expr: &Expr) -> Value {
        match expr.kind {
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
    fn expr_into_rvalue(&mut self, expr: &Expr) -> Rvalue {
        match &expr.kind {
            &thir::ExprKind::Binary(op, left, right) => {
                let left = self.as_value(self.thir.expr(left));
                let right = self.as_value(self.thir.expr(right));
                self.build_binary_op(op, left, right)
            },
            thir::ExprKind::Call(callee,args) => {
                let args = args.iter().map(|arg|{
                    self.as_value(self.thir.expr(*arg))
                }).collect::<Box<[_]>>();
                let callee = self.as_value(self.thir.expr(*callee));
                Rvalue::Call(callee, args)
            }
            thir::ExprKind::Field { .. }
            | thir::ExprKind::Var(..)
            | thir::ExprKind::Index(..)
            | thir::ExprKind::Deref(..)
            | thir::ExprKind::Constant(..)
            | thir::ExprKind::Literal(..) => Rvalue::Use(self.as_value(expr)),

            thir::ExprKind::Ref(..) => {
                let temp = self.new_temp();
                self.expr_into_place(expr, temp.into());
                Rvalue::Use(Value::Load(temp.into()))
            }
            _ => todo!("The rest of the rvalues {:?}", expr),
        }
    }
    fn lower_expr_as_place(&mut self, expr: &Expr) -> PlaceBuilder {
        match &expr.kind {
            thir::ExprKind::Var(var) => PlaceBuilder::new(
                self.vars
                    .get(var)
                    .copied()
                    .expect("Local variable should be here"),
            ),
            thir::ExprKind::Field {
                receiver, field, ..
            } => self
                .lower_expr_as_place(self.thir.expr(*receiver))
                .field(*field),
            thir::ExprKind::Deref(base) => self.lower_expr_as_place(self.thir.expr(*base)).deref(),
            thir::ExprKind::Index(base, index) => {
                let place = self.lower_expr_as_place(self.thir.expr(*base));
                let index_temp = self.new_temp();
                self.expr_into_place(expr, index_temp.into());

                let len_temp = self.new_temp();
                let assert_temp = self.new_temp();
                let span = self.thir.expr(*index).span;
                self.push_assign(len_temp.into(), Rvalue::Len(place.clone().to_place()), span);

                let index = Value::Load(index_temp.into());
                let len = Value::Load(len_temp.into());
                self.push_assign(
                    assert_temp.into(),
                    Rvalue::Binary(
                        mir::BinaryOp::LesserThan,
                        Box::new((index.clone(), len.clone())),
                    ),
                    span,
                );

                let new_block = self.new_block();
                self.terminate(Terminator {
                    span,
                    kind: mir::TerminatorKind::Assert(
                        mir::AssertCondition::BoundsCheck { index, len },
                        new_block,
                    ),
                });
                self.switch_to_block(new_block);

                place.index(index_temp)
            }
            _ => {
                let temp = self.new_temp();
                self.expr_into_place(expr, temp.into());
                PlaceBuilder::new(temp)
            }
        }
    }
    fn lower_expr_as_stmt(&mut self, expr: &thir::Expr) {
        match &expr.kind {
            &thir::ExprKind::Assign(place, value) => {
                let place = self.lower_expr_as_place(self.thir.expr(place)).to_place();
                self.expr_into_place(self.thir.expr(value), place);
            }
            _ => {
                let temp = self.new_temp();
                self.expr_into_place(expr, temp.into());
            }
        }
    }
    fn lower_stmt(&mut self, stmt: &thir::Stmt) {
        match &stmt.kind {
            &thir::StmtKind::Expr(expr) => {
                self.lower_expr_as_stmt(self.thir.expr(expr));
            }
            thir::StmtKind::Let(pat, expr) => {
                let expr = self.thir.expr(*expr);
                self.expr_into_pattern(expr, pat);
            }
        }
    }
    fn lower_block(&mut self, place: Place, block: &Block) {
        for &stmt in &block.stmts {
            self.lower_stmt(self.thir.stmt(stmt));
        }
        if let Some(expr) = block.expr {
            self.expr_into_place(self.thir.expr(expr), place);
        }
    }
    fn expr_into_place(&mut self, expr: &thir::Expr, place: Place) {
        match &expr.kind {
            &thir::ExprKind::If(condition, then_expr, else_expr) => {
                let cond_value = self.as_value(self.thir.expr(condition));

                let then_block = self.new_block();
                let else_block = self.new_block();
                let join_block = self.new_block();
                self.terminate(Terminator {
                    span: self.thir.expr(condition).span,
                    kind: mir::TerminatorKind::Switch(
                        cond_value,
                        Box::new([(Constant::Bool(false), else_block)]),
                        then_block,
                    ),
                });
                //Then branch
                self.switch_to_block(then_block);
                self.expr_into_place(self.thir.expr(then_expr), place.clone());
                self.goto(expr.span, join_block);

                //Else branch
                self.switch_to_block(else_block);
                if let Some(else_expr) = else_expr {
                    self.expr_into_place(self.thir.expr(else_expr), place);
                } else {
                    self.push_unit_assign(place, expr.span.end());
                }
                self.goto(expr.span, join_block);

                self.switch_to_block(join_block);
            }
            thir::ExprKind::Block(block) => self.lower_block(place, self.thir.block(*block)),
            thir::ExprKind::Ref(mutable, operand) => {
                let operand_place = self
                    .lower_expr_as_place(self.thir.expr(*operand))
                    .to_place();
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
            thir::ExprKind::Loop(body) => {
                let old_block = self.current_block;
                let loop_start = self.switch_to_new_block();
                self.switch_to_block(old_block);
                self.goto(expr.span,loop_start);
                self.switch_to_block(loop_start);
                self.lower_expr_as_stmt(self.thir.expr(*body));
                self.goto(expr.span, loop_start);
                self.switch_to_new_block();
            }
            thir::ExprKind::NeverToAny(_) => {
                todo!("NEver to any")
            },
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
            | thir::ExprKind::Deref(..) => {
                let rvalue = self.expr_into_rvalue(expr);
                self.push_assign(place, rvalue, expr.span);
            }
            _ => todo!("The rest of expr"),
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
            PatternKind::Binding(id, name, by_ref, mutable, _) => {
                let local = self.new_var(LocalVar(*id), *name);
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
    fn expr_into_pattern(&mut self, expr: &thir::Expr, pattern: &Pattern) {
        match &pattern.kind {
            &PatternKind::Binding(id, name, ByRef::No, _, _) => {
                let var_local = self.new_var(LocalVar(id), name);
                let place = PlaceBuilder::new(var_local).to_place();
                let value = self.expr_into_rvalue(expr);
                self.push_assign(place, value, pattern.span);
            }
            _ => {
                let place = self.lower_expr_as_place(expr);
                self.place_into_pattern(place, pattern);
            }
        }
    }
    pub fn build(mut self) -> mir::Body {
        for param in &self.thir.info.params {
            let local = self.new_param();
            self.place_into_pattern(PlaceBuilder::new(local), &param.pattern);
        }
        let value = self.thir.expr(self.thir.value);
        self.expr_into_place(value, Place::return_place());
        self.terminate(Terminator {
            span: value.span,
            kind: mir::TerminatorKind::Return,
        });
        self.finish()
    }
}

use std::hash::Hash;

use crate::{
    define_id,
    frontend::hir::DefId,
    indexvec::IndexVec,
    span::{Span, symbol::Symbol},
    types::{FieldIndex, GenericArgs, Region, Type, VariantCaseIndex},
};

define_id! {
    #[derive(Debug)]
    pub struct Local{}
}
define_id! {
    #[derive(Debug)]
    pub struct StmtIndex{}
}
define_id! {
    #[derive(Debug)]
    pub struct BasicBlock{}
}
#[derive(Debug, Clone, Copy)]
pub enum LocalInfo {
    UserDefined(Symbol),
    Param,
    Temp,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub locals: IndexVec<Local, LocalInfo>,
    pub blocks: IndexVec<BasicBlock, BasicBlockInfo>,
}
#[derive(Debug, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}
#[derive(Debug, Clone, Copy)]
pub enum PlaceProjection {
    Deref,
    Field(FieldIndex, Option<VariantCaseIndex>),
    Index(Local),
}
#[derive(Debug, Clone, Copy)]
pub enum PlaceBase {
    Local(Local),
    Return,
}
#[derive(Debug, Clone)]
pub struct Place {
    pub base: PlaceBase,
    pub projections: Box<[PlaceProjection]>,
}
impl Place {
    pub fn return_place() -> Self {
        Self {
            base: PlaceBase::Return,
            projections: Box::new([]),
        }
    }
}
impl From<Local> for Place {
    fn from(value: Local) -> Self {
        Self {
            base: PlaceBase::Local(value),
            projections: Box::new([]),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Constant {
    Function(DefId, GenericArgs),
    Int(i64),
    Bool(bool),
    String(Symbol),
    ZeroSized(Type),
}
#[derive(Debug, Clone)]
pub enum Value {
    Load(Place),
    Constant(Constant),
}
#[derive(Debug, Clone, Copy)]
pub enum BorrowKind {
    Read,
    ReadWrite,
}
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    LesserThan,
    GreaterThan,
    LesserEquals,
    GreaterEquals,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AggregateKind {
    Array,
    Tuple,
    Struct(DefId, GenericArgs),
}
#[derive(Debug, Clone)]
pub enum Rvalue {
    Use(Value),
    Call(Value, Box<[Value]>),
    Aggregate(Box<AggregateKind>, IndexVec<FieldIndex, Value>),
    Ref(BorrowKind, Place),
    Discrimant(Place),
    Binary(BinaryOp, Box<(Value, Value)>),
    Len(Place),
    Unary(UnaryOp, Value),
}
#[derive(Debug, Clone)]
pub enum StmtKind {
    Assign(Place, Rvalue),
    Noop,
}
#[derive(Debug, Clone)]
pub enum AssertCondition {
    BoundsCheck { index: Value, len: Value },
    NoOverflow(Value, Value),
}
#[derive(Debug, Clone)]
pub enum TerminatorKind {
    Switch(Value, Box<[(Constant, BasicBlock)]>, BasicBlock),
    Unreachable,
    Goto(BasicBlock),
    Assert(AssertCondition, BasicBlock),
    Return,
}
#[derive(Debug, Clone)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}
#[derive(Debug, Clone)]
pub struct BasicBlockInfo {
    pub stmts: IndexVec<StmtIndex, Stmt>,
    pub terminator: Terminator,
}

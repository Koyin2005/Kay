use std::{
    fmt::{Display, Write},
    hash::Hash,
    str::FromStr,
};

use crate::{
    context::{CtxtRef, TypeDefKind},
    define_id,
    frontend::hir::DefId,
    indexvec::{Idx, IndexVec},
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
#[derive(Debug, Clone)]
pub struct LocalInfo {
    pub ty: Type,
    pub kind: LocalKind,
}
#[derive(Debug, Clone, Copy)]
pub enum LocalKind {
    UserDefined(Symbol),
    Param(Option<Symbol>),
    Temp,
    Return,
}
#[derive(Debug, Clone)]
pub struct BodyInfo {
    pub id: DefId,
}
#[derive(Debug, Clone)]
pub struct Body {
    pub info: BodyInfo,
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
impl PlaceProjection {
    pub fn apply_to(self, ty: &Type, ctxt: CtxtRef) -> Type {
        match self {
            Self::Deref => {
                let Type::Ref(ty, _, _) = ty else {
                    unreachable!("Cannot dereference {:?}", ty)
                };
                (**ty).clone()
            }
            Self::Index(_) => {
                let Type::Array(ty) = ty else {
                    unreachable!("Cannot get element of {:?}", ty)
                };
                (**ty).clone()
            }
            Self::Field(field, case) => match ty {
                Type::Tuple(fields) => fields[field.into_index()].clone(),
                Type::Nominal(def, args) => {
                    let type_def = ctxt.type_def(*def);
                    ctxt.type_of(match &type_def.kind {
                        TypeDefKind::Struct(struct_def) => struct_def.fields[field].id,
                        TypeDefKind::Variant(variant_cases) => {
                            variant_cases[case.expect("Should be within bounds")]
                                .1
                                .fields[field]
                                .id
                        }
                    })
                    .instantiate(args.clone())
                }
                _ => unreachable!("Cannot get field of type"),
            },
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub enum PlaceBase {
    Local(Local),
    Return,
}
#[derive(Debug, Clone)]
pub struct Place {
    pub local: Local,
    pub projections: Box<[PlaceProjection]>,
}
impl Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("_{}", self.0))
    }
}
impl Place {
    pub fn new_from_local(local: Local) -> Self {
        Self {
            local,
            projections: Box::new([]),
        }
    }
    pub fn type_of(&self, locals: &IndexVec<Local, LocalInfo>, ctxt: CtxtRef) -> Type {
        let mut ty = locals[self.local].ty.clone();
        for projection in self.projections.iter() {
            ty = projection.apply_to(&ty, ctxt);
        }
        ty
    }
    pub fn return_place() -> Self {
        Self {
            local: Local(0),
            projections: Box::new([]),
        }
    }
}
impl From<Local> for Place {
    fn from(value: Local) -> Self {
        Self {
            local: value,
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
    And,
    Or,
}
impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Divide => "/",
            Self::Equals => "==",
            Self::GreaterEquals => ">=",
            Self::LesserEquals => "<=",
            Self::Multiply => "*",
            Self::NotEquals => "!=",
            Self::Subtract => "-",
            Self::LesserThan => "<",
            Self::GreaterThan => ">",
            Self::And => "&",
            Self::Or => "|",
        }
    }
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
pub enum AssertMessage {
    BoundsCheck { index: Value, len: Value },
    NoOverflow(BinaryOp, Value, Value),
    DivisionByZero(Value),
    NegOverflow(Value),
}
#[derive(Debug, Clone)]
pub enum TerminatorKind {
    Switch(Value, Box<[(Constant, BasicBlock)]>, BasicBlock),
    Unreachable,
    Goto(BasicBlock),
    Assert(Value, bool, AssertMessage, BasicBlock),
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

pub mod debug;

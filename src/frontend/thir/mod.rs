use crate::{
    define_id,
    frontend::{
        ast::{BinaryOp, ByRef, LiteralKind, Mutable, UnaryOp},
        hir::{DefId, Definition, HirId},
    },
    indexvec::IndexVec,
    span::{Span, symbol::Symbol},
    types::{FieldIndex, GenericArgs, Type},
};

define_id!(
    pub struct ExprId {}
);
define_id!(
    pub struct ArmId {}
);
define_id!(
    pub struct StmtId {}
);
pub struct Param {
    pub pattern: Pattern,
}
pub struct Thir {
    pub bodies: Box<[Body]>,
}

pub struct Body {
    pub owner: DefId,
    pub params: Vec<Param>,
    pub arms: IndexVec<ArmId, Arm>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
}
pub enum PatternKind {
    Wilcard,
    Binding(HirId, Symbol, ByRef, Mutable),
    Case(DefId, GenericArgs, Box<[Pattern]>),
}
pub struct Pattern {
    pub ty: Type,
    pub span: Span,
    pub kind: PatternKind,
}
pub struct Expr {
    pub ty: Type,
    pub span: Span,
    pub kind: ExprKind,
}
pub struct Arm {
    pub pattern: Pattern,
    pub body: ExprId,
}
pub struct Stmt {
    pub kind: StmtKind,
}
pub enum StmtKind {
    Let(Box<Pattern>, ExprId),
    Expr(ExprId),
}
pub enum ExprKind {
    Literal(LiteralKind),
    Match(ExprId, Box<[ArmId]>),
    Call(ExprId, Box<[ExprId]>),
    Binary(BinaryOp, ExprId, ExprId),
    Unary(UnaryOp, ExprId),
    Index(ExprId, ExprId),
    Field {
        receiver: ExprId,
        field: FieldIndex,
        field_span: Span,
    },
    Var(LocalVar),
    VariantCase {
        case_id: DefId,
        generic_args: GenericArgs,
        fields: Box<[ExprId]>,
    },
    If(ExprId, ExprId, Option<ExprId>),
    Assign(ExprId, ExprId),
    Array(Box<[ExprId]>),
    Tuple(Box<[ExprId]>),
    Constant(Definition, GenericArgs),
    Block {
        stmts: Box<[StmtId]>,
        result: Option<ExprId>,
    },
    NeverToAny(ExprId),
    Return(Option<ExprId>),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalVar(pub HirId);

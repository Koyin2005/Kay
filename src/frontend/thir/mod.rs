use crate::{
    define_id,
    frontend::{
        ast::{ByRef, LiteralKind, Mutable},
        hir::{DefId, Definition, HirId},
    },
    indexvec::IndexVec,
    mir,
    span::{Span, symbol::Symbol},
    types::{FieldIndex, GenericArgs, Type, VariantCaseIndex},
};
pub mod visit;
define_id!(
    #[derive(Debug)]
    pub struct ExprId {}
);
define_id!(
    #[derive(Debug)]

    pub struct ArmId {}
);
define_id!(
    #[derive(Debug)]

    pub struct StmtId {}
);
define_id!(
    #[derive(Debug)]

    pub struct BlockId {}
);
pub struct Param {
    pub pattern: Pattern,
}
pub struct Thir {
    pub bodies: Box<[Body]>,
}
pub struct Body {
    pub info: BodyInfo,
    pub value: ExprId,
}
impl Body {
    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.info.exprs[id]
    }
    pub fn block(&self, id: BlockId) -> &Block {
        &self.info.blocks[id]
    }
    pub fn arm(&self, id: ArmId) -> &Arm {
        &self.info.arms[id]
    }
    pub fn stmt(&self, id: StmtId) -> &Stmt {
        &self.info.stmts[id]
    }
}
pub struct BodyInfo {
    pub owner: DefId,
    pub params: Vec<Param>,
    pub blocks: IndexVec<BlockId, Block>,
    pub arms: IndexVec<ArmId, Arm>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
}
pub struct Block {
    pub stmts: Box<[StmtId]>,
    pub expr: Option<ExprId>,
}
pub enum PatternKind {
    Wilcard,
    Binding(HirId, Symbol, ByRef, Mutable, Type),
    Case(DefId, VariantCaseIndex, GenericArgs, Box<[Pattern]>),
    Tuple(Box<[Pattern]>),
    Lit(LiteralKind),
}
pub struct Pattern {
    pub ty: Type,
    pub span: Span,
    pub kind: PatternKind,
}
#[derive(Debug)]
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
    pub span: Span,
    pub kind: StmtKind,
}
pub enum StmtKind {
    Let(Box<Pattern>, ExprId),
    Expr(ExprId),
}
#[derive(Debug)]
pub struct ExprField {
    pub field: FieldIndex,
    pub expr: ExprId,
}
#[derive(Debug, Clone, Copy)]
pub enum LogicalOp {
    And,
    Or,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}
#[derive(Debug)]
pub enum ExprKind {
    Literal(LiteralKind),
    Match(ExprId, Box<[ArmId]>),
    Call(ExprId, Box<[ExprId]>),
    Binary(mir::BinaryOp, ExprId, ExprId),
    Logical(LogicalOp, ExprId, ExprId),
    Unary(UnaryOp, ExprId),
    Deref(ExprId),
    Ref(Mutable, ExprId),
    Index(ExprId, ExprId),
    Loop(LoopLabel, ExprId),
    Field {
        receiver: ExprId,
        field: FieldIndex,
        field_span: Span,
    },
    Var(LocalVar),
    VariantCase {
        type_id: DefId,
        generic_args: GenericArgs,
        case_index: VariantCaseIndex,
        fields: Box<[ExprId]>,
    },
    Struct {
        id: DefId,
        generic_args: GenericArgs,
        fields: Box<[ExprField]>,
    },
    If(ExprId, ExprId, Option<ExprId>),
    Assign(ExprId, ExprId),
    Array(Box<[ExprId]>),
    Tuple(Box<[ExprId]>),
    Constant(Definition, GenericArgs),
    Block(BlockId),
    NeverToAny(ExprId),
    Return(Option<ExprId>),
    Break(LoopLabel, Option<ExprId>),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalVar(pub HirId);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LoopLabel(pub HirId);

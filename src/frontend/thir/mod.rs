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
pub mod visit;
define_id!(
    pub struct ExprId {}
);
define_id!(
    pub struct ArmId {}
);
define_id!(
    pub struct StmtId {}
);
define_id!(
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
    Binding(HirId, Symbol, ByRef, Mutable),
    Case(DefId, GenericArgs, Box<[Pattern]>),
    Tuple(Box<[Pattern]>),
    Lit(LiteralKind),
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
    pub span: Span,
    pub kind: StmtKind,
}
pub enum StmtKind {
    Let(Box<Pattern>, ExprId),
    Expr(ExprId),
}
pub struct ExprField {
    pub field: FieldIndex,
    pub expr: ExprId,
}
pub enum ExprKind {
    Literal(LiteralKind),
    Match(ExprId, Box<[ArmId]>),
    Call(ExprId, Box<[ExprId]>),
    Binary(BinaryOp, ExprId, ExprId),
    Unary(UnaryOp, ExprId),
    Index(ExprId, ExprId),
    Loop(ExprId),
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
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalVar(pub HirId);

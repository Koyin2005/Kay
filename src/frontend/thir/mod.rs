use fxhash::FxHashMap;

use crate::{define_id, frontend::{ast::{BinaryOp, ByRef, LiteralKind, Mutable}, hir::{DefId, Definition, HirId}}, indexvec::IndexVec, span::{symbol::Symbol, Span}, types::{GenericArgs, Type, VariantCaseIndex}};

define_id!(pub struct ExprId{});
define_id!(pub struct ArmId{});
pub struct Param{
    pub pattern : Pattern,
}
pub struct Thir{
    pub bodies : Box<[Body]>
}

pub struct Body{
    pub owner : DefId,
    pub params : Vec<Param>,
    pub arms : IndexVec<ArmId,Arm>,
    pub exprs : IndexVec<ExprId,Expr>,
}
pub enum PatternKind {
    Wilcard,
    Binding(HirId,Symbol,ByRef,Mutable),
    Case(DefId,VariantCaseIndex,GenericArgs,Box<[Pattern]>),
}
pub struct Pattern{
    pub ty : Type,
    pub span : Span,
    pub kind : PatternKind
}
pub struct Expr{
    pub ty : Type,
    pub span : Span,
    pub kind : ExprKind
}
pub struct Arm{
    pub pattern : Pattern,
    pub body : ExprId
}
pub enum ExprKind {
    Literal(LiteralKind),
    Match(ExprId,Box<[ArmId]>),
    Call(ExprId,Box<[ExprId]>),
    Binary(BinaryOp,ExprId,ExprId),
    Var(HirId),
    Tuple(Box<[ExprId]>),
    Constant(Definition,GenericArgs),
    NeverToAny(ExprId)
}
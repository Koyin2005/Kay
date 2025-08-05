use indexmap::IndexMap;

use crate::{
    define_id,
    span::{Span, symbol::Ident},
};

define_id! {
    #[derive(Debug)]
    pub struct HirId{}
}
define_id! {
    #[derive(Debug)]
    pub struct DefId{}
}
impl DefId {
    pub const FIRST: DefId = DefId(0);
    pub fn add(self, amount: usize) -> Self {
        Self::new(self.0 as usize + amount)
    }
}
pub enum PatternKind {
    Tuple(Vec<Pattern>),
    Ref(Box<Pattern>),
    Deref(Box<Pattern>),
    Wildcard,
}
pub struct Pattern {
    pub id: HirId,
    pub span: Span,
    pub kind: PatternKind,
}

pub struct Block {
    pub id: HirId,
    pub span: Span,
    pub stmts: Vec<Stmt>,
    pub result: Option<Expr>,
}
pub enum StmtKind {
    Let(Pattern, Box<Expr>),
    Item(DefId),
}
pub struct Stmt {
    pub id: HirId,
    pub span: Span,
    pub kind: StmtKind,
}
pub enum ExprKind {
    Loop(Box<Block>),
    If(Box<Expr>, Box<Block>, Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
}

pub struct Expr {
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}
pub struct Param {
    pub pat: Pattern,
}
pub enum TypeKind {}
pub struct Type {
    pub id: HirId,
    pub span: Span,
    pub kind: TypeKind,
}
pub struct Body {
    pub params: Vec<Param>,
    pub value: Expr,
}
pub enum FunctionReturnType {
    Implicit(Span),
    Explicit(Type),
}
pub struct FunctionSig {
    pub inputs: Vec<Type>,
    pub return_ty: FunctionReturnType,
}
pub struct FunctionDef {
    pub id: DefId,
    pub sig: FunctionSig,
    pub span: Span,
}
pub struct VariantCase {
    pub span: Span,
    pub fields: Vec<Type>,
}
pub struct VariantDef {
    pub span: Span,
    pub cases: Vec<VariantCase>,
}
pub struct StructField {
    pub id: HirId,
    pub def_id: DefId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}
pub struct StructDef {
    pub span: Span,
    pub fields: Vec<StructField>,
}
pub enum TypeDefKind {
    Struct(StructDef),
    Variant(VariantDef),
}
pub struct TypeDef {
    pub id: DefId,
    pub span: Span,
    pub kind: TypeDefKind,
}
pub enum ItemKind {
    Function(FunctionDef),
    TypeDef(TypeDef),
}
pub struct Item {
    pub kind: ItemKind,
}
pub struct Hir {
    pub items: IndexMap<DefId, Item>,
    pub bodies: IndexMap<HirId, Body>,
}

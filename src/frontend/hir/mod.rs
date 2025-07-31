use crate::{define_id, span::Span};

define_id! {
    #[derive(Debug)]
    pub struct HirId{}
}

define_id! {
    #[derive(Debug)]
    pub struct ItemId{}
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
    Item(ItemId),
}
pub struct Stmt {
    pub id: HirId,
    pub span: Span,
    pub kind: StmtKind,
}
pub enum ExprKind {
    Loop(Box<Block>),
    Break(Option<Box<Expr>>),
}

pub struct Expr {
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}

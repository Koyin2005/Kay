use crate::{
    define_id,
    span::{Span, symbol::Symbol},
};

#[derive(Clone, Debug)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub enum StmtKind {
    ExprWithSemi(Box<Expr>),
    Expr(Box<Expr>),
}
#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub type BinaryOp = Spanned<BinaryOpKind>;

#[derive(Clone, Debug)]
pub enum BinaryOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,

    And,
    Or,

    Equals,
    NotEquals,
}
impl BinaryOpKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Divide => "/",
            Self::Multiply => "*",
            Self::Subtract => "-",
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::And => "and",
            Self::Or => "or",
        }
    }
}
impl std::fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Debug)]
pub enum LiteralKind {
    Int(i64),
    Bool(bool),
    String(Symbol),
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Tuple(Vec<Expr>),
    Block(Block),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Literal(LiteralKind),
    Grouped(Box<Expr>),
    Error,
}

define_id! {
    #[derive(Debug)]
    struct NodeId{

    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NodeId").field(&self.0).finish()
    }
}

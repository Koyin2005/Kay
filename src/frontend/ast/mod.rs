use crate::{
    define_id,
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
};
pub mod pretty_print;

#[derive(Clone, Debug)]
pub struct Param {
    pub pattern: Pattern,
    pub ty: Type,
}
#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
}
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
    Item(Box<ItemKind>),
    Let(Box<Pattern>, Option<Box<Type>>, Box<Expr>),
    ExprWithSemi(Box<Expr>),
    Expr(Box<Expr>),
}
#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub type BinaryOp = Spanned<BinaryOpKind>;
pub type UnaryOp = Spanned<UnaryOpKind>;

#[derive(Clone, Debug, Copy)]
pub enum BinaryOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,

    And,
    Or,

    Equals,
    NotEquals,

    LesserThan,
    GreaterThan,

    LesserEquals,
    GreaterEquals,
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
            Self::LesserEquals => "<",
            Self::LesserThan => ">",
            Self::GreaterEquals => ">=",
            Self::GreaterThan => ">",
        }
    }
}
impl std::fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
#[derive(Clone, Debug, Copy)]
pub enum UnaryOpKind {
    Negate,
    Ref(Mutable),
}
impl UnaryOpKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Negate => "-",
            Self::Ref(Mutable::Yes(_)) => "ref mut",
            Self::Ref(Mutable::No) => "ref",
        }
    }
}
impl std::fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Function(FunctionDef),
}
#[derive(Clone, Debug)]
pub enum LiteralKind {
    Int(i64),
    Bool(bool),
    String(Symbol),
}

#[derive(Clone, Debug)]
pub enum IteratorExprKind {
    Range(Box<Expr>, Box<Expr>),
    Expr(Box<Expr>),
}
#[derive(Clone, Debug)]
pub struct IteratorExpr {
    pub span: Span,
    pub kind: IteratorExprKind,
}
#[derive(Clone, Debug)]
pub enum ExprKind {
    Tuple(Vec<Expr>),
    Block(Block),
    If(Box<Expr>, Box<Block>, Option<Box<Expr>>),
    Assign(Box<Expr>, Box<Expr>, Span),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Deref(Span, Box<Expr>),
    Literal(LiteralKind),
    Array(Vec<Expr>),
    While(Box<Expr>, Box<Block>),
    For(Box<Expr>, Box<IteratorExpr>, Box<Block>),
    Ident(Symbol),
    Grouped(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
    Field(Box<Expr>, Ident),
    As(Box<Expr>, Box<Type>),
}
#[derive(Clone, Debug)]
pub struct Pattern {
    pub span: Span,
    pub kind: PatternKind,
    pub id: NodeId,
}

#[derive(Clone, Debug, Copy)]
pub enum Mutable {
    Yes(Span),
    No,
}
#[derive(Clone, Debug, Copy)]
pub enum ByRef {
    Yes(Span),
    No,
}
#[derive(Clone, Debug)]
pub enum PatternKind {
    Ident(Symbol, Mutable, ByRef),
    Tuple(Vec<Pattern>),
    Grouped(Box<Pattern>),
    Literal(LiteralKind),
    Deref(Box<Pattern>),
    Wildcard,
}
#[derive(Clone, Debug)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Int,
    Bool,
    Uint,
    Never,
    Underscore,
    Grouped(Box<Type>),
    Tuple(Vec<Type>),
    Ref(Box<Type>),
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

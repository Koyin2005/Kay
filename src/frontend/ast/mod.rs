use crate::{
    define_id,
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
};

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
    pub body: Box<Expr>,
}
#[derive(Clone, Debug)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct Item {
    pub id: NodeId,
    pub span: Span,
    pub kind: ItemKind,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub enum StmtKind {
    Item(Box<Item>),
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
impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

pub type BinaryOp = Spanned<BinaryOpKind>;
pub type UnaryOp = Spanned<UnaryOpKind>;

#[derive(Clone, Debug, Copy, PartialEq)]
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
pub struct TypeDef {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub kind: TypeDefKind,
}
#[derive(Clone, Debug)]
pub enum TypeDefKind {
    Struct(Box<Struct>),
    Variant(Box<Variant>),
}
#[derive(Clone, Debug)]
pub enum ItemKind {
    Function(FunctionDef),
    Type(TypeDef),
}
#[derive(Clone, Debug, Copy)]
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
pub struct ExprField {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub expr: Expr,
}
#[derive(Clone, Debug)]
pub struct MatchArm {
    pub id: NodeId,
    pub span: Span,
    pub pat: Pattern,
    pub body: Expr,
}
#[derive(Clone, Debug, Copy)]
pub struct PathSegment {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Loop(Block),
    Tuple(Vec<Expr>),
    Block(Block),
    Match(Box<Expr>, Box<[MatchArm]>),
    If(Box<Expr>, Box<Block>, Option<Box<Expr>>),
    Assign(Box<Expr>, Box<Expr>, Span),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    Deref(Span, Box<Expr>),
    Literal(LiteralKind),
    Init(Option<QualifiedName>, Vec<ExprField>),
    Array(Vec<Expr>),
    While(Box<Expr>, Box<Block>),
    For(Box<Pattern>, Box<IteratorExpr>, Box<Block>),
    Ident(Symbol),
    Path(QualifiedName),
    Grouped(Box<Expr>),
    Underscore,
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
    Case(QualifiedName, Vec<Pattern>),
    Wildcard,
}
#[derive(Debug, Clone)]
pub struct VariantField {
    pub id: NodeId,
    pub ty: Type,
}
#[derive(Debug, Clone)]
pub struct VariantCase {
    pub id: NodeId,
    pub name: Ident,
    pub span: Span,
    pub fields: Option<Vec<VariantField>>,
}
#[derive(Debug, Clone)]
pub struct Variant {
    pub id: NodeId,
    pub span: Span,
    pub cases: Vec<VariantCase>,
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}
#[derive(Debug, Clone)]
pub struct Struct {
    pub id: NodeId,
    pub span: Span,
    pub fields: Vec<StructField>,
}
#[derive(Clone, Debug)]
pub struct Type {
    pub id: NodeId,
    pub kind: TypeKind,
    pub span: Span,
}
#[derive(Clone, Debug)]
pub struct QualifiedName {
    pub id: NodeId,
    pub span: Span,
    pub head: PathSegment,
    pub tail: Vec<PathSegment>,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Named(Box<QualifiedName>),
    Int,
    Bool,
    Uint,
    Never,
    String,
    Variant(Box<Variant>),
    Struct(Box<Struct>),
    Array(Box<Type>),
    Grouped(Box<Type>),
    Tuple(Vec<Type>),
    Ref(Mutable, Box<Type>),
    Fun(Vec<Type>, Option<Box<Type>>),
}
define_id! {
    #[derive(Debug)]
    pub struct NodeId{

    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NodeId").field(&self.0).finish()
    }
}

pub struct Ast {
    pub items: Vec<Item>,
}

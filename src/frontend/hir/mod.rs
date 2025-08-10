use indexmap::IndexMap;

use crate::{
    define_id,
    frontend::ast::{BinaryOp, ByRef, LiteralKind, Mutable, UnaryOp},
    span::{
        Span,
        symbol::{Ident, Symbol},
    },
};
#[derive(Clone, Copy, PartialEq, Debug, Hash, Eq)]
pub enum IntType {
    Signed,
    Unsigned,
}
#[derive(Clone, Copy, PartialEq, Debug, Hash, Eq)]
pub enum PrimitiveType {
    Int(IntType),
    String,
    Bool,
    Never,
}
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
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Definition {
    Builtin(Builtin),
    Def(DefId, DefKind),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DefKind {
    Struct,
    Field,
    Function,
    Variant,
    VariantCase,
}
impl DefKind {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Variant => "variant",
            Self::Struct => "struct",
            Self::Field => "field",
            Self::Function => "function",
            Self::VariantCase => "case",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Builtin {
    Option,
    OptionSome,
    OptionNone,
    Next,
    IntoIter,
    Println,
}
#[derive(Clone, Copy, Debug)]
pub enum Resolution<VarId = HirId> {
    Builtin(Builtin),
    Def(DefId, DefKind),
    Variable(VarId),
    Err,
}
impl<Id> Resolution<Id> {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Builtin(_) => "builtin",
            Self::Variable(_) => "variable",
            Self::Def(_, kind) => kind.as_str(),
            Self::Err => "{error}",
        }
    }
}
#[derive(Debug)]
pub struct Path {
    pub id: HirId,
    pub res: Resolution,
}
#[derive(Debug)]
pub enum PatternKind {
    Literal(LiteralKind),
    Tuple(Vec<Pattern>),
    Ref(Box<Pattern>),
    Deref(Box<Pattern>),
    Case(Resolution, Vec<Pattern>),
    Binding(HirId, Symbol, Mutable, ByRef),
    Wildcard,
}
#[derive(Debug)]
pub struct Pattern {
    pub id: HirId,
    pub span: Span,
    pub kind: PatternKind,
}
#[derive(Debug)]
pub struct Block {
    pub id: HirId,
    pub span: Span,
    pub stmts: Vec<Stmt>,
    pub result: Option<Expr>,
}
#[derive(Debug)]
pub enum StmtKind {
    Let(Pattern, Option<Type>, Box<Expr>),
    Expr(Expr),
    ExprWithSemi(Expr),
    Item(DefId),
}
#[derive(Debug)]
pub struct Stmt {
    pub id: HirId,
    pub span: Span,
    pub kind: StmtKind,
}
#[derive(Debug)]
pub struct MatchArm {
    pub id: HirId,
    pub span: Span,
    pub pat: Pattern,
    pub body: Expr,
}
#[derive(Debug)]
pub struct OutsideLoop;
#[derive(Debug)]
pub struct ExprField {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum ExprKind {
    Assign(Span, Box<Expr>, Box<Expr>),
    As(Box<Expr>, Type),
    Literal(LiteralKind),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Loop(Box<Block>),
    Path(Path),
    Deref(Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<MatchArm>),
    Unary(UnaryOp, Box<Expr>),
    Break(Result<HirId, OutsideLoop>, Option<Box<Expr>>),
    Block(Box<Block>),
    Return(Option<Box<Expr>>),
    Field(Box<Expr>, Ident),
    Init(Option<Path>, Vec<ExprField>),
    Err,
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}
#[derive(Debug)]
pub struct Expr {
    pub id: HirId,
    pub span: Span,
    pub kind: ExprKind,
}
#[derive(Debug)]
pub struct Param {
    pub pat: Pattern,
}
#[derive(Debug)]
pub struct StructTypeField {
    pub name: Ident,
    pub ty: Type,
}
#[derive(Debug)]
pub struct VariantTypeCase {
    pub name: Ident,
    pub fields: Vec<Type>,
}
#[derive(Debug)]
pub enum TypeKind {
    Tuple(Vec<Type>),
    Path(Path),
    Infer,
    Variant(Vec<VariantTypeCase>),
    Struct(Vec<StructTypeField>),
    Array(Box<Type>),
    Ref(Mutable, Box<Type>),
    Primitive(PrimitiveType),
}
#[derive(Debug)]
pub struct Type {
    pub id: HirId,
    pub span: Span,
    pub kind: TypeKind,
}
#[derive(Debug)]
pub struct Body {
    pub params: Vec<Param>,
    pub value: Expr,
}
#[derive(Debug)]
pub struct FunctionSig {
    pub inputs: Vec<Type>,
    pub output: Option<Type>,
}
#[derive(Debug)]
pub struct FunctionDef {
    pub id: DefId,
    pub sig: FunctionSig,
    pub body_id: HirId,
    pub span: Span,
}
#[derive(Debug)]
pub struct VariantField {
    pub id: DefId,
    pub span: Span,
    pub ty: Type,
}
#[derive(Debug)]
pub struct VariantCase {
    pub id: DefId,
    pub span: Span,
    pub fields: Vec<VariantField>,
}
#[derive(Debug)]
pub struct VariantDef {
    pub span: Span,
    pub cases: Vec<VariantCase>,
}
#[derive(Debug)]
pub struct StructField {
    pub id: DefId,
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
}
#[derive(Debug)]
pub struct StructDef {
    pub span: Span,
    pub fields: Vec<StructField>,
}
#[derive(Debug)]
pub enum TypeDefKind {
    Struct(StructDef),
    Variant(VariantDef),
}
#[derive(Debug)]
pub struct TypeDef {
    pub id: DefId,
    pub span: Span,
    pub kind: TypeDefKind,
}
#[derive(Debug)]
pub enum ItemKind {
    Function(FunctionDef),
    TypeDef(TypeDef),
}
#[derive(Debug)]
pub struct Item {
    pub id: DefId,
    pub kind: ItemKind,
}
#[derive(Debug)]
pub struct Hir {
    pub items: IndexMap<DefId, Item>,
    pub bodies: IndexMap<HirId, Body>,
}

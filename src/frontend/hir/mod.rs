use indexmap::IndexMap;

use crate::{
    builtins::Builtins,
    define_id,
    frontend::ast::{BinaryOp, ByRef, LiteralKind, Mutable, UnaryOp},
    indexvec::IndexVec,
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
impl From<DefId> for Definition {
    fn from(value: DefId) -> Self {
        Self::Def(value)
    }
}
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum Definition {
    Def(DefId),
}
impl From<Definition> for DefId {
    fn from(Definition::Def(def_id): Definition) -> Self {
        def_id
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DefKind {
    Struct,
    Field,
    Module,
    Function,
    Variant,
    VariantCase,
    TypeParam,
    RegionParam,
}
impl DefKind {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Variant => "variant",
            Self::Struct => "struct",
            Self::Module => "module",
            Self::Field => "field",
            Self::Function => "function",
            Self::VariantCase => "case",
            Self::RegionParam => "region param",
            Self::TypeParam => "type param",
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum Resolution<VarId = HirId> {
    Def(DefId, DefKind),
    Variable(VarId),
    Err,
}
impl<Id> Resolution<Id> {
    pub fn as_var(self) -> Option<Id> {
        match self {
            Self::Variable(id) => Some(id),
            _ => None,
        }
    }
    pub fn as_str(&self) -> &str {
        match self {
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
    Case(Resolution, Option<GenericArgs>, Vec<Pattern>),
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
#[derive(Debug, Clone, Copy)]
pub struct OutsideLoop;
#[derive(Debug)]
pub struct ExprField {
    pub id: HirId,
    pub span: Span,
    pub name: Ident,
    pub expr: Expr,
}
#[derive(Debug)]
pub enum Iterator {
    Ranged(Span, Box<Expr>, Box<Expr>),
    Expr(HirId, Box<Expr>),
}
#[derive(Debug)]
pub enum ExprKind {
    Assign(Span, Box<Expr>, Box<Expr>),
    Ascribe(Box<Expr>, Type),
    Literal(LiteralKind),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Loop(Box<Block>, LoopSource),
    For(Box<Pattern>, Box<Iterator>, Box<Block>),
    Path(Path, Option<GenericArgs>),
    Call(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<MatchArm>),
    Unary(UnaryOp, Box<Expr>),
    Break(Result<HirId, OutsideLoop>, Option<Box<Expr>>),
    Block(Box<Block>),
    Return(Option<Box<Expr>>),
    Field(Box<Expr>, Ident),
    Init(Option<Path>, Vec<ExprField>),
    Index(Box<Expr>, Box<Expr>),
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
pub enum GenericArg {
    Type(Type),
    Region(Region),
}
impl GenericArg {
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Type(_) => "type",
            Self::Region(_) => "region",
        }
    }
}
#[derive(Debug)]
pub struct GenericArgs {
    pub span: Span,
    pub args: Vec<GenericArg>,
}
#[derive(Debug, Clone)]
pub enum Region {
    Static,
    Param(Ident, DefId),
    Err,
}
#[derive(Debug)]
pub enum TypeKind {
    Tuple(Vec<Type>),
    Path(Path, Option<GenericArgs>),
    Infer,
    Array(Box<Type>),
    Ref(Mutable, Option<Region>, Box<Type>),
    Primitive(PrimitiveType),
    Fun(Vec<Type>, Option<Box<Type>>),
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
    pub name: Ident,
    pub has_body: bool,
    pub generics: Generics,
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
    pub name: Ident,
    pub span: Span,
    pub fields: Option<Vec<VariantField>>,
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
    pub name: Ident,
    pub generics: Generics,
    pub kind: TypeDefKind,
}
#[derive(Debug)]
pub enum ItemKind {
    Function(FunctionDef),
    TypeDef(TypeDef),
    Module(Symbol),
}
#[derive(Debug)]
pub struct Item {
    pub id: DefId,
    pub span: Span,
    pub kind: ItemKind,
}
#[derive(Debug)]
pub struct Hir {
    pub items: IndexMap<DefId, Item>,
    pub bodies: IndexMap<HirId, Body>,
    pub def_info: IndexVec<DefId, DefInfo>,
    pub builtins: Builtins,
}
#[derive(Debug)]
pub struct DefInfo {
    pub parent: Option<DefId>,
    pub kind: DefKind,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopSource {
    While,
    Explicit,
}
#[derive(Clone, Copy, Debug)]
pub enum GenericParamKind {
    Region,
    Type,
}
impl GenericParamKind {
    pub fn kind(&self) -> &'static str {
        match self {
            Self::Region => "region",
            Self::Type => "type",
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub struct GenericParam {
    pub def_id: DefId,
    pub name: Ident,
    pub kind: GenericParamKind,
}
#[derive(Clone, Debug)]
pub struct Generics {
    pub span: Span,
    pub params: Vec<GenericParam>,
}
impl Generics {
    pub const fn empty() -> Self {
        Self {
            span: Span::EMPTY,
            params: Vec::new(),
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Builtin {
    Len,
    Panic,
}

impl Builtin {
    pub const COUNT: usize = 2;
}

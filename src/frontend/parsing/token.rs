use crate::span::{Span, symbol::Symbol};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Semicolon,
    Equals,
    LesserThan,
    GreaterThan,
    Plus,
    Minus,
    Star,
    Slash,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Dot,
    Colon,
    Bang,
    Coma,
    ///^
    Caret,
    ///|
    Pipe,

    Arrow,
    ThickArrow,
    BangEquals,
    EqualsEquals,
    LesserEquals,
    GreaterEquals,
    DotDot,

    As,
    End,
    Else,
    If,
    While,
    Do,
    For,
    Then,
    Fun,
    Break,
    Return,
    Let,
    Mut,
    Match,
    And,
    Struct,
    With,
    Ref,
    Or,
    In,
    Is,
    Variant,
    Loop,
    Import,

    Uint,
    Int,
    Bool,
    String,
    Never,
    Wildcard,
    Type,

    Ident(Symbol),
    Literal(Literal),
    Unknown(char),
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringComplete {
    Yes,
    No,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Literal {
    Int(Symbol),
    True,
    False,
    String(Symbol, StringComplete),
}
#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
impl Token {
    pub fn empty() -> Self {
        Self {
            kind: TokenKind::Bang,
            span: Span::EMPTY,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.span.is_empty()
    }
}

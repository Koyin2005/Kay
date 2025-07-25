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
    Caret,

    Arrow,
    BangEquals,
    EqualsEquals,
    LesserEquals,
    GreaterEquals,
    DotDot,

    Begin,
    As,
    End,
    Then,
    Else,
    If,
    While,
    Do,
    For,
    Print,
    Fun,
    Break,
    Return,
    Let,
    Mut,
    And,
    Struct,
    Ref,
    Or,
    In,

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
            span: Span::new(0, 0),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.span.is_empty()
    }
}

use crate::span::{Span, symbol::Symbol};

#[derive(Clone, Copy, Debug)]
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
    Dot,
    Colon,
    Bang,

    EqualsEquals,
    LesserEquals,
    GreaterEquals,

    End,
    Then,
    Else,
    If,
    While,
    Do,
    For,
    Begin,
    Print,
    Fun,
    Let,
    Mut,
    And,
    Or,

    Uint,
    Int,
    Bool,
    String,
    Never,

    Ident(Symbol),
    Literal(Literal),
    Unknown(char),
    Eof,
}

#[derive(Clone, Copy, Debug)]
pub enum StringComplete {
    Yes,
    No,
}
#[derive(Clone, Copy, Debug)]
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

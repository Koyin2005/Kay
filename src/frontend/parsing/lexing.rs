use std::{iter::Peekable, str::CharIndices};

use crate::{
    SourceInfo,
    frontend::parsing::token::{Literal, StringComplete, Token, TokenKind},
    span::{
        Span,
        symbol::{Symbol, keywords},
    },
};

pub struct Lexer<'source> {
    source: &'source SourceInfo,
    curr_offset: u32,
    start_offset: u32,
    chars: Peekable<CharIndices<'source>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceInfo) -> Self {
        Self {
            source,
            chars: source.source().char_indices().peekable(),
            curr_offset: 0,
            start_offset: 0,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    self.advance();
                }
                _ => {
                    break;
                }
            }
        }
    }
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }
    fn advance(&mut self) {
        self.chars.next();
        self.curr_offset += 1;
    }
    ///Returns a symbol formed from the start_offset to the end_offset and its length
    fn symbol(&self) -> (Symbol, usize) {
        (
            Symbol::intern(
                &self.source.source()[self.start_offset as usize..self.curr_offset as usize],
            ),
            (self.curr_offset - self.start_offset) as usize,
        )
    }
    fn ident_or_keyword(&mut self) -> (TokenKind, usize) {
        while let Some(c) = self.peek()
            && (c == '_' || c.is_ascii_alphanumeric())
        {
            self.advance();
        }
        let (symbol, len) = self.symbol();
        let kind = match symbol {
            keywords::IF => TokenKind::If,
            keywords::THEN => TokenKind::Then,
            keywords::ELSE => TokenKind::Else,
            keywords::END => TokenKind::End,
            keywords::BEGIN => TokenKind::Begin,
            keywords::DO => TokenKind::Do,
            keywords::WHILE => TokenKind::While,
            keywords::FOR => TokenKind::For,
            keywords::TRUE => TokenKind::Literal(Literal::True),
            keywords::FALSE => TokenKind::Literal(Literal::False),
            keywords::PRINT => TokenKind::Print,
            keywords::FUN => TokenKind::Fun,
            symbol => TokenKind::Ident(symbol),
        };
        (kind, len)
    }
    fn number(&mut self) -> (TokenKind, usize) {
        while let Some('0'..='9') = self.peek() {
            self.advance();
        }
        let (symbol, len) = self.symbol();
        (TokenKind::Literal(Literal::Int(symbol)), len)
    }
    fn string(&mut self) -> (TokenKind, usize) {
        while let Some(c) = self.peek()
            && c != '\"'
        {
            self.advance();
        }
        let is_complete = if matches!(self.peek(), Some('\"')) {
            self.advance();
            StringComplete::Yes
        } else {
            StringComplete::No
        };
        let (symbol, len) = self.symbol();
        (
            TokenKind::Literal(Literal::String(symbol, is_complete)),
            len,
        )
    }
    fn next_token(&mut self) -> Token {
        macro_rules! single_token {
            ($token_kind:expr) => {
                ($token_kind, 1)
            };
        }
        macro_rules! comp_token {
            ($c:expr,$comp_token_kind:expr,$single_token_kind:expr) => {
                if let Some($c) = self.peek() {
                    self.advance();
                    ($comp_token_kind, 2)
                } else {
                    ($single_token_kind, 1)
                }
            };
        }
        self.skip_whitespace();
        let Some(c) = self.peek() else {
            return Token {
                kind: TokenKind::Eof,
                span: Span::new(self.curr_offset, 1),
            };
        };
        self.start_offset = self.curr_offset;
        self.advance();
        let (kind, len) = match c {
            c if c.is_ascii_alphabetic() || c == '_' => self.ident_or_keyword(),
            '\"' => self.string(),
            '1'..='9' => self.number(),
            '/' => single_token!(TokenKind::Slash),
            '*' => single_token!(TokenKind::Star),
            '+' => single_token!(TokenKind::Plus),
            '-' => single_token!(TokenKind::Minus),
            ';' => single_token!(TokenKind::Semicolon),
            '(' => single_token!(TokenKind::LeftParen),
            ')' => single_token!(TokenKind::RightParen),
            '.' => single_token!(TokenKind::Dot),
            ':' => single_token!(TokenKind::Colon),
            '=' => comp_token!('=', TokenKind::EqualsEquals, TokenKind::Equals),
            '<' => comp_token!('=', TokenKind::LesserEquals, TokenKind::LesserThan),
            '>' => comp_token!('=', TokenKind::GreaterEquals, TokenKind::GreaterThan),
            c => (TokenKind::Unknown(c), c.len_utf8()),
        };
        Token {
            kind,
            span: Span::new(self.start_offset, len as u32),
        }
    }
    pub fn tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            tokens.push(token);
            if let TokenKind::Eof = token.kind {
                break tokens;
            }
        }
    }
}

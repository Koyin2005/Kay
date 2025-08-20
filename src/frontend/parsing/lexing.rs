use std::{iter::Peekable, str::CharIndices};

use crate::{
    frontend::parsing::token::{Literal, StringComplete, Token, TokenKind},
    span::{SourceRef, Span, symbol::Symbol},
};

pub struct Lexer<'source> {
    source: &'source SourceRef,
    curr_offset: u32,
    start_offset: u32,
    chars: Peekable<CharIndices<'source>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceRef) -> Self {
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
                '/' => {
                    let Some(next_char) = self.peek_next() else {
                        break;
                    };
                    match next_char {
                        '/' => {
                            self.advance();
                            while let Some(c) = self.peek()
                                && c != '\n'
                            {
                                self.advance();
                            }
                        }
                        '*' => {
                            let mut depth = 1;
                            self.advance();
                            while let Some(c) = self.peek() {
                                let Some(next_c) = self.peek_next() else {
                                    self.advance();
                                    break;
                                };
                                self.advance();
                                if c == '/' && next_c == '*' {
                                    depth += 1;
                                } else if c == '*' && next_c == '/' {
                                    depth -= 1;
                                    self.advance();
                                }
                                if depth == 0 {
                                    break;
                                }
                            }
                        }
                        _ => break,
                    }
                }
                _ => {
                    break;
                }
            }
        }
    }
    fn peek_next(&mut self) -> Option<char> {
        let mut temp_iter = self.chars.clone();
        temp_iter.nth(1).map(|(_, c)| c)
    }
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }
    fn advance(&mut self) {
        let new_index = self.chars.next().map(|(i, c)| (i + c.len_utf8()) as u32);
        self.curr_offset = new_index.unwrap_or(self.curr_offset + 1);
    }

    fn current_symbol_str(&self) -> &str {
        &self.source.source()[self.start_offset as usize..self.curr_offset as usize]
    }
    ///Returns a symbol formed from the current start_offset to the current end_offset and its length
    fn curent_symbol(&self) -> (Symbol, usize) {
        (
            Symbol::intern(
                &self.source.source()[self.start_offset as usize..self.curr_offset as usize],
            ),
            (self.curr_offset - self.start_offset) as usize,
        )
    }
    ///Returns a symbol formed from the start_offset to the end_offset and its length
    fn symbol(&self, start_offset: u32, end_offset: u32) -> (Symbol, usize) {
        (
            Symbol::intern(&self.source.source()[start_offset as usize..end_offset as usize]),
            (end_offset - start_offset) as usize,
        )
    }
    fn ident_or_keyword(&mut self) -> (TokenKind, usize) {
        while let Some(c) = self.peek()
            && (c == '_' || c.is_ascii_alphanumeric())
        {
            self.advance();
        }
        let len = (self.curr_offset - self.start_offset) as usize;
        let kind = match self.current_symbol_str() {
            "loop" => TokenKind::Loop,
            "with" => TokenKind::With,
            "then" => TokenKind::Then,
            "if" => TokenKind::If,
            "is" => TokenKind::Is,
            "else" => TokenKind::Else,
            "do" => TokenKind::Do,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "true" => TokenKind::Literal(Literal::True),
            "false" => TokenKind::Literal(Literal::False),
            "fun" => TokenKind::Fun,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "int" => TokenKind::Int,
            "uint" => TokenKind::Uint,
            "bool" => TokenKind::Bool,
            "string" => TokenKind::String,
            "never" => TokenKind::Never,
            "struct" => TokenKind::Struct,
            "break" => TokenKind::Break,
            "return" => TokenKind::Return,
            "in" => TokenKind::In,
            "end" => TokenKind::End,
            "_" => TokenKind::Wildcard,
            "ref" => TokenKind::Ref,
            "as" => TokenKind::As,
            "type" => TokenKind::Type,
            "variant" => TokenKind::Variant,
            "match" => TokenKind::Match,
            symbol => TokenKind::Ident(Symbol::intern(symbol)),
        };
        (kind, len)
    }
    fn number(&mut self) -> (TokenKind, usize) {
        while let Some('0'..='9') = self.peek() {
            self.advance();
        }
        let (symbol, len) = self.curent_symbol();
        (TokenKind::Literal(Literal::Int(symbol)), len)
    }
    fn string(&mut self) -> (TokenKind, usize) {
        while let Some(c) = self.peek()
            && c != '"'
        {
            if c == '\\' && matches!(self.peek_next(), Some('"')) {
                self.advance();
            }
            self.advance();
        }
        let (is_complete, end_offset) = if matches!(self.peek(), Some('\"')) {
            self.advance();
            (StringComplete::Yes, self.curr_offset - 1)
        } else {
            (StringComplete::No, self.curr_offset)
        };
        let len = end_offset - self.start_offset;
        let (symbol, _) = self.symbol(self.start_offset + 1, end_offset);
        (
            TokenKind::Literal(Literal::String(symbol, is_complete)),
            len as usize,
        )
    }
    pub fn next_token(&mut self) -> Token {
        macro_rules! single_token {
            ($token_kind:expr) => {
                ($token_kind, 1)
            };
        }
        macro_rules! comp_token {
            ($c:expr,TokenKind::$comp_token_kind:ident,TokenKind::$single_token_kind:ident) => {
                if let Some($c) = self.peek() {
                    self.advance();
                    (TokenKind::$comp_token_kind, 2)
                } else {
                    (TokenKind::$single_token_kind, 1)
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
            '0'..='9' => self.number(),
            '!' => comp_token!('=', TokenKind::BangEquals, TokenKind::Bang),
            '/' => single_token!(TokenKind::Slash),
            '*' => single_token!(TokenKind::Star),
            '+' => single_token!(TokenKind::Plus),
            '-' => comp_token!('>', TokenKind::Arrow, TokenKind::Minus),
            ';' => single_token!(TokenKind::Semicolon),
            '(' => single_token!(TokenKind::LeftParen),
            ')' => single_token!(TokenKind::RightParen),
            '[' => single_token!(TokenKind::LeftBracket),
            ']' => single_token!(TokenKind::RightBracket),
            '.' => comp_token!('.', TokenKind::DotDot, TokenKind::Dot),
            ':' => single_token!(TokenKind::Colon),
            ',' => single_token!(TokenKind::Coma),
            '^' => single_token!(TokenKind::Caret),
            '{' => single_token!(TokenKind::LeftBrace),
            '}' => single_token!(TokenKind::RightBrace),
            '|' => single_token!(TokenKind::Pipe),
            '=' => match self.peek() {
                Some('=') => {
                    self.advance();
                    (TokenKind::EqualsEquals, 2)
                }
                Some('>') => {
                    self.advance();
                    (TokenKind::ThickArrow, 2)
                }
                _ => (TokenKind::Equals, 1),
            },
            '<' => comp_token!('=', TokenKind::LesserEquals, TokenKind::LesserThan),
            '>' => comp_token!('=', TokenKind::GreaterEquals, TokenKind::GreaterThan),
            c => (TokenKind::Unknown(c), c.len_utf8()),
        };
        Token {
            kind,
            span: Span::new(self.start_offset, len as u32),
        }
    }
}

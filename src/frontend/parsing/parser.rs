use std::cell::Cell;

use crate::{
    Lexer,
    errors::{Diagnostic, DiagnosticReporter, IntoDiagnosticMessage},
    frontend::{
        ast::{
            BinaryOp, BinaryOpKind, Block, ByRef, Expr, ExprKind, FunctionDef, ItemKind,
            IteratorExpr, IteratorExprKind, LiteralKind, Mutable, NodeId, Param, Pattern,
            PatternKind, Stmt, StmtKind, Type, TypeKind, UnaryOp, UnaryOpKind,
        },
        parsing::token::{Literal, StringComplete, Token, TokenKind},
    },
    indexvec::Idx,
    span::{Span, symbol::Ident},
};

/// Multiple variant may have 1 element if it ends with a coma
enum ElementsParsed<T> {
    Multiple(Vec<T>, Option<Span>),
    Single(T),
}

impl<T> From<ElementsParsed<T>> for Vec<T> {
    fn from(value: ElementsParsed<T>) -> Self {
        match value {
            ElementsParsed::Single(element) => vec![element],
            ElementsParsed::Multiple(elements, _) => elements,
        }
    }
}
pub struct Parser<'source> {
    diag_reporter: DiagnosticReporter<'source>,
    lexer: Lexer<'source>,
    current_token: Token,
    next_id: NodeId,
    panic_mode: Cell<bool>,
}
pub struct ParseError;

type ParseResult<T> = Result<T, ParseError>;
impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>, diag_reporter: DiagnosticReporter<'source>) -> Self {
        Self {
            current_token: Token::empty(),
            lexer,
            diag_reporter,
            next_id: NodeId::new(0),
            panic_mode: Cell::new(false),
        }
    }
    fn new_id(&mut self) -> NodeId {
        let node = self.next_id;
        self.next_id = NodeId::new(self.next_id.into_index() + 1);
        node
    }

    fn error_at(&self, msg: impl IntoDiagnosticMessage, span: Span) {
        if self.panic_mode.get() {
            return;
        }
        self.panic_mode.set(true);
        self.diag_reporter.add(Diagnostic::new(msg, span));
    }
    fn error_at_current(&self, msg: impl IntoDiagnosticMessage) {
        self.error_at(msg, self.current_token.span);
    }
    fn match_ident(&mut self) -> Option<Ident> {
        if let TokenKind::Ident(name) = self.current_token.kind {
            let span = self.current_token.span;
            self.advance();
            Some(Ident { symbol: name, span })
        } else {
            None
        }
    }
    fn expect_ident(&mut self, msg: impl IntoDiagnosticMessage) -> ParseResult<Ident> {
        let span = self.current_token.span;
        let TokenKind::Ident(symbol) = self.current_token.kind else {
            self.error_at_current(msg);
            return Err(ParseError);
        };
        self.advance();
        Ok(Ident { symbol, span })
    }
    fn expect(&mut self, kind: TokenKind, msg: impl IntoDiagnosticMessage) -> ParseResult<()> {
        if !self.match_current(kind) {
            self.error_at_current(msg);
            return Err(ParseError);
        }
        Ok(())
    }
    fn advance(&mut self) {
        self.current_token = loop {
            let next_token = self.lexer.next_token();
            if let TokenKind::Unknown(c) = next_token.kind {
                self.error_at(format!("Unknown '{}' character.", c), next_token.span);
                continue;
            }
            break next_token;
        };
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current_token.kind == kind
    }
    fn match_current(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }
    fn is_at_eof(&self) -> bool {
        self.check(TokenKind::Eof)
    }

    fn infix_binding_power(&self) -> Option<(u32, u32)> {
        Some(match self.current_token.kind {
            TokenKind::Equals => (2, 1),
            TokenKind::EqualsEquals | TokenKind::BangEquals => (3, 4),
            TokenKind::LesserThan
            | TokenKind::GreaterThan
            | TokenKind::LesserEquals
            | TokenKind::GreaterEquals => (5, 6),
            TokenKind::And | TokenKind::Or => (7, 8),
            TokenKind::Plus | TokenKind::Minus => (9, 10),
            TokenKind::Star | TokenKind::Slash => (11, 12),
            _ => return None,
        })
    }
    fn parse_literal(&mut self, literal: Literal) -> ParseResult<(Span, LiteralKind)> {
        let span = self.current_token.span;
        let kind = match literal {
            Literal::Int(value) => LiteralKind::Int(match value.as_str().parse() {
                Ok(value) => value,
                Err(err) => {
                    self.error_at_current(err.to_string());
                    return Err(ParseError);
                }
            }),
            Literal::True => LiteralKind::Bool(true),
            Literal::False => LiteralKind::Bool(false),
            Literal::String(string, complete) => {
                if let StringComplete::No = complete {
                    self.error_at_current("Unterminated string.");
                }
                LiteralKind::String(string)
            }
        };
        self.advance();
        Ok((span, kind))
    }
    fn parse_grouped_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();

        let elements =
            self.parse_delimited_by(TokenKind::RightParen, |parser| parser.parse_expr(0))?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightParen, "Expected ')' to enclose '('.");
        let span = start.combined(end);
        Ok(Expr {
            id: self.new_id(),
            span,
            kind: match elements {
                ElementsParsed::Multiple(elements, _) => ExprKind::Tuple(elements),
                ElementsParsed::Single(element) => ExprKind::Grouped(Box::new(element)),
            },
        })
    }
    fn parse_block(&mut self, start_span: Span) -> ParseResult<Block> {
        let (id, stmts) = self.parse_block_body([TokenKind::End])?;
        let end_span = self.current_token.span;
        let span = start_span.combined(end_span);
        let _ = self.expect(TokenKind::End, "Expected 'end'.");
        Ok(Block { id, stmts, span })
    }
    fn parse_block_expr(&mut self, start_span: Span) -> ParseResult<Expr> {
        let block = self.parse_block(start_span)?;
        Ok(Expr {
            id: self.new_id(),
            span: block.span,
            kind: ExprKind::Block(block),
        })
    }
    fn parse_if_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let condition = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::Then, "Expected 'then' after if condition.");
        let (then_body, has_else) = {
            let start_span = self.current_token.span;
            let (id, stmts) = self.parse_block_body([TokenKind::End, TokenKind::Else])?;
            let end_span = self.current_token.span;
            let span = start_span.combined(end_span);
            (Block { id, stmts, span }, self.check(TokenKind::Else))
        };
        let (end, else_branch) = if has_else {
            let start_span = self.current_token.span;
            self.advance();
            if self.check(TokenKind::If) {
                let if_expr = self.parse_if_expr()?;
                (start_span.combined(if_expr.span), Some(if_expr))
            } else {
                let block = self.parse_block_expr(start_span)?;
                (block.span, Some(block))
            }
        } else {
            let end = self.current_token.span;
            let _ = self.expect(TokenKind::End, "Expected 'end'.");
            (end, None)
        };
        Ok(Expr {
            id: self.new_id(),
            span: start.combined(end),
            kind: ExprKind::If(
                Box::new(condition),
                Box::new(then_body),
                else_branch.map(Box::new),
            ),
        })
    }
    fn parse_for_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let pattern = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::In, "Expected 'in' after 'for' pattern.");
        let expr = self.parse_expr(0)?;
        let iterator = if self.match_current(TokenKind::DotDot) {
            let end = self.parse_expr(0)?;
            let span = start.combined(end.span);
            IteratorExpr {
                span,
                kind: IteratorExprKind::Range(Box::new(expr), Box::new(end)),
            }
        } else {
            IteratorExpr {
                span: expr.span,
                kind: IteratorExprKind::Expr(Box::new(expr)),
            }
        };
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'for' iterator.");
        let block = self.parse_block(self.current_token.span)?;
        let span = start.combined(block.span);
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::For(Box::new(pattern), Box::new(iterator), Box::new(block)),
            span,
        })
    }
    fn parse_while_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let condition = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'while' condition.");
        let block = self.parse_block(self.current_token.span)?;
        let end = block.span;
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::While(Box::new(condition), Box::new(block)),
            span: start.combined(end),
        })
    }
    fn parse_exprs(&mut self, end: TokenKind) -> ParseResult<Vec<Expr>> {
        self.parse_delimited_by(end, |parser| parser.parse_expr(0))
            .map(Vec::from)
    }
    fn parse_array_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let elements = self.parse_exprs(TokenKind::RightBracket)?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightBracket, "Expected ']'.");
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::Array(elements),
            span: start.combined(end),
        })
    }
    fn unary_op(&mut self) -> Option<UnaryOp> {
        let start_token = self.current_token;
        if self.match_current(TokenKind::Minus) {
            Some(UnaryOp {
                node: UnaryOpKind::Negate,
                span: start_token.span,
            })
        } else if self.match_current(TokenKind::Ref) {
            let next_span = start_token.span;
            if self.match_current(TokenKind::Mut) {
                Some(UnaryOp {
                    node: UnaryOpKind::Ref(Mutable::Yes(next_span)),
                    span: start_token.span.combined(next_span),
                })
            } else {
                Some(UnaryOp {
                    node: UnaryOpKind::Ref(Mutable::No),
                    span: start_token.span,
                })
            }
        } else {
            None
        }
    }
    fn is_expr_start(&self) -> bool {
        match self.current_token.kind {
            TokenKind::Ident(..)
            | TokenKind::Literal(..)
            | TokenKind::Begin
            | TokenKind::LeftParen
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::Minus
            | TokenKind::If
            | TokenKind::While
            | TokenKind::For => true,
            _ => false,
        }
    }
    fn parse_optional_expr(&mut self) -> Option<ParseResult<Expr>> {
        if self.is_expr_start() {
            Some(self.parse_expr(0))
        } else {
            None
        }
    }
    fn parse_expr_prefix(&mut self) -> ParseResult<Expr> {
        match self.current_token.kind {
            TokenKind::Literal(literal) => {
                self.parse_literal(literal).map(|(span, literal)| Expr {
                    id: self.new_id(),
                    kind: ExprKind::Literal(literal),
                    span,
                })
            }
            TokenKind::LeftParen => self.parse_grouped_expr(),
            TokenKind::Begin => {
                let start_span = self.current_token.span;
                self.advance();
                self.parse_block_expr(start_span)
            }
            TokenKind::If => self.parse_if_expr(),
            TokenKind::While => self.parse_while_expr(),
            TokenKind::LeftBracket => self.parse_array_expr(),
            TokenKind::For => self.parse_for_expr(),
            TokenKind::Ident(name) => {
                let span = self.current_token.span;
                self.advance();
                Ok(Expr {
                    id: self.new_id(),
                    kind: ExprKind::Ident(name),
                    span,
                })
            }
            TokenKind::Break => {
                let start = self.current_token.span;
                self.advance();
                let expr = self.parse_optional_expr().transpose()?;
                let span = if let Some(expr) = expr.as_ref() {
                    expr.span.combined(start)
                } else {
                    start
                };
                Ok(Expr {
                    id: self.new_id(),
                    kind: ExprKind::Break(expr.map(Box::new)),
                    span,
                })
            }
            TokenKind::Return => {
                let start = self.current_token.span;
                self.advance();
                let expr = self.parse_optional_expr().transpose()?;
                let span = if let Some(expr) = expr.as_ref() {
                    expr.span.combined(start)
                } else {
                    start
                };
                Ok(Expr {
                    id: self.new_id(),
                    kind: ExprKind::Return(expr.map(Box::new)),
                    span,
                })
            }
            _ => {
                let Some(op) = self.unary_op() else {
                    self.error_at_current("Expected an expression.");
                    return Err(ParseError);
                };
                let expr = self.parse_expr(13)?;
                let span = expr.span.combined(op.span);
                Ok(Expr {
                    id: self.new_id(),
                    kind: ExprKind::Unary(op, Box::new(expr)),
                    span,
                })
            }
        }
    }
    fn parse_rest_infix(&mut self, mut lhs: Expr, min_bp: u32) -> ParseResult<Expr> {
        while let Some((l_bp, r_bp)) = self.infix_binding_power()
            && l_bp >= min_bp
        {
            let op_span = self.current_token.span;
            let op_kind = match self.current_token.kind {
                TokenKind::Plus => BinaryOpKind::Add,
                TokenKind::Minus => BinaryOpKind::Subtract,
                TokenKind::Star => BinaryOpKind::Multiply,
                TokenKind::Slash => BinaryOpKind::Divide,
                TokenKind::And => BinaryOpKind::And,
                TokenKind::Or => BinaryOpKind::Or,
                TokenKind::EqualsEquals => BinaryOpKind::Equals,
                TokenKind::BangEquals => BinaryOpKind::NotEquals,
                TokenKind::LesserThan => BinaryOpKind::LesserThan,
                TokenKind::GreaterThan => BinaryOpKind::GreaterThan,
                TokenKind::LesserEquals => BinaryOpKind::LesserEquals,
                TokenKind::GreaterEquals => BinaryOpKind::GreaterEquals,
                TokenKind::Equals => {
                    self.advance();
                    let rhs = self.parse_expr(r_bp)?;
                    let id = self.new_id();
                    let span = lhs.span.combined(rhs.span);
                    lhs = Expr {
                        id,
                        kind: ExprKind::Assign(Box::new(lhs), Box::new(rhs), op_span),
                        span,
                    };
                    continue;
                }
                kind => unreachable!("Can only have infix ops here, but got {:?}", kind),
            };
            self.advance();
            let rhs = self.parse_expr(r_bp)?;
            let id = self.new_id();
            let span = lhs.span.combined(rhs.span);
            lhs = Expr {
                id,
                kind: ExprKind::Binary(
                    BinaryOp {
                        span: op_span,
                        node: op_kind,
                    },
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                span,
            };
        }
        Ok(lhs)
    }
    fn parse_call(&mut self, callee: Expr) -> ParseResult<Expr> {
        let start = callee.span;
        self.advance();
        let args = self.parse_exprs(TokenKind::RightParen)?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightParen, "Expected ')' after arguments.");
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::Call(Box::new(callee), args),
            span: start.combined(end),
        })
    }
    fn parse_expr_postfix(&mut self, mut lhs: Expr) -> ParseResult<Expr> {
        loop {
            lhs = match self.current_token.kind {
                TokenKind::LeftParen => self.parse_call(lhs)?,
                TokenKind::Dot => {
                    self.advance();
                    let field_name =
                        if let TokenKind::Literal(Literal::Int(field)) = self.current_token.kind {
                            let span = self.current_token.span;
                            self.advance();
                            Ident {
                                symbol: field,
                                span,
                            }
                        } else {
                            self.expect_ident("Expect a valid field name")?
                        };
                    Expr {
                        id: self.new_id(),
                        span: lhs.span.combined(field_name.span),
                        kind: ExprKind::Field(Box::new(lhs), field_name),
                    }
                }
                TokenKind::As => {
                    self.advance();
                    let ty = self.parse_type()?;
                    Expr {
                        id: self.new_id(),
                        span: lhs.span.combined(ty.span),
                        kind: ExprKind::As(Box::new(lhs), Box::new(ty)),
                    }
                }
                TokenKind::Caret => {
                    let caret_span = self.current_token.span;
                    self.advance();
                    Expr {
                        id: self.new_id(),
                        span: lhs.span.combined(caret_span),
                        kind: ExprKind::Deref(caret_span, Box::new(lhs)),
                    }
                }
                _ => break Ok(lhs),
            };
        }
    }
    fn parse_expr(&mut self, min_bp: u32) -> ParseResult<Expr> {
        let lhs = self.parse_expr_prefix()?;
        let lhs = self.parse_expr_postfix(lhs)?;
        self.parse_rest_infix(lhs, min_bp)
    }
    fn expr_needs_semi(&self, expr: &ExprKind) -> bool {
        match expr {
            ExprKind::If(..) | ExprKind::Block(..) | ExprKind::While(..) | ExprKind::For(..) => {
                false
            }
            _ => true,
        }
    }
    fn parse_expr_stmt<const N: usize>(&mut self, stmt_ends: [TokenKind; N]) -> ParseResult<Stmt> {
        let expr = self.parse_expr(0)?;
        let end_token = self.current_token;

        let (span, kind) = if self.match_current(TokenKind::Semicolon) {
            (
                expr.span.combined(end_token.span),
                StmtKind::ExprWithSemi(Box::new(expr)),
            )
        } else {
            if self.expr_needs_semi(&expr.kind) && !stmt_ends.into_iter().any(|end| self.check(end))
            {
                self.error_at("Expected a ';'.", expr.span);
            }
            (expr.span, StmtKind::Expr(Box::new(expr)))
        };
        let id = self.new_id();
        Ok(Stmt { id, kind, span })
    }
    fn parse_prefix_pattern(&mut self) -> ParseResult<Pattern> {
        let (kind, span) = match self.current_token.kind {
            TokenKind::LeftParen => {
                let start = self.current_token.span;
                self.advance();
                let patterns = self
                    .parse_delimited_by(TokenKind::RightParen, |parser| parser.parse_pattern())?;
                let end = self.current_token.span;
                let _ = self.expect(TokenKind::RightParen, "Expected ')' at end of pattern.");
                (
                    match patterns {
                        ElementsParsed::Single(element) => PatternKind::Grouped(Box::new(element)),
                        ElementsParsed::Multiple(elements, _) => PatternKind::Tuple(elements),
                    },
                    start.combined(end),
                )
            }
            TokenKind::Wildcard => {
                let span = self.current_token.span;
                self.advance();
                (PatternKind::Wildcard, span)
            }
            TokenKind::Ident(name) => {
                let span = self.current_token.span;
                self.advance();
                (PatternKind::Ident(name, Mutable::No, ByRef::No), span)
            }
            TokenKind::Mut => {
                let span = self.current_token.span;
                self.advance();
                if let Some(name) = self.match_ident() {
                    (
                        PatternKind::Ident(name.symbol, Mutable::Yes(span), ByRef::No),
                        name.span,
                    )
                } else {
                    let pattern = self.parse_pattern()?;
                    self.error_at("Expected a variable name after 'mut'.", pattern.span);
                    return Ok(pattern);
                }
            }
            TokenKind::Ref => {
                let ref_span = self.current_token.span;
                self.advance();
                if let Some(name) = self.match_ident() {
                    (
                        PatternKind::Ident(name.symbol, Mutable::No, ByRef::Yes(ref_span)),
                        name.span,
                    )
                } else if self.check(TokenKind::Mut) {
                    let mut_span = self.current_token.span;
                    self.advance();
                    if let Some(name) = self.match_ident() {
                        (
                            PatternKind::Ident(
                                name.symbol,
                                Mutable::Yes(mut_span),
                                ByRef::Yes(ref_span),
                            ),
                            name.span,
                        )
                    } else {
                        let pattern = self.parse_pattern()?;
                        self.error_at(
                            "Expected a variable name or 'mut' after 'ref'.",
                            pattern.span,
                        );
                        return Ok(pattern);
                    }
                } else {
                    let pattern = self.parse_pattern()?;
                    self.error_at(
                        "Expected a variable name or 'mut' after 'ref'.",
                        pattern.span,
                    );
                    return Ok(pattern);
                }
            }
            TokenKind::Literal(literal) => {
                let (span, literal) = self.parse_literal(literal)?;
                (PatternKind::Literal(literal), span)
            }
            _ => {
                self.error_at_current("Invalid pattern.");
                return Err(ParseError);
            }
        };
        Ok(Pattern {
            id: self.new_id(),
            span: span,
            kind: kind,
        })
    }
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let mut pattern = self.parse_prefix_pattern()?;
        loop {
            match self.current_token.kind {
                TokenKind::Caret => {
                    let span = self.current_token.span;
                    self.advance();
                    pattern = Pattern {
                        id: self.new_id(),
                        span: pattern.span.combined(span),
                        kind: PatternKind::Deref(Box::new(pattern)),
                    }
                }
                _ => break Ok(pattern),
            }
        }
    }
    fn parse_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current_token.span;
        let (kind, span) = match self.current_token.kind {
            TokenKind::Int => {
                self.advance();
                (TypeKind::Int, start_span)
            }
            TokenKind::Bool => {
                self.advance();
                (TypeKind::Bool, start_span)
            },
            TokenKind::Uint => {
                self.advance();
                (TypeKind::Uint, start_span)
            },
            TokenKind::Never => {
                self.advance();
                (TypeKind::Never, start_span)
            },
            TokenKind::LeftParen => {
                self.advance();
                let types = self
                    .parse_delimited_by(TokenKind::RightParen, |this| this.parse_type())?
                    .into();
                let span = start_span.combined(self.current_token.span);
                let _ = self.expect(TokenKind::RightParen, "Expected ')' at end of tuple type.");
                (TypeKind::Tuple(types), span)
            },
            _ => return Err(ParseError),
        };
        Ok(Type {
            id: self.new_id(),
            kind,
            span,
        })
    }
    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        let start = self.current_token.span;
        self.advance();
        let pattern = self.parse_pattern()?;
        let ty = if self.match_current(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let _ = self.expect(TokenKind::Equals, "Expected '='.");
        let expr = self.parse_expr(0)?;
        let end = self.current_token.span;
        let _ = self.expect(
            TokenKind::Semicolon,
            "Expected ';' at end of 'let' statement.",
        );
        Ok(Stmt {
            id: self.new_id(),
            kind: StmtKind::Let(Box::new(pattern), ty.map(Box::new), Box::new(expr)),
            span: start.combined(end),
        })
    }
    fn parse_delimited_by<T>(
        &mut self,
        delimiter: TokenKind,
        mut f: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<ElementsParsed<T>> {
        let mut parsed = ElementsParsed::Multiple(Vec::new(), None);
        while !self.is_at_eof() && !self.check(delimiter) {
            let element = f(self)?;
            let coma_span = self.current_token.span;
            if !self.match_current(TokenKind::Coma) {
                match parsed {
                    ElementsParsed::Multiple(ref mut elements, ref mut old_coma_span) => {
                        if elements.is_empty() {
                            parsed = ElementsParsed::Single(element);
                        } else {
                            elements.push(element);
                            *old_coma_span = None;
                        }
                    }
                    ElementsParsed::Single(first_element) => {
                        parsed = ElementsParsed::Multiple(vec![element, first_element], None);
                    }
                }
                break;
            }

            match parsed {
                ElementsParsed::Multiple(ref mut elements, ref mut old_coma_span) => {
                    elements.push(element);
                    *old_coma_span = Some(coma_span);
                }
                ElementsParsed::Single(first_element) => {
                    parsed =
                        ElementsParsed::Multiple(vec![element, first_element], Some(coma_span));
                }
            }
        }
        Ok(parsed)
    }
    fn parse_fun_param(&mut self) -> ParseResult<Param> {
        let pattern = self.parse_pattern()?;
        let _ = self.expect(TokenKind::Colon, "Expected ':' after param pattern.");
        let ty = self.parse_type()?;
        Ok(Param { pattern, ty })
    }
    fn parse_fun_def(&mut self) -> ParseResult<Stmt> {
        let start = self.current_token.span;
        self.advance();

        let function_name = self.expect_ident("Expected a function name")?;
        let _ = self.expect(TokenKind::LeftParen, "Expected '(' after 'function' name.");
        let params = self
            .parse_delimited_by(TokenKind::RightParen, Self::parse_fun_param)?
            .into();
        let _ = self.expect(
            TokenKind::RightParen,
            "Expected ')' after 'function' arguments.",
        );

        let body = self.parse_block(self.current_token.span)?;

        let end = self.current_token.span;

        let span = start.combined(end);
        Ok(Stmt {
            kind: StmtKind::Item(Box::new(ItemKind::Function(FunctionDef {
                id: self.new_id(),
                span,
                name: function_name,
                params,
                body,
            }))),
            id: self.new_id(),
            span,
        })
    }
    fn parse_stmt<const N: usize>(&mut self, stmt_ends: [TokenKind; N]) -> ParseResult<Stmt> {
        match self.current_token.kind {
            TokenKind::Let => self.parse_let_stmt(),
            TokenKind::Fun => self.parse_fun_def(),
            _ => self.parse_expr_stmt(stmt_ends),
        }
    }
    fn parse_block_body<const N: usize>(
        &mut self,
        ends: [TokenKind; N],
    ) -> ParseResult<(NodeId, Vec<Stmt>)> {
        let id = self.new_id();
        let mut stmts = Vec::new();
        while !self.is_at_eof() && !ends.into_iter().any(|end| self.check(end)) {
            stmts.push(self.parse_stmt(ends)?);
        }
        Ok((id, stmts))
    }
    pub fn parse(mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        self.advance();
        while !self.is_at_eof() {
            self.panic_mode.set(false);
            let Ok(stmt) = self.parse_stmt([]) else {
                while !self.is_at_eof() {
                    if matches!(
                        self.current_token.kind,
                        TokenKind::Semicolon | TokenKind::End
                    ) {
                        self.advance();
                        break;
                    }
                    self.advance();
                    if matches!(
                        self.current_token.kind,
                        TokenKind::Do | TokenKind::For | TokenKind::While
                    ) {
                        break;
                    }
                }
                continue;
            };
            stmts.push(stmt);
        }
        self.diag_reporter.emit();
        Ok(stmts)
    }
}

use std::{cell::Cell, vec};

use crate::{
    errors::{Diagnostic, DiagnosticReporter, IntoDiagnosticMessage}, frontend::{
        ast::{
            BinaryOp, BinaryOpKind, Block, ByRef, Expr, ExprField, ExprKind, FunctionDef, GenericArg, GenericArgs, GenericParam, GenericParams, Item, ItemKind, IteratorExpr, IteratorExprKind, LiteralKind, MatchArm, Module, Mutable, NodeId, Param, PathSegment, Pattern, PatternKind, QualifiedName, Stmt, StmtKind, Struct, StructField, Type, TypeDef, TypeDefKind, TypeKind, UnaryOp, UnaryOpKind, Variant, VariantCase, VariantField
        },
        parsing::token::{Literal, StringComplete, Token, TokenKind},
    }, indexvec::Idx, span::{
        symbol::{Ident, Symbol}, Span
    }, Lexer
};
#[derive(Clone, Copy)]
enum BlockKind {
    Else(),
    Normal(),
}
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
    diag_reporter: DiagnosticReporter,
    lexer: Lexer<'source>,
    current_token: Token,
    next_id: NodeId,
    panic_mode: Cell<bool>,
    mod_name: Symbol,
}
pub struct ParseError;

type ParseResult<T> = Result<T, ParseError>;
impl<'source> Parser<'source> {
    pub fn new(
        name: &str,
        lexer: Lexer<'source>,
        diag_reporter: DiagnosticReporter,
        start_id: NodeId,
    ) -> Self {
        Self {
            mod_name: Symbol::intern(name),
            current_token: Token::empty(),
            lexer,
            diag_reporter,
            next_id: start_id,
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
        if !self.matches_current(kind) {
            self.error_at_current(msg);
            return Err(ParseError);
        }
        Ok(())
    }
    fn expect_at(
        &mut self,
        kind: TokenKind,
        msg: impl IntoDiagnosticMessage,
        span: Span,
    ) -> ParseResult<()> {
        if !self.matches_current(kind) {
            self.error_at(msg, span);
            return Err(ParseError);
        }
        Ok(())
    }
    fn advance(&mut self) {
        self.current_token = loop {
            let next_token = self.lexer.next_token();
            if let TokenKind::Unknown(c) = next_token.kind {
                self.error_at(format!("Unknown '{c}' character."), next_token.span);
                continue;
            }
            break next_token;
        };
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.current_token.kind == kind
    }
    fn match_current(&mut self, kind: TokenKind) -> Option<Token> {
        if self.check(kind) {
            let current = self.current_token;
            self.advance();
            Some(current)
        } else {
            None
        }
    }
    fn matches_current(&mut self, kind: TokenKind) -> bool {
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
            TokenKind::Or => (5, 6),
            TokenKind::And => (8, 7),
            TokenKind::LesserThan
            | TokenKind::GreaterThan
            | TokenKind::LesserEquals
            | TokenKind::GreaterEquals => (9, 10),
            TokenKind::Plus | TokenKind::Minus => (11, 12),
            TokenKind::Star | TokenKind::Slash => (13, 14),
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
    fn parse_block_with_end(&mut self, start_span: Span) -> ParseResult<Block> {
        let mut block = self.parse_block_body([TokenKind::End], start_span)?;
        let curr_span = self.current_token.span;
        if self
            .expect(TokenKind::End, "Expected 'end' at end of block.")
            .is_ok()
        {
            block.span = block.span.combined(curr_span);
        }
        Ok(block)
    }
    fn parse_block_body<const N: usize>(
        &mut self,
        endings: [TokenKind; N],
        start_span: Span,
    ) -> ParseResult<Block> {
        let id = self.new_id();
        let mut stmts = Vec::new();
        while !self.is_at_eof() && !endings.iter().any(|&end| self.check(end)) {
            stmts.push(
                self.parse_stmt(Some(
                    endings
                        .contains(&TokenKind::Else)
                        .then(BlockKind::Else)
                        .unwrap_or(BlockKind::Normal()),
                ))?,
            );
        }
        let end_span = self.current_token.span;
        let span = start_span.combined(end_span);
        Ok(Block { id, stmts, span })
    }
    fn parse_block_expr(&mut self, start_span: Span) -> ParseResult<Expr> {
        let block = self.parse_block_with_end(start_span)?;
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
        let then_start_span = self.current_token.span;
        let _ = self.expect(TokenKind::Then, "Expected 'then' after 'if' condition.");
        let (then_body, has_else) = {
            (
                self.parse_block_body([TokenKind::Else, TokenKind::End], then_start_span)?,
                self.check(TokenKind::Else),
            )
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
            let _ = self.expect(TokenKind::End, "Expected 'end' for 'if'.");
            (then_body.span, None)
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
        let pattern = self.parse_pattern()?;
        let _ = self.expect(TokenKind::In, "Expected 'in' after 'for' pattern.");
        let expr = self.parse_expr(0)?;
        let iterator = if self.matches_current(TokenKind::DotDot) {
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
        let start_span = self.current_token.span;
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'for' iterator.");
        let block = self.parse_block_with_end(start_span)?;
        let span = start.combined(block.span);
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::For(Box::new(pattern), Box::new(iterator), Box::new(block)),
            span,
        })
    }
    fn parse_init_expr_fields(&mut self) -> ParseResult<(Vec<ExprField>, Span)> {
        let start = self.current_token.span;
        self.advance();

        let fields: Vec<_> = self
            .parse_delimited_by(TokenKind::RightBrace, |this| {
                let start_span = this.current_token.span;
                let name = this.expect_ident("Expected field name");
                if this
                    .expect(TokenKind::Equals, "Expected '=' after field name.")
                    .is_err()
                    && this.check(TokenKind::Colon)
                {
                    this.advance();
                }
                let expr = this.parse_expr(0)?;
                Ok(ExprField {
                    id: this.new_id(),
                    span: start_span.combined(expr.span),
                    name: name?,
                    expr,
                })
            })?
            .into();

        let end_span = self.current_token.span;
        let _ = self.expect(TokenKind::RightBrace, "Expected '}'.");
        Ok((fields, start.combined(end_span)))
    }
    fn parse_field_expr(&mut self, lhs: Expr) -> ParseResult<Expr> {
        self.advance();
        let (field_name, is_int) =
            if let TokenKind::Literal(Literal::Int(field)) = self.current_token.kind {
                let span = self.current_token.span;
                self.advance();
                (
                    Ident {
                        symbol: field,
                        span,
                    },
                    true,
                )
            } else {
                (self.expect_ident("Expect a valid field name")?, false)
            };

        let (kind, span) = match lhs.kind {
            ExprKind::Ident(name) if !is_int => {
                let span = lhs.span.combined(field_name.span);
                (
                    ExprKind::Path(QualifiedName {
                        id: self.new_id(),
                        span,
                        head: PathSegment {
                            id: lhs.id,
                            span: lhs.span,
                            name: Ident {
                                symbol: name,
                                span: lhs.span,
                            },
                        },
                        tail: vec![PathSegment {
                            id: self.new_id(),
                            span: field_name.span,
                            name: field_name,
                        }],
                    }),
                    span,
                )
            }
            ExprKind::Path(mut path) if !is_int => {
                path.tail.push(PathSegment {
                    id: self.new_id(),
                    span: field_name.span,
                    name: field_name,
                });
                (ExprKind::Path(path), lhs.span.combined(field_name.span))
            }
            _ => {
                let span = lhs.span.combined(field_name.span);
                (ExprKind::Field(Box::new(lhs), field_name), span)
            }
        };
        Ok(Expr {
            id: self.new_id(),
            span,
            kind,
        })
    }
    fn parse_init_expr(&mut self, lhs: Option<Expr>) -> ParseResult<Expr> {
        let start = self.current_token.span;
        let name = lhs
            .and_then(|expr| {
                if let ExprKind::Path(name) = expr.kind {
                    Some(Ok(name))
                } else if let ExprKind::Ident(name) = expr.kind {
                    Some(Ok(QualifiedName {
                        id: expr.id,
                        span: expr.span,
                        head: PathSegment {
                            id: self.new_id(),
                            span: expr.span,
                            name: Ident {
                                symbol: name,
                                span: expr.span,
                            },
                        },
                        tail: Vec::new(),
                    }))
                } else if let ExprKind::Underscore = expr.kind {
                    None
                } else {
                    self.error_at("Invalid initializer", expr.span);
                    Some(Err(ParseError))
                }
            })
            .transpose()?;
        let (fields, end_span) = self.parse_init_expr_fields()?;
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::Init(name, fields),
            span: start.combined(end_span),
        })
    }
    fn parse_while_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let condition = self.parse_expr(0)?;
        let start_span = self.current_token.span;
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'while' condition.");
        let block = self.parse_block_with_end(start_span)?;
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
        if self.matches_current(TokenKind::Minus) {
            Some(UnaryOp {
                node: UnaryOpKind::Negate,
                span: start_token.span,
            })
        } else if self.matches_current(TokenKind::Ref) {
            let next_span = start_token.span;
            if self.matches_current(TokenKind::Mut) {
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
        matches!(
            self.current_token.kind,
            TokenKind::Ident(..)
                | TokenKind::Literal(..)
                | TokenKind::Do
                | TokenKind::LeftParen
                | TokenKind::Return
                | TokenKind::Break
                | TokenKind::Minus
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::LeftBracket
                | TokenKind::Loop
                | TokenKind::Ref
                | TokenKind::Match
                | TokenKind::Wildcard
        )
    }
    fn parse_optional_expr(&mut self) -> Option<ParseResult<Expr>> {
        if self.is_expr_start() {
            Some(self.parse_expr(0))
        } else {
            None
        }
    }
    fn parse_match_expr(&mut self) -> ParseResult<Expr> {
        let start_span = self.current_token.span;
        self.advance();
        let scrutinee = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::With, "Expected 'with' after 'match' scrutinee.");
        let arms: Vec<_> = self
            .parse_delimited_by(TokenKind::End, |this| {
                let span = this.current_token.span;
                let pattern = this.parse_pattern()?;
                let _ = this.expect(TokenKind::ThickArrow, "Expected '=>' ater pattern.");
                let body = this.parse_expr(0)?;
                Ok(MatchArm {
                    id: this.new_id(),
                    span: span.combined(body.span),
                    pat: pattern,
                    body,
                })
            })?
            .into();
        let end_span = self.current_token.span;
        let _ = self.expect(TokenKind::End, "Expected 'end'.");
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::Match(Box::new(scrutinee), Box::from(arms)),
            span: start_span.combined(end_span),
        })
    }
    fn parse_loop(&mut self) -> ParseResult<Expr> {
        let start_span = self.current_token.span;
        self.advance();
        let block = self.parse_block_with_end(start_span)?;
        let span = block.span;
        Ok(Expr {
            id: self.new_id(),
            kind: ExprKind::Loop(block),
            span,
        })
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
            TokenKind::Wildcard => {
                let span = self.current_token.span;
                self.advance();
                Ok(Expr {
                    id: self.new_id(),
                    kind: ExprKind::Underscore,
                    span,
                })
            }
            TokenKind::Do => {
                let span = self.current_token.span;
                self.advance();
                self.parse_block_expr(span)
            }
            TokenKind::LeftParen => self.parse_grouped_expr(),
            TokenKind::If => self.parse_if_expr(),
            TokenKind::While => self.parse_while_expr(),
            TokenKind::LeftBracket => self.parse_array_expr(),
            TokenKind::For => self.parse_for_expr(),
            TokenKind::Match => self.parse_match_expr(),
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
            TokenKind::Loop => self.parse_loop(),
            _ => {
                let Some(op) = self.unary_op() else {
                    self.error_at_current("Expected an expression.");
                    return Err(ParseError);
                };
                let expr = self.parse_expr(25)?;
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
                TokenKind::Dot => self.parse_field_expr(lhs)?,
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
                TokenKind::LeftBrace => self.parse_init_expr(Some(lhs))?,
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
        !matches!(
            expr,
            ExprKind::If(..)
                | ExprKind::Block(..)
                | ExprKind::While(..)
                | ExprKind::For(..)
                | ExprKind::Match(..)
                | ExprKind::Loop(..)
        )
    }
    fn parse_expr_stmt(&mut self, in_block: Option<BlockKind>) -> ParseResult<Stmt> {
        let expr = self.parse_expr(0)?;
        let (span, kind) = if let Some(semi_token) = self.match_current(TokenKind::Semicolon) {
            (
                expr.span.combined(semi_token.span),
                StmtKind::ExprWithSemi(Box::new(expr)),
            )
        } else {
            if self.expr_needs_semi(&expr.kind)
                && (in_block.is_none_or(|kind| match kind {
                    BlockKind::Else() => {
                        !self.check(TokenKind::Else) && !self.check(TokenKind::End)
                    }
                    BlockKind::Normal() => !self.check(TokenKind::End),
                }))
            {
                self.error_at("Expected a ';'.", expr.span);
            }
            (expr.span, StmtKind::Expr(Box::new(expr)))
        };
        let id = self.new_id();
        Ok(Stmt { id, kind, span })
    }
    fn parse_case_pattern_fields(&mut self, start: Span) -> ParseResult<(Vec<Pattern>, Span)> {
        Ok(
            if let Some(prev) = self.match_current(TokenKind::LeftParen) {
                let start = prev.span;
                let patterns: Vec<_> = self
                    .parse_delimited_by(TokenKind::RightParen, |this| this.parse_pattern())?
                    .into();
                let last_paren = self.current_token.span;
                let end_span = self
                    .expect(TokenKind::RightParen, "Expected ')'.")
                    .is_ok()
                    .then_some(last_paren)
                    .and_then(|_| patterns.last().map(|pat| pat.span))
                    .unwrap_or(start);
                (patterns, end_span)
            } else {
                (Vec::new(), start)
            },
        )
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

                let tail = self.parse_qual_name_tail()?;
                if tail.is_empty() {
                    (PatternKind::Ident(name, Mutable::No, ByRef::No), span)
                } else {
                    let (fields, end) = self.parse_case_pattern_fields(span)?;
                    (
                        PatternKind::Case(
                            QualifiedName {
                                id: self.new_id(),
                                span: tail
                                    .last()
                                    .map(|last| span.combined(last.span))
                                    .unwrap_or(span),
                                head: PathSegment {
                                    id: self.new_id(),
                                    span,
                                    name: Ident::from_symbol(name, span),
                                },
                                tail,
                            },
                            fields,
                        ),
                        span.combined(end),
                    )
                }
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
            span,
            kind,
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
    fn parse_struct_def(&mut self) -> ParseResult<Struct> {
        let start_span = self.current_token.span;
        let _ = self.expect(TokenKind::LeftBrace, "Expected '{'.");
        let fields: Vec<_> = self
            .parse_delimited_by(TokenKind::RightBrace, |this| {
                let name = this.expect_ident("Expected a valid field name.")?;
                let _ = this.expect(TokenKind::Colon, "Expected ':' after field name.");
                let ty = this.parse_type()?;
                Ok(StructField {
                    id: this.new_id(),
                    span: name.span.combined(ty.span),
                    name,
                    ty,
                })
            })?
            .into();
        let end_span = self.current_token.span;
        let _ = self.expect(TokenKind::RightBrace, "Expected '}' after struct fields.");
        Ok(Struct {
            id: self.new_id(),
            span: start_span.combined(end_span),
            fields,
        })
    }
    fn parse_variant_case(&mut self) -> ParseResult<VariantCase> {
        let name = self.expect_ident("Expected a variant name")?;
        let (fields, end) = if self.matches_current(TokenKind::LeftParen) {
            let fields = self
                .parse_delimited_by(TokenKind::RightParen, |this| {
                    let field = this.parse_type()?;
                    Ok(VariantField {
                        id: this.new_id(),
                        ty: field,
                    })
                })?
                .into();
            let end = self.current_token.span;
            let _ = self.expect(
                TokenKind::RightParen,
                "Expected ')' at the end of variant case.",
            );
            (Some(fields), end)
        } else {
            (None, name.span)
        };
        Ok(VariantCase {
            id: self.new_id(),
            name,
            span: name.span.combined(end),
            fields,
        })
    }
    fn parse_variant_def(&mut self) -> ParseResult<Variant> {
        let start_span = self.current_token.span;
        let mut cases = Vec::new();
        let mut end_span = start_span;
        while self.matches_current(TokenKind::Pipe) {
            let case = self.parse_variant_case()?;
            if !self.check(TokenKind::Pipe) {
                end_span = case.span;
            }
            cases.push(case);
        }
        Ok(Variant {
            id: self.new_id(),
            span: start_span.combined(end_span),
            cases,
        })
    }
    fn parse_qual_name(&mut self) -> ParseResult<QualifiedName> {
        let head = self.expect_ident("Expected a valid name.")?;
        let tail = self.parse_qual_name_tail()?;
        Ok(QualifiedName {
            id: self.new_id(),
            head: PathSegment {
                id: self.new_id(),
                span: head.span,
                name: head,
            },
            span: tail
                .last()
                .map(|last| head.span.combined(last.span))
                .unwrap_or(head.span),
            tail,
        })
    }
    fn parse_qual_name_tail(&mut self) -> ParseResult<Vec<PathSegment>> {
        let mut names = Vec::new();
        while let TokenKind::Dot = self.current_token.kind {
            self.advance();
            let name = self.expect_ident("Expected a valid ident.")?;
            names.push(PathSegment {
                id: self.new_id(),
                span: name.span,
                name,
            });
        }
        Ok(names)
    }
    fn parse_generic_args(&mut self) -> ParseResult<GenericArgs>{
        let start_span = self.current_token.span;
        self.advance();
        let args : Vec<_> = self.parse_delimited_by(TokenKind::RightBracket, |this|{
            let ty = this.parse_type()?;
            Ok(GenericArg{ ty})
        })?.into();
        let end_span = self.current_token.span;
        let _ = self.expect(TokenKind::RightBracket, "Expected ']' after generic arguments.");
        Ok(GenericArgs { span: start_span.combined(end_span) , args })
    }
    fn parse_optional_generic_args(&mut self) -> ParseResult<Option<GenericArgs>>{
        if self.check(TokenKind::LeftBracket){
            Ok(Some(self.parse_generic_args()?))
        }
        else{
            Ok(None)
        }
    }
    fn parse_type(&mut self) -> ParseResult<Type> {
        let start_span = self.current_token.span;
        let (kind, span) = match self.current_token.kind {
            TokenKind::Int => {
                self.advance();
                (TypeKind::Int, start_span)
            }
            TokenKind::Wildcard => {
                self.advance();
                (TypeKind::Underscore, start_span)
            }
            TokenKind::Bool => {
                self.advance();
                (TypeKind::Bool, start_span)
            }
            TokenKind::Uint => {
                self.advance();
                (TypeKind::Uint, start_span)
            }
            TokenKind::Never => {
                self.advance();
                (TypeKind::Never, start_span)
            }
            TokenKind::String => {
                self.advance();
                (TypeKind::String, start_span)
            }
            TokenKind::LeftParen => {
                self.advance();
                let types =
                    self.parse_delimited_by(TokenKind::RightParen, |this| this.parse_type())?;
                let span = start_span.combined(self.current_token.span);
                let _ = self.expect(TokenKind::RightParen, "Expected ')' at end of tuple type.");
                (
                    match types {
                        ElementsParsed::Single(element) => TypeKind::Grouped(Box::new(element)),
                        ElementsParsed::Multiple(elements, _) => TypeKind::Tuple(elements),
                    },
                    span,
                )
            }
            TokenKind::Ref => {
                self.advance();
                let mutable = if let Some(token) = self.match_current(TokenKind::Mut) {
                    Mutable::Yes(token.span)
                } else {
                    Mutable::No
                };
                let ty = self.parse_type()?;
                let span = start_span.combined(ty.span);
                (TypeKind::Ref(mutable, Box::new(ty)), span)
            }
            TokenKind::LeftBracket => {
                self.advance();
                let ty = self.parse_type()?;
                let end_span = self.current_token.span;
                let _ = self.expect(
                    TokenKind::RightBracket,
                    "Expected ']' at end of array type.",
                );
                (TypeKind::Array(Box::new(ty)), start_span.combined(end_span))
            }
            TokenKind::Pipe => {
                let variant = self.parse_variant_def()?;
                let span = variant.span;
                (TypeKind::Variant(Box::new(variant)), span)
            }
            TokenKind::LeftBrace => {
                let struct_def = self.parse_struct_def()?;
                let span = struct_def.span;
                (TypeKind::Struct(Box::new(struct_def)), span)
            }
            TokenKind::Ident(_) => {
                let name = self.parse_qual_name()?;
                let name_span = name.span;
                let generic_args = self.parse_optional_generic_args()?;
                let span = if let Some(args) = generic_args.as_ref(){
                    name_span.combined(args.span)
                } else {
                    name_span
                };
                (TypeKind::Named(Box::new(name),generic_args), span)
            }
            TokenKind::Fun => {
                let start = self.current_token.span;
                self.advance();
                let _ = self.expect(TokenKind::LeftParen, "Expected '('.");
                let params = self
                    .parse_delimited_by(TokenKind::RightParen, |this| this.parse_type())?
                    .into();
                let paren_span = self.current_token.span;
                let _ = self.expect(TokenKind::RightParen, "Expected ')'.");
                let (end_span, return_ty) = if self.matches_current(TokenKind::Arrow) {
                    let ty = self.parse_type()?;
                    (ty.span, Some(Box::new(ty)))
                } else {
                    (paren_span, None)
                };

                (TypeKind::Fun(params, return_ty), start.combined(end_span))
            }
            _ => {
                self.error_at_current("Invalid type.");
                return Err(ParseError);
            }
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
        let ty = if self.matches_current(TokenKind::Colon) {
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
            if !self.matches_current(TokenKind::Coma) {
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
    fn parse_fun_def(&mut self) -> ParseResult<FunctionDef> {
        let start = self.current_token.span;
        self.advance();

        let function_name = self.expect_ident("Expected a function name")?;
        let generic_params = self.parse_optional_generic_params()?;
        let _ = self.expect(TokenKind::LeftParen, "Expected '(' after 'function' name.");
        let params = self
            .parse_delimited_by(TokenKind::RightParen, Self::parse_fun_param)?
            .into();
        let _ = self.expect(
            TokenKind::RightParen,
            "Expected ')' after 'function' arguments.",
        );
        let return_type = self
            .matches_current(TokenKind::Arrow)
            .then(|| self.parse_type())
            .transpose()?;

        let _ = self.expect(TokenKind::Equals, "Expected '=' before function body.");
        let body = self.parse_expr(0)?;
        let span = start.combined(body.span);
        Ok(FunctionDef {
            id: self.new_id(),
            span,
            name: function_name,
            params,
            generics: generic_params,
            body: Box::new(body),
            return_type,
        })
    }
    fn parse_type_def(&mut self) -> ParseResult<TypeDef> {
        let start_span = self.current_token.span;
        self.advance();
        let name = self.expect_ident("Expect a valid type name.")?;
        let generic_params = self.parse_optional_generic_params()?;
        let (end_span, kind) = if self.matches_current(TokenKind::Equals) {
            match self.current_token.kind {
                TokenKind::LeftBrace => {
                    let struct_def = self.parse_struct_def()?;
                    (struct_def.span, TypeDefKind::Struct(Box::new(struct_def)))
                }
                TokenKind::Pipe => {
                    let variant_def = self.parse_variant_def()?;
                    (
                        variant_def.span,
                        TypeDefKind::Variant(Box::new(variant_def)),
                    )
                }
                _ => {
                    self.error_at_current("Expected valid type definition");
                    return Err(ParseError);
                }
            }
        } else {
            let _ = self.expect_at(
                TokenKind::Semicolon,
                "Expected a '=' or ';' after type name.",
                name.span,
            );
            (
                name.span,
                TypeDefKind::Variant(Box::new(Variant {
                    id: self.new_id(),
                    span: name.span,
                    cases: Vec::new(),
                })),
            )
        };
        let end_span = if let Some(prev_token) = self.match_current(TokenKind::Semicolon) {
            prev_token.span
        } else {
            end_span
        };
        let span = start_span.combined(end_span);
        Ok(TypeDef {
            id: self.new_id(),
            span,
            name,
            generics: generic_params,
            kind,
        })
    }
    fn parse_import_name(&mut self) -> ParseResult<QualifiedName> {
        let path = self.parse_qual_name()?;
        let _ = self.expect(TokenKind::Semicolon, "Expected ';' at end of 'import'.");
        Ok(path)
    }
    fn parse_optional_generic_params(&mut self) -> ParseResult<Option<GenericParams>> {
        self.check(TokenKind::LeftBracket)
            .then(|| self.parse_generic_params())
            .transpose()
    }
    fn parse_generic_params(&mut self) -> ParseResult<GenericParams> {
        let start_span = self.current_token.span;
        self.advance();
        let params = Vec::from(self.parse_delimited_by(TokenKind::RightBracket, |this| {
            let name = this.expect_ident("Expected a type param.")?;
            Ok(GenericParam {
                id: this.new_id(),
                name,
            })
        })?);
        let end_span = self.current_token.span;
        let _ = self.expect(
            TokenKind::RightBracket,
            "Expected a ']' at the end of generic parameters.",
        );
        Ok(GenericParams {
            id: self.new_id(),
            span: start_span.combined(end_span),
            params,
        })
    }
    fn try_parse_item(&mut self) -> Option<ParseResult<Item>> {
        Some(Ok(match self.current_token.kind {
            TokenKind::Fun => {
                let Ok(function_def) = self.parse_fun_def() else {
                    return Some(Err(ParseError));
                };
                Item {
                    id: self.new_id(),
                    span: function_def.span,
                    kind: ItemKind::Function(function_def),
                }
            }
            TokenKind::Type => {
                let Ok(type_def) = self.parse_type_def() else {
                    return Some(Err(ParseError));
                };
                Item {
                    id: self.new_id(),
                    span: type_def.span,
                    kind: ItemKind::Type(type_def),
                }
            }
            TokenKind::Import => {
                let span = self.current_token.span;
                self.advance();
                let name = self.parse_import_name().ok()?;
                Item {
                    id: self.new_id(),
                    span: span.combined(name.span),
                    kind: ItemKind::Import(name),
                }
            }
            _ => return None,
        }))
    }
    fn parse_stmt(&mut self, in_block: Option<BlockKind>) -> ParseResult<Stmt> {
        if let Some(item) = self.try_parse_item() {
            item.map(|item| Stmt {
                id: self.new_id(),
                span: item.span,
                kind: StmtKind::Item(Box::new(item)),
            })
        } else {
            match self.current_token.kind {
                TokenKind::Let => self.parse_let_stmt(),
                _ => self.parse_expr_stmt(in_block),
            }
        }
    }
    fn synchronize(&mut self, for_items_only: bool) {
        while !self.is_at_eof() {
            if matches!(
                self.current_token.kind,
                TokenKind::Semicolon | TokenKind::End
            ) {
                self.advance();
                break;
            }
            self.advance();
            if matches!(self.current_token.kind, TokenKind::Type | TokenKind::Fun)
                || (!for_items_only
                    && matches!(
                        self.current_token.kind,
                        TokenKind::While
                            | TokenKind::For
                            | TokenKind::Do
                            | TokenKind::If
                            | TokenKind::Let
                            | TokenKind::Match
                    ))
            {
                break;
            }
        }
    }
    pub fn parse(mut self) -> ParseResult<Module> {
        self.advance();
        let start_token = self.current_token;
        let items = std::iter::from_fn(|| {
            if self.is_at_eof() {
                return None;
            }
            self.panic_mode.set(false);
            let Some(item) = self.try_parse_item() else {
                self.error_at_current("Expected a valid item.");
                self.synchronize(true);
                return Some(Err(ParseError));
            };
            let Ok(item) = item else {
                self.synchronize(false);
                return Some(Err(ParseError));
            };
            Some(Ok(item))
        })
        .filter_map(|item| item.ok())
        .collect::<Vec<_>>();
        self.diag_reporter.emit();
        let end_span = items
            .last()
            .map(|item| item.span)
            .unwrap_or(start_token.span);
        Ok(Module {
            id: self.new_id(),
            span: start_token.span.combined(end_span),
            name: self.mod_name,
            items,
        })
    }
}

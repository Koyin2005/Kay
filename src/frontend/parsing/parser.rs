use std::cell::Cell;

use crate::{
    errors::{Diagnostic, DiagnosticReporter, IntoDiagnosticMessage}, frontend::{
        ast::{BinaryOp, BinaryOpKind, Block, Expr, ExprKind, IteratorExpr, IteratorExprKind, LiteralKind, NodeId, Stmt, StmtKind, UnaryOp, UnaryOpKind},
        parsing::token::{Literal, StringComplete, Token, TokenKind},
    }, indexvec::Idx, span::{self, symbol::{Ident, Symbol}, Span}, Lexer
};

pub struct Parser<'source> {
    diag_reporter: DiagnosticReporter<'source>,
    lexer: Lexer<'source>,
    current_token: Token,
    next_id: NodeId,
    panic_mode : Cell<bool>
}
pub struct ParseError;

type ParseResult<T> = Result<T,ParseError>;
impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>, diag_reporter: DiagnosticReporter<'source>) -> Self {
        Self {
            current_token: Token::empty(),
            lexer,
            diag_reporter,
            next_id: NodeId::new(0),
            panic_mode : Cell::new(false)
        }
    }
    fn new_id(&mut self) -> NodeId {
        let node = self.next_id;
        self.next_id = NodeId::new(self.next_id.into_index() + 1);
        node
    }

    fn error_at(&self, msg: impl IntoDiagnosticMessage, span: Span){
        if self.panic_mode.get(){
            return;
        }
        self.panic_mode.set(true);
        self.diag_reporter.add(Diagnostic::new(
            msg,
            span
        ));
    }
    fn error_at_current(&self, msg: impl IntoDiagnosticMessage){
        self.error_at(msg, self.current_token.span);
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
    fn expect(&mut self, kind: TokenKind,msg: impl IntoDiagnosticMessage) -> ParseResult<()> {
        if !self.match_current(kind){
            self.error_at_current(msg);
            return Err(ParseError);
        }
        Ok(())
    }
    fn advance(&mut self) {
        self.current_token = loop {
            let next_token = self.lexer.next_token();
            if let TokenKind::Unknown(c) = next_token.kind {
                self.error_at(format!("Unknown '{}' character.", c),next_token.span);
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
            TokenKind::Equals => (1,2), 
            TokenKind::EqualsEquals | TokenKind::BangEquals => (3,4),
            TokenKind::LesserThan | TokenKind::GreaterThan | 
            TokenKind::LesserEquals | TokenKind::GreaterEquals => (5,6),
            TokenKind::And | TokenKind::Or => (7, 8),
            TokenKind::Plus | TokenKind::Minus => (9, 10),
            TokenKind::Star | TokenKind::Slash => (11, 12),
            _ => return None,
        })
    }
    fn parse_literal(&mut self, literal: Literal) -> ParseResult<Expr>{
        let span = self.current_token.span;
        let kind = match literal {
            Literal::Int(value) => LiteralKind::Int(match value.as_str().parse() {
                Ok(value) => value,
                Err(err) => {
                    self.error_at_current(err.to_string());
                    return Err(ParseError);
                },
            }),
            Literal::True => LiteralKind::Bool(true),
            Literal::False => LiteralKind::Bool(false),
            Literal::String(string, complete) => {
                if let StringComplete::No = complete{
                    self.error_at_current("Unterminated string.");
                }
                LiteralKind::String(string)
            },
        };
        self.advance();
        Ok(Expr {
            id:self.new_id(),
            kind:ExprKind::Literal(kind),
            span,
        })
    }
    fn parse_grouped_expr(&mut self) -> ParseResult<Expr> {
        let start = self.current_token.span;
        self.advance();
        let mut exprs = Vec::new();
        let mut had_coma = false;
        while !self.check(TokenKind::RightParen) {
            exprs.push(self.parse_expr(0)?);
            if !self.match_current(TokenKind::Coma) {
                break;
            }
            had_coma = true;
        }
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightParen,"Expected ')' to enclose '('.");
        let span = start.combined(end);
        Ok(Expr {
            id:self.new_id(),
            span,
            kind: if exprs.is_empty() || had_coma {
                ExprKind::Tuple(exprs)
            } else {
                let first_expr = exprs.remove(0);
                ExprKind::Grouped(Box::new(first_expr))
            },
        })
    }
    fn parse_block(&mut self) -> ParseResult<Block>{
        self.expect(TokenKind::LeftBrace, "Expected '{' at the start of the block")?;
        self.parse_block_rest()
    }
    fn parse_block_expr(&mut self) -> ParseResult<Expr>{
        let block = self.parse_block()?;
        Ok(Expr {
            id:self.new_id(),
            span: block.span,
            kind: ExprKind::Block(block),
        })
    }
    fn parse_if_expr(&mut self) -> ParseResult<Expr>{
        let start = self.current_token.span;
        self.advance();
        let condition = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::Then,"Expected 'then' after if condition.");
        let then_body = self.parse_block()?;
        let mut end = then_body.span;
        let else_branch = self.match_current(TokenKind::Else).then(|| {
            let else_branch = if self.check(TokenKind::If) {
                self.parse_if_expr()
            } else {
                self.parse_block_expr()
            }?;
            end = else_branch.span;
            Ok(else_branch)
        }).transpose()?;
        Ok(Expr {
            id:self.new_id(),
            span: start.combined(end),
            kind: ExprKind::If(
                Box::new(condition),
                Box::new(then_body),
                else_branch.map(Box::new),
            ),
        })
    }
    fn parse_for_expr(&mut self) -> ParseResult<Expr>{
        let start = self.current_token.span;
        self.advance();
        let pattern = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::In, "Expected 'in' after 'for' pattern.");
        let expr = self.parse_expr(0)?;
        let iterator = if self.match_current(TokenKind::DotDot){
            let end = self.parse_expr(0)?;
            let span = start.combined(end.span);
            IteratorExpr{ span, kind: IteratorExprKind::Range(Box::new(expr), Box::new(end)) }
        }
        else{
            IteratorExpr{ span: expr.span, kind: IteratorExprKind::Expr(Box::new(expr))}
        };
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'for' iterator.");
        let block = self.parse_block()?;
        let span = start.combined(block.span);
        Ok(Expr { id: self.new_id(), kind: ExprKind::For(Box::new(pattern), Box::new(iterator),Box::new(block)), span })
    }
    fn parse_while_expr(&mut self) -> ParseResult<Expr>{
        let start = self.current_token.span;
        self.advance();
        let condition = self.parse_expr(0)?;
        let _ = self.expect(TokenKind::Do, "Expected 'do' after 'while' condition.");
        let block = self.parse_block()?;
        let end = block.span;
        Ok(Expr { id: self.new_id(), kind: ExprKind::While(Box::new(condition), Box::new(block)), span: start.combined(end)})
    }
    fn parse_exprs(&mut self, end: TokenKind) -> ParseResult<Vec<Expr>>{
        let mut exprs = Vec::new();
        while !self.is_at_eof() && !self.check(end) {
            exprs.push(self.parse_expr(0)?);
            if !self.match_current(TokenKind::Coma){
                break;
            }
        }
        Ok(exprs)
    }
    fn parse_array_expr(&mut self) -> ParseResult<Expr>{
        let start = self.current_token.span;
        self.advance();
        let elements = self.parse_exprs(TokenKind::RightBracket)?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightBracket, "Expected ']'.");
        Ok(Expr { id: self.new_id(), kind: ExprKind::Array(elements), span: start.combined(end)})
    }
    fn unary_op(&mut self) -> Option<UnaryOp>{
        let start_token = self.current_token;
        self.match_current(TokenKind::Minus).then_some( UnaryOp{node: UnaryOpKind::Negate,span:start_token.span})
    }
    fn is_expr_start(&self) -> bool{
        match self.current_token.kind{
            TokenKind::Ident(..) | TokenKind::Literal(..) | TokenKind::LeftBrace | TokenKind::LeftParen | TokenKind::Return |
             TokenKind::Break | TokenKind::Minus | TokenKind::If | TokenKind::While | TokenKind::For => true,
            _ => false
        }
    }
    fn parse_optional_expr(&mut self) -> Option<ParseResult<Expr>>{
        if self.is_expr_start(){
            Some(self.parse_expr(0))
        }
        else{
            None
        }
    }
    fn parse_expr_prefix(&mut self) -> ParseResult<Expr>{
        match self.current_token.kind {
            TokenKind::Literal(literal) => {
                self.parse_literal(literal)
            }
            TokenKind::LeftParen => {
                self.parse_grouped_expr()
            }
            TokenKind::LeftBrace => {
                self.parse_block_expr()
            }
            TokenKind::If => {
                self.parse_if_expr()
            },
            TokenKind::While => {
                self.parse_while_expr()
            },
            TokenKind::LeftBracket => {
                self.parse_array_expr()
            },
            TokenKind::For => {
                self.parse_for_expr()
            },
            TokenKind::Ident(name) => {
                let span = self.current_token.span;
                self.advance();
                Ok(Expr{ id : self.new_id(), kind: ExprKind::Ident(name), span})
            },
            TokenKind::Break => {
                let start = self.current_token.span;
                self.advance();
                let expr = self.parse_optional_expr().transpose()?;
                let span = if let Some(expr) = expr.as_ref() { expr.span.combined(start)} else { start};
                Ok(Expr { id: self.new_id(), kind: ExprKind::Break(expr.map(Box::new)), span })
            },
            _ => {
                let Some(op) = self.unary_op() else {
                    self.error_at_current("Expected an expression.");
                    return Err(ParseError)
                };
                let expr = self.parse_expr(13)?;
                let span = expr.span.combined(op.span);
                Ok(Expr{ id : self.new_id(), kind : ExprKind::Unary(op,Box::new(expr)), span })

            }
        }
    }
    fn parse_rest_infix(&mut self,mut lhs: Expr, min_bp: u32) -> ParseResult<Expr>{
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
                        kind: ExprKind::Assign(
                            Box::new(lhs),
                            Box::new(rhs),
                            op_span
                        ),
                        span,
                    };
                    continue;
                }
                kind => unreachable!("Can only have infix ops here, but got {:?}",kind),
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
    fn parse_call(&mut self, callee: Expr) -> ParseResult<Expr>{
        let start = callee.span;
        self.advance();
        let args = self.parse_exprs(TokenKind::RightParen)?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::RightParen, "Expected ')' after arguments.");
        Ok(Expr { id: self.new_id(), kind: ExprKind::Call(Box::new(callee), args), span: start.combined(end) })
    }
    fn parse_expr_postfix(&mut self, mut lhs: Expr) -> ParseResult<Expr>{
        loop{   
            lhs = match self.current_token.kind {
                TokenKind::LeftParen => {
                    self.parse_call(lhs)?
                },
                TokenKind::Dot => {
                    self.advance();
                    let field_name = if let TokenKind::Literal(Literal::Int(field)) = self.current_token.kind{
                        let span = self.current_token.span;
                        self.advance();
                        Ident{ symbol: field, span }
                    }
                    else{
                        self.expect_ident("Expect a valid field name")?
                    };
                    Expr{id:self.new_id(),span:lhs.span.combined(field_name.span),kind:ExprKind::Field(Box::new(lhs),field_name)}
                },
                _ => break Ok(lhs)
            };
        }
    }
    fn parse_expr(&mut self, min_bp: u32) -> ParseResult<Expr> {
        let lhs = self.parse_expr_prefix()?;
        let lhs = self.parse_expr_postfix(lhs)?;
        self.parse_rest_infix(lhs, min_bp)
    }
    fn expr_needs_semi(&self, expr: &ExprKind) -> bool{
        match expr{
            ExprKind::If(..) | ExprKind::Block(..) | ExprKind::While(..) | ExprKind::For(..) => false,
            _ => true
        }
    }
    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt>{
        let expr = self.parse_expr(0)?;
        let end_token = self.current_token;
        
        let (span, kind) = if self.match_current(TokenKind::Semicolon) {
            (
                expr.span.combined(end_token.span),
                StmtKind::ExprWithSemi(Box::new(expr)),
            )
        } else {
            if self.expr_needs_semi(&expr.kind) && !self.check(TokenKind::RightBrace){
                self.error_at("Expected a ';'.", expr.span);
            }
            (expr.span, StmtKind::Expr(Box::new(expr)))
        };
        let id = self.new_id();
        Ok(Stmt { id, kind, span })
    }
    fn parse_let_stmt(&mut self) -> ParseResult<Stmt>{
        let start = self.current_token.span;
        self.advance();
        let name = self.expect_ident("Expected valid variable name")?;
        let name_expr = Expr{id:self.new_id(), kind: ExprKind::Ident(name.symbol), span:name.span};
        let _ = self.expect(TokenKind::Equals, "Expected '='.");
        let expr = self.parse_expr(0)?;
        let end = self.current_token.span;
        let _ = self.expect(TokenKind::Semicolon, "Expected ';' at end of 'let' statement.");
        Ok(Stmt { id: self.new_id(), kind: StmtKind::Let(Box::new(name_expr), Box::new(expr)), span: start.combined(end) })
    }
    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.current_token.kind{
            TokenKind::Let => {
                self.parse_let_stmt()
            },
            _ => self.parse_expr_stmt()
        }
    }
    fn parse_block_rest(&mut self) -> ParseResult<Block> {
        let span = self.current_token.span;
        let id = self.new_id();
        let mut stmts = Vec::new();
        while !self.is_at_eof() && !self.check(TokenKind::RightBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(TokenKind::RightBrace,"Expected '}'.")?;
        let span = span.combined(self.current_token.span);
        Ok(Block { id, stmts, span })
    }
    pub fn parse(mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = Vec::new();
        self.advance();
        while !self.is_at_eof(){
            let Ok(stmt) = self.parse_stmt() else{
                while !self.is_at_eof() {
                    if matches!(self.current_token.kind,TokenKind::Semicolon|TokenKind::RightBrace){
                        self.advance();
                        break;
                    }
                    self.advance();
                    if matches!(self.current_token.kind,TokenKind::LeftBrace|TokenKind::For|TokenKind::While){
                        break;
                    }
                }
                self.panic_mode.set(false);
                continue;
            };
            stmts.push(stmt);
        }
        self.diag_reporter.emit();
        Ok(stmts)
    }
}

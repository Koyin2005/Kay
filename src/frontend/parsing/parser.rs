use crate::{
    Lexer,
    frontend::{
        ast::{BinaryOp, BinaryOpKind, Block, Expr, ExprKind, LiteralKind, NodeId, Stmt, StmtKind},
        parsing::token::{Literal, StringComplete, Token, TokenKind},
    },
    indexvec::Idx,
};

pub struct Parser<'source> {
    lexer: Lexer<'source>,
    prev_token: Token,
    current_token: Token,
    next_id: NodeId,
}

impl<'source> Parser<'source> {
    pub fn new(mut lexer: Lexer<'source>) -> Self {
        let first_token = lexer.next_token();
        Self {
            prev_token: first_token,
            current_token: first_token,
            lexer,
            next_id: NodeId::new(0),
        }
    }
    fn new_id(&mut self) -> NodeId {
        let node = self.next_id;
        self.next_id = NodeId::new(self.next_id.into_index() + 1);
        node
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        self.match_current(kind)
    }
    fn advance(&mut self) {
        self.prev_token = self.current_token;

        self.current_token = loop {
            let next_token = self.lexer.next_token();
            if let TokenKind::Unknown(_) = next_token.kind {
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
            TokenKind::EqualsEquals | TokenKind::BangEquals => (1, 2),
            TokenKind::And | TokenKind::Or => (3, 4),
            TokenKind::Plus | TokenKind::Minus => (5, 6),
            TokenKind::Star | TokenKind::Slash => (7, 8),
            _ => return None,
        })
    }
    fn parse_expr(&mut self, min_bp: u32) -> Expr {
        self.advance();
        let id = self.new_id();
        let mut lhs = match self.prev_token.kind {
            TokenKind::Literal(literal) => {
                let kind = match literal {
                    Literal::Int(value) => LiteralKind::Int(match value.as_str().parse() {
                        Ok(value) => value,
                        Err(_) => todo!("Handle too large int literals"),
                    }),
                    Literal::True => LiteralKind::Bool(true),
                    Literal::False => LiteralKind::Bool(false),
                    Literal::String(string, StringComplete::Yes) => LiteralKind::String(string),
                    Literal::String(_, StringComplete::No) => {
                        todo!("Handle incomplete strings")
                    }
                };
                Expr {
                    id,
                    kind: ExprKind::Literal(kind),
                    span: self.current_token.span,
                }
            }
            TokenKind::LeftParen => {
                let start = self.prev_token.span;
                let mut exprs = Vec::new();
                let mut had_coma = false;
                while !self.check(TokenKind::RightParen) {
                    exprs.push(self.parse_expr(0));
                    if !self.match_current(TokenKind::Coma) {
                        break;
                    }
                    had_coma = true;
                }
                self.expect(TokenKind::RightParen);
                let end = self.prev_token.span;
                let span = start.combined(end);
                Expr {
                    id,
                    span,
                    kind: if exprs.is_empty() || had_coma {
                        ExprKind::Tuple(exprs)
                    } else {
                        let first_expr = exprs.remove(0);
                        ExprKind::Grouped(Box::new(first_expr))
                    },
                }
            }
            TokenKind::LeftBrace => {
                let block = self.parse_block();
                Expr {
                    id,
                    span: block.span,
                    kind: ExprKind::Block(block),
                }
            }
            TokenKind::If => {
                let start = self.prev_token.span;
                let condition = self.parse_expr(0);
                self.expect(TokenKind::Then);

                let then_body = self.parse_block_expr();
                let mut end = then_body.span;
                let else_branch = self.match_current(TokenKind::Else).then(|| {
                    let else_branch = if self.check(TokenKind::If) {
                        self.parse_expr(0)
                    } else {
                        self.parse_block_expr()
                    };
                    end = else_branch.span;
                    else_branch
                });
                Expr {
                    id,
                    span: start.combined(end),
                    kind: ExprKind::If(
                        Box::new(condition),
                        Box::new(then_body),
                        else_branch.map(Box::new),
                    ),
                }
            }
            kind => todo!("Other exprs {:?} at {:?}", kind, self.prev_token.span),
        };
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
                _ => unreachable!("Can only have infix ops here"),
            };
            self.advance();
            let rhs = self.parse_expr(r_bp);
            let id = self.new_id();
            let span = lhs.span.combined(rhs.span);
            lhs = Expr {
                id,
                kind: ExprKind::Binary(
                    BinaryOp {
                        span: op_span,
                        value: op_kind,
                    },
                    Box::new(lhs),
                    Box::new(rhs),
                ),
                span,
            };
        }
        lhs
    }
    fn parse_stmt(&mut self) -> Stmt {
        let id = self.new_id();
        let expr = self.parse_expr(0);
        let (span, kind) = if self.match_current(TokenKind::Semicolon) {
            (
                expr.span.combined(self.prev_token.span),
                StmtKind::ExprWithSemi(Box::new(expr)),
            )
        } else {
            (expr.span, StmtKind::Expr(Box::new(expr)))
        };
        Stmt { id, kind, span }
    }
    fn parse_block_expr(&mut self) -> Expr {
        let id = self.new_id();
        let block = self.parse_block();
        Expr {
            id,
            span: block.span,
            kind: ExprKind::Block(block),
        }
    }
    fn parse_block(&mut self) -> Block {
        self.expect(TokenKind::LeftBrace);
        let span = self.prev_token.span;
        let id = self.new_id();
        let mut stmts = Vec::new();
        while !self.is_at_eof() && !self.check(TokenKind::RightBrace) {
            stmts.push(self.parse_stmt());
        }
        self.expect(TokenKind::RightBrace);
        let span = span.combined(self.prev_token.span);
        Block { id, stmts, span }
    }
    pub fn parse(mut self) -> Block {
        self.parse_block()
    }
}

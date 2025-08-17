use std::cell::Cell;

use fxhash::FxHashMap;
use indexmap::IndexMap;

use crate::{
    errors::DiagnosticReporter,
    frontend::{
        ast::{self, ItemKind, NodeId},
        hir::{self, Body, DefId, Hir, HirId},
        resolution::resolve::ResolveResults,
    },
    span::{Span, symbol::Symbol},
};

pub struct AstLower<'diag> {
    resolution_results: ResolveResults,
    node_ids_to_hir_ids: FxHashMap<NodeId, HirId>,
    bodies: IndexMap<HirId, hir::Body>,
    items: IndexMap<DefId, hir::Item>,
    current_loop_label: Option<HirId>,
    diag: &'diag DiagnosticReporter,
    next_id: Cell<u32>,
}
impl<'diag> AstLower<'diag> {
    pub fn new(resolution_results: ResolveResults, diag: &'diag DiagnosticReporter) -> Self {
        Self {
            diag,
            resolution_results,
            bodies: IndexMap::new(),
            next_id: Cell::new(0),
            items: IndexMap::new(),
            node_ids_to_hir_ids: FxHashMap::default(),
            current_loop_label: None,
        }
    }
    fn map_res(&self, id: NodeId) -> Option<hir::Resolution> {
        self.resolution_results
            .get_resolution(id)
            .map(|res| match res {
                hir::Resolution::Variable(id) => hir::Resolution::Variable(
                    self.node_ids_to_hir_ids
                        .get(&id)
                        .copied()
                        .expect("There should always be a hir id for a variable"),
                ),
                hir::Resolution::Builtin(builtin) => hir::Resolution::Builtin(builtin),
                hir::Resolution::Def(id, kind) => hir::Resolution::Def(id, kind),
                hir::Resolution::Err => hir::Resolution::Err,
            })
    }
    fn lower_id(&mut self, id: NodeId) -> HirId {
        let hir_id = self.next_hir_id();
        self.node_ids_to_hir_ids.insert(id, hir_id);
        hir_id
    }
    fn next_hir_id(&self) -> HirId {
        let id = HirId::new(self.next_id.get() as usize);
        self.next_id
            .update(|id| id.checked_add(1).expect("Requires too many hir-ids."));
        id
    }
    fn expect_def_id(&self, id: NodeId) -> DefId {
        self.resolution_results.expect_def_id(id)
    }
    fn lower_function_sig<'a>(
        &self,
        params: impl Iterator<Item = &'a ast::Type>,
        return_ty: Option<&ast::Type>,
    ) -> hir::FunctionSig {
        hir::FunctionSig {
            inputs: params.map(|ty| self.lower_ty(ty)).collect(),
            output: return_ty.map(|ty| self.lower_ty(ty)),
        }
    }
    fn lower_path(&self, name: &ast::QualifiedName) -> hir::Path {
        hir::Path {
            id: self.next_hir_id(),
            res: self.map_res(name.id).unwrap_or(hir::Resolution::Err),
        }
    }
    fn lower_ty(&self, ty: &ast::Type) -> hir::Type {
        hir::Type {
            id: self.next_hir_id(),
            span: ty.span,
            kind: match &ty.kind {
                ast::TypeKind::Grouped(grouped_ty) => {
                    return {
                        let mut lowered_ty = self.lower_ty(grouped_ty);
                        lowered_ty.span = grouped_ty.span;
                        lowered_ty
                    };
                }
                ast::TypeKind::Never => hir::TypeKind::Primitive(hir::PrimitiveType::Never),
                ast::TypeKind::Bool => hir::TypeKind::Primitive(hir::PrimitiveType::Bool),
                ast::TypeKind::Uint => {
                    hir::TypeKind::Primitive(hir::PrimitiveType::Int(hir::IntType::Unsigned))
                }
                ast::TypeKind::Int => {
                    hir::TypeKind::Primitive(hir::PrimitiveType::Int(hir::IntType::Signed))
                }
                ast::TypeKind::String => hir::TypeKind::Primitive(hir::PrimitiveType::String),
                ast::TypeKind::Ref(mutable, ty) => {
                    hir::TypeKind::Ref(*mutable, Box::new(self.lower_ty(ty)))
                }
                ast::TypeKind::Array(ty) => hir::TypeKind::Array(Box::new(self.lower_ty(ty))),
                ast::TypeKind::Tuple(elements) => {
                    hir::TypeKind::Tuple(elements.iter().map(|ty| self.lower_ty(ty)).collect())
                }
                ast::TypeKind::Struct(struct_) => hir::TypeKind::Struct(
                    struct_
                        .fields
                        .iter()
                        .map(|field| hir::StructTypeField {
                            name: field.name,
                            ty: self.lower_ty(&field.ty),
                        })
                        .collect(),
                ),
                ast::TypeKind::Variant(variant) => hir::TypeKind::Variant(
                    variant
                        .cases
                        .iter()
                        .map(|case| hir::VariantTypeCase {
                            name: case.name,
                            fields: case.fields.as_ref().map(|fields| {
                                fields
                                    .iter()
                                    .map(|field| self.lower_ty(&field.ty))
                                    .collect()
                            }),
                        })
                        .collect(),
                ),
                ast::TypeKind::Named(name) => hir::TypeKind::Path(self.lower_path(name)),
                ast::TypeKind::Fun(params, return_ty) => hir::TypeKind::Fun(
                    params.iter().map(|ty| self.lower_ty(ty)).collect(),
                    return_ty.as_ref().map(|ty| Box::new(self.lower_ty(ty))),
                ),
            },
        }
    }
    fn lower_pattern(&mut self, pattern: &ast::Pattern) -> hir::Pattern {
        macro_rules! lower_pattern {
            ($kind : expr) => {
                hir::Pattern {
                    id: self.next_hir_id(),
                    span: pattern.span,
                    kind: $kind,
                }
            };
        }
        match pattern.kind {
            ast::PatternKind::Case(ref name, ref elements) => {
                lower_pattern!(hir::PatternKind::Case(
                    self.map_res(name.id).unwrap_or(hir::Resolution::Err),
                    elements.iter().map(|pat| self.lower_pattern(pat)).collect()
                ))
            }
            ast::PatternKind::Grouped(ref pat) => {
                let mut pat = self.lower_pattern(pat);
                pat.span = pattern.span;
                pat
            }
            ast::PatternKind::Deref(ref pat) => {
                let pat = self.lower_pattern(pat);
                lower_pattern!(hir::PatternKind::Deref(Box::new(pat)))
            }
            ast::PatternKind::Literal(literal) => {
                lower_pattern!(hir::PatternKind::Literal(literal))
            }
            ast::PatternKind::Wildcard => lower_pattern!(hir::PatternKind::Wildcard),
            ast::PatternKind::Ident(name, mutable, by_ref) => {
                let pat_id = self.lower_id(pattern.id);
                let Some(hir::Resolution::Variable(hir_id)) = self.map_res(pattern.id) else {
                    unreachable!("There should be a binding for patterns always")
                };
                hir::Pattern {
                    id: pat_id,
                    span: pattern.span,
                    kind: hir::PatternKind::Binding(hir_id, name, mutable, by_ref),
                }
            }
            ast::PatternKind::Tuple(ref elements) => lower_pattern!(hir::PatternKind::Tuple(
                elements
                    .iter()
                    .map(|element| self.lower_pattern(element))
                    .collect(),
            )),
        }
    }
    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> hir::Stmt {
        hir::Stmt {
            id: self.next_hir_id(),
            span: stmt.span,
            kind: match stmt.kind {
                ast::StmtKind::Expr(ref expr) => hir::StmtKind::Expr(self.lower_expr(expr)),
                ast::StmtKind::ExprWithSemi(ref expr) => {
                    hir::StmtKind::ExprWithSemi(self.lower_expr(expr))
                }
                ast::StmtKind::Item(ref item) => {
                    let (id, item) = self.lower_item(item);
                    self.items.insert(id, item);
                    hir::StmtKind::Item(id)
                }
                ast::StmtKind::Let(ref pat, ref ty, ref expr) => hir::StmtKind::Let(
                    self.lower_pattern(pat),
                    ty.as_ref().map(|ty| self.lower_ty(ty)),
                    Box::new(self.lower_expr(expr)),
                ),
            },
        }
    }
    fn lower_block_expr(&mut self, block: &ast::Block) -> hir::Expr {
        hir::Expr {
            id: self.next_hir_id(),
            span: block.span,
            kind: hir::ExprKind::Block(Box::new(self.lower_block(block))),
        }
    }
    fn lower_literal(&mut self, span: Span, literal: ast::LiteralKind) -> ast::LiteralKind {
        match literal {
            ast::LiteralKind::Bool(_) | ast::LiteralKind::Int(_) => literal,
            ast::LiteralKind::String(string) => {
                let content = string.as_str();
                if content.contains('\\') {
                    let mut unescaped = String::with_capacity(content.len());
                    let mut chars = content.chars().peekable();
                    while let Some(curr_char) = chars.next() {
                        let next_char = match curr_char {
                            '\\' => match chars.peek().copied() {
                                Some(c) => {
                                    chars.next();
                                    match c {
                                        'n' => '\n',
                                        'r' => '\r',
                                        '0' => '\0',
                                        't' => '\t',
                                        '\\' => '\\',
                                        '"' => '"',
                                        m => {
                                            self.diag.emit_diag(
                                                format!("Invalid escape character '{m}'."),
                                                span,
                                            );
                                            unescaped.push('\\');
                                            m
                                        }
                                    }
                                }
                                None => '\\',
                            },
                            c => c,
                        };
                        unescaped.push(next_char);
                    }
                    ast::LiteralKind::String(Symbol::intern(&unescaped))
                } else {
                    literal
                }
            }
        }
    }
    fn lower_block(&mut self, block: &ast::Block) -> hir::Block {
        let mut stmts: Vec<_> = block
            .stmts
            .iter()
            .map(|stmt| self.lower_stmt(stmt))
            .collect();
        let expr = stmts
            .pop_if(|stmt| matches!(stmt.kind, hir::StmtKind::Expr(_)))
            .and_then(|stmt| match stmt.kind {
                hir::StmtKind::Expr(expr) => Some(expr),
                _ => None,
            });
        hir::Block {
            id: self.next_hir_id(),
            span: block.span,
            stmts,
            result: expr,
        }
    }
    fn lower_expr(&mut self, expr: &ast::Expr) -> hir::Expr {
        macro_rules! lower_expr {
            ($kind:expr) => {
                hir::Expr {
                    id: self.next_hir_id(),
                    span: expr.span,
                    kind: $kind,
                }
            };
        }
        match &expr.kind {
            ast::ExprKind::Grouped(expr) => self.lower_expr(expr),
            ast::ExprKind::Tuple(elements) => lower_expr! {
                hir::ExprKind::Tuple(elements.iter().map(|element| self.lower_expr(element)).collect())
            },
            ast::ExprKind::Block(block) => {
                lower_expr!(hir::ExprKind::Block(Box::new(self.lower_block(block))))
            }
            ast::ExprKind::If(condition, then_branch, else_branch) => {
                let condition = self.lower_expr(condition);
                let then_branch = self.lower_block_expr(then_branch);
                let else_branch = else_branch
                    .as_ref()
                    .map(|expr| Box::new(self.lower_expr(expr)));
                lower_expr!(hir::ExprKind::If(
                    Box::new(condition),
                    Box::new(then_branch),
                    else_branch
                ))
            }
            ast::ExprKind::While(condition, body) => self.lower_while_expr(expr, condition, body),
            ast::ExprKind::For(pat, iterator, body) => {
                self.lower_for_expr(expr, pat, iterator, body)
            }
            ast::ExprKind::Break(expr) => {
                let id = self.current_loop_label.ok_or(hir::OutsideLoop);
                let expr = expr
                    .as_ref()
                    .map(|expr| self.lower_expr(expr))
                    .map(Box::new);
                lower_expr!(hir::ExprKind::Break(id, expr))
            }
            ast::ExprKind::Ident(_) => {
                lower_expr!(hir::ExprKind::Path(hir::Path {
                    id: self.next_hir_id(),
                    res: self.map_res(expr.id).unwrap_or(hir::Resolution::Err)
                }))
            }
            ast::ExprKind::Literal(literal) => {
                lower_expr!(hir::ExprKind::Literal(
                    self.lower_literal(expr.span, *literal)
                ))
            }
            ast::ExprKind::Array(elements) => lower_expr!(hir::ExprKind::Array(
                elements
                    .iter()
                    .map(|element| self.lower_expr(element))
                    .collect()
            )),
            ast::ExprKind::Call(callee, args) => lower_expr!(hir::ExprKind::Call(
                Box::new(self.lower_expr(callee)),
                args.iter().map(|arg| self.lower_expr(arg)).collect()
            )),
            ast::ExprKind::Match(scrutinee, arms) => {
                lower_expr!(hir::ExprKind::Match(
                    Box::new(self.lower_expr(scrutinee)),
                    arms.iter()
                        .map(|arm| {
                            hir::MatchArm {
                                id: self.next_hir_id(),
                                span: arm.span,
                                pat: self.lower_pattern(&arm.pat),
                                body: self.lower_expr(&arm.body),
                            }
                        })
                        .collect()
                ))
            }
            ast::ExprKind::Binary(op, left, right) => {
                lower_expr!(hir::ExprKind::Binary(
                    *op,
                    Box::new(self.lower_expr(left)),
                    Box::new(self.lower_expr(right))
                ))
            }
            ast::ExprKind::Unary(op, operand) => lower_expr!(hir::ExprKind::Unary(
                *op,
                Box::new(self.lower_expr(operand))
            )),
            ast::ExprKind::Return(operand) => lower_expr!(hir::ExprKind::Return(
                operand
                    .as_ref()
                    .map(|operand| Box::new(self.lower_expr(operand)))
            )),
            ast::ExprKind::Field(sub_expr, field) => lower_expr!(hir::ExprKind::Field(
                Box::new(self.lower_expr(sub_expr)),
                *field
            )),
            ast::ExprKind::Init(name, fields) => {
                lower_expr!(hir::ExprKind::Init(
                    name.as_ref().map(|name| self.lower_path(name)),
                    fields
                        .iter()
                        .map(|field| {
                            hir::ExprField {
                                id: self.next_hir_id(),
                                span: field.span,
                                name: field.name,
                                expr: self.lower_expr(&field.expr),
                            }
                        })
                        .collect()
                ))
            }
            ast::ExprKind::Deref(_, expr) => {
                lower_expr!(hir::ExprKind::Deref(Box::new(self.lower_expr(expr))))
            }
            ast::ExprKind::As(expr, ty) => {
                lower_expr!(hir::ExprKind::As(
                    Box::new(self.lower_expr(expr)),
                    self.lower_ty(ty)
                ))
            }
            ast::ExprKind::Path(path) => {
                let path_head = self.map_res(path.head.id).unwrap_or(hir::Resolution::Err);
                match path_head {
                    hir::Resolution::Variable(id) => {
                        let var = hir::Expr {
                            id: self.next_hir_id(),
                            span: path.head.span,
                            kind: hir::ExprKind::Path(hir::Path {
                                id: self.next_hir_id(),
                                res: hir::Resolution::Variable(id),
                            }),
                        };
                        path.tail.iter().fold(var, |curr, field| hir::Expr {
                            id: self.next_hir_id(),
                            span: field.span.combined(curr.span),
                            kind: hir::ExprKind::Field(Box::new(curr), field.name),
                        })
                    }
                    _ => hir::Expr {
                        id: self.next_hir_id(),
                        span: expr.span,
                        kind: hir::ExprKind::Path(hir::Path {
                            id: self.next_hir_id(),
                            res: self.map_res(path.id).unwrap_or(hir::Resolution::Err),
                        }),
                    },
                }
            }
            ast::ExprKind::Underscore => {
                self.diag
                    .emit_diag("Cannot use '_' in this position.", expr.span);
                hir::Expr {
                    id: self.next_hir_id(),
                    span: expr.span,
                    kind: hir::ExprKind::Err,
                }
            }
            ast::ExprKind::Assign(lhs, rhs, span) => self.lower_assign_expr(expr, lhs, rhs, *span),
        }
    }
    fn lower_assign_expr(
        &mut self,
        expr: &ast::Expr,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        span: Span,
    ) -> hir::Expr {
        if let ast::ExprKind::Underscore = lhs.kind {
            hir::Expr {
                id: self.next_hir_id(),
                span: expr.span,
                kind: hir::ExprKind::Block(Box::new(hir::Block {
                    id: self.next_hir_id(),
                    span: expr.span,
                    stmts: vec![hir::Stmt {
                        id: self.next_hir_id(),
                        span: expr.span,
                        kind: hir::StmtKind::Let(
                            hir::Pattern {
                                id: self.next_hir_id(),
                                span: lhs.span,
                                kind: hir::PatternKind::Wildcard,
                            },
                            None,
                            Box::new(self.lower_expr(rhs)),
                        ),
                    }],
                    result: None,
                })),
            }
        } else {
            hir::Expr {
                id: self.next_hir_id(),
                span: expr.span,
                kind: hir::ExprKind::Assign(
                    span,
                    Box::new(self.lower_expr(lhs)),
                    Box::new(self.lower_expr(rhs)),
                ),
            }
        }
    }
    fn lower_while_expr(
        &mut self,
        expr: &ast::Expr,
        condition: &ast::Expr,
        body: &ast::Block,
    ) -> hir::Expr {
        /*
        Essentially this is lowered from :
            while $condition
                $loop_body

        into:
            'anon_label : loop{
                if $condition
                    $loop_body
                else { break 'anon_label; }
            }
        */
        let loop_id = self.next_hir_id();
        let loop_block_id = self.next_hir_id();
        let if_stmt_id = self.next_hir_id();
        let if_expr_id = self.next_hir_id();
        let old_loop_label = self.current_loop_label.replace(loop_id);

        let condition = self.lower_expr(condition);
        let body = self.lower_block_expr(body);

        self.current_loop_label = old_loop_label;
        let span = expr.span.end();
        let break_expr = hir::Expr {
            id: self.next_hir_id(),
            span,
            kind: hir::ExprKind::Break(Ok(loop_id), None),
        };
        let else_block = hir::Expr {
            id: self.next_hir_id(),
            span,
            kind: hir::ExprKind::Block(Box::new(hir::Block {
                id: self.next_hir_id(),
                span,
                stmts: vec![hir::Stmt {
                    id: self.next_hir_id(),
                    span,
                    kind: hir::StmtKind::ExprWithSemi(break_expr),
                }],
                result: None,
            })),
        };
        let if_expr = hir::Expr {
            id: if_expr_id,
            span: expr.span,
            kind: hir::ExprKind::If(
                Box::new(condition),
                Box::new(body),
                Some(Box::new(else_block)),
            ),
        };
        hir::Expr {
            id: loop_id,
            span: expr.span,
            kind: hir::ExprKind::Loop(
                Box::new(hir::Block {
                    id: loop_block_id,
                    span: expr.span,
                    stmts: vec![hir::Stmt {
                        id: if_stmt_id,
                        span: expr.span,
                        kind: hir::StmtKind::Expr(if_expr),
                    }],
                    result: None,
                }),
                hir::LoopSource::While,
            ),
        }
    }
    fn lower_for_expr(
        &mut self,
        expr: &ast::Expr,
        pat: &ast::Pattern,
        iterator: &ast::IteratorExpr,
        body: &ast::Block,
    ) -> hir::Expr {
        let pat = self.lower_pattern(pat);
        let iterator = match &iterator.kind {
            ast::IteratorExprKind::Expr(expr) => {
                hir::Iterator::Expr(Box::new(self.lower_expr(expr)))
            }
            ast::IteratorExprKind::Range(start, end) => hir::Iterator::Ranged(
                iterator.span,
                Box::new(self.lower_expr(start)),
                Box::new(self.lower_expr(end)),
            ),
        };
        let body = self.lower_block(body);
        hir::Expr {
            id: self.next_hir_id(),
            span: expr.span,
            kind: hir::ExprKind::For(Box::new(pat), Box::new(iterator), Box::new(body)),
        }
    }
    fn lower_item(&mut self, item: &ast::Item) -> (DefId, hir::Item) {
        let span = item.span;
        let (id, kind) = match &item.kind {
            ItemKind::Type(type_def) => {
                let id = self.expect_def_id(type_def.id);
                let kind = match &type_def.kind {
                    ast::TypeDefKind::Struct(struct_def) => {
                        let fields = struct_def
                            .fields
                            .iter()
                            .map(|field| hir::StructField {
                                id: self.expect_def_id(field.id),
                                span: field.span,
                                name: field.name,
                                ty: self.lower_ty(&field.ty),
                            })
                            .collect();
                        hir::TypeDefKind::Struct(hir::StructDef {
                            span: struct_def.span,
                            fields,
                        })
                    }
                    ast::TypeDefKind::Variant(variant_def) => {
                        let cases = variant_def
                            .cases
                            .iter()
                            .map(|case| hir::VariantCase {
                                id: self.expect_def_id(case.id),
                                name: case.name,
                                span: case.span,
                                fields: case.fields.as_ref().map(|fields| {
                                    fields
                                        .iter()
                                        .map(|field| hir::VariantField {
                                            id: self.expect_def_id(field.id),
                                            span: field.ty.span,
                                            ty: self.lower_ty(&field.ty),
                                        })
                                        .collect()
                                }),
                            })
                            .collect();
                        hir::TypeDefKind::Variant(hir::VariantDef {
                            span: variant_def.span,
                            cases,
                        })
                    }
                };
                (
                    id,
                    hir::ItemKind::TypeDef(hir::TypeDef {
                        id,
                        span,
                        kind,
                        name: type_def.name,
                    }),
                )
            }
            ItemKind::Function(function_def) => {
                let id = self.expect_def_id(function_def.id);
                let params = function_def
                    .params
                    .iter()
                    .map(|param| hir::Param {
                        pat: self.lower_pattern(&param.pattern),
                    })
                    .collect();
                let value = self.lower_expr(&function_def.body);
                let body_id = value.id;
                let body = Body { params, value };
                self.bodies.insert(body_id, body);
                (
                    id,
                    hir::ItemKind::Function(hir::FunctionDef {
                        id,
                        name: function_def.name,
                        sig: self.lower_function_sig(
                            function_def.params.iter().map(|param| &param.ty),
                            function_def.return_type.as_ref(),
                        ),
                        body_id,
                        span,
                    }),
                )
            }
        };
        (id, hir::Item { id, kind })
    }
    pub fn lower_ast(mut self, ast: &ast::Ast) -> Hir {
        let items = ast
            .items
            .iter()
            .map(|item| self.lower_item(item))
            .collect::<Vec<_>>();
        self.items.extend(items);
        self.diag.emit();
        Hir {
            items: self.items,
            bodies: self.bodies,
            def_info: self.resolution_results.get_def_info(),
        }
    }
}

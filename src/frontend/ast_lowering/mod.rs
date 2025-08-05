use std::cell::Cell;

use fxhash::FxHashMap;
use indexmap::IndexMap;

use crate::{
    frontend::{
        ast::{self, ItemKind, NodeId},
        hir::{self, Body, DefId, Hir, HirId},
        resolution::resolve::ResolveResults,
    },
    span::{Span, symbol::symbols},
};

pub struct AstLower {
    resolution_results: ResolveResults,
    node_ids_to_hir_ids: FxHashMap<NodeId, HirId>,
    bodies: IndexMap<HirId, hir::Body>,
    items: IndexMap<DefId, hir::Item>,
    current_loop_label: Option<HirId>,
    next_id: Cell<u32>,
}
impl AstLower {
    pub fn new(resolution_results: ResolveResults) -> Self {
        Self {
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
                hir::Resolution::Builtin => hir::Resolution::Builtin,
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
    fn lower_ty(&self, ty: &ast::Type) -> hir::Type {
        hir::Type {
            id: self.next_hir_id(),
            span: ty.span,
            kind: match &ty.kind {
                ast::TypeKind::Underscore => hir::TypeKind::Infer,
                ast::TypeKind::Grouped(grouped_ty) => {
                    return {
                        let mut lowered_ty = self.lower_ty(grouped_ty);
                        lowered_ty.span = grouped_ty.span;
                        lowered_ty
                    };
                }
                _ => todo!("{ty:?} THE REST OF TYPE LOWERING"),
            },
        }
    }
    fn lower_pattern(&mut self, pattern: &ast::Pattern) -> hir::Pattern {
        let kind = match pattern.kind {
            ast::PatternKind::Grouped(ref pat) => {
                return {
                    let mut pat = self.lower_pattern(pat);
                    pat.span = pattern.span;
                    pat
                };
            }
            ast::PatternKind::Deref(ref pat) => {
                let pat = self.lower_pattern(pat);
                hir::PatternKind::Deref(Box::new(pat))
            }
            ast::PatternKind::Literal(_) => todo!("LITERAL PATTERNS"),
            ast::PatternKind::Wildcard => hir::PatternKind::Wildcard,
            ast::PatternKind::Ident(name, mutable, by_ref) => {
                let pat_id = self.lower_id(pattern.id);
                let Some(hir::Resolution::Variable(hir_id)) = self.map_res(pattern.id) else {
                    unreachable!("There should be a binding for patterns always")
                };
                return hir::Pattern {
                    id: pat_id,
                    span: pattern.span,
                    kind: hir::PatternKind::Binding(hir_id, name, mutable, by_ref),
                };
            }
            ast::PatternKind::Tuple(ref elements) => hir::PatternKind::Tuple(
                elements
                    .iter()
                    .map(|element| self.lower_pattern(element))
                    .collect(),
            ),
        };
        hir::Pattern {
            id: self.next_hir_id(),
            kind,
            span: pattern.span,
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
        let block = self.lower_block(block);
        hir::Expr {
            id: self.next_hir_id(),
            span: block.span,
            kind: hir::ExprKind::Block(Box::new(block)),
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
            ast::ExprKind::While(condition, body) => {
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
                let span = Span::new(expr.span.info().end_offset - 1, 1);
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
                    kind: hir::ExprKind::Loop(Box::new(hir::Block {
                        id: loop_block_id,
                        span: expr.span,
                        stmts: vec![hir::Stmt {
                            id: if_stmt_id,
                            span: expr.span,
                            kind: hir::StmtKind::Expr(if_expr),
                        }],
                        result: None,
                    })),
                }
            }
            ast::ExprKind::For(pat, iterator, body) => {
                self.lower_for_expr(expr, pat, iterator, body)
            }
            ast::ExprKind::Break(expr) => {
                let id = self.current_loop_label.ok_or(hir::NoLabel);
                let expr = expr
                    .as_ref()
                    .map(|expr| self.lower_expr(expr))
                    .map(Box::new);
                lower_expr!(hir::ExprKind::Break(id, expr))
            }
            ast::ExprKind::Ident(_) => {
                lower_expr!(hir::ExprKind::Path(
                    self.map_res(expr.id).unwrap_or(hir::Resolution::Err)
                ))
            }
            _ => todo!("{:?}THE RESTs", expr),
        }
    }
    fn lower_for_expr(
        &mut self,
        expr: &ast::Expr,
        pat: &ast::Pattern,
        iterator: &ast::IteratorExpr,
        body: &ast::Block,
    ) -> hir::Expr {
        /*
            This goes from :
                for $pat in $expr
                    $loop_body

                {
                    let iterator = iter($expr);
                    'l : loop{
                        match next($iterator) {
                            Option.Some(v) => {
                                let $pat = v;
                                $loop_body
                            },
                            Option.None => { break 'l;}
                        }
                    }
                }
        */
        let outer_block_expr_id = self.next_hir_id();
        let outer_block_id = self.next_hir_id();
        let (let_stmt, iter_pat_id) = {
            let let_stmt_id = self.next_hir_id();
            let iter_pat_id = self.next_hir_id();
            let iterator = match &iterator.kind {
                ast::IteratorExprKind::Range(start, end) => hir::Expr {
                    id: self.next_hir_id(),
                    span: start.span.combined(end.span),
                    kind: hir::ExprKind::Range(
                        Box::new(self.lower_expr(start)),
                        Box::new(self.lower_expr(end)),
                    ),
                },
                ast::IteratorExprKind::Expr(expr) => self.lower_expr(expr),
            };
            (
                hir::Stmt {
                    id: let_stmt_id,
                    span: iterator.span,
                    kind: hir::StmtKind::Let(
                        hir::Pattern {
                            id: iter_pat_id,
                            span: iterator.span,
                            kind: hir::PatternKind::Binding(
                                iter_pat_id,
                                symbols::ITER,
                                ast::Mutable::Yes(iterator.span),
                                ast::ByRef::No,
                            ),
                        },
                        None,
                        Box::new(hir::Expr {
                            id: self.next_hir_id(),
                            span: iterator.span,
                            kind: hir::ExprKind::Call(
                                Box::new(hir::Expr {
                                    id: self.next_hir_id(),
                                    span: iterator.span,
                                    kind: hir::ExprKind::Path(hir::Resolution::Builtin),
                                }),
                                vec![iterator],
                            ),
                        }),
                    ),
                },
                iter_pat_id,
            )
        };
        let loop_stmt = {
            let loop_stmt_id = self.next_hir_id();
            let loop_id = self.next_hir_id();
            let loop_block_id = self.next_hir_id();
            let old_loop_label = self.current_loop_label.replace(loop_id);

            let match_stmt = {
                let match_stmt_id = self.next_hir_id();
                let match_expr_id = self.next_hir_id();

                let next_call = {
                    let next_call_id = self.next_hir_id();
                    let next_func_id = self.next_hir_id();
                    let iter_mut_ref_id = self.next_hir_id();
                    let iter_expr_id = self.next_hir_id();
                    hir::Expr {
                        id: next_call_id,
                        span: iterator.span,
                        kind: hir::ExprKind::Call(
                            Box::new(hir::Expr {
                                id: next_func_id,
                                span: iterator.span,
                                kind: hir::ExprKind::Path(hir::Resolution::Builtin),
                            }),
                            vec![hir::Expr {
                                id: iter_mut_ref_id,
                                span: iterator.span,
                                kind: hir::ExprKind::Ref(
                                    ast::Mutable::Yes(iterator.span),
                                    Box::new(hir::Expr {
                                        id: iter_expr_id,
                                        span: iterator.span,
                                        kind: hir::ExprKind::Path(hir::Resolution::Variable(
                                            iter_pat_id,
                                        )),
                                    }),
                                ),
                            }],
                        ),
                    }
                };

                let some_arm = {
                    hir::MatchArm {
                        id: self.next_hir_id(),
                        span: body.span,
                        pat: hir::Pattern {
                            id: self.next_hir_id(),
                            span: pat.span,
                            kind: hir::PatternKind::Case(
                                hir::Resolution::Builtin,
                                vec![self.lower_pattern(pat)],
                            ),
                        },
                        //The actual body
                        body: self.lower_block_expr(body),
                    }
                };

                let none_arm = {
                    hir::MatchArm {
                        id: self.next_hir_id(),
                        span: body.span,
                        pat: hir::Pattern {
                            id: self.next_hir_id(),
                            span: pat.span,
                            kind: hir::PatternKind::Case(hir::Resolution::Builtin, vec![]),
                        },
                        //The break
                        body: hir::Expr {
                            id: self.next_hir_id(),
                            span: body.span,
                            kind: hir::ExprKind::Break(Ok(loop_id), None),
                        },
                    }
                };
                hir::Stmt {
                    id: match_stmt_id,
                    span: body.span,
                    kind: hir::StmtKind::Expr(hir::Expr {
                        id: match_expr_id,
                        span: body.span,
                        kind: hir::ExprKind::Match(Box::new(next_call), vec![some_arm, none_arm]),
                    }),
                }
            };
            let stmts = vec![match_stmt];

            self.current_loop_label = old_loop_label;
            hir::Stmt {
                id: loop_stmt_id,
                span: body.span,
                kind: hir::StmtKind::Expr(hir::Expr {
                    id: loop_id,
                    span: body.span,
                    kind: hir::ExprKind::Loop(Box::new(hir::Block {
                        id: loop_block_id,
                        span: body.span,
                        stmts,
                        result: None,
                    })),
                }),
            }
        };
        hir::Expr {
            id: outer_block_expr_id,
            span: expr.span,
            kind: hir::ExprKind::Block(Box::new(hir::Block {
                id: outer_block_id,
                span: expr.span,
                stmts: vec![let_stmt, loop_stmt],
                result: None,
            })),
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
                                span: case.span,
                                fields: case
                                    .fields
                                    .iter()
                                    .map(|field| hir::VariantField {
                                        id: self.expect_def_id(field.id),
                                        span: field.ty.span,
                                        ty: self.lower_ty(&field.ty),
                                    })
                                    .collect(),
                            })
                            .collect();
                        hir::TypeDefKind::Variant(hir::VariantDef {
                            span: variant_def.span,
                            cases,
                        })
                    }
                };
                (id, hir::ItemKind::TypeDef(hir::TypeDef { id, span, kind }))
            }
            ItemKind::Function(function_def) => {
                let id = self.expect_def_id(function_def.id);
                let value = self.lower_block_expr(&function_def.body);
                let body_id = value.id;
                let body = Body {
                    params: function_def
                        .params
                        .iter()
                        .map(|param| hir::Param {
                            pat: self.lower_pattern(&param.pattern),
                        })
                        .collect(),
                    value,
                };
                self.bodies.insert(body_id, body);
                (
                    id,
                    hir::ItemKind::Function(hir::FunctionDef {
                        id,
                        sig: self.lower_function_sig(
                            function_def.params.iter().map(|param| &param.ty),
                            function_def.return_type.as_ref(),
                        ),
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
        Hir {
            items: self.items,
            bodies: self.bodies,
        }
    }
}

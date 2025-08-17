use fxhash::FxHashSet;
use indexmap::{IndexMap, map::Entry};

use crate::{
    Resolver,
    frontend::{
        ast::{
            self, Ast, Block, Expr, ExprKind, FunctionDef, Item, ItemKind, NodeId, PathSegment,
            Pattern, PatternKind, StmtKind, Type, TypeDef, TypeDefKind,
        },
        ast_visit::{Visitor, walk_ast, walk_block, walk_expr, walk_iterator, walk_pat, walk_type},
        hir::{Builtin, DefId, DefKind, Definition, Resolution},
    },
    span::{
        Span,
        symbol::{Ident, Symbol, symbols},
    },
};

#[derive(Debug)]
struct ScopeData {
    bindings: IndexMap<Symbol, Resolution<NodeId>>,
}
struct Namespace {
    children: Vec<(Symbol, Definition)>,
}
pub struct NameRes<'rsv, 'src> {
    resolver: &'rsv mut Resolver<'src>,
    scopes: Vec<ScopeData>,
    namespaces: IndexMap<Definition, Namespace>,
}

impl<'a, 'b> NameRes<'a, 'b> {
    pub fn new(resolver: &'a mut Resolver<'b>) -> Self {
        let root_scope = ScopeData {
            bindings: [
                (symbols::PRINTLN, Resolution::Builtin(Builtin::Println)),
                (symbols::OPTION, Resolution::Builtin(Builtin::Option)),
            ]
            .into_iter()
            .collect(),
        };
        let namespaces = [(
            Definition::Builtin(Builtin::Option),
            Namespace {
                children: vec![
                    (symbols::SOME, Definition::Builtin(Builtin::OptionSome)),
                    (symbols::NONE, Definition::Builtin(Builtin::OptionNone)),
                ],
            },
        )];
        Self {
            resolver,
            scopes: [root_scope].into_iter().collect(),
            namespaces: namespaces.into_iter().collect(),
        }
    }
    fn push_scope(&mut self) {
        self.scopes.push(ScopeData {
            bindings: IndexMap::new(),
        });
    }
    fn pop_scope(&mut self) {
        if self.scopes.len() == 1 {
            return;
        }
        self.scopes.pop();
    }
    fn current_scope_mut(&mut self) -> &mut ScopeData {
        self.scopes
            .last_mut()
            .expect("There should always be at least 1 scope")
    }
    fn create_item_binding(&mut self, name: Symbol, res: Resolution<NodeId>, span: Span) {
        match self.current_scope_mut().bindings.entry(name) {
            Entry::Occupied(_) => self
                .resolver
                .error(format!("Repeated item {}.", name.as_str()), span),
            Entry::Vacant(entry) => {
                entry.insert(res);
            }
        }
    }
    fn in_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        self.push_scope();
        f(self);
        self.pop_scope();
    }
    fn get_binding_in_scope(&self, name: Symbol) -> Option<Resolution<NodeId>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(&name))
            .copied()
    }
    fn expect_def_id(&self, id: NodeId) -> DefId {
        self.resolver
            .node_ids_to_defs
            .get(&id)
            .copied()
            .expect("Should have a def-id")
    }
    fn create_binding(
        &mut self,
        name: Symbol,
        kind: Resolution<NodeId>,
    ) -> Option<Resolution<NodeId>> {
        self.current_scope_mut().bindings.insert(name, kind)
    }
    fn collect_bindings_in_patttern(pattern: &Pattern, bindings: &mut Vec<(Symbol, NodeId, Span)>) {
        match pattern.kind {
            PatternKind::Ident(name, _mut, _by_ref) => {
                bindings.push((name, pattern.id, pattern.span));
            }
            PatternKind::Deref(ref pat) | PatternKind::Grouped(ref pat) => {
                Self::collect_bindings_in_patttern(pat, bindings);
            }

            PatternKind::Case(_, ref patterns) | PatternKind::Tuple(ref patterns) => patterns
                .iter()
                .for_each(|pat| Self::collect_bindings_in_patttern(pat, bindings)),
            PatternKind::Literal(_) | PatternKind::Wildcard => (),
        }
    }
    fn apply_bindings(
        &mut self,
        bindings: Vec<(Symbol, NodeId, Span)>,
        repeat_error: impl Fn(Symbol) -> String,
    ) {
        let mut seen_bindings = FxHashSet::default();
        for (name, id, span) in bindings {
            if !seen_bindings.insert(name) {
                self.resolver.error(repeat_error(name), span);
            }
            self.create_binding(name, Resolution::Variable(id));
            self.resolver
                .resolutions
                .insert(id, Resolution::Variable(id));
        }
    }
    fn resolve_name_in_current_scope(
        &mut self,
        id: NodeId,
        name: Ident,
    ) -> Option<Resolution<NodeId>> {
        let Some(current) = self.get_binding_in_scope(name.symbol) else {
            self.resolver.error(
                format!("Cannot find '{}' in scope.", name.symbol.as_str()),
                name.span,
            );
            return None;
        };
        self.resolver.resolutions.insert(id, current);
        Some(current)
    }
    fn resolve_path(
        &mut self,
        id: NodeId,
        head: PathSegment,
        segments: impl Iterator<Item = PathSegment>,
    ) -> Option<Resolution<NodeId>> {
        let mut current = self.resolve_name_in_current_scope(head.id, head.name)?;
        for next_seg in segments {
            let definition = match current {
                Resolution::Variable(_) => return None,
                Resolution::Err => return None,
                Resolution::Def(_, DefKind::Field | DefKind::Function | DefKind::VariantCase) => {
                    self.resolver.error(
                        format!(
                            "'{}' has no item '{}'.",
                            current.as_str(),
                            next_seg.name.symbol.as_str()
                        ),
                        next_seg.name.span,
                    );
                    return None;
                }
                Resolution::Builtin(builtin) => Definition::Builtin(builtin),
                Resolution::Def(id, DefKind::Struct | DefKind::Variant) => Definition::Def(id),
            };
            let Some(namespace) = self.namespaces.get(&definition) else {
                self.resolver.error(
                    format!(
                        "'{}' has no item '{}'.",
                        current.as_str(),
                        next_seg.name.symbol.as_str()
                    ),
                    next_seg.name.span,
                );
                return None;
            };
            let Some(next) = namespace.children.iter().find_map(|&(name, def)| {
                (name == next_seg.name.symbol).then_some(match def {
                    Definition::Builtin(builtin) => Resolution::Builtin(builtin),
                    Definition::Def(id) => Resolution::Def(id, self.resolver.info[id].kind),
                })
            }) else {
                self.resolver.error(
                    format!("'type' has no item '{}'.", next_seg.name.symbol.as_str()),
                    next_seg.span,
                );
                return None;
            };
            current = next;
            self.resolver.resolutions.insert(next_seg.id, current);
        }
        self.resolver.resolutions.insert(id, current);
        Some(current)
    }
    fn resolve_type_def(&mut self, type_def: &TypeDef) {
        match &type_def.kind {
            TypeDefKind::Struct(struct_def) => {
                let mut seen_fields = FxHashSet::default();
                for field in struct_def.fields.iter() {
                    if !seen_fields.insert(field.name.symbol) {
                        self.resolver.error(
                            format!("Repeated field '{}'.", field.name.symbol.as_str()),
                            field.span,
                        );
                    }
                    self.visit_ty(&field.ty);
                }
            }
            TypeDefKind::Variant(variant_def) => {
                let mut seen_cases = FxHashSet::default();
                for case in variant_def.cases.iter() {
                    if !seen_cases.insert(case.name.symbol) {
                        self.resolver.error(
                            format!("Repeated case '{}'.", case.name.symbol.as_str()),
                            case.span,
                        );
                    }
                    for field in case.fields.iter().flatten() {
                        self.visit_ty(&field.ty);
                    }
                }
            }
        }
    }
    pub(super) fn define_item(&mut self, item: &Item) {
        match item.kind {
            ItemKind::Function(ref function_def) => {
                self.create_item_binding(
                    function_def.name.symbol,
                    Resolution::Def(self.expect_def_id(function_def.id), DefKind::Function),
                    function_def.name.span,
                );
            }
            ItemKind::Type(ref type_def) => {
                let def_id = self.expect_def_id(type_def.id);
                self.create_item_binding(
                    type_def.name.symbol,
                    Resolution::Def(
                        def_id,
                        match type_def.kind {
                            TypeDefKind::Struct(..) => DefKind::Struct,
                            TypeDefKind::Variant(..) => DefKind::Variant,
                        },
                    ),
                    type_def.name.span,
                );
                match &type_def.kind {
                    TypeDefKind::Struct(struct_def) => {
                        let mut children = Vec::new();
                        for field in struct_def.fields.iter() {
                            let field_def_id = self.expect_def_id(field.id);
                            children.push((field.name.symbol, Definition::Def(field_def_id)));
                        }
                        self.namespaces
                            .insert(Definition::Def(def_id), Namespace { children });
                    }
                    TypeDefKind::Variant(variant_def) => {
                        let mut children = Vec::new();
                        for case in variant_def.cases.iter() {
                            let case_id = self.expect_def_id(case.id);
                            children.push((case.name.symbol, Definition::Def(case_id)));
                        }
                        self.namespaces
                            .insert(Definition::Def(def_id), Namespace { children });
                    }
                }
            }
        }
    }
    fn resolve_type(&mut self, ty: &Type) {
        use crate::frontend::ast::TypeKind;
        match &ty.kind {
            TypeKind::Named(name) => {
                self.resolve_path(name.id, name.head, name.tail.iter().copied());
            }
            _ => walk_type(self, ty),
        }
    }
    fn resolve_block(&mut self, block: &Block) {
        for item in block.stmts.iter().filter_map(|stmt| match stmt.kind {
            StmtKind::Item(ref item) => Some(&**item),
            _ => None,
        }) {
            self.define_item(item);
        }
        walk_block(self, block);
    }
    fn resolve_expr(&mut self, expr: &Expr) {
        match expr.kind {
            ExprKind::Ident(name) => {
                self.resolve_name_in_current_scope(
                    expr.id,
                    Ident {
                        symbol: name,
                        span: expr.span,
                    },
                );
            }
            ExprKind::Init(ref name, _) => {
                if let Some(name) = name {
                    self.resolve_path(name.id, name.head, name.tail.iter().copied());
                }
                walk_expr(self, expr);
            }
            ExprKind::Path(ref name) => {
                self.resolve_path(name.id, name.head, name.tail.iter().copied());
            }
            ExprKind::Block(ref block) => {
                self.in_scope(|this| {
                    this.resolve_block(block);
                });
            }
            ExprKind::If(ref condition, ref body, ref else_) => {
                self.in_scope(|this| {
                    this.resolve_expr(condition);
                    this.resolve_block(body);
                });
                if let Some(else_) = else_ {
                    self.resolve_expr(else_);
                }
            }
            ExprKind::While(ref condition, ref block) => {
                self.in_scope(|this| {
                    this.resolve_expr(condition);
                    this.resolve_block(block);
                });
            }
            ExprKind::For(ref pattern, ref iterator, ref body) => {
                self.in_scope(|this| {
                    walk_iterator(this, iterator);
                    this.resolve_pattern(pattern);
                    this.resolve_block(body);
                });
            }
            ExprKind::Match(ref scrutinee, ref arms) => {
                self.resolve_expr(scrutinee);
                for arm in arms {
                    self.in_scope(|this| {
                        this.resolve_pattern(&arm.pat);
                        this.resolve_expr(&arm.body);
                    });
                }
            }
            _ => walk_expr(self, expr),
        }
    }
    fn resolve_pattern(&mut self, pattern: &Pattern) {
        let mut bindings = Vec::new();
        Self::collect_bindings_in_patttern(pattern, &mut bindings);
        self.apply_bindings(bindings, |name| {
            format!("Repeated binding '{}'.", name.as_str())
        });
        self.visit_pat(pattern);
    }
    fn resolve_function_def(&mut self, function_def: &FunctionDef) {
        let mut bindings = Vec::new();
        for param in function_def.params.iter() {
            Self::collect_bindings_in_patttern(&param.pattern, &mut bindings);
        }
        self.apply_bindings(bindings, |name| {
            format!("Repeated binding '{}' in parameter list.", name.as_str())
        });
        for param in function_def.params.iter() {
            self.resolve_type(&param.ty);
        }
        if let Some(return_ty) = &function_def.return_type {
            self.resolve_type(return_ty);
        }
        self.resolve_expr(&function_def.body);
    }
}

impl Visitor for NameRes<'_, '_> {
    fn visit_expr(&mut self, expr: &Expr) {
        self.resolve_expr(expr);
    }
    fn visit_item(&mut self, item: &Item) {
        match item.kind {
            ItemKind::Type(ref type_def) => {
                self.resolve_type_def(type_def);
            }
            ItemKind::Function(ref function_def) => {
                self.resolve_function_def(function_def);
            }
        }
    }
    fn visit_let_stmt(&mut self, pat: &Pattern, ty: Option<&Type>, expr: &Expr) {
        self.resolve_expr(expr);
        self.resolve_pattern(pat);
        if let Some(ty) = ty {
            self.resolve_type(ty);
        }
    }
    fn visit_pat(&mut self, pat: &Pattern) {
        if let ast::PatternKind::Case(path, _) = &pat.kind {
            self.resolve_path(path.id, path.head, path.tail.iter().copied());
        }
        walk_pat(self, pat)
    }
    fn visit_block(&mut self, block: &Block) {
        self.resolve_block(block);
    }
    fn visit_ast(&mut self, ast: &Ast) {
        for item in ast.items.iter() {
            self.define_item(item);
        }
        walk_ast(self, ast);
    }
    fn visit_ty(&mut self, ty: &Type) {
        self.resolve_type(ty);
    }
}

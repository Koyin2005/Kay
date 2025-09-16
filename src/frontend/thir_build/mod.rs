use crate::{context::CtxtRef, frontend::{hir::{self, Builtin, DefId, DefKind, HirId, Resolution}, thir::{Body, Expr, ExprId, ExprKind, Param, Pattern, PatternKind, Thir}, typecheck::{Coercion, TypeCheckResults}}, indexvec::IndexVec};

pub struct ThirBuild<'ctxt>{
    ctxt : CtxtRef<'ctxt>,
    bodies : Vec<Body>
}
impl<'ctxt> ThirBuild<'ctxt>{
    
    pub fn new(ctxt: CtxtRef<'ctxt>) -> Self{
        Self { ctxt, bodies: Vec::new() }
    }
    
    pub fn build(&mut self, owner: hir::DefId,  body: &hir::Body, results : TypeCheckResults){
        let Some(body) = ThirBuilder::new(self.ctxt, owner,results).build(body) else {
            return;
        };
        self.bodies.push(body);
    }
    pub fn finish(self) -> Thir{
        Thir { bodies: self.bodies.into_boxed_slice()}
    }
}
struct ThirBuilder<'ctxt>{
    ctxt : CtxtRef<'ctxt>,
    body : Body,
    results : TypeCheckResults
    
}
impl<'ctxt> ThirBuilder<'ctxt>{
    pub fn new(ctxt: CtxtRef<'ctxt>,owner : DefId, results: TypeCheckResults) -> Self{
        Self { ctxt, body: Body { owner, params: Vec::new(), arms: IndexVec::new(), exprs: IndexVec::new() }, results }
    }
    fn lower_expr(&mut self, expr: &hir::Expr) -> ExprId{
        let id = expr.id;
        let mut expr = self.make_expr(expr);
        if let Some(coercion) = self.results.get_coercion(id){
            match coercion{
                Coercion::NeverToAny(ty) => {
                    let span = expr.span;
                    let expr_id = self.body.exprs.push(expr);
                    expr = Expr { ty: ty.clone(), span, kind: ExprKind::NeverToAny(expr_id) };
                }
            }
        }
        self.body.exprs.push(expr)
    }
    fn get_res(&self, id: HirId) -> Option<Resolution>{
        self.results.get_res(id).copied()
    }
    fn lower_exprs<'a>(&mut self, exprs: impl IntoIterator<Item = &'a hir::Expr>) -> Box<[ExprId]>{
        exprs.into_iter().map(|expr| self.lower_expr(expr)).collect()
    }
    fn make_expr(&mut self, expr: &hir::Expr) -> Expr{
        let kind = match &expr.kind{
            hir::ExprKind::Err => unreachable!("Can't use an ExprKind::Err in thir"),
            hir::ExprKind::Tuple( elements) => {
                ExprKind::Tuple(self.lower_exprs(elements))
            },
            hir::ExprKind::Binary(op,left,right) => {
                ExprKind::Binary(*op, self.lower_expr(&left), self.lower_expr(right))
            },
            hir::ExprKind::Call(callee, args) => {
                let variant_case = match &callee.kind{
                    hir::ExprKind::Path(path) => {
                        let Some(res) = self.get_res(path.id) else {
                            panic!("Resolution not finished")
                        };
                        match res{
                            Resolution::Def(id, DefKind::VariantCase) => {
                                Some(id)
                            },
                            _ => None
                        }
                    },
                    _ => None
                };
                todo!("ACTUAL CALLS")
            },
            _ => todo!("OTHER EXPRS")

        };
        Expr { ty: self.results.type_of(expr.id), span: expr.span, kind }
    }
    fn lower_pattern(&mut self, pattern : &hir::Pattern) -> Pattern{
        let ty = self.results.type_of(pattern.id);
        Pattern { ty, span: pattern.span, kind: match pattern.kind {
            hir::PatternKind::Binding(id,name,is_mut,by_ref) => {
                PatternKind::Binding(id,name,by_ref, is_mut)
            },
            hir::PatternKind::Case(..) => todo!("CASE PATTERNS"),
            hir::PatternKind::Wildcard => PatternKind::Wilcard,
            hir::PatternKind::Deref(..) => todo!("DEREF PATTERNS"),
            hir::PatternKind::Literal(..) => todo!("LITERAL PATTERNS"),
            hir::PatternKind::Tuple(..) => todo!("TUPLE PATTERNS")
        } }
    }
    pub fn build(mut self, hir: &hir::Body ) -> Option<Body>{
        if self.results.had_error(){
            return None;
        }
        for param in hir.params.iter(){
            let param = Param{ pattern : self.lower_pattern(&param.pat)};
            self.body.params.push(param);
        }
        self.lower_expr(&hir.value);
        Some(self.body)
    }
}

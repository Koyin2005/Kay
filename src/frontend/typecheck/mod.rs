use crate::{
    context::CtxtRef,
    errors::DiagnosticReporter,
    frontend::{
        ast::LiteralKind,
        hir::{Block, Body, DefId, Expr, ExprKind, IntType, Pattern, PrimitiveType, Stmt, StmtKind},
        ty_lower::TypeLower,
    },
    span::symbol::Ident,
    types::{format::TypeFormat, Type},
};
#[derive(Clone, Copy)]
enum Expected<'a> {
    Type(&'a Type),
    None,
}
pub struct TypeCheck<'ctxt> {
    context: CtxtRef<'ctxt>,
    id: DefId,
    body: &'ctxt Body,
}
impl<'ctxt> TypeCheck<'ctxt> {
    pub fn new(context: CtxtRef<'ctxt>, id: DefId) -> Option<Self> {
        let body = context.get_body_for(id)?;
        Some(Self { context, id, body })
    }
    fn diag<'a>(&'a self) -> &'a DiagnosticReporter
    where
        'a: 'ctxt,
    {
        self.context.diag()
    }
    fn check_lit(&self, literal: LiteralKind, expected_ty: Expected) -> Type {
        match literal {
            LiteralKind::Bool(_) => Type::new_bool(),
            LiteralKind::Int(_) => Type::new_int(
                if matches!(
                    expected_ty,
                    Expected::Type(Type::Primitive(PrimitiveType::Int(IntType::Unsigned)))
                ) {
                    IntType::Unsigned
                } else {
                    IntType::Signed
                },
            ),
            LiteralKind::String(_) => Type::new_ref_str(),
        }
    }
    fn check_field(&self, receiver: &Expr, field: Ident) -> Type {
        let receiver = self.check_expr(receiver, Expected::None);
        match receiver {
            Type::Struct(fields) => {
                if let Some(ty) = fields.iter().find_map(|receiver_field| {
                    (receiver_field.name == field.symbol).then(|| receiver_field.ty.clone())
                }) {
                    ty
                } else {
                    todo!("NO FIELD")
                }
            }
            Type::Tuple(fields) => {
                if let Ok(index) = field.symbol.as_str().parse::<usize>() {
                    fields
                        .get(index)
                        .cloned()
                        .unwrap_or_else(|| todo!("NO TUPLE FIELD"))
                } else {
                    todo!("DEFO NOT TUPLE FIELD")
                }
            }
            Type::Nominal(..) => todo!("HANDLE NOMINAL TYPES"),
            _ => todo!("BAD TYPE"),
        }
    }
    fn check_tuple(&self, fields: &[Expr], expected_ty: Expected) -> Type {
        let field_tys = if let Expected::Type(Type::Tuple(fields)) = expected_ty {
            fields
        } else {
            &Vec::new()
        };
        let field_tys = fields.iter().enumerate().map(|(i, field)| {
            let expected_ty = field_tys
                .get(i)
                .map(Expected::Type)
                .unwrap_or_else(|| Expected::None);
            self.check_expr(field, expected_ty)
        });
        Type::new_tuple_from_iter(field_tys)
    }
    fn check_stmt(&self, stmt: &Stmt){
        match &stmt.kind{
            StmtKind::Expr(expr) => {
                self.check_expr(expr, Expected::Type(&Type::new_unit()));
            },
            StmtKind::ExprWithSemi(expr) => {
                self.check_expr(expr, Expected::None);
            },
            StmtKind::Item(_) => {
                //Don't check items here
            },
            StmtKind::Let(..) => todo!("CHECK LETS")
        }
    }
    fn check_block(&self, block: &Block, expected_ty: Expected) -> Type{
        for stmt in block.stmts.iter(){
            self.check_stmt(stmt);
        }
        if let Some(ref expr) = block.result{
            self.check_expr(expr, expected_ty)
        }
        else{
            Type::new_unit()
        }
    }
    fn check_expr(&self, expr: &Expr, expected_ty: Expected) -> Type {
        let Expr { kind, .. } = expr;
        let ty = match kind {
            &ExprKind::Literal(literal) => self.check_lit(literal, expected_ty),
            &ExprKind::Field(ref reciever, field) => self.check_field(reciever, field),
            ExprKind::Tuple(elements) => self.check_tuple(elements, expected_ty),
            ExprKind::As(expr, ty) => self.check_expr(
                expr,
                Expected::Type(&TypeLower::new(self.context, false).lower(ty)),
            ),
            ExprKind::Block(block) => self.check_block(block, expected_ty),
            ExprKind::Err => Type::Err,
            kind => todo!("OTHER EXPR KINDS {:?}", kind),
        };
        if !ty.is_error()
            && let Expected::Type(expect_ty) = expected_ty
            && &ty != expect_ty
        {
            let format = TypeFormat::new(self.context);
            self.diag().emit_diag(
                format!(
                    "Expected '{}' got '{}'.",
                    format.format_type(expect_ty),
                    format.format_type(&ty)
                ),
                expr.span,
            );
            Type::Err
        }
        else{
            ty
        }
    }
    fn check_pattern(&self, _pat: &Pattern, _expected_ty: Type) {
        todo!("CHECK PATTERN")
    }
    pub fn check(self) {
        let (params, return_ty) = self.context.signature_of(self.id);
        for (param, ty) in self.body.params.iter().zip(params) {
            self.check_pattern(&param.pat, ty);
        }
        self.check_expr(&self.body.value, Expected::Type(&return_ty));
    }
}

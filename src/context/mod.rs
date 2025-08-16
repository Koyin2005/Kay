use crate::{
    errors::DiagnosticReporter,
    frontend::{
        hir::{self, Builtin, DefId, DefKind, Definition, Hir},
        ty_lower::TypeLower,
    },
    indexvec::IndexVec,
    span::symbol::{Ident, Symbol},
    types::{FieldIndex, GenericArg, Type, VariantCaseIndex},
};
use indexmap::IndexMap;
use typed_arena::Arena;
pub struct FieldDef {
    pub id: DefId,
    pub name: Symbol,
}
pub struct TypeDefCase {
    pub fields: IndexVec<FieldIndex, FieldDef>,
}
pub enum TypeDefKind {
    Variant(IndexVec<VariantCaseIndex, (DefId, TypeDefCase)>),
    Struct(TypeDefCase),
}
pub struct TypeDef {
    pub id: DefId,
    pub kind: TypeDefKind,
}
impl TypeDef {
    pub fn case_with_id(&self, id: DefId) -> Option<&TypeDefCase> {
        match &self.kind {
            TypeDefKind::Variant(cases) => {
                cases.iter().find_map(
                    |(case_id, variant)| {
                        if id == *case_id { Some(variant) } else { None }
                    },
                )
            }
            _ => None,
        }
    }
}
pub enum NodeInfo<'hir> {
    Field(&'hir hir::StructField),
    VariantField(&'hir hir::VariantField),
    VariantCase(&'hir hir::VariantCase),
    Item(&'hir hir::Item),
}
pub type CtxtRef<'ctxt> = &'ctxt GlobalContext<'ctxt>;
pub struct GlobalContext<'hir> {
    hir: &'hir Hir,
    diag: &'hir DiagnosticReporter,
    nodes: IndexMap<DefId, NodeInfo<'hir>>,
    type_def_arena: Arena<TypeDef>,
}

impl<'hir> GlobalContext<'hir> {
    pub(crate) fn new(
        hir: &'hir Hir,
        diag: &'hir DiagnosticReporter,
        nodes: IndexMap<DefId, NodeInfo<'hir>>,
    ) -> Self {
        Self {
            hir,
            diag,
            nodes,
            type_def_arena: Arena::new(),
        }
    }
    pub fn diag<'a>(&'a self) -> &'a DiagnosticReporter
    where
        'a: 'hir,
    {
        self.diag
    }
    pub fn expect_item(&self, id: DefId) -> &hir::Item {
        &self.hir.items[&id]
    }
    pub fn signature_of(&'hir self, id: DefId) -> (Vec<Type>, Type) {
        match &self.expect_item(id).kind {
            hir::ItemKind::Function(function) => {
                let lower = TypeLower::new(self, true);
                let (params, return_ty) = lower.lower_function_sig(&function.sig);
                (params.collect(), return_ty)
            }
            hir::ItemKind::TypeDef(_) => (Vec::new(), Type::new_unit()),
        }
    }
    pub fn symbol(&self, def: Definition) -> Symbol {
        match def {
            Definition::Builtin(builtin) => builtin.as_symbol(),
            Definition::Def(id, _) => self.ident(id).symbol,
        }
    }
    pub fn ident(&self, id: DefId) -> Ident {
        match &self.expect_item(id).kind {
            hir::ItemKind::Function(function) => function.name,
            hir::ItemKind::TypeDef(ty) => ty.name,
        }
    }
    pub fn kind(&self, id: DefId) -> DefKind {
        self.hir.def_info[id].kind
    }
    pub fn get_parent(&self, id: DefId) -> Option<DefId> {
        self.hir.def_info[id].parent
    }
    pub fn type_def(&self, id: DefId) -> &TypeDef {
        let hir::ItemKind::TypeDef(type_def) = &self.expect_item(id).kind else {
            panic!("Expected type def for {:?}.", id)
        };
        self.type_def_arena.alloc(TypeDef {
            id,
            kind: match &type_def.kind {
                hir::TypeDefKind::Struct(struct_def) => TypeDefKind::Struct(TypeDefCase {
                    fields: struct_def
                        .fields
                        .iter()
                        .map(|field| FieldDef {
                            id: field.id,
                            name: field.name.symbol,
                        })
                        .collect(),
                }),
                hir::TypeDefKind::Variant(variant_def) => TypeDefKind::Variant(
                    variant_def
                        .cases
                        .iter()
                        .map(|case| {
                            (
                                case.id,
                                TypeDefCase {
                                    fields: case
                                        .fields
                                        .iter()
                                        .enumerate()
                                        .map(|(i, field)| FieldDef {
                                            id: field.id,
                                            name: Symbol::intern(&i.to_string()),
                                        })
                                        .collect(),
                                },
                            )
                        })
                        .collect(),
                ),
            },
        })
    }
    pub fn expect_parent(&self, id: DefId) -> DefId {
        self.get_parent(id)
            .unwrap_or_else(|| panic!("Expected a parent for {:?}.", id))
    }
    pub fn type_of_builtin(&self, builtin: Builtin) -> Type {
        let option_ty = || {
            Type::new_nominal_with_args(
                Definition::Builtin(Builtin::Option),
                [GenericArg(Type::Generic(Symbol::intern("T"), 0))],
            )
        };
        match builtin {
            Builtin::Println => Type::Err,
            Builtin::Option => option_ty(),
            Builtin::OptionNone => Type::new_function([], option_ty()),
            Builtin::OptionSome => {
                Type::new_function([Type::Generic(Symbol::intern("T"), 0)], option_ty())
            }
        }
    }
    pub fn type_of(&self, id: DefId) -> Type {
        match self.nodes[&id] {
            NodeInfo::Field(field) => TypeLower::new(self, false).lower(&field.ty),
            NodeInfo::VariantField(field) => TypeLower::new(self, false).lower(&field.ty),
            NodeInfo::VariantCase(_) => self.type_of(self.expect_parent(id)),
            NodeInfo::Item(item) => match &item.kind {
                hir::ItemKind::Function(function) => {
                    let lowerer = TypeLower::new(self, false);
                    let (params, return_ty) = lowerer.lower_function_sig(&function.sig);
                    Type::new_function(params, return_ty)
                }
                hir::ItemKind::TypeDef(..) => Type::new_nominal(Definition::Def(id, self.kind(id))),
            },
        }
    }
    pub fn expect_body_for(&self, id: DefId) -> &hir::Body {
        self.get_body_for(id)
            .expect("There should be a body for this item")
    }
    pub fn get_body_for(&self, id: DefId) -> Option<&hir::Body> {
        match self.expect_item(id).kind {
            hir::ItemKind::Function(ref function) => self.hir.bodies.get(&function.body_id),
            hir::ItemKind::TypeDef(_) => None,
        }
    }
}

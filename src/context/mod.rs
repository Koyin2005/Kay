use crate::{
    errors::DiagnosticReporter,
    frontend::{
        hir::{self, Builtin, DefId, DefKind, Definition, Hir},
        ty_lower::TypeLower,
    },
    indexvec::IndexVec,
    span::symbol::{Ident, Symbol},
    types::{FieldIndex, GenericArg, Type, TypeScheme, VariantCaseIndex},
};
use indexmap::IndexMap;
use typed_arena::Arena;

pub struct FieldDef {
    pub id: Definition,
    pub name: Symbol,
}
pub struct TypeDefCase {
    pub fields: IndexVec<FieldIndex, FieldDef>,
}
pub enum TypeDefKind {
    Variant(IndexVec<VariantCaseIndex, (Definition, TypeDefCase)>),
    Struct(TypeDefCase),
}
pub struct TypeDef {
    pub kind: TypeDefKind,
}
impl TypeDef {
    pub fn as_struct(&self) -> Option<&TypeDefCase> {
        match &self.kind {
            TypeDefKind::Struct(struct_case) => Some(struct_case),
            _ => None,
        }
    }
    pub fn case_with_def(&self, def: Definition) -> Option<&TypeDefCase> {
        match &self.kind {
            TypeDefKind::Variant(cases) => {
                cases.iter().find_map(
                    |(case, variant)| {
                        if def == *case { Some(variant) } else { None }
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
                let lower = TypeLower::new(self);
                let (params, return_ty) = lower.lower_function_sig(&function.sig);
                (params.collect(), return_ty)
            }
            hir::ItemKind::TypeDef(_) => (Vec::new(), Type::new_unit()),
            hir::ItemKind::Module(_) => (Vec::new(), Type::Err),
        }
    }
    pub fn symbol(&self, def: Definition) -> Symbol {
        match def {
            Definition::Builtin(builtin) => builtin.as_symbol(),
            Definition::Def(id) => self.ident(id).symbol,
        }
    }
    pub fn ident(&self, id: DefId) -> Ident {
        let item = self.expect_item(id);
        match &item.kind {
            hir::ItemKind::Function(function) => function.name,
            hir::ItemKind::TypeDef(ty) => ty.name,
            hir::ItemKind::Module(name) => Ident {
                symbol: *name,
                span: item.span,
            },
        }
    }
    pub fn kind(&self, id: DefId) -> DefKind {
        self.hir.def_info[id].kind
    }
    pub fn get_parent(&self, id: DefId) -> Option<DefId> {
        self.hir.def_info[id].parent
    }
    pub fn generic_arg_count(&self, def: Definition) -> u32 {
        match def {
            Definition::Builtin(builtin) => match builtin {
                Builtin::Option
                | Builtin::OptionNone
                | Builtin::OptionSome
                | Builtin::OptionSomeField => 1,
                Builtin::Println => 0,
            },
            //Todo get this to handle generic user defined types
            Definition::Def(_) => 0,
        }
    }
    pub fn type_def(&self, def: Definition) -> &TypeDef {
        let type_def = match def {
            Definition::Builtin(builtin) => match builtin {
                Builtin::Option => TypeDef {
                    kind: TypeDefKind::Variant(
                        [
                            (
                                Definition::Builtin(Builtin::OptionSome),
                                TypeDefCase {
                                    fields: [FieldDef {
                                        id: Definition::Builtin(Builtin::OptionSomeField),
                                        name: Symbol::intern("0"),
                                    }]
                                    .into_iter()
                                    .collect(),
                                },
                            ),
                            (
                                Definition::Builtin(Builtin::OptionNone),
                                TypeDefCase {
                                    fields: IndexVec::new(),
                                },
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                },
                _ => panic!("Invalid for type_def {}.", builtin.as_str()),
            },
            Definition::Def(id) => {
                let hir::ItemKind::TypeDef(type_def) = &self.expect_item(id).kind else {
                    panic!("Expected type def for {:?}.", id)
                };
                TypeDef {
                    kind: match &type_def.kind {
                        hir::TypeDefKind::Struct(struct_def) => TypeDefKind::Struct(TypeDefCase {
                            fields: struct_def
                                .fields
                                .iter()
                                .map(|field| FieldDef {
                                    id: Definition::Def(field.id),
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
                                        Definition::Def(case.id),
                                        TypeDefCase {
                                            fields: case
                                                .fields
                                                .iter()
                                                .flatten()
                                                .enumerate()
                                                .map(|(i, field)| FieldDef {
                                                    id: Definition::Def(field.id),
                                                    name: Symbol::intern(&i.to_string()),
                                                })
                                                .collect(),
                                        },
                                    )
                                })
                                .collect(),
                        ),
                    },
                }
            }
        };
        self.type_def_arena.alloc(type_def)
    }
    pub fn expect_parent(&self, id: DefId) -> DefId {
        self.get_parent(id)
            .unwrap_or_else(|| panic!("Expected a parent for {:?}.", id))
    }
    pub fn expect_parent_of_def(&self, def: Definition) -> Definition {
        match def {
            Definition::Builtin(builtin) => Definition::Builtin(builtin.expect_parent()),
            Definition::Def(id) => Definition::Def(
                self.get_parent(id)
                    .unwrap_or_else(|| panic!("Expected a parent for {:?}.", id)),
            ),
        }
    }
    pub fn type_of_builtin(&self, builtin: Builtin) -> TypeScheme {
        let t_param = || Type::Generic(Symbol::intern("T"), 0);
        let option_ty = || {
            Type::new_nominal_with_args(
                Definition::Builtin(Builtin::Option),
                [GenericArg(t_param())],
            )
        };
        match builtin {
            Builtin::Println => TypeScheme::new(Type::Err, 0),
            Builtin::Option => TypeScheme::new(option_ty(), 1),
            Builtin::OptionNone => TypeScheme::new(option_ty(), 1),
            Builtin::OptionSome => TypeScheme::new(Type::new_function([t_param()], option_ty()), 1),
            Builtin::OptionSomeField => TypeScheme::new(t_param(), 1),
        }
    }
    pub fn expect_node(&self, id: DefId) -> &NodeInfo<'_> {
        &self.nodes[&id]
    }
    pub fn expect_variant_case_node(&self, id: DefId) -> &hir::VariantCase {
        let NodeInfo::VariantCase(case) = self.expect_node(id) else {
            panic!("Expected a variant case")
        };
        case
    }
    pub fn type_of(&self, def: Definition) -> TypeScheme {
        let ty_lower = TypeLower::new(self);
        TypeScheme::new(
            match def {
                Definition::Builtin(builtin) => return self.type_of_builtin(builtin),
                Definition::Def(id) => match self.nodes[&id] {
                    NodeInfo::Field(field) => ty_lower.lower(&field.ty),
                    NodeInfo::VariantField(field) => ty_lower.lower(&field.ty),
                    NodeInfo::VariantCase(case) => {
                        let variant_ty = self
                            .type_of(self.expect_parent_of_def(case.id.into()))
                            .skip_instantiate();
                        if let Some(ref fields) = case.fields {
                            Type::new_function(
                                fields.iter().map(|field| ty_lower.lower(&field.ty)),
                                variant_ty,
                            )
                        } else {
                            variant_ty
                        }
                    }
                    NodeInfo::Item(item) => match &item.kind {
                        hir::ItemKind::Function(function) => {
                            let (params, return_ty) = ty_lower.lower_function_sig(&function.sig);
                            Type::new_function(params, return_ty)
                        }
                        hir::ItemKind::TypeDef(..) => Type::new_nominal(Definition::Def(id)),
                        hir::ItemKind::Module(..) => Type::Err,
                    },
                },
            },
            0,
        )
    }

    pub fn expect_body_for(&self, id: DefId) -> &hir::Body {
        self.get_body_for(id)
            .expect("There should be a body for this item")
    }
    pub fn get_body_for(&self, id: DefId) -> Option<&hir::Body> {
        match self.expect_item(id).kind {
            hir::ItemKind::Function(ref function) => self.hir.bodies.get(&function.body_id),
            hir::ItemKind::TypeDef(_) => None,
            hir::ItemKind::Module(_) => None,
        }
    }
}

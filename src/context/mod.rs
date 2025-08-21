use crate::{
    errors::DiagnosticReporter,
    frontend::{
        hir::{self, Builtin, DefId, DefKind, Definition, Hir},
        ty_lower::TypeLower,
    },
    indexvec::IndexVec,
    span::symbol::{Ident, Symbol},
    types::{self, FieldIndex, GenericArg, Type, TypeScheme, VariantCaseIndex},
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
pub struct Generics{
    pub owner : DefId,
    pub params : Vec<hir::GenericParam>
}
impl Generics{
    pub fn index_of(&self, param_name: Symbol) -> Option<u32>{
        self.params.iter().position(|param|{
            param.name.symbol == param_name
        }).map(|param| param.try_into().expect("Should have less than u32::MAX generic params."))
    }
    pub fn expect_index(&self, param_name: Symbol) -> u32{
        self.index_of(param_name).expect("This generic param should be here")
    }
    pub fn len(&self) -> u32{
        self.params.len().try_into().expect("Should have less than u32::MAX generic params.")
    }
    pub fn as_args(&self) -> types::GenericArgs{
        self.params.iter().enumerate().map(|(i,param)| GenericArg(Type::Generic(param.name.symbol, i.try_into().expect("Should have less than u32::MAX generic params.")))).collect()
    }
}
pub enum NodeInfo<'hir> {
    Field(&'hir hir::StructField),
    VariantField(&'hir hir::VariantField),
    VariantCase(&'hir hir::VariantCase),
    Item(&'hir hir::Item),
    GenericParam(&'hir hir::GenericParam)
}
pub type CtxtRef<'ctxt> = &'ctxt GlobalContext<'ctxt>;
pub struct GlobalContext<'hir> {
    hir: &'hir Hir,
    diag: &'hir DiagnosticReporter,
    nodes: IndexMap<DefId, NodeInfo<'hir>>,
    type_def_arena: Arena<TypeDef>,
    generics_arena : Arena<Generics>
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
            generics_arena : Arena::new(),
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
        match &self.nodes[&id] {
            NodeInfo::Field(field) => field.name,
            NodeInfo::VariantCase(case) => case.name,
            NodeInfo::VariantField(_) => unreachable!("This can't be accessed"),
            NodeInfo::Item(item) => match &item.kind {
                hir::ItemKind::Function(function) => function.name,
                hir::ItemKind::TypeDef(ty) => ty.name,
                hir::ItemKind::Module(name) => Ident {
                    symbol: *name,
                    span: item.span,
                },
            },
            NodeInfo::GenericParam(param) => param.name
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
            Definition::Def(def) => match self.kind(def) {
                DefKind::VariantCase => self.generic_arg_count(self.expect_parent_of_def(def.into())),
                DefKind::Field | DefKind::Module | DefKind::GenericParam  => 0,
                DefKind::Struct | DefKind::Function | DefKind::Variant => self.generics_for(def).len()
            }
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
    pub fn generics_for(&self, id: DefId) -> &Generics{
        let generics = match &self.expect_item(id).kind{
            hir::ItemKind::Function(function_def) => &function_def.generics,
            hir::ItemKind::Module(_) => {
                static GENERICS : &hir::Generics = &hir::Generics::empty();
                GENERICS
            },
            hir::ItemKind::TypeDef(type_def) => &type_def.generics
        };
        self.generics_arena.alloc(Generics { owner: id, params: generics.params.iter().map(|param|{
            *param
        }).collect() })
    }
    pub fn type_of(&self, def: Definition) -> TypeScheme {
        let ty_lower = TypeLower::new(self);
        match def {
                Definition::Builtin(builtin) => return self.type_of_builtin(builtin),
                Definition::Def(id) => match self.nodes[&id] {
                    NodeInfo::GenericParam(param) => {
                        let generics = self.generics_for(self.expect_parent(id));
                        let param_index = generics.expect_index(param.name.symbol);
                        TypeScheme::new(Type::Generic(param.name.symbol,param_index),0)
                    },
                    NodeInfo::Field(field) => TypeScheme::new(ty_lower.lower(&field.ty),0),
                    NodeInfo::VariantField(field) => TypeScheme::new(ty_lower.lower(&field.ty),0),
                    NodeInfo::VariantCase(case) => {
                        let parent_id = self.expect_parent(case.id);
                        let generics = self.generics_for(parent_id);
                        let variant_ty = self
                            .type_of(parent_id.into())
                            .skip_instantiate();
                        if let Some(ref fields) = case.fields {
                            TypeScheme::new(Type::new_function(
                                fields.iter().map(|field| ty_lower.lower(&field.ty)),
                                variant_ty,
                            ),generics.len())
                        } else {
                            TypeScheme::new(variant_ty, generics.len())
                        }
                    }
                    NodeInfo::Item(item) => {
                        let generics = self.generics_for(item.id);
                        let ty = match &item.kind {
                            hir::ItemKind::Function(function) => {
                                let (params, return_ty) = ty_lower.lower_function_sig(&function.sig);
                                Type::new_function(params, return_ty)
                            }
                            hir::ItemKind::TypeDef(..) => Type::new_nominal_with_args(Definition::Def(id),self.generics_for(id).as_args()),
                            hir::ItemKind::Module(..) => Type::Err,
                        };
                        TypeScheme::new(ty, generics.len())
                    },
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
            hir::ItemKind::Module(_) => None,
        }
    }
}

use std::cell::RefCell;

use crate::{
    diagnostics::DiagnosticReporter,
    frontend::{
        hir::{self, DefId, DefKind, Definition, Hir},
        ty_lower::TypeLower,
    },
    indexvec::IndexVec,
    span::symbol::{Ident, Symbol},
    types::{
        self, FieldIndex, GenericArg, GenericArgs, Region, Type, TypeScheme, VariantCaseIndex,
    },
};
use fxhash::{FxHashMap, FxHashSet};
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
    id: DefId,
    pub kind: TypeDefKind,
}
impl TypeDef {
    pub fn index_of_case_with_id(&self, id: DefId) -> VariantCaseIndex {
        match &self.kind {
            TypeDefKind::Struct(_) if self.id == id => VariantCaseIndex::new(0),
            TypeDefKind::Variant(cases) => cases
                .iter()
                .position(|(Definition::Def(case_id), _)| *case_id == id)
                .map(VariantCaseIndex::new)
                .unwrap_or_else(|| unreachable!("Can only have valid indices")),
            _ => unreachable!("Can only have valid ids"),
        }
    }
    pub fn case_count(&self) -> usize {
        match &self.kind {
            TypeDefKind::Struct(_) => 1,
            TypeDefKind::Variant(cases) => cases.len(),
        }
    }
    pub fn cases(&self) -> IndexVec<VariantCaseIndex, (Definition, &TypeDefCase)> {
        match &self.kind {
            TypeDefKind::Struct(struct_case) => [(Definition::Def(self.id), struct_case)]
                .into_iter()
                .collect(),
            TypeDefKind::Variant(cases) => cases.iter().map(|(def, case)| (*def, case)).collect(),
        }
    }
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
#[derive(Clone, Copy)]
pub struct GenericParamDef {
    pub name: Symbol,
    pub index: u32,
    pub id: DefId,
    pub kind: hir::GenericParamKind,
}
pub struct Generics {
    pub owner: Definition,
    pub params: Vec<GenericParamDef>,
}
impl Generics {
    pub fn index_of(&self, param_name: Symbol) -> Option<u32> {
        self.params
            .iter()
            .position(|param| param.name == param_name)
            .map(|param| {
                param
                    .try_into()
                    .expect("Should have less than u32::MAX generic params.")
            })
    }
    pub fn expect_index(&self, param_name: Symbol) -> u32 {
        self.index_of(param_name)
            .expect("This generic param should be here")
    }
    pub fn count(&self) -> u32 {
        self.params
            .len()
            .try_into()
            .expect("Should have less than u32::MAX generic params.")
    }
    pub fn params(&self) -> impl Iterator<Item = &GenericParamDef> {
        self.params.iter()
    }
    pub fn as_args(&self) -> types::GenericArgs {
        self.params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let index = i
                    .try_into()
                    .expect("Should have less than u32::MAX generic params.");
                match param.kind {
                    hir::GenericParamKind::Region => {
                        GenericArg::Region(Region::Generic(param.name, index))
                    }
                    hir::GenericParamKind::Type => {
                        GenericArg::Type(Type::Generic(param.name, index))
                    }
                }
            })
            .collect()
    }
}
pub enum NodeInfo<'hir> {
    Field(&'hir hir::StructField),
    VariantField(&'hir hir::VariantField),
    VariantCase(&'hir hir::VariantCase),
    Item(&'hir hir::Item),
    GenericParam(&'hir hir::GenericParam),
}
pub type CtxtRef<'ctxt> = &'ctxt GlobalContext<'ctxt>;
pub struct GlobalContext<'hir> {
    hir: &'hir Hir,
    diag: &'hir DiagnosticReporter,
    nodes: IndexMap<DefId, NodeInfo<'hir>>,
    type_def_arena: Arena<TypeDef>,
    generics_arena: Arena<Generics>,
    is_infifnite: RefCell<FxHashMap<DefId, bool>>,
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
            generics_arena: Arena::new(),
            is_infifnite: FxHashMap::default().into(),
        }
    }
    pub fn diag<'a>(&'a self) -> &'a DiagnosticReporter
    where
        'a: 'hir,
    {
        self.diag
    }
    pub fn expect_sig_of(&self, id: DefId) -> &hir::FunctionSig {
        let hir::ItemKind::Function(function_def) = &self.expect_item(id).kind else {
            unreachable!("Expected a function for item")
        };
        &function_def.sig
    }
    pub fn expect_item(&self, id: DefId) -> &hir::Item {
        &self.hir.items[&id]
    }
    pub fn signature_of(&'hir self, id: DefId) -> (Vec<Type>, Type) {
        match &self.expect_item(id).kind {
            hir::ItemKind::Function(function) => {
                let lower = TypeLower::new(self, None, None);
                let (params, return_ty) = lower.lower_function_sig(&function.sig);
                (params.collect(), return_ty)
            }
            hir::ItemKind::TypeDef(_) => (Vec::new(), Type::new_unit()),
            hir::ItemKind::Module(_) => (Vec::new(), Type::Err),
        }
    }
    pub fn symbol(&self, def: Definition) -> Symbol {
        match def {
            Definition::Def(id) => self.ident(id).symbol,
        }
    }
    pub fn ident(&self, id: DefId) -> Ident {
        match &self.nodes[&id] {
            NodeInfo::Field(field) => field.name,
            NodeInfo::VariantCase(case) => case.name,
            NodeInfo::VariantField(_) => unreachable!("Variant fields can't have idents"),
            NodeInfo::Item(item) => match &item.kind {
                hir::ItemKind::Function(function) => function.name,
                hir::ItemKind::TypeDef(ty) => ty.name,
                hir::ItemKind::Module(name) => Ident {
                    symbol: *name,
                    span: item.span,
                },
            },
            NodeInfo::GenericParam(param) => param.name,
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
            Definition::Def(def) => match self.kind(def) {
                DefKind::VariantCase => {
                    self.generic_arg_count(self.expect_parent_of_def(def.into()))
                }
                DefKind::Field | DefKind::Module | DefKind::TypeParam | DefKind::RegionParam => 0,
                DefKind::Struct | DefKind::Function | DefKind::Variant => {
                    self.generics_for(def.into()).count()
                }
            },
        }
    }
    pub fn type_def(&self, def: Definition) -> &TypeDef {
        let type_def = match def {
            Definition::Def(id) => {
                let hir::ItemKind::TypeDef(type_def) = &self.expect_item(id).kind else {
                    panic!("Expected type def for {:?}.", id)
                };
                TypeDef {
                    id,
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
            Definition::Def(id) => Definition::Def(
                self.get_parent(id)
                    .unwrap_or_else(|| panic!("Expected a parent for {:?}.", id)),
            ),
        }
    }
    pub fn expect_node(&self, id: DefId) -> &NodeInfo<'_> {
        &self.nodes[&id]
    }
    pub fn expect_module(&self, id: DefId) -> Symbol{
        let item = self.expect_item(id);
        let hir::ItemKind::Module(module_name) = item.kind else {
            panic!("Expected a module")
        };
        module_name
    }
    pub fn expect_variant_case_node(&self, id: DefId) -> &hir::VariantCase {
        let NodeInfo::VariantCase(case) = self.expect_node(id) else {
            panic!("Expected a variant case")
        };
        case
    }
    pub fn generics_for(&self, def: Definition) -> &Generics {
        let generics = match def {
            Definition::Def(id) => match self.nodes[&id] {
                NodeInfo::Item(item) => match &item.kind {
                    hir::ItemKind::Function(function) => &function.generics.params,
                    hir::ItemKind::Module(_) => &Vec::new(),
                    hir::ItemKind::TypeDef(type_def) => &type_def.generics.params,
                },
                NodeInfo::Field(_) => &Vec::new(),
                NodeInfo::GenericParam(_) => &Vec::new(),
                NodeInfo::VariantCase(case) => {
                    return self.generics_for(self.expect_parent_of_def(case.id.into()));
                }
                NodeInfo::VariantField(_) => &Vec::new(),
            },
        };
        let params = generics
            .iter()
            .enumerate()
            .map(|(index, param)| GenericParamDef {
                name: param.name.symbol,
                index: index
                    .try_into()
                    .expect("Should have less than u32::MAX generic params"),
                id: param.def_id,
                kind: param.kind,
            })
            .collect();
        self.generics_arena.alloc(Generics { owner: def, params })
    }
    pub fn type_of(&self, def: Definition) -> TypeScheme {
        let ty_lower = TypeLower::new(self, None, None);
        match def {
            Definition::Def(id) => match self.nodes[&id] {
                NodeInfo::GenericParam(param) => {
                    let generics = self.generics_for(self.expect_parent(id).into());
                    let param_index = generics.expect_index(param.name.symbol);
                    TypeScheme::new(Type::Generic(param.name.symbol, param_index), 0)
                }
                NodeInfo::Field(field) => TypeScheme::new(ty_lower.lower(&field.ty), 0),
                NodeInfo::VariantField(field) => TypeScheme::new(ty_lower.lower(&field.ty), 0),
                NodeInfo::VariantCase(case) => {
                    let parent_id = self.expect_parent(case.id);
                    let generics = self.generics_for(parent_id.into());
                    let variant_ty = self.type_of(parent_id.into()).skip_instantiate();
                    if let Some(ref fields) = case.fields {
                        TypeScheme::new(
                            Type::new_function(
                                fields.iter().map(|field| ty_lower.lower(&field.ty)),
                                variant_ty,
                            ),
                            generics.count(),
                        )
                    } else {
                        TypeScheme::new(variant_ty, generics.count())
                    }
                }
                NodeInfo::Item(item) => {
                    let generics = self.generics_for(item.id.into());
                    let ty = match &item.kind {
                        hir::ItemKind::Function(function) => {
                            let (params, return_ty) = ty_lower.lower_function_sig(&function.sig);
                            Type::new_function(params, return_ty)
                        }
                        hir::ItemKind::TypeDef(..) => Type::new_nominal_with_args(
                            Definition::Def(id),
                            self.generics_for(id.into()).as_args(),
                        ),
                        hir::ItemKind::Module(..) => Type::Err,
                    };
                    TypeScheme::new(ty, generics.count())
                }
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
    pub fn is_infinite(&self, id: DefId) -> bool {
        if let Some(infinite) = self.is_infifnite.borrow().get(&id) {
            return *infinite;
        }
        fn is_infinite_type(ctxt: &GlobalContext, ty: &Type, seen: &mut FxHashSet<DefId>) -> bool {
            match ty {
                Type::Err
                | Type::Primitive(_)
                | Type::Generic(..)
                | Type::Function(..)
                | Type::Ref(..) => false,
                Type::Tuple(fields) => fields
                    .iter()
                    .any(|field| is_infinite_type(ctxt, field, seen)),
                Type::Array(ty) => is_infinite_type(ctxt, ty, seen),
                &Type::Nominal(Definition::Def(type_id), ref generic_args) => {
                    is_infinite_type_def(ctxt, type_id, generic_args, seen)
                }
                Type::Infer(_) => {
                    unreachable!("Can't determine whether inferred type '_' is recursive.")
                }
            }
        }
        fn is_infinite_type_def(
            ctxt: &GlobalContext,
            type_def_id: DefId,
            args: &GenericArgs,
            seen: &mut FxHashSet<DefId>,
        ) -> bool {
            if !seen.insert(type_def_id) {
                return true;
            }
            match &ctxt.type_def(type_def_id.into()).kind {
                TypeDefKind::Struct(case) => case.fields.iter().any(|field| {
                    let ty = ctxt.type_of(field.id).instantiate(args.clone());
                    is_infinite_type(ctxt, &ty, seen)
                }),
                TypeDefKind::Variant(variants) => variants.iter().any(|&(_, ref case)| {
                    case.fields.iter().any(|field| {
                        let ty = ctxt.type_of(field.id).instantiate(args.clone());
                        is_infinite_type(ctxt, &ty, seen)
                    })
                }),
            }
        }
        let is_infinite = match self.expect_node(id) {
            NodeInfo::Field(field) => is_infinite_type(
                self,
                &self.type_of(field.id.into()).skip_instantiate(),
                &mut FxHashSet::default(),
            ),
            NodeInfo::Item(item) => match item.kind {
                hir::ItemKind::TypeDef(ref ty) => is_infinite_type_def(
                    self,
                    ty.id,
                    &self.generics_for(ty.id.into()).as_args(),
                    &mut FxHashSet::default(),
                ),
                _ => unreachable!("Can't use on non-type def or field"),
            },
            _ => unreachable!("Can't use on non-type def or field"),
        };
        self.is_infifnite.borrow_mut().insert(id, is_infinite);
        is_infinite
    }
    pub fn is_inhabited(&self, ty: &Type) -> bool {
        match ty {
            Type::Primitive(hir::PrimitiveType::Never) => false,
            Type::Primitive(
                hir::PrimitiveType::Int(_) | hir::PrimitiveType::Bool | hir::PrimitiveType::String,
            ) => true,
            Type::Function(_, _) | Type::Generic(_, _) | Type::Infer(_) | Type::Err => true,
            &Type::Nominal(Definition::Def(id), ref args) => {
                if self.is_infinite(id) {
                    return true;
                }
                match &self.type_def(id.into()).kind {
                    TypeDefKind::Struct(case) => case.fields.iter().all(|field| {
                        self.is_inhabited(&self.type_of(field.id).instantiate(args.clone()))
                    }),
                    TypeDefKind::Variant(cases) => cases.iter().any(|(_, case)| {
                        case.fields.iter().all(|field| {
                            self.is_inhabited(&self.type_of(field.id).instantiate(args.clone()))
                        })
                    }),
                }
            }
            Type::Tuple(fields) => fields.iter().all(|field| self.is_inhabited(field)),
            Type::Ref(ty, _, _) => self.is_inhabited(ty),
            Type::Array(element_ty) => self.is_inhabited(element_ty),
        }
    }
    pub fn debug_name(&self, id: DefId) -> String {
        let mut name = if let Some(parent) = self.get_parent(id) {
            let mut name = self.debug_name(parent);
            name.push('.');
            name
        } else {
            String::with_capacity(2)
        };
        name.push_str(self.ident(id).symbol.as_str());
        name
    }
    pub fn root_module_of(&self, mut id: DefId) -> DefId{
        let mut old_parent = None;
        while let Some(curr) = self.get_parent(id) {
            old_parent = Some(curr);
            id = curr;
        }
        old_parent.expect("This should be called on a non-root module")
    }
}

use fxhash::FxHashMap;

use crate::{
    frontend::hir::{DefId, HirId, Resolution},
    types::{FieldIndex, GenericArgs, Type},
};

pub mod function;
pub mod well_formed;
#[derive(PartialEq, Eq, Clone, Copy)]
enum LocalSource {
    Param,
    Let,
}
#[derive(Debug)]
pub enum Coercion {
    NeverToAny(Type),
}
#[derive(Debug)]
pub struct TypeCheckResults {
    owner: DefId,
    types: FxHashMap<HirId, Type>,
    coercions: FxHashMap<HirId, Coercion>,
    resolutions: FxHashMap<HirId, Resolution>,
    generic_args: FxHashMap<HirId, GenericArgs>,
    fields: FxHashMap<HirId, FieldIndex>,
    had_error: bool,
}
impl TypeCheckResults {
    pub fn had_error(&self) -> bool {
        self.had_error
    }
    pub fn owner(&self) -> DefId {
        self.owner
    }
    pub fn type_of(&self, id: HirId) -> Type {
        self.types.get(&id).expect("Expected an id").clone()
    }
    pub fn expect_field(&self, id: HirId) -> FieldIndex {
        self.fields.get(&id).copied().expect("Expected a field")
    }
    pub fn get_coercion(&self, id: HirId) -> Option<&Coercion> {
        self.coercions.get(&id)
    }
    pub fn get_res(&self, id: HirId) -> Option<Resolution> {
        self.resolutions.get(&id).copied()
    }
    pub fn get_generic_args_or_empty(&self, id: HirId) -> &GenericArgs {
        static EMPTY: GenericArgs = GenericArgs::empty();
        self.generic_args.get(&id).unwrap_or(&EMPTY)
    }
    pub fn get_generic_args(&self, id: HirId) -> Option<&GenericArgs> {
        self.generic_args.get(&id)
    }
}

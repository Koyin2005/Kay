use fxhash::FxHashMap;

use crate::{
    frontend::hir::{DefId, HirId, Resolution},
    types::{FieldIndex, GenericArgs, Origin, Type},
};
mod coercion;
pub mod function;
pub mod results;
pub mod well_formed;
#[derive(PartialEq, Eq, Clone, Copy)]
enum LocalSource {
    Param,
    Let,
}
#[derive(Debug)]
pub enum Coercion {
    NeverToAny(Type),
    RefCoercion(Box<Origin>),
}

use crate::{
    frontend::hir::{Builtin, DefId},
    span::symbol::{Symbol, symbols},
};

#[derive(Debug)]
pub struct Builtins {
    ids: [Option<DefId>; Builtin::COUNT],
}
impl Builtins {
    pub fn new() -> Self {
        Self {
            ids: [None; Builtin::COUNT],
        }
    }
    pub fn get_item(&self, builtin: Builtin) -> Option<DefId> {
        self.ids[builtin as usize]
    }
    pub fn set_item(&mut self, builtin: Builtin, id: DefId) {
        self.ids[builtin as usize] = Some(id);
    }
}
pub fn builtin_from_name(name: Symbol) -> Option<Builtin> {
    match name {
        symbols::PANIC => Some(Builtin::Panic),
        symbols::LEN => Some(Builtin::Len),
        symbols::OPTION => Some(Builtin::Option),
        symbols::SOME => Some(Builtin::Some),
        symbols::NONE => Some(Builtin::None),
        symbols::PRINTLN => Some(Builtin::Println),
        _ => None,
    }
}

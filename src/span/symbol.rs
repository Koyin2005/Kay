use std::{
    fmt::Debug,
    sync::{LazyLock, Mutex},
};

use indexmap::IndexSet;
use typed_arena::Arena;

use crate::span::Span;

struct SymbolInterner(Mutex<SymbolInternerInner>);

struct SymbolInternerInner {
    arena: Arena<u8>,
    seen: IndexSet<&'static [u8]>,
}

impl SymbolInterner {
    fn new() -> Self {
        let seen = {
            let mut seen = IndexSet::default();
            for value in ALL_SYMBOLS {
                seen.insert_full(value.as_bytes());
            }
            seen
        };
        Self(Mutex::new(SymbolInternerInner {
            arena: Arena::new(),
            seen,
        }))
    }
    fn intern_str(&self, string: &str) -> u32 {
        self.intern(string.as_bytes())
    }

    fn get_str(&self, symbol: Symbol) -> &str {
        //SAFETY : A symbol can only be produced from
        // a valid UTF-8 string.
        unsafe { str::from_utf8_unchecked(self.get(symbol.0)) }
    }
    fn intern(&self, bytes: &[u8]) -> u32 {
        let mut inner = self.0.lock().unwrap();
        if let Some(index) = inner.seen.get_index_of(&bytes) {
            return index as u32;
        }
        // SAFETY: we can extend the arena allocation to `'static` because we
        // only access these while the arena is still alive.
        let bytes = &*inner.arena.alloc_extend(bytes.iter().copied());
        let bytes: &'static [u8] = unsafe { &*(bytes as *const [u8]) };

        let (index, _) = inner.seen.insert_full(bytes);
        index
            .try_into()
            .expect("There should be less than u32::MAX strings interned")
    }
    fn get(&self, index: u32) -> &[u8] {
        let inner = self.0.lock().unwrap();
        inner
            .seen
            .get_index(index as usize)
            .expect("There should be an index still")
    }
}

static SYMBOL_INTERNER: LazyLock<SymbolInterner> = LazyLock::new(SymbolInterner::new);
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Symbol {
    const fn new(index: u32) -> Self {
        Self(index)
    }
    pub fn intern(string: &str) -> Self {
        Self::new(SYMBOL_INTERNER.intern_str(string))
    }
    pub fn as_str(&self) -> &str {
        //SAFETY : Symbol interner will outlive the string, so reducing its lifetime is fine
        unsafe { std::mem::transmute(SYMBOL_INTERNER.get_str(*self)) }
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
#[derive(Clone, Copy, Debug)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

const ALL_SYMBOLS: &[&str] = &["main", "0", "iter", "println"];

pub mod symbols {
    use crate::span::symbol::{ALL_SYMBOLS, Symbol};

    const fn find_symbol_index(txt: &'static str) -> u32 {
        let mut i = 0;
        let txt_bytes = txt.as_bytes();
        while i < ALL_SYMBOLS.len() {
            let curr = ALL_SYMBOLS[i];
            let curr_bytes = curr.as_bytes();
            let is_equal = if curr.len() == txt.len() {
                let mut j = 0;
                let mut is_equal = true;
                while j < curr.len() {
                    if curr_bytes[j] != txt_bytes[j] {
                        is_equal = false;
                        break;
                    }
                    j += 1;
                }
                is_equal
            } else {
                false
            };
            if is_equal {
                return i as u32;
            }
            i += 1;
        }
        panic!("Found an unknown symbol.")
    }
    pub const ITER: Symbol = Symbol::new(find_symbol_index("iter"));
    pub const PRINTLN: Symbol = Symbol::new(find_symbol_index("println"));
}

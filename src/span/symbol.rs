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
    fn intern_str(&self, string: &str) -> Symbol {
        Symbol(self.intern(string.as_bytes()))
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
        SYMBOL_INTERNER.intern_str(string)
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
#[derive(Clone, Copy)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

macro_rules! symbols {

    (keywords{ $($const_kw_name:ident, $kw_name:ident = $kw_value:expr)*} $($const_name:ident, $name:ident = $value:expr)*) => {
        enum Sym{
            $($kw_name,)*
            $($name,)*
        }
        pub mod keywords{
            use crate::span::symbol::Symbol;
            use super::Sym;
            $(pub const $const_kw_name : Symbol = Symbol::new(Sym::$kw_name as u32);)*
        }
        $(pub const $const_name : Symbol = Symbol::new(Sym::$name as u32);)*


        pub const ALL_SYMBOLS : &'static [&'static str] = &[ $($kw_value,)* $($value,)* ];
    };
}

symbols! {
    keywords{
        IF, If = "if"
        ELSE, Else = "else"
        THEN, Then  = "then"
        END, End = "end"
        BEGIN, Begin = "begin"
        WHILE, While = "while"
        FOR, For = "for"
        DO, Do = "do"
        TRUE, True = "true"
        FALSE, False = "false"
        PRINT, Print = "print"
        FUN, Fun = "fun"
    }



}

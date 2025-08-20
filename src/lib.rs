pub mod config;
pub mod context;
pub mod errors;
mod frontend;
pub(crate) mod indexvec;
pub(crate) mod span;
pub(crate) mod types;

pub use frontend::parsing::{lexing::Lexer, parser::Parser};
pub use frontend::{
    ast_lowering::AstLower, item_collect::ItemCollect, resolution::resolve::Resolver,
    typecheck::TypeCheck,
};
pub use span::SourceFiles;
pub use span::SourceInfo;

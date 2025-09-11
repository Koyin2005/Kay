pub mod config;
pub mod context;
pub mod diagnostics;
mod frontend;
pub(crate) mod indexvec;
pub(crate) mod span;
pub(crate) mod types;

pub use frontend::ast::Ast;
pub use frontend::ast::NodeId;
pub use frontend::parsing::{lexing::Lexer, parser::Parser};
pub use frontend::{
    ast_lowering::AstLower, item_collect::ItemCollect, resolution::resolve::Resolver,
    typecheck::TypeCheck,
};
pub use span::SourceFiles;
pub use span::SourceInfo;
pub use frontend::thir_build::ThirBuild;

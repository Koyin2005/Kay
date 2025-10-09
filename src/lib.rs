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
pub use frontend::thir_build::ThirBuild;
pub use frontend::{
    ast_lowering::AstLower, borrow_check::function::BorrowCheck, item_collect::ItemCollect,
    pattern_check::PatCheck, resolution::resolve::Resolver, typecheck::function::TypeCheck,
};
pub use span::SourceFiles;
pub use span::SourceInfo;

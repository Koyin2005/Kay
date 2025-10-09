use std::rc::Rc;

use pl5::{
    Ast, AstLower, BorrowCheck, ItemCollect, Lexer, NodeId, Parser, PatCheck, Resolver,
    SourceFiles, ThirBuild, TypeCheck,
    config::{Config, ConfigError, SourceError},
    diagnostics::DiagnosticReporter,
};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let config = match Config::new(&args) {
        Ok(config) => config,
        Err(error) => match error {
            ConfigError::ExpectedArgs { expected, got } => {
                eprintln!(
                    "Expected {} command line arg{}got {}.",
                    expected,
                    if expected == 1 { " " } else { "s " },
                    got
                );
                return;
            }
            ConfigError::FileDoesNotExist(name) => {
                eprintln!("'{name}' does not exist.");
                return;
            }
            ConfigError::InvalidFile => {
                eprintln!("Invalid file.");
                return;
            }
        },
    };
    let source_files = match config.get_all_source_files() {
        Ok(source_files) => source_files,
        Err(error) => match error {
            SourceError::ReadFileFailed(error) => {
                eprintln!("Error occured : {}", error);
                return;
            }
        },
    };
    let source_files = match SourceFiles::new(source_files) {
        Ok(source_files) => Rc::new(source_files),
        Err(_) => {
            eprintln!("A file was too large");
            return;
        }
    };
    let mut next_id = NodeId::FIRST;
    let modules = source_files
        .get_source_files()
        .iter()
        .enumerate()
        .filter_map(|(file_index, source_file)| {
            let file_index = file_index.try_into().ok()?;
            let lexer = Lexer::new(source_file, file_index);
            let parse_diagnostics = DiagnosticReporter::new(source_files.clone());
            let parser = Parser::new(source_file.name(), lexer, parse_diagnostics, next_id);
            parser.parse().ok().inspect(|module| {
                next_id = module.id.plus(1);
            })
        });
    let ast = Ast {
        modules: modules.collect(),
    };
    let name_res_diagnostics = DiagnosticReporter::new(source_files.clone());
    let results = Resolver::new(&name_res_diagnostics).resolve(&ast);
    let ast_lower_diagnostics = DiagnosticReporter::new(source_files.clone());
    let hir = AstLower::new(results, &ast_lower_diagnostics).lower_ast(&ast);
    let global_diagnostics = DiagnosticReporter::new(source_files.clone());
    let context = ItemCollect::new(&global_diagnostics).collect(&hir);
    let context_ref = &context;
    let type_check_results = hir.items.iter().filter_map(|(_, item)| {
        let Some(typeck) = TypeCheck::new(context_ref, item.id) else {
            return None;
        };
        Some(typeck.check())
    });
    if global_diagnostics.had_error() {
        global_diagnostics.emit();
        return;
    }
    let mut thir_build = ThirBuild::new(context_ref);
    for results in type_check_results {
        let body = context.expect_body_for(results.owner());
        thir_build.build(results.owner(), body, results);
    }
    let mut thir = thir_build.finish();
    for body in thir.bodies.iter_mut() {
        BorrowCheck::new(&body, context_ref).check();
        PatCheck::new(body, context_ref).check();
    }
    global_diagnostics.emit();
}

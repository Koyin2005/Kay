use pl5::{
    AstLower, ItemCollect, Lexer, Parser, Resolver, SourceInfo, TypeCheck,
    config::{Config, ConfigError},
    errors::DiagnosticReporter,
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
            ConfigError::InvalidFile => {
                eprintln!("Invalid file.");
                return;
            }
        },
    };
    let source = match std::fs::read_to_string(config.path_str()) {
        Ok(path) => path,
        Err(error) => {
            match error.kind() {
                std::io::ErrorKind::NotFound => {
                    eprintln!("No file or path at {}.", config.path_str())
                }
                _ => eprintln!("An error occurred : {error}."),
            }
            return;
        }
    };
    let source_file = match SourceInfo::new(source) {
        Ok(source_info) => source_info,
        Err(_) => {
            eprintln!("The file was too large");
            return;
        }
    };
    let lexer = Lexer::new(&source_file);
    let parse_diagnostics = DiagnosticReporter::new(&source_file);
    let parser = Parser::new(lexer, parse_diagnostics);
    let Ok(ast) = parser.parse() else {
        return;
    };
    let name_res_diagnostics = DiagnosticReporter::new(&source_file);
    let results = Resolver::new(&name_res_diagnostics).resolve(&ast);

    let ast_lower_diagnostics = DiagnosticReporter::new(&source_file);
    let hir = AstLower::new(results, &ast_lower_diagnostics).lower_ast(&ast);

    let global_diagnostics = DiagnosticReporter::new(&source_file);
    let context = ItemCollect::new(&global_diagnostics).collect(&hir);
    let context_ref = &context;
    for (_, item) in hir.items.iter() {
        let Some(typeck) = TypeCheck::new(context_ref, item.id) else {
            continue;
        };
        typeck.check();
    }
    global_diagnostics.emit();
}

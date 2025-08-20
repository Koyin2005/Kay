use pl5::{
    AstLower, ItemCollect, Lexer, Parser, Resolver, SourceFiles, TypeCheck,
    config::{Config, ConfigError, KAE_EXTENSION, PathKind},
    errors::DiagnosticReporter,
};

enum SourceError {
    ReadFileFailed,
    InvalidFile(String),
}
fn get_source(config: &Config) -> Result<Box<[String]>, SourceError> {
    fn find_files_with_kae_extension(path: &str) -> Result<Box<[String]>, SourceError> {
        let dir = std::fs::read_dir(path).expect("Already checked its a directory");
        dir.filter_map(|entry| entry.ok())
            .map(|entry| {
                let path = entry.path();
                if path
                    .extension()
                    .is_some_and(|ext| ext.to_string_lossy().ends_with(KAE_EXTENSION))
                {
                    read_file_source(&entry.path().to_string_lossy())
                } else {
                    Err(SourceError::InvalidFile(
                        path.to_string_lossy().into_owned(),
                    ))
                }
            })
            .collect()
    }

    fn read_file_source(path: &str) -> Result<String, SourceError> {
        match std::fs::read_to_string(path) {
            Ok(source) => Ok(source),
            Err(error) => {
                match error.kind() {
                    std::io::ErrorKind::NotFound => {
                        eprintln!("No file or path at {}.", path)
                    }
                    _ => eprintln!("An error occurred : {error}."),
                }
                Err(SourceError::ReadFileFailed)
            }
        }
    }

    let source_files = match config.path_kind() {
        PathKind::Folder => find_files_with_kae_extension(config.path_str())?,
        PathKind::Source => {
            let source = read_file_source(config.path_str())?;
            Box::new([source])
        }
    };
    Ok(source_files)
}
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
    let source_files = match get_source(&config) {
        Ok(source_files) => source_files,
        Err(error) => match error {
            SourceError::ReadFileFailed => return,
            SourceError::InvalidFile(file) => {
                eprintln!("Invalid file '{}'.", file);
                return;
            }
        },
    };
    let source_files = match SourceFiles::new(source_files) {
        Ok(source_files) => source_files,
        Err(_) => {
            eprintln!("A file was too large");
            return;
        }
    };
    let Some(source_ref) = source_files.get_source_files().first().cloned() else {
        return;
    };
    let lexer = Lexer::new(&source_ref);
    let parse_diagnostics = DiagnosticReporter::new(source_ref.clone());
    let parser = Parser::new(lexer, parse_diagnostics);
    let Ok(ast) = parser.parse() else {
        return;
    };
    let name_res_diagnostics = DiagnosticReporter::new(source_ref.clone());
    let results = Resolver::new(&name_res_diagnostics).resolve(&ast);

    let ast_lower_diagnostics = DiagnosticReporter::new(source_ref.clone());
    let hir = AstLower::new(results, &ast_lower_diagnostics).lower_ast(&ast);

    let global_diagnostics = DiagnosticReporter::new(source_ref.clone());
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

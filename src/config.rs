use std::path::{Path, PathBuf};
pub const KAE_EXTENSION: &str = "k";
pub const KAE_EXTENSION_WITH_DOT: &str = ".k";
pub enum ConfigError {
    NoArgs,
    NoFileArg,
    FileDoesNotExist(String),
    InvalidFile,
}
pub enum SourceError {
    ReadFileFailed(std::io::Error),
}
#[derive(Debug)]
struct FilePath {
    path: PathBuf,
    name: String,
}
pub struct SourceFile {
    pub name: String,
    pub data: String,
}
enum PathKind {
    Folder(Vec<FilePath>),
}
fn try_as_file_path(path: &Path) -> Option<FilePath> {
    let file_name = path
        .file_name()
        .and_then(|name| name.to_str())
        .and_then(|name| name.split_terminator(KAE_EXTENSION_WITH_DOT).next())?;
    if file_name.char_indices().all(|(i, c)| {
        if i == 0 {
            c == '_' || c.is_ascii_alphabetic()
        } else {
            c.is_ascii_alphanumeric() || c == '_'
        }
    }) {
        Some(FilePath {
            path: path.into(),
            name: file_name.to_string(),
        })
    } else {
        None
    }
}
pub struct Config {
    kind: PathKind,
    emit_mir_file_names: Vec<String>,
}
impl Config {
    pub fn new(args: &[String]) -> Result<Self, ConfigError> {
        let [_, args @ ..] = args else {
            return Err(ConfigError::NoArgs);
        };
        let [file_name, rest @ ..] = args else {
            return Err(ConfigError::NoFileArg);
        };
        let path = Path::new(file_name);
        if !path.exists() {
            return Err(ConfigError::FileDoesNotExist(file_name.to_string()));
        }
        let emit_mir_flag_index = rest
            .iter()
            .enumerate()
            .find_map(|(index, name)| (name == "--emit_mir").then_some(index));
        let mir_file_names = if let Some(emit_mir_flag_index) = emit_mir_flag_index {
            rest[emit_mir_flag_index + 1..]
                .iter()
                .map(|name| name.strip_prefix('-'))
                .take_while(Option::is_some)
                .filter_map(std::convert::identity)
                .map(str::to_string)
                .collect()
        } else {
            Vec::new()
        };
        Ok(Self {
            emit_mir_file_names: mir_file_names,
            kind: if path.is_dir() {
                PathKind::Folder({
                    let dir = std::fs::read_dir(path).expect("Should be a valid file.");
                    let files = dir
                        .filter_map(|entry| entry.ok())
                        .filter_map(|dir| try_as_file_path(&dir.path()))
                        .collect();
                    files
                })
            } else {
                return Err(ConfigError::InvalidFile);
            },
        })
    }
    pub fn get_all_mir_file_names(&self) -> Vec<&str> {
        self.emit_mir_file_names
            .iter()
            .map(|string| string.as_str())
            .collect()
    }
    pub fn get_all_source_files(&self) -> Result<Box<[SourceFile]>, SourceError> {
        match &self.kind {
            PathKind::Folder(files) => Ok(files
                .iter()
                .map(|file| {
                    Ok(SourceFile {
                        name: file.name.to_string(),
                        data: std::fs::read_to_string(&file.path)
                            .map_err(SourceError::ReadFileFailed)?,
                    })
                })
                .collect::<Result<Box<[_]>, _>>()?),
        }
    }
}

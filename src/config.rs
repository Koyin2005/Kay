use std::path::{Path, PathBuf};
pub const KAE_EXTENSION: &str = "k";
pub const KAE_EXTENSION_WITH_DOT: &str = ".k";
pub enum ConfigError {
    ExpectedArgs { expected: usize, got: usize },
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
    File,
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
    file_path: FilePath,
    kind: PathKind,
}
impl Config {
    pub fn new(args: &[String]) -> Result<Self, ConfigError> {
        let [_, file_name] = args else {
            return Err(ConfigError::ExpectedArgs {
                expected: 1,
                got: args.len() - 1,
            });
        };
        let path = Path::new(file_name);
        if !path.exists() {
            return Err(ConfigError::FileDoesNotExist(file_name.to_string()));
        }
        Ok(Self {
            file_path: try_as_file_path(path).ok_or(ConfigError::InvalidFile)?,
            kind: if path.is_file() {
                PathKind::File
            } else {
                PathKind::Folder({
                    let dir = std::fs::read_dir(path).expect("Should be a valid file.");
                    let files = dir.filter_map(|entry| entry.ok())
                        .filter_map(|dir| try_as_file_path(&dir.path()))
                        .collect();
                    files
                })
            },
        })
    }
    pub fn get_all_source_files(&self) -> Result<Box<[SourceFile]>, SourceError> {
        match &self.kind {
            PathKind::File => Ok(Box::new([SourceFile {
                name: self.file_path.name.to_string(),
                data: std::fs::read_to_string(&self.file_path.path)
                    .map_err(SourceError::ReadFileFailed)?,
            }])),
            PathKind::Folder(files) => {
                Ok(files
                    .iter()
                    .map(|file| {
                        Ok(SourceFile {
                            name: file.name.to_string(),
                            data: std::fs::read_to_string(&file.path)
                                .map_err(SourceError::ReadFileFailed)?,
                        })
                    })
                    .collect::<Result<Box<[_]>, _>>()?)
            }
        }
    }
}

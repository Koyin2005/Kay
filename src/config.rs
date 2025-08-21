use std::path::{Path, PathBuf};
pub const KAE_EXTENSION: &str = "k";
pub const KAE_EXTENSION_WITH_DOT: &str = ".k";
pub enum ConfigError {
    ExpectedArgs { expected: usize, got: usize },
    InvalidFile,
}
pub enum PathKind {
    Folder,
    Source,
}
pub struct Config {
    path: PathBuf,
    is_source_file: bool,
}
impl Config {
    pub fn path_kind(&self) -> PathKind {
        if self.is_source_file {
            PathKind::Source
        } else {
            PathKind::Folder
        }
    }
    pub fn path_str(&self) -> &str {
        self.path.as_path().to_str().expect("Should be valid utf8")
    }
    pub fn file_name(&self) -> String {
        Path::new(self.path_str())
            .file_name()
            .expect("Already pre checked it to be a file path")
            .to_string_lossy()
            .into_owned()
    }
    pub fn new(args: &[String]) -> Result<Self, ConfigError> {
        let [_, file_name] = args else {
            return Err(ConfigError::ExpectedArgs {
                expected: 1,
                got: args.len() - 1,
            });
        };
        let path = Path::new(file_name);
        let is_source_file = if path.is_dir() {
            false
        } else {
            let Some(extension) = path.extension() else {
                return Err(ConfigError::InvalidFile);
            };
            let KAE_EXTENSION = extension.to_string_lossy().as_ref() else {
                return Err(ConfigError::InvalidFile);
            };
            true
        };
        Ok(Self {
            path: path.into(),
            is_source_file,
        })
    }
}

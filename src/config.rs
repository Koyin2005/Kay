use std::path::Path;

pub enum ConfigError {
    ExpectedArgs { expected: usize, got: usize },
    InvalidFile,
}
pub struct Config {
    path: String,
}
impl Config {
    pub fn path_str(&self) -> &str {
        &self.path
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
        let Some(extension) = Path::new(file_name).extension() else {
            return Err(ConfigError::InvalidFile);
        };
        const KAE_EXTENSION: &str = "k";
        let KAE_EXTENSION = extension.to_string_lossy().as_ref() else {
            return Err(ConfigError::InvalidFile);
        };
        Ok(Self {
            path: file_name.into(),
        })
    }
}

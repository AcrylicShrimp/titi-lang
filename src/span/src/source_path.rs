use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SourcePath {
    Real(PathBuf),
    Virtual(String),
}

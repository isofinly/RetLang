use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// What can go wrong while linking object files and libraries.
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(
    code(ret_lang::linker::error),
    help("Resolve the undefined or duplicate symbols highlighted below")
)]
pub struct LinkError {
    #[source_code]
    pub object_code: Option<NamedSource<String>>,
    #[label("{label}")]
    pub span: Option<SourceSpan>,
    pub object_name: String,
    pub label: String,
    pub message: String,
}

impl LinkError {
    pub fn new(
        object_name: &str,
        object_text: Option<&str>,
        span: Option<SourceSpan>,
        label: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            object_code: object_text.map(|txt| NamedSource::new(object_name, txt.to_owned())),
            span,
            object_name: object_name.into(),
            label: label.into(),
            message: message.into(),
        }
    }
}

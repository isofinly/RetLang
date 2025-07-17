use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// What can go wrong while lexing one source file.
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(
    code(ret_lang::lexer::error),
    help("Check the syntax at the highlighted location")
)]
pub struct ParseError {
    #[source_code]
    pub source_code: NamedSource<String>,
    #[label("{label}")]
    pub span: SourceSpan,
    pub message: String,
    pub label: String,
}

impl ParseError {
    pub fn new(
        filename: &str,
        full_src: &str,
        span: SourceSpan,
        label: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            source_code: NamedSource::new(filename, full_src.to_owned()),
            span,
            label: label.into(),
            message: message.into(),
        }
    }
}

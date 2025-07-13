use std::io;
use std::path::PathBuf;

use miette::{IntoDiagnostic, Result};

pub fn link(objects: &[PathBuf], output: &PathBuf) -> Result<()> {
    // TODO: real linker
    let _ = objects;
    std::fs::write(output, b"").into_diagnostic()?;
    Ok(())
}

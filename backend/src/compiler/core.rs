use crate::compiler::{ast::Program, semantics::check_semantics};

pub fn compile(tokens: Program) -> miette::Result<Vec<u8>> {
    // TODO: real compiler -> returns “object” bytes
    check_semantics(&tokens)?;
    let _ = tokens;

    Ok(Vec::new())
}

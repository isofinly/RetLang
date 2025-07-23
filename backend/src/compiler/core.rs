use crate::compiler::{ast::Program, semantics::check_semantics, transpiler::CodeBuilder};

pub fn compile(tokens: Program) -> miette::Result<String> {
    // TODO: real compiler -> returns “object” bytes
    check_semantics(&tokens)?;
    let code = CodeBuilder::new(&tokens).with_all().build();

    Ok(code)
}

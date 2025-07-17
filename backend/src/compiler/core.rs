use crate::{compiler::ast::Program, lexer::tokens::Token};

pub fn compile(tokens: Program) -> Vec<u8> {
    // TODO: real compiler -> returns “object” bytes
    let _ = tokens;
    Vec::new()
}

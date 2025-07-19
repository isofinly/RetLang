use crate::compiler::ast::{Expression, Instruction, Program};

pub fn check_semantics(ast: &Program) -> miette::Result<()> {
    check_stack_init(&ast)?;
    check_calls(&ast)?;

    Ok(())
}

fn check_stack_init(ast: &Program) -> miette::Result<()> {
    for init in &ast.stack_init.initial {
        if !ast.gadgets.iter().any(|gadget| &gadget.name == init) {
            return Err(miette::miette!("Undefined gadget name: {}", init));
        }
    }
    Ok(())
}

fn check_calls(ast: &Program) -> miette::Result<()> {
    let gadget_defs = ast
        .gadgets
        .iter()
        .map(|gadget| &gadget.name)
        .collect::<Vec<_>>();
    for gadget in &ast.gadgets {
        for instruction in gadget.body.instructions.as_slice() {
            match instruction {
                Instruction::Call(call) => {
                    if !gadget_defs.contains(&&call.callee) {
                        eprintln!("Found call to undefined identifier {}", call.callee)
                        // TODO: Think about it
                        // return Err(miette::miette!("Undefined gadget name: {}", call.callee));
                    }
                }
                _ => {}
            }
            match &gadget.body.ret.value {
                Some(Expression::Call(call)) => {
                    if !gadget_defs.contains(&&call.callee) {
                        eprintln!("Found call to undefined identifier {}", call.callee);
                        // TODO: Think about it
                        // return Err(miette::miette!("Undefined gadget name: {}", call.callee));
                    }
                }
                _ => {}
            }
        }
    }

    Ok(())
}

use std::collections::HashSet;

use crate::compiler::ast::{
    Condition, Expression, GadgetDef, Identifier, Instruction, MemoryOp, Program, StackOp,
};

pub fn check_semantics(ast: &Program) -> miette::Result<()> {
    check_stack_init(&ast)?;
    check_calls(&ast)?;
    for gadget in &ast.gadgets {
        track_local_variables(&gadget)?;
    }

    Ok(())
}

//
// Symbol Collection
//

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
                        eprintln!("WARN: Found call to undefined identifier {:?}", call.callee)
                        // TODO: Think about it
                        // return Err(miette::miette!("Undefined gadget name: {}", call.callee));
                    }
                }
                _ => {}
            }
            match &gadget.body.ret.value {
                Some(Expression::Call(call)) => {
                    if !gadget_defs.contains(&&call.callee) {
                        eprintln!("WARN: Found call to undefined identifier {:?}", call.callee);
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

//
// Per-Gadget Analysis
//

fn collect_uses(expr: &Expression) -> HashSet<Identifier> {
    let mut uses = HashSet::new();
    match expr {
        Expression::Identifier(id) => {
            uses.insert(id.clone());
        }
        Expression::MemoryRef(inner) => {
            uses.extend(collect_uses(inner));
        }
        Expression::StackRef(inner) => {
            uses.extend(collect_uses(inner));
        }
        Expression::Binary(binary) => {
            uses.extend(collect_uses(&binary.lhs));
            uses.extend(collect_uses(&binary.rhs));
        }
        Expression::Unary(unary) => {
            uses.extend(collect_uses(&unary.operand));
        }
        Expression::Call(call) => {
            for arg in &call.args {
                uses.extend(collect_uses(arg));
            }
        }
        _ => {} // Literals have no uses
    }
    uses
}

fn collect_condition_uses(condition: &Condition) -> HashSet<Identifier> {
    let mut uses = collect_uses(&condition.lhs);
    uses.extend(collect_uses(&condition.rhs));
    uses
}

/// All identifiers that this instruction *defines*
fn collect_defs(inst: &Instruction) -> HashSet<Identifier> {
    let mut defs = HashSet::new();
    match inst {
        Instruction::Assignment(a) => {
            defs.insert(a.target.clone());
        }
        Instruction::StackOp(op) => match op {
            StackOp::Pop(t) => {
                defs.insert(t.clone());
            }
            StackOp::Peek { target, .. } => {
                defs.insert(target.clone());
            }
            _ => {}
        },
        Instruction::Arithmetic(a) => {
            defs.insert(a.dest.clone());
        }
        Instruction::MemoryOp(mem) => {
            if let MemoryOp::Load { target, .. } = mem {
                defs.insert(target.clone());
            }
        }
        Instruction::ConditionalMod(c) => {
            defs.insert(c.target.clone());
        }
        _ => {}
    }
    defs
}

fn track_local_variables(gadget: &GadgetDef) -> miette::Result<()> {
    // which identifiers can be locals
    let candidate_locals: HashSet<Identifier> = gadget
        .body
        .instructions
        .iter()
        .flat_map(collect_defs)
        .collect();

    // check defined before use
    let mut defined: HashSet<Identifier> = HashSet::new();
    let mut used_before_def: HashSet<Identifier> = HashSet::new();

    for inst in &gadget.body.instructions {
        let (uses, defs) = {
            let mut uses = HashSet::new();
            let defs = collect_defs(inst);

            match inst {
                Instruction::Assignment(a) => {
                    uses.extend(collect_uses(&a.value));
                }
                Instruction::StackOp(op) => match op {
                    StackOp::Push(expr) => uses.extend(collect_uses(expr)),
                    StackOp::Swap { left, right } => {
                        uses.extend(collect_uses(left));
                        uses.extend(collect_uses(right));
                    }
                    StackOp::Peek { offset, .. } => uses.extend(collect_uses(offset)),
                    _ => {}
                },
                Instruction::Arithmetic(a) => {
                    uses.insert(a.dest.clone()); // read-modify-write
                    uses.extend(collect_uses(&a.lhs));
                    uses.extend(collect_uses(&a.rhs));
                }
                Instruction::MemoryOp(mem) => match mem {
                    MemoryOp::Store { value, address } => {
                        uses.extend(collect_uses(value));
                        uses.extend(collect_uses(address));
                    }
                    MemoryOp::Load { address, .. } => uses.extend(collect_uses(address)),
                },
                Instruction::ConditionalMod(c) => {
                    uses.extend(collect_condition_uses(&c.condition));
                    uses.extend(collect_uses(&c.value));
                }
                Instruction::Call(call) => {
                    for arg in &call.args {
                        uses.extend(collect_uses(arg));
                    }
                }
            }

            (uses, defs)
        };

        for u in uses {
            if candidate_locals.contains(&u) && !defined.contains(&u) {
                used_before_def.insert(u);
            }
        }

        defined.extend(defs);
    }

    if let Some(ret_expr) = &gadget.body.ret.value {
        for u in collect_uses(ret_expr) {
            if candidate_locals.contains(&u) && !defined.contains(&u) {
                used_before_def.insert(u);
            }
        }
    }

    if !used_before_def.is_empty() {
        return Err(miette::miette!(
            "Used before definition (potential uninit reads): {:?}",
            used_before_def
        ));
    }

    Ok(())
}

// Ensure offsets/expressions are int-typed.
// * Address expr must be int-typed
// * Condition lhs/rhs must be comparable (both int or both string).

//
// Type Inference/Checking
//

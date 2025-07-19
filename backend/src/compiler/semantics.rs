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
            // TODO: Verify this
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

fn track_local_variables(gadget: &GadgetDef) -> miette::Result<()> {
    let mut defined_vars: HashSet<Identifier> = HashSet::new();
    let mut used_before_def: HashSet<Identifier> = HashSet::new();

    for instruction in &gadget.body.instructions {
        let (uses, defs): (HashSet<Identifier>, HashSet<Identifier>) = match instruction {
            Instruction::Assignment(assign) => {
                let defs = {
                    let mut d = HashSet::new();
                    d.insert(assign.target.clone());
                    d
                };
                (collect_uses(&assign.value), defs)
            }
            Instruction::StackOp(stack_op) => match stack_op {
                StackOp::Push(expr) => (collect_uses(expr), HashSet::new()),
                StackOp::Pop(target) => {
                    let defs = {
                        let mut d = HashSet::new();
                        d.insert(target.clone());
                        d
                    };
                    (HashSet::new(), defs)
                }
                StackOp::Peek { target, offset } => {
                    let defs = {
                        let mut d = HashSet::new();
                        d.insert(target.clone());
                        d
                    };
                    (collect_uses(offset), defs)
                }
                StackOp::Swap { left, right } => {
                    let mut u = collect_uses(left);
                    u.extend(collect_uses(right));
                    (u, HashSet::new())
                }
            },
            Instruction::Arithmetic(arith) => {
                let mut u = {
                    let mut uh = HashSet::new();
                    uh.insert(arith.dest.clone());
                    uh
                }; // Reads dest for compound op
                u.extend(collect_uses(&arith.lhs));
                u.extend(collect_uses(&arith.rhs));
                let defs = {
                    let mut d = HashSet::new();
                    d.insert(arith.dest.clone());
                    d
                };
                (u, defs)
            }
            Instruction::MemoryOp(mem_op) => match mem_op {
                MemoryOp::Store { value, address } => {
                    let mut u = collect_uses(value);
                    u.extend(collect_uses(address));
                    (u, HashSet::new())
                }
                MemoryOp::Load { target, address } => {
                    let defs = {
                        let mut d = HashSet::new();
                        d.insert(target.clone());
                        d
                    };
                    (collect_uses(address), defs)
                }
            },
            Instruction::ConditionalMod(cond_mod) => {
                let mut u = collect_condition_uses(&cond_mod.condition);
                u.extend(collect_uses(&cond_mod.value));

                let defs = {
                    let mut d = HashSet::new();
                    d.insert(cond_mod.target.clone());
                    d
                };
                (u, defs)
            }
            Instruction::Call(call) => {
                let mut u = HashSet::new();
                for arg in &call.args {
                    u.extend(collect_uses(arg));
                }

                (u, HashSet::new())
            }
        };

        for u in &uses {
            if !defined_vars.contains(u) {
                used_before_def.insert(u.clone());
            }
        }

        for d in &defs {
            if defined_vars.contains(d) {
                // TODO: Currently each assignment counts as definition. Maybe allow reassign
                return Err(miette::miette!("Duplicate definition of variable: {}", d));
            }
        }

        defined_vars.extend(defs);
    }

    if let Some(ret_value) = &gadget.body.ret.value {
        let ret_uses = collect_uses(ret_value);
        for u in ret_uses {
            if !defined_vars.contains(&u) {
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

// Ensure every gadget ends with ReturnStmt (already in parse, but confirm).

// Ensure offsets/expressions are int-typed (see typing below).

// Address expr must be int-typed

// Condition lhs/rhs must be comparable (both int or both string).

//
// Type Inference/Checking
//

//
// Per-Gadget Analysis
//

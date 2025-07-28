use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use retc::compiler::{
    ast::{
        Assignment, BinOp, BinaryExpr, Call, CompOp, Condition, ConditionalMod, Expression,
        GadgetBody, GadgetDef, Header, Instruction, Literal, MemoryOp, Program, ReturnStmt,
        StackInit, StackOp,
    },
    transpiler::CodeBuilder,
};

fn bench_transpiling(c: &mut Criterion) {
    let mut group = c.benchmark_group("transpiler_benchmarks");

    group.bench_function("rule110_program", |b| {
        let p = Program {
            headers: vec![Header::System(vec!["stdio", "h"])],
            gadgets: vec![
                GadgetDef {
                    doc: None,
                    name: "main",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Literal(Literal::Int(30)),
                                address: Expression::Literal(Literal::Int(1000)),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Literal(Literal::Int(0)),
                                address: Expression::Literal(Literal::Int(1001)),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Literal(Literal::Int(1)),
                                address: Expression::Literal(Literal::Int(59)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "generation_loop_check",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "generation_loop_check",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "gen_count",
                                address: Expression::Literal(Literal::Int(1000)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "next_gadget",
                                value: Expression::Identifier("generation_loop_done"),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("gen_count"),
                                    op: CompOp::Gt,
                                    rhs: Expression::Literal(Literal::Int(0)),
                                },
                                target: "next_gadget",
                                value: Expression::Identifier("generation_loop_body"),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "next_gadget",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "generation_loop_body",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "gen_count",
                                address: Expression::Literal(Literal::Int(1000)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "gen_count",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("gen_count"),
                                    op: BinOp::Sub,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Identifier("gen_count"),
                                address: Expression::Literal(Literal::Int(1000)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "generation_loop_check",
                            ))),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "cell_loop_init",
                            ))),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_loop_init",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "generation_loop_done",
                    body: GadgetBody {
                        instructions: vec![Instruction::Call(Call {
                            callee: "printf",
                            args: vec![
                                Expression::Literal(Literal::Str("%s")),
                                Expression::Literal(Literal::Str(
                                    "Rule 110 simulation finished.\\n",
                                )),
                            ],
                        })],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "do_nothing",
                    body: GadgetBody {
                        instructions: vec![],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_loop_init",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Literal(Literal::Int(0)),
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_newline",
                            ))),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_loop_check",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_loop_check",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "i",
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "next_gadget",
                                value: Expression::Identifier("do_nothing"),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Lt,
                                    rhs: Expression::Literal(Literal::Int(60)),
                                },
                                target: "next_gadget",
                                value: Expression::Identifier("print_loop_body"),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "next_gadget",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_loop_body",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "i",
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "current_buf_base",
                                address: Expression::Literal(Literal::Int(1001)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("current_buf_base"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("i"),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "cell_val",
                                address: Expression::Identifier("addr"),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_and_increment",
                            ))),
                            Instruction::Assignment(Assignment {
                                target: "print_gadget",
                                value: Expression::Identifier("print_dot"),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("cell_val"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                },
                                target: "print_gadget",
                                value: Expression::Identifier("print_hash"),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_gadget",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_dot",
                    body: GadgetBody {
                        instructions: vec![Instruction::Call(Call {
                            callee: "printf",
                            args: vec![
                                Expression::Literal(Literal::Str("%s")),
                                Expression::Literal(Literal::Str(".")),
                            ],
                        })],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_hash",
                    body: GadgetBody {
                        instructions: vec![Instruction::Call(Call {
                            callee: "printf",
                            args: vec![
                                Expression::Literal(Literal::Str("%s")),
                                Expression::Literal(Literal::Str("#")),
                            ],
                        })],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_and_increment",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "i",
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "i",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("i"),
                                    op: BinOp::Add,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Identifier("i"),
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "print_loop_check",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "print_newline",
                    body: GadgetBody {
                        instructions: vec![Instruction::Call(Call {
                            callee: "printf",
                            args: vec![
                                Expression::Literal(Literal::Str("%s")),
                                Expression::Literal(Literal::Str("\\n")),
                            ],
                        })],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "cell_loop_init",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Literal(Literal::Int(0)),
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "swap_buffers",
                            ))),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "cell_loop_check",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "cell_loop_check",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "i",
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "next_gadget",
                                value: Expression::Identifier("do_nothing"),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Lt,
                                    rhs: Expression::Literal(Literal::Int(60)),
                                },
                                target: "next_gadget",
                                value: Expression::Identifier("apply_rule_110"),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "next_gadget",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "apply_rule_110",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "i",
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "src_base",
                                address: Expression::Literal(Literal::Int(1001)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "dest_base",
                                value: Expression::Literal(Literal::Int(60)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("src_base"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(60)),
                                },
                                target: "dest_base",
                                value: Expression::Literal(Literal::Int(0)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "left_val",
                                value: Expression::Literal(Literal::Int(0)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "right_val",
                                value: Expression::Literal(Literal::Int(0)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "left_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("src_base"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("i"),
                                })),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Gt,
                                    rhs: Expression::Literal(Literal::Int(0)),
                                },
                                target: "left_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("left_addr"),
                                    op: BinOp::Sub,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                })),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Gt,
                                    rhs: Expression::Literal(Literal::Int(0)),
                                },
                                target: "left_val",
                                value: Expression::MemoryRef(Box::new(Expression::Identifier(
                                    "left_addr",
                                ))),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "right_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("src_base"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("i"),
                                })),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Lt,
                                    rhs: Expression::Literal(Literal::Int(59)),
                                },
                                target: "right_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("right_addr"),
                                    op: BinOp::Add,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                })),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("i"),
                                    op: CompOp::Lt,
                                    rhs: Expression::Literal(Literal::Int(59)),
                                },
                                target: "right_val",
                                value: Expression::MemoryRef(Box::new(Expression::Identifier(
                                    "right_addr",
                                ))),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "center_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("src_base"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("i"),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "center_val",
                                address: Expression::Identifier("center_addr"),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "pattern",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("left_val"),
                                    op: BinOp::Mul,
                                    rhs: Expression::Literal(Literal::Int(4)),
                                })),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "center_pattern",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("center_val"),
                                    op: BinOp::Mul,
                                    rhs: Expression::Literal(Literal::Int(2)),
                                })),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "pattern",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("pattern"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("center_pattern"),
                                })),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "pattern",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("pattern"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("right_val"),
                                })),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(0)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("pattern"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                },
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(1)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("pattern"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(2)),
                                },
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(1)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("pattern"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(3)),
                                },
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(1)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("pattern"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(5)),
                                },
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(1)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("pattern"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(6)),
                                },
                                target: "new_state",
                                value: Expression::Literal(Literal::Int(1)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "dest_addr",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("dest_base"),
                                    op: BinOp::Add,
                                    rhs: Expression::Identifier("i"),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Identifier("new_state"),
                                address: Expression::Identifier("dest_addr"),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "i",
                                value: Expression::Binary(Box::new(BinaryExpr {
                                    lhs: Expression::Identifier("i"),
                                    op: BinOp::Add,
                                    rhs: Expression::Literal(Literal::Int(1)),
                                })),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Identifier("i"),
                                address: Expression::Literal(Literal::Int(1002)),
                            }),
                            Instruction::StackOp(StackOp::Push(Expression::Identifier(
                                "cell_loop_check",
                            ))),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
                GadgetDef {
                    doc: None,
                    name: "swap_buffers",
                    body: GadgetBody {
                        instructions: vec![
                            Instruction::MemoryOp(MemoryOp::Load {
                                target: "current_buf",
                                address: Expression::Literal(Literal::Int(1001)),
                            }),
                            Instruction::Assignment(Assignment {
                                target: "new_buf",
                                value: Expression::Literal(Literal::Int(60)),
                            }),
                            Instruction::ConditionalMod(ConditionalMod {
                                condition: Condition {
                                    lhs: Expression::Identifier("current_buf"),
                                    op: CompOp::Eq,
                                    rhs: Expression::Literal(Literal::Int(60)),
                                },
                                target: "new_buf",
                                value: Expression::Literal(Literal::Int(0)),
                            }),
                            Instruction::MemoryOp(MemoryOp::Store {
                                value: Expression::Identifier("new_buf"),
                                address: Expression::Literal(Literal::Int(1001)),
                            }),
                        ],
                        ret: ReturnStmt { value: None },
                    },
                },
            ],
            stack_init: StackInit {
                initial: vec!["main"],
            },
        };

        b.iter(|| black_box(CodeBuilder::new(&p).with_all().build()));
    });

    group.finish();
}

criterion_group!(benches, bench_transpiling);
criterion_main!(benches);

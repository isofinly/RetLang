use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

use retc::{
    lexer::tokens::{Token, TokenKind},
    parser::core::parse,
};

fn generate_large_program_tokens(num_push_instructions: usize) -> Vec<Token<'static>> {
    let mut tokens = Vec::new();

    tokens.push(Token {
        kind: TokenKind::Gadget,
        line: 1,
        column: 1,
    });
    tokens.push(Token {
        kind: TokenKind::Identifier("main"),
        line: 1,
        column: 8,
    });
    tokens.push(Token {
        kind: TokenKind::Colon,
        line: 1,
        column: 13,
    });

    for i in 0..num_push_instructions {
        tokens.push(Token {
            kind: TokenKind::Push,
            line: (2 + i),
            column: 1,
        });
        tokens.push(Token {
            kind: TokenKind::Int(1),
            line: (2 + i),
            column: 6,
        });
    }

    tokens.push(Token {
        kind: TokenKind::Ret,
        line: (2 + num_push_instructions),
        column: 1,
    });

    tokens.push(Token {
        kind: TokenKind::Stack,
        line: (3 + num_push_instructions),
        column: 1,
    });
    tokens.push(Token {
        kind: TokenKind::Colon,
        line: (3 + num_push_instructions),
        column: 7,
    });
    tokens.push(Token {
        kind: TokenKind::LBracket,
        line: (3 + num_push_instructions),
        column: 8,
    });
    tokens.push(Token {
        kind: TokenKind::Identifier("main"),
        line: (3 + num_push_instructions),
        column: 9,
    });
    tokens.push(Token {
        kind: TokenKind::RBracket,
        line: (3 + num_push_instructions),
        column: 13,
    });

    // Eof
    tokens.push(Token {
        kind: TokenKind::Eof,
        line: (4 + num_push_instructions),
        column: 1,
    });

    tokens
}

fn bench_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_benchmarks");

    group.bench_function("parse_multiple_external_headers", |b| {
        let tokens = [
            Token {
                kind: TokenKind::External,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba"),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 2,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h"),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::External,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 3,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba2"),
                line: 3,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 3,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("h"),
                line: 3,
                column: 19,
            },
            Token {
                kind: TokenKind::Gt,
                line: 3,
                column: 20,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 10,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("basic_assignment", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("x"),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Assign,
                line: 2,
                column: 7,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("test"),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 13,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 14,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("arithmetic_operation", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("calc"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier("result"),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Plus,
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Assign,
                line: 2,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("a"),
                line: 2,
                column: 16,
            },
            Token {
                kind: TokenKind::Plus,
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("b"),
                line: 2,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("calc"),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 13,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 14,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("stack_operations", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("stack_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Push,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(100),
                line: 2,
                column: 10,
            },
            Token {
                kind: TokenKind::Pop,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("temp"),
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::Ret,
                line: 4,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 5,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 5,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("stack_test"),
                line: 5,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 5,
                column: 19,
            },
            Token {
                kind: TokenKind::Eof,
                line: 5,
                column: 20,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("memory_operations", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("memory_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Store,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(1000),
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 2,
                column: 16,
            },
            Token {
                kind: TokenKind::Load,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("value"),
                line: 3,
                column: 10,
            },
            Token {
                kind: TokenKind::Int(1000),
                line: 3,
                column: 16,
            },
            Token {
                kind: TokenKind::Ret,
                line: 4,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 5,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 5,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("memory_test"),
                line: 5,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 5,
                column: 20,
            },
            Token {
                kind: TokenKind::Eof,
                line: 5,
                column: 21,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("conditional_operations", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("cond_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::If,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("x"),
                line: 2,
                column: 8,
            },
            Token {
                kind: TokenKind::Lt,
                line: 2,
                column: 10,
            },
            Token {
                kind: TokenKind::Int(10),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Then,
                line: 2,
                column: 15,
            },
            Token {
                kind: TokenKind::Identifier("result"),
                line: 2,
                column: 20,
            },
            Token {
                kind: TokenKind::Assign,
                line: 2,
                column: 27,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 2,
                column: 29,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("cond_test"),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 18,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 19,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("stack_swap_operation", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("swap_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::StackSwap,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 2,
                column: 16,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("swap_test"),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 18,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 19,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("function_call", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("call_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("printf"),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::LParen,
                line: 2,
                column: 11,
            },
            Token {
                kind: TokenKind::Str("Hello"),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::RParen,
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("call_test"),
                line: 4,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 4,
                column: 18,
            },
            Token {
                kind: TokenKind::Eof,
                line: 4,
                column: 19,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("multiple_gadgets", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("first"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Push,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 2,
                column: 10,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Gadget,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("second"),
                line: 4,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 4,
                column: 15,
            },
            Token {
                kind: TokenKind::Push,
                line: 5,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 5,
                column: 10,
            },
            Token {
                kind: TokenKind::Ret,
                line: 6,
                column: 5,
            },
            Token {
                kind: TokenKind::Stack,
                line: 7,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 7,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 7,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("first"),
                line: 7,
                column: 9,
            },
            Token {
                kind: TokenKind::Comma,
                line: 7,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("second"),
                line: 7,
                column: 16,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 7,
                column: 22,
            },
            Token {
                kind: TokenKind::Eof,
                line: 7,
                column: 23,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("ret_with_value", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("return_test"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::Ret,
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 2,
                column: 9,
            },
            Token {
                kind: TokenKind::Stack,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 3,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 3,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("return_test"),
                line: 3,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 3,
                column: 20,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 21,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("edge_overall_full_program", |b| {
        let tokens = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("full"),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 13,
            },
            // Assignment: a = 42
            Token {
                kind: TokenKind::Identifier("a"),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Assign,
                line: 2,
                column: 7,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 2,
                column: 9,
            },
            // Arithmetic: result + = b + c
            Token {
                kind: TokenKind::Identifier("result"),
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Plus,
                line: 3,
                column: 12,
            },
            Token {
                kind: TokenKind::Assign,
                line: 3,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("b"),
                line: 3,
                column: 16,
            },
            Token {
                kind: TokenKind::Plus,
                line: 3,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("c"),
                line: 3,
                column: 20,
            },
            // Store: store 1000 42
            Token {
                kind: TokenKind::Store,
                line: 4,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(1000),
                line: 4,
                column: 11,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 4,
                column: 16,
            },
            // Conditional: if 1 < 2 then f = "g"
            Token {
                kind: TokenKind::If,
                line: 5,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 5,
                column: 8,
            },
            Token {
                kind: TokenKind::Lt,
                line: 5,
                column: 10,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 5,
                column: 12,
            },
            Token {
                kind: TokenKind::Then,
                line: 5,
                column: 14,
            },
            Token {
                kind: TokenKind::Identifier("f"),
                line: 5,
                column: 19,
            },
            Token {
                kind: TokenKind::Assign,
                line: 5,
                column: 21,
            },
            Token {
                kind: TokenKind::Str("g"),
                line: 5,
                column: 23,
            },
            // Peek: peek temp [0]
            Token {
                kind: TokenKind::Peek,
                line: 6,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("temp"),
                line: 6,
                column: 10,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 6,
                column: 15,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 6,
                column: 16,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 6,
                column: 17,
            },
            // Stack swap: stack_swap 0 1
            Token {
                kind: TokenKind::StackSwap,
                line: 7,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 7,
                column: 16,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 7,
                column: 18,
            },
            // Load: load m n
            Token {
                kind: TokenKind::Load,
                line: 8,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("m"),
                line: 8,
                column: 10,
            },
            Token {
                kind: TokenKind::Identifier("n"),
                line: 8,
                column: 12,
            },
            // Pop: pop temp
            Token {
                kind: TokenKind::Pop,
                line: 9,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("temp"),
                line: 9,
                column: 9,
            },
            // Push: push p
            Token {
                kind: TokenKind::Push,
                line: 10,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("p"),
                line: 10,
                column: 10,
            },
            // Return with value
            Token {
                kind: TokenKind::Ret,
                line: 11,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier("result"),
                line: 11,
                column: 9,
            },
            // Stack initialization
            Token {
                kind: TokenKind::Stack,
                line: 12,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 12,
                column: 7,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 12,
                column: 8,
            },
            Token {
                kind: TokenKind::Identifier("full"),
                line: 12,
                column: 9,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 12,
                column: 13,
            },
            Token {
                kind: TokenKind::Eof,
                line: 12,
                column: 14,
            },
        ];
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("parse_large_program_40KB", |b| {
        let tokens = generate_large_program_tokens(2555);
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.bench_function("parse_large_program_1MB", |b| {
        let tokens = generate_large_program_tokens(17471);
        b.iter(|| black_box(parse(&tokens).unwrap()));
    });

    group.finish();
}

criterion_group!(benches, bench_parsing);
criterion_main!(benches);

#![allow(dead_code)]
//! All lexical categories recognised by the lexer.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,   // 1-based
    pub column: usize, // 1-based (UTF-8 byte offset is also fine)
}

/// “Kind” is the part the parser actually cares about.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /*──────────── Atoms ────────────*/
    Identifier(String),
    Int(i64),
    Str(String),

    /*──────────── Keywords ─────────*/
    Gadget,    // gadget
    Stack,     // stack
    Ret,       // ret
    Push,      // push
    Pop,       // pop
    Peek,      // peek
    StackSwap, // stack_swap
    Store,     // store
    Load,      // load
    If,        // if
    Then,      // then

    /*──────────── Punctuation ──────*/
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;   (only appears once, after stack_init)
    LBracket,  // [
    RBracket,  // ]
    LParen,    // (   (you may drop these if you really never need them)
    RParen,    // )

    /*──────────── Operators ────────*/
    Assign, // =
    // arithmetic / bit-wise
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    Amp,     // &
    Pipe,    // |
    Caret,   // ^
    Shl,     // <<
    Shr,     // >>
    // comparison
    EqEq,  // ==
    NotEq, // !=
    Lt,    // <
    Le,    // <=
    Gt,    // >
    Ge,    // >=

    /*──────────── Misc ─────────────*/
    Eof, // virtual token injected by the lexer
}

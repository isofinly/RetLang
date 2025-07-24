//! All lexical categories recognised by the lexer.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: usize,   // 1-based
    pub column: usize, // 1-based
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    Int(i64),
    Str(&'a str),

    External,
    Dot,

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

    Comma,    // ,
    Colon,    // :
    LBracket, // [
    RBracket, // ]
    LParen,   // (
    RParen,   // )

    Assign,  // =
    Plus,    // +
    Minus,   // -
    Star,    // *
    Slash,   // /
    Percent, // %
    Not,     // !

    Amp,   // &
    Pipe,  // |
    Caret, // ^
    Shl,   // <<
    Shr,   // >>

    EqEq,  // ==
    NotEq, // !=
    Lt,    // <
    Le,    // <=
    Gt,    // >
    Ge,    // >=

    Eof,
}

impl Token<'_> {
    pub fn at<'a>(kind: TokenKind<'a>, src: &str, offset: usize) -> Token<'a> {
        let (line, column) = crate::utils::loc::byte_offset_to_line_col(src, offset);

        Token { kind, line, column }
    }
}

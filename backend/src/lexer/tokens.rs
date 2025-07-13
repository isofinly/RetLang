//! All lexical categories recognised by the lexer.

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,   // 1-based
    pub column: usize, // 1-based (UTF-8 byte offset is also fine)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Identifier(String),
    Int(i64),
    Str(String),

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

impl Token {
    pub fn at(kind: TokenKind, src: &str, offset: usize) -> Self {
        let (line, column) = byte_offset_to_line_col(src, offset);
        Self { kind, line, column }
    }
}

fn byte_offset_to_line_col(src: &str, offset: usize) -> (usize, usize) {
    let clamped_offset = offset.min(src.len());

    let newline_count = src[..clamped_offset]
        .bytes()
        .filter(|&b| b == b'\n')
        .count();
    let line = newline_count + 1;

    let last_nl = src[..clamped_offset]
        .rfind('\n')
        .map(|i| i + 1)
        .unwrap_or(0);
    let column = clamped_offset - last_nl + 1; // 1-based

    (line, column)
}

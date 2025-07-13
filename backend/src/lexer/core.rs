use crate::lexer::{
    error::LexError,
    tokens::{Token, TokenKind},
};

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}
fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

pub fn lex(filename: &str, contents: &str) -> miette::Result<Vec<Token>> {
    use TokenKind::*;

    let mut tokens = Vec::new();
    let mut i = 0; // byte offset in `contents`
    let bytes = contents.as_bytes();

    while i < bytes.len() {
        let c = contents[i..].chars().next().unwrap();

        if c.is_whitespace() {
            i += c.len_utf8();
            continue;
        }

        if c == '/' && i + 1 < bytes.len() && contents[i + 1..].starts_with('/') {
            while i < bytes.len() && contents.as_bytes()[i] != b'\n' {
                i += 1;
            }
            continue;
        }

        if is_identifier_start(c) {
            let start = i;
            i += c.len_utf8();
            while i < bytes.len() && is_identifier_continue(contents[i..].chars().next().unwrap()) {
                i += contents[i..].chars().next().unwrap().len_utf8();
            }
            let text = &contents[start..i];
            let kind = match text {
                "gadget" => Gadget,
                "stack" => Stack,
                "ret" => Ret,
                "push" => Push,
                "pop" => Pop,
                "peek" => Peek,
                "stack_swap" => StackSwap,
                "store" => Store,
                "load" => Load,
                "if" => If,
                "then" => Then,
                _ => Identifier(text.to_owned()),
            };

            tokens.push(Token::at(kind, contents, start));
            continue;
        }

        if is_digit(c)
            || (c == '-'
                && i + 1 < bytes.len()
                && is_digit(contents[i + 1..].chars().next().unwrap()))
        {
            let start = i;
            i += c.len_utf8();
            while i < bytes.len() && is_digit(contents[i..].chars().next().unwrap()) {
                i += contents[i..].chars().next().unwrap().len_utf8();
            }
            let num = contents[start..i].parse::<i64>().unwrap();

            tokens.push(Token::at(Int(num), contents, start));
            continue;
        }

        if c == '"' {
            let start = i;
            i += 1; // skip opening quote
            while i < bytes.len() && contents.as_bytes()[i] != b'"' {
                i += 1;
            }
            if i == bytes.len() {
                return Err(miette::miette!("unterminated string literal"));
            }
            let inside = &contents[start + 1..i];
            i += 1; // skip closing quote

            tokens.push(Token::at(Str(inside.to_owned()), contents, start));
            continue;
        }

        let two = &contents[i..].chars().take(2).collect::<String>();
        let three = &contents[i..].chars().take(3).collect::<String>();

        let (kind, consumed) = match three.as_str() {
            ">>=" | "<<=" => {
                return Err(LexError::new(
                    filename,
                    contents,
                    miette::SourceSpan::new(i.into(), contents[i..i + 3].chars().count()),
                    "Not supported",
                    format!("Operands <<= and >>= are not supported"),
                )
                .into());
            }
            _ => match two.as_str() {
                "<<" => (Shl, 2),
                ">>" => (Shr, 2),
                "==" => (EqEq, 2),
                "!=" => (NotEq, 2),
                "<=" => (Le, 2),
                ">=" => (Ge, 2),
                _ => match c {
                    ',' => (Comma, 1),
                    ':' => (Colon, 1),
                    '[' => (LBracket, 1),
                    ']' => (RBracket, 1),
                    '(' => (LParen, 1),
                    ')' => (RParen, 1),
                    '=' => (Assign, 1),
                    '+' => (Plus, 1),
                    '-' => (Minus, 1),
                    '*' => (Star, 1),
                    '/' => (Slash, 1),
                    '%' => (Percent, 1),
                    '&' => (Amp, 1),
                    '|' => (Pipe, 1),
                    '^' => (Caret, 1),
                    '<' => (Lt, 1),
                    '>' => (Gt, 1),
                    _ => {
                        let span = miette::SourceSpan::new(i.into(), 1);
                        return Err(LexError::new(
                            filename,
                            contents,
                            span,
                            "unknown",
                            format!("cannot process character `{c}`"),
                        )
                        .into());
                    }
                },
            },
        };

        tokens.push(Token::at(kind, contents, i));
        i += consumed;
    }

    tokens.push(Token::at(TokenKind::Eof, contents, i));
    Ok(tokens)
}

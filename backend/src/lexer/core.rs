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
                "external" => External,
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

            if i < bytes.len() && is_identifier_continue(contents[i..].chars().next().unwrap()) {
                let span = miette::SourceSpan::new(start.into(), i - start);
                return Err(LexError::new(
                    filename,
                    contents,
                    span,
                    "invalid number",
                    "numbers cannot be immediately followed by identifier characters".to_string(),
                )
                .into());
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
                    "Operands <<= and >>= are not supported".to_string(),
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
                    '!' => (Not, 1),
                    '&' => (Amp, 1),
                    '|' => (Pipe, 1),
                    '^' => (Caret, 1),
                    '<' => (Lt, 1),
                    '>' => (Gt, 1),
                    '.' => (Dot, 1),
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

mod tests {
    #![allow(unused)]
    use super::*;

    #[test]
    fn test_external_token_header_1() {
        let input = "external: <aboba.h>";
        let tokens = lex("testing.ret", input).unwrap();

        let expected = vec![
            Token {
                kind: TokenKind::External,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba".to_string()),
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 1,
                column: 19,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 20,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_external_token_header_2() {
        let input = r#"external: "aboba.h""#;
        let tokens = lex("testing.ret", input).unwrap();

        let expected = vec![
            Token {
                kind: TokenKind::External,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 9,
            },
            Token {
                kind: TokenKind::Str("aboba.h".to_string()),
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 20,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_multiple_external_headers() {
        let input = "external: <aboba.h>\nexternal: <aboba2.h>";
        let tokens = lex("testing.ret", input).unwrap();

        let expected = vec![
            Token {
                kind: TokenKind::External,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 9,
            },
            Token {
                kind: TokenKind::Lt,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Identifier("aboba".to_string()),
                line: 1,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Gt,
                line: 1,
                column: 19,
            },
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
                kind: TokenKind::Identifier("aboba2".to_string()),
                line: 2,
                column: 12,
            },
            Token {
                kind: TokenKind::Dot,
                line: 2,
                column: 18,
            },
            Token {
                kind: TokenKind::Identifier("h".to_string()),
                line: 2,
                column: 19,
            },
            Token {
                kind: TokenKind::Gt,
                line: 2,
                column: 20,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 21,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn valid_keywords_all() {
        let input = "gadget\npush\npop\npeek\nstack_swap\nif\nthen\nret\nstack\nstore\nload";
        let tokens = lex("testing.ret", input).unwrap();

        let expected = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Push,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Pop,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Peek,
                line: 4,
                column: 1,
            },
            Token {
                kind: TokenKind::StackSwap,
                line: 5,
                column: 1,
            },
            Token {
                kind: TokenKind::If,
                line: 6,
                column: 1,
            },
            Token {
                kind: TokenKind::Then,
                line: 7,
                column: 1,
            },
            Token {
                kind: TokenKind::Ret,
                line: 8,
                column: 1,
            },
            Token {
                kind: TokenKind::Stack,
                line: 9,
                column: 1,
            },
            Token {
                kind: TokenKind::Store,
                line: 10,
                column: 1,
            },
            Token {
                kind: TokenKind::Load,
                line: 11,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 11,
                column: 5,
            },
        ];

        assert_eq!(tokens, expected);
    }
    #[test]
    fn invalid_keywords_case_sensitive() {
        let input = "Gadget\nPush";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("Gadget")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("Push")),
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 5,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn edge_keywords_adjacent() {
        let input = "gadgetifthenret";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("gadgetifthenret")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 16,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_identifiers_basic() {
        let input = "foo bar_123 _var A1";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("foo")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("bar_123")),
                line: 1,
                column: 5,
            },
            Token {
                kind: TokenKind::Identifier(String::from("_var")),
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Identifier(String::from("A1")),
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 20,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn invalid_identifiers_start_with_digit() {
        let input = "123var";
        let tokens = lex("testing.ret", input);

        // TODO: Make it a concrete error
        assert!(tokens.is_err());
    }

    #[test]
    fn edge_identifiers_min_max() {
        let input = "a\nz_99999999999999999999\nvery_long_identifier_with_many_underscores_and_digits_1234567890";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("a")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("z_99999999999999999999")),
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from(
                    "very_long_identifier_with_many_underscores_and_digits_1234567890",
                )),
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 65,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn edge_identifiers_keyword_like() {
        let input = "gadget_ push1";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("gadget_")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("push1")),
                line: 1,
                column: 9,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 14,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_int_literals_positive() {
        let input = "0 1 1234567890";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Int(0),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 3,
            },
            Token {
                kind: TokenKind::Int(1234567890),
                line: 1,
                column: 5,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 15,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_int_literals_negative() {
        let input = "-0 -1 -999999999999";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Int(0),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(-1),
                line: 1,
                column: 4,
            },
            Token {
                kind: TokenKind::Int(-999999999999),
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 20,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn invalid_int_literals_non_digit() {
        let input = "12a3 -12.3";
        let tokens = lex("testing.ret", input);

        // TODO: Make it a concrete error
        assert!(tokens.is_err());
    }

    #[test]
    fn edge_int_literals_boundaries() {
        let input = "- 0 +1";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Minus,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Int(0),
                line: 1,
                column: 3,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 5,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 6,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 7,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_string_literals_basic() {
        let input = r#""hello" "world!""#;
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Str(String::from("hello")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Str(String::from("world!")),
                line: 1,
                column: 9,
            },
            // TODO: Decide how to handle empty strings
            // Token {
            //     kind: TokenKind::Str(String::from("")),
            //     line: 1,
            //     column: 11,
            // },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 17,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn invalid_string_literals_unclosed() {
        let input = r#""hello"#;
        let tokens = lex("testing.ret", input);

        assert!(tokens.is_err());
    }

    #[test]
    fn invalid_string_literals_embedded_quote() {
        let input = r#""hel"lo""#;
        let tokens = lex("testing.ret", input);

        assert!(tokens.is_err());
    }

    #[test]
    fn edge_string_literals_special_chars() {
        let input = r#""!@#$%^&*()_+-=[]{}|;':,./<>?""#;
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Str(String::from("!@#$%^&*()_+-=[]{}|;':,./<>?")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 31,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_operators_all() {
        let input = "+ - * / % & | ^ << >> == != < <= > >= = [ ] : , !";
        let tokens = lex("testing.ret", input);
        let expected = [
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Minus,
                line: 1,
                column: 3,
            },
            Token {
                kind: TokenKind::Star,
                line: 1,
                column: 5,
            },
            Token {
                kind: TokenKind::Slash,
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::Percent,
                line: 1,
                column: 9,
            },
            Token {
                kind: TokenKind::Amp,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Pipe,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Caret,
                line: 1,
                column: 15,
            },
            Token {
                kind: TokenKind::Shl,
                line: 1,
                column: 17,
            },
            Token {
                kind: TokenKind::Shr,
                line: 1,
                column: 20,
            },
            Token {
                kind: TokenKind::EqEq,
                line: 1,
                column: 23,
            },
            Token {
                kind: TokenKind::NotEq,
                line: 1,
                column: 26,
            },
            Token {
                kind: TokenKind::Lt,
                line: 1,
                column: 29,
            },
            Token {
                kind: TokenKind::Le,
                line: 1,
                column: 31,
            },
            Token {
                kind: TokenKind::Gt,
                line: 1,
                column: 34,
            },
            Token {
                kind: TokenKind::Ge,
                line: 1,
                column: 36,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 39,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 41,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 43,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 45,
            },
            Token {
                kind: TokenKind::Comma,
                line: 1,
                column: 47,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 49,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 50,
            },
        ];

        assert_eq!(tokens.unwrap(), expected);
    }

    #[test]
    fn valid_not_operator() {
        let input = "!";
        let tokens = lex("testing.ret", input).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 2,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn valid_not_operator_expressions() {
        let input = "!var !(x == y)";
        let tokens = lex("testing.ret", input).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("var".to_string()),
                line: 1,
                column: 2,
            },
            Token {
                kind: TokenKind::Not,
                line: 1,
                column: 6,
            },
            Token {
                kind: TokenKind::LParen,
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::Identifier("x".to_string()),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::EqEq,
                line: 1,
                column: 10,
            },
            Token {
                kind: TokenKind::Identifier("y".to_string()),
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::RParen,
                line: 1,
                column: 14,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 15,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn invalid_operators_unknown() {
        let input = "~ ++ --";
        let tokens = lex("testing.ret", input);

        assert!(tokens.is_err());
    }

    #[test]
    fn edge_operators_compound() {
        let input = "<<= >>=";
        let tokens = lex("testing.ret", input);

        assert!(tokens.is_err());
    }

    #[test]
    fn valid_comments_basic() {
        let input = "// This is a comment\n//Another";
        let tokens = lex("testing.ret", input).unwrap();
        let expected = [Token {
            kind: TokenKind::Eof,
            line: 2,
            // TODO: column 10 or 1
            column: 10,
        }];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn invalid_comments_multiline() {
        let input = "/* multi */";
        let tokens = lex("testing.ret", input).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Slash,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Star,
                line: 1,
                column: 2,
            },
            Token {
                kind: TokenKind::Identifier(String::from("multi")),
                line: 1,
                column: 4,
            },
            Token {
                kind: TokenKind::Star,
                line: 1,
                column: 10,
            },
            Token {
                kind: TokenKind::Slash,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 12,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn edge_comments_empty_and_end_of_line() {
        let input = "foo //\nbar // comment with symbols !@#";
        let tokens = lex("testing.ret", input).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("foo")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("bar")),
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Eof,
                line: 2,
                column: 32,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn valid_mixed_all_tokens() {
        let input = r#"gadget foo: push 42\npop bar // comment\nret "string""#;
        let tokens = lex("testing.ret", input.replace("\\n", "\n").as_str()).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Gadget,
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("foo")),
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Colon,
                line: 1,
                column: 11,
            },
            Token {
                kind: TokenKind::Push,
                line: 1,
                column: 13,
            },
            Token {
                kind: TokenKind::Int(42),
                line: 1,
                column: 18,
            },
            Token {
                kind: TokenKind::Pop,
                line: 2,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier(String::from("bar")),
                line: 2,
                column: 5,
            },
            Token {
                kind: TokenKind::Ret,
                line: 3,
                column: 1,
            },
            Token {
                kind: TokenKind::Str(String::from("string")),
                line: 3,
                column: 5,
            },
            Token {
                kind: TokenKind::Eof,
                line: 3,
                column: 13,
            },
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn edge_no_whitespace_adjacent_tokens() {
        let input = "a=1+b[2]";
        let tokens = lex("testing.ret", input.replace("\\n", "\n").as_str()).unwrap();
        let expected = [
            Token {
                kind: TokenKind::Identifier(String::from("a")),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Assign,
                line: 1,
                column: 2,
            },
            Token {
                kind: TokenKind::Int(1),
                line: 1,
                column: 3,
            },
            Token {
                kind: TokenKind::Plus,
                line: 1,
                column: 4,
            },
            Token {
                kind: TokenKind::Identifier(String::from("b")),
                line: 1,
                column: 5,
            },
            Token {
                kind: TokenKind::LBracket,
                line: 1,
                column: 6,
            },
            Token {
                kind: TokenKind::Int(2),
                line: 1,
                column: 7,
            },
            Token {
                kind: TokenKind::RBracket,
                line: 1,
                column: 8,
            },
            Token {
                kind: TokenKind::Eof,
                line: 1,
                column: 9,
            },
        ];

        assert_eq!(tokens, expected);
    }
}

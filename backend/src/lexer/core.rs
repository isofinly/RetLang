use miette::Result;
use regex::Regex;

use crate::{
    diagf,
    lexer::{error::LexError, tokens::Token},
};

pub fn lex(filename: &str, contents: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();

    let re = Regex::new(r"(?m)//.*$").unwrap();

    let s = re.replace_all(contents, "").into_owned();

    for (n, line) in s.lines().enumerate() {
        // Token metadata
        let trimmed = line.trim_start();
        let col = line.len() - trimmed.len() + 1;
        let at_span = crate::utils::loc::span_from_linecol(&s, n + 1, col, trimmed.len());
        // Actual token kind parsing

        // let loc = crate::utils::core::Loc {
        //     input_path: filename,
        //     line_number: n,
        //     line_offset: col,
        // };

        // match trimmed.split(" ").collect::<Vec<&str>>()[0]
        //     .replace(":", "")
        //     .as_str()
        // {
        //     "" => continue,
        //     "gadget" => {}
        //     "stack" => {
        //         let source_stack = line.split(": ").collect::<Vec<&str>>()[1];
        //         let stack_value = source_stack[1..source_stack.len() - 1]
        //             .split(",")
        //             .collect::<Vec<&str>>()
        //             .iter()
        //             .map(|s| s.trim().trim_start().to_string())
        //             .collect::<Vec<String>>();
        //         if stack_value.is_empty() {
        //             return Err(LexError::new(
        //                 filename,
        //                 &s,
        //                 at_span,
        //                 "empty",
        //                 "stack initial value cannot be empty",
        //             )
        //             .into());
        //         }
        //         let stack = crate::compiler::ast::StackInit {
        //             initial: stack_value,
        //         };
        //         println!("Stack: {:?}", stack);
        //         // ...
        //     }
        //     "ret" => {}
        //     _ => {
        //         diagf!(at loc, "line {}", line);
        //         continue;
        //     }
        // }
    }

    Ok(tokens)
}

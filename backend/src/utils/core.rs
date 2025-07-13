use std::fmt;

#[derive(Clone, Copy, Debug)]
pub struct Loc<'a> {
    pub input_path: &'a str,
    pub line_number: usize,
    pub line_offset: usize,
}

impl<'a> fmt::Display for Loc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.input_path, self.line_number, self.line_offset
        )
    }
}

/// A macro for printing diagnostic messages to stderr.
///
/// It supports two forms:
/// 1. `diagf!(at loc, "format string", args...)`: Uses the provided `Loc` object.
/// 2. `diagf!("format string", args...)`: Infers the `Loc` from the call site
///    using `file!()` and `line!()`. The line_offset will be 1 (start of line).
///
/// It uses `eprintln!` which adds a newline.
#[macro_export]
macro_rules! diagf {
    (at $loc:expr, $fmt:expr $(, $args:expr)*) => {{
        eprintln!("{}: {}", $loc, format_args!($fmt $(, $args)*));
    }};
    ($fmt:expr $(, $args:expr)*) => {{
        let inferred_loc = $crate::utils::core::Loc {
            input_path: file!(),
            line_number: line!() as usize,
            line_offset: 1,
        };
        eprintln!("{}: {}", inferred_loc, format_args!($fmt $(, $args)*));
    }};
}

/// A macro for signaling missing implementations or unimplemented features.
///
/// It supports two forms:
/// 1. `missingf!(at loc, "format string", args...)`: Uses the provided `Loc` object.
/// 2. `missingf!("format string", args...)`: Infers the `Loc` from the call site.
///
/// It prints a TODO message with location, an info message indicating
/// where the macro was invoked, and then causes a panic.
#[macro_export]
macro_rules! missingf {
    (at $loc:expr, $fmt:expr $(, $args:expr)*) => {{
        eprintln!("{}: TODO: {}", $loc, format_args!($fmt $(, $args)*));
        eprintln!("{}:{}: INFO: implementation should go here", file!(), line!());
        panic!("Missing implementation encountered at {}:{}", file!(), line!());
    }};
    ($fmt:expr $(, $args:expr)*) => {{
        let inferred_loc = $crate::utils::core::Loc {
            input_path: file!(),
            line_number: line!() as usize,
            line_offset: 1,
        };
        eprintln!("{}: TODO: {}", inferred_loc, format_args!($fmt $(, $args)*));
        eprintln!("{}:{}: INFO: implementation should go here", file!(), line!());
        panic!("Missing implementation encountered at {}:{}", file!(), line!());
    }};
}

// Example usage:
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagf_explicit_loc() {
        let my_loc = Loc {
            input_path: "src/parser.rs",
            line_number: 100,
            line_offset: 25,
        };

        diagf!(
            at my_loc,
            "Explicit loc: Unexpected token '{}' found.",
            "IDENTIFIER"
        );
    }

    #[test]
    fn test_diagf_inferred_loc() {
        diagf!("Inferred loc: Just a general message here.");
        diagf!("Inferred loc: Value is {}", 42);
    }

    #[test]
    #[should_panic(expected = "Missing implementation encountered")]
    fn test_missingf_explicit_loc() {
        let my_loc = Loc {
            input_path: "src/codegen.rs",
            line_number: 50,
            line_offset: 10,
        };

        missingf!(
            at my_loc,
            "Explicit loc: Feature '{}' is not yet supported for type '{}'.",
            "generics",
            "MyCustomType"
        );
    }

    #[test]
    #[should_panic(expected = "Missing implementation encountered")]
    fn test_missingf_inferred_loc() {
        missingf!("Inferred loc: This feature needs to be implemented.");
    }
}

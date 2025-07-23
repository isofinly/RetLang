use std::process::{Command, Output};
use std::{fmt, io};

#[derive(Debug)]
pub enum CompilerError {
    Io(io::Error),
    ExitFailure(i32, String),
    NotFound,
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompilerError::Io(e) => write!(f, "IO error: {}", e),
            CompilerError::ExitFailure(code, stderr) => {
                write!(f, "Compiler exited with code {}: {}", code, stderr)
            }
            CompilerError::NotFound => {
                write!(f, "No suitable C compiler (clang or gcc) found in $PATH")
            }
        }
    }
}

impl std::error::Error for CompilerError {}

fn detect_compiler() -> Result<String, CompilerError> {
    fn is_available(binary: &str) -> bool {
        Command::new(binary)
            .arg("--version")
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }

    let candidates = if cfg!(windows) {
        vec!["gcc.exe", "clang.exe", "clang", "gcc"]
    } else {
        vec!["gcc", "clang", "cc"]
    };

    for candidate in candidates {
        if is_available(candidate) {
            return Ok(candidate.to_string());
        }
    }

    Err(CompilerError::NotFound)
}

/// Compile a C source file into a binary/executable at the specified output path.
///
/// * `src_path` – path to the `.c` file
/// * `out_path` – desired output executable path
/// * `libs` – list of extra libraries to link against (e.g. ["m", "pthread"])
pub fn compile_c(src_path: &str, out_path: &str, libs: &[String]) -> Result<(), CompilerError> {
    let compiler = detect_compiler()?;

    let mut cmd = Command::new(&compiler);

    cmd.arg("-Wall").arg("-std=c99");

    let actual_out = if cfg!(windows) && !out_path.ends_with(".exe") {
        format!("{}.exe", out_path)
    } else {
        out_path.to_string()
    };

    cmd.arg(src_path).arg("-o").arg(&actual_out);

    for lib in libs.iter().filter(|l| !l.is_empty()) {
        cmd.arg(format!("-l{}", lib));
    }

    let Output { status, stderr, .. } = cmd.output().map_err(CompilerError::Io)?;

    if status.success() {
        Ok(())
    } else {
        let code = status.code().unwrap_or(-1);
        Err(CompilerError::ExitFailure(
            code,
            String::from_utf8_lossy(&stderr).into(),
        ))
    }
}

mod compiler;
mod lexer;
mod parser;
mod utils;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, MietteHandlerOpts, Result};
use std::path::PathBuf;

use crate::utils::gcc::compile_c;

#[derive(Parser)]
#[command(
    name = "ret-lang",
    version = "0.1.0",
    author = "isofinly",
    about = "RET-lang toolchain"
)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    #[command(visible_aliases = &["lx"])]
    Lex { input: PathBuf },
    #[command(visible_aliases = &[ "c"])]
    Compile {
        input: PathBuf,
        #[arg(short, long, default_value = "./.build/out.o")]
        output: PathBuf,
        #[arg(short, long, default_value = "")]
        libraries: Vec<String>,
    },
    #[command(visible_aliases = &["p"])]
    Parse { input: PathBuf },
    #[command(visible_aliases = &["t"])]
    Transpile { input: PathBuf, output: PathBuf },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(
            MietteHandlerOpts::new()
                .color(true)
                .context_lines(3)
                .build(),
        )
    }))?;

    match cli.cmd {
        Cmd::Lex { input } => {
            let src = std::fs::read_to_string(&input).into_diagnostic()?;
            let tokens = lexer::core::lex(input.file_name().unwrap().to_str().unwrap(), &src)?;
            tokens.into_iter().for_each(|t| println!("{t:?}"));
        }

        Cmd::Parse { input } => {
            let src = std::fs::read_to_string(&input).into_diagnostic()?;
            let tokens = lexer::core::lex(input.file_name().unwrap().to_str().unwrap(), &src)?;
            let ast = parser::core::parse(tokens.as_slice())?;
            println!("{ast:?}");
        }

        Cmd::Transpile { input, output } => {
            let src = std::fs::read_to_string(&input).into_diagnostic()?;
            let tokens = lexer::core::lex(input.file_name().unwrap().to_str().unwrap(), &src)?;
            let ast = parser::core::parse(tokens.as_slice())?;
            let js_code = compiler::core::compile(ast)?;
            if !output.exists() {
                std::fs::create_dir_all(output.parent().unwrap()).into_diagnostic()?;
            }
            std::fs::write(&output, js_code).into_diagnostic()?;
        }

        Cmd::Compile {
            input,
            output,
            libraries,
        } => {
            let src = std::fs::read_to_string(&input).into_diagnostic()?;
            let tokens = lexer::core::lex(input.file_name().unwrap().to_str().unwrap(), &src)?;
            let ast = parser::core::parse(tokens.as_slice())?;
            let obj_bin = compiler::core::compile(ast)?;
            if let Some(parent) = output.parent() {
                if !parent.exists() {
                    std::fs::create_dir_all(parent).into_diagnostic()?;
                }
            }
            let mut c_source_path = output.clone();
            c_source_path.set_extension("c");
            std::fs::write(&c_source_path, obj_bin).into_diagnostic()?;
            match compile_c(
                c_source_path.to_str().unwrap(),
                output.to_str().unwrap(),
                libraries.as_slice(),
            ) {
                Ok(_) => {
                    println!("Compilation successful");
                }
                Err(e) => {
                    eprintln!("Compilation failed: {e:?}");
                    std::process::exit(1);
                }
            }
        }
    }

    Ok(())
}

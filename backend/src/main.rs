mod compiler;
mod lexer;
mod linker;
mod utils;

use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, MietteHandlerOpts, Result};
use std::path::PathBuf;

#[derive(Parser)]
#[command(
    name = "ret-lang",
    version = "0.1.0",
    author = "isofinly",
    about = "RET-lang compiler"
)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    Lex {
        input: PathBuf,
    },
    Compile {
        input: PathBuf,
        #[arg(short, long, default_value = "out.o")]
        output: PathBuf,
    },
    Link {
        objects: Vec<PathBuf>,
        #[arg(short, long, default_value = "a.out")]
        output: PathBuf,
    },
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
            tokens.into_iter().for_each(|t| {
                println!("{:?}", t);
            });
        }

        Cmd::Compile { input, output } => {
            let src = std::fs::read_to_string(&input).into_diagnostic()?;
            let tokens = lexer::core::lex(input.file_name().unwrap().to_str().unwrap(), &src)?;
            let obj_bin = compiler::core::compile(tokens);
            std::fs::write(&output, obj_bin).into_diagnostic()?;
            println!("wrote {}", output.display());
        }

        Cmd::Link { objects, output } => {
            linker::core::link(&objects, &output)?;
            println!("linked {}", output.display());
        }
    }

    Ok(())
}

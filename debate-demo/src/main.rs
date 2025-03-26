mod error;

use std::{path::PathBuf, str};

use anyhow::Context;
use debate::{
    from_args::FromArgs,
    help::{self, ParameterUsage, ParameterValueKind, Repetition, Requirement, Usage as _},
    printers::DebugUsage,
};
use debate_derive::{FromArgs, ParameterUsage, Usage, Value};
use debate_parser::ArgumentsParser;
use lazy_format::make_lazy_format;

use crate::error::BuildError;

#[derive(FromArgs, Usage, Debug)]
#[debate(help, author)]
struct Arguments {
    #[debate(short, long = "foo")]
    path: PathBuf,

    #[debate(short, long)]
    verbose: bool,

    #[debate(short)]
    second_path: Option<PathBuf>,

    #[debate(long = "cool-value", default)]
    value: i32,

    #[debate(flatten)]
    inner: Alphabet,

    #[debate(flatten)]
    subcommand: Subcommand,

    extra: Option<String>,
}

#[derive(FromArgs, Usage, Debug)]
struct Alphabet {
    #[debate(long, short, default)]
    alpha: u32,

    #[debate(long, short, default)]
    beta: u32,

    #[debate(long, short, default)]
    gamma: u32,

    #[debate(long)]
    direction: Option<Direction>,
}

#[derive(Debug, Clone, Copy, Value, ParameterUsage)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

enum FlagChoice {
    OutDir(PathBuf),
    WorkingDir,
    OutFile(PathBuf),
}

#[derive(FromArgs, Debug)]
#[debate(subcommand)]
enum Subcommand {
    #[debate(fallback)]
    None,

    Clean,
    Build {
        #[debate(long)]
        target: PathBuf,
    },
    Add {
        item: String,
    },
}

fn main() -> anyhow::Result<()> {
    let args: Vec<Vec<u8>> = std::env::args_os()
        .map(|arg| arg.into_encoded_bytes())
        .collect();

    println!(
        "usage:\n{}",
        make_lazy_format!(|fmt| Arguments::describe(&mut DebugUsage::new(fmt)))
    );

    let args: Result<Arguments, BuildError> = Arguments::from_parser(ArgumentsParser::new(
        args.iter().skip(1).map(|arg| arg.as_slice()),
    ));

    let args = args.context("error parsing CLI argument")?;

    println!("{args:#?}");

    Ok(())
}

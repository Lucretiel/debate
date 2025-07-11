use std::path::PathBuf;

use anyhow::Context;
use debate::{errors::BuildError, from_args::FromArgs, help::Usage as _};
use debate_derive::{FromArgs, ParameterUsage, Usage, Value};
use debate_parser::ArgumentsParser;
use lazy_format::make_lazy_format;

#[derive(FromArgs, Usage, Debug)]
#[debate(help)]
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

    /// The direction you
    /// Want to go
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

#[derive(FromArgs, Usage, Debug)]
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

fn main() {
    let args = debate::arguments::LoadedArguments::from_env();
    let args = Arguments::from_parser(args.parser());

    println!("{args:#?}");
}

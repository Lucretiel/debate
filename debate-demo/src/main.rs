use std::path::PathBuf;
use std::str;

use debate::from_args::FromArgs;
use debate_derive::FromArgs;
use debate_parser::{Arg, ArgumentsParser};

#[derive(FromArgs, Debug)]
#[debate(help, author)]
struct Arguments {
    #[debate(short, long)]
    path: PathBuf,

    #[debate(short, long)]
    verbose: bool,

    #[debate(short, long)]
    second_path: Option<PathBuf>,

    #[debate(long, default)]
    value: i32,

    input: String,
    other_inputs: Vec<String>,
}

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

#[derive(Debug)]
struct Error(String);

impl<'arg> debate::from_args::Error<'arg> for Error {
    type StateError<A> = Self;

    fn from_state_error<A>(
        error: Self::StateError<A>,
        kind: debate::from_args::ParameterKind<'arg>,
        argument: Option<Arg<'arg>>,
    ) -> Self {
        Self(format!("{} for {kind:?} ({argument:?})", error.0))
    }

    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self {
        Self(format!("required field '{field}' was absent"))
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self(msg.to_string())
    }
}

impl<'arg, A> debate::from_args::StateError<'arg, A> for Error {
    type ParameterError = Self;

    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self(format!("field '{field}' {}", error.0))
    }

    fn unrecognized(_: A) -> Self {
        Self(format!("unrecognized argument"))
    }

    fn rejected() -> Self {
        Self(format!("rejected argument"))
    }
}

impl<'arg> debate::parameter::Error<'arg> for Error {
    fn needs_arg() -> Self {
        Self(format!("requires an argument"))
    }

    fn got_arg(arg: Arg<'arg>) -> Self {
        Self(format!("got an unexpected argument: {:?}", arg))
    }

    fn got_additional_instance() -> Self {
        Self("appeared more than once on the command line".to_string())
    }

    fn invalid_utf8(arg: Arg<'arg>) -> Self {
        Self(format!("invalid UTF-8 in argument: {:?}", arg))
    }

    fn parse_error(arg: &str, msg: impl std::fmt::Display) -> Self {
        Self(format!("parse error in argument '{}': {}", arg, msg))
    }

    fn byte_parse_error(arg: Arg<'arg>, msg: impl std::fmt::Display) -> Self {
        Self(format!("byte parse error in argument '{:?}': {}", arg, msg))
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self(msg.to_string())
    }
}

fn main() {
    let args: Vec<Vec<u8>> = std::env::args_os()
        .map(|arg| arg.into_encoded_bytes())
        .collect();

    let args: Result<Arguments, Error> = Arguments::from_args(ArgumentsParser::new(
        args.iter().skip(1).map(|arg| arg.as_slice()),
    ));

    let args = args.unwrap();

    println!("{args:#?}")
}

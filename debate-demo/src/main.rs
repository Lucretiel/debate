use std::path::PathBuf;
use std::str;

use debate::{
    FromArgs,
    error::{FlagError, ParameterError},
};
use debate_derive::FromArgs;
use debate_parser::{Arg, ArgumentsParser};

#[derive(FromArgs, Debug)]
#[debate(help, author)]
pub struct Arguments {
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

impl FlagError for Error {
    type ParameterError = Self;

    fn positional(error: Self::ParameterError) -> Self {
        Self(format!("Positional argument error: {}", error.0))
    }

    fn long(option: Arg<'_>, error: Self::ParameterError) -> Self {
        Self(format!(
            "Long option error for '--{}': {}",
            String::from_utf8_lossy(option.bytes()),
            error.0
        ))
    }

    fn short(option: u8, error: Self::ParameterError) -> Self {
        Self(format!(
            "Short option error for '-{}': {}",
            option as char, error.0
        ))
    }

    fn unrecognized_long(option: Arg<'_>, argument: Option<Arg<'_>>) -> Self {
        let arg_str = argument.map_or("".to_string(), |arg| {
            format!(
                " with argument '{}'",
                String::from_utf8_lossy(option.bytes()),
            )
        });
        Self(format!(
            "Unrecognized long option '--{}'{arg_str}",
            String::from_utf8_lossy(option.bytes()),
        ))
    }

    fn unrecognized_short(option: u8) -> Self {
        Self(format!("Unrecognized short option '{}'", option as char))
    }

    fn unrecognized_positional(arg: Arg<'_>) -> Self {
        Self(format!(
            "Unrecognized positional argument '{}'",
            String::from_utf8_lossy(arg.bytes()),
        ))
    }

    fn absent_parameter(error: Self::ParameterError) -> Self {
        Self(format!("Absent parameter error: {}", error.0))
    }
}

impl ParameterError for Error {
    fn required() -> Self {
        Self("Required parameter missing".to_string())
    }

    fn needs_arg() -> Self {
        Self("Parameter needs an argument".to_string())
    }

    fn got_arg(_arg: Arg<'_>) -> Self {
        Self("Unexpected argument provided".to_string())
    }

    fn got_additional_instance() -> Self {
        Self("Got additional instance of parameter".to_string())
    }

    fn invalid_utf8(_arg: Arg<'_>) -> Self {
        Self("Invalid UTF-8 sequence".to_string())
    }

    fn parse_error(_arg: &str, _msg: impl std::fmt::Display) -> Self {
        Self(format!("Parse error: {}", _msg))
    }

    fn custom(_msg: impl std::fmt::Display) -> Self {
        Self(format!("Custom error: {}", _msg))
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

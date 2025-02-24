use std::path::PathBuf;
use std::str;

use anyhow::Context;
use debate::{
    from_args::{self, BuildFromArgs, FromArgs},
    parameter::{self, Parameter},
    state::{self, State},
};
use debate_derive::{FromArgs, Value};
use debate_parser::{Arg, ArgumentsParser};

// #[derive(FromArgs, Debug)]
// #[debate(help, author)]
// struct Arguments {
//     #[debate(short, long = "foo")]
//     path: PathBuf,

//     #[debate(short, long)]
//     verbose: bool,

//     #[debate(short)]
//     second_path: Option<PathBuf>,

//     #[debate(long = "cool-value", default)]
//     value: i32,

//     #[debate(flatten)]
//     inner: Alphabet,

//     input: String,
//     other_inputs: Vec<String>,
// }

// #[derive(FromArgs, Debug)]
// struct Alphabet {
//     #[debate(long, short, default)]
//     alpha: u32,

//     #[debate(long, short, default)]
//     beta: u32,

//     #[debate(long, short, default)]
//     gamma: u32,

//     #[debate(long)]
//     direction: Option<Direction>,
// }

// #[derive(Debug, Clone, Copy, Value)]
// enum Direction {
//     Up,
//     Down,
//     Left,
//     Right,
// }

// enum FlagChoice {
//     OutDir(PathBuf),
//     WorkingDir,
//     OutFile(PathBuf),
// }

// enum Subcommand {
//     Clean,
//     Build { target: PathBuf },
//     Add { item: String },
// }

// enum SubcommandFieldState {
//     Clean,
//     Build { target: Option<PathBuf> },
//     Add { item: Option<String> },
// }

// #[derive(Default)]
// struct SubcommandState {
//     fields: Option<SubcommandFieldState>,
//     position: u16,
// }

// impl<'arg> State<'arg> for SubcommandState {
//     fn add_positional<E>(&mut self, argument: Arg<'arg>) -> Result<(), E>
//     where
//         E: state::Error<'arg, ()>,
//     {
//         match self.fields {
//             None => {
//                 self.fields = match argument.bytes() {
//                     b"clean" => SubcommandFieldState::Clean,
//                     b"build" => SubcommandFieldState::Build { target: None },
//                     b"add" => SubcommandFieldState::Add { item: None },
//                     _ => return Err(E::unrecognized(())),
//                 };

//                 Ok(())
//             }
//             Some(state) => match state {
//                 SubcommandFieldState::Clean => Err(E::unrecognized(())),
//                 SubcommandFieldState::Build { target } => Err(E::unrecognized(())),
//                 SubcommandFieldState::Add { ref mut item } => {
//                     if self.position <= 0 {
//                         match match item {
//                             None => match Parameter::arg(argument) {
//                                 Ok(value) => {
//                                     *item = Some(value);
//                                     Ok(())
//                                 },
//                                 Err(err),
//                             },
//                             Some(old) => Parameter::add_arg(old, argument),
//                         } {
//                             Ok(()) => return Ok(()),
//                             Err()
//                         }
//                     }
//                 }
//             },
//         }
//     }

//     fn add_long_option<E>(&mut self, option: Arg<'arg>, argument: Arg<'arg>) -> Result<(), E>
//     where
//         E: state::Error<'arg, ()>,
//     {
//         todo!()
//     }

//     fn add_long<A, E>(&mut self, option: Arg<'arg>, argument: A) -> Result<(), E>
//     where
//         A: debate_parser::ArgAccess<'arg>,
//         E: state::Error<'arg, A>,
//     {
//         todo!()
//     }

//     fn add_short<A, E>(&mut self, option: u8, argument: A) -> Result<(), E>
//     where
//         A: debate_parser::ArgAccess<'arg>,
//         E: state::Error<'arg, A>,
//     {
//         todo!()
//     }
// }

// impl<'arg> BuildFromArgs<'arg> for Subcommand {
//     type State = SubcommandState;

//     fn build<E>(state: Self::State) -> Result<Self, E>
//     where
//         E: from_args::Error<'arg>,
//     {
//         match state.fields {
//             None => Err(E::required("subcommand", None, None)),
//             Some(fields) => match fields {
//                 SubcommandFieldState::Clean => Ok(Self::Clean),
//                 SubcommandFieldState::Build { target } => Ok(Self::Build {
//                     target: match target {
//                         Some(target) => target,
//                         None => Parameter::absent()
//                             .map_err(|_| E::required("target", Some("target"), None))?,
//                     },
//                 }),
//                 SubcommandFieldState::Add { item } => Ok(Self::Add {
//                     item: match item {
//                         Some(item) => item,
//                         None => Parameter::absent()
//                             .map_err(|_| E::required("item", Some("item"), None))?,
//                     },
//                 }),
//             },
//         }
//     }
// }

#[derive(Debug, thiserror::Error)]
enum BuildError {
    #[error("required field {0:?} wasn't present")]
    Required(&'static str),

    #[error("{0}")]
    Custom(String),

    #[error("--{0}: {1}")]
    Long(String, StateError),

    #[error("-{0}: {1}")]
    Short(char, StateError),

    #[error("argument {0}: {1}")]
    Positional(String, StateError),
}

impl<'arg> from_args::Error<'arg> for BuildError {
    type StateError<A> = StateError;

    fn positional(argument: Arg<'arg>, error: Self::StateError<()>) -> Self {
        Self::Positional(
            String::from_utf8_lossy(argument.bytes()).into_owned(),
            error,
        )
    }

    fn long_with_argument(
        option: Arg<'arg>,
        _argument: Arg<'arg>,
        error: Self::StateError<()>,
    ) -> Self {
        Self::long::<()>(option, error)
    }

    fn long<A>(option: Arg<'arg>, error: Self::StateError<A>) -> Self {
        Self::Long(String::from_utf8_lossy(option.bytes()).into_owned(), error)
    }

    fn short<A>(option: u8, error: Self::StateError<A>) -> Self {
        Self::Short(option as char, error)
    }

    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self {
        Self::Required(field)
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self::Custom(msg.to_string())
    }
}

#[derive(Debug, thiserror::Error)]
enum StateError {
    #[error("error for field {0}: {1}")]
    Parameter(&'static str, ParameterError),

    #[error("unrecognized argument")]
    Unrecognized,
}

impl<'arg, A> state::Error<'arg, A> for StateError {
    type ParameterError = ParameterError;
    fn parameter(field: &'static str, error: Self::ParameterError) -> Self {
        Self::Parameter(field, error)
    }

    fn unrecognized(_: A) -> Self {
        Self::Unrecognized
    }

    fn flattened(field: &'static str, error: Self) -> Self {
        error
    }

    fn unknown_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Parameter(
            "subcommand",
            ParameterError::Custom("invalid subcommand".to_owned()),
        )
    }

    fn rejected() -> Self {
        Self::Unrecognized
    }
}

#[derive(Debug, thiserror::Error)]
enum ParameterError {
    #[error("this parameter requires an argument")]
    NeedArg,

    #[error("this parameter doesn't take an argument (got {0})")]
    GotArg(String),

    #[error("this parameter appeared more than once on the command line")]
    GotExtra,

    #[error("argument contained invalid UTF8")]
    InvalidUtf8,

    #[error("error parsing argument {argument:?}: {error}")]
    ParseError { argument: String, error: String },

    #[error("error parsing raw bytes: {error}")]
    ByteParseError { error: String },

    #[error("{0}")]
    Custom(String),
}

impl<'arg> debate::parameter::Error<'arg> for ParameterError {
    fn needs_arg() -> Self {
        Self::NeedArg
    }
    fn got_arg(arg: Arg<'arg>) -> Self {
        Self::GotArg(String::from_utf8_lossy(arg.bytes()).into_owned())
    }

    fn got_additional_instance() -> Self {
        Self::GotExtra
    }

    fn invalid_utf8(_: Arg<'arg>) -> Self {
        Self::InvalidUtf8
    }

    fn parse_error(arg: &str, msg: impl std::fmt::Display) -> Self {
        Self::ParseError {
            argument: arg.to_string(),
            error: msg.to_string(),
        }
    }

    fn byte_parse_error(_: Arg<'arg>, error: impl std::fmt::Display) -> Self {
        Self::ByteParseError {
            error: error.to_string(),
        }
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self::Custom(msg.to_string())
    }
}

fn main() -> anyhow::Result<()> {
    // let args: Vec<Vec<u8>> = std::env::args_os()
    //     .map(|arg| arg.into_encoded_bytes())
    //     .collect();

    // let args: Result<Arguments, BuildError> = Arguments::from_args(ArgumentsParser::new(
    //     args.iter().skip(1).map(|arg| arg.as_slice()),
    // ));

    // let args = args.context("error parsing CLI argument")?;

    // println!("{args:#?}");

    Ok(())
}

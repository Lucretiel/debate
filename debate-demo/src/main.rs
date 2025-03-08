use std::ops::Add;
use std::str;
use std::{ops::Sub, path::PathBuf};

use anyhow::Context;
use debate::build;
use debate::help::{Repetition, Requirement};
use debate::{
    from_args::{self, FromArgs},
    help::{Descriptions, Usage, UsageHelper, ValueParameter},
    state,
};
use debate_derive::{FromArgs, Value};
use debate_parser::{Arg, ArgumentsParser};
use lazy_format::make_lazy_format;

// Recursive expansion of FromArgs macro
// ======================================

#[derive(FromArgs, Debug)]
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

impl Usage for Arguments {
    fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
    where
        R: debate::help::Receiver,
    {
        receiver.option(
            debate::help::Tags::LongShort {
                long: "foo",
                short: 'p',
            },
            Some(ValueParameter {
                metavariable: "PATH",
                values: None,
            }),
            debate::help::Requirement::Mandatory,
            Repetition::Single,
            Descriptions {
                short: "the path",
                long: "the path",
            },
        )?;

        receiver.option(
            debate::help::Tags::LongShort {
                long: "verbose",
                short: 'v',
            },
            None,
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "enable verbose output",
                long: "enable verbose output",
            },
        )?;

        receiver.option(
            debate::help::Tags::Short { short: 's' },
            Some(ValueParameter {
                metavariable: "SECOND_PATH",
                values: None,
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "the second path",
                long: "the second path",
            },
        )?;

        receiver.option(
            debate::help::Tags::Long { long: "cool-value" },
            Some(ValueParameter {
                metavariable: "VALUE",
                values: None,
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "a cool value",
                long: "a cool value",
            },
        )?;

        receiver.group("Inner", UsageHelper::<Alphabet>::new())?;
        receiver.group("Subcommand", UsageHelper::<Subcommand>::new())?;

        receiver.positional(
            ValueParameter {
                metavariable: "EXTRA",
                values: None,
            },
            Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "a positional field",
                long: "a positional field",
            },
        )?;

        Ok(())
    }
}

#[derive(FromArgs, Debug)]
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

impl Usage for Alphabet {
    fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
    where
        R: debate::help::Receiver,
    {
        receiver.option(
            debate::help::Tags::LongShort {
                long: "alpha",
                short: 'a',
            },
            Some(ValueParameter {
                metavariable: "ALPHA",
                values: None,
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "alpha value",
                long: "alpha value",
            },
        )?;

        receiver.option(
            debate::help::Tags::LongShort {
                long: "beta",
                short: 'b',
            },
            Some(ValueParameter {
                metavariable: "BETA",
                values: None,
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "beta value",
                long: "beta value",
            },
        )?;

        receiver.option(
            debate::help::Tags::LongShort {
                long: "gamma",
                short: 'g',
            },
            Some(ValueParameter {
                metavariable: "GAMMA",
                values: None,
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "gamma value",
                long: "gamma value",
            },
        )?;

        receiver.option(
            debate::help::Tags::Long { long: "direction" },
            Some(ValueParameter {
                metavariable: "DIRECTION",
                values: Some(&["up", "down", "left", "right"]),
            }),
            debate::help::Requirement::Optional,
            Repetition::Single,
            Descriptions {
                short: "direction value",
                long: "direction value",
            },
        )?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Value)]
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

impl Usage for Subcommand {
    fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
    where
        R: debate::help::Receiver,
    {
        struct CleanSubcommand;
        impl Usage for CleanSubcommand {
            fn describe<R>(_receiver: &mut R) -> Result<(), R::Err>
            where
                R: debate::help::Receiver,
            {
                Ok(())
            }
        }

        struct BuildSubcommand;
        impl Usage for BuildSubcommand {
            fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
            where
                R: debate::help::Receiver,
            {
                receiver.option(
                    debate::help::Tags::Long { long: "target" },
                    Some(ValueParameter {
                        metavariable: "TARGET",
                        values: None,
                    }),
                    debate::help::Requirement::Mandatory,
                    Repetition::Single,
                    Descriptions {
                        short: "build target",
                        long: "build target",
                    },
                )?;

                Ok(())
            }
        }

        struct AddSubcommand;
        impl Usage for AddSubcommand {
            fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
            where
                R: debate::help::Receiver,
            {
                receiver.positional(
                    ValueParameter {
                        metavariable: "ITEM",
                        values: None,
                    },
                    Requirement::Mandatory,
                    Repetition::Single,
                    Descriptions {
                        short: "the item to add",
                        long: "the item to add",
                    },
                )
            }
        }

        struct SubcommandGroups;

        impl Usage for SubcommandGroups {
            fn describe<R>(receiver: &mut R) -> Result<(), R::Err>
            where
                R: debate::help::Receiver,
            {
                receiver.subcommand(
                    "clean",
                    Descriptions {
                        short: "clean something",
                        long: "clean something",
                    },
                    UsageHelper::<CleanSubcommand>::new(),
                )?;

                receiver.subcommand(
                    "build",
                    Descriptions {
                        short: "build a target",
                        long: "build a target",
                    },
                    UsageHelper::<BuildSubcommand>::new(),
                )?;

                receiver.subcommand(
                    "add",
                    Descriptions {
                        short: "add an item",
                        long: "add an item",
                    },
                    UsageHelper::<AddSubcommand>::new(),
                )?;

                Ok(())
            }
        }

        receiver.exclusive_group(
            Requirement::Optional,
            UsageHelper::<SubcommandGroups>::new(),
        )
    }
}

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

    #[error("required subcommand was absent")]
    Subcommand,

    #[error("got --help, need to print a usage message")]
    HelpRequested,
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
}

impl build::Error for BuildError {
    fn required(field: &'static str, long: Option<&'static str>, short: Option<char>) -> Self {
        Self::Required(field)
    }

    fn custom(msg: impl std::fmt::Display) -> Self {
        Self::Custom(msg.to_string())
    }

    fn flattened(field: &'static str, error: Self) -> Self {
        error
    }

    fn required_subcommand(expected: &'static [&'static str]) -> Self {
        Self::Subcommand
    }

    fn help_requested() -> Self {
        todo!()
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

    fn wrong_subcommand_for_argument(subcommand: &str, allowed: &[&'static str]) -> Self {
        todo!()
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
    let args: Vec<Vec<u8>> = std::env::args_os()
        .map(|arg| arg.into_encoded_bytes())
        .collect();

    let args: Result<Arguments, BuildError> = Arguments::from_parser(ArgumentsParser::new(
        args.iter().skip(1).map(|arg| arg.as_slice()),
    ));

    let args = args.context("error parsing CLI argument")?;

    println!("{args:#?}");

    Ok(())
}

/*!
Container type for raw arguments retrieved from the environment.
 */

use std::{
    ffi::{OsStr, OsString},
    vec::Vec,
};

use debate_parser::ArgumentsParser;

use crate::{
    build,
    from_args::{self, FromArgs},
    help::Usage,
};

/**
Helper type for loading arguments from the environment. Usually you can
just use [`#[debate::main]`][crate::main] instead of reaching for this type.

This type exists for roughly two purposes: to provide a convenient
owned container for args retrieved from [`mod@std::env`], and to be an object
that a [`FromArgs`] can borrow from.
*/
#[derive(Debug, Clone)]
pub struct LoadedArguments {
    arguments: Vec<OsString>,
}

impl LoadedArguments {
    /// Create a [`LoadedArguments`] via [`std::env::args`].
    pub fn from_env() -> Self {
        Self {
            arguments: std::env::args_os().collect(),
        }
    }

    /**
    Get the first argument in the arguments list; this is usually the name
    of the program being executed.
    */
    pub fn argv0(&self) -> &OsStr {
        self.arguments[0].as_os_str()
    }

    /// Create a new [`ArgumentsParser`] that borrows from `self`.
    pub fn parser(&self) -> ArgumentsParser<'_, impl Iterator<Item = &[u8]>> {
        ArgumentsParser::new(
            self.arguments
                .get(1..)
                .unwrap_or(&[])
                .iter()
                .map(|arg| arg.as_encoded_bytes()),
        )
    }

    /**
    Try to parse a [`FromArgs`] type from the loaded arguments; return a
    structured error if there were any issues.
    */
    pub fn try_parse<'a, T, E>(&'a self) -> Result<T, E>
    where
        T: FromArgs<'a>,
        E: from_args::Error<'a> + build::Error,
    {
        T::try_from_parser(self.parser())
    }

    /**
    Parse a [`FromArgs`] type from the loaded arguments. If there is an error,
    print a useful error message than exit with an error code. If help was
    requested, print a usage message, then exist with a success code.
    */
    pub fn parse<'a, T>(&'a self) -> T
    where
        T: FromArgs<'a> + Usage,
    {
        T::from_parser(self.parser())
    }
}
